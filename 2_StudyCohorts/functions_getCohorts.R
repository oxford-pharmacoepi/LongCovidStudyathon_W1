# Functions used throughout the study

do_exclusion <- function(cdm, cohort, id, S_start_date) {
  # function that applies exclusion criteria and gets attrition of specified base cohort
  attrition <- dplyr::tibble(
    number_observations = cohort %>% dplyr::tally() %>% dplyr::pull(),
    reason = "Starting events"
  )
  
  # Apply washout 42 days
  cohort <- cohort %>% 
    addCohortIntersectDays(cdm, targetCohortTable = InitialCohortsName, targetCohortId = id, window = list(c(-Inf,-1)), order = "last", nameStyle = "date_previous") %>%
    computeQuery()
  
  cohort <- cohort %>%
    filter(is.na(.data$date_previous) | .data$date_previous < -42) %>% computeQuery()
  
  attrition <- rbind(attrition, 
                     dplyr::tibble(
                       number_observations = cohort %>% dplyr::tally() %>%
                         dplyr::pull(), reason = "Event washout"))
  cohort <- cohort %>% dplyr::select(-date_previous) %>% computeQuery()
  
  # Check the individuals are in observation at cohort entry
  if(sql_dem) {
    cohort <- cohort %>% addInObservation_sql(cdm) %>%
      filter(.data$in_observation == 1) %>% computeQuery()
  } else {
    cohort <- cohort %>% addInObservation(cdm) %>%
      filter(.data$in_observation == 1) %>% computeQuery()
  }

  attrition <- rbind(attrition,
                     dplyr::tibble(number_observations = cohort %>%
                                     dplyr::tally() %>% dplyr::pull(),
                                   reason = "In observation at cohort entry"))
  
  # Prior observation 365 days
  if(sql_dem) {
    cohort <- cohort %>% addPriorObservation_sql(cdm) %>% computeQuery() 
  } else {
    cohort <- cohort %>% addPriorObservation(cdm) %>% computeQuery() 
  }
  
  cohort <- cohort %>% filter(.data$prior_observation >= 365) %>% computeQuery()
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = cohort %>%
                                     dplyr::tally()
                                   %>% dplyr::pull(), reason = paste0("365 days of prior observation")))
  
  cohort <- cohort %>% dplyr::select(-c(prior_observation)) %>% computeQuery()
  
  # Historical influenza 90 days
  cohort <- cohort %>%
    addCohortIntersectDays(cdm, targetCohortTable = InitialCohortsName, targetCohortId = 3, window = list(c(-90,-1)), order = "last", nameStyle = "last_flu") %>%
    computeQuery()
  
  cohort <- cohort %>% dplyr::filter(is.na(.data$last_flu)) %>% computeQuery()
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = cohort %>%
                                     dplyr::tally()
                                   %>% dplyr::pull(), reason = "Historical influenza"))
  cohort <- cohort %>% dplyr::select(-last_flu) %>% computeQuery()
  
  
  # keep only people starting after S_start_date
  cohort <- cohort %>% dplyr::filter(.data$cohort_start_date > S_start_date) %>% computeQuery()
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = cohort %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = paste0("Entry after ",
                                                                      S_start_date)))
  
  # censor on observation_end, death, end of covid testing or study end date
  cohort <- cohort %>% 
    addCohortIntersectDate(cdm, targetCohortTable = InitialCohortsName, targetCohortId = 1, window = list(c(1, 365)), order = "first", nameStyle = "next_covid") %>%
    computeQuery()
  # censor on observation_end, death, study end date, or covid (re)infection
  cohort <- cohort %>% dplyr::mutate(one_year_date = 
                                       CDMConnector::dateadd("cohort_start_date", 365)) %>%
    dplyr::mutate(end_covid_testing_date =  as.Date(.env$covid_end_date)) %>% # asked in the CodeToRun file
    dplyr::left_join(observation_death, by = c("subject_id")) %>%
    computeQuery()
  
  cohort <- cohort %>% 
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
      !(is.na(.data$death_date)) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
      !(is.na(.data$next_covid)) & .data$cohort_end_date > .data$next_covid, .data$next_covid, .data$cohort_end_date))) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
      !(is.na(.data$one_year_date)) & .data$cohort_end_date > .data$one_year_date, .data$one_year_date, .data$cohort_end_date))) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
      !(is.na(.data$end_covid_testing_date)) & .data$cohort_end_date > .data$end_covid_testing_date, .data$end_covid_testing_date, .data$cohort_end_date))) %>%
    dplyr::mutate(follow_up_days = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date")) %>% 
    dplyr::mutate(reason_censoring = ifelse(!(is.na(.data$death_date)) & cohort_end_date == .data$death_date, "death",
                                            ifelse(cohort_end_date == .data$one_year_date, "one year of follow_up",
                                                   ifelse(cohort_end_date == .data$end_covid_testing_date, "End of COVID-19 testing",
                                                          ifelse(cohort_end_date == .data$observation_period_end_date,
                                                                 "end of data collection or exit from database",
                                                                 ifelse(cohort_end_date == .data$next_covid, "covid infection", NA )))))) %>% computeQuery()
  
  # exclude if follow-up < 120 days
  excluded_followup <- cohort %>% dplyr::filter(.data$follow_up_days < 120) %>%
    computeQuery()
  reason_exclusion <- excluded_followup %>% dplyr::group_by(.data$reason_censoring) %>%
    tally() %>% collect()
  cohort <- cohort %>% dplyr::filter(.data$follow_up_days >= 120) %>% 
    dplyr::select(-follow_up_days, -reason_censoring) %>% computeQuery()
  
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = cohort %>%
                                     dplyr::tally()
                                   %>% dplyr::pull(), reason = paste0(
                                     "> 120 days follow-up")))
  
  # get first or subsequent events
  cohort <- cohort %>% dplyr::group_by(subject_id) %>% dbplyr::window_order(.data$cohort_start_date) %>%
    dplyr::mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% computeQuery()
  
  cohort <- cohort %>% dplyr::select(subject_id, cohort_definition_id, 
                                     cohort_start_date, cohort_end_date, seq) %>% 
    computeQuery()
  
  first_event <- cohort %>% dplyr::filter(seq == 1) %>% dplyr::select(-seq) %>% computeQuery()
  subs_events <- cohort %>% dplyr::filter(seq != 1) %>% dplyr::select(-seq) %>% computeQuery()
  
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = first_event %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = "First event only"))
  
  # Not for any "re-event" cohort
  # No historical covid-19 infection
  first_event <- first_event %>% 
    addCohortIntersectDate(cdm, targetCohortTable = InitialCohortsName, targetCohortId = 1, window = list(c(-Inf, -1)), order = "last", nameStyle = "event") %>%
    computeQuery()
  
  first_event <- first_event %>% dplyr::filter(is.na(.data$event)) %>% computeQuery()
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = first_event %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = "Historical COVID-19"))
  first_event <- first_event %>% dplyr::select(-c(event)) %>% computeQuery()
  
  return(list(first_event,subs_events,attrition,reason_exclusion))
}

create_outcome <- function(cdm, window, new_ids) {
  counter <- 1
  for(i in window){
    new_id <- new_ids[counter]
    name_cohort <- Initial_cohorts$cohort_name[i]
    current <- cdm[[InitialCohortsName]] %>% 
      dplyr::filter(cohort_definition_id == i) %>% dplyr::select(
        "subject_id",
        "cohort_start_date"
      ) %>% computeQuery() 
    attrition <- dplyr::tibble(
      number_observations = current %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Starting events"
    )
    
    current <- current %>% dplyr::mutate(cohort_end_date = .data$cohort_start_date) %>%
      computeQuery()
    
    current <- current %>% dplyr::mutate(cohort_definition_id = new_id) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      computeQuery()
    
    if(isTRUE(counter == 1)) {
      currenttable <- current %>% computeQuery()
      
    } else {
      currenttable <- dplyr::union_all(currenttable, current) %>% computeQuery()
    }
    
    write.csv(
      attrition,
      file = here::here(output_at, paste0("attrition_",Initial_cohorts$cohort_name[i],".csv"))
    )
    
    counter <- counter + 1
  }
  
  return(currenttable)
}

create_any_cohort <- function(cdm, window, cohort_id, tableold, name) {
  cohorts <- tableold %>%
    dplyr::filter(cohort_definition_id %in% window) %>%
    computeQuery()
  
  attrition <- dplyr::tibble(
    number_observations = cohorts %>% dplyr::tally() %>% dplyr::pull(),
    reason = "Starting events"
  )
  
  any_cohort <- cohorts %>% dplyr::select(-cohort_definition_id) %>%
    dplyr::mutate(cohort_definition_id = cohort_id) %>% 
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    computeQuery()
  
  tableold <- dplyr::union_all(tableold,any_cohort) %>% 
    computeQuery()
  
  write.csv(
    attrition,
    file = here::here(output_at, paste0("attrition_any_",name,".csv"))
  )
  
  return(tableold)  
}

do_overlap <- function(cdm, base_cohort_id, outcome_cohort_id, overlap_cohort_id, tableName) {
  bases <- c("inf", "reinf", "testneg")
  base <- cdm[[BaseCohortsName]] %>% 
    dplyr::filter(cohort_definition_id == base_cohort_id) %>%
    computeQuery()
  
  outcome <- cdm[[tableName]] %>% 
    dplyr::filter(cohort_definition_id == outcome_cohort_id) %>%
    computeQuery()
  
  overlap <- base %>% 
    dplyr::inner_join(
      outcome %>% 
        dplyr::select(subject_id, outcome_date = cohort_start_date, 
                      outcome_end = cohort_end_date),
      by = "subject_id"
    ) %>% distinct() %>%
    computeQuery()
  
  attrition <- dplyr::tibble(
    number_observations = overlap %>% dplyr::tally() %>% dplyr::pull(),
    reason = "Starting events"
  )
  
  overlap <- overlap %>%
    dplyr::mutate(cohort_definition_id = overlap_cohort_id) %>%
    dplyr::mutate(time_diff = !!CDMConnector::datediff("outcome_date","cohort_start_date")) %>%
    dplyr::filter(time_diff < -90 & time_diff > -366) %>%
    dplyr::select(-time_diff) %>%
    computeQuery()
  
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = overlap %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = "Outcome in window (90,365)"))
  
  overlap <- overlap %>% 
    addCohortIntersectFlag(
      cdm, targetCohortTable = tableName, targetCohortId = outcome_cohort_id, 
      window = list(c(-180,-1)), nameStyle = "event") %>%
    computeQuery()
  
  if("event" %in% colnames(overlap)) {
    overlap <- overlap %>% 
      dplyr::filter(event == 0) %>% 
      dplyr::select(-c(event)) %>%
      computeQuery()
    
  }
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = overlap %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = "180 days of washout for the outcome"))
  
  overlap <- overlap %>% 
    dplyr::select(subject_id,cohort_definition_id,outcome_date,outcome_end) %>%
    dplyr::rename("cohort_start_date" = "outcome_date") %>% 
    dplyr::rename("cohort_end_date" = "outcome_end") %>%
    distinct() %>%
    computeQuery()
  
  write.csv(
    attrition,
    file = here::here(output_at, paste0("attrition_",bases[base_cohort_id],"_",Initial_cohorts$cohort_name[outcome_cohort_id + 4],".csv"))
  )
  
  return(overlap)
}

overlap_per_base <- function(base_id, base_name, base_table) {
  message("Getting overlap lc cohorts")
  info(logger, '--- Getting overlap lc cohorts')
  
  # Infection + 1 LC symptom / LC code, any symptom, 1 PASC event, any PASC, 1 MC
  overlap_cohorts <- do_overlap(cdm, base_id, 1, 1, tableName = LongCovidCohortsName)
  
  for(i in c(2:25)) {
    if(cdm[[LongCovidCohortsName]] %>% 
       dplyr::filter(cohort_definition_id == i) %>% tally() %>% pull() > 5) {
      overlap_cohorts <- dplyr::union_all(
        overlap_cohorts,
        do_overlap(cdm, base_id, i, i, tableName = LongCovidCohortsName)
      ) %>%
        computeQuery()
    }
  }
  
  if(cdm[[LongCovidCohortsName]] %>% 
     dplyr::filter(cohort_definition_id == 27) %>% tally() %>% pull() > 5) {
    overlap_cohorts <- dplyr::union_all(
      overlap_cohorts,
      do_overlap(cdm, base_id, 27, 26, tableName = LongCovidCohortsName)
    ) %>%
      computeQuery()
  }
  
  overlap_all_lc <- overlap_cohorts %>%
    dplyr::filter(cohort_definition_id %in% c(1:26)) %>%
    dplyr::mutate(cohort_definition_id = 27) %>%
    dplyr::group_by(subject_id) %>%
    dbplyr::window_order(cohort_start_date) %>%
    dplyr::mutate(seq = row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(seq == 1) %>%
    dplyr::select(-seq) %>%
    computeQuery()
  
  overlap_cohorts <- dplyr::union_all(
    overlap_cohorts,
    overlap_all_lc
  ) %>%
    computeQuery()
  
  if(!onlyLC) {
    message("Getting overlap pasc cohorts")
    info(logger, '--- Getting overlap pasc cohorts')
    
    for(i in c(1:10)) {
      if(cdm[[PascCohortsName]] %>% 
         dplyr::filter(cohort_definition_id == i) %>% tally() %>% pull() > 5) {
        overlap_cohorts <- dplyr::union_all(
          overlap_cohorts,
          do_overlap(cdm, base_id, i, 27 + i, tableName = PascCohortsName)
        ) %>%
          computeQuery()
      }
    }
  
  overlap_all_pasc <- overlap_cohorts %>%
    dplyr::filter(cohort_definition_id %in% c(28:37)) %>%
    dplyr::mutate(cohort_definition_id = 38) %>%
    dplyr::group_by(subject_id) %>%
    dbplyr::window_order(cohort_start_date) %>%
    dplyr::mutate(seq = row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(seq == 1) %>%
    dplyr::select(-seq) %>%
    computeQuery()
  overlap_cohorts <- dplyr::union_all(
    overlap_cohorts,
    overlap_all_pasc
  ) %>%
    computeQuery()
  
  message("Getting overlap mc cohorts")
  info(logger, '--- Getting overlap mc cohorts')
  
  for(i in c(1:10)) {
    if(cdm[[MCCohortsName]] %>% 
       dplyr::filter(cohort_definition_id == i) %>% tally() %>% pull() > 5) {
      overlap_cohorts <- dplyr::union_all(
        overlap_cohorts,
        do_overlap(cdm, base_id, i, 38 + i, tableName = MCCohortsName)
      ) %>%
        computeQuery()
    }
  }
  
}
  
  return(overlap_cohorts)
  
}

