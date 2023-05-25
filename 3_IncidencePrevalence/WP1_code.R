# WP1: Incidence and Prevalence

# Output folder for WP1
output_ip <- file.path(tempDir,"IP")
  if (!file.exists(output_ip)){
    dir.create(output_ip, recursive = TRUE)}

# ----------------------------------------------------------------
# 1a: LC on base cohorts
info(logger, '-- Calculating incidence and prevalence for outcomes in base cohorts')

names_cohorts <- names_final_cohorts %>%
  dplyr::filter(table_name == OverlapCohortsName) %>%
  dplyr::select(cohort_name) %>% dplyr::pull()

names_lc <- names_final_cohorts %>%
  dplyr::filter(table_name == LongCovidCohortsName) %>%
  dplyr::select(cohort_name) %>% dplyr::pull()

if(!onlyLC) {
  names_pasc <- names_final_cohorts %>%
    dplyr::filter(table_name == PascCohortsName) %>%
    dplyr::select(cohort_name) %>% dplyr::pull()
  
  names_mc <- names_final_cohorts %>%
    dplyr::filter(table_name == MCCohortsName) %>%
    dplyr::select(cohort_name) %>% dplyr::pull()
}

# Change cohort_start_dates of base cohorts to 90 days after index date
cdm[[BaseCohortsName]] <- cdm[[BaseCohortsName]] %>%
  dplyr::mutate(cohort_start_date = CDMConnector::dateadd(cohort_start_date, 90)) %>%
  computeQuery()

calculate_IP <- function(base_id, outcome_id, tableBase, tableOutcome) {
  message("Calculating IP for base ", base_id," and outcome ",outcome_id)
  
  base_name <- ifelse(base_id == 1, "Inf",
                      ifelse(base_id == 2, "Reinf", 
                             ifelse(base_id == 3, "Neg", NA)))
  
  date_to_consider <- as.Date("2020-09-01")
  date_to_end <-as.Date(latest_data_availability)
  
  message("- No strata and sex strata")
  # No strata and sex strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = tableBase,
    strataCohortId = base_id,
    startDate = date_to_consider,
    endDate = date_to_end,
    sex = c("Male", "Female", "Both")
  )
  if(cdm$denominator %>% tally() %>% pull() != 0) {
  for(i in outcome_id) {
  cdm$outcome <- cdm[[tableOutcome]] %>%
    dplyr::filter(.data$cohort_definition_id == i)
  if(cdm$outcome %>% tally() %>% pull() != 0) {
    inc <- IncidencePrevalence::estimateIncidence(
      cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
      interval = c("years","months","overall"),
      completeDatabaseIntervals = FALSE, minCellCount = 5)
    
    study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
      cdm=cdm, resultList=list(inc))
    
    IncidencePrevalence::exportIncidencePrevalenceResults(
      result=study_results, zipName=paste0(names_cohorts[i],"_AllandSex"),
      outputFolder=output_ip) 
  }
  }
  }
  message("- Age strata")
  # Age strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = tableBase,
    strataCohortId = base_id,
    startDate = date_to_consider,
    endDate = date_to_end,
    ageGroup = list(c(0,6),c(7,11),c(12,18),c(19,40),c(41,64),c(65,120))
  )
  if(cdm$denominator %>% tally() %>% pull() != 0) {
  for(i in outcome_id) {
  cdm$outcome <- cdm[[tableOutcome]] %>%
    dplyr::filter(.data$cohort_definition_id == i)
  if(cdm$outcome %>% tally() %>% pull() != 0) {
    inc <- IncidencePrevalence::estimateIncidence(
      cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
      interval = c("years","months","overall"),
      completeDatabaseIntervals = FALSE,  
       minCellCount = 5)

  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
    cdm=cdm, resultList=list(inc))
  write.csv(study_results$incidence_estimates, file = here::here(output_ip, paste0("estimates_",names_cohorts[i],"_Age")))
  write.csv(study_results$incidence_attrition, file = here::here(output_ip, paste0("attrition_",names_cohorts[i],"_Age")))
  
  IncidencePrevalence::exportIncidencePrevalenceResults(
    result=study_results, zipName=paste0(names_cohorts[i],"_Age"),
    outputFolder=output_ip) 
  }
  }
  }
}

# base + one symptom or event or condition / any symptom or event
if(!onlyLC) {
  calculate_IP(1, c(1:26, 79, 82:91, 112, 115:137), BaseCohortsName, OverlapCohortsName)
  info(logger, '--- Long Covid, PACS and MC on infection cohort done')
  calculate_IP(2, c(27:52, 80, 92:101, 113, 138:160), BaseCohortsName, OverlapCohortsName)
  info(logger, '--- Long Covid, PACS and MC on reinfection cohort done')
  calculate_IP(3, c(53:78, 81, 102:111, 114, 161:183), BaseCohortsName, OverlapCohortsName)
  info(logger, '--- Long Covid, PACS and MC on test negative cohort done')
} else {
  calculate_IP(1, c(1:26, 53), BaseCohortsName, OverlapCohortsName)
  info(logger, '--- Long Covid on infection cohort done')
  calculate_IP(2, c(27:52, 54), BaseCohortsName, OverlapCohortsName)
  info(logger, '--- Long Covid on reinfection cohort done')
}

# ----------------------------------------------------------------
# 1b: LC on source population

info(logger, '-- Calculating incidence and prevalence for outcomes and base cohorts in source population')

calculate_IP_allpop <- function(outcome_id, date_to_consider, date_to_end, tableOutcome, symptom = FALSE) {
  
  message("Calculating IP for outcome ",outcome_id)
  
  message("- No strata and sex strata")
  # No strata and sex strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    startDate = date_to_consider,
    endDate = date_to_end,
    sex = c("Male", "Female", "Both")
  )
  if(cdm$denominator %>% tally() %>% pull() != 0) {
  for(i in outcome_id) {
  cdm$outcome <- cdm[[tableOutcome]] %>%
    dplyr::filter(.data$cohort_definition_id == i)
  if(cdm$outcome %>% tally() %>% pull() != 0) {
    inc <- IncidencePrevalence::estimateIncidence(
      cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
      interval = c("years","months","overall"),
      completeDatabaseIntervals = FALSE,  
       minCellCount = 5)
  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
    cdm=cdm, resultList=list(inc))
  # CHANGE to get csvs, not ZIPS!
  
  if(name == "LC") {
    exportIncidencePrevalenceResults(
      result=study_results, zipName=paste0("Allpop_",names_cohorts[i],"_AllandSex"), 
      outputFolder=output_ip) 
  }  else if (name == "PASC") {
    exportIncidencePrevalenceResults(
      result=study_results, zipName=paste0("Allpop_",names_lc[i],"_AllandSex"), 
      outputFolder=output_ip) 
  } else if (name == "MC") {
    exportIncidencePrevalenceResults(
      result=study_results, zipName=paste0("Allpop_",names_pasc[i],"_AllandSex"), 
      outputFolder=output_ip) 
  } else {
    exportIncidencePrevalenceResults(
      result=study_results, zipName=paste0("Allpop_",names_mc[i],"_AllandSex"), 
      outputFolder=output_ip) 
  }
  
  }
  }
  }
  
  message("- Age strata")
  # Age strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    startDate = date_to_consider,
    endDate = date_to_end,
    ageGroup = list(c(0,6),c(7,11),c(12,18),c(19,40),c(41,64),c(65,120))
  )
  if(cdm$denominator %>% tally() %>% pull() != 0) {
  for(i in outcome_id) {
    cdm$outcome <- cdm[[tableOutcome]] %>%
      dplyr::filter(.data$cohort_definition_id == i)
    if(cdm$outcome %>% tally() %>% pull() != 0) {
      inc <- IncidencePrevalence::estimateIncidence(
        cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
        interval = c("years","months","overall"),
        completeDatabaseIntervals = FALSE,  
         minCellCount = 5)

  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
    cdm=cdm, resultList=list(inc))
  if(name == "LC") {
    exportIncidencePrevalenceResults(
      result=study_results, zipName=paste0("Allpop_",names_cohorts[i],"_Age"), 
      outputFolder=output_ip) 
  }  else if (name == "PASC") {
    exportIncidencePrevalenceResults(
      result=study_results, zipName=paste0("Allpop_",names_lc[i],"_Age"), 
      outputFolder=output_ip) 
  } else if (name == "MC") {
    exportIncidencePrevalenceResults(
      result=study_results, zipName=paste0("Allpop_",names_pasc[i],"_Age"), 
      outputFolder=output_ip) 
  } else {
    exportIncidencePrevalenceResults(
      result=study_results, zipName=paste0("Allpop_",names_mc[i],"_Age"), 
      outputFolder=output_ip) 
  }
  
    }
  }
  }
}

# only outcomes
calculate_IP_allpop(c(1:27), as.Date("2020-09-01"), as.Date(latest_data_availability), LongCovidCohortsName, name = "LC")
info(logger, '--- Long Covid only symptoms on general population done')
if(!onlyLC) {
  calculate_IP_allpop(c(1:10), as.Date("2020-09-01"), as.Date(latest_data_availability), PascCohortsName, name = "PASC")
  info(logger, '--- PACS only events on general population done')
  calculate_IP_allpop(c(1:23), as.Date("2020-09-01"), as.Date(latest_data_availability), MCCohortsName, name = "MC")
  info(logger, '--- MC only conditions on general population done')
}

# base + one symptom or event or condition / any symptoms or events
if(!onlyLC) {
  calculate_IP_allpop(c(1:183), as.Date("2020-09-01"), as.Date(latest_data_availability), OverlapCohortsName)
  info(logger, '--- Long Covid, PACS and MC on general population done')
} else {
  calculate_IP_allpop(c(1:54), as.Date("2020-09-01"), as.Date(latest_data_availability), OverlapCohortsName)
  info(logger, '--- Long Covid on general population done')
}

