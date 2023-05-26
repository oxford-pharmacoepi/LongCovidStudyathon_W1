# From initial cohorts, get base and outcome cohorts for the study
# Get attrition too

# get functions used throughout this script
source(here("2_StudyCohorts","functions_getCohorts.R"))

# observation period + death table
observation_death <- cdm$observation_period %>%
  dplyr::select("subject_id" = "person_id", "observation_period_end_date") %>%
  left_join(cdm$death %>% dplyr::select("subject_id" = "person_id", "death_date"),
            by = "subject_id") %>%
  mutate(death = ifelse(!(is.na(death_date)), 1,0)) %>%
  compute()

# Output folder for Attrition
output_at <- file.path(tempDir,"Attrition")
if (!file.exists(output_at)){
  dir.create(output_at, recursive = TRUE)}

# ---------------------------------------------------------------------
# BASE COHORTS

message("Getting base cohorts")
info(logger, '-- Getting base cohorts')

# Get initial cohorts to build the study cohorts
newinf_init <- cdm[[InitialCohortsName]] %>% 
  dplyr::filter(.data$cohort_definition_id == 1) %>% dplyr::select(
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %>% computeQuery() 

negative_init <- cdm[[InitialCohortsName]] %>% 
  dplyr::filter(.data$cohort_definition_id == 2) %>% dplyr::select(
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %>% computeQuery() 

covid <- do_exclusion(cdm, newinf_init, id = 1,
                      S_start_date = study_start_date, covidcensor = FALSE)
if(!onlyLC) {
  nocovid <- do_exclusion(cdm, negative_init, id = 2,
                          S_start_date = study_start_date)
}
  
# New infection "final": inclusion/exclusion
new_infection <- covid[[1]]
new_infection <- new_infection %>% dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  compute()

# Reinfection
reinfection <- covid[[2]]
reinfection <- reinfection %>% dplyr::mutate(cohort_definition_id = 2) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  compute()

# Attritions
attrition_positive <- covid[[3]]
attrition_positive <- attrition_positive %>% dplyr::mutate(cohort_definition_id = 1) %>%
  compute()

# Attritions for the censoring sub-part
attrition_censor_positive <- covid[[4]]
attrition_censor_positive <- attrition_censor_positive %>% dplyr::mutate(cohort_definition_id = 1) %>%
  compute()

if(!onlyLC) {
  # Tested negative "final"
  negativetest <- nocovid[[1]]
  negativetest <- negativetest %>% dplyr::mutate(cohort_definition_id = 3) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    compute()
  attrition_negative <- nocovid[[3]]
  attrition_negative <- attrition_negative %>% dplyr::mutate(cohort_definition_id = 3) %>%
    compute()
  attrition_censor_negative <- nocovid[[4]]
  attrition_censor_negative <- attrition_censor_negative %>% dplyr::mutate(cohort_definition_id = 3) %>%
    compute()
  attrition <- rbind(attrition_positive,attrition_negative)
  attrition_censor <- rbind(attrition_censor_positive,attrition_censor_negative)
  bases <- dplyr::union_all(new_infection, reinfection)
  bases <- dplyr::union_all(bases, negativetest)
} else {
  attrition <-attrition_positive
  attrition_censor <-attrition_censor_positive
  bases <- dplyr::union_all(new_infection, reinfection)
}

write_csv(
  attrition,
  file = here::here(output_at, "attrition_base.csv")
)
write_csv(
  attrition_censor,
  file = here::here(output_at, "attrition_base_censoring.csv")
)

if(!onlyLC) {
  names_final_cohorts <- dplyr::tibble(table_name = BaseCohortsName,
                                       cohort_definition_id = c(1:3),
                                       cohort_name = c("infection","reinfection","test_negative"))
} else {
  names_final_cohorts <- dplyr::tibble(table_name = BaseCohortsName,
                                       cohort_definition_id = c(1:2),
                                       cohort_name = c("infection","reinfection"))
}

# Save attributes of the cohort
attr(new_infection, "cohort_set") <- names_final_cohorts %>% 
  dplyr::filter(table_name == BaseCohortsName) %>%
  dplyr::select(cohort_definition_id, cohort_name) 

attr(new_infection, "cohort_count") <- getCohortCount(new_infection)

cdm[[BaseCohortsName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(new_infection, BaseCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = insertTable(attr(new_infection, "cohort_set"), cdm, paste0(BaseCohortsName, "_set")),
  cohortCountRef = insertTable(attr(new_infection, "cohort_count"), cdm, paste0(BaseCohortsName, "_count"))
)

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c(InitialCohortsName,BaseCohortsName))

# ---------------------------------------------------------------------
# OUTCOME COHORTS

message("Getting outcome cohorts")
info(logger, '-- Getting outcome cohorts')

# Long covid symptoms
table_lc <- create_outcome(cdm, window = c(5:29), new_ids = c(1:25))

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = LongCovidCohortsName,
                                           cohort_definition_id = c(1:25),
                                           cohort_name = Initial_cohorts$cohort_name[5:29]))

# Any LC symptom
table_lc <- create_any_cohort(cdm, c(1:25), cohort_id = 26, tableold = table_lc, name = "lc")

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = LongCovidCohortsName,
                                           cohort_definition_id = 26, cohort_name = "any_lc_symptom"))

# LC code
table_lccode <- create_outcome(cdm, window = 4, new_ids = 27)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = LongCovidCohortsName,
                                           cohort_definition_id = 27, cohort_name = "lc_code"))

lc_final <- dplyr::union_all(table_lc, table_lccode)

# Save attributes of the cohort
attr(lc_final, "cohort_set") <- names_final_cohorts %>% 
  dplyr::filter(table_name == LongCovidCohortsName) %>%
  dplyr::select(cohort_definition_id, cohort_name) 

attr(lc_final, "cohort_count") <- getCohortCount(lc_final)

cdm[[LongCovidCohortsName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(lc_final, LongCovidCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = insertTable(attr(lc_final, "cohort_set"), cdm, paste0(LongCovidCohortsName, "_set")),
  cohortCountRef = insertTable(attr(lc_final, "cohort_count"), cdm, paste0(LongCovidCohortsName, "_count"))
)

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName))

if(!onlyLC) {
  # PASC events
  table_pasc <- create_outcome(cdm, window = c(30:39), new_ids = c(1:10))
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = PascCohortsName,
                                             cohort_definition_id = c(1:10),
                                             cohort_name = Initial_cohorts$cohort_name[30:39]))
  
  # Any PASC event
  table_pasc <- create_any_cohort(cdm, c(1:10), cohort_id = 11, tableold = table_pasc, name = "pasc")
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = PascCohortsName,
                                             cohort_definition_id = 11, cohort_name = "any_pasc_event"))
  
  # Save attributes of the cohort
  attr(table_pasc, "cohort_set") <- names_final_cohorts %>% 
    dplyr::filter(table_name == PascCohortsName) %>%
    dplyr::select(cohort_definition_id, cohort_name) 
  
  attr(table_pasc, "cohort_count") <- getCohortCount(table_pasc)
  
  cdm[[PascCohortsName]] <- newGeneratedCohortSet(
    cohortRef = computeQuery(table_pasc, PascCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
    cohortSetRef = insertTable(attr(table_pasc, "cohort_set"), cdm, paste0(PascCohortsName, "_set")),
    cohortCountRef = insertTable(attr(table_pasc, "cohort_count"), cdm, paste0(PascCohortsName, "_count"))
  )
  
  # Medical conditions
  table_mc <- create_outcome(cdm, window = c(40:49), new_ids = c(1:10)) 
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = MCCohortsName,
                                             cohort_definition_id = c(1:10),
                                             cohort_name = Initial_cohorts$cohort_name[40:49]))
  

  # Save attributes of the cohort
  attr(table_mc, "cohort_set") <- names_final_cohorts %>% 
    dplyr::filter(table_name == MCCohortsName) %>%
    dplyr::select(cohort_definition_id, cohort_name) 
  
  attr(table_mc, "cohort_count") <- getCohortCount(table_mc)
  
  cdm[[MCCohortsName]] <- newGeneratedCohortSet(
    cohortRef = computeQuery(table_mc, MCCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
    cohortSetRef = insertTable(attr(table_mc, "cohort_set"), cdm, paste0(MCCohortsName, "_set")),
    cohortCountRef = insertTable(attr(table_mc, "cohort_count"), cdm, paste0(MCCohortsName, "_count"))
  )
  
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MCCohortsName))
}

# --------------------------------------------------------------------
# OVERLAPING COHORTS

# Infection/Reinfection/Test Negative + 1 LC symptom / LC code
overlap_cohorts <- list()
bases <- c("inf", "reinf", "testneg")
if(!onlyLC) {
  for(b in c(1:3)) {
    for(i in c(1:25)) {
      names_final_cohorts <- rbind(names_final_cohorts,
                                   dplyr::tibble(table_name = OverlapCohortsName,
                                                 cohort_definition_id = 26*(b-1) + i, 
                                                 cohort_name =paste0(bases[b],"_",Initial_cohorts$cohort_name[i+4])))
      if(cdm[[LongCovidCohortsName]] %>% 
         dplyr::filter(cohort_definition_id == i) %>% tally() %>% pull() > 5) {
        overlap_cohorts[[26*(b-1) + i]] <- do_overlap(cdm, b, i, 26*(b-1) + i, tableName = LongCovidCohortsName)
      }
    }
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = OverlapCohortsName,
                                               cohort_definition_id = 26*(b-1) + 26, 
                                               cohort_name =paste0(bases[b],"_lc_code")))
    if(cdm[[LongCovidCohortsName]] %>% 
       dplyr::filter(cohort_definition_id == 27) %>% tally() %>% pull() > 5) {
      overlap_cohorts[[26*(b-1) + 26]] <- do_overlap(cdm, b, 27, 26*(b-1) + 26, tableName = LongCovidCohortsName)
    }
  }
  overlap_cohorts <- bind_rows(overlap_cohorts)
} else {
  for(b in c(1:2)) {
    for(i in c(1:25)) {
      names_final_cohorts <- rbind(names_final_cohorts,
                                   dplyr::tibble(table_name = OverlapCohortsName,
                                                 cohort_definition_id = 26*(b-1) + i, 
                                                 cohort_name =paste0(bases[b],"_",Initial_cohorts$cohort_name[i+4])))
      if(cdm[[LongCovidCohortsName]] %>% 
         dplyr::filter(cohort_definition_id == i) %>% tally() %>% pull() > 5) {
        overlap_cohorts[[26*(b-1) + i]] <- do_overlap(cdm, b, i, 26*(b-1) + i, tableName = LongCovidCohortsName)
      }
    }
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = OverlapCohortsName,
                                               cohort_definition_id = 26*(b-1) + 26, 
                                               cohort_name =paste0(bases[b],"_lc_code")))
    if(cdm[[LongCovidCohortsName]] %>% 
       dplyr::filter(cohort_definition_id == 27) %>% tally() %>% pull() > 5) {
      overlap_cohorts[[26*(b-1) + 26]] <- do_overlap(cdm, b, 27, 26*(b-1) + 26, tableName = LongCovidCohortsName)
    }
  }
  
  overlap_cohorts <- bind_rows(overlap_cohorts)
}

# Infection/Reinfection/Test Negative + any symptom
if(!onlyLC) {
  for(b in c(1:3)) {
    overlap_all_lc <- overlap_cohorts %>%
      dplyr::filter(cohort_definition_id %in% c((26*(b-1) + 1):(26*(b-1) + 25))) %>%
      dplyr::mutate(cohort_definition_id == 78 + b) %>%
      dplyr::group_by(subject_id) %>%
      dplyr::arrange(cohort_start_date) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      computeQuery()
    overlap_cohorts <- dplyr::union_all(
      overlap_cohorts,
      overlap_all_lc
    )
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = OverlapCohortsName,
                                               cohort_definition_id = 78 + b, 
                                               cohort_name =paste0(bases[b],"_any_lc_symptom")))
  }
} else {
  for(b in c(1:2)) {
    overlap_all_lc <- overlap_cohorts %>%
      dplyr::filter(cohort_definition_id %in% c((26*(b-1) + 1):(26*(b-1) + 25))) %>%
      dplyr::mutate(cohort_definition_id == 52 + b) %>%
      dplyr::group_by(subject_id) %>%
      dplyr::arrange(cohort_start_date) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      computeQuery()
    overlap_cohorts <- dplyr::union_all(
      overlap_cohorts,
      overlap_all_lc
    )
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = OverlapCohortsName,
                                               cohort_definition_id = 52 + b, 
                                               cohort_name =paste0(bases[b],"_any_lc_symptom")))
  }
}

if(!onlyLC) {
  # Infection/Reinfection/Test Negative + 1 PASC event
  overlap_cohorts_pasc <- list()
  for(b in c(1:3)) {
    for(i in c(1:10)) {
      names_final_cohorts <- rbind(names_final_cohorts,
                                   dplyr::tibble(table_name = OverlapCohortsName,
                                                 cohort_definition_id = 81 + 10*(b-1) + i, 
                                                 cohort_name =paste0(bases[b],"_",Initial_cohorts$cohort_name[i+29])))
      if(cdm[[PascCohortsName]] %>% 
         dplyr::filter(cohort_definition_id == i) %>% tally() %>% pull() > 5) {
        overlap_cohorts_pasc[[10*(b-1) + i]] <- do_overlap(cdm, b, i, 81 + 10*(b-1) + i, tableName = PascCohortsName)
      }
    }
  }
  
  overlap_cohorts_pasc <- bind_rows(overlap_cohorts_pasc)
  
  # Infection/Reinfection/Test Negative + any PASC event
  for(b in c(1:3)) {
    overlap_all_pasc <- overlap_cohorts_pasc %>%
      dplyr::filter(cohort_definition_id %in% c((81 + 10*(b-1) + 1):(81 + 10*(b-1) + 10))) %>%
      dplyr::mutate(cohort_definition_id == 81 + 30 + b) %>%
      dplyr::group_by(subject_id) %>%
      dplyr::arrange(cohort_start_date) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      computeQuery()
    overlap_cohorts_pasc <- dplyr::union_all(
      overlap_cohorts_pasc,
      overlap_all_pasc
    )
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = OverlapCohortsName,
                                               cohort_definition_id = 81 + 30 + b, 
                                               cohort_name =paste0(bases[b],"_any_pasc_event")))
  }
  
  # Infection/Reinfection/Test Negative + 1 Medical condition
  overlap_cohorts_mc <- list()
  for(b in c(1:3)) {
    for(i in c(1:23)) {
      names_final_cohorts <- rbind(names_final_cohorts,
                                   dplyr::tibble(table_name = OverlapCohortsName,
                                                 cohort_definition_id = 81 + 33 + 10*(b-1) + i, 
                                                 cohort_name =paste0(bases[b],"_",Initial_cohorts$cohort_name[i+39])))
      if(cdm[[MCCohortsName]] %>% 
         dplyr::filter(cohort_definition_id == i) %>% tally() %>% pull() > 5) {
        overlap_cohorts_mc[[10*(b-1) + i]] <- do_overlap(cdm, b, i, 81 + 33 + 10*(b-1) + i, tableName = MCCohortsName)
      }
    }
  }
  
  overlap_cohorts_mc <- bind_rows(overlap_cohorts_mc)
  
  overlap_all <- dplyr::union_all(
    overlap_cohorts,
    overlap_cohorts_pasc
  )
  
  overlap_all <- dplyr::union_all(
    overlap_all,
    overlap_cohorts_mc
  )
} else {
  overlap_all <- overlap_cohorts
}

# Save attributes of the cohort
attr(overlap_all, "cohort_set") <- names_final_cohorts %>% 
  dplyr::filter(table_name == OverlapCohortsName) %>%
  dplyr::select(cohort_definition_id, cohort_name) 

attr(overlap_all, "cohort_count") <- getCohortCount(overlap_all)

cdm[[MCCohortsName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(overlap_all, OverlapCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = insertTable(attr(overlap_all, "cohort_set"), cdm, paste0(OverlapCohortsName, "_set")),
  cohortCountRef = insertTable(attr(overlap_all, "cohort_count"), cdm, paste0(OverlapCohortsName, "_count"))
)

if(!onlyLC) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MCCohortsName,OverlapCohortsName))
} else {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     OverlapCohortsName))
}

# --------------------------------------------------------------------
# Print counts of all cohorts (if >5) 

finalCounts <- tibble::tibble(cohort_name = "start", n = 0)

for(name in CohortNames) {
  if(name %in% names(cdm)) {
    finalCounts <- finalCounts %>%
      dplyr::union(cdm[[name]] %>% 
      dplyr::group_by(cohort_definition_id) %>% 
      tally() %>% 
      collect() %>% 
      right_join(names_final_cohorts %>% dplyr::filter(table_name == name), 
                 by = c("cohort_definition_id")) %>% 
      dplyr::mutate(n = as.numeric(n)) %>% 
      dplyr::mutate(n = if_else(is.na(n), 0, n)) %>%
      dplyr::mutate(n = ifelse(n <= 5, NA, n)) %>% 
      dplyr::select(cohort_name, n))
  }
}

finalCounts <- finalCounts[-1,]

# Export csv
write_csv(finalCounts,
          file = file.path(tempDir,
            paste0(db.name,"_finalcounts.csv")
          )
)

write_csv(names_final_cohorts,
          file = file.path(tempDir,
                           paste0(db.name,"_cohorts.csv")
          )
)
