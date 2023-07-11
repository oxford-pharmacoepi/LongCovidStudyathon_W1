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
  computeQuery()

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
                      S_start_date = study_start_date)
if(!onlyLC && negative_init %>% dplyr::tally() %>% dplyr::pull() != 0) {
  nocovid <- do_exclusion(cdm, negative_init, id = 2,
                          S_start_date = study_start_date)
}

# New infection "final": inclusion/exclusion
new_infection <- covid[[1]]
new_infection <- new_infection %>% dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  computeQuery()


# Reinfection
reinfection <- covid[[2]]
reinfection <- reinfection %>% dplyr::mutate(cohort_definition_id = 2) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  computeQuery()


# Attritions
attrition_positive <- covid[[3]]
attrition_positive <- attrition_positive %>% dplyr::mutate(cohort_definition_id = 1) %>%
  computeQuery()


# Attritions for the censoring sub-part
attrition_censor_positive <- covid[[4]]
attrition_censor_positive <- attrition_censor_positive %>% dplyr::mutate(cohort_definition_id = 1) %>%
  computeQuery()


if(!onlyLC && negative_init %>% dplyr::tally() %>% dplyr::pull() != 0) {
  # Tested negative "final"
  negativetest <- nocovid[[1]]
  negativetest <- negativetest %>% dplyr::mutate(cohort_definition_id = 3) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    computeQuery()
  
  attrition_negative <- nocovid[[3]]
  attrition_negative <- attrition_negative %>% dplyr::mutate(cohort_definition_id = 3) %>%
    computeQuery()
  
  attrition_censor_negative <- nocovid[[4]]
  attrition_censor_negative <- attrition_censor_negative %>% dplyr::mutate(cohort_definition_id = 3) %>%
    computeQuery()
  
  attrition <- rbind(attrition_positive,attrition_negative)
  attrition_censor <- rbind(attrition_censor_positive,attrition_censor_negative)
  bases <- dplyr::union_all(new_infection, reinfection) %>%
    computeQuery()
  bases <- dplyr::union_all(bases, negativetest) %>%
    computeQuery()
  
} else {
  attrition <-attrition_positive
  attrition_censor <-attrition_censor_positive
  bases <- dplyr::union_all(new_infection, reinfection) %>%
    computeQuery()
  
}

write.csv(
  attrition,
  file = here::here(output_at, "attrition_base.csv")
)
write.csv(
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
attr(bases, "cohort_set") <- names_final_cohorts %>% 
  dplyr::filter(table_name == BaseCohortsName) %>%
  dplyr::select(cohort_definition_id, cohort_name) 

attr(bases, "cohort_count") <- getCohortCount(bases)

cdm[[BaseCohortsName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(bases, BaseCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = insertTable(attr(bases, "cohort_set"), cdm, paste0(BaseCohortsName, "_set")),
  cohortCountRef = insertTable(attr(bases, "cohort_count"), cdm, paste0(BaseCohortsName, "_count"))
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

lc_final <- dplyr::union_all(table_lc, table_lccode) %>%
  computeQuery()


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
  
  if(!onlyLC) {
    cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                      cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                       PascCohortsName,MCCohortsName))
  } else {
    cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                      cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName
                                       ))
  }

}

# --------------------------------------------------------------------
# OVERLAPING COHORTS

message("Getting overlap inf cohorts")
info(logger, '-- Getting overlap inf cohorts')

overlap_cohorts <- overlap_per_base(1, "inf", OverlapCohortsInfName)

if(!onlyLC) {
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = OverlapCohortsInfName,
                                             cohort_definition_id = c(1:48), 
                                             cohort_name =c(paste0("inf_",Initial_cohorts$cohort_name[(5:29)]),"inf_lc_code","inf_any_lc",
                                                            paste0("inf_",Initial_cohorts$cohort_name[(30:39)]),"inf_any_pasc",
                                                            paste0("inf_",Initial_cohorts$cohort_name[(40:49)]))))
} else {
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = OverlapCohortsInfName,
                                             cohort_definition_id = c(1:27), 
                                             cohort_name =c(paste0("inf_",Initial_cohorts$cohort_name[(5:29)]),"inf_lc_code","inf_any_lc")))
}

# Save attributes of the cohort
attr(overlap_cohorts, "cohort_set") <- names_final_cohorts %>% 
  dplyr::filter(table_name == OverlapCohortsInfName) %>%
  dplyr::select(cohort_definition_id, cohort_name) 

attr(overlap_cohorts, "cohort_count") <- getCohortCount(overlap_cohorts)

cdm[[OverlapCohortsInfName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(overlap_cohorts, OverlapCohortsInfName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = insertTable(attr(overlap_cohorts, "cohort_set"), cdm, paste0(OverlapCohortsInfName, "_set")),
  cohortCountRef = insertTable(attr(overlap_cohorts, "cohort_count"), cdm, paste0(OverlapCohortsInfName, "_count"))
)

message("Getting overlap reinf cohorts")
info(logger, '-- Getting overlap reinf cohorts')

overlap_cohorts <- overlap_per_base(2, "reinf", OverlapCohortsReinfName)

if(!onlyLC) {
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = OverlapCohortsReinfName,
                                             cohort_definition_id = c(1:48), 
                                             cohort_name =c(paste0("reinf_",Initial_cohorts$cohort_name[(5:29)]),"reinf_lc_code","reinf_any_lc",
                                                            paste0("reinf_",Initial_cohorts$cohort_name[(30:39)]),"reinf_any_pasc",
                                                            paste0("reinf_",Initial_cohorts$cohort_name[(40:49)]))))
} else {
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = OverlapCohortsReinfName,
                                             cohort_definition_id = c(1:27), 
                                             cohort_name =c(paste0("reinf_",Initial_cohorts$cohort_name[(5:29)]),"reinf_lc_code","reinf_any_lc")))
}

# Save attributes of the cohort
attr(overlap_cohorts, "cohort_set") <- names_final_cohorts %>% 
  dplyr::filter(table_name == OverlapCohortsReinfName) %>%
  dplyr::select(cohort_definition_id, cohort_name) 

attr(overlap_cohorts, "cohort_count") <- getCohortCount(overlap_cohorts)

cdm[[OverlapCohortsReinfName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(overlap_cohorts, OverlapCohortsReinfName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = insertTable(attr(overlap_cohorts, "cohort_set"), cdm, paste0(OverlapCohortsReinfName, "_set")),
  cohortCountRef = insertTable(attr(overlap_cohorts, "cohort_count"), cdm, paste0(OverlapCohortsReinfName, "_count"))
)

if(!onlyLC) {
  message("Getting overlap testneg cohorts")
  info(logger, '-- Getting overlap testneg cohorts')
  
  overlap_cohorts <- overlap_per_base(3, "testneg", OverlapCohortsTestnegName)
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = OverlapCohortsTestnegName,
                                             cohort_definition_id = c(1:48), 
                                             cohort_name =c(paste0("testneg_",Initial_cohorts$cohort_name[(5:29)]),"testneg_lc_code","testneg_any_lc",
                                                            paste0("testneg_",Initial_cohorts$cohort_name[(30:39)]),"testneg_any_pasc",
                                                            paste0("testneg_",Initial_cohorts$cohort_name[(40:49)]))))
  
  # Save attributes of the cohort
  attr(overlap_cohorts, "cohort_set") <- names_final_cohorts %>% 
    dplyr::filter(table_name == OverlapCohortsTestnegName) %>%
    dplyr::select(cohort_definition_id, cohort_name) 
  
  attr(overlap_cohorts, "cohort_count") <- getCohortCount(overlap_cohorts)
  
  cdm[[OverlapCohortsTestnegName]] <- newGeneratedCohortSet(
    cohortRef = computeQuery(overlap_cohorts, OverlapCohortsTestnegName, FALSE, attr(cdm, "write_schema"), TRUE),
    cohortSetRef = insertTable(attr(overlap_cohorts, "cohort_set"), cdm, paste0(OverlapCohortsTestnegName, "_set")),
    cohortCountRef = insertTable(attr(overlap_cohorts, "cohort_count"), cdm, paste0(OverlapCohortsTestnegName, "_count"))
  )
}

if(!onlyLC) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MCCohortsName,OverlapCohortsInfName,
                                     OverlapCohortsReinfName, OverlapCohortsTestnegName))
} else {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     OverlapCohortsInfName, OverlapCohortsReinfName))
}

# --------------------------------------------------------------------

# Change cohort_start_dates of base cohorts to 90 days after index date
bases <- cdm[[BaseCohortsName]] %>%
  dplyr::mutate(cohort_start_date = CDMConnector::dateadd("cohort_start_date", 90)) %>%
  computeQuery()

attr(bases, "cohort_set") <- names_final_cohorts %>% 
  dplyr::filter(table_name == BaseCohortsName) %>%
  dplyr::select(cohort_definition_id, cohort_name) 

attr(bases, "cohort_count") <- getCohortCount(bases)

cdm[[BaseCohortsName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(bases, BaseCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = insertTable(attr(bases, "cohort_set"), cdm, paste0(BaseCohortsName, "_set")),
  cohortCountRef = insertTable(attr(bases, "cohort_count"), cdm, paste0(BaseCohortsName, "_count"))
)

# ----------------------------------------------------------------------
# Print counts of all cohorts (if >5) 

message("Getting cohort counts")
info(logger, '-- Getting cohort counts')

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
write.csv(finalCounts,
          file = file.path(tempDir,
                           paste0(db.name,"_finalcounts.csv")
          )
)

write.csv(names_final_cohorts,
          file = file.path(tempDir,
                           paste0(db.name,"_cohorts.csv")
          )
)
