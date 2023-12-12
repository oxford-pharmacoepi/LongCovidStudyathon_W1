# WP1: Incidence and Prevalence

# Output folder for WP1
output_ip <- file.path(tempDir,"IP")
if (!file.exists(output_ip)){
  dir.create(output_ip, recursive = TRUE)}

  cdm <- cdmFromCon(db, cdm_database_schema,  writeSchema = c(schema = results_database_schema,
                                                              prefix = table_stem),
                    cohortTables = CohortNames)


# ----------------------------------------------------------------
# 1a: LC on base cohorts
info(logger, '-- Calculating incidence and prevalence for outcomes in base cohorts')

names_final_cohorts <- read.csv(here::here(tempDir,paste0(db.name,"_cohorts.csv")))

message("Calculating IP for inf")
info(logger, "Calculating IP for inf")

message("- No strata and sex strata")
# No strata and sex strata
cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  targetCohortTable = BaseCohortsName,
  cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability)),
  targetCohortId = 1,
  sex = c("Male", "Female", "Both")
)

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsInfName, 
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE, minCellCount = 5)

write.csv(inc, file = here::here(output_ip, paste0("Inf_AllandSex.csv")))

attr(inc, "attrition") <- attr(inc, "attrition") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Inf_AllandSex_attrition.csv")))

message("- Age strata")
# Age strata
cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  targetCohortTable = BaseCohortsName,
  targetCohortId = 1,
  cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability)),
  ageGroup = list(c(0,6),c(7,11),c(12,18),c(19,40),c(41,64),c(65,150))
)

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsInfName, 
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE,  
  minCellCount = 5)

write.csv(inc, file = here::here(output_ip, paste0("Inf_Age.csv")))

attr(inc, "attrition") <- attr(inc, "attrition") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Inf_Age_attrition.csv")))


message("Calculating IP for reinf")
info(logger, "Calculating IP for reinf")

message("- No strata and sex strata")
# No strata and sex strata
cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  targetCohortTable = BaseCohortsName,
  cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability)),
  targetCohortId = 2,
  sex = c("Male", "Female", "Both")
)

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsReinfName, 
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE, minCellCount = 5)

write.csv(inc, file = here::here(output_ip, paste0("Reinf_AllandSex.csv")))

attr(inc, "attrition") <- attr(inc, "attrition") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Reinf_AllandSex_attrition.csv")))

message("- Age strata")
# Age strata
cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  targetCohortTable = BaseCohortsName,
  targetCohortId = 2,
  cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability)),
  ageGroup = list(c(0,6),c(7,11),c(12,18),c(19,40),c(41,64),c(65,150))
)

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsReinfName, 
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE,  
  minCellCount = 5)

write.csv(inc, file = here::here(output_ip, paste0("Reinf_Age.csv")))

attr(inc, "attrition") <- attr(inc, "attrition") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Reinf_Age_attrition.csv")))


if(!onlyLC && !noTestNeg) {
  message("Calculating IP for testneg")
  info(logger, "Calculating IP for testneg")
  
  message("- No strata and sex strata")
  # No strata and sex strata
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    name = "denominator",
    targetCohortTable = BaseCohortsName,
    cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability)),
    targetCohortId = 3,
    sex = c("Male", "Female", "Both")
  )
  
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsTestnegName, 
    interval = c("years","months","overall"),
    completeDatabaseIntervals = FALSE, minCellCount = 5)
  
  write.csv(inc, file = here::here(output_ip, paste0("Testneg_AllandSex.csv")))
  
  attr(inc, "attrition") <- attr(inc, "attrition") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
  write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Testneg_AllandSex_attrition.csv")))
  
  message("- Age strata")
  # Age strata
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    name = "denominator",
    targetCohortTable = BaseCohortsName,
    targetCohortId = 3,
    cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability)),
    ageGroup = list(c(0,6),c(7,11),c(12,18),c(19,40),c(41,64),c(65,150))
  )
  
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsTestnegName, 
    interval = c("years","months","overall"),
    completeDatabaseIntervals = FALSE,  
    minCellCount = 5)
  
  write.csv(inc, file = here::here(output_ip, paste0("Testneg_Age.csv")))
  
  attr(inc, "attrition") <- attr(inc, "attrition") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
  write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Testneg_Age_attrition.csv")))
}

# ----------------------------------------------------------------
# 1b: LC on source population

info(logger, '-- Calculating incidence and prevalence for outcomes and base cohorts in source population, sex strata')

message("- No strata and sex strata")
# No strata and sex strata
cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability)),
  daysPriorHistory = 365,
  sex = c("Male", "Female", "Both")
)

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = LongCovidCohortsName, 
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE,  
  minCellCount = 5)

write.csv(inc, file = here::here(output_ip, paste0("Allpop_LC_AllandSex.csv")))

attr(inc, "attrition") <- attr(inc, "attrition") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Allpop_LC_AllandSex_attrition.csv")))

if(!onlyLC) {
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = PascCohortsName, 
    interval = c("years","months","overall"),
    completeDatabaseIntervals = FALSE,  
    minCellCount = 5)
  
  write.csv(inc, file = here::here(output_ip, paste0("Allpop_Pasc_AllandSex.csv")))
  
  attr(inc, "attrition") <- attr(inc, "attrition") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
  write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Allpop_Pasc_AllandSex_attrition.csv")))
  
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = MCCohortsName, 
    interval = c("years","months","overall"),
    completeDatabaseIntervals = FALSE,  
    minCellCount = 5)
  
  write.csv(inc, file = here::here(output_ip, paste0("Allpop_MC_AllandSex.csv")))
  
  attr(inc, "attrition") <- attr(inc, "attrition") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
  write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Allpop_MC_AllandSex_attrition.csv")))
}

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsInfName, 
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE,  
  minCellCount = 5)

write.csv(inc, file = here::here(output_ip, paste0("Allpop_inf_AllandSex.csv")))

attr(inc, "attrition") <- attr(inc, "attrition") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Allpop_inf_AllandSex_attrition.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsReinfName, 
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE,  
  minCellCount = 5)

write.csv(inc, file = here::here(output_ip, paste0("Allpop_reinf_AllandSex.csv")))

attr(inc, "attrition") <- attr(inc, "attrition") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Allpop_reinf_AllandSex_attrition.csv")))

if(!onlyLC && !noTestNeg) {
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsTestnegName, 
    interval = c("years","months","overall"),
    completeDatabaseIntervals = FALSE,  
    minCellCount = 5)
  
  write.csv(inc, file = here::here(output_ip, paste0("Allpop_testneg_AllandSex.csv")))
  
  attr(inc, "attrition") <- attr(inc, "attrition") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
  write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Allpop_testneg_AllandSex_attrition.csv")))
}

info(logger, '-- Calculating incidence and prevalence for outcomes and base cohorts in source population, age strata')

message("- Age strata")
# Age strata
cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability)),
  daysPriorHistory = 365,
  ageGroup = list(c(0,6),c(7,11),c(12,18),c(19,40),c(41,64),c(65,150))
)

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = LongCovidCohortsName, 
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE,  
  minCellCount = 5)

write.csv(inc, file = here::here(output_ip, paste0("Allpop_LC_Age.csv")))

attr(inc, "attrition") <- attr(inc, "attrition") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Allpop_LC_Age_attrition.csv")))

if(!onlyLC) {
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = PascCohortsName, 
    interval = c("years","months","overall"),
    completeDatabaseIntervals = FALSE,  
    minCellCount = 5)
  
  write.csv(inc, file = here::here(output_ip, paste0("Allpop_Pasc_Age.csv")))
  
  attr(inc, "attrition") <- attr(inc, "attrition") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
  write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Allpop_Pasc_Age_attrition.csv")))
  
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = MCCohortsName, 
    interval = c("years","months","overall"),
    completeDatabaseIntervals = FALSE,  
    minCellCount = 5)
  
  write.csv(inc, file = here::here(output_ip, paste0("Allpop_MC_Age.csv")))
  
  attr(inc, "attrition") <- attr(inc, "attrition") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
  write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Allpop_MC_Age_attrition.csv")))
}

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsInfName, 
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE,  
  minCellCount = 5)

write.csv(inc, file = here::here(output_ip, paste0("Allpop_inf_Age.csv")))

attr(inc, "attrition") <- attr(inc, "attrition") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Allpop_inf_Age_attrition.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsReinfName, 
  interval = c("years","months","overall"),
  completeDatabaseIntervals = FALSE,  
  minCellCount = 5)

write.csv(inc, file = here::here(output_ip, paste0("Allpop_reinf_Age.csv")))

attr(inc, "attrition") <- attr(inc, "attrition") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Allpop_reinf_Age_attrition.csv")))

if(!onlyLC && !noTestNeg)  {
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsTestnegName, 
    interval = c("years","months","overall"),
    completeDatabaseIntervals = FALSE,  
    minCellCount = 5)
  
  write.csv(inc, file = here::here(output_ip, paste0("Allpop_testneg_Age.csv")))
  
  attr(inc, "attrition") <- attr(inc, "attrition") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("number") | dplyr::starts_with("excluded"), ~ dplyr::if_else(.x < 5, NA, .x)))
  write.csv(attr(inc, "attrition"), file = here::here(output_ip, paste0("Allpop_testneg_Age_attrition.csv")))
}

info(logger, '-- Getting information of base cohorts for Table One')

# Get information of the base cohorts for Table One
if(sql_dem) {
  bases <- cdm[[BaseCohortsName]] %>%
    dplyr::mutate(cohort_start_date = CDMConnector::dateadd("cohort_start_date", -90)) %>%
    dplyr::mutate(cohort_start_date = as.Date(cohort_start_date)) %>%
    dplyr::mutate(cohort_end_date = as.Date(cohort_end_date)) %>%
    PatientProfiles::addDemographics_sql(cdm) %>%
    dplyr::relocate("sex", .after = last_col()) %>%
    dplyr::collect()
} else {
  bases <- cdm[[BaseCohortsName]] %>%
    dplyr::mutate(cohort_start_date = CDMConnector::dateadd("cohort_start_date", -90)) %>%
    dplyr::mutate(cohort_start_date = as.Date(cohort_start_date)) %>%
    dplyr::mutate(cohort_end_date = as.Date(cohort_end_date)) %>%
    PatientProfiles::addDemographics(cdm) %>%
    dplyr::relocate("sex", .after = last_col()) %>%
    dplyr::collect()
}


result <- PatientProfiles::summariseResult(
  bases, 
  strata = list("base_id" = "cohort_definition_id"),
  functions = list(
    numericVariables = c("mean","median", "q25", "q75","sd", "iqr","min","max"),
    dateVariables = c("mean","median", "q25", "q75","min","max"),
    binaryVariables = c("count", "percentage"),
    categoricalVariables = c("count", "percentage")
  ))

write.csv(result, file = here::here(tempDir, "tableOne_bases.csv"))

cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  daysPriorObservation = 365,
  cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability))
)

if(sql_dem) {
  allpop <- cdm$denominator %>%
    PatientProfiles::addDemographics_sql(cdm) %>%
    dplyr::mutate(cohort_start_date = as.Date(cohort_start_date)) %>%
    dplyr::mutate(cohort_end_date = as.Date(cohort_end_date)) %>%
    dplyr::relocate("sex", .after = last_col()) %>%
    dplyr::collect()
} else {
  allpop <- cdm$denominator %>%
    PatientProfiles::addDemographics(cdm) %>%
    dplyr::mutate(cohort_start_date = as.Date(cohort_start_date)) %>%
    dplyr::mutate(cohort_end_date = as.Date(cohort_end_date)) %>%
    dplyr::relocate("sex", .after = last_col()) %>%
    dplyr::collect()
}
  
result2 <- PatientProfiles::summariseResult(allpop)

write.csv(result2, file = here::here(tempDir, "tableOne_allpop.csv"))

