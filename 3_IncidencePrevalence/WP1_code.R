# WP1: Incidence and Prevalence

# Output folder for WP1
output_ip <- file.path(tempDir,"IP")
if (!file.exists(output_ip)){
  dir.create(output_ip, recursive = TRUE)}

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
  strataTable = BaseCohortsName,
  cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability)),
  strataCohortId = 1,
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
  strataTable = BaseCohortsName,
  strataCohortId = 1,
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
  strataTable = BaseCohortsName,
  cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability)),
  strataCohortId = 2,
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
  strataTable = BaseCohortsName,
  strataCohortId = 2,
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


if(!onlyLC) {
  message("Calculating IP for testneg")
  info(logger, "Calculating IP for testneg")
  
  message("- No strata and sex strata")
  # No strata and sex strata
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = BaseCohortsName,
    cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability)),
    strataCohortId = 3,
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
    strataTable = BaseCohortsName,
    strataCohortId = 3,
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

if(!onlyLC) {
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = OverlapCohortsTestneg_Name, 
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
  cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability)),
  ageGroup = list(c(0,6),c(7,11),c(12,18),c(19,40),c(41,64),c(65,150))
)

cdm$denominator <- cdm$denominator %>%
  PatientProfiles::addPriorHistory(cdm) %>%
  dplyr::filter(prior_history > 365) %>%
  dplyr::select(- "prior_history") %>%
  computeQuery()

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

if(!onlyLC)  {
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
bases <- cdm[[BaseCohortsName]] %>%
  PatientProfiles::addDemographics(cdm) %>%
  dplyr::mutate(cohort_start_date = as.Date(cohort_start_date)) %>%
  dplyr::mutate(cohort_end_date = as.Date(cohort_end_date)) %>%
  dplyr::collect()

result <- PatientProfiles::summariseResult(bases, strata = list("base_id" = "cohort_definition_id"))

write.csv(result, file = here::here(tempDir, "tableOne_bases.csv"))

cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm =  cdm,
  cohortDateRange = c(as.Date("2020-09-01"), as.Date(latest_data_availability))
)

cdm$denominator <- cdm$denominator %>%
  PatientProfiles::addPriorHistory(cdm) %>%
  dplyr::filter(prior_history > 365) %>%
  dplyr::select(- "prior_history") %>%
  computeQuery()

allpop <- cdm$denominator %>%
  PatientProfiles::addDemographics(cdm) %>%
  dplyr::mutate(cohort_start_date = as.Date(cohort_start_date)) %>%
  dplyr::mutate(cohort_end_date = as.Date(cohort_end_date)) %>%
  dplyr::collect()
  
result2 <- PatientProfiles::summariseResult(allpop)

write.csv(result2, file = here::here(tempDir, "tableOne_allpop.csv"))

