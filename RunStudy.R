# Create zip file
zipName <- paste0(db.name,"_Results")
tempDir <- zipName
tempDirCreated <- FALSE
if (!dir.exists(tempDir)) {
  dir.create(tempDir)
  tempDirCreated <- TRUE
}

start <- Sys.time()

# Start log
log_file <- paste0(tempDir, "/log.txt")
# Think what to do if already there. Overwrite, append?
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# Create table names to use throughout the study
InitialCohortsName <- paste0(table_stem,"_initialcohorts")
BaseCohortsName <- paste0(table_stem,"_basecohorts")
LongCovidCohortsName <- paste0(table_stem,"_lccohorts")
PascCohortsName <- paste0(table_stem,"_pasccohorts")
MCCohortsName <- paste0(table_stem,"_mccohorts")
OverlapCohortsInfName <- paste0(table_stem,"_overlapinfcohorts")
OverlapCohortsReinfName <- paste0(table_stem,"_overlapreinfcohorts")
OverlapCohortsTestnegName <- paste0(table_stem,"_overlaptestnegcohorts")

# Read functions needed throughout the study
source(here::here("functions.R"))
source(here::here("checks.R"))

# Create vector with all names
if(!onlyLC){
  CohortNames <- c(InitialCohortsName, BaseCohortsName, LongCovidCohortsName,
                   PascCohortsName, MCCohortsName, OverlapCohortsInfName,
                   OverlapCohortsReinfName,OverlapCohortsTestnegName)
} else {
  CohortNames <- c(InitialCohortsName, BaseCohortsName, LongCovidCohortsName,
                   OverlapCohortsInfName, OverlapCohortsReinfName)
}

# Read initial cohorts
if (readInitialCohorts){
  info(logger, 'INSTANTIATING INITIAL COHORTS')
  cdm <- cdmFromCon(db, cdm_database_schema, 
                    writeSchema = results_database_schema)
  source(here("1_InitialCohorts", "InstantiateStudyCohorts.R"), local=TRUE)
  info(logger, 'GOT INITIAL COHORTS')
} else {
  info(logger, 'INITIAL COHORTS ALREADY INSTANTIATED')
  Initial_cohorts <- CDMConnector::readCohortSet(
    here::here("1_InitialCohorts", "Jsons")) %>%
    dplyr::mutate(cohort_name = substr(cohort_name, 5, nchar(cohort_name)))
  cdm <- cdmFromCon(
    db, cdm_database_schema, writeSchema = results_database_schema,
    cohortTables = InitialCohortsName)
  info(logger, 'INITIAL COHORTS READ')
}

# Instantiate study cohorts
if(getStudyCohorts) {
  info(logger, 'GETTING STUDY COHORTS')
  source(here("2_StudyCohorts","GetStudyCohorts.R"), local = TRUE)
  info(logger, 'GOT STUDY COHORTS')
}

# Objective 1: Incidence and Prevalence
if(doIncidencePrevalence) {
  source(here("3_IncidencePrevalence","WP1_code.R"), local = TRUE)
  info(logger, 'GOT INCIDENCE AND PREVALENCE')
}

info(logger, 'SAVED RESULTS IN THE OUTPUT FOLDER')

zip::zip(zipfile = paste0(zipName, ".zip"),
         files = list.files(tempDir, full.names = TRUE))

print("Done!")
print("If all has worked, there should now be a zip file with your results
      in the output folder to share")
print("Thank you for running the study!")
Sys.time() - start
readLines(log_file)

if (tempDirCreated) {
  unlink(tempDir, recursive = TRUE)
}
