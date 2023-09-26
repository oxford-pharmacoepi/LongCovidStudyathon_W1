insertTable <- function(x,
                        cdm, 
                        name,
                        overwrite = TRUE) {
  con <- attr(cdm, "dbcon")
  writeSchema <- attr(cdm, "write_schema")
  checkTableExist <- name %in% CDMConnector::listTables(con, writeSchema)
  if (checkTableExist) {
    if (overwrite) {
      DBI::dbRemoveTable(con, CDMConnector:::inSchema(writeSchema, name))
    } else {
      stop(paste0("'", name, "' table already exists."))
    }
  }
  DBI::dbCreateTable(con, CDMConnector:::inSchema(writeSchema, name), x)
  DBI::dbAppendTable(con, CDMConnector:::inSchema(writeSchema, name), x)
  if (methods::is(con, "duckdb_connection")) {
    ref <- dplyr::tbl(con, paste(c(writeSchema, name), collapse = "."))
  } else if (length(writeSchema) == 2) {
    ref <- dplyr::tbl(con,
                      dbplyr::in_catalog(writeSchema[[1]], writeSchema[[2]], name))
  } else if (length(writeSchema) == 1) {
    ref <- dplyr::tbl(con, dbplyr::in_schema(writeSchema, name))
  } else {
    ref <- dplyr::tbl(con, name)
  }
  return(ref)
}


getCohortCount <- function(cohort) {
  cohort %>%
    group_by(cohort_definition_id) %>%
    summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    collect() %>%
    arrange(cohort_definition_id)
}

addDemographics_sql <- function(x,
                            cdm = attr(x, "cdm_reference"),
                            indexDate = "cohort_start_date",
                            age = TRUE,
                            ageName = "age",
                            ageDefaultMonth = 1,
                            ageDefaultDay = 1,
                            ageImposeMonth = FALSE,
                            ageImposeDay = FALSE,
                            ageGroup = NULL,
                            sex = TRUE,
                            sexName = "sex",
                            priorObservation = TRUE,
                            priorObservationName = "prior_observation",
                            futureObservation = TRUE,
                            futureObservationName = "future_observation") {
  ## change ageDefaultMonth, ageDefaultDay to integer
  
  if (typeof(ageDefaultMonth) == "character") {
    ageDefaultMonth <- as.integer(ageDefaultMonth)
  }
  
  if (typeof(ageDefaultDay) == "character") {
    ageDefaultDay <- as.integer(ageDefaultDay)
  }
  
  ## check for standard types of user error
  personVariable <- checkX(x)
  checkCdm(cdm, c("person", "observation_period"))
  checkmate::assertLogical(age, any.missing = FALSE, len = 1)
  checkmate::assertIntegerish(
    ageDefaultMonth,
    lower = 1, upper = 31, any.missing = FALSE, len = 1,
    null.ok = !age
  )
  checkmate::assertIntegerish(
    ageDefaultDay,
    lower = 1, upper = 31, any.missing = FALSE, len = 1,
    null.ok = !age
  )
  checkmate::assertLogical(ageImposeMonth, any.missing = FALSE, len = 1)
  checkmate::assertLogical(ageImposeDay, any.missing = FALSE, len = 1)
  ageGroup <- checkAgeGroup(ageGroup)
  checkmate::assertLogical(sex, any.missing = FALSE, len = 1)
  checkmate::assertLogical(priorObservation, any.missing = FALSE, len = 1)
  checkmate::assertLogical(futureObservation, any.missing = FALSE, len = 1)
  checkVariableInX(indexDate, x, !(age | priorObservation | futureObservation))
  if (!(age | sex | priorObservation | futureObservation)) {
    cli::cli_abort("age, sex, priorObservation, futureObservation can not be FALSE")
  }
  
  # check variable names
  if (age) {
    ageName <- checkSnakeCase(ageName)
  }
  if (sex) {
    sexName <- checkSnakeCase(sexName)
  }
  if (priorObservation) {
    priorObservationName <- checkSnakeCase(priorObservationName)
  }
  if (futureObservation) {
    futureObservationName <- checkSnakeCase(futureObservationName)
  }
  
  checkNewName(ageName, x)
  checkNewName(sexName, x)
  checkNewName(priorObservationName, x)
  checkNewName(futureObservationName, x)
  
#  if (age == TRUE || priorObservation == TRUE || futureObservation == TRUE) {
#    checkmate::assert_true(
#      inherits(
#        x %>%
#          utils::head(1) %>%
#          dplyr::pull(indexDate),
#        c("Date", "POSIXt")
#      )
#    )
#  }
  
  # Start code
  startTibble <- x
  startNames <- colnames(x)
  
  personDetails <- cdm[["person"]] %>%
    dplyr::select(
      "person_id",
      "gender_concept_id",
      "year_of_birth",
      "month_of_birth",
      "day_of_birth"
    ) %>%
    dplyr::rename(!!personVariable := "person_id")
  
  if (priorObservation == TRUE || futureObservation == TRUE) {
    # most recent observation period (in case there are multiple)
    obsPeriodDetails <- x %>%
      dplyr::select(dplyr::all_of(c(personVariable, indexDate))) %>%
      dplyr::distinct() %>%
      dplyr::inner_join(
        cdm[["observation_period"]] %>%
          dplyr::rename(!!personVariable := "person_id") %>%
          dplyr::select(
            dplyr::all_of(personVariable),
            "observation_period_start_date",
            "observation_period_end_date"
          ),
        by = personVariable
      ) %>%
      dplyr::filter(.data$observation_period_start_date <=
                      .data[[indexDate]] &
                      .data$observation_period_end_date >=
                      .data[[indexDate]])
  }
  
  # update dates
  if (age) {
    personDetails <- personDetails %>%
      dplyr::filter(!is.na(.data$year_of_birth)) %>%
      addDateOfBirth(
        name = "date_of_birth",
        missingDay = ageDefaultDay,
        missingMonth = ageDefaultMonth,
        imposeDay = ageImposeDay,
        imposeMonth = ageImposeMonth
      )
  }
  
  # join if not the person table
  if (any(!c("person_id", "gender_concept_id") %in% colnames(x))) {
    x <- x %>%
      dplyr::left_join(
        personDetails %>%
          dplyr::select(dplyr::any_of(c(
            personVariable,
            "date_of_birth",
            "gender_concept_id",
            "observation_period_start_date",
            "observation_period_end_date"
          ))),
        by = personVariable
      )
  }
  
  if (priorObservation == TRUE || futureObservation == TRUE) {
    x <- x %>%
      dplyr::left_join(obsPeriodDetails,
                       by = c(personVariable, indexDate)
      )
  }
  
  if (age == TRUE) {
    aQ <- ageQuery(indexDate, name = ageName)
  } else {
    aQ <- NULL
  }
  
  if (sex == TRUE) {
    sQ <- sexQuery(name = sexName)
  } else {
    sQ <- NULL
  }
  
  if (priorObservation == TRUE) {
    pHQ <- priorObservationQuery(indexDate, name = priorObservationName)
  } else {
    pHQ <- NULL
  }
  
  if (futureObservation == TRUE) {
    fOQ <- futureObservationQuery(indexDate, name = futureObservationName)
  } else {
    fOQ <- NULL
  }
  
  x <- x %>%
    dplyr::mutate(
      !!!aQ,
      !!!sQ,
      !!!pHQ,
      !!!fOQ
    )
  
  x <- x %>%
    dplyr::select(
      dplyr::all_of(startNames),
      dplyr::any_of(c(
        ageName, sexName,
        priorObservationName,
        futureObservationName
      ))
    )
  
  if (sex == TRUE) {
    x <- x %>%
      dplyr::mutate(!!sexName := dplyr::if_else(!is.na(.data[[sexName]]),
                                                .data[[sexName]],
                                                "None"
      ))
  }
  
  x <- x %>%
    CDMConnector::computeQuery()
  
  if (!is.null(ageGroup)) {
    x <- addCategories(
      x = x,
      variable = ageName,
      categories = ageGroup,
      missingCategoryValue = "None"
    )
  }
  
  # put back the initial attributes to the output tibble
  x <- x %>% addAttributes(startTibble)
  
  return(x)
}



ageQuery <- function(indexDate, name) {
  return(glue::glue('floor(dbplyr::sql(
    CDMConnector::datediff(
      start = "date_of_birth",
      end = "{indexDate}",
      interval = "year"
    )
  ))') %>%
           rlang::parse_exprs() %>%
           rlang::set_names(glue::glue(name)))
}

sexQuery <- function(name) {
  return(glue::glue('dplyr::case_when(
      .data$gender_concept_id == 8507 ~ "Male",
      .data$gender_concept_id == 8532 ~ "Female",
      TRUE ~ as.character(NA))') %>%
           rlang::parse_exprs() %>%
           rlang::set_names(glue::glue(name)))
}

priorObservationQuery <- function(indexDate, name) {
  return(glue::glue('CDMConnector::datediff("observation_period_start_date",
                      "{indexDate}")') %>%
           rlang::parse_exprs() %>%
           rlang::set_names(glue::glue(name)))
}

futureObservationQuery <- function(indexDate, name) {
  return(glue::glue('CDMConnector::datediff("{indexDate}",
                          "observation_period_end_date")') %>%
           rlang::parse_exprs() %>%
           rlang::set_names(glue::glue(name)))
}

addPriorObservation_sql <- function(x,
                                cdm = attr(x, "cdm_reference"),
                                indexDate = "cohort_start_date",
                                priorObservationName = "prior_observation") {
  x <- x %>%
    addDemographics_sql(
      cdm = cdm,
      indexDate = indexDate,
      age = FALSE,
      ageGroup = NULL,
      ageDefaultDay = NULL,
      ageDefaultMonth = NULL,
      ageImposeDay = FALSE,
      ageImposeMonth = FALSE,
      sex = FALSE,
      priorObservation = TRUE,
      priorObservationName = priorObservationName,
      futureObservation = FALSE,
      ageName = NULL,
      sexName = NULL,
      futureObservationName = NULL
    )
  
  return(x)
}
