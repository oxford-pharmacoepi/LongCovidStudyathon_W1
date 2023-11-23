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

addInObservation_sql <- function(x,
                             cdm = attr(x, "cdm_reference"),
                             indexDate = "cohort_start_date",
                             name = "in_observation") {
  ## check for standard types of user error
  personVariable <- checkX(x)
  checkCdm(cdm, c("observation_period"))
  checkVariableInX(indexDate, x)
  checkmate::assertCharacter(name, any.missing = FALSE, len = 1)
  name <- checkNewName(name, x)
  
  # Start code
  name <- rlang::enquo(name)
  
  x <- x %>%
    addDemographics_sql(
      cdm = cdm,
      indexDate = indexDate,
      age = FALSE,
      sex = FALSE,
      priorObservation = TRUE,
      futureObservation = TRUE
    ) %>%
    dplyr::mutate(
      !!name := as.numeric(dplyr::if_else(
        is.na(.data$prior_observation) | is.na(.data$future_observation) | .data$prior_observation < 0 | .data$future_observation < 0, 0, 1
      ))
    ) %>%
    dplyr::select(
      -"prior_observation", -"future_observation"
    )
  
  x <- x %>%
    CDMConnector::computeQuery()
  
  return(x)
}

addIntersect_sql <- function(x,
                         cdm = attr(x, "cdm_reference"),
                         tableName,
                         value,
                         filterVariable = NULL,
                         filterId = NULL,
                         idName = NULL,
                         window = list(c(0, Inf)), # list
                         indexDate = "cohort_start_date",
                         censorDate = NULL,
                         targetStartDate = getStartName(tableName),
                         targetEndDate = getEndName(tableName),
                         order = "first",
                         nameStyle = "{value}_{id_name}_{window_name}") {
  # initial checks
  personVariable <- checkX(x)
  checkmate::assertCharacter(tableName, len = 1, any.missing = FALSE)
  checkCdm(cdm, tableName)
  personVariableTable <- checkX(cdm[[tableName]])
  extraValue <- checkValue(value, cdm[[tableName]], tableName)
  filterTbl <- checkFilter(filterVariable, filterId, idName, cdm[[tableName]])
  windowTbl <- checkWindow(window)
  checkVariableInX(indexDate, x)
  checkVariableInX(targetStartDate, cdm[[tableName]], FALSE, "targetStartDate")
  checkVariableInX(targetEndDate, cdm[[tableName]], TRUE, "targetEndDate")
  checkmate::assertChoice(order, c("first", "last"))
  checkNameStyle(nameStyle, filterTbl, windowTbl, value)
  checkVariableInX(censorDate, x, TRUE, "censorDate")
  if (!is.null(censorDate)) {
    checkCensorDate(x, censorDate)
  }
  if (!is.null(idName)) {
    idName <- checkSnakeCase(idName)
  }
  
  tablePrefix <- c(sample(letters, 5, TRUE), "_") %>% paste0(collapse = "")
  
  startTibble <- x
  originalColnames <- colnames(x)
  
  # define overlapTable that contains the events of interest
  overlapTable <- cdm[[tableName]]
  if (!is.null(filterTbl)) {
    overlapTable <- overlapTable %>%
      dplyr::filter(.data[[filterVariable]] %in% .env$filterId)
  } else {
    filterVariable <- "id"
    filterTbl <- dplyr::tibble("id" = 1, "id_name" = "all")
    overlapTable <- dplyr::mutate(overlapTable, "id" = 1)
  }
  
  overlapTable <- overlapTable %>%
    dplyr::select(
      !!personVariable := dplyr::all_of(personVariableTable),
      "id" = dplyr::all_of(filterVariable),
      "overlap_start_date" = dplyr::all_of(targetStartDate),
      "overlap_end_date" = dplyr::all_of(targetEndDate %||% targetStartDate),
      dplyr::all_of(extraValue)
    )
  
  result <- x %>%
    addFutureObservation_sql(
      cdm = cdm,
      indexDate = indexDate,
      futureObservationName = "days_to_add"
    ) %>%
    dplyr::mutate("censor_date" = !!CDMConnector::dateadd(
      indexDate, "days_to_add"
    )) %>%
    dplyr::mutate("censor_date" = .data[[censorDate %||% "censor_date"]])
  
  result <- result %>%
    dplyr::select(
      dplyr::all_of(personVariable),
      "index_date" = dplyr::all_of(indexDate),
      "censor_date"
    ) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(overlapTable, by = personVariable) %>%
    CDMConnector::computeQuery(
      name = paste0(tablePrefix, "individuals"), temporary = FALSE,
      schema = attr(cdm, "write_schema"), overwrite = TRUE
    )
  
  resultCountFlag <- NULL
  resultDateTimeOther <- NULL
  # Start loop for different windows
  
  for (i in c(1:nrow(windowTbl))) {
    resultW <- result
    if (!is.infinite(windowTbl$upper[i])) {
      resultW <- resultW %>%
        dplyr::mutate(indicator = dplyr::if_else(.data$index_date >= as.Date(!!CDMConnector::dateadd(
          date = "overlap_start_date", number = -windowTbl$upper[i]
        )), 1, 0))
    } else {
      resultW <- resultW %>% dplyr::mutate(indicator = 1)
    }
    
    resultW <- resultW %>%
      dplyr::mutate(indicator = dplyr::if_else(.data$overlap_start_date > .data$censor_date,
                                               0, .data$indicator
      ))
    
    if (!is.infinite(windowTbl$lower[i])) {
      resultW <- resultW %>%
        dplyr::mutate(indicator = dplyr::if_else(.data$index_date > as.Date(!!CDMConnector::dateadd(
          date = "overlap_end_date", number = -windowTbl$lower[i]
        )), 0, .data$indicator))
    }
    resultW <- resultW %>%
      CDMConnector::computeQuery(
        name = paste0(tablePrefix, "window"), temporary = FALSE,
        schema = attr(cdm, "write_schema"), overwrite = TRUE
      )
    
    # add count or flag
    if ("count" %in% value | "flag" %in% value) {
      resultCF <- resultW %>%
        dplyr::group_by(.data[[personVariable]], .data$index_date, .data$id) %>%
        dplyr::summarise(count = sum(.data$indicator, na.rm = TRUE), .groups = "drop") %>%
        dplyr::left_join(filterTbl, by = "id", copy = TRUE) %>%
        dplyr::select(-"id") %>%
        dplyr::mutate("window_name" = !!tolower(windowTbl$window_name[i]))
      if ("flag" %in% value) {
        resultCF <- resultCF %>% dplyr::mutate(flag = dplyr::if_else(.data$count > 0, 1, 0))
      }
      if (!("count" %in% value)) {
        resultCF <- dplyr::select(resultCF, -"count")
      }
      
      if (i == 1) {
        resultCountFlag <- resultCF %>%
          CDMConnector::computeQuery(
            name = paste0(tablePrefix, "win_count_flag"), temporary = FALSE,
            schema = attr(cdm, "write_schema"), overwrite = TRUE
          )
      } else {
        resultCountFlag <- appendPermanent(
          x = resultCF,
          name = paste0(tablePrefix, "win_count_flag"),
          schema = attr(cdm, "write_schema")
        )
      }
    }
    # add date, time or other
    if (length(value[!(value %in% c("count", "flag"))]) > 0) {
      resultDTO <- resultW %>%
        dplyr::filter(.data$indicator == 1) %>%
        dplyr::group_by(.data[[personVariable]], .data$index_date, .data$id)
      if (order == "first") {
        resultDTO <- resultDTO %>%
          dplyr::summarise(
            date = min(.data$overlap_start_date, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        resultDTO <- resultDTO %>%
          dplyr::summarise(
            date = max(.data$overlap_start_date, na.rm = TRUE),
            .groups = "drop"
          )
      }
      resultDTO <- resultDTO %>%
        dplyr::right_join(
          resultW %>%
            dplyr::select(dplyr::all_of(c(personVariable, "index_date", "id"))) %>%
            dplyr::distinct(),
          by = c(personVariable, "index_date", "id")
        )
      if ("days" %in% value) {
        resultDTO <- resultDTO %>%
          dplyr::mutate(
            days = !!CDMConnector::datediff("index_date", "date", interval = "day")
          )
      }
      if (length(extraValue) > 0) {
        resultDTO <- resultDTO %>%
          dplyr::left_join(
            resultW %>%
              dplyr::select(
                dplyr::all_of(personVariable), "index_date", "id",
                "date" = "overlap_start_date", dplyr::all_of(extraValue)
              ) %>%
              dplyr::inner_join(
                resultDTO %>%
                  dplyr::select(dplyr::all_of(
                    c(personVariable, "index_date", "id", "date")
                  )),
                by = c(personVariable, "index_date", "id", "date")
              ) %>%
              dplyr::group_by(.data[[personVariable]], .data$index_date, .data$id) %>%
              dplyr::summarise(
                dplyr::across(
                  dplyr::all_of(extraValue), ~ str_flatten(.x, collapse = "; ")
                ),
                .groups = "drop"
              ),
            by = c(personVariable, "index_date", "id")
          )
      }
      resultDTO <- resultDTO %>%
        dplyr::left_join(filterTbl, by = "id", copy = TRUE) %>%
        dplyr::select(-"id") %>%
        dplyr::mutate("window_name" = !!tolower(windowTbl$window_name[i]))
      if (!("date" %in% value)) {
        resultDTO <- dplyr::select(resultDTO, -"date")
      }
      
      if (i == 1) {
        resultDateTimeOther <- resultDTO %>%
          CDMConnector::computeQuery(
            name = paste0(tablePrefix, "win_date_days"), temporary = FALSE,
            schema = attr(cdm, "write_schema"), overwrite = TRUE
          )
      } else {
        resultDateTimeOther <- appendPermanent(
          x = resultDTO,
          name = paste0(tablePrefix, "win_date_days"),
          schema = attr(cdm, "write_schema")
        )
      }
    }
  }
  
  if (any(c("flag", "count") %in% value)) {
    resultCountFlag <- resultCountFlag %>%
      tidyr::pivot_longer(
        dplyr::any_of(c("count", "flag")),
        names_to = "value",
        values_to = "values"
      ) %>%
      tidyr::pivot_wider(
        names_from = c("value", "id_name", "window_name"),
        values_from = "values",
        names_glue = nameStyle,
        values_fill = 0
      ) %>%
      dplyr::rename(!!indexDate := "index_date") %>%
      dplyr::rename_all(tolower)
    
    namesToEliminate <- intersect(colnames(x), colnames(resultCountFlag))
    namesToEliminate <- namesToEliminate[
      !(namesToEliminate %in% c(personVariable, indexDate))
    ]
    x <- x %>%
      dplyr::select(-dplyr::all_of(namesToEliminate)) %>%
      dplyr::left_join(
        resultCountFlag,
        by = c(personVariable, indexDate)
      )
    currentColnames <- colnames(x)
    x <- x %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(currentColnames[!(currentColnames %in% originalColnames)]),
        ~ dplyr::if_else(is.na(.x), 0, .x)
      ))
    x <- x %>%
      CDMConnector::computeQuery(
        name = paste0(tablePrefix, "count_flag"), temporary = FALSE,
        schema = attr(cdm, "write_schema"), overwrite = TRUE
      )
  }
  
  if (length(value[!(value %in% c("count", "flag"))]) > 0) {
    values <- value[!(value %in% c("count", "flag"))]
    for (val in values) {
      resultDateTimeOtherX <- resultDateTimeOther %>%
        dplyr::select(
          "subject_id", "index_date", dplyr::all_of(val), "id_name",
          "window_name"
        ) %>%
        tidyr::pivot_longer(
          dplyr::all_of(val),
          names_to = "value",
          values_to = "values"
        ) %>%
        tidyr::pivot_wider(
          names_from = c("value", "id_name", "window_name"),
          values_from = "values",
          names_glue = nameStyle
        ) %>%
        dplyr::rename(!!indexDate := "index_date") %>%
        dplyr::rename_all(tolower)
      
      namesToEliminate <- intersect(colnames(x), colnames(resultDateTimeOtherX))
      namesToEliminate <- namesToEliminate[
        !(namesToEliminate %in% c(personVariable, indexDate))
      ]
      
      x <- x %>%
        dplyr::select(-dplyr::all_of(namesToEliminate)) %>%
        dplyr::left_join(resultDateTimeOtherX,
                         by = c(personVariable, indexDate)
        )
    }
    
    x <- x %>%
      CDMConnector::computeQuery(
        name = paste0(tablePrefix, "date_days"), temporary = FALSE,
        schema = attr(cdm, "write_schema"), overwrite = TRUE
      )
    
  }
  
  colnames <- expand.grid(value = value, id_name = filterTbl$id_name, window_name = windowTbl$window_name) %>%
    dplyr::mutate(column = glue::glue(nameStyle, value = .data$value, id_name = .data$id_name, window_name = .data$window_name)) %>%
    dplyr::mutate(val = ifelse(value %in% c("flag", "count"), 0,
                               ifelse(value %in% "date", as.Date(NA),
                                      ifelse(value %in% "days", as.numeric(NA), as.character(NA))
                               )
    )) %>%
    dplyr::select(.data$column, .data$val) %>%
    dplyr::mutate(column = checkSnakeCase(.data$column, verbose = F)) %>%
    dplyr::anti_join(dplyr::tibble(column = colnames(x)), by = "column")
  
  if (colnames %>% dplyr::tally() %>% dplyr::pull() != 0) {
    x <- x %>%
      dplyr::cross_join(
        colnames %>%
          tidyr::pivot_wider(
            names_from = .data$column,
            values_from = .data$val
          ),
        copy = TRUE
      )
  }
  
  x <- CDMConnector::computeQuery(x)
  
  CDMConnector::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))
  
  # put back the initial attributes to the output tibble
  x <- x %>% addAttributes(startTibble)
  
  return(x)
}

addFutureObservation_sql <- function(x,
                                 cdm = attr(x, "cdm_reference"),
                                 indexDate = "cohort_start_date",
                                 futureObservationName = "future_observation") {
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
      priorObservation = FALSE,
      futureObservation = TRUE,
      futureObservationName = futureObservationName,
      ageName = NULL,
      sexName = NULL,
      priorObservationName = NULL
    )
  
  return(x)
}

addCohortIntersectFlag_sql <- function(x,
                                   cdm = attr(x, "cdm_reference"),
                                   targetCohortTable,
                                   targetCohortId = NULL,
                                   indexDate = "cohort_start_date",
                                   censorDate = NULL,
                                   targetStartDate = "cohort_start_date",
                                   targetEndDate = "cohort_end_date",
                                   window = list(c(0, Inf)),
                                   nameStyle = "{cohort_name}_{window_name}") {
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId, targetCohortTable)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)
  
  x <- x %>%
    addIntersect_sql(
      cdm = cdm,
      tableName = targetCohortTable,
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = parameters$id_name,
      value = "flag",
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      nameStyle = nameStyle,
      censorDate = censorDate
    )
  
  return(x)
}

addCohortIntersectDays_sql <- function(x,
                                   cdm = attr(x, "cdm_reference"),
                                   targetCohortTable,
                                   targetCohortId = NULL,
                                   indexDate = "cohort_start_date",
                                   censorDate = NULL,
                                   targetDate = "cohort_start_date",
                                   order = "first",
                                   window = c(0, Inf),
                                   nameStyle = "{cohort_name}_{window_name}") {
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId, targetCohortTable)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)
  
  x <- x %>%
    addIntersect_sql(
      cdm = cdm,
      tableName = targetCohortTable,
      indexDate = indexDate,
      value = "days",
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = parameters$id_name,
      window = window,
      targetStartDate = targetDate,
      targetEndDate = NULL,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate
    )
  
  return(x)
}

addCohortIntersectDate_sql <- function(x,
                                   cdm = attr(x, "cdm_reference"),
                                   targetCohortTable,
                                   targetCohortId = NULL,
                                   indexDate = "cohort_start_date",
                                   censorDate = NULL,
                                   targetDate = "cohort_start_date",
                                   order = "first",
                                   window = c(0, Inf),
                                   nameStyle = "{cohort_name}_{window_name}") {
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId, targetCohortTable)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)
  
  x <- x %>%
    addIntersect_sql(
      cdm = cdm,
      tableName = targetCohortTable,
      indexDate = indexDate,
      value = "date",
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = parameters$id_name,
      window = window,
      targetStartDate = targetDate,
      targetEndDate = NULL,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate
    )
  
  return(x)
}