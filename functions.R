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
