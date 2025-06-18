
datasets <- omock::availableMockDatasets() |>
  purrr::keep(\(x) x != "empty_cdm")

for (nm in datasets) {
  cli::cli_inform("{.strong Analysisng data set: {.pkg {nm}}}:")
  cdm <- omock::mockCdmFromDataset(datasetName = nm)

  # driver
  drv <- duckdb::duckdb(dbdir = here::here("Database", paste0(nm, ".duckdb")))
  con <- duckdb::dbConnect(drv = drv)
  src <- CDMConnector::dbSource(con = con, writeSchema = "main")
  cdm <- omopgenerics::insertCdmTo(cdm = cdm, to = src)

  summary <- character()

  nInd <- cdm$person |>
    dplyr::tally() |>
    dplyr::pull()
  summary <- c(summary, paste0("Number Individuals = ", nInd), "")

  conditions <- cdm$condition_occurrence |>
    PatientProfiles::addConceptName(column = "condition_concept_id", nameStyle = "concept_name") |>
    dplyr::group_by(concept_name) |>
    dplyr::summarise(n = dplyr::n_distinct(person_id)) |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(n)) |>
    utils::head(5) |>
    dplyr::mutate(mes = paste0(concept_name, ": ", n)) |>
    dplyr::pull("mes")
  names(conditions) <- rep("*", length(conditions))
  summary <- c(summary, conditions, "")

  medications <- cdm$drug_exposure |>
    PatientProfiles::addConceptName(column = "drug_concept_id", nameStyle = "concept_name") |>
    dplyr::group_by(concept_name) |>
    dplyr::summarise(n = dplyr::n_distinct(person_id)) |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(n)) |>
    utils::head(5) |>
    dplyr::mutate(mes = paste0(concept_name, ": ", n)) |>
    dplyr::pull("mes")
  names(medications) <- rep("*", length(medications))
  summary <- c(summary, medications, "")

  # message
  cat(c(summary, ""), sep = "\n")

  # disconnect
  duckdb::dbDisconnect(conn = con)
  duckdb::duckdb_shutdown(drv)
}
