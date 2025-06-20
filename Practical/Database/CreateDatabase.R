
cdm <- omock::mockCdmFromDataset(datasetName = "synthea-covid19-200k")

# read extra condition occurrence rows
conditionOccurrence <- readr::read_csv(file = here::here("Database", "condition_occurrence_extra_rows.csv"), show_col_types = FALSE) |>
  dplyr::mutate(
    condition_occurrence_id = as.integer(dplyr::row_number() + max(cdm$condition_occurrence$condition_occurrence_id)),
    person_id = as.integer(person_id),
    condition_concept_id = as.integer(condition_concept_id),
    condition_start_date = as.Date(condition_start_date),
    condition_start_datetime = condition_start_date,
    condition_end_date = condition_start_date,
    condition_end_datetime = condition_start_date,
    condition_type_concept_id = 38000175L,
    condition_status_concept_id = 0L,
    stop_reason = NA_character_,
    provider_id = NA_integer_,
    visit_occurrence_id = 0L,
    visit_detail_id = 0L,
    condition_source_value = NA_character_,
    condition_source_concept_id = 0L,
    condition_status_source_value = NA_character_
  )
cdm$condition_occurrence <- cdm$condition_occurrence |>
  dplyr::union_all(conditionOccurrence)

# read extra drug exposure rows
drugExposure <- readr::read_csv(file = here::here("Database", "drug_exposure_extra_rows.csv"), show_col_types = FALSE) |>
  dplyr::mutate(
    drug_exposure_id = as.integer(dplyr::row_number() + max(cdm$drug_exposure$drug_exposure_id)),
    person_id = as.integer(person_id),
    drug_concept_id = as.integer(drug_concept_id),
    drug_exposure_start_date = as.Date(drug_exposure_start_date),
    drug_exposure_end_date = as.Date(drug_exposure_end_date),
    drug_exposure_start_datetime = drug_exposure_start_date,
    drug_exposure_end_datetime = drug_exposure_end_date,
    verbatim_end_date = drug_exposure_end_date,
    drug_type_concept_id = 32869L,
    stop_reason = NA_character_,
    refills = 0L,
    quantity = as.numeric(quantity),
    days_supply = as.integer(days_supply),
    sig = NA_character_,
    route_concept_id = 0L,
    lot_number = "0",
    provider_id = 0L,
    visit_occurrence_id = 0L,
    visit_detail_id = 0L,
    drug_source_value = NA_character_,
    drug_source_concept_id = 0L,
    route_source_value = NA_character_,
    dose_unit_source_value = NA_character_
  )
cdm$drug_exposure <- cdm$drug_exposure |>
  dplyr::union_all(drugExposure)

dbdir <- here::here("Database", "TestDatabase.duckdb")

# delete database if exist
if (file.exists(dbdir)) {
  drv <- duckdb::duckdb(dbdir = dbdir)
  duckdb::duckdb_shutdown(drv = drv)
  unlink(x = dbdir)
}

# copy data to duckdb database
drv <- duckdb::duckdb(dbdir = dbdir)
con <- duckdb::dbConnect(drv = drv)
DBI::dbExecute(con, "CREATE SCHEMA results")
src <- CDMConnector::dbSource(con = con, writeSchema = "main")
cdm <- CDMConnector::insertCdmTo(cdm = cdm, to = src)
CDMConnector::cdmDisconnect(cdm = cdm)

# delete objects
rm(cdm, conditionOccurrence, drugExposure, drv, con, src, dbdir)
