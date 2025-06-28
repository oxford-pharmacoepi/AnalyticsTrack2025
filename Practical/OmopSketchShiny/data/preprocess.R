# shiny is prepared to work with this resultList:
resultList <- list(
  "summarise_omop_snapshot" = list(result_type = "summarise_omop_snapshot"),
  "summarise_observation_period" = list(result_type = "summarise_observation_period"),
  "summarise_clinical_records" = list(result_type = "summarise_clinical_records"),
  "summarise_record_count" = list(result_type = "summarise_record_count"),
  "summarise_missing_data" = list(result_type = "summarise_missing_data"),
  "summarise_in_observation" = list(result_type = "summarise_in_observation"),
  "summarise_characteristics" = list(result_type = "summarise_characteristics"),
  "summarise_table_quality" = list(result_type = "summarise_table_quality")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))
data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- values

save(data, choices, selected, values, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, values, choices, selected, resultList, data)
