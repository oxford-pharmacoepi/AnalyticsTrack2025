# install pak
install.packages("pak")
library(pak)

# install packages that we will use
pkg_install(c(
  "DBI", "here", "usethis", "dplyr", "dbplyr", "CDMConnector",
  "PatientProfiles", "IncidencePrevalence", "CohortConstructor",
  "DrugUtilisation", "OmopSketch", "visOmopResults", "CohortCharacteristics",
  "usethis", "gt", "shiny", "bslib", "OmopViewer", "omock", "DiagrammeR",
  "DiagrammeRsvg", "rsvg", "png", "duckdb/duckdb-r"
))

# edit r environment
# run the following command
library(usethis)
edit_r_environ()

# the `.Renviron` file should be open now
# please write there:
# MOCK_DATASETS_FOLDER="/path/to/my/mockDatasets/folder/"

# RESTART R so the change is done

library(omock)

# this should show the folder that you wrote in the .Renviron file:
mockDatasetsFolder()

# download mock dataset
options(timeout = 600)
downloadMockDataset(datasetName = "GiBleed", overwrite = FALSE)
downloadMockDataset(datasetName = "synpuf-1k_5.3", overwrite = FALSE)
downloadMockDataset(datasetName = "synthea-covid19-10k", overwrite = FALSE)
downloadMockDataset(datasetName = "synthea-covid19-200k", overwrite = FALSE)

# you can check now that a .zip file has been created in
# /path/to/my/mockDatasets/folder/GiBleed.zip

# You can create a local mock reference
cdm <- mockCdmFromDataset(datasetName = "GiBleed")

cdm
