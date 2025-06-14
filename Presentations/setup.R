# install pak
install.packages("pak")
library(pak)

# install packages that we will use
pkg_install(c(
  "DBI", "duckdb", "here", "usethis", "dplyr", "dbplyr", "CDMConnector",
  "PatientProfiles", "IncidencePrevalence", "CohortConstructor",
  "DrugUtilisation", "OmopSketch", "visOmopResults", "CohortCharacteristics",
  "usethis", "gt", "shiny", "bslib", "OmopViewer", "omock"
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
downloadMockDataset(datasetName = "GiBleed")

# you can check now that a .zip file has been created in
# /path/to/my/mockDatasets/folder/GiBleed.zip

# You can create a local mock reference
cdm <- mockCdmFromDataset(datasetName = "GiBleed")

cdm

# We can insert this dataset to a duckdb database
library(CDMConnector)
library(duckdb)

dbs <- dbSource(con = dbConnect(drv = duckdb()), writeSchema = "main")
dbcdm <- insertCdmTo(cdm = cdm, to = dbs)

dbcdm
