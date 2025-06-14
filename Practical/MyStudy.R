
library(omock)
library(CDMConnector)
library(duckdb)
library(here)

# create the database
# source(here("Database", "CreateDatabase.R"))

# create a cdm reference
con <- dbConnect(drv = duckdb(dbdir = here("Database", "myDatabase.duckdb")))
cdm <- cdmFromCon(
  con = con,
  cdmSchema = "main",
  writeSchema = "results",
  writePrefix = "rwess_",
  cdmName = ""
)
