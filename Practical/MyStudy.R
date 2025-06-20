# load libraries ----
library(CDMConnector)
library(duckdb)
library(here)

# create the database ----
# every time that you source the database will be created again
# uncomment to run!
# source(here("Database", "CreateDatabase.R"))

# create a cdm reference ----
# create a connection to the database
con <- dbConnect(drv = duckdb(dbdir = here("Database", "TestDatabase.duckdb")))

# create the cdm object
cdm <- cdmFromCon(
  con = con,
  cdmSchema = "main",
  writeSchema = "results",
  writePrefix = "rwess_",
  cdmName = "SummerSchool2025"
)
cdm

# characterise the database ----
