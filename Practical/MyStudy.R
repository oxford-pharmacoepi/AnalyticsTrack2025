# load libraries ----
library(CDMConnector)
library(duckdb)
library(here)
library(OmopSketch)
library(CodelistGenerator)

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

# DAY 1 ----
# characterise the database

# get and visualise snapshot
snapshot <- summariseOmopSnapshot(cdm = cdm)
tableOmopSnapshot(result = snapshot)

# summarise observation periods
op <- summariseObservationPeriod(cdm$observation_period)
tableObservationPeriod(result = op)

# characterise and visualise condition_occurrence and drug_exposure
summaryTables <- summariseClinicalRecords(
  cdm = cdm,
  omopTableName = c("drug_exposure", "condition_occurrence")
)
tableClinicalRecords(result = summaryTables)

# summarise missing values in
missings <- summariseMissingData(cdm = cdm, omopTableName = c("drug_exposure", "condition_occurrence"))
tableMissingData(result = missings)

# visualise when individuals are in observation
inObservation <- summariseInObservation(cdm$observation_period,interval = "years")
plotInObservation(inObservation)

# see trends of records
recordTrends <- summariseRecordCount(
  cdm = cdm,
  omopTableName = c("drug_exposure", "condition_occurrence"),
  interval = "years"
)
plotRecordCount(result = recordTrends, colour = "omop_table")

# summarise concepts
concepts <- summariseConceptIdCounts(cdm = cdm, omopTableName = c("condition_occurrence", "drug_exposure"))
tableTopConceptCounts(result = concepts)
tableConceptIdCounts(result = concepts)

# characterise the database
result <- databaseCharacteristics(
  cdm = cdm,
  sex = TRUE,
  ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf)),
  dateRange = c("2014-01-01", "2021-12-31")
)
shinyCharacteristics(result = result, directory = here())

# DAY 2 ----
# create codelists of interest for `acetaminophen`, `ibuprofen`, `codeine` and
# `tramadol`.
codelist1 <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("acetaminophen", "ibuprofen", "codeine", "tramadol"),
  nameStyle = "{concept_name}"
)

# create a second codelist that only includes drugs from a single ingredients
# and with 'oral' route.
codelist2 <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("acetaminophen", "ibuprofen", "codeine", "tramadol"),
  nameStyle = "{concept_name}",
  routeCategory = "oral",
  ingredientRange = c(1, 1)
)

# compare codelists
compareCodelists(codelist1 = codelist1["acetaminophen"], codelist2 = codelist2["acetaminophen"])

# export the second group of codelists (single ingredient and oral route) to the
# exposures folder
exportCodelist(x = codelist2, path = here("Codelists", "exposures"), type = "csv")

# create codelists for 2 indications of interest: 'headache' and 'cough' and
# save them in the `indications` folder.

# Create the following drug users (exposures) cohort:
# I1. Collapse records using a gap of 7 days
# I2. Require at least 365 days of prior observation
# I3. Include only record during the study period that you previously defined.
# I4. Require a washout of 365 days (no use of the same drug, e.g. the
# acetaminophen cohort has 0 records of acetaminophen use in the last 365 days).

# Create a pain cohort:
# I1. Require at least 365 days of prior observation
# I2. Include only record during the study period that you previously defined.
# I3. Require a washout of 365 days (no record of pain in the last 365 days).

# Read all the codelists from `conditions`, `exposures`, `indications` and
# `medications` and combine them (codelist = c(codelist1, codelist2, ...))

# Create a base cohort for any of the different codelists.

# Use PhenotypeR on this cohort, create the shiny and explore it

# If you have time you can create more indications or conditions cohorts, you
# can explore the shiny created with OmopSketch to see present concepts.

# DAY 3 ----
# read the codelists of interest
exposures <- importCodelist(path = here("Codelists", "exposures"))
medications <- importCodelist(path = here("Codelists", "medications"))
conditions <- importCodelist(path = here("Codelists", "conditions"))
indications <- importCodelist(path = here("Codelists", "indications"))

# create the drug users (exposures) cohort
# I1. Collapse records using a gap of 7 days
# I2. Require at least 365 days of prior observation
# I3. Include only record during the study period that you previously defined.
# I4. Require a washout of 365 days (no use of the same drug, e.g. the
# acetaminophen cohort has 0 records of acetaminophen use in the last 365 days).

# Export in separate png files the attrition of each cohort.

# See overlap between cohorts

# Analyse the timing between the `pain` cohort and any of the 4 exposures.

# Characterise the exposures cohort
# Include the following information:
# - demographics
# - any record of conditions any time prior
# - records of indication in the prior 7 days
# - records of indication in the prior 365 days
# - records of medications in the prior 365 days

# Visualise the result in a gt table, the table should compare the 4 cohorts
# separated in 4 adjacent columns. Export the table into a word document.

# Large scale characterisation, perform a large scale characterisation on the
# `pain` cohort. Use the following windows: c(-90, -1), c(0, 0), c(1, 90); do it
# for the following tables: `drug_exposure` and `condition_occurrence`.

# Generate the denominator cohort to conduct an incidence prevalence analysis
# restrict to your study period, create 5 age groups using (20/25 years bands),
# [IMPORTANT, remember to create an `overall` group, e.g. c(0, 150)]
# and stratify by sex.

# Create the outcome cohort, as incidence prevalence needs alls the records so
# it applies prior observation and washouts correctly cretae the outcome cohort
# as:
# bse cohort using the exposures codelist (end date to event_end_date)
# I1. Collapse records using a gap of 7 days

# Estimate incidence of the drug exposure cohorts, using repeated events and a
# washout of 365 days.

# Visualise the incidence of the different drugs in a plot, facet by sex
# (horizontally) and age (vertically) and colour by exposure. Which is the
# exposure with a higher incidence? Do we see any interesting trend?

# Repeat the analyses for point and period prevalence if you have extra time.

# DAY 4 ----

# Indication, using the `exposures` cohort let's see if we can identify the
# different indications (create a base cohort just using the indications
# codelists), define 3 indication windows (c(0, 0), c(-7, 0), c(-30, 0)) and use
# as unknown indication the `condition_occurrence` and `observation` tables.

# Visualise the results in a table.

# Characterise the treatments before and after pain record. Using the `pain`
# cohort, as anchor summarise the treatments received before and after the
# pain index date (cohort_start_date). Use the following windows: c(-30, -16);
# c(-15, -1); c(0, 0); c(1, 15); c(16, 30); c(31, 45); c(46, 60).

# Visualise the result in a plot

# Discontinuation, first we will do a proportion of patients covered analysis

# Drug restart

# disconnect ----
cdmDisconnect(cdm = cdm)
