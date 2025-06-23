# load libraries ----
library(CDMConnector)
library(duckdb)
library(here)
library(OmopSketch)
library(CodelistGenerator)
library(CohortConstructor)
library(PhenotypeR)
library(PatientProfiles)
library(CohortCharacteristics)
library(IncidencePrevalence)
library(DrugUtilisation)
library(CohortSurvival)
library(dplyr)
library(gt)

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
  achillesSchema = "achilles",
  writePrefix = "rwess_",
  cdmName = "SummerSchool2025",
  # cohortTables = c("exposures", "pain", "base_cohort") # uncomment if the cohorts have already been created (DAY 3 / DAY 4)
)
cdm

# DAY 1 - DATABASE CHARACTERISATION ----

# Extract metadata, summarise snapshot and visualise it in a table.

# Summarise `observation_period` and visualise it in a table. Are there multiple
# records per person? Which is the mean follow up?

# Characterise and summarise `condition_occurrence` and `drug_exposure`.
# Visualise the result in a table.
# Are there any records outside of observation? Should we be worried about it?
# Which are the source vocabularies used?
# Are there any non standard concept that we should worry about?

# Summarise missing values in `drug_exposure` and `condition_occurrence`.
# Visualise the result in a table. Any of the main columns have missing values?
# Are there any worring missing values?

# Summarise when individuals are in observation (use yearly or quarterly data so
# you can see clearly the trends). Visualise the result in a plot.
# When most of the individuals are in observation? Define a `studyPeriod`
# given the results of the 'in observation' analysis.

# Summarise the yearly record trends in `drug_exposure` and
# `condition_occurrence`. Visualise the result in a plot. Are there any worring
# trends? Did you see a change due to covid?

# Summarise the concept ids in `condition_occurrence` and `drug_exposure`.
# Visualise the result in a table to see the top concepts.

# Now create a reactable with all the concepts, can you explore and see if you
# can find counts for concepts you might be interested? e.g. acetaminophen,
# codeine, tramadol, ...

# To finalise the practice of today characterise the cdm. Stratify the results
# by sex, by 5 year bands age groups and apply as dateRange the studyPeriod that
# you decided in the previous steps. Visualise the results using a shiny app.
# Explore the shiny app, do you see any other analysis that are present in this
# shiny? Spend the rest of the practical exploring the shiny to see the
# characterisation of your cdm.
# Note characterisation of the database will take 4 to 8 minutes.

# DAY 2 - CODELISTS AND COHORTS----

# exposure codelists, create codelists for the following ingredients:
# `acetaminophen`, `ibuprofen`, `codeine` and `tramadol`.

# exposure codelists, create a second codelist that only includes drugs from a
# single ingredient and with 'oral' route.

# Let's now compare the codelists, can you find how many codes are present in
# codelists1 that are not present in codelist2 and the other way around?
# You will need some bespoke code to do that.

# Export the second group of codelists (single ingredient and oral route) to the
# exposures folder. In this case we provide the code, please change the variable
# 'codelist2' if you used a different name for the codelist object. As you can
# see we export the codelists as `csv` files as they are easier to review.
exportCodelist(
  x = codelist2, # change 'codelist2' if you used a different name for the codelist object
  path = here("Codelists", "exposures"),
  type = "csv"
)

# Create codelists for 2 indications of interest: 'headache' and 'cough' and
# save them in the `indications` folder. Use `csv` as type to export the
# codelists in the `here("Codelists", "indications")` folder.

# NOTE there are solutions for codelists (indications and exposures), manually
# copy those codelists into the relevant folders if you had any problem creating
# them.

# Read all the codelists that we have already created and some other that are
# provided.
exposures <- importCodelist(path = here("Codelists", "exposures"), type = "csv")
indications <- importCodelist(path = here("Codelists", "indications"), type = "csv")

# Let's create some cohorts now
# Create the `exposures` cohort:
# I1. Collapse records using a gap of 7 days.
# I2. Require at least 365 days of prior observation
# I3. Include only record during the study period that you previously defined.
# I4. Require a washout of 365 days (no use of the same drug, e.g. the
# acetaminophen cohort has 0 records of acetaminophen use in the last 365 days).

# Create a `pain` cohort:
# I1. Require at least 365 days of prior observation
# I2. Include only record during the study period that you previously defined.
# I3. Require a washout of 365 days (no record of pain in the last 365 days).

# Read all the codelists from `conditions`, `exposures`, `indications` and
# `medications` and combine them (codelist = c(codelist1, codelist2, ...))

# Create a base cohort for any of the different codelists.

# Use PhenotypeR on this cohort, create the shiny and explore it.
# Note phenotypeDiagnostics can take several minutes.

# If you have time you can create more indications or conditions cohorts, you
# can explore the shiny created with OmopSketch to see present concepts.

# DAY 3 ----
# read the codelists of interest
exposures <- importCodelist(path = here("Codelists", "exposures"), type = "csv")
medications <- importCodelist(path = here("Codelists", "medications"), type = "csv")
conditions <- importCodelist(path = here("Codelists", "conditions"), type = "csv")
indications <- importCodelist(path = here("Codelists", "indications"), type = "csv")

# Export in separate png files the attrition of each cohort in `exposures`
# table.

# See overlap between cohorts in `exposures` table. Visualise the result in a
# plot.

# Characterise the exposures cohort
# Include the following information:
# - demographics
# - any record of conditions any time prior
# - records of indication in the prior 7 days
# - records of indication in the prior 365 days
# - records of medications in the prior 365 days
# - number of visits in the prior 365 days
# Visualise the result in a gt table, the table should compare the 4 cohorts
# separated in 4 adjacent columns. Export the table into a word document.

# Large scale characterisation, perform a large scale characterisation on the
# `pain` cohort. Use the following windows: c(-90, -1), c(0, 0), c(1, 90); do it
# for the following tables: `drug_exposure` and `condition_occurrence`.
# Visualise the result in two tables, first a table that displays the top
# concepts per window; and then a second one that takes as a reference the
# c(-90, -1) window to compare the values across the different windows.

# Generate the denominator cohort to conduct an incidence prevalence analysis
# restrict to your study period, create 5 age groups using (20/25 years bands),
# [IMPORTANT, remember to create an `overall` group, e.g. c(0, 150)]
# and stratify by sex. Include in the analysis the individuals after 365 days of
# prior observation.

# Create the outcome cohort, as incidence prevalence needs alls the records so
# it applies prior observation and washouts correctly cretae the outcome cohort
# as:
# base cohort using the exposures codelist (end date to event_end_date)
# I1. Collapse records using a gap of 7 days
cdm$outcome <- conceptCohort(
  cdm = cdm,
  conceptSet = exposures,
  name = "outcome"
) |>
  collapseCohorts(gap = 7)

# Estimate yearly incidence of the drug exposure cohorts, using repeated events
# and a washout of 365 days.

# Visualise the incidence of the different drugs in a plot, facet by sex
# (horizontally) and age (vertically) and colour by exposure. Which is the
# exposure with a higher incidence? Do we see any interesting trend?

# Repeat the analyses for point and period prevalence if you have extra time.

# DAY 4 ----

# Indication, using the `exposures` cohort let's see if we can identify the
# different indications (create a base cohort just using the indications
# codelists), define 3 indication windows (c(0, 0), c(-7, 0), c(-30, 0)) and use
# as unknown indication the `condition_occurrence` and `observation` tables.
# Consider using mutually exclusive = FALSE as we have many indications.

# Visualise the results in a table.

# Characterise the treatments before and after pain record. Using the `pain`
# cohort, as anchor summarise the treatments received before and after the
# pain index date (cohort_start_date). Use the following windows: c(-30, -16);
# c(-15, -1); c(0, 0); c(1, 15); c(16, 30); c(31, 45); c(46, 60).

# Visualise the result in a plot

# Discontinuation
# First, we will do a proportion of patients covered analysis for the
# `outcome` (exposures) cohort in the 180 days after starting the treatment.
# Visualise the result in a plot colouring by exposure.

# Now we will do it as a survival analysis. Using also 180 days followUp time.
# Visualise the result in a plot colouring by exposure.

# Compare both results, are they similar? Which one do you think it is more
# informative?

# Drug restart, lets analyse what happens after they finish the treatment. First
# we will create the cohorts of interest. In this case acetaminophen, let's
# create a cohort with only the acetaminophen records, and another one with
# all the other exposures (switch)
cdm$acetaminophen <- subsetCohorts(
  cohort = cdm$outcome,
  cohortId = "acetaminophen",
  name = "acetaminophen"
)
cdm$switch <- subsetCohorts(
  cohort = cdm$outcome,
  cohortId = c("ibuprofen", "tramadol", "codeine"),
  name = "switch"
)

# Now let's analyse the drug restart patterns, in the next year how many
# individuals restart or switch to another treatment.

# disconnect ----
cdmDisconnect(cdm = cdm)
