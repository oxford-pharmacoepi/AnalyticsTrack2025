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
snapshot <- summariseOmopSnapshot(cdm = cdm)
tableOmopSnapshot(result = snapshot)

# Summarise `observation_period` and visualise it in a table. Are there multiple
# records per person? Which is the mean follow up?
op <- summariseObservationPeriod(cdm$observation_period)
tableObservationPeriod(result = op)

# Characterise and summarise `condition_occurrence` and `drug_exposure`.
# Visualise the result in a table.
# Are there any records outside of observation? Should we be worried about it?
# Which are the source vocabularies used?
# Are there any non standard concept that we should worry about?
summaryTables <- summariseClinicalRecords(
  cdm = cdm,
  omopTableName = c("drug_exposure", "condition_occurrence")
)
tableClinicalRecords(result = summaryTables)

# Summarise missing values in `drug_exposure` and `condition_occurrence`.
# Visualise the result in a table. Any of the main columns have missing values?
# Are there any worring missing values?
missings <- summariseMissingData(
  cdm = cdm,
  omopTableName = c("drug_exposure", "condition_occurrence")
)
tableMissingData(result = missings)

# Summarise when individuals are in observation (use yearly or quarterly data so
# you can see clearly the trends). Visualise the result in a plot.
# When most of the individuals are in observation? Define a `studyPeriod`
# given the results of the 'in observation' analysis.
inObservation <- summariseInObservation(
  observationPeriod = cdm$observation_period,
  interval = "years"
)
plotInObservation(result = inObservation)
studyPeriod <- as.Date(c("2014-01-01", "2021-12-31"))

# Summarise the yearly record trends in `drug_exposure` and
# `condition_occurrence`. Visualise the result in a plot. Are there any worring
# trends? Did you see a change due to covid?
recordTrends <- summariseRecordCount(
  cdm = cdm,
  omopTableName = c("drug_exposure", "condition_occurrence"),
  interval = "years"
)
plotRecordCount(result = recordTrends, colour = "omop_table")

# Summarise the concept ids in `condition_occurrence` and `drug_exposure`.
# Visualise the result in a table to see the top concepts.
concepts <- summariseConceptIdCounts(
  cdm = cdm,
  omopTableName = c("condition_occurrence", "drug_exposure")
)
tableTopConceptCounts(result = concepts)

# Now create a reactable with all the concepts, can you explore and see if you
# can find counts for concepts you might be interested? e.g. acetaminophen,
# codeine, tramadol, ...
tableConceptIdCounts(result = concepts)

# To finalise the practice of today characterise the cdm. Stratify the results
# by sex, by 5 year bands age groups and apply as dateRange the studyPeriod that
# you decided in the previous steps. Visualise the results using a shiny app.
# Explore the shiny app, do you see any other analysis that are present in this
# shiny? Spend the rest of the practical exploring the shiny to see the
# characterisation of your cdm.
# Note characterisation of the database will take 4 to 8 minutes.
result <- databaseCharacteristics(
  cdm = cdm,
  sex = TRUE,
  ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf)),
  dateRange = studyPeriod
)
shinyCharacteristics(result = result, directory = here())

# DAY 2 - CODELISTS AND COHORTS----

# exposure codelists, create codelists for the following ingredients:
# `acetaminophen`, `ibuprofen`, `codeine` and `tramadol`.
codelist1 <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("acetaminophen", "ibuprofen", "codeine", "tramadol"),
  nameStyle = "{concept_name}"
)

# exposure codelists, create a second codelist that only includes drugs from a
# single ingredient and with 'oral' route.
codelist2 <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("acetaminophen", "ibuprofen", "codeine", "tramadol"),
  nameStyle = "{concept_name}",
  routeCategory = "oral",
  ingredientRange = c(1, 1)
)

# Let's now compare the codelists, can you find how many codes are present in
# codelists1 that are not present in codelist2 and the other way around?
# You will need some bespoke code to do that.
for (nm in names(codelist1)) {
  cat(paste0("Differences in `", nm, "` codelist:\n"))
  compareCodelists(codelist1 = codelist1[nm], codelist2 = codelist2[nm]) |>
    group_by(codelist) |>
    tally() |>
    print()
}

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
indications <- list(
  # you can customise more the codelist reviewing the codelist if you want
  headache = getCandidateCodes(cdm = cdm, keywords = "headache")$concept_id,
  cough = getCandidateCodes(cdm = cdm, keywords = "cough")$concept_id
)
exportCodelist(
  x = indications,
  path = here("Codelists", "indications"),
  type = "csv"
)

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
cdm$exposures <- conceptCohort(
  cdm = cdm,
  conceptSet = exposures,
  name = "exposures"
) |>
  # inclusion criteria 1
  collapseCohorts(gap = 7) |>
  # inclusion criteria 2
  requirePriorObservation(minPriorObservation = 365) |>
  # inclusion criteria 3
  requireInDateRange(dateRange = studyPeriod) |>
  # inclusion criteria 4
  requireConceptIntersect(
    cohortId = "acetaminophen",
    conceptSet = exposures["acetaminophen"],
    window = c(-365, -1),
    intersections = c(0, 0)
  ) |>
  requireConceptIntersect(
    cohortId = "ibuprofen",
    conceptSet = exposures["ibuprofen"],
    window = c(-365, -1),
    intersections = c(0, 0)
  ) |>
  requireConceptIntersect(
    cohortId = "codeine",
    conceptSet = exposures["codeine"],
    window = c(-365, -1),
    intersections = c(0, 0)
  ) |>
  requireConceptIntersect(
    cohortId = "tramadol",
    conceptSet = exposures["tramadol"],
    window = c(-365, -1),
    intersections = c(0, 0)
  )

# Create a `pain` cohort:
# I1. Require at least 365 days of prior observation
# I2. Include only record during the study period that you previously defined.
# I3. Require a washout of 365 days (no record of pain in the last 365 days).
cdm$pain <- conceptCohort(
  cdm = cdm,
  conceptSet = indications["pain"],
  name = "pain"
) |>
  # inclusion criteria 1
  requirePriorObservation(minPriorObservation = 365) |>
  # inclusion criteria 2
  requireInDateRange(dateRange = studyPeriod) |>
  # inclusion criteria 3
  requireConceptIntersect(
    conceptSet = indications["pain"],
    window = c(-365, -1),
    intersections = c(0, 0)
  )

# Read all the codelists from `conditions`, `exposures`, `indications` and
# `medications` and combine them (codelist = c(codelist1, codelist2, ...))
conditions <- importCodelist(path = here("Codelists", "conditions"), type = "csv")
medications <- importCodelist(path = here("Codelists", "medications"), type = "csv")
codelists <- c(conditions, medications, indications, exposures) |>
  newCodelist()

# Create a base cohort for any of the different codelists.
cdm$base_cohort <- conceptCohort(
  cdm = cdm,
  conceptSet = codelists,
  name = "base_cohort"
)

# Use PhenotypeR on this cohort, create the shiny and explore it.
# Note phenotypeDiagnostics can take several minutes.
Sys.time()
result <- phenotypeDiagnostics(cohort = cdm$base_cohort)
Sys.time()
shinyDiagnostics(result = result, directory = here()) # note this code will overwrite the prior shiny of OmopSketch

# If you have time you can create more indications or conditions cohorts, you
# can explore the shiny created with OmopSketch to see present concepts.

# DAY 3 ----
# read the codelists of interest
exposures <- importCodelist(path = here("Codelists", "exposures"), type = "csv")
indications <- importCodelist(path = here("Codelists", "indications"), type = "csv")

# Export in separate png files the attrition of each cohort in `exposures`
# table.
for (nm in names(exposures)) {
  cdm$exposures |>
    summariseCohortAttrition(cohortId = nm) |>
    plotCohortAttrition()
}

# See overlap between cohorts in `exposures` table. Visualise the result in a
# plot.
overlap <- summariseCohortOverlap(cohort = cdm$exposures)
plotCohortOverlap(result = overlap, uniqueCombinations = FALSE)

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
characteristics <- cdm$exposures |>
  summariseCharacteristics(
    demographics = TRUE,
    conceptIntersectFlag = list(
      "Indications prior 7 days" = list(
        conceptSet = indications, window = c(-7, 0)
      ),
      "Indications prior 30 days" = list(
        conceptSet = indications, window = c(-30, 0)
      )
    ),
    tableIntersectCount = list(
      "Number visits in prior year" = list(
        tableName = "visit_occurrence", window = c(-365, 0)
      )
    )
  )
tableCharacteristics(result = characteristics)

# Large scale characterisation, perform a large scale characterisation on the
# `pain` cohort. Use the following windows: c(-90, -1), c(0, 0), c(1, 90); do it
# for the following tables: `drug_exposure` and `condition_occurrence`.
# Visualise the result in two tables, first a table that displays the top
# concepts per window; and then a second one that takes as a reference the
# c(-90, -1) window to compare the values across the different windows.
lsc <- summariseLargeScaleCharacteristics(
  cohort = cdm$pain,
  window = list(c(-90, -1), c(0, 0), c(1, 90)),
  eventInWindow = "condition_occurrence",
  episodeInWindow = "drug_exposure"
)
tableTopLargeScaleCharacteristics(result = lsc)
tableLargeScaleCharacteristics(
  result = lsc,
  compareBy = "variable_level",
  smdReference = c("-90 to -1")
)

# Generate the denominator cohort to conduct an incidence prevalence analysis
# restrict to your study period, create 5 age groups using (20/25 years bands),
# [IMPORTANT, remember to create an `overall` group, e.g. c(0, 150)]
# and stratify by sex. Include in the analysis the individuals after 365 days of
# prior observation.
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = studyPeriod,
  ageGroup = list(c(0, 150), c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
  sex = c("Both", "Female", "Male"),
  daysPriorObservation = 365L
)

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
incidence <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  outcomeWashout = 365,
  repeatedEvents = TRUE,
  interval = "years"
)

# Visualise the incidence of the different drugs in a plot, facet by sex
# (horizontally) and age (vertically) and colour by exposure. Which is the
# exposure with a higher incidence? Do we see any interesting trend?
plotIncidence(
  result = incidence,
  colour = "outcome_cohort_name",
  facet = denominator_age_group ~ denominator_sex
)

# Repeat the analyses for point and period prevalence if you have extra time.
pointPrevalence <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  timePoint = "start"
)
plotPrevalence(
  result = pointPrevalence,
  colour = "outcome_cohort_name",
  facet = denominator_age_group ~ denominator_sex
)

periodPrevalence <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years"
)
plotPrevalence(
  result = periodPrevalence,
  colour = "outcome_cohort_name",
  facet = denominator_age_group ~ denominator_sex
)

# DAY 4 ----

# Indication, using the `exposures` cohort let's see if we can identify the
# different indications (create a base cohort just using the indications
# codelists), define 3 indication windows (c(0, 0), c(-7, 0), c(-30, 0)) and use
# as unknown indication the `condition_occurrence` and `observation` tables.
# Consider using mutually exclusive = FALSE as we have many indications.
cdm$indications <- conceptCohort(
  cdm = cdm,
  conceptSet = indications,
  name = "indications"
)
indicationSummary <- summariseIndication(
  cohort = cdm$exposures,
  indicationCohortName = "indications",
  indicationWindow = list(c(0, 0), c(-7, 0), c(-30, 0)),
  unknownIndicationTable = c("condition_occurrence", "observation"),
  mutuallyExclusive = FALSE
)

# Visualise the results in a table.
tableIndication(result = indicationSummary)

# Characterise the treatments before and after pain record. Using the `pain`
# cohort, as anchor summarise the treatments received before and after the
# pain index date (cohort_start_date). Use the following windows: c(-30, -16);
# c(-15, -1); c(0, 0); c(1, 15); c(16, 30); c(31, 45); c(46, 60).
treatments <- summariseTreatment(
  cohort = cdm$pain,
  window = list(c(-30, -16), c(-15, -1),  c(0, 0),  c(1, 15),  c(16, 30),  c(31, 45), c(46, 60)),
  treatmentCohortName = "exposures",
  mutuallyExclusive = FALSE
)

# Visualise the result in a plot
plotTreatment(result = treatments)

# Discontinuation
# First, we will do a proportion of patients covered analysis for the
# `outcome` (exposures) cohort in the 180 days after starting the treatment.
# Visualise the result in a plot colouring by exposure.
ppc <- summariseProportionOfPatientsCovered(
  cohort = cdm$outcome,
  followUpDays = 180
)
plotProportionOfPatientsCovered(result = ppc, facet = "cdm_name", colour = "cohort_name")

# Now we will do it as a survival analysis. Using also 180 days followUp time.
# Visualise the result in a plot colouring by exposure.
surv <- list()
for (nm in seq_along(exposures)) {
  surv[[nm]] <- estimateSingleEventSurvival(
    cdm = cdm,
    targetCohortTable = "outcome",
    targetCohortId = nm,
    outcomeCohortTable = "outcome",
    outcomeCohortId = nm,
    outcomeDateVariable = "cohort_end_date",
    outcomeWashout = Inf,
    followUpDays = 180
  )
}
surv <- bind(surv)
plotSurvival(result = surv, facet = "cdm_name", colour = "target_cohort")

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
drugRestart <- summariseDrugRestart(
  cohort = cdm$acetaminophen,
  switchCohortTable = "switch",
  followUpDays = 180
)
plotDrugRestart(result = drugRestart)

# disconnect ----
cdmDisconnect(cdm = cdm)
