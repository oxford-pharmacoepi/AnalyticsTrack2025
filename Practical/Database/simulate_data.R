
drugs <- list(
  list(
    # ibuprofen
    incidence = 1.3,
    concepts = c(19019071, 19019072, 19019073, 19019979, 19107275),
    ingredient = 1177480L,
    duration_median = 10,
    duration_iqr = c(5, 45),
    daily_dose_median = 1200,
    daily_dose_iqr = c(500, 2500),
    dose_age = -0.01,
    use_age = 0.01,
    relapse_30 = 0.20,
    indications = c(headache = 0.25, soft_pain = 0.75)
  ),
  list(
    # acetaminophen
    incidence = 3.1,
    concepts = c(19020053, 1125393, 1127433, 19107244, 19107439, 19107243),
    ingredient = 1125315L,
    duration_median = 7,
    duration_iqr = c(2, 30),
    daily_dose_median = 2500,
    daily_dose_iqr = c(500, 2500),
    dose_age = -0.01,
    use_age = 0.01,
    relapse_30 = 0.10,
    indications = c(headache = 0.30, soft_pain = 0.70)
  ),
  list(
    # codeine
    incidence = 0.3,
    concepts = c(40223078, 40223099, 40223115),
    ingredient = 1201620L,
    duration_median = 10,
    duration_iqr = c(3, 30),
    daily_dose_median = 100,
    daily_dose_iqr = c(50, 200),
    dose_age = -0.02,
    use_age = 0.02,
    relapse_30 = 0.01,
    indications = c(back_pain = 0.45, neck_pain = 0.25, osteoarthritis = 0.05, cough = 0.25)
  ),
  list(
    # tramadol
    incidence = 0.2,
    concepts = c(19107233, 19134116, 19135538),
    ingredient = 1103314L,
    duration_median = 10,
    duration_iqr = c(3, 30),
    daily_dose_median = 200,
    daily_dose_iqr = c(50, 400),
    dose_age = -0.02,
    use_age = 0.02,
    relapse_30 = 0.01,
    indications = c(back_pain = 0.5, neck_pain = 0.3, osteoarthritis = 0.15, cough = 0.05)
  )
)

set.seed(123456)
indications = list(
  headache = c("378253" = 0.75, "4113841" = 0.1, "4012515" = 0.15),
  soft_pain = c("78232" = 0.1, "4166666" = 0.1, "4128327" = 0.2, "442752" = 0.2, "4090553" = 0.1, "4115169" = 0.1, "4115170" = 0.1),
  back_pain = c("4046660" = 0.15, "194133" = 0.3, "4149974" = 0.3, "4133643" = 0.25),
  neck_pain = c("24134" = 0.55, "37310729" = 0.15, "37016631" = 0.3),
  osteoarthritis = c("80180" = 1),
  cough = c("4195384" = 0.15, "254761" = 0.65, "4167374" = 0.1, "4109381" = 0.05, "4048098" = 0.05)
) |>
  # add noise
  purrr::map(\(x) {
    x <- x + runif(n = length(x), min = 0, max = 0.2)
    x/sum(x)
  })

cdm <- omock::mockCdmFromDataset(datasetName = "synthea-heart-10k")

x <- cdm$observation_period |>
  PatientProfiles::addDateOfBirth() |>
  dplyr::mutate(
    age_start = as.numeric(observation_period_start_date - date_of_birth) / 365.25,
    age_end = as.numeric(observation_period_end_date - date_of_birth) / 365.25
  ) |>
  dplyr::select(person_id, age_start, age_end, observation_period_start_date, observation_period_end_date)

durations <- c(1, 2, 3, 4, 5, 7, 10, 14, 21, 28, 30, 45, 60, 90)
quantity <- c()

# add use of medicines
for (k in seq_along(drugs)) {
  nr <- rpois(1, drugs[[k]]$incidence * nrow(cdm$person))
  m <- drugs[[k]]$use_age
  concepts <- drugs[[k]]$concepts
  records <- dplyr::tibble(person_id = sample(x = x$person_id, size = nr, replace = TRUE)) |>
    dplyr::left_join(x, by = "person_id") |>
    dplyr::mutate(
      age_record = 1/m * log(runif(n = nr) * (exp(m*age_end) - exp(m*age_start)) + exp(m*age_start)),
      duration = 10,
      drug_exposure_start_date = observation_period_start_date + round((age_record - age_start) / 365.25),
      drug_exposure_end_date = drug_exposure_start_date + duration,
      drug_concept_id = sample(x = concepts, size = nr, replace = TRUE),
      drug_exposure_id = as.integer(dplyr::row_number() + max(cdm$drug_exposure$drug_exposure_id)),
      drug_exposure_start_datetime = drug_exposure_start_date,
      drug_exposure_end_datetime = drug_exposure_end_date,
      verbatim_end_date = verbatim_end_date,
      drug_type_concept_id = 32869L,
      stop_reason = NA_character_,
      refills = NA_integer_,
      quantity = 1, # needs calculation
    )

}

