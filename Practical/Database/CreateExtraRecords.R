getDist <- function(values, p) {
  probs <- dnorm(x = seq_along(values), mean = p[1], sd = p[2])
  probs / sum(probs)
}
getQ <- function(values, probs, q) {
  values[min(which(cumsum(probs) >= q))]
}
err <- function(x, y) {
  sum(abs(x[!is.na(y)] - y[!is.na(y)]))
}
getProbabilities <- function(values,
                             expected) {
  my_error_fun <- function(p) {
    probs <- getDist(values = values, p = p)
    c(0.05, 0.25, 0.50, 0.75, 0.95) |>
      purrr::map_dbl(\(x) getQ(values = values, probs = probs, q = x)) |>
      err(expected)
  }


  result <- optim(par = c(length(values)/2, 1), fn = my_error_fun)

  p <- result$par
  probs <- getDist(values = values, p = p)
  c(0.05, 0.25, 0.50, 0.75, 0.95) |>
    purrr::map_dbl(\(x) getQ(values = values, probs = probs, q = x)) |>
    print()
  print(expected)

  getDist(values = values, p = result$par)
}
generateDrugExposure <- function(x, nr, m, durations, probsD, concepts, relapse) {
  if (nr == length(x$person_id)) {
    y <- x
  } else {
    y <- dplyr::tibble(person_id = sample(x = x$person_id, size = nr, replace = TRUE)) |>
      dplyr::left_join(x, by = "person_id")
  }
  y |>
    dplyr::mutate(
      age_record = 1/m * log(runif(n = nr) * (exp(m*age_end) - exp(m*age_start)) + exp(m*age_start)),
      duration = sample(x = durations, size = nr, replace = TRUE, prob = probsD),
      drug_exposure_start_date = observation_period_start_date + round((age_record - age_start) * 365.25),
      drug_exposure_end_date = drug_exposure_start_date + duration - 1,
      drug_concept_id = sample(x = concepts, size = nr, replace = TRUE),
      relapse = dplyr::if_else(runif(n = nr) < relapse, 1, 0)
    )
}

drugs <- list(
  list(
    # ibuprofen
    incidence = 1.3,
    concepts = c(19019071, 19019072, 19019073, 19019979, 19107275),
    ingredient = 1177480L,
    duration_params = c(3, 5, 10, 45, 60),
    quantity_params = c(NA, 500, 1200, 2500, NA),
    use_age = 0.01,
    relapse_30 = 0.20,
    indications = c(headache = 0.25, soft_pain = 0.75)
  ),
  list(
    # acetaminophen
    incidence = 3.1,
    concepts = c(19020053, 1125393, 1127433, 19107244, 19107439, 19107243),
    ingredient = 1125315L,
    duration_params = c(1, 2, 7, 30, 45),
    quantity_params = c(NA, 500, 2500, 3500, NA),
    use_age = 0.01,
    relapse_30 = 0.10,
    indications = c(headache = 0.30, soft_pain = 0.70)
  ),
  list(
    # codeine
    incidence = 0.3,
    concepts = c(40223078, 40223099, 40223115),
    ingredient = 1201620L,
    duration_params = c(2, 3, 10, 30, 45),
    quantity_params = c(NA, 50, 100, 200, NA),
    use_age = 0.02,
    relapse_30 = 0.01,
    indications = c(back_pain = 0.45, neck_pain = 0.25, osteoarthritis = 0.05, cough = 0.25)
  ),
  list(
    # tramadol
    incidence = 0.2,
    concepts = c(19107233, 19134116, 19135538),
    ingredient = 1103314L,
    duration_params = c(4, 5, 14, 30, 45),
    quantity_params = c(NA, 50, 200, 400, NA),
    use_age = 0.02,
    relapse_30 = 0.01,
    indications = c(back_pain = 0.5, neck_pain = 0.3, osteoarthritis = 0.15, cough = 0.05)
  )
)

set.seed(123456)
ind = list(
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

cdm <- omock::mockCdmFromDataset(datasetName = "synthea-covid19-200k")

x <- cdm$observation_period |>
  PatientProfiles::addDateOfBirth() |>
  dplyr::mutate(
    age_start = as.numeric(observation_period_start_date - date_of_birth) / 365.25,
    age_end = as.numeric(observation_period_end_date - date_of_birth) / 365.25
  ) |>
  dplyr::select(person_id, age_start, age_end, observation_period_start_date, observation_period_end_date, date_of_birth)

durations <- c(1, 2, 3, 4, 5, 7, 10, 14, 21, 28, 30, 45, 60, 90)
quantities <- c(1, 2, 4, 7, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
dds <- seq(25, 5000, by = 25)

drugExposure <- list()
conditionOccurrence <- list()

for (k in seq_along(drugs)) {
  dk <- drugs[[k]]
  probsD <- getProbabilities(values = durations, expected = dk$duration_params)

  records <- generateDrugExposure(
    x = x,
    nr = rpois(1, dk$incidence * nrow(cdm$person)),
    m = dk$use_age,
    durations = durations,
    probsD = probsD,
    concepts = dk$concepts,
    relapse = dk$relapse_30
  )
  records$first <- 1

  while(sum(records$relapse) > 0) {
    new_records <- records |>
      dplyr::filter(relapse == 1) |>
      dplyr::select("person_id", "date_of_birth", "observation_period_start_date" = "drug_exposure_end_date") |>
      dplyr::mutate(
        observation_period_end_date = observation_period_start_date + 30,
        age_start = as.numeric(observation_period_start_date - date_of_birth) / 365.25,
        age_end = as.numeric(observation_period_end_date - date_of_birth) / 365.25
      ) |>
      generateDrugExposure(
        nr = sum(records$relapse),
        m = dk$use_age,
        durations = durations,
        probsD = probsD,
        concepts = dk$concepts,
        relapse = dk$relapse_30
      )

    new_records$first <- 0
    records$relapse <- 0

    records <- dplyr::union_all(records, new_records)
  }

  # indications
  indicationRecords <- records |>
    dplyr::filter(first == 1) |>
    dplyr::select("person_id", "drug_exposure_start_date") |>
    dplyr::mutate(
      indication = sample(x = names(dk$indications), size = dplyr::n(), replace = TRUE, prob = unname(dk$indications)),
      condition_start_date = drug_exposure_start_date - round(rexp(dplyr::n(), rate = 1/30)) - 1,
      condition_concept_id = dplyr::case_when(
        indication == "headache" ~ sample(x = names(ind$headache), size = dplyr::n(), replace = TRUE, unname(ind$headache)),
        indication == "soft_pain" ~ sample(x = names(ind$soft_pain), size = dplyr::n(), replace = TRUE, unname(ind$soft_pain)),
        indication == "back_pain" ~ sample(x = names(ind$back_pain), size = dplyr::n(), replace = TRUE, unname(ind$back_pain)),
        indication == "neck_pain" ~ sample(x = names(ind$neck_pain), size = dplyr::n(), replace = TRUE, unname(ind$neck_pain)),
        indication == "osteoarthritis" ~ sample(x = names(ind$osteoarthritis), size = dplyr::n(), replace = TRUE, unname(ind$osteoarthritis)),
        indication == "cough" ~ sample(x = names(ind$cough), size = dplyr::n(), replace = TRUE, unname(ind$cough))
      ) |>
        as.integer()
    )

  print(summary(indicationRecords$condition_concept_id))

  conditionOccurrence[[k]] <- indicationRecords |>
    dplyr::select(dplyr::any_of(omopgenerics::omopColumns("condition_occurrence")))

  # calculate dosage
  doses <- cdm$drug_strength |>
    dplyr::filter(drug_concept_id %in% dk$concepts) |>
    dplyr::select("drug_concept_id", "amount_value")
  probDD <- getProbabilities(values = dds, expected = dk$quantity_params)

  records <- records |>
    dplyr::left_join(doses, by = "drug_concept_id") |>
    dplyr::mutate(
      dd = sample(x = dds, size = nrow(records), replace = TRUE, prob = probDD),
      quantity = dd * duration / amount_value,
      quantity = purrr::map_dbl(quantity, \(x) quantities[which.min(abs(quantities - x))]),
      days_supply = as.integer(duration)
    )

  print(summary(records$quantity))

  drugExposure[[k]] <- records |>
    dplyr::select(dplyr::any_of(omopgenerics::omopColumns("drug_exposure")))
}

drugExposure |>
  dplyr::bind_rows() |>
  readr::write_csv(file = here::here("Database", "drug_exposure_extra_rows.csv"))

femaleCond <- c(4128327, 4113841)
otherCond <- ind |> purrr::map(\(x) names(x)) |> unlist() |> as.integer()
females <- cdm$person |>
  dplyr::filter(gender_concept_id == 8532) |>
  dplyr::pull(person_id)

conditionOccurrence |>
  dplyr::bind_rows() |>
  dplyr::mutate(condition_concept_id = dplyr::if_else(
    person_id %in% females & condition_concept_id %in% femaleCond,
    sample(x = otherCond, size = dplyr::n(), replace = TRUE),
    condition_concept_id
  )) |>
  readr::write_csv(file = here::here("Database", "condition_occurrence_extra_rows.csv"))
