# Generated by OmopViewer 0.3.0
# Be careful editing this file

server <- function(input, output, session) {
  # download raw data -----
  output$download_raw <- shiny::downloadHandler(
    filename = "results.csv",
    content = function(file) {
      data |>
        omopgenerics::bind() |>
        omopgenerics::exportSummarisedResult(fileName = file)
    }
  )
  # summarise_omop_snapshot -----
  ## get summarise_omop_snapshot data
  getSummariseOmopSnapshotData <- shiny::reactive({
    data[["summarise_omop_snapshot"]] |>
      dplyr::filter(
        .data$cdm_name %in% input$summarise_omop_snapshot_cdm_name,
        .data$variable_name %in% input$summarise_omop_snapshot_variable_name
      )
  })
  getSummariseOmopSnapshotTable <- shiny::reactive({
    getSummariseOmopSnapshotData() |>
      OmopSketch::tableOmopSnapshot()
  })
  output$summarise_omop_snapshot_table <- gt::render_gt({
    getSummariseOmopSnapshotTable()
  })
  output$summarise_omop_snapshot_table_download <- shiny::downloadHandler(
    filename = paste0("table_snapshot.", input$summarise_omop_snapshot_table_format),
    content = function(file) {
      gt::gtsave(getSummariseOmopSnapshotTable(), file)
    }
  )
  # summarise_observation_period -----
  ## get summarise_observation_period data
  getSummariseObservationPeriodData <- shiny::reactive({
    data[["summarise_observation_period"]] |>
      dplyr::filter(
        .data$cdm_name %in% input$summarise_observation_period_cdm_name,
        .data$variable_name %in% input$summarise_observation_period_variable_name,
        .data$estimate_name %in% input$summarise_observation_period_estimate_name
      ) |>
      omopgenerics::filterGroup(.data$observation_period_ordinal %in% input$summarise_observation_period_observation_period_ordinal) |>
      omopgenerics::filterStrata(
        .data$age_group %in% input$summarise_observation_period_age_group,
        .data$sex %in% input$summarise_observation_period_sex
      )
  })
  getSummariseObservationPeriodTable <- shiny::reactive({
    getSummariseObservationPeriodData() |>
      OmopSketch::tableObservationPeriod()
  })
  output$summarise_observation_period_table <- gt::render_gt({
    getSummariseObservationPeriodTable()
  })
  output$summarise_observation_period_table_download <- shiny::downloadHandler(
    filename = paste0("table_obsevation_period.", input$summarise_observation_period_table_format),
    content = function(file) {
      gt::gtsave(getSummariseObservationPeriodTable(), file)
    }
  )
  getSummariseObservationPeriodPlot <- shiny::reactive({
    getSummariseObservationPeriodData() |>
      OmopSketch::plotObservationPeriod(
        variableName = input$summarise_observation_period_plot_variable,
        plotType = input$summarise_observation_period_plot_plot_type,
        facet = input$summarise_observation_period_plot_facet,
        colour = input$summarise_observation_period_plot_colour
      )
  })
  output$summarise_observation_period_plot <- shiny::renderPlot({
    getSummariseObservationPeriodPlot()
  })
  output$summarise_observation_period_plot_download <- shiny::downloadHandler(
    filename = "plot_observation_period.png",
    content = function(file) {
      plt <- getSummariseObservationPeriodPlot()
      ggplot2::ggsave(
        filename = file,
        plot = plt,
        width = as.numeric(input$summarise_observation_period_plot_width),
        height = as.numeric(input$summarise_observation_period_plot_height),
        units = input$summarise_observation_period_plot_units,
        dpi = as.numeric(input$summarise_observation_period_plot_dpi)
      )
    }
  )
  # summarise_clinical_records -----
  ## get summarise_clinical_records data
  getSummariseClinicalRecordsData <- shiny::reactive({
    data[["summarise_clinical_records"]] |>
      dplyr::filter(
        .data$cdm_name %in% input$summarise_clinical_records_cdm_name,
        .data$variable_name %in% input$summarise_clinical_records_variable_name,
        .data$estimate_name %in% input$summarise_clinical_records_estimate_name
      ) |>
      omopgenerics::filterGroup(.data$omop_table %in% input$summarise_clinical_records_omop_table) |>
      omopgenerics::filterStrata(
        .data$age_group %in% input$summarise_clinical_records_age_group,
        .data$sex %in% input$summarise_clinical_records_sex
      ) |>
      omopgenerics::filterSettings(
        .data$study_period_end %in% input$summarise_clinical_records_study_period_end,
        .data$study_period_start %in% input$summarise_clinical_records_study_period_start
      )
  })
  getSummariseClinicalRecordsTidy <- shiny::reactive({
    tidyDT(getSummariseClinicalRecordsData(), input$summarise_clinical_records_tidy_columns, input$summarise_clinical_records_tidy_pivot_estimates)
  })
  output$summarise_clinical_records_tidy <- DT::renderDT({
    getSummariseClinicalRecordsTidy()
  })
  output$summarise_clinical_records_tidy_download <- shiny::downloadHandler(
    filename = "tidy_results.csv",
    content = function(file) {
      getSummariseClinicalRecordsData() |>
        omopgenerics::tidy() |>
        readr::write_csv(file = file)
    }
  )
  getSummariseClinicalRecordsTable <- shiny::reactive({
    getSummariseClinicalRecordsData() |>
      OmopSketch::tableClinicalRecords()
  })
  output$summarise_clinical_records_table <- gt::render_gt({
    getSummariseClinicalRecordsTable()
  })
  output$summarise_clinical_records_table_download <- shiny::downloadHandler(
    filename = paste0("table_clinical_records.", input$summarise_clinical_records_table_format),
    content = function(file) {
      gt::gtsave(getSummariseClinicalRecordsTable(), file)
    }
  )
  # summarise_record_count -----
  ## get summarise_record_count data
  getSummariseRecordCountData <- shiny::reactive({
    data[["summarise_record_count"]] |>
      dplyr::filter(
        .data$cdm_name %in% input$summarise_record_count_cdm_name,
        .data$variable_name %in% input$summarise_record_count_variable_name,
        .data$estimate_name %in% input$summarise_record_count_estimate_name
      ) |>
      omopgenerics::filterGroup(.data$omop_table %in% input$summarise_record_count_omop_table) |>
      omopgenerics::filterStrata(
        .data$age_group %in% input$summarise_record_count_age_group,
        .data$sex %in% input$summarise_record_count_sex
      ) |>
      omopgenerics::filterSettings(
        .data$interval %in% input$summarise_record_count_interval,
        .data$study_period_end %in% input$summarise_record_count_study_period_end,
        .data$study_period_start %in% input$summarise_record_count_study_period_start
      )
  })
  getSummariseRecordCountTidy <- shiny::reactive({
    tidyDT(getSummariseRecordCountData(), input$summarise_record_count_tidy_columns, input$summarise_record_count_tidy_pivot_estimates)
  })
  output$summarise_record_count_tidy <- DT::renderDT({
    getSummariseRecordCountTidy()
  })
  output$summarise_record_count_tidy_download <- shiny::downloadHandler(
    filename = "tidy_results.csv",
    content = function(file) {
      getSummariseRecordCountData() |>
        omopgenerics::tidy() |>
        readr::write_csv(file = file)
    }
  )
  getSummariseRecordCountPlot <- shiny::reactive({
    getSummariseRecordCountData() |>
      OmopSketch::plotRecordCount(
        facet = input$summarise_record_count_plot_facet,
        colour = input$summarise_record_count_plot_colour
      )
  })
  output$summarise_record_count_plot <- shiny::renderPlot({
    getSummariseRecordCountPlot()
  })
  output$summarise_record_count_plot_download <- shiny::downloadHandler(
    filename = "plot_record_count.png",
    content = function(file) {
      plt <- getSummariseRecordCountPlot()
      ggplot2::ggsave(
        filename = file,
        plot = plt,
        width = as.numeric(input$summarise_record_count_plot_width),
        height = as.numeric(input$summarise_record_count_plot_height),
        units = input$summarise_record_count_plot_units,
        dpi = as.numeric(input$summarise_record_count_plot_dpi)
      )
    }
  )
  # summarise_missing_data -----
  ## get summarise_missing_data data
  getSummariseMissingDataData <- shiny::reactive({
    data[["summarise_missing_data"]] |>
      dplyr::filter(
        .data$cdm_name %in% input$summarise_missing_data_cdm_name,
        .data$variable_name %in% input$summarise_missing_data_variable_name,
        .data$estimate_name %in% input$summarise_missing_data_estimate_name
      ) |>
      omopgenerics::filterGroup(.data$omop_table %in% input$summarise_missing_data_omop_table) |>
      omopgenerics::filterStrata(
        .data$age_group %in% input$summarise_missing_data_age_group,
        .data$sex %in% input$summarise_missing_data_sex
      ) |>
      omopgenerics::filterSettings(
        .data$study_period_end %in% input$summarise_missing_data_study_period_end,
        .data$study_period_start %in% input$summarise_missing_data_study_period_start
      )
  })
  getSummariseMissingDataTidy <- shiny::reactive({
    tidyDT(getSummariseMissingDataData(), input$summarise_missing_data_tidy_columns, input$summarise_missing_data_tidy_pivot_estimates)
  })
  output$summarise_missing_data_tidy <- DT::renderDT({
    getSummariseMissingDataTidy()
  })
  output$summarise_missing_data_tidy_download <- shiny::downloadHandler(
    filename = "tidy_results.csv",
    content = function(file) {
      getSummariseMissingDataData() |>
        omopgenerics::tidy() |>
        readr::write_csv(file = file)
    }
  )
  getSummariseMissingDataTable <- shiny::reactive({
    getSummariseMissingDataData() |>
      OmopSketch::tableMissingData()
  })
  output$summarise_missing_data_table <- gt::render_gt({
    getSummariseMissingDataTable()
  })
  output$summarise_missing_data_table_download <- shiny::downloadHandler(
    filename = paste0("table_missing_data.", input$summarise_missing_data_table_format),
    content = function(file) {
      gt::gtsave(getSummariseMissingDataTable(), file)
    }
  )
  # summarise_in_observation -----
  ## get summarise_in_observation data
  getSummariseInObservationData <- shiny::reactive({
    data[["summarise_in_observation"]] |>
      dplyr::filter(
        .data$cdm_name %in% input$summarise_in_observation_cdm_name,
        .data$variable_name %in% input$summarise_in_observation_variable_name,
        .data$estimate_name %in% input$summarise_in_observation_estimate_name
      ) |>
      omopgenerics::filterGroup(.data$omop_table %in% input$summarise_in_observation_omop_table) |>
      omopgenerics::filterStrata(
        .data$sex %in% input$summarise_in_observation_sex,
        .data$age_group %in% input$summarise_in_observation_age_group
      ) |>
      omopgenerics::filterSettings(
        .data$interval %in% input$summarise_in_observation_interval,
        .data$study_period_end %in% input$summarise_in_observation_study_period_end,
        .data$study_period_start %in% input$summarise_in_observation_study_period_start
      )
  })
  getSummariseInObservationTidy <- shiny::reactive({
    tidyDT(getSummariseInObservationData(), input$summarise_in_observation_tidy_columns, input$summarise_in_observation_tidy_pivot_estimates)
  })
  output$summarise_in_observation_tidy <- DT::renderDT({
    getSummariseInObservationTidy()
  })
  output$summarise_in_observation_tidy_download <- shiny::downloadHandler(
    filename = "tidy_results.csv",
    content = function(file) {
      getSummariseInObservationData() |>
        omopgenerics::tidy() |>
        readr::write_csv(file = file)
    }
  )
  getSummariseInObservationPlot <- shiny::reactive({
    getSummariseInObservationData() |>
      dplyr::filter(.data$variable_name == input$summarise_in_observation_plot_variable) |>
      OmopSketch::plotInObservation(
        facet = input$summarise_in_observation_plot_facet,
        colour = input$summarise_in_observation_plot_colour
      )
  })
  output$summarise_in_observation_plot <- shiny::renderPlot({
    getSummariseInObservationPlot()
  })
  output$summarise_in_observation_plot_download <- shiny::downloadHandler(
    filename = "plot_in_observation.png",
    content = function(file) {
      plt <- getSummariseInObservationPlot()
      ggplot2::ggsave(
        filename = file,
        plot = plt,
        width = as.numeric(input$summarise_in_observation_plot_width),
        height = as.numeric(input$summarise_in_observation_plot_height),
        units = input$summarise_in_observation_plot_units,
        dpi = as.numeric(input$summarise_in_observation_plot_dpi)
      )
    }
  )
  # summarise_characteristics -----
  ## get summarise_characteristics data
  getSummariseCharacteristicsData <- shiny::reactive({
    data[["summarise_characteristics"]] |>
      dplyr::filter(
        .data$cdm_name %in% input$summarise_characteristics_cdm_name,
        .data$variable_name %in% input$summarise_characteristics_variable_name,
        .data$estimate_name %in% input$summarise_characteristics_estimate_name
      ) |>
      omopgenerics::filterGroup(.data$cohort_name %in% input$summarise_characteristics_cohort_name) |>
      omopgenerics::filterStrata(.data$sex %in% input$summarise_characteristics_sex)
  })
  getSummariseCharacteristicsTable <- shiny::reactive({
    getSummariseCharacteristicsData() |>
      CohortCharacteristics::tableCharacteristics(
        header = input$summarise_characteristics_table_header,
        groupColumn = input$summarise_characteristics_table_group_column,
        hide = input$summarise_characteristics_table_hide
      )
  })
  output$summarise_characteristics_table <- gt::render_gt({
    getSummariseCharacteristicsTable()
  })
  output$summarise_characteristics_table_download <- shiny::downloadHandler(
    filename = paste0("table_characteristics.", input$summarise_characteristics_table_format),
    content = function(file) {
      gt::gtsave(getSummariseCharacteristicsTable(), file)
    }
  )
  getSummariseCharacteristicsPlot <- shiny::reactive({
    getSummariseCharacteristicsData() |>
      dplyr::filter(.data$variable_name == input$summarise_characteristics_plot_variable) |>
      CohortCharacteristics::plotCharacteristics(
        plotType = input$summarise_characteristics_plot_plot_type,
        facet = input$summarise_characteristics_plot_facet,
        colour = input$summarise_characteristics_plot_colour
      )
  })
  output$summarise_characteristics_plot <- shiny::renderPlot({
    getSummariseCharacteristicsPlot()
  })
  output$summarise_characteristics_plot_download <- shiny::downloadHandler(
    filename = "plot_characteristics.png",
    content = function(file) {
      plt <- getSummariseCharacteristicsPlot()
      ggplot2::ggsave(
        filename = file,
        plot = plt,
        width = as.numeric(input$summarise_characteristics_plot_width),
        height = as.numeric(input$summarise_characteristics_plot_height),
        units = input$summarise_characteristics_plot_units,
        dpi = as.numeric(input$summarise_characteristics_plot_dpi)
      )
    }
  )
  # summarise_table_quality -----
  ## get summarise_table_quality data
  getSummariseTableQualityData <- shiny::reactive({
    data[["summarise_table_quality"]] |>
      dplyr::filter(
        .data$cdm_name %in% input$summarise_table_quality_cdm_name,
        .data$variable_name %in% input$summarise_table_quality_variable_name,
        .data$estimate_name %in% input$summarise_table_quality_estimate_name
      ) |>
      omopgenerics::filterGroup(.data$omop_table %in% input$summarise_table_quality_omop_table) |>
      omopgenerics::filterStrata(
        .data$sex %in% input$summarise_table_quality_sex,
        .data$age_group %in% input$summarise_table_quality_age_group
      ) |>
      omopgenerics::filterSettings(
        .data$study_period_end %in% input$summarise_table_quality_study_period_end,
        .data$study_period_start %in% input$summarise_table_quality_study_period_start
      )
  })
  getSummariseTableQualityTidy <- shiny::reactive({
    tidyDT(getSummariseTableQualityData(), input$summarise_table_quality_tidy_columns, input$summarise_table_quality_tidy_pivot_estimates)
  })
  output$summarise_table_quality_tidy <- DT::renderDT({
    getSummariseTableQualityTidy()
  })
  output$summarise_table_quality_tidy_download <- shiny::downloadHandler(
    filename = "tidy_results.csv",
    content = function(file) {
      getSummariseTableQualityData() |>
        omopgenerics::tidy() |>
        readr::write_csv(file = file)
    }
  )
  getSummariseTableQualityTable <- shiny::reactive({
    getSummariseTableQualityData() |>
      simpleTable(
        header = input$summarise_table_quality_table_header,
        group = input$summarise_table_quality_table_group_column,
        hide = input$summarise_table_quality_table_hide
      )
  })
  output$summarise_table_quality_table <- gt::render_gt({
    getSummariseTableQualityTable()
  })
  output$summarise_table_quality_table_download <- shiny::downloadHandler(
    filename = paste0("table.", input$summarise_table_quality_table_format),
    content = function(file) {
      gt::gtsave(getSummariseTableQualityTable(), file)
    }
  )
}
