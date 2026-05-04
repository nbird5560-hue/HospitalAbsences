#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#'
#' @noRd
app_server <- function(input, output, session) {

  # Importing data
  dat_raw <- reactive({ import_data() })

  onStop(function() {
    message("App stopping. Shutting down parallel workers...")
    # Shut down the future plan to free up the CPU cores/RAM
    future::plan(future::sequential)
  })

  processed_data <- mod_DataSelection_server("DataSelection_1", raw_data = dat_raw)

  fitted_models <- mod_ModelFitter_server("ModelFitting_1", .data = processed_data)

  observe({
    req(fitted_models())
    message("Models have been fitted successfully!")
  })

  mod_Predictor_server("Predictor_1", model_data = fitted_models)

  trends_df <- reactive({
    req(dat_raw())
    dat_raw() %>% dplyr::filter(staff_group != "All staff groups") })
  mod_TrendVisualizer_server("TrendVisualizer_1", df = trends_df)

  processed_trends <- mod_SummaryStatistics_server("Summary_1", trends_df)


}
