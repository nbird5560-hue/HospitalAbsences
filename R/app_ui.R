#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyWidgets
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fluidPage(
      # We use a container-fluid to give the tabs maximum room
      titlePanel("Hospital Staff Absences Analysis Tool"),

      tabsetPanel(
        id = "main_tabs",
        type = "pills",
        # Page 1: Summary Statistics
        tabPanel(
          "Summary Statistics",
          icon = icon("list-alt"),
          br(),
          fluidRow(
            column(
              width = 12,
              mod_SummaryStatistics_ui("Summary_1")
            )
          )
        ),
        # Page 2: Exploratory trends
        tabPanel(
          "Trend Analysis",
          icon = icon("chart-line"),
          br(),
          fluidRow(
            column(width = 12, mod_TrendVisualizer_ui("TrendVisualizer_1"))
          )
        ),
        # Page 3: Data Selection and Models
        tabPanel(
          "Modeling & Prediction",
          icon = icon("gears"),
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 4,
              mod_DataSelection_ui("DataSelection_1"),
            ),
            mainPanel(
              width = 8,
              mod_ModelFitter_ui("ModelFitting_1"),
              mod_Predictor_ui("Predictor_1")
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "HospitalAbsences"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
