#' DataSelection UI Function
#'
#' @description \emph{mod_DataSelection.R} is the brain of the \strong{Data Selection Panel}
#' which allows the user to filter data included within the scope of the
#' Modeling and Prediction tab.  See in-line comments for more info.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import shinyWidgets
#' @import dplyr
mod_DataSelection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h2("1. Data Selection"),
      selectInput(
        inputId = ns("data_scope_selection"),
        label = "Compare at Population level or by Staffing Group:",
        choices = c("By Staff Group" = "by_staff", "All Staff Groups" = "all_staff"),
        selected = "by_staff"
      ),
      hr(),
      h4("Select Model Variables:"),
      mod_var_select_ui(ns("region_select"), var_dictionary[["nhse_region_name"]], TRUE),
      mod_var_select_ui(ns("benchmark_select"), var_dictionary[["benchmark_group"]], TRUE),

      # Conditional panel for staff group
      conditionalPanel(
        condition = sprintf("input['%s'] == 'by_staff'", ns("data_scope_selection")),
        h4("Incorporate Random Effect:"),
        mod_var_select_ui(ns("staff_select"), var_dictionary[["staff_group"]], TRUE)
      )
    )
  )
}

#' DataSelection Server Functions
#'
#' @noRd
mod_DataSelection_server <- function(id, raw_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initializations
    region_res <- mod_var_select_server("region_select", raw_data, "nhse_region_name")
    bench_res  <- mod_var_select_server("benchmark_select", raw_data, "benchmark_group")
    staff_res  <- mod_var_select_server("staff_select", raw_data, "staff_group")
    staff_bench_res <- mod_var_select_server("staff_benchmark_select", raw_data, "bench_region")

    # Rendering conditional interaction UI
    output$staff_container <- renderUI({
      if (input$data_scope_selection == "by_staff") {
        tagList(
          h4("Incorporate Random Effect:"),
          mod_var_select_ui(ns("staff_select"), var_dictionary[["staff_group"]], TRUE)
        )
      } else NULL
    })

    # Reactive data processing
    processed_data <- reactive({
      req(raw_data(), input$data_scope_selection)
      df <- raw_data()

      # Data scope filtration
      if (input$data_scope_selection == "all_staff") {
        df <- df %>% filter(staff_group == "All staff groups")
      } else {
        df <- df %>% filter(staff_group != "All staff groups")
      }


      # Filteration of factor levels as specified by drop down inputs
      if (isTRUE(region_res$include())) {
        levels <- region_res$selected_levels()
        if (!is.null(levels)) {
          df <- df %>% filter(nhse_region_name %in% levels)
        }
      }

      if (isTRUE(bench_res$include())) {
        levels <- bench_res$selected_levels()
        if (!is.null(levels)) {
          df <- df %>% filter(benchmark_group %in% levels)
        }
      }

      if (isTRUE(staff_res$include())) {
        levels <- staff_res$selected_levels()
        if (!is.null(levels)) {
          df <- df %>% filter(staff_group %in% levels)
        }
      }
      resp <- "hours_lost"
      # Determining which columns to keep of optional vars
      optvars <- c()
      if (isTRUE(region_res$include())) optvars <- c(optvars, "nhse_region_name")
      if (isTRUE(bench_res$include()))  optvars <- c(optvars, "benchmark_group")
      if (isTRUE(staff_res$include()))  optvars <- c(optvars, "staff_group")

      # Conditional Inclusion
      if (isTRUE(region_res$include()) && isTRUE(bench_res$include()) && isTRUE(staff_bench_res$include())) {
        optvars <- c(optvars, "bench_region")
      }

      # Formulaic handling cases
      attr(df[[resp]], "select") <- "response"
      if ("staff_group" %in% names(df)) attr(df[["staff_group"]], "select") <- "grouping"

      # Module-specific quick data compatibility reform
      df <- df %>% mutate(across(where(is.character), ~as.factor(.x)),
                          month = as.numeric(month))

      # Conditional inclusion of random effect variable
      if(isTRUE(staff_res$include())) {
        sg <- "staff_group"
      } else {
        sg <- NULL
      }

      # Applying attributes with a helper function
      df <- apply_attr(df,"select","first", c("month", "year","hours_available", sg))
      df <- apply_attr(df,"select","second", c("month", "year","month_year","hours_available", "timestep", sg))

      list(
        df = df,
        optvars = optvars
      )
    })
    return(processed_data)
  })
}
