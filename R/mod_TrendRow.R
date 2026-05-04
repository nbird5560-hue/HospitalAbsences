#' TrendRow UI Function
#'
#' @description \emph{mod_TrendRow.R} constructs and controls rows of the Trend Analysis
#' rows-input UI and their elements' functionalities.  Simple trend lines are
#' created by filtering the data for select the values of the drop-down inputs
#' and returned to the TrendVizualizer parent server for plotting. See in-line
#' comments for more info.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TrendRow_ui <- function(id, index) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("row_container"),
      style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
      span(tags$b(paste0("#", index)), style = "width: 30px;"),

      # Dynamic UI placeholders
      div(style = "flex: 1;", uiOutput(ns("ui_region"))),
      div(style = "flex: 1;", uiOutput(ns("ui_benchmark"))),
      div(style = "flex: 1;", uiOutput(ns("ui_staff"))),

      # UI for row count instance creation and termination
      actionButton(ns("add"), "", icon = icon("plus"), class = "btn-success btn-sm"),
      actionButton(ns("remove"), "", icon = icon("trash"), class = "btn-danger btn-sm")
    )
  )
}



#' TrendRow Server Functions
#'
#' @noRd

mod_TrendRow_server <- function(id, df_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper to get current data
    get_data <- reactive({
      d <- df_reactive()
      req(nrow(d) > 0)
      d
    })

    # Rendering the Select _____ Inputs
    output$ui_region <- renderUI({
      choices <- c("All Regions" = "", sort(unique(as.character(get_data()$nhse_region_name))))
      selectInput(ns("region"), NULL, choices = choices, width = "100%")
    })

    output$ui_benchmark <- renderUI({
      choices <- c("All Benchmarks" = "", sort(unique(as.character(get_data()$benchmark_group))))
      selectInput(ns("benchmark"), NULL, choices = choices, width = "100%")
    })

    output$ui_staff <- renderUI({
      choices <- c("All Staff" = "", sort(unique(as.character(get_data()$staff_group))))
      selectInput(ns("staff"), NULL, choices = choices, width = "100%")
    })

    # Calculation logic
    row_data <- reactive({
      data <- get_data()
      # Filter data for region, benchmark group, staff group
      if (shiny::isTruthy(input$region))    data <- data[data$nhse_region_name == input$region, ]
      if (shiny::isTruthy(input$benchmark)) data <- data[data$benchmark_group == input$benchmark, ]
      if (shiny::isTruthy(input$staff))     data <- data[data$staff_group == input$staff, ]

      # Conversion of month to strictly ordered leveled factor so that x axis
      # reads smoothly and in order
      data %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(mean_rate = mean(sickness_rate, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(trend_id = id, month = factor(month, levels = month.abb))
    })

    # returning to TrendVizualizer model
    return(list(
      data = row_data,
      add = reactive(input$add),
      remove = reactive(input$remove),
      label = reactive({
        # Trend labels for legend
        paste(input$region %||% "All", input$benchmark %||% "All", input$staff %||% "All", sep = " | ")
      })
    ))
  })
}
