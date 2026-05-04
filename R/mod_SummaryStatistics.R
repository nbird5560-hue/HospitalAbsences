#' SummaryStatistics UI Function
#'
#' @description \file{mod_SummaryStatistics.R} provides a dynamically managed
#' table of summary metrics. This version displays the summary table above
#' the filter controls for immediate feedback.  See in-line comments for more info.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_SummaryStatistics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      # Header and Global Controls
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
          h3(icon("table"), "Dynamic Summary Statistics Comparison Tool"),
          actionButton(ns("clear_all"), "Clear All", icon = icon("broom"), class = "btn-warning")),

      # Results Table
      div(style = "background: white; padding: 10px; border-radius: 4px; border: 1px solid #ddd; margin-bottom: 20px;",
          DT::DTOutput(ns("summary_table"))),

      hr(),
      h4("Summary Stat Subpopulation Definitions:"),

      # Headers for the TrendRow inputs
      div(style = "display: flex; gap: 10px; font-weight: bold; margin-bottom: 5px; padding-left: 40px;",
          div(style = "flex: 1;", "NHSE Region"),
          div(style = "flex: 1;", "Benchmark Group"),
          div(style = "flex: 1;", "Staff Group"),
          div(style = "width: 80px;", "")
      ),

      # Container for dynamic rows (filing cabinet)
      tags$div(id = ns("rows_placeholder"))
    )
  )
}

#' SummaryStatistics Server Functions
#'
#' @noRd
mod_SummaryStatistics_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Tracking unique input IDs
    active_rows <- reactiveVal(character(0))
    row_outputs <- reactiveValues()
    counter     <- reactiveVal(0)

    # Add row and fingerprint it
    add_row <- function() {
      new_id <- paste0("stat_row_", counter() + 1)
      counter(counter() + 1)

      insertUI(
        selector = paste0("#", ns("rows_placeholder")),
        where = "beforeEnd",
        ui = mod_TrendRow_ui(ns(new_id), length(active_rows()) + 1)
      )

      res <- mod_TrendRow_server(new_id, df)
      row_outputs[[new_id]] <- res

      observeEvent(res$add(), { add_row() })
      observeEvent(res$remove(), { remove_row(new_id) })

      active_rows(c(active_rows(), new_id))
    }

    # Remove row and terminate row instance
    remove_row <- function(row_id) {
      if (length(active_rows()) <= 1) return()
      removeUI(selector = paste0("#", ns(paste0(row_id, "-row_container"))))
      active_rows(active_rows()[active_rows() != row_id])
    }

    # Clear all button
    observeEvent(input$clear_all, {
      purrr::walk(active_rows(), function(row_id) {
        removeUI(selector = paste0("#", ns(paste0(row_id, "-row_container"))))
      })
      active_rows(character(0))
      add_row()
    })

    # Reinitialize first row
    observeEvent(df(), {
      if(length(active_rows()) == 0) add_row()
    }, once = TRUE)

    # Data Aggregation and Summary Stats
    combined_stats <- reactive({
      req(length(active_rows()) > 0)

      purrr::map_df(active_rows(), function(r_id) {
        req(row_outputs[[r_id]])

        d <- row_outputs[[r_id]]$data()

        # Pull specific inputs from the row namespace
        reg <- input[[paste0(r_id, "-region")]]
        ben <- input[[paste0(r_id, "-benchmark")]]
        stf <- input[[paste0(r_id, "-staff")]]

        # Handles population-level cases
        reg_lbl <- if(shiny::isTruthy(reg)) reg else "R: Population"
        ben_lbl <- if(shiny::isTruthy(ben)) ben else "B: Population"
        stf_lbl <- if(shiny::isTruthy(stf)) stf else "S: Population"

        # Constructing label for Filter Group
        lbl <- paste(reg_lbl, ben_lbl, stf_lbl, sep = " | ")

        # If all 3 are population level, collapse to "Population Level"
        if (lbl == "R: Population | B: Population | S: Population") {
          lbl <- "Population Level"
        }
        # Given Populated df, return summary stat slice
        if(nrow(d) > 0) {
          d %>%
            dplyr::summarise(
              `Filter Group` = lbl,
              `Months Observed` = dplyr::n(),
              `Avg Sickness Rate (%)` = mean(mean_rate, na.rm = TRUE),
              `Median Rate (%)` = stats::median(mean_rate, na.rm = TRUE),
              `Std. Dev` = stats::sd(mean_rate, na.rm = TRUE),
              `Min Rate` = min(mean_rate, na.rm = TRUE),
              `Max Rate` = max(mean_rate, na.rm = TRUE)
            )
        }
      })
    })

    # Combines slices and renders output
    output$summary_table <- DT::renderDT({
      req(combined_stats())

      DT::datatable(
        combined_stats(),
        rownames = FALSE,
        options = list(
          dom = 't',
          scrollX = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        )
      ) %>%
        DT::formatRound(columns = 3:7, digits = 4)
    })
  })
}
