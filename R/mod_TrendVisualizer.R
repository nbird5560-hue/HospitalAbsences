#' TrendVisualizer UI Function
#'
#' @description \emph{mod_TrendVisualizer.R} is the brain which orchestrates the
#' Trend Analysis tab's UI controls and manipulates the trend row UI instances.
#' See in-line comments for more info.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TrendVisualizer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      # Title
      div(style = "display: flex; justify-content: space-between; align-items: center;",
          h3("Trend Comparison (Historical Data)"),
          actionButton(ns("clear_all"), "Clear All", icon = icon("broom"), class = "btn-warning")),
      # Plot rendering
      plotOutput(ns("trend_plot"), height = "400px"),
      hr(),

      # Headers for the controls below
      div(style = "display: flex; gap: 10px; font-weight: bold; margin-bottom: 5px; padding-left: 40px;",
          div(style = "flex: 1;", "NHSE Region"),
          div(style = "flex: 1;", "Benchmark Group"),
          div(style = "flex: 1;", "Staff Group"),
          div(style = "width: 80px;", "")
      ),

      # Rendering rows
      tags$div(id = ns("rows_placeholder"))
    )
  )
}
#' TrendVisualizer Server Functions
#'
#' @noRd
mod_TrendVisualizer_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # For tracking row IDs and their data
    active_rows <- reactiveVal(character(0)) # keeps track of visible rows
    row_outputs <- reactiveValues() # "Filing cabinet" for UI row data
    counter <- reactiveVal(0) # Fingerprinting

    # Function to add a new row
    add_row <- function() {
      new_id <- paste0("row_", counter() + 1)
      counter(counter() + 1)

      # UI Injection
      insertUI(
        selector = paste0("#", ns("rows_placeholder")),
        where = "beforeEnd",
        ui = mod_TrendRow_ui(ns(new_id), length(active_rows()) + 1)
      )

      # Server Initialization
      res <- mod_TrendRow_server(new_id, df)
      row_outputs[[new_id]] <- res

      # Track buttons
      observeEvent(res$add(), {add_row()})
      observeEvent(res$remove(), {remove_row(new_id)})

      #Updating active rows
      active_rows(c(active_rows(), new_id))
    }

    # Function to remove a row
    remove_row <- function(row_id) {
      # Does nothing if not enough rows
      if (length(active_rows()) <= 1) {
        return()
      }

      # Terminates removed row
      removeUI(selector = paste0("#", ns(paste0(row_id, "-row_container"))))

      # Updating active rows
      active_rows(active_rows()[active_rows() != row_id])
    }

    # Clears all row elements
    observeEvent(input$clear_all, {
      # Walks through active rows and terminates each
      purrr::walk(active_rows(), function(row_id) {
        removeUI(selector = paste0("#", ns(paste0(row_id, "-row_container"))))
      })

      # Resets the tracking variables
      active_rows(character(0))

      # Re-initializes with a single fresh row
      add_row()
    })

    # Initial row upon startup
    observeEvent(df, {
      if(length(active_rows()) == 0) add_row()
    }, once = TRUE)

    # Combining all active row data for plotting
    combined_data <- reactive({
      req(length(active_rows()) > 0)

      # Creating a list of the current active row IDs to preserve order
      current_order <- active_rows()

      purrr::map_df(current_order, function(r_id) {
        req(row_outputs[[r_id]])

        d <- row_outputs[[r_id]]$data()

        reg <- input[[paste0(r_id, "-region")]]
        ben <- input[[paste0(r_id, "-benchmark")]]
        stf <- input[[paste0(r_id, "-staff")]]

        reg_lbl <- if(shiny::isTruthy(reg)) reg else "R: Population"
        ben_lbl <- if(shiny::isTruthy(ben)) ben else "B: Population"
        stf_lbl <- if(shiny::isTruthy(stf)) stf else "S: Population"

        lbl <- paste(reg_lbl, ben_lbl, stf_lbl, sep = " | ")

        if (lbl == "R: Population | B: Population | S: Population") {
          lbl <- "Population Level"
        }

        if(nrow(d) > 0) {
          d$label <- lbl
        }
        d
      }) %>%
        # Ensures consistent trend line colors
        dplyr::mutate(label = factor(label, levels = unique(label)))
    })
    # Rendering the Plot
    output$trend_plot <- renderPlot({
      d <- combined_data()
      req(nrow(d) > 0)

      ggplot2::ggplot(d, ggplot2::aes(x = month, y = mean_rate, color = label, group = label)) +
        ggplot2::geom_line(linewidth = 1.2) +
        ggplot2::geom_point(size = 3) +
        ggplot2::labs(
          title = "Sickness Rate Trends",
          y = "Mean Sickness Rate (%)",
          x = "Month",
          color = "Trend Definitions"
        ) +

        # Today's month vertical line
        ggplot2::geom_vline(
          xintercept = format(lubridate::today(),"%b"),
          linetype = "dashed",
          color = "grey",
          linewidth = 1
        ) +
        ggplot2::annotate(
          "text",
          x = format(lubridate::today(),"%b"),
          y = max(d$mean_rate, na.rm = T),
          label = paste("Current Month:",format(lubridate::today(),"%b")),
          color = "grey",
          angle = 90,
          vjust = -1.5
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
    })
  })
}
