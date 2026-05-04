#' Predictor UI Function
#'
#' @description \emph{mod_Predictor.R} handles all predictions of `hours_available` using
#' the ensemble and controls the \strong{Ensemble Prediction Panel}. Based on user
#' inputs, a data slice is created and then `hours_lost` responses are predicted
#' upon across the ensemble, and then averaged.  During this process, 95% Prediction
#' interval and 95% Bayesian uncertainty intervals are produced and plotted on a
#' box and whiskers plot for comparison.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Predictor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h2("3. Ensemble Prediction"),
      # Conditional UIs to indicate readiness of utility
      uiOutput(ns("dynamic_inputs")),
      hr(),
      uiOutput(ns("predict_btn_container")),
      br(),
      uiOutput(ns("predicted_panel"))
    )
  )
}

#' Predictor Server Functions
#'
#' @noRd
mod_Predictor_server <- function(id, model_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Identify required inputs based on model terms
    required_input_ids <- reactive({
      req(model_data()$models)
      m <- model_data()$models[[1]]
      vars <- names(m$var.summary)

      mapping <- list(
        "staff_group" = "in_staff_group",
        "benchmark_group" = "in_benchmark",
        "nhse_region_name" = "in_region"
      )
      active_ids <- unname(unlist(mapping[vars[vars %in% names(mapping)]]))
      return(c(active_ids, "month_year"))
    })

    # Dynamic Inputs Generation
    output$dynamic_inputs <- renderUI({
      req(model_data()$models)
      m <- model_data()$models[[1]]
      df <- model_data()$data
      vars <- names(m$var.summary)

      tagList(
        if ("staff_group" %in% vars) {
          selectInput(ns("in_staff_group"), "Select Staff Group", choices = unique(df$staff_group))
        },
        if ("benchmark_group" %in% vars) {
          selectInput(ns("in_benchmark"), "Select Benchmark Group", choices = unique(df$benchmark_group))
        },
        if ("nhse_region_name" %in% vars) {
          selectInput(ns("in_region"), "Select Region", choices = unique(df$nhse_region_name))
        },
        div(
          shinyWidgets::airDatepickerInput(
            inputId = ns("month_year"),
            label = "Select Month and Year",
            value = Sys.Date(),
            view = "months",
            minView = "months",
            dateFormat = "MMM yyyy"
          )
        ),
        div(
          shinyWidgets::autonumericInput(
            inputId = ns("num_input"),
            label = "Enter Monthly Labor Hours Available (If Known)",
            value = NULL,
            decimalPlaces = 1
          ),
          shiny::helpText("Leaving this blank may affect predictive accuracy")
        )
      )
    })

    # Predict button UI render
    output$predict_btn_container <- renderUI({
      ids <- required_input_ids()
      ready <- all(sapply(ids, function(x) shiny::isTruthy(input[[x]])))

      if (!ready) return(helpText("Select all inputs above to enable prediction."))

      actionButton(ns("predict_btn"), "Calculate Prediction", class = "btn-info")
    })

    # Prediction logic with simulation for intervals
    prediction <- eventReactive(input$predict_btn, {
      req(model_data()$models, model_data()$data, input$month_year)

      m <- model_data()$models
      df <- model_data()$data
      my <- input$month_year

      .month <- lubridate::month(my)
      .year <- lubridate::year(my)
      .timestep <- (.year - min(df$year)) * 12 + .month

      # Step 1: Handle hours_available projection if missing
      if (!shiny::isTruthy(input$num_input)) {
        hist_df <- df %>% dplyr::filter(month == .month)
        if (shiny::isTruthy(input$in_staff_group)) {
          hist_df <- hist_df %>% dplyr::filter(staff_group == input$in_staff_group)
        }
        if (shiny::isTruthy(input$in_benchmark)) {
          hist_df <- hist_df %>% dplyr::filter(benchmark_group == input$in_benchmark)
        }
        if (shiny::isTruthy(input$in_region)) {
          hist_df <- hist_df %>% dplyr::filter(nhse_region_name == input$in_region)
        }

        req(nrow(hist_df) > 2)

        set.seed(787)
        boot_results <- lapply(1:1000, function(i) {
          boot_sample <- hist_df[sample(nrow(hist_df), replace = TRUE), ]
          trend_mod <- lm(hours_available ~ year, data = boot_sample)
          predict(trend_mod, newdata = data.frame(year = .year))
        })
        .hours_available <- mean(unlist(boot_results), na.rm = TRUE)
      } else {
        .hours_available <- input$num_input
      }

      # Creating prediction slice
      pred_df <- data.frame(
        hours_available = as.numeric(.hours_available),
        month = .month,
        year = .year,
        timestep = .timestep,
        staff_group = input$in_staff_group %||% NA,
        benchmark_group = input$in_benchmark %||% NA,
        nhse_region_name = input$in_region %||% NA
      )

      # Running Ensemble Simulation
      n_sims <- 100
      sim_results <- lapply(m, function(x) {
        tryCatch({
          p <- predict(x, newdata = pred_df, se.fit = TRUE)
          sigma <- summary(x)$sigma

          # Guards against singular models/NAs
          se_val <- if(is.null(p$se.fit) || is.na(p$se.fit)) 0 else p$se.fit
          sig_val <- if(is.null(sigma) || is.na(sigma)) 0 else sigma

          # Simulate Bayesian Uncertainty (Sampling dist of mean)
          b_sim <- rnorm(n_sims, mean = p$fit, sd = se_val)

          # Simulate Prediction Interval (Mean + Residual Variance)
          combined_sd <- sqrt(pmax(se_val^2 + sig_val^2, 0.0001))
          p_sim <- rnorm(n_sims, mean = p$fit, sd = combined_sd)

          list(
            bayesian = as.numeric(exp(b_sim)),
            prediction = as.numeric(exp(p_sim))
          )
        }, error = function(e) NULL)
      })

      # Aggregation and Formating
      sim_results <- Filter(Negate(is.null), sim_results)
      req(length(sim_results) > 0)

      bayesian_pool <- as.numeric(unlist(lapply(sim_results, `[[`, "bayesian")))
      prediction_pool <- as.numeric(unlist(lapply(sim_results, `[[`, "prediction")))

      list(
        mean = mean(bayesian_pool, na.rm = TRUE),
        bayesian_pi = quantile(bayesian_pool, c(0.025, 0.975), na.rm = TRUE),
        prediction_pi = quantile(prediction_pool, c(0.025, 0.975), na.rm = TRUE),
        raw_b = bayesian_pool[!is.na(bayesian_pool)],
        raw_p = prediction_pool[!is.na(prediction_pool)]
      )
    })

    # Results panel render
    output$predicted_panel <- renderUI({
      res <- prediction()
      req(res)

      if (is.na(res$mean)) {
        return(div(class = "alert alert-warning", "Prediction unavailable for these inputs."))
      }

      tags$div(
        style = "margin-top: 20px; padding: 20px; border-radius: 8px; background-color: #ffffff; border-left: 5px solid #17a2b8; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
        tags$h4("Predicted Hours Lost to Sickness", style = "margin: 0 0 10px 0; color: #555;"),
        tags$div(style = "font-size: 2.5rem; font-weight: bold; color: #17a2b8;",
                 format(round(res$mean, 0), big.mark = ",")),

        tags$div(
          style = "margin-top: 10px; display: flex; gap: 20px;",
          tags$div(
            tags$strong("95% Bayesian Uncertainty Interval:"), br(),
            format(round(res$bayesian_pi[1], 0), big.mark = ","), " - ",
            format(round(res$bayesian_pi[2], 0), big.mark = ",")
          ),
          tags$div(
            tags$strong("95% Prediction Interval:"), br(),
            format(round(res$prediction_pi[1], 0), big.mark = ","), " - ",
            format(round(res$prediction_pi[2], 0), big.mark = ",")
          )
        ),
        br(),
        plotOutput(ns("interval_plot"), height = "250px"),
        tags$small(class = "text-muted", paste("Estimate for", format(input$month_year, "%B %Y")))
      )
    })

    # Uncertainty Plot
    output$interval_plot <- renderPlot({
      res <- prediction()
      req(res, res$raw_b, res$raw_p)

      plot_df <- data.frame(
        val = c(res$raw_b, res$raw_p),
        type = c(rep("Bayesian Uncertainty", length(res$raw_b)),
                 rep("Prediction Interval", length(res$raw_p)))
      )

      ggplot(plot_df, aes(x = type, y = val, fill = type)) +
        geom_boxplot(alpha = 0.7, outlier.shape = NA) +
        coord_flip() +
        theme_minimal() +
        labs(x = NULL, y = "Hours Lost", title = "Uncertainty Distribution Comparison") +
        scale_fill_manual(values = c("Bayesian Uncertainty" = "#17a2b8",
                                     "Prediction Interval" = "#6c757d")) +
        theme(legend.position = "none",
              plot.title = element_text(face = "bold", size = 14),
              axis.text.y = element_text(size = 11, face = "bold")) +
        scale_y_continuous(labels = scales::comma)
    })
  })
}
