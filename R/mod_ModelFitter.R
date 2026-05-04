#' ModelFitter UI Function
#'
#' @description \emph{mod_ModelFitter.R} dynamically builds GAM(M) ensembles for
#' future prediction, and comprises the user input in the \strong{Model Fitting Panel}.
#'
#' Key variable names are parsed from data passed from
#' \emph{mod_DataSelection.R} through the top level parent module \emph{app_server.R}.
#' These names (character vector) are then conditionally transformed such that
#' additional functions including splines wrap certain variables. Then, these are
#' composed into a \pkg{mgcv}-readable formula and fed into \emph{mgcv::bam()} to
#' fit GAM(M)s.
#'
#' An initial model is used to estimate a temporal autocorrelation parameter for
#' the ensemble, whose models are then fit in parallel using the \pkg{parallel}
#' package. This ensemble, the filtered data, and the formula are passed on to
#' \emph{mod_Predictor.R} for prediction.
#'
#' Definitions of helper functions can be found in \emph{app_config.R}.
#'
#' See in-line comments for more info.
#'
#' @return A list including the model ensemble, the GAM(M) formula structure, and the data
#' used for fitting the GAM(M)s.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_ModelFitter_ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    tagList(
      h2("2. Model Fitting"),
      h5("Press button to start fitting when data selection is complete."),
      actionButton(
        inputId = ns("fit_btn"),
        label = "Fit Model Ensemble",
        icon = icon("calculator"),
        class = "btn-success",
        style = "width: 100%; margin-bottom: 15px;"
      ),
      shinyWidgets::progressBar(
        id = ns("fit_progress"),
        value = 0,
        display_pct = TRUE,
        title = "Model Fitting Status",
        status = "primary"
      )
    )
  )
}

#' ModelFitter Server Functions
#'
#' @noRd
mod_ModelFitter_server <- function(id, .data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Fits GAM/GAMM ensemble
    model_results <- eventReactive(input$fit_btn, {
      req(.data()$df)

      # Progress Bar
      shinyWidgets::updateProgressBar(session, "fit_progress", value = 0, title = "Press Button To Commence Fitting...")

      # Importing data passed from parent module
      optvars <- .data()$optvars
      df <- .data()$df
      df$year <- as.numeric(as.character(df$year)) # Quick module-specific reformat
      df <- droplevels(df)

      # Check for non-empty dataset
      validate(
        need(nrow(df) > 0, "The filtered dataset has no rows. Please adjust your filters to include levels for each chosen predictor")
      )


      # Progress Bar
      shinyWidgets::updateProgressBar(session, "fit_progress", value = 10, title = "Preparing formulas...")

      # These lines combine helper functions to select columns names with specific
      # attribute "select", remove those which are single-level-remaining factors,
      # and perform updates such as wrapping terms with splines
      first <- update_vars(strip_1_level(get_attr_values_names(df, "select", "first"), df), df)
      second <- update_vars(strip_1_level(c(get_attr_values_names(df, "select", "second") ,optvars), df), df)

      resp <- "hours_lost"

      # Helper to build formula object from LHS and RHS string elements
      fit_form <- function(resp, regr){
        form <- paste(regr, collapse = " + ")
        form <- paste(resp, form, sep = " ~ ")
        as.formula(form)
      }

      form1 <- fit_form(resp, first) # Formula for rho estimation
      form2 <- fit_form(resp, second) # Formula for GAM(M) ensemble fitting

      # Check for adequate data
      validate(
        need(nrow(df) > 30, "Not enough data points to fit the model (minimum 30 required).")
      )

      # Progress Bar
      shinyWidgets::updateProgressBar(session, "fit_progress", value = 30, title = "Estimating autocorrelation tuning parameter...")

      initial_mod <- tryCatch({
        mgcv::bam(form1, data = df, family = mgcv::tw(), discrete = TRUE)
      }, error = function(e) {
        return(NULL)
      })

      # Check if initial_mod could be fit
      validate(
        need(!is.null(initial_mod), "Initial model fit failed. Try another data filtration.")
      )

      #Estimating rho
      rho_est <- stats::acf(stats::residuals(initial_mod), plot=FALSE)$acf[2]

      # Progress Bar
      shinyWidgets::updateProgressBar(session, "fit_progress", value = 60, title = "Parallelized Ensemble Building... this may take a minute...")

      # Helper for fitting GAM(M) ensemble members
      fit_bagged_member <- function(seed, df, rho_val, form) {
        set.seed(seed)

        # Bootstrap sample
        sub_df <- df[sample(nrow(df), floor(0.8 * nrow(df))), ]

        #releveling factors
        sub_df <- droplevels(sub_df)

        # wrapped in tryCatch({}) in case bam models fail to fit
        tryCatch({
          # Subsample size defensive check
          if(nrow(sub_df) < 20) stop("Sub-sample too small")

          # Fit bam
          mgcv::bam(form,
                    data = sub_df,
                    family = mgcv::tw(),
                    rho = rho_val,
                    discrete = TRUE,
                    nthreads = 1) # Each worker fits a model then grabs another
        }, error = function(e) {
          message(paste("Worker error at seed", seed, ":", e$message))
          return(NULL)
        })
      }

      n_ensembles <- max(1, future::nbrOfWorkers())

      # Parallelizing ensemble model fits
      bagged_models <- future.apply::future_lapply(
        1:n_ensembles, # 1:1 ratio between models and active cores
        function(i) {fit_bagged_member(i, df, rho_est, form2)},
        future.seed = TRUE,
        future.packages = "mgcv"
      )

      # Removing failed fits from ensemble
      results <- Filter(Negate(is.null), bagged_models)

      # Check if all models failed to converge
      if (length(results) == 0) {
        shiny::showNotification("All model fits failed. Check data filters or variable variance.", type = "error")
        return(NULL)
      }

      # Progress Bar
      shinyWidgets::updateProgressBar(session, "fit_progress", value = 100, title = "Complete.")
      return(list(models = results, form = form2, data = df))
    })

    return(model_results)
  })
}
