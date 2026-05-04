#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @import mgcv
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = function() {
        library(mgcv)
        n_workers <- max(1,ceiling(parallel::detectCores()/2))# may need to lower
        future::plan(future::multisession,
                     workers = n_workers)
        message(paste("--- Parallel plan initialized with", n_workers, "workers ---"))

        shiny::onStop(function() {
          message("--- Shutting down parallel workers ---")
          future::plan(future::sequential)
          gc(full = TRUE)
        })

        # Backup check
        if (!is.null(onStart)) onStart()
      },
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
