#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "HospitalAbsences")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}

var_dictionary <- list(
  "nhse_region_name" = "Region",
  "benchmark_group" = "Benchmark Group",
  "staff_group" = "Staff Group",
  "bench_region" = "Benchmark Group x Region"
)

wrap_spline <- function(vars, x, bs, k=3){
  .x <- paste0("^(", x, ")$")
  stringr::str_replace(vars, .x, paste0("s(", x, ", bs='", bs,"', k=", k, ")"))
}

apply_attr <- function(df, .names, .attr, value) {
  df %>% dplyr::mutate(dplyr::across(dplyr::all_of(.names), \(x) {
    attr(x, .attr) <- unique(c(attr(x, .attr), value))
    return(x)
  }))
}

apply_attr <- function(df, .attr, value, ...) {
  .names <- c(...)
  df %>% dplyr::mutate(dplyr::across(dplyr::all_of(.names), \(x) {
    attr(x, .attr) <- unique(c(attr(x, .attr), value))
    return(x)
  }))
}


get_attr_values_names <- function (df, .attr, value){
  nm <- names(df)
  .x <- setNames(nm, nm) %>%
    sapply(\(x) {
      value %in% attr(df[[x]], .attr)
  })
  names(.x[.x])
}

update_vars <- function (vars, df) {
  if ("month" %in% vars) vars <- wrap_spline(vars, "month", "cc", length(unique(df$month)))
  if ("month_year" %in% vars) vars <- wrap_spline(vars, "month_year", "re", length(unique(df$month_year)))
  if ("staff_group" %in% vars) vars <- wrap_spline(vars, "staff_group", "re", length(unique(df$staff_group)))
  if ("bench_region" %in% vars) vars <- stringr::str_replace(vars,"bench_region","benchmark_group:nhse_region_name")
  if ("hours_available" %in% vars) vars <- stringr::str_replace(vars,"(hours_available)","offset(log(\\1))")
  return(vars)
}


strip_1_level <- function(string, df) {
  `%nin%` <- Negate(`%in%`)
  df <- droplevels(df)
  tf <- "bench_region" %in% string
  remove <- names(dplyr::select(droplevels(df), where(~ is.factor(.x) && nlevels(.x) == 1)))
  if(tf) remove <- c(remove, "bench_region")
  string[string %nin% remove]
}


