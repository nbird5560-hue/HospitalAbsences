#' @import ggplot2
#' @import mgcv
#' @importFrom stats as.formula lm predict quantile rnorm setNames

utils::globalVariables(c("staff_group", "nhse_region_name", "benchmark_group",
                         "month", "year", "hours_lost", "mean_rate", "type", "val",
                         "sickness_rate", "month_idx", "fte_days_lost", "fte_days_available",
                         "sickness_absence_rate_percent", "cluster_group", "label"))
