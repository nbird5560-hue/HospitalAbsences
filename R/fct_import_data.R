#' Process NHS Sickness Absence Data
#'
#' @description \emph{fct_import_data.R} reads all CSV files from the data structure,
#' cleans column names, narrows the scope of the data to only hospitals and community providers,
#' and creates columns appropriate for use in other modules.
#'
#' @param data_path Character. The path to the folder containing the CSV files.
#'
#' @return A processed tibble with cleaned names, and narrowed scope
#'
#' @importFrom readr read_csv
#' @importFrom dplyr rename_with mutate select filter arrange bind_cols cur_group_id
#' @importFrom lubridate dmy
#' @importFrom tidyr drop_na
#' @importFrom tibble as_tibble
#' @export
import_data <- function(
    data_path = system.file("app/data_raw", package = "HospitalAbsences")
    ) {

  if (data_path == "") {
    data_path <- "inst/app/data_raw"
  }

  # Collecting file paths
  files <- list.files(path = data_path, pattern = "\\.csv$", full.names = TRUE)

  if (length(files) == 0) {
    stop("No CSV files found in the provided directory.")
  }

  # Reading all .csv files and merging them
  raw <- readr::read_csv(files, id = "file_path", show_col_types = FALSE)

  # Data cleaning
  dat <- raw %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(
      date = lubridate::dmy(date),
      sickness_rate = sickness_absence_rate_percent / 100
    ) %>%
    dplyr::select(
      date, nhse_region_name, cluster_group, benchmark_group,
      staff_group, fte_days_lost, fte_days_available, sickness_rate
    ) %>%
    dplyr::filter(
      cluster_group %in% c("Acute", "Community Provider Trust"),
      benchmark_group != "Care Trust"
    ) %>%
    dplyr::mutate(hours_lost = fte_days_lost*7.5,
                  hours_available = fte_days_available*7.5,
                  bench_region = interaction(benchmark_group, nhse_region_name, sep = "_"),
                  month = format(date,"%b"),
                  year = format(date,"%Y"),
                  month_year = interaction(month, year, sep = "_"),
                  month_idx = (as.numeric(year) - min(as.numeric(year)))*12 + as.numeric(format(date, "%m")),
                  start_series = month_idx == 1) %>%
    tidyr::drop_na(sickness_rate, hours_lost) %>%

    # Generatinge timestep based on date groups
    dplyr::mutate(timestep = dplyr::cur_group_id(), .by = date) %>%
    dplyr::select(-cluster_group, -fte_days_lost, -fte_days_available)
 dat$staff_group[which(dat$staff_group == "HCHS doctors")] <- "HCHS Doctors"
 dat
}
