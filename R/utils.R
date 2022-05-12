#' @export
finite_diff_5pt_cent <- function(x, y) {
  h <- dplyr::lead(x) - x
  (-dplyr::lead(y, 2) + 8 * dplyr::lead(y) - 8 * dplyr::lag(y) + dplyr::lag(y, 2)) / 12 * h
}

finite_diff_5pt_forw <- function(x, y) {
  h <- dplyr::lead(x) - x
  (-25 * y + 48 * dplyr::lead(y) - 36 * dplyr::lead(y, 2) + 16 * dplyr::lead(y, 3) - 3 * dplyr::lead(y, 4)) / 12 * h
}

finite_diff_5pt_back <- function(x, y) {
  h <- x - dplyr::lag(x)
  (25 * y - 48 * dplyr::lag(y) + 36 * dplyr::lag(y, 2) - 16 * dplyr::lag(y, 3) + 3 * dplyr::lag(y, 4)) / 12 * h
}


#' Clean time column
#'
#' @param .data     Float : Dataframe
#' @param .time_col String : Name of the time column from the plate reader experiment
#' @return The dataframe with the formated time column as elapsed hours
#' @export
clean_time <- function(.data, .time_col) {

  # .data |>
  #   mutate(
  #     "{{ .time_col }}" := lubridate::as_datetime({{ .time_col }}),
  #     "{{ .time_col }}" := {{ .time_col }} - first({{ .time_col }}),
  #     "{{ .time_col }}" := as.numeric({{ .time_col }} / 3600)
  #   )

    .data |>
    mutate(
      across(.cols = {{.time_col}}, .fns = \(x) lubridate::as_datetime(x), .names = "{.col}"),
      across(.cols = {{.time_col}}, .fns = \(x) x - first(x), .names = "{.col}"),
      across(.cols = {{.time_col}}, .fns = \(x) as.numeric(x) / 3600, .names = "{.col}"))
}

#' Format into long format
#'
#' @param .data Float : Dataframe
#' @param wells Tidyselect : Tidyselect matching all the wells columns from your plate reader experiment
#' @return The dataframe in long/tidy format
#' @export format_wellplate_long
format_wellplate_long <- function(.data, wells = matches(regex("^[A-Za-z]{1}\\d{1,2}"))) {
  .data |>
    pivot_longer(
      cols = {{ wells }},
      names_to = "well"
    ) |>
    drop_na(value)
}

is_excel_file <- function(file) {
  if (is.null(file)) {
    return(FALSE)
  }
  return(fs::path_ext(file) %in% c("xlsx", "xls"))
}


#' @export
fit_data <- function(.data, .groups, .value, .time, model = RichardModel, optimizer = DeOptimizer, loss_fun = least_absolute_deviation) {
  optimizer <- optimizer$new(loss_fun)

  .data |>
    select({{ .groups }}, {{ .value }}, {{ .time }}) |>
    group_by(across(c({{ .groups }}))) |>
    nest(
      {{ .time }} := {{ .time }},
      {{ .value }} := {{ .value }}
    ) |>
    rowwise() |>
    mutate(model = list(model$new({{ .time  }}, {{ .value }}))) |>
    group_walk(\(x, y) {
      x$model[[1]]$optimize(optimizer)
    })
}


#' @export
read_tabular_file <- function(filepath, signal_name, sheet = NULL) {
  filepath <- fs::path(filepath)

  df <- switch(fs::path_ext(filepath),
    "xlsx" = readxl::read_xlsx(path = filepath, sheet = sheet),
    "xls"  = readxl::read_xls(path = filepath, sheet = sheet),
    "csv"  = readr::read_csv(file = filepath, col_names = TRUE),
    "tsv"  = readr::read_tsv(file = filepath, col_names = TRUE),
    readr::read_delim(file = filepath, col_names = TRUE)) |>
      mutate(signal = signal_name)

  return(df)
}

#' @export
#' @title M_score
m_score <- function(data) {
  .6745 * (data - median(data)) / mad(data)
}
