#' @export
finite_diff_5pt_cent <- function(x,y) {
  h <- dplyr::lead(x) - x
  (-dplyr::lead(y, 2) + 8*dplyr::lead(y) - 8*dplyr::lag(y) + dplyr::lag(y, 2)) / 12*h
}

finite_diff_5pt_forw <- function(x,y) {
  h <- dplyr::lead(x) - x
  (-25*y + 48*dplyr::lead(y) - 36*dplyr::lead(y,2) + 16*dplyr::lead(y,3) - 3*dplyr::lead(y,4)) /12*h
}

finite_diff_5pt_back <- function(x,y) {
  h <- x-dplyr::lag(x)
  (25*y - 48*dplyr::lag(y) + 36*dplyr::lag(y,2) - 16*dplyr::lag(y,3) + 3*dplyr::lag(y,4)) /12*h
}


#' Clean time column
#'
#' @param .data     Float : Dataframe
#' @param .time_col String : Name of the time column from the plate reader experiment
#' @return The dataframe with the formated time column as elapsed hours
#' @export
clean_time <- function(.data, .time_col) {
  .data |>
    mutate(
      {{.time_col}} := lubridate::as_datetime({{.time_col}}),
      {{.time_col}} := {{.time_col}} - first({{.time_col}}),
      {{.time_col}} := as.numeric({{.time_col}}/3600)
    )
}

#' Format into long format
#'
#' @param .data Float : Dataframe
#' @param wells Tidyselect : Tidyselect matching all the wells columns from your plate reader experiment
#' @return The dataframe in long/tidy format
#' @export format_wellplate_long
format_wellplate_long <- function(.data, wells = matches(regex("^[A-Za-z]{1}\\d{1,2}"))) {
  .data |>
    pivot_longer(cols = {{wells}},
                 names_to = "well") |>
    drop_na(value)
}



#' @export
fit_data <- function(.data, .groups ,.value, .time) {
  optimizer <- DeOptimizer$new(least_absolute_deviation)

  .data |>
    select({{.groups}}, {{.value}}, {{.time}}) |>
    group_by(across(c({{.groups}}))) |>
    nest({{.time}}  := {{.time}},
         {{.value}} := {{.value}}) |>
    rowwise() |>
    mutate(model = list(RichardModel$new({{.value}}, {{.time}}))) |>
    group_walk(\(x, y){
      x$model[[1]]$optimize(optimizer)})

}

print_data <- function(.data) {
  print(.data)
  .data
}



#' @export
#' @title M_score
m_score <- function(data) {.6745*(data-median(data))/mad(data)}
