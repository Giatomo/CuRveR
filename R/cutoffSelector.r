
cutoffSelectorUI <- function(id, selected_cutoff_type = NULL, across_signal = FALSE, same_for_blanks = FALSE) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  cutoff_types <- c("individual", "conditions", "global")

  tagList(
    fillRow(
      flex = c(NA, NA, NA),
      selectizeInput(ns("selected_cutoff_type"), label = NULL, choices = cutoff_types, selected = selected_cutoff_type),
      checkboxInput(ns("across_signal"), "Share cutoff across signals", value = across_signal),
      checkboxInput(ns("same_for_blanks"), "Share cutoff between replicates and blanks", value = across_signal)
    ),
    flexRow(
      column(
              12,
              plotOutput("plot",
                dblclick = "cutoff_click",
                brush = brushOpts("cutoff_brush",
                  direction = "x",
                  resetOnNew = TRUE,
                  fill = "#3DB2FF",
                  opacity = .15
                )
              )
            )
    )
  )
}

cutoffSelectorServer <- function(id, .data, .xdata, .group)({
  moduleServer(
    id,
    function(input, output, session) {

      toReturn <- reactiveValues(
        data = isolate(.data),
        last_left_cutoff = NULL,
        last_right_cutoff = NULL,
        plot = NULL
      )

      .data |>
        group_by({{ .group }}) |>
        mutate(
          left_cutoff = input$cutoff_brush[["xmin"]] %||% rvs$last_left_cutoff %||% left_cutoff,
          right_cutoff = input$cutoff_brush[["xmax"]] %||% rvs$last_right_cutoff %||% right_cutoff
        )  |>
        ungroup() -> .data



      return(toReturn)

    })
})


plot_cutoffs <- function(df, cutoff_type = NULL, selected = NULL, signal = NULL, type = NULL){

    group <- switch(cutoff_type,
        "individual" = "well",
        "condition"  = "condition",
        "global"     = NULL)

    data <- if_else(is.null(group), df, df |> filter({{ group }} == selected))
    data <- if_else(is.null(signal), data, data |> filter({{ signal }} == signal))
    data <- if_else(is.null(type), data, data |> filter({{ type }} == type))

    annotations <- data |>
        summarise(
            left_cutoff = median(left_cutoff),
            right_cutoff = median(right_cutoff),
            value = max(value))
    data  |>
        ggplot(aes(x = Time, y = value, group = {{ group }}, color = {{ signal }}, fill = {{ signal }})) +
        geom_line(size = 1) +
        geom_point(color = "white",  size = 3.5, shape = 23) +
        geom_vline(aes(xintercept = median(left_cutoff)), size = 1, linetype = "dashed") +
        geom_vline(aes(xintercept = median(right_cutoff)), size = 1, linetype = "dashed") +
        annotate(geom = "rect", ymin = 0, ymax = annotations$value + 0.2 * annotations$value, xmin = annotations$left_cutoff, xmax = annotations$right_cutoff, alpha = 0.15, fill = "#3DB2FF") +
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(breaks = seq(0, max(data[["Time"]]), by = 2)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme_minimal() +
        theme(text = element_text(size = 20))
}