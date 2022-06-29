server_format <- function(input, output, session, rvs) {

observeEvent(input$format_data, {

    rvs$data |>
    select(-rvs$columns)  |>
    clean_time(rvs$xdata) |>
    format_wellplate_long(wells = matches(rvs$ydata)) |>
    mutate(
      left_cutoff = min(.data[[rvs$xdata]]),
      right_cutoff = max(.data[[rvs$xdata]])
    ) -> rvs$cleaned


    rvs$entities <- gtools::mixedsort(unique(rvs$cleaned[["well"]]))
    rvs$all_signals <- unique(rvs$cleaned[["signal"]])

    change_page("conditions")

  })

  # Initial columns rank list
  columnsUI <- reactive({
      variables_rank_list(
        id = "columns",
        group = "ColumnsSorter",
        text = "Columns",
        labels = rvs$columns,
      )
    })

  output$columns <- renderUI({
    columnsUI()
  })

  # X data rank list
  xDataUI <- reactive({
    variables_rank_list(
      id = "xdata",
      group = "ColumnsSorter",
      text = "X Data",
      labels = rvs$xdata,
      put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
    )
  })

  output$xdata <- renderUI({
    xDataUI()
  })

  # Y data rank list
  yDataUI <- reactive({
    variables_rank_list(
      id = "ydata",
      group = "ColumnsSorter",
      text = "Y Data(s)",
      labels = rvs$ydata,
    )
  })

  output$ydata <- renderUI({
    yDataUI()
  })

  # Update rank lists
  observeEvent({
      input[["columns"]]
      input[["xdata"]]
      input[["ydata"]]
    }, {
      rvs$xdata   <- input[["xdata"]]
      rvs$ydata   <- input[["ydata"]]

      rvs$xdata   <- gtools::mixedsort(rvs$xdata)
      rvs$ydata   <- gtools::mixedsort(rvs$ydata)

      req(input[["columns"]])
      rvs$columns <- input[["columns"]]
      rvs$columns <- gtools::mixedsort(rvs$columns)
    }
  )
}