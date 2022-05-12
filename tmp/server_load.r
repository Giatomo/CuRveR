ACCEPTED_FILETYPES <- c("xlsx", "xls", "csv", "tsv", "txt")

is_excel_file <- function(file) {
    if (is.null(file)) {
        return(FALSE)
        }
   return(fs::path_ext(file) %in% c("xlsx", "xls"))
}

input_file_row <- function(input, file_input_id, sheet_input_id, name_input_id, file_choices){

    fluidRow(

        column(4,
            shiny::selectInput(
                inputId = file_input_id,
                label = NULL,
                choices = file_choices,
                selected = input[[file_input_id]] %||% NULL)),

        if (is_excel_file(input[[file_input_id]])) {
            column(4,
                shiny::selectInput(
                    inputId = sheet_input_id,
                    label = NULL,
                    choices = readxl::excel_sheets(input[[file_input_id]]),
                    selected = input[[sheet_input_id]] %||% NULL))
                }
        else {
            column(4)
            },

        column(4,
            textInput(
                inputId = name_input_id,
                label = NULL,
                value = input[[name_input_id]] %||% ""))
    )
}


load_server <- function(input, output, session, rvs) {

  ACCEPTED_FILETYPES <- c("xlsx", "xls", "csv", "tsv", "txt")
  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFiles::shinyFileChoose(input, "files", roots = volumes, filetypes = ACCEPTED_FILETYPES)

  output$signals <- renderUI({
    req(!is.integer(input$files))

    files <- shinyFiles::parseFilePaths(volumes, input$files)

    paths <- files$datapath

    names(paths) <- files$name

    purrr::map(
      1:input$n_signals,
      \(x) {
        file_n <- paste0("file_", x)
        sheet_n <- paste0("sheet_", x)
        name_n <- paste0("signal_", x)

        input_file_row(input, file_n, sheet_n, name_n, paths)
      }
    )
  })

  observeEvent(input$load_data, {

    files <- shinyFiles::parseFilePaths(volumes, input$files)

    rvs$columns <- NULL
    rvs$xdata <- NULL
    rvs$ydata <- NULL

    paths <- files$datapath

    names(paths) <- files$name


    rvs$data <- purrr::map(
      1:input$n_signals,
      \(x) {
        read_tabular_file(
          filepath =    input[[paste0("file_", x)]],
          sheet =       input[[paste0("sheet_", x)]],
          signal_name = input[[paste0("signal_", x)]])
          }) |> dplyr::bind_rows()

    rvs$columns <- colnames(rvs$data)[colnames(rvs$data) != "signal"]

    change_page("format")
  })

}