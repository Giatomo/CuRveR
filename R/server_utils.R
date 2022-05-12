`%||%` <- function(a, b) if (is.null(a)) b else a

`%!in%` <- Negate(`%in%`)

increment_trigger <- function(trigger) {
    ifelse(is.null(trigger), 0, trigger) + 1
    }

modalSavePlot <- function() {
  modalDialog(
    easyClose = TRUE,
    fade = FALSE,
    title = "Save",
    textInput("filename", "Filename"),
    selectInput("filetype", "Filetype", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg")),
    numericInput("dpi", "Resolution", 300, min = 0, max = 1200, step = 10),
    numericInput("width", "width", shiny100),
    numericInput("height", "height", 100),
    selectInput("units", "Units", choices = c("in", "cm", "mm", "px")),
    downloadButton("downloadPlot", "Download"),
  )
}

modalSaveData <- function() {
  modalDialog(
    easyClose = TRUE,
    fade = FALSE,
    title = "Save",
    textInput("filename", "Filename"),
    selectInput("filetype", "Filetype", choices = c("csv")),
    downloadButton("downloadData", "Download"),
  )
}


namingModal <- function(id, condition_exist = FALSE) { # nolint
  ns <- NS(id)

  modalDialog(
    easyClose = TRUE,
    fade = FALSE,
    textInput(ns("name"), "Enter condition name",
      placeholder = 'Try "WT" or "Treatment_X"'
    ),
    if(condition_exist) {
      div(tags$b("Condition already exist", style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("ok"), "OK")
    )
  )
}


save_file <- function(to_save, filetype, input, session = getSession(), roots) {
  match_input <- deparse(substitute(input)) |> str_match("(.*)\\$(.*)")

  save_fun <- switch(filetype,
    "yaml" = quote(yaml::write_yaml(to_save, path$datapath)),
  )

  allowed_files <- switch(filetype,
    "yaml" = c("", "yml", "yaml"),
  )

  shinyFiles::shinyFileSave(get(match_input[2]), match_input[3], roots = roots, session = session, filetypes = allowed_files)

  if (is.integer(input)) {
    return(FALSE)
  } else {
    path <- shinyFiles::parseSavePath(roots, input)
    eval(save_fun)
    return(TRUE)
  }
}



tabledownloadHandler <- function(table, file_name, file_type) {
  downloadHandler(
    filename = function() {
      paste0(file_name, ".", file_type)
    },
    content = function(file) {
      switch(file_type,
        csv = write_csv(table, file = file, col_names = TRUE),
        tsv = write_tsv(table, file = file, col_names = TRUE),
      )
    }
  )
}

plotdownloadHandler <- function(table, file_name, file_type) {
  downloadHandler(
    filename = function() {
      paste0(input$filename, ".", input$filetype)
    },
    content = function(file) {
      ggsave(file, plot = overview(), device = input$filetype, width = input$width, height = input$height, units = input$units, dpi = input$dpi)
    }
  )
}

color_tile_from_0 <- function(...) {
  formattable::formatter("span", style = \(x) {
    formattable::style(
      display = "block",
      padding = "0 4px",
      `border-radius` = "4px",
      `background-color` = formattable::csscolor(formattable::gradient(c(0, as.numeric(x)), ...))[2:length(x)]
    )
  })
}

color_tile_0_to_100 <- function(...) {
  formattable::formatter("span", style = \(x) {
    formattable::style(
      display = "block",
      padding = "0 4px",
      `border-radius` = "4px",
      `background-color` = formattable::csscolor(formattable::gradient(c(0, as.numeric(x), 100), ...))[2:length(x)-1]
    )
  })
}




download_object_as_rds <- function(object, file_path = tempfile(pattern = "template_", tmpdir = fs::path_home(), fileext = ".Rds")) {
  downloadHandler(
    filename = function() {
      file_path
    },
    content = function(file) {
      saveRDS(object, file)
    }
  )
}

download_object_as_yaml <- function(object, file_path = tempfile(pattern = "template_", tmpdir = fs::path_home(), fileext = ".yaml")) {
  downloadHandler(
    filename = function() {
      file_path
    },
    content = function(file) {
      yaml::write_yaml(object, file)
    }
  )
}


input_file_row <- function(input, file_input_id, sheet_input_id, name_input_id, file_choices) {
  fluidRow(
    column(
      4,
      shiny::selectInput(
        inputId = file_input_id,
        label = NULL,
        choices = file_choices,
        selected = input[[file_input_id]] %||% NULL
      )
    ),
    if (is_excel_file(input[[file_input_id]])) {
      column(
        4,
        shiny::selectInput(
          inputId = sheet_input_id,
          label = NULL,
          choices = readxl::excel_sheets(input[[file_input_id]]),
          selected = input[[sheet_input_id]] %||% NULL
        )
      )
    } else {
      column(4)
    },
    column(
      4,
      textInput(
        inputId = name_input_id,
        label = NULL,
        value = input[[name_input_id]] %||% ""
      )
    )
  )
}

variables_rank_list <- function(id, group, text, labels, data, group_attribute, treatment_attribute, put = TRUE, pull = TRUE, multidrag = TRUE, remove_dragged_elements = FALSE) {
  onAdd_func <- NULL

  if (missing(id)) {
    id <- paste(group_attribute, treatment_attribute, sep = "_")
  }

  if (missing(labels)) {
    labels <- data[[group_attribute]][[treatment_attribute]]
  }

  if (remove_dragged_elements) {
    onAdd_func <- htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
  }

  rank_list(
    text = text,
    labels = labels,
    input_id = id,
    options = sortable_options(
      group = list(
        name = group,
        put = put,
        pull = pull
      ),
      multiDrag = multidrag,
      onAdd = onAdd_func
    )
  )
}



#  observeEvent({
#       input$cutoff_brush
#       input$same_cutoff
#     }, {
#       cutoff_selection <- function (df, signal, well, cutoff, same_cutoff = FALSE)

#       if (same_cutoff == TRUE) {
#         isolate({
#           rvs$data |>
#             rowwise() |>
#             mutate(
#               left_cutoff =  input$cutoff_brush[["xmin"]] %||% rvs$last_left_cutoff %||% left_cutoff,
#               right_cutoff = input$cutoff_brush[["xmax"]] %||% rvs$last_right_cutoff %||% right_cutoff
#             ) |>
#             ungroup() -> rvs$data
#         })
#       } else {
#         req(input$cutoff_brush)

#         isolate({
#           rvs$data |>
#             rowwise() |>
#             mutate(
#               left_cutoff =  if_else(well == input$selected_well_cutoff && signal == input$selected_signal_cutoff, input$cutoff_brush[["xmin"]], left_cutoff),
#               right_cutoff = if_else(well == input$selected_well_cutoff && signal == input$selected_signal_cutoff, input$cutoff_brush[["xmax"]], right_cutoff)
#             ) |>
#             ungroup() -> rvs$data
#         })
#       }

#       req(input$cutoff_brush)

#       rvs$last_left_cutoff <- input$cutoff_brush[["xmin"]]
#       rvs$last_right_cutoff <- input$cutoff_brush[["xmax"]]
#     }
#   )
