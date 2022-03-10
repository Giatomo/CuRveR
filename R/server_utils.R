`%||%` <- function(a, b) if (is.null(a)) b else a

modalSavePlot <- function() {
  modalDialog(
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
    title = "Save",
    textInput("filename", "Filename"),
    selectInput("filetype", "Filetype", choices = c("csv")),
    downloadButton("downloadData", "Download"),
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



namingModal <- function(id, condition_exist = FALSE) { # nolint
  ns <- NS(id)

  modalDialog(
    textInput(ns("name"), "Enter condition name",
      placeholder = 'Try "WT" or "Treatment_X"'
    ),
    if (condition_exist) {
      div(tags$b("Condition already exist", style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("ok"), "OK")
    )
  )
}

conditionManagerUI <- function(id, choices, selected = NULL) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    fillRow(
      flex = c(NA, NA, NA, NA, NA, NA),
      selectizeInput(ns("selected_condition"), label = NULL, choices = choices, selected = selected),
      actionButton(ns("create"), NULL, icon = icon("plus")),
      actionButton(ns("remove"), NULL, icon = icon("trash-alt")),
      actionButton(ns("rename"), NULL, icon = icon("edit")),
      shinyFiles::shinyFilesButton(ns("import"), label = NULL, title = "Please select a file", multiple = FALSE, icon = icon("file-import"), style = "margin-left:5px"),
      shinyFiles::shinySaveButton(ns("save"), label = NULL, title = "Save file as...", icon = icon("save"))
    )
  )
}

conditionManagerServer <- function(id, conditions) {
  moduleServer(
    id,
    function(input, output, session) {
      toReturn <- reactiveValues(
        conditions = isolate(conditions),
        trigger = NULL,
        selected = isolate(input$selected_condition)
      )

      action <- reactiveVal("")
      volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())

      observeEvent(input$import, {
        shinyFiles::shinyFileChoose(input, "import", roots = volumes, session = session, filetypes = c("yaml"))
        if (is.integer(input$import)) {
          return(toReturn)
        } else {
          yaml <- shinyFiles::parseFilePaths(volumes, input$import)
          toReturn$conditions <- yaml::read_yaml(yaml$datapath)
          toReturn$selected <- names(toReturn$conditions)[1]
          toReturn$trigger <- ifelse(is.null(toReturn$trigger), 0, toReturn$trigger) + 1
          return(toReturn)
        }
      })

      observeEvent(input$save, {
        if (!save_file(to_save = toReturn$conditions, filetype = "yaml", input = input$save, session = session, roots = volumes)) {
          return(toReturn)
        }

        toReturn$trigger <- ifelse(is.null(toReturn$trigger), 0, toReturn$trigger) + 1
        return(toReturn)
      })

      observeEvent(input$remove, {
        # ADD CONFIRMATION ?
        toReturn$conditions[[input$selected_condition]] <- NULL
        toReturn$selected <- names(toReturn$conditions)[length(toReturn$conditions)]
        toReturn$trigger <- ifelse(is.null(toReturn$trigger), 0, toReturn$trigger) + 1
        return(toReturn)
      })

      observeEvent(input$create, {
        action("create")
        showModal(namingModal(id))
      })

      observeEvent(input$rename, {
        action("rename")
        showModal(namingModal(id))
      })

      # In modal
      observeEvent(input$ok, {
        if (input$name %in% names(toReturn$conditions)) {
          showModal(namingModal(id, condition_exist = TRUE))
        } else {
          if (action() == "create") {
            toReturn$conditions[[input$name]] <- list(replicates = list(), blanks = list())
          } else if (action() == "rename") {
            toReturn$conditions[[input$name]] <- toReturn$conditions[[input$selected_condition]]
            toReturn$conditions[[input$selected_condition]] <- NULL
          }
          removeModal()
        }
        toReturn$selected <- input$name
        toReturn$trigger <- ifelse(is.null(toReturn$trigger), 0, toReturn$trigger) + 1
      })

      observeEvent(input$selected_condition, {
        toReturn$selected <- input$selected_condition
        toReturn$trigger <- ifelse(is.null(toReturn$trigger), 0, toReturn$trigger) + 1
      })
      return(toReturn)
    }
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
