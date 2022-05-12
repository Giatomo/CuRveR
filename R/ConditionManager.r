conditionManagerUI <- function(id, choices, selected = NULL) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fillRow(
      flex = c(NA, NA, NA, NA, NA, NA),
      shiny::selectizeInput(ns("selected_condition"), label = NULL, choices = choices, selected = selected),
      shiny::actionButton(ns("create"), NULL, icon = icon("plus")),
      shiny::actionButton(ns("remove"), NULL, icon = icon("trash-alt")),
      shiny::actionButton(ns("rename"), NULL, icon = icon("edit")),
      shinyFiles::shinyFilesButton(ns("import"), label = NULL, title = "Please select a file", multiple = FALSE, icon = shiny::icon("file-import"), style = "margin-left:5px"),
      shinyFiles::shinySaveButton(ns("save"), label = NULL, title = "Save file as...", icon = shiny::icon("save"))
    )
  )
}

conditionManagerServer <- function(id, conditions, session) {
  shiny::moduleServer(
    id = id,
    session = session,
    module = function(input, output, session) {
      toReturn <- shiny::reactiveValues(
        conditions = shiny::isolate(conditions),
        trigger = NULL,
        selected = shiny::isolate(input$selected_condition)
      )

      action <- shiny::reactiveVal("")

      volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())

      # User press import button
      shiny::observeEvent(input$import, {
        shinyFiles::shinyFileChoose(input, "import", roots = volumes, session = session, filetypes = c("yaml", "yml"))
        # No selection
        if (is.integer(input$import)) {
          return(toReturn)
        # Valid selection
        } else {
          yaml <- shinyFiles::parseFilePaths(volumes, input$import)
          toReturn$conditions <- yaml::read_yaml(yaml$datapath)
          toReturn$selected <- names(toReturn$conditions)[1]
          toReturn$trigger <- increment_trigger(toReturn$trigger)
          return(toReturn)
        }
      })

      # User press save button
      shiny::observeEvent(input$save, {
        is_saved <- save_file(to_save = toReturn$conditions, filetype = "yaml", input = input$save, session = session, roots = volumes)

        # File saved correctly
        if (is_saved) {
          toReturn$trigger <- increment_trigger(toReturn$trigger)
        }

        return(toReturn)
      })

      # User press remove button
      shiny::observeEvent(input$remove, {
        # TODO ADD CONFIRMATION ?
        # remove condition
        toReturn$conditions[[input$selected_condition]] <- NULL
        # select last condition
        toReturn$selected <- names(toReturn$conditions)[length(toReturn$conditions)]
        toReturn$trigger <- increment_trigger(toReturn$trigger)
        return(toReturn)
      })

      # User press create button
      shiny::observeEvent(input$create, {
        action("create")
        print(action())
        print(session$ns)
        # shiny::showModal(shiny::modalDialog())
        shiny::showModal(namingModal(id), session = session)
        print("modal shown")
      })

      # User press rename button
      shiny::observeEvent(input$rename, {
        action("rename")
        shiny::showModal(namingModal(id), session = session)
      })

      # In modal
      # User validate
      shiny::observeEvent(input$ok, {
        print("ok")
        # Name already exist -> restart modal
        if (input$name %in% names(toReturn$conditions)) {
          shiny::showModal(namingModal(id, condition_exist = TRUE))
        # Name is valid
        } else {
          # Create new condition
          if (action() == "create") {
            toReturn$conditions[[input$name]] <- list(replicates = list(), blanks = list())
          # Rename current condition
          } else if (action() == "rename") {
            toReturn$conditions[[input$name]] <- toReturn$conditions[[input$selected_condition]]
            toReturn$conditions[[input$selected_condition]] <- NULL
          }
          shiny::removeModal()
        }
        toReturn$selected <- input$name
        toReturn$trigger <- increment_trigger(toReturn$trigger)
      })

      shiny::observeEvent(input$selected_condition, {
        toReturn$selected <- input$selected_condition
        toReturn$trigger <- increment_trigger(toReturn$trigger)
      })
      return(toReturn)
    }
  )
}
