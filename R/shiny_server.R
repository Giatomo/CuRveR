#' @import sortable
#' @import purrr
#' @import readxl
#' @import lubridate
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import ggplot2
#' @import sf
#' @import svglite
#' @import shiny
#' @import gtools
#' @import readr

#' @include server_utils.R
#' @include models.R
#' @include optimizer.R
#' @include fitness.R

ACCEPTED_FILETYPES <- c("xlsx", "xls", "csv", "tsv", "txt")


is_excel_file <- function(file) {
  if (is.null(file)) {
    return(FALSE)
  }
  return(fs::path_ext(file) %in% c("xlsx", "xls"))
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

shiny_server <- function(input, output, session) {
  rvs <- reactiveValues(conditions = list(Example_condition = list(replicates = character(), blanks = character())))

  max_tab <- 8

  ##################################################################
  ##                            Footer                            ##
  ##################################################################

  ## ------------------------------
  ##  Previous/Next page buttons
  ## ------------------------------

  observeEvent(input$next_tab, {
    current_tab <- as.numeric(input$current_tab)
    if (current_tab < max_tab) {
      updateNavbarPage(session, "current_tab", selected = paste(current_tab + 1))
    }
  })

  ##################################################################
  ##                         Data loading                         ##
  ##################################################################

  ## -------------------------
  ##  Map Name - File/Sheet
  ## -------------------------


  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFiles::shinyFileChoose(input, "files", roots = volumes, filetypes = ACCEPTED_FILETYPES)

  output$signals <- renderUI({
    req(!is.integer(input$files))

    files <- shinyFiles::parseFilePaths(volumes, input$files)


    paths <- files$datapath

    names(paths) <- files$name
    print(paths)
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

  ## -------------
  ##  Load Data
  ## -------------


  observeEvent(input$load_data, {
    files <- shinyFiles::parseFilePaths(volumes, input$files)


    paths <- files$datapath

    names(paths) <- files$name

    rvs$data <- purrr::map(
      1:input$n_signals,
      \(x) {
        file_n <- paste0("file_", x)
        sheet_n <- paste0("sheet_", x)
        name_n <- paste0("signal_", x)

        if (is_excel_file(fs::path(input[[file_n]]))) {
          readxl::read_xlsx(
            path = fs::path(input[[file_n]]),
            sheet = input[[sheet_n]]
          ) |> mutate(signal = input[[name_n]])
        } else {
          read_csv(
            file = fs::path(input[[file_n]]),
            col_names = TRUE
          ) |> mutate(signal = input[[name_n]])
        }
      }
    ) |> dplyr::bind_rows()


    # Tidy loaded data

    rvs$columns <<- colnames(rvs$data)

    rvs$data |>
      select(-matches(regex("T°"))) |>
      clean_time(Time) |>
      format_wellplate_long() |>
      extract(well, into = c("row", "col"), regex = "(\\w)(\\d+)", remove = FALSE) |>
      mutate(
        col = as.numeric(col),
        left_cutoff = min(Time),
        right_cutoff = max(Time)
      ) -> rvs$data



    rvs$entities <<- gtools::mixedsort(unique(rvs$data[["well"]]))
    
    rvs$all_signals <<- unique(rvs$data[["signal"]])


    updateNavbarPage(session, "current_tab", selected = "2")
  })


#######################
  columnsUI <- reactive({
      variables_rank_list(
        id = "columns",
        group = "ColumnsSorter",
        text = "Columns",
        labels = rvs$columns,
        remove_dragged_elements = TRUE
      )
    })

  xDataUI <- reactive({
    variables_rank_list(
      id = "xdata",
      group = "ColumnsSorter",
      text = "X Data",
      labels = rvs$xdata,
      put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
      remove_dragged_elements = TRUE
    )
  })

  yDataUI <- reactive({
    variables_rank_list(
      id = "ydata",
      group = "ColumnsSorter",
      text = "Y Data(s)",
      labels = rvs$ydata,
      remove_dragged_elements = TRUE
    )
  })

  infoDataUI <- reactive({
    variables_rank_list(
      id = "infodata",
      group = "ColumnsSorter",
      text = "Info Data",
      labels = rvs$infodata,
      remove_dragged_elements = TRUE
    )
  })

  output$columns <- renderUI({
    columnsUI()
  })
  output$xdata <- renderUI({
    xDataUI()
  })
  output$ydata <- renderUI({
    yDataUI()
  })
  output$infodata <- renderUI({
    infoDataUI()
  })



  observeEvent(
    {
      input[["columns"]]
      input[["xdata"]]
      input[["ydata"]]
      input[["infodata"]]

    },
    {
      rvs$columns <- input[["columns"]]
      rvs$xdata <- input[["xdata"]]
      rvs$ydata <- input[["ydata"]]
      rvs$infodata <- input[["infodata"]]

      rvs$columns <- gtools::mixedsort(rvs$columns)
      rvs$xdata <- gtools::mixedsort(rvs$xdata)
      rvs$ydata <- gtools::mixedsort(rvs$ydata)
      rvs$infodata <- gtools::mixedsort(rvs$infodata)
    }
  )
#########################

  #################################################################
  ##                        Data overview                        ##
  #################################################################

  ## -------------------
  ##  Signal Selector
  ## -------------------

  output$signal_selector_overview <- renderUI({
    req(rvs$data)
    selectInput("selected_signal_overview", label = NULL, choices = unique(rvs$data[["signal"]]))
  })



  ## -----------------------
  ##  Plot Plate Overview
  ## -----------------------

  overview <- reactive({
    req(rvs$data, input$selected_signal_overview)

    rvs$data |>
      filter(signal == input$selected_signal_overview) |>
      ggplot(aes(x = Time, y = value)) +
      geom_line() +
      facet_grid(col ~ row) +
      theme_minimal()
  })

  output$overview <- renderPlot({
    overview()
  })

  observeEvent(input$dl_overview, {
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste0(input$filename, ".", input$filetype)
      },
      content = function(file) {
        ggsave(file, plot = overview(), device = input$filetype, width = input$width, height = input$height, units = input$units, dpi = input$dpi)
      }
    )
    showModal(modalSavePlot())
  })

  observeEvent(input$goto_condition, {
    updateNavbarPage(session, "current_tab", selected = "3")
  })
  #################################################################
  ##                     Conditions & Blanks                     ##
  #################################################################

  wells <- reactive({
    gtools::mixedsort(unique(rvs$data[["well"]]))
  })



  condition_manager <- reactiveValues(conditions = list())
  condition_manager <- conditionManagerServer("cond_manager", condition_manager$conditions)

  entitiesUI <- reactive({
    variables_rank_list(
      id = "data",
      group = "ConditionsSorter",
      text = "Entities",
      labels = rvs$entities,
      pull = "clone",
      remove_dragged_elements = TRUE
    )
  })

  observeEvent(
    condition_manager$trigger,
    {
      output$condition_manager <- renderUI({
        conditionManagerUI("cond_manager",
          names(condition_manager$conditions),
          selected = condition_manager$selected
        )
      })
    },
    ignoreNULL = FALSE
  )

  replicatesUI <- reactive({
    req(condition_manager$selected)

    variables_rank_list(
      id = paste(condition_manager$selected, "replicates", sep = "_"),
      group = "ConditionsSorter",
      text = "Replicates",
      data = condition_manager$conditions,
      group_attribute = condition_manager$selected,
      treatment_attribute = "replicates"
    )
  })

  blanksUI <- reactive({
    req(condition_manager$selected)

    variables_rank_list(
      id = paste(condition_manager$selected, "blanks", sep = "_"),
      group = "ConditionsSorter",
      text = "Blanks",
      data = condition_manager$conditions,
      group_attribute = condition_manager$selected,
      treatment_attribute = "blanks"
    )
  })

  output$entities <- renderUI({
    entitiesUI()
  })
  output$replicates <- renderUI({
    replicatesUI()
  })
  output$blanks <- renderUI({
    blanksUI()
  })

  observeEvent(
    {
      input[[paste(condition_manager$selected, "blanks", sep = "_")]]
      input[[paste(condition_manager$selected, "replicates", sep = "_")]]
      input[["data"]]
    },
    {
      req(condition_manager$selected)

      condition_manager$conditions[[condition_manager$selected]][["blanks"]] <- input[[paste(condition_manager$selected, "blanks", sep = "_")]]
      condition_manager$conditions[[condition_manager$selected]][["replicates"]] <- input[[paste(condition_manager$selected, "replicates", sep = "_")]]
      rvs$entities <- input[["data"]]

      condition_manager$conditions[[condition_manager$selected]][["blanks"]] <- gtools::mixedsort(unique(condition_manager$conditions[[condition_manager$selected]][["blanks"]]))
      condition_manager$conditions[[condition_manager$selected]][["replicates"]] <- gtools::mixedsort(unique(condition_manager$conditions[[condition_manager$selected]][["replicates"]]))
      rvs$entities <- gtools::mixedsort(rvs$entities)
    }
  )



  condition_overview <- reactive({
    req(condition_manager$conditions)
    if (length(condition_manager$conditions) > 0) {
      condition_manager$conditions |>
        rrapply::rrapply(how = "melt") |>
        unnest_longer(value) |>
        rename(condition = L1, type = L2, well = value) |>
        extract(well, into = c("row", "col"), regex = "(\\w)(\\d+)", remove = FALSE) |>
        drop_na(well, row, col) |>
        mutate(col = as.numeric(col)) |>
        ggplot(aes(col, row)) +
        ggpattern::geom_tile_pattern(aes(pattern_density = type, fill = condition),
          pattern = "stripe",
          pattern_angle = 45,
          pattern_fill = "black"
        ) +
        ggpattern::scale_pattern_density_discrete(breaks = c(0.5, 0)) +
        scale_x_discrete(breaks = factor(1:12), limits = factor(1:12)) +
        scale_y_discrete(breaks = factor(LETTERS[8:1]), limits = factor(LETTERS[8:1])) +
        theme_minimal()
    }
  })

  output$condition_recap <- renderPlot({
    condition_overview()
  })


  #################################################################
  ##                           Cutoffs                           ##
  #################################################################

  output$signal_selector_cutoff <- renderUI({
    req(rvs$all_signals)
    selectInput("selected_signal_cutoff", label = NULL, choices = rvs$all_signals)
  })

  output$well_selector_cutoff <- renderUI({
    req(rvs$entities)
    selectInput("selected_well_cutoff", label = NULL, choices = rvs$entities)
  })


  observeEvent(
    {
      input$cutoff_brush
      input$same_cutoff
    },
    {
      req(rvs$data, input$selected_signal_cutoff, input$selected_well_cutoff)

      if (input$same_cutoff == TRUE) {
        isolate({
          rvs$data |>
            rowwise() |>
            mutate(
              left_cutoff = input$cutoff_brush[["xmin"]] %||% rvs$last_left_cutoff %||% left_cutoff,
              right_cutoff = input$cutoff_brush[["xmax"]] %||% rvs$last_right_cutoff %||% right_cutoff
            ) |>
            ungroup() -> rvs$data
        })
      } else {
        req(input$cutoff_brush)

        isolate({
          rvs$data |>
            rowwise() |>
            mutate(
              left_cutoff = if_else(well == input$selected_well_cutoff && signal == input$selected_signal_cutoff, input$cutoff_brush[["xmin"]], left_cutoff),
              right_cutoff = if_else(well == input$selected_well_cutoff && signal == input$selected_signal_cutoff, input$cutoff_brush[["xmax"]], right_cutoff)
            ) |>
            ungroup() -> rvs$data
        })
      }


      req(input$cutoff_brush)

      rvs$last_left_cutoff <- input$cutoff_brush[["xmin"]]
      rvs$last_right_cutoff <- input$cutoff_brush[["xmax"]]
    }
  )

  output$plot <- renderPlot({
    req(rvs$data, input$selected_signal_cutoff, input$selected_well_cutoff)

    max_value <- max(rvs$data[["value"]])
    max_time <- max(rvs$data[["Time"]])

    annot <- rvs$data |>
      filter(
        signal == input$selected_signal_cutoff,
        well == input$selected_well_cutoff
      ) |>
      summarise(
        left_cutoff = median(left_cutoff),
        right_cutoff = median(right_cutoff),
        value = max(value)
      )

    rvs$data |>
      filter(
        signal == input$selected_signal_cutoff,
        well == input$selected_well_cutoff
      ) |>
      ggplot(aes(x = Time, y = value)) +
      annotate(
        geom = "rect",
        ymin = 0,
        ymax = annot$value + 0.2 * annot$value,
        xmin = annot$left_cutoff,
        xmax = annot$right_cutoff,
        alpha = 0.15,
        fill = "#3DB2FF"
      ) +
      geom_line(color = "#F9B208", size = 1) +
      geom_point(color = "white", fill = "#F98404", size = 3.5, shape = 23) +
      geom_vline(aes(xintercept = median(left_cutoff)),
        size = 1, linetype = "dashed"
      ) +
      geom_vline(aes(xintercept = median(right_cutoff)),
        size = 1, linetype = "dashed"
      ) +
      expand_limits(x = 0, y = 0) +
      scale_x_continuous(breaks = seq(0, max_time, by = 2)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_minimal() +
      theme(text = element_text(size = 20))
  })

  #################################################################
  ##                             Fit                             ##
  #################################################################

  observeEvent(input$fit, {
    req(rvs$data, condition_manager$conditions)

    condition <- condition_manager$conditions |>
      rrapply::rrapply(how = "melt") |>
      unnest_longer(value) |>
      rename(condition = L1, type = L2, well = value)

    rvs$data |>
      full_join(condition, by = c("well" = "well")) |>
      rowwise() |>
      mutate(in_cutoff = between(Time, left_cutoff, right_cutoff)) |>
      group_by(condition, signal) |>
      mutate(blank = mean(value[in_cutoff & type == "blanks"])) |>
      rowwise() |>
      mutate(value = if_else(is.na(value - blank), value, value - blank)) |>
      filter(type != "blanks") |>
      ungroup() -> data_to_fit

    data_to_fit |>
      rowwise() |>
      filter(between(Time, left_cutoff, right_cutoff)) |>
      fit_data(c(signal, condition), value, Time) -> rvs$parameters_by_condition

    rvs$parameters_by_condition |>
      mutate(params = list(model$estimated)) |>
      unnest_wider(params) -> rvs$parameters

    print(rvs$parameters)

    rvs$parameters_by_condition |>
      mutate(params = list(model$estimated)) |>
      unnest_wider(params) |>
      mutate(fit = list(model$predict(Time)))  |>
      select(-model) |>
      rowwise() |>
      mutate(
        Time = c(Time),
        value = c(value),
        fit = c(fit)
      ) |>
      unnest(c(signal, condition, Time, value, fit)) -> rvs$data_by_condition

    rvs$parameters_by_condition |>
      mutate(params = list(model$estimated)) |>
      unnest_wider(params)  |>
      select(-model) -> rvs$parameters

    rvs$parameters_by_condition |>
      mutate(fit = list(model$predict(Time)))  |>
      rowwise() |>
      mutate(
        Time = c(Time),
        value = c(value),
        fit = c(fit)) |>
      select(-model) |>
      unnest(c(signal, condition, Time, value, fit)) |>
      group_by(signal, condition) |>
      summarise(
        VEcv = variance_explained(value, fit),
        d_r = willmott_index_agreement(value, fit)) -> rvs$perfomances


    rvs$all_conditions <- unique(rvs$data_by_condition[["condition"]])

    output$fit_data <- formattable::renderFormattable({
      formattable::formattable(
        rvs$perfomances,
        list(
          VEcv = color_tile_from_0("lightpink", "lightblue"),
          d_r  = color_tile_from_0("lightpink", "lightblue")
        )
      )
    })

    output$data_by_condition <- formattable::renderFormattable({
      formattable::formattable(rvs$data_by_condition)
    })

    output$data_recap <- formattable::renderFormattable({
      formattable::formattable(rvs$parameters)
    })
  })


  observeEvent(input$dl_performance, {
    output$downloadData <- tabledownloadHandler(rvs$perfomances, input$filename, input$filetype)

    showModal(modalDialog(
      title = "Save",
      textInput("filename", "Filename"),
      selectInput("filetype", "Filetype", choices = c("csv")),
      downloadButton("downloadData", "Download"),
    ))
  })

  observeEvent(input$dl_data, {
    output$downloadData <- tabledownloadHandler(rvs$data_by_condition, input$filename, input$filetype)

    showModal(modalDialog(
      title = "Save",
      textInput("filename", "Filename"),
      selectInput("filetype", "Filetype", choices = c("csv")),
      downloadButton("downloadData", "Download"),
    ))
  })


  #################################################################
  ##                            Plots                            ##
  #################################################################

  output$signal_selector_plot <- renderUI({
    req(rvs$all_signals)
    radioButtons("selected_signal_plot", label = "Signal", choices = rvs$all_signals)
  })

  output$condition_selector_plot <- renderUI({
    req(rvs$all_signals)
    checkboxGroupInput("selected_condition_plot", label = "Conditions", choices = rvs$all_conditions, selected = rvs$all_conditions)
  })

  final_plot <- reactive({

    req(rvs$data_by_condition)


    print(rvs$data_by_condition)

    rvs$data_by_condition |>
      filter(signal == input$selected_signal_plot) |>
      filter(condition %in% input$selected_condition_plot) |>
      drop_na(condition) -> plot_data

    rvs$data_by_condition |>
      filter(signal == input$selected_signal_plot) |>
      filter(condition %in% input$selected_condition_plot) |>
      drop_na(condition) |>
      group_by(condition, Time) |>
      summarise(sd = sd(value), mean = mean(value)) -> summarized_plot_data

    if (input$plot_type == "metric_comparison") {
      plot_data |>
        ggplot(aes(y = condition, x = !!as.symbol(input$metric_to_compare))) +
        geom_point(size = 5, color = "#e8871a") +
        theme_minimal() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) -> p
    } else if (input$plot_type == "metric_visualization") {
      print("metric_visualization")

      plot_data |>
        mutate(intercept = richard(s, p_max, p_min, r_max, s) - r_max * s) |>
        ggplot(aes(x = Time, y = value, group = well)) -> p


      for (metrics in input$metric_to_visualize) {

      }
      if ("data_point" %in% input$metric_to_visualize) {
        p <- p + geom_point(aes(x = Time, y = value, group = well), alpha = .4, color = "#7570b3")
      }

      if ("p_max" %in% input$metric_to_visualize) {
        p <- p + geom_hline(aes(yintercept = p_max), linetype = "dashed", color = "#1B9E77")
      }

      if ("p_min" %in% input$metric_to_visualize) {
        p <- p + geom_hline(aes(yintercept = p_min), linetype = "dashed", color = "#1B9E77")
      }

      if ("s" %in% input$metric_to_visualize) {
        p <- p + geom_segment(aes(
          x = s, xend = s,
          y = 0, yend = richard(s, p_max, p_min, r_max, s)
        ),
        linetype = "dashed",
        color = "#1B9E77"
        )
      }

      if ("r_max" %in% input$metric_to_visualize) {
        p <- p + geom_line(aes(y = linear(Time, r_max, intercept)), linetype = "dashed", color = "#1B9E77")
      }


      if ("fit" %in% input$metric_to_visualize) {
        p <- p + geom_line(aes(y = fit), color = "black")
      }

      p <- p + scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
        facet_wrap(~condition) +
        theme_minimal()
    } else if (input$plot_type == "data_comparison") {
      plot_data |>
        ggplot(aes(x = Time, color = condition, fill = condition)) -> p

      if ("data_points" %in% input$data_to_compare) {
        p <- p + geom_point(aes(y = value, group = well), size = 2, alpha = .4)
      }

      if ("sd_halo" %in% input$data_to_compare) {
        p <- p + geom_ribbon(
          data = summarized_plot_data,
          aes(
            ymin = mean - sd,
            ymax = mean + sd
          ),
          alpha = .2
        )
      }

      if ("sd_bar" %in% input$data_to_compare) {
        p <- p + geom_errorbar(
          data = summarized_plot_data,
          aes(
            ymin = mean - sd,
            ymax = mean + sd
          ),
          alpha = .4
        )
      }

      if ("mean" %in% input$data_to_compare) {
        p <- p + geom_line(
          data = summarized_plot_data,
          aes(y = mean),
          size = 1,
          linetype = "dashed"
        )
      }

      if ("fit" %in% input$data_to_compare) {
        p <- p + geom_line(aes(y = fit, group = well), size = 1)
      }


      p <- p +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
        theme_minimal()
    }

    p
  })

  output$final_plot <- renderPlot({
    final_plot()
  })

  observeEvent(input$dl_final_plot, {
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste0(input$filename, ".", input$filetype)
      },
      content = function(file) {
        ggsave(file, plot = final_plot(), device = input$filetype, width = input$width, height = input$height, units = input$units, dpi = input$dpi)
      }
    )
    showModal(modalSavePlot())
  })
}
