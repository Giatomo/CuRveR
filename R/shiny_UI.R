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
#' @import tibble

#' @include hello.R


models <- c("Richard", "Linear")
optimizers <- c("Genetic algorithm")
loss_functions <- c("LAD", "OLS")


shiny_UI <- navbarPage(
  title = div(img(
    src = "Logo.svg",
    height = "100%"
  )),
  windowTitle = "CuRveR",
  id = "current_tab",

  tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css",
        integrity = "sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH",
        crossorigin = "anonymous"),
      tags$script(
        src = "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js",
        integrity = "sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm",
        crossorigin = "anonymous")
    ),

  ##################################################################
  ##                            Footer                            ##
  ##################################################################



  ##################################################################
  ##                         Data loading                         ##
  ##################################################################

  tabPanel("Load Data",
    value = 1,
    fluidRow(
      shinyFiles::shinyFilesButton("files", label = "File select", title = "Please select a file", multiple = TRUE)
    ),
    fluidRow(
      column(
        4,
        numericInput("n_signals", "Numbers of signal(s) to analyse:",
          value = 1, min = 1, max = 10, step = 1
        ),
      )
    ),
    fluidRow(
      column(
        12,
        uiOutput("signals")
      )
    ),
    fluidRow(
      column(2,
        offset = 5,
        actionButton("load_data", "Load data", style = "display:center-align")
      )
    )
  ),



  tabPanel("Format Data",
  value = 10,
    fluidRow(
        column(
          4,
          div(style = "width:100%;overflow-x: scroll;height:50vh;overflow-y: scroll;", uiOutput("columns"))
        ),
        column(
          4,
          div(style = "width:100%;overflow-x: scroll;height:50vh;overflow-y: scroll;", uiOutput("xdata"))
        ),
        column(
          4,
          div(style = "width:100%;overflow-x: scroll;height:50vh;overflow-y: scroll;", uiOutput("ydata"))
        )
      ),
      fluidRow(
        actionButton("format_data", "Format")
      )
  ),

  #################################################################
  ##                        Data overview                        ##
  #################################################################

  tabPanel("Overview",
    value = 2,
    fluidRow(
      column(
        3,
        uiOutput("signal_selector_overview")
      )
    ),
    fluidRow(
      column(
        12,
        plotOutput("overview", height = "80vh")
      )
    ),
    fluidRow(
      column(
        3,
        actionButton("dl_overview", "Save")
      ),
      column(4,
        offset = 1,
        actionButton("goto_condition", "Next step")
      )
    )
  ),

  #################################################################
  ##                     Conditions & Blanks                     ##
  #################################################################

  tabPanel("Conditions",
    value = 3,
    fluidRow(
      uiOutput("condition_manager"),
      fluidRow(
        column(
          4,
          div(style = "width:100%;overflow-x: scroll;height:50vh;overflow-y: scroll;", uiOutput("entities"))
        ),
        column(
          4,
          div(style = "width:100%;overflow-x: scroll;height:50vh;overflow-y: scroll;", uiOutput("replicates"))
        ),
        column(
          4,
          div(style = "width:100%;overflow-x: scroll;height:50vh;overflow-y: scroll;", uiOutput("blanks"))
        ),
      )
    )
  ),

  #################################################################
  ##                           Cutoffs                           ##
  #################################################################

  tabPanel("Cutoffs",
    value = 4,
    fluidRow(
      column(
        4,
        uiOutput("signal_selector_cutoff")
      ),
      column(
        4,
        uiOutput("well_selector_cutoff")
      ),
      column(
        4,
        checkboxInput("same_cutoff", "Same cutoff for all wells")
      )
    ),
    fluidRow(
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
  ),

  #################################################################
  ##                             Fit                             ##
  #################################################################

  tabPanel("Fit",
    value = 5,
    shiny::fillRow(
      flex = c(NA, NA, NA, NA),
      shiny::selectInput("optim_method", "Model method", choices = models),
      shiny::selectInput("optim_method", "Optimization method", choices = optimizers),
      shiny::selectInput("loss_fct", "Loss function", choices = loss_functions),
      shiny::actionButton("fit", "Fit")
    ),
    fluidRow(
      column(
        12,
        div(
          style = "width:100%;overflow-x: scroll;height:15vh;overflow-y: scroll;",
          formattable::formattableOutput("fit_data")
        )
      )
    ),
    fluidRow(
      column(
        3,
        actionButton("dl_performance", "Save")
      )
    ),
    fluidRow(
      column(
        12,
        div(
          style = "width:100%;overflow-x: scroll;height:30vh;overflow-y: scroll;",
          formattable::formattableOutput("data_by_condition")
        )
      )
    ),
    fluidRow(
      column(
        3,
        actionButton("dl_data", "Save")
      )
    ),
    fluidRow(
      column(
        12,
        div(
          style = "width:100%;overflow-x: scroll;height:15vh;overflow-y: scroll;",
          formattable::formattableOutput("data_recap")
        )
      )
    ),
  ),

  #################################################################
  ##                            Plots                            ##
  #################################################################

  tabPanel("Plots",
    value = 6,
    fluidRow(
      column(
        3,
        radioButtons("plot_type", "Plot Type", choices = c(
          "Metric comparison" = "metric_comparison",
          "Metric Visualization" = "metric_visualization",
          "Data Comparison" = "data_comparison"
        ))
      ),
      column(
        3,
        conditionalPanel(
          "input.plot_type == 'metric_comparison'",
          radioButtons("metric_to_compare", "Metric",
            choiceValues = c("r_max", "p_max", "p_min", "s"),
            choiceNames = c(
              "r max",
              "p max",
              "p min",
              "s"
            )
          )
        ),
        conditionalPanel(
          "input.plot_type == 'metric_visualization'",
          checkboxGroupInput("metric_to_visualize", "Metric",
            choiceValues = c("r_max", "p_max", "p_min", "s", "fit", "data_point"),
            choiceNames = c(
              "r max",
              "p max",
              "p min",
              "s",
              "Fit",
              "Data points"
            ),
            selected = c("r_max", "p_max", "p_min", "s", "fit", "data_point")
          )
        ),
        conditionalPanel(
          "input.plot_type == 'data_comparison'",
          checkboxGroupInput("data_to_compare", "Plot features",
            choices = c(
              "Mean" = "mean",
              "SD as bar" = "sd_bar",
              "SD as halo" = "sd_halo",
              "Fit" = "fit",
              "Data points" = "data_points"
            ),
            selected = c("mean", "sd_halo", "fit", "data_points")
          )
        )
      ),
      column(
        3,
        uiOutput("signal_selector_plot")
      ),
      column(
        3,
        uiOutput("condition_selector_plot")
      )
    ),
    fluidRow(
      column(
        12,
        plotOutput("final_plot"),
      )
    ),
    fluidRow(
      column(
        3,
        actionButton("dl_final_plot", "Save")
      )
    )
  )
)
