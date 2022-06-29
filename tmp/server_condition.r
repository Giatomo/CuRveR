server_condition <- function(input, output, session, rvs) {

  rvs$condition_manager <- reactiveValues(conditions = list())
  rvs$condition_manager <- conditionManagerServer("cond_manager", rvs$condition_manager$conditions, session = session)

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
    rvs$condition_manager$trigger, {
      output$condition_manager <- renderUI({
        conditionManagerUI("cond_manager",
          names(rvs$condition_manager$conditions),
          selected = rvs$condition_manager$selected
        )
      })
    },
    ignoreNULL = FALSE
  )

  replicatesUI <- reactive({
    req(rvs$condition_manager$selected)

    variables_rank_list(
      id = paste(rvs$condition_manager$selected, "replicates", sep = "_"),
      group = "ConditionsSorter",
      text = "Replicates",
      data = rvs$condition_manager$conditions,
      group_attribute = rvs$condition_manager$selected,
      treatment_attribute = "replicates"
    )
  })

  blanksUI <- reactive({
    req(rvs$condition_manager$selected)

    variables_rank_list(
      id = paste(rvs$condition_manager$selected, "blanks", sep = "_"),
      group = "ConditionsSorter",
      text = "Blanks",
      data = rvs$condition_manager$conditions,
      group_attribute = rvs$condition_manager$selected,
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

  observeEvent({
      input[[paste(rvs$condition_manager$selected, "blanks", sep = "_")]]
      input[[paste(rvs$condition_manager$selected, "replicates", sep = "_")]]
      input[["data"]]
    }, {
      req(rvs$condition_manager$selected)

      rvs$condition_manager$conditions[[rvs$condition_manager$selected]][["blanks"]] <- input[[paste(rvs$condition_manager$selected, "blanks", sep = "_")]]
      rvs$condition_manager$conditions[[rvs$condition_manager$selected]][["replicates"]] <- input[[paste(rvs$condition_manager$selected, "replicates", sep = "_")]]
      rvs$entities <- input[["data"]]

      rvs$condition_manager$conditions[[rvs$condition_manager$selected]][["blanks"]] <- gtools::mixedsort(unique(rvs$condition_manager$conditions[[rvs$condition_manager$selected]][["blanks"]]))
      rvs$condition_manager$conditions[[rvs$condition_manager$selected]][["replicates"]] <- gtools::mixedsort(unique(rvs$condition_manager$conditions[[rvs$condition_manager$selected]][["replicates"]]))
      rvs$entities <- gtools::mixedsort(rvs$entities)
    }
  )
}