router <- make_router(
  route("/",  htmlTemplate(filename = "www/main.html", document_ = TRUE)),
  route("load", htmlTemplate(filename = "www/load.html", document_ = TRUE), server = load_server),
  route("format", htmlTemplate(filename = "www/format.html", document_ = TRUE), server = server_format),
  route("conditions", htmlTemplate(filename = "www/conditions.html", document_ = TRUE), server = server_condition)
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  router$ui
)

server <- function(input, output, session) {

    r <- shiny::reactiveValues(conditions = list(Example_condition = list(replicates = character(), blanks = character())))
    router$server(input, output, session, rvs = r)


    output$current_page <- renderUI({
    shiny.router::get_page(session)
  })

}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
