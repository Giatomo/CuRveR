#' @import shiny
#' @export run_curver
run_curver <- function() {
  shinyApp(ui = shiny_UI, server = shiny_server)
}
