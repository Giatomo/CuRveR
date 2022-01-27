`%||%` <- function(a, b) if (is.null(a)) b else a

modalSavePlot <- function() {
  modalDialog(
    title = "Save",
    textInput("filename", "Filename"),
    selectInput("filetype", "Filetype", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg" )),
    numericInput("dpi", "Resolution", 300,  min = 0, max = 1200, step = 10),
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




tabledownloadHandler  <- function(table, file_name, file_type) {
  downloadHandler(

    filename = function() {
      paste0(file_name, ".", file_type)
    },

    content  = function(file) {
      switch (file_type,
        csv = write_csv(table, file = file, col_names = TRUE),
        tsv = write_tsv(table, file = file, col_names = TRUE),
      )
    }
  )
}

plotdownloadHandler  <- function(table, file_name, file_type) {
  downloadHandler(
    filename = function() { paste0(input$filename, ".", input$filetype) },
    content = function(file) { ggsave(file, plot = overview(), device = input$filetype, width = input$width, height = input$height, units = input$units, dpi = input$dpi) }
  )
}

color_tile_from_0 <- function(...) {
  formattable::formatter("span", style = \(x) {formattable::style(display = "block",
                                                                  padding = "0 4px",
                                                                  `border-radius` = "4px",
                                                                  `background-color` = formattable::csscolor(formattable::gradient(c(0,as.numeric(x)),...))[2:length(x)])})
}




download_object_as_rds <- function(object, file_path = tempfile(pattern = "template_", tmpdir = fs::path_home(), fileext = ".Rds")) {

  downloadHandler(
    filename = function() {
      file_path
    },
    content = function(file) {
      saveRDS(object, file)
    })

}
