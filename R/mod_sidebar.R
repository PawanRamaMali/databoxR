mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  sidebarMenu(
    fileInput(ns("folder"), "Upload SDTM/ADaM Folder", multiple = TRUE)
  )
}

mod_sidebar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      req(input$folder)
      input$folder$datapath
    })
  })
}
