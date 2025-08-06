mod_preview_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "Dataset Preview", width = 12,
        reactableOutput(ns("preview_table"))
    )
  )
}

mod_preview_server <- function(id, filepaths) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactive({
      req(filepaths())
      read_sdtm_adam(filepaths()[1]) # read first for demo
    })
    output$preview_table <- renderReactable({
      req(dataset())
      reactable(head(dataset(), 100))
    })
    dataset
  })
}
