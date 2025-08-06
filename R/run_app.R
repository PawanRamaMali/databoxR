#' Run the databoxR Shiny Application
#'
#' This function launches the main databoxR application for exploring SDTM/ADaM datasets
#'
#' @param ... arguments to pass to shinyApp
#' @return A shiny application object
#' @export
#' @examples
#' \dontrun{
#'   run_databox()
#' }
run_databox <- function(...) {
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = "databoxR"),
    sidebar = bs4Dash::dashboardSidebar(
      mod_sidebar_ui("sidebar")
    ),
    body = bs4Dash::dashboardBody(
      mod_preview_ui("preview"),
      mod_metadata_ui("metadata"),
      mod_eda_ui("eda")
    )
  )
  
  server <- function(input, output, session) {
    # File paths from sidebar
    filepath <- mod_sidebar_server("sidebar")
    
    # Dataset from preview module
    dataset <- mod_preview_server("preview", filepath)
    
    # Pass dataset to other modules
    mod_metadata_server("metadata", dataset)
    mod_eda_server("eda", dataset)
  }
  
  shiny::shinyApp(ui = ui, server = server, ...)
}