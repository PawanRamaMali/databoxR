#' Run the databoxR Shiny Application
#'
#' This function launches the main databoxR application for exploring SDTM/ADaM datasets
#'
#' @param demo_data Logical, if TRUE loads sample datasets automatically. Default is FALSE.
#' @param ... arguments to pass to shinyApp
#' @return A shiny application object
#' @export
#' @examples
#' \dontrun{
#'   run_databox()
#'   run_databox(demo_data = TRUE)  # Load with sample data
#' }
run_databox <- function(demo_data = FALSE, ...) {
  ui <- bs4Dash::dashboardPage(
    title = "databoxR - SDTM/ADaM Explorer",
    header = bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "databoxR",
        color = "primary",
        href = "#",
        image = NULL
      )
    ),
    sidebar = bs4Dash::dashboardSidebar(
      skin = "light",
      status = "primary",
      elevation = 3,
      mod_sidebar_ui("sidebar")
    ),
    body = bs4Dash::dashboardBody(
      bs4Dash::tabItems(
        bs4Dash::tabItem(
          tabName = "upload",
          h3("File Upload & Management", class = "text-primary"),
          p("Upload your SDTM/ADaM datasets or use the demo data to get started."),
          br(),
          fluidRow(
            bs4Dash::box(
              title = "Getting Started", 
              status = "info", 
              solidHeader = TRUE,
              width = 12,
              "Welcome to databoxR! Upload your files using the sidebar or enable demo mode to explore sample datasets.",
              br(), br(),
              if (demo_data) {
                div(
                  class = "alert alert-success",
                  role = "alert",
                  style = "margin: 10px 0;",
                  icon("check-circle"), " Demo data has been loaded automatically! Select a file from the sidebar to begin exploring."
                )
              } else {
                div(
                  class = "alert alert-info", 
                  role = "alert",
                  style = "margin: 10px 0;",
                  icon("info-circle"), " Use the file upload widget in the sidebar to get started with your own data."
                )
              }
            )
          )
        ),
        bs4Dash::tabItem(
          tabName = "explorer",
          mod_preview_ui("preview")
        ),
        bs4Dash::tabItem(
          tabName = "analysis",
          div(class = "container-fluid px-4",
            div(class = "row g-4",
              # Enhanced header section
              div(class = "col-12 mb-3",
                div(class = "d-flex align-items-center justify-content-between",
                  div(
                    h2(class = "text-primary mb-1",
                      icon("microscope", class = "fa-fw me-2"),
                      "Advanced Analytics Suite"
                    ),
                    p(class = "text-muted mb-0", 
                      "Comprehensive metadata exploration and statistical analysis tools")
                  ),
                  div(class = "d-flex gap-2",
                    tags$button(class = "btn btn-outline-primary btn-sm",
                      type = "button", 
                      onclick = "document.getElementById('metadata-card').scrollIntoView({behavior: 'smooth'});",
                      icon("search"), " Metadata"
                    ),
                    tags$button(class = "btn btn-outline-success btn-sm",
                      type = "button",
                      onclick = "document.getElementById('eda-card').scrollIntoView({behavior: 'smooth'});", 
                      icon("chart-line"), " Analytics"
                    )
                  )
                )
              ),
              # Metadata Explorer Section
              div(class = "col-12",
                div(id = "metadata-card", mod_metadata_ui("metadata"))
              ),
              # Divider
              div(class = "col-12",
                hr(class = "my-4", style = "border-top: 2px solid #e9ecef;")
              ),
              # EDA Section  
              div(class = "col-12",
                div(id = "eda-card", mod_eda_ui("eda"))
              )
            )
          )
        )
      )
    ),
    controlbar = NULL,
    footer = bs4Dash::dashboardFooter(
      left = "databoxR v0.1.0",
      right = "Clinical Data Explorer"
    )
  )
  
  server <- function(input, output, session) {
    # File paths from sidebar (with optional demo data)
    sidebar_data <- mod_sidebar_server("sidebar", demo_data = demo_data)
    
    # Dataset from preview module
    dataset <- mod_preview_server("preview", sidebar_data$filepath, sidebar_data$demo_datasets)
    
    # Pass dataset to other modules
    mod_metadata_server("metadata", dataset)
    mod_eda_server("eda", dataset)
    
    # Auto-navigate to explorer when data is loaded
    observeEvent(dataset(), {
      if (!is.null(dataset())) {
        bs4Dash::updateTabItems(session, "sidebar", "explorer")
        showNotification("Dataset loaded successfully! Navigate to other tabs to explore.", 
                        type = "default", duration = 3)
      }
    })
    
    # Show welcome message for demo mode
    if (demo_data) {
      showModal(modalDialog(
        title = "Welcome to databoxR Demo!",
        "Sample CDISC datasets have been loaded. Explore the different tabs to see the features:",
        tags$ul(
          tags$li("Dataset Explorer: Interactive data preview"),
          tags$li("Analysis: Metadata exploration and EDA")
        ),
        footer = modalButton("Let's Explore!"),
        easyClose = TRUE
      ))
    }
  }
  
  shiny::shinyApp(ui = ui, server = server, ...)
}