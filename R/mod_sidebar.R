mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  bs4Dash::sidebarMenu(
    id = ns("sidebar"),
    bs4Dash::menuItem(
      text = "File Upload",
      tabName = "upload",
      icon = shiny::icon("upload")
    ),
    bs4Dash::menuItem(
      text = "Dataset Explorer",
      tabName = "explorer", 
      icon = shiny::icon("table")
    ),
    bs4Dash::menuItem(
      text = "Data Analysis",
      tabName = "analysis",
      icon = shiny::icon("chart-bar")
    ),
    br(),
    div(style = "padding: 10px;",
      fileInput(
        ns("folder"), 
        "Upload SDTM/ADaM Files",
        multiple = TRUE,
        accept = c(".xpt", ".csv", ".txt")
      ),
      conditionalPanel(
        condition = "output.files_uploaded",
        ns = ns,
        h5("Uploaded Files:"),
        DT::dataTableOutput(ns("file_list"))
      )
    )
  )
}

mod_sidebar_server <- function(id, demo_data = FALSE) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for file management
    files_data <- reactiveValues(
      paths = NULL,
      names = NULL,
      selected_file = NULL
    )
    
    # Load demo data if requested
    if (demo_data) {
      observe({
        demo_files <- list.files(system.file("extdata", package = "databoxR"), 
                                pattern = "*.csv", full.names = TRUE)
        demo_names <- basename(demo_files)
        
        if (length(demo_files) > 0) {
          files_data$paths <- demo_files
          files_data$names <- demo_names
          
          showNotification("Demo data loaded automatically!", type = "default")
        }
      })
    }
    
    # Handle file uploads
    observeEvent(input$folder, {
      req(input$folder)
      files_data$paths <- input$folder$datapath
      files_data$names <- input$folder$name
    })
    
    # Output file list table
    output$file_list <- DT::renderDataTable({
      req(files_data$names)
      data.frame(
        File = files_data$names,
        Size = file.size(files_data$paths),
        stringsAsFactors = FALSE
      )
    }, options = list(pageLength = 10, dom = 't'), selection = 'single')
    
    # Track if files are uploaded
    output$files_uploaded <- reactive({
      !is.null(files_data$paths)
    })
    outputOptions(output, "files_uploaded", suspendWhenHidden = FALSE)
    
    # Return selected file path
    reactive({
      if (!is.null(input$file_list_rows_selected)) {
        files_data$paths[input$file_list_rows_selected]
      } else if (!is.null(files_data$paths)) {
        files_data$paths[1]  # Default to first file
      } else {
        NULL
      }
    })
  })
}
