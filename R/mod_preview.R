mod_preview_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    bs4Dash::box(
      title = "Dataset Preview", 
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      div(style = "margin-bottom: 15px;",
        h5("Dataset Summary"),
        verbatimTextOutput(ns("dataset_summary"))
      ),
      div(style = "margin-bottom: 15px;",
        fluidRow(
          column(4, numericInput(ns("max_rows"), "Max Rows to Display:", value = 100, min = 10, max = 1000)),
          column(4, checkboxInput(ns("show_all_cols"), "Show All Columns", value = FALSE)),
          column(4, downloadButton(ns("download_data"), "Download Data", class = "btn-outline-primary"))
        )
      ),
      reactableOutput(ns("preview_table")),
      br(),
      h5("Variable Information"),
      DT::dataTableOutput(ns("variable_info"))
    )
  )
}

mod_preview_server <- function(id, filepath) {
  moduleServer(id, function(input, output, session) {
    
    # Load dataset when filepath changes
    dataset <- reactive({
      req(filepath())
      tryCatch({
        read_sdtm_adam(filepath())
      }, error = function(e) {
        showNotification(paste("Error loading file:", e$message), type = "error")
        return(NULL)
      })
    })
    
    # Dataset summary
    output$dataset_summary <- renderText({
      req(dataset())
      info <- get_dataset_info(dataset())
      paste(
        paste("Rows:", info$nrows),
        paste("Columns:", info$ncols),
        paste("File:", basename(filepath())),
        sep = " | "
      )
    })
    
    # Main preview table
    output$preview_table <- renderReactable({
      req(dataset())
      data_to_show <- dataset()
      
      # Limit rows
      if (nrow(data_to_show) > input$max_rows) {
        data_to_show <- head(data_to_show, input$max_rows)
      }
      
      # Limit columns if not showing all
      if (!input$show_all_cols && ncol(data_to_show) > 10) {
        data_to_show <- data_to_show[, 1:10]
      }
      
      reactable(
        data_to_show,
        filterable = TRUE,
        searchable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100),
        defaultPageSize = 25,
        theme = reactableTheme(
          borderColor = "#ddd",
          stripedColor = "#f6f8fa"
        )
      )
    })
    
    # Variable information table
    output$variable_info <- DT::renderDataTable({
      req(dataset())
      info <- get_dataset_info(dataset())
      
      var_df <- data.frame(
        Variable = info$variables,
        Type = as.character(info$variable_types),
        Missing_Count = info$missing_counts,
        Missing_Percent = paste0(info$missing_percent, "%"),
        stringsAsFactors = FALSE
      )
      
      var_df
    }, options = list(pageLength = 15, scrollX = TRUE))
    
    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("databoxR_export_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(dataset(), file, row.names = FALSE)
      }
    )
    
    # Return dataset for other modules
    dataset
  })
}
