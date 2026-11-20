mod_preview_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    bs4Dash::box(
      title = "Dataset Preview", 
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      # Loading overlay
      conditionalPanel(
        condition = "output.dataset_loaded == false",
        ns = ns,
        div(
          style = "text-align: center; padding: 50px;",
          h4("No dataset loaded", style = "color: #6c757d;"),
          p("Upload files using the sidebar or enable demo mode to get started.", style = "color: #6c757d;"),
          icon("upload", "fa-3x", style = "color: #dee2e6;")
        )
      ),
      # Main content when data is loaded
      conditionalPanel(
        condition = "output.dataset_loaded == true",
        ns = ns,
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
        div(
          shinycssloaders::withSpinner(reactableOutput(ns("preview_table")), type = 4),
          br(),
          h5("Variable Information"),
          shinycssloaders::withSpinner(DT::dataTableOutput(ns("variable_info")), type = 4)
        ),
        # Reproducible R code for the current preview (load + column/row limits)
        div(style = "margin-top: 15px;",
          tags$details(
            tags$summary(
              style = "cursor: pointer; font-weight: 500; padding: 4px 0;",
              icon("code"), " Reproducible R code"
            ),
            div(style = "margin-top: 10px;",
              downloadButton(ns("download_script"), "Download R script",
                             class = "btn-outline-secondary btn-sm"),
              br(), br(),
              verbatimTextOutput(ns("r_code"))
            )
          )
        )
      )
    )
  )
}

mod_preview_server <- function(id, filepath, demo_datasets = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Load dataset when filepath changes
    dataset <- reactive({
      req(filepath())
      
      # Show loading notification
      show_id <- showNotification("Loading dataset...", duration = NULL, type = "default")
      
      result <- tryCatch({
        # Check if this is demo data
        if (startsWith(filepath(), "DEMO:")) {
          domain <- sub("DEMO:", "", filepath())
          # Get demo datasets (could be reactive or static)
          datasets <- if (is.reactive(demo_datasets)) demo_datasets() else demo_datasets
          if (!is.null(datasets) && domain %in% names(datasets)) {
            data <- datasets[[domain]]
            
            # Ensure proper data types for demo data
            if ("AGE" %in% names(data)) {
              data$AGE <- as.numeric(data$AGE)
            }
            if ("VSSTRESN" %in% names(data)) {
              data$VSSTRESN <- as.numeric(data$VSSTRESN)
            }
            if ("AESEQ" %in% names(data)) {
              data$AESEQ <- as.numeric(data$AESEQ)
            }
            if ("VSSEQ" %in% names(data)) {
              data$VSSEQ <- as.numeric(data$VSSEQ)
            }
            
            removeNotification(show_id)
            showNotification(paste("Demo data loaded:", nrow(data), "rows and", ncol(data), "columns"), 
                            type = "default", duration = 3)
            return(data)
          } else {
            stop("Demo dataset not found: ", domain)
          }
        } else {
          # Regular file reading
          data <- read_sdtm_adam(filepath(), validate_data = FALSE)
          removeNotification(show_id)
          showNotification(paste("Successfully loaded", nrow(data), "rows and", ncol(data), "columns"), 
                          type = "default", duration = 3)
          return(data)
        }
      }, error = function(e) {
        removeNotification(show_id)
        showNotification(paste("Error loading file:", e$message), type = "error", duration = 10)
        return(NULL)
      })
      
      result
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
      if (is.null(dataset())) {
        return(NULL)
      }
      
      data_to_show <- dataset()
      
      # Validate data
      if (nrow(data_to_show) == 0) {
        showNotification("Dataset is empty", type = "warning")
        return(NULL)
      }
      
      # Limit rows
      if (nrow(data_to_show) > input$max_rows) {
        data_to_show <- head(data_to_show, input$max_rows)
      }
      
      # Limit columns if not showing all
      if (!input$show_all_cols && ncol(data_to_show) > 10) {
        data_to_show <- data_to_show[, 1:10]
      }
      
      tryCatch({
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
          ),
          defaultColDef = colDef(minWidth = 100)
        )
      }, error = function(e) {
        showNotification(paste("Error creating table:", e$message), type = "error")
        return(NULL)
      })
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

    # Reproducible R code mirroring the current preview state
    # (data load + the same column / row limits applied to the displayed table)
    preview_code <- reactive({
      req(dataset())
      data <- dataset()

      # Load line: demo datasets vs. real files
      if (startsWith(filepath(), "DEMO:")) {
        domain <- sub("DEMO:", "", filepath())
        load_expr <- paste0("data <- get_demo_dataset(", r_literal(domain), ")")
      } else {
        load_expr <- build_load_expr(filepath())
      }

      # Column selection: matches the "Show All Columns" behaviour
      cols <- NULL
      if (!isTRUE(input$show_all_cols) && ncol(data) > 10) {
        cols <- names(data)[1:10]
      }
      transform <- build_dplyr_expr("data", filters = NULL, cols = cols)

      # Row cap: matches "Max Rows to Display"
      n <- input$max_rows
      if (!is.null(n) && is.finite(n) && nrow(data) > n) {
        head_step <- paste0("utils::head(", format(n, scientific = FALSE), ")")
        transform <- if (identical(transform, "data")) {
          paste0("data |>\n  ", head_step)
        } else {
          paste0(transform, " |>\n  ", head_step)
        }
      }

      build_full_script(load_expr, transform_expr = transform)
    })

    output$r_code <- renderText({
      preview_code()
    })

    output$download_script <- downloadHandler(
      filename = function() {
        paste0("databoxR_script_", Sys.Date(), ".R")
      },
      content = function(file) {
        writeLines(preview_code(), file)
      }
    )
    
    # Track if dataset is loaded for UI conditionals
    output$dataset_loaded <- reactive({
      !is.null(dataset())
    })
    outputOptions(output, "dataset_loaded", suspendWhenHidden = FALSE)
    
    # Return dataset for other modules
    return(dataset)
  })
}
