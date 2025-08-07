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
      selected_file = NULL,
      demo_datasets = NULL,
      is_demo = FALSE
    )
    
    # Enhanced demo data loading with synthetic data generation
    if (demo_data) {
      observe({
        tryCatch({
          # Generate synthetic demo datasets
          demo_datasets <- generate_demo_data(n_subjects = 150)
          
          # Store datasets in reactive values for direct access
          files_data$demo_datasets <- demo_datasets
          files_data$paths <- names(demo_datasets)  # Use domain names as "paths"
          files_data$names <- paste0("Demo ", names(demo_datasets), " Dataset")
          files_data$is_demo <- TRUE  # Flag to indicate these are demo datasets
          
          showNotification(
            paste("Demo data generated:", length(demo_datasets), "synthetic datasets (DM, AE, VS)"),
            type = "message", duration = 4
          )
          
        }, error = function(e) {
          showNotification(
            paste("Failed to generate demo data:", e$message),
            type = "error", duration = 8
          )
        })
      })
    }
    
    # Enhanced file upload handling with validation
    observeEvent(input$folder, {
      req(input$folder)
      
      # Validate file extensions
      valid_extensions <- c("xpt", "csv", "txt")
      file_extensions <- tolower(tools::file_ext(input$folder$name))
      valid_files <- file_extensions %in% valid_extensions
      
      if (!all(valid_files)) {
        invalid_files <- input$folder$name[!valid_files]
        showNotification(
          paste("Skipping unsupported files:", paste(invalid_files, collapse = ", ")),
          type = "warning", duration = 8
        )
      }
      
      if (any(valid_files)) {
        # Only keep valid files
        files_data$paths <- input$folder$datapath[valid_files]
        files_data$names <- input$folder$name[valid_files]
        
        # Check for very large files
        file_sizes <- file.size(files_data$paths)
        large_files <- file_sizes > 100 * 1024^2  # 100MB
        
        if (any(large_files)) {
          large_file_names <- files_data$names[large_files]
          showNotification(
            paste("Large files detected:", paste(large_file_names, collapse = ", "), 
                  "- loading may take longer"),
            type = "message", duration = 10
          )
        }
        
        showNotification(
          paste("Successfully uploaded", length(files_data$names), "file(s)"),
          type = "message", duration = 3
        )
      } else {
        showNotification(
          "No valid files found. Supported formats: .xpt, .csv, .txt",
          type = "error", duration = 8
        )
      }
    })
    
    # Enhanced file list table with validation
    output$file_list <- DT::renderDataTable({
      req(files_data$names)
      
      # Validate file paths exist and are readable
      valid_files <- sapply(files_data$paths, function(path) {
        file.exists(path) && file.access(path, mode = 4) == 0
      })
      
      if (!all(valid_files)) {
        invalid_count <- sum(!valid_files)
        showNotification(
          paste("Warning:", invalid_count, "file(s) are not accessible and will be skipped"),
          type = "warning", duration = 5
        )
      }
      
      # Get file information with error handling
      file_info <- sapply(files_data$paths, function(path) {
        if (file.exists(path)) {
          tryCatch({
            size_bytes <- file.size(path)
            list(
              size = format_file_size(size_bytes),
              size_bytes = size_bytes,
              readable = file.access(path, mode = 4) == 0,
              modified = file.info(path)$mtime
            )
          }, error = function(e) {
            list(size = "Error", size_bytes = 0, readable = FALSE, modified = NA)
          })
        } else {
          list(size = "Not Found", size_bytes = 0, readable = FALSE, modified = NA)
        }
      }, simplify = FALSE)
      
      # Create enhanced file table
      file_df <- data.frame(
        File = files_data$names,
        Size = sapply(file_info, function(x) x$size),
        Type = tools::file_ext(files_data$names),
        Status = ifelse(sapply(file_info, function(x) x$readable), 
                       "✓ Ready", "⚠ Issue"),
        Modified = sapply(file_info, function(x) {
          if (is.na(x$modified)) {
            "Unknown"
          } else {
            format(x$modified, "%Y-%m-%d %H:%M")
          }
        }),
        stringsAsFactors = FALSE
      )
      
      # Sort by file size (largest first) for better UX
      size_order <- order(sapply(file_info, function(x) x$size_bytes), decreasing = TRUE)
      file_df[size_order, ]
    }, options = list(
      pageLength = 10, 
      dom = 't',
      columnDefs = list(
        list(targets = c(1, 3), className = "dt-center"),  # Center align size and status
        list(targets = 4, className = "dt-right")  # Right align modified date
      )
    ), selection = 'single')
    
    # Track if files are uploaded
    output$files_uploaded <- reactive({
      !is.null(files_data$paths)
    })
    outputOptions(output, "files_uploaded", suspendWhenHidden = FALSE)
    
    
    # Return both filepath and demo datasets
    return(list(
      filepath = reactive({
        # Handle demo data differently
        if (!is.null(files_data$is_demo) && files_data$is_demo) {
          if (!is.null(input$file_list_rows_selected) && length(files_data$paths) > 0) {
            selected_idx <- input$file_list_rows_selected
            if (selected_idx > 0 && selected_idx <= length(files_data$paths)) {
              # Return a special identifier for demo data
              return(paste0("DEMO:", files_data$paths[selected_idx]))
            }
          } else if (!is.null(files_data$paths) && length(files_data$paths) > 0) {
            # Default to first demo dataset
            return(paste0("DEMO:", files_data$paths[1]))
          }
          return(NULL)
        }
        
        # Handle regular file uploads
        if (!is.null(input$file_list_rows_selected) && length(files_data$paths) > 0) {
          selected_idx <- input$file_list_rows_selected
          
          # Validate selection index
          if (selected_idx > 0 && selected_idx <= length(files_data$paths)) {
            selected_path <- files_data$paths[selected_idx]
            
            # Final validation before returning
            if (file.exists(selected_path) && file.access(selected_path, mode = 4) == 0) {
              return(selected_path)
            } else {
              showNotification(
                paste("Selected file is not accessible:", basename(selected_path)),
                type = "error", duration = 5
              )
              return(NULL)
            }
          }
        } else if (!is.null(files_data$paths) && length(files_data$paths) > 0) {
          # Default to first valid file
          for (path in files_data$paths) {
            if (file.exists(path) && file.access(path, mode = 4) == 0) {
              return(path)
            }
          }
          showNotification(
            "No accessible files found in the uploaded set",
            type = "warning", duration = 5
          )
          return(NULL)
        } else {
          return(NULL)
        }
      }),
      demo_datasets = reactive(files_data$demo_datasets)
    ))
  })
}
