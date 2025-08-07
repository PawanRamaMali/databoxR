mod_metadata_ui <- function(id) {
  ns <- NS(id)
  bs4Dash::box(
    title = tags$div(
      icon("search", class = "fa-fw"),
      "Metadata Explorer",
      style = "color: #007bff;"
    ),
    status = "primary", 
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    headerBorder = FALSE,
    tabsetPanel(
      id = ns("metadata_tabs"),
      type = "pills",
      tabPanel(
        title = tags$span(
          style = "display: flex; align-items: center; gap: 10px; padding: 8px 16px; margin-right: 12px; background: linear-gradient(135deg, #f8f9fa, #e9ecef); border-radius: 8px; border: 1px solid #dee2e6; font-weight: 500;",
          icon("table", class = "fa-sm"),
          "Variables"
        ),
        value = "labels",
        br(),
        div(class = "card border-0 shadow-sm",
          div(class = "card-header bg-gradient-primary text-white",
            h5(class = "card-title mb-0",
              icon("list-ul", class = "fa-fw me-2"),
              "Variable Dictionary"
            )
          ),
          div(class = "card-body p-0",
            div(style = "height: 450px; overflow: auto;",
              shinycssloaders::withSpinner(
                DT::dataTableOutput(ns("variable_labels")),
                type = 6, color = "#007bff"
              )
            )
          )
        )
      ),
      tabPanel(
        title = tags$span(
          style = "display: flex; align-items: center; gap: 10px; padding: 8px 16px; margin-right: 12px; background: linear-gradient(135deg, #f8f9fa, #e9ecef); border-radius: 8px; border: 1px solid #dee2e6; font-weight: 500;",
          icon("chart-bar", class = "fa-sm"),
          "Analysis"
        ),
        value = "dictionary",
        br(),
        div(class = "row g-3 mb-4",
          div(class = "col-md-6",
            div(class = "card border-0 shadow-sm h-100",
              div(class = "card-body",
                h6(class = "card-title text-primary",
                  icon("filter", class = "fa-fw me-2"),
                  "Variable Selection"
                ),
                selectInput(ns("dict_variable"), 
                           label = NULL,
                           choices = NULL, 
                           width = "100%")
              )
            )
          ),
          div(class = "col-md-6",
            div(class = "card border-0 shadow-sm h-100",
              div(class = "card-body",
                h6(class = "card-title text-primary",
                  icon("cogs", class = "fa-fw me-2"),
                  "Analysis Type"
                ),
                selectInput(ns("dict_type"), 
                           label = NULL,
                           choices = c(
                             "📊 Summary Statistics" = "summary", 
                             "📈 Frequency Analysis" = "frequency",
                             "🔍 Missing Data Profile" = "missing"
                           ),
                           width = "100%")
              )
            )
          )
        ),
        div(class = "row g-3",
          div(class = "col-lg-6",
            div(class = "card border-0 shadow-sm h-100",
              div(class = "card-header bg-gradient-success text-white",
                h6(class = "card-title mb-0",
                  icon("calculator", class = "fa-fw me-2"),
                  "Statistical Summary"
                )
              ),
              div(class = "card-body",
                div(style = "height: 280px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 0.9em;",
                  shinycssloaders::withSpinner(
                    verbatimTextOutput(ns("variable_analysis")),
                    type = 4, color = "#28a745"
                  )
                )
              )
            )
          ),
          div(class = "col-lg-6",
            div(class = "card border-0 shadow-sm h-100",
              div(class = "card-header bg-gradient-info text-white",
                h6(class = "card-title mb-0",
                  icon("chart-area", class = "fa-fw me-2"),
                  "Data Visualization"
                )
              ),
              div(class = "card-body d-flex align-items-center justify-content-center",
                shinycssloaders::withSpinner(
                  plotlyOutput(ns("variable_plot"), height = "280px"),
                  type = 5, color = "#17a2b8"
                )
              )
            )
          )
        )
      ),
      tabPanel(
        title = tags$span(
          style = "display: flex; align-items: center; gap: 10px; padding: 8px 16px; margin-right: 12px; background: linear-gradient(135deg, #f8f9fa, #e9ecef); border-radius: 8px; border: 1px solid #dee2e6; font-weight: 500;",
          icon("check-circle", class = "fa-sm"),
          "CDISC"
        ),
        value = "cdisc",
        br(),
        div(class = "card border-0 shadow-sm",
          div(class = "card-header bg-gradient-warning text-dark",
            h5(class = "card-title mb-0",
              icon("shield-alt", class = "fa-fw me-2"),
              "CDISC Compliance Validation"
            ),
            p(class = "card-text small mb-0 mt-2", 
              "Automated checks for CDISC standards compliance and data quality")
          ),
          div(class = "card-body p-0",
            div(style = "height: 450px; overflow: auto;",
              shinycssloaders::withSpinner(
                DT::dataTableOutput(ns("cdisc_checks")),
                type = 7, color = "#ffc107"
              )
            )
          )
        )
      )
    )
  )
}

mod_metadata_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    # Variable labels table
    output$variable_labels <- DT::renderDataTable({
      req(dataset())
      
      # Extract variable labels if they exist
      labels <- sapply(dataset(), function(x) {
        label <- attr(x, "label")
        if (is.null(label)) "" else as.character(label)
      })
      
      data.frame(
        Variable = names(dataset()),
        Label = labels,
        Type = sapply(dataset(), class),
        stringsAsFactors = FALSE
      )
    }, options = list(
      pageLength = 10,
      scrollX = TRUE,
      scrollY = "300px",
      dom = 'frtip',
      autoWidth = TRUE,
      columnDefs = list(list(width = "200px", targets = "_all"))
    ))
    
    # Update variable choices for dictionary
    observe({
      req(dataset())
      updateSelectInput(session, "dict_variable", choices = names(dataset()))
    })
    
    # Variable analysis
    output$variable_analysis <- renderText({
      req(input$dict_variable, dataset())
      var_data <- dataset()[[input$dict_variable]]
      
      if (input$dict_type == "summary") {
        if (is.numeric(var_data)) {
          paste(
            "Summary Statistics:",
            paste("Min:", min(var_data, na.rm = TRUE)),
            paste("Max:", max(var_data, na.rm = TRUE)),
            paste("Mean:", round(mean(var_data, na.rm = TRUE), 3)),
            paste("Median:", median(var_data, na.rm = TRUE)),
            paste("Missing:", sum(is.na(var_data))),
            sep = "\n"
          )
        } else {
          paste(
            "Character/Factor Summary:",
            paste("Unique values:", length(unique(var_data))),
            paste("Missing:", sum(is.na(var_data))),
            paste("Most common:", names(sort(table(var_data), decreasing = TRUE))[1]),
            sep = "\n"
          )
        }
      } else {
        freq_table <- table(var_data, useNA = "ifany")
        paste(capture.output(print(freq_table)), collapse = "\n")
      }
    })
    
    # Variable plot with plotly (no margin issues)
    output$variable_plot <- renderPlotly({
      # Check if we have the required inputs
      if (is.null(input$dict_variable) || input$dict_variable == "" || is.null(dataset())) {
        return(plot_ly() %>%
          layout(
            title = "Please select a variable to visualize",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          ))
      }
      
      # Check if variable exists in dataset
      if (!input$dict_variable %in% names(dataset())) {
        return(plot_ly() %>%
          layout(
            title = paste("Variable", input$dict_variable, "not found"),
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          ))
      }
      
      var_data <- dataset()[[input$dict_variable]]
      
      # Check if variable has any data
      if (is.null(var_data) || length(var_data) == 0) {
        return(plot_ly() %>%
          layout(
            title = "No data available for selected variable",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          ))
      }
      
      # Remove NA values for plotting
      var_data_clean <- var_data[!is.na(var_data)]
      
      # Check if there's any data to plot after removing NAs
      if (length(var_data_clean) == 0) {
        return(plot_ly() %>%
          layout(
            title = "All values are missing for this variable",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          ))
      }
      
      # Check if all values are the same
      if (length(unique(var_data_clean)) == 1) {
        return(plot_ly() %>%
          layout(
            title = paste("Constant value:", unique(var_data_clean)[1]),
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          ))
      }
      
      # Create the plot
      tryCatch({
        if (is.numeric(var_data_clean)) {
          # Numeric variable - histogram
          mean_val <- mean(var_data_clean)
          median_val <- median(var_data_clean)
          
          plot_ly(x = ~var_data_clean, type = "histogram", 
                  marker = list(color = "lightblue", line = list(color = "white", width = 1))) %>%
            layout(
              title = paste("Distribution of", input$dict_variable),
              xaxis = list(title = input$dict_variable),
              yaxis = list(title = "Frequency"),
              shapes = list(
                list(type = "line", x0 = mean_val, x1 = mean_val, 
                     y0 = 0, y1 = 1, yref = "paper",
                     line = list(color = "red", width = 2, dash = "dash")),
                list(type = "line", x0 = median_val, x1 = median_val,
                     y0 = 0, y1 = 1, yref = "paper", 
                     line = list(color = "blue", width = 2, dash = "dash"))
              ),
              annotations = list(
                list(x = mean_val, y = 1, yref = "paper", text = paste("Mean:", round(mean_val, 2)),
                     showarrow = TRUE, arrowcolor = "red", arrowsize = 0.5),
                list(x = median_val, y = 0.9, yref = "paper", text = paste("Median:", round(median_val, 2)),
                     showarrow = TRUE, arrowcolor = "blue", arrowsize = 0.5)
              )
            )
                 
        } else {
          # Categorical variable - bar plot
          freq_table <- table(var_data_clean)
          n_categories <- length(freq_table)
          
          if (n_categories <= 20) {
            df <- data.frame(
              categories = names(freq_table),
              counts = as.numeric(freq_table)
            )
            
            plot_ly(df, x = ~categories, y = ~counts, type = "bar",
                    marker = list(color = "lightgreen", line = list(color = "white", width = 1))) %>%
              layout(
                title = paste("Frequency of", input$dict_variable),
                xaxis = list(title = "", tickangle = -45),
                yaxis = list(title = "Count")
              )
          } else {
            # Show top 10 categories
            top_categories <- sort(freq_table, decreasing = TRUE)[1:10]
            df <- data.frame(
              categories = names(top_categories),
              counts = as.numeric(top_categories)
            )
            
            plot_ly(df, x = ~categories, y = ~counts, type = "bar",
                    marker = list(color = "lightgreen", line = list(color = "white", width = 1))) %>%
              layout(
                title = paste("Top 10 categories of", input$dict_variable),
                xaxis = list(title = "", tickangle = -45),
                yaxis = list(title = "Count")
              )
          }
        }
      }, error = function(e) {
        plot_ly() %>%
          layout(
            title = paste("Error creating plot:", e$message),
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          )
      })
    })
    
    # CDISC compliance checks
    output$cdisc_checks <- DT::renderDataTable({
      req(dataset())
      
      # Basic CDISC checks
      checks <- list()
      var_names <- names(dataset())
      
      # Check for standard CDISC variables
      standard_vars <- c("STUDYID", "DOMAIN", "USUBJID", "SUBJID")
      missing_standard <- standard_vars[!standard_vars %in% var_names]
      
      if (length(missing_standard) > 0) {
        checks <- append(checks, list(
          data.frame(
            Check = "Missing Standard Variables",
            Status = "Warning",
            Details = paste(missing_standard, collapse = ", "),
            stringsAsFactors = FALSE
          )
        ))
      }
      
      # Check variable name patterns
      if (any(grepl("^[a-z]", var_names))) {
        lowercase_vars <- var_names[grepl("^[a-z]", var_names)]
        checks <- append(checks, list(
          data.frame(
            Check = "Lowercase Variable Names",
            Status = "Info", 
            Details = paste(head(lowercase_vars, 5), collapse = ", "),
            stringsAsFactors = FALSE
          )
        ))
      }
      
      # Combine all checks
      if (length(checks) == 0) {
        data.frame(
          Check = "All Basic Checks Passed",
          Status = "Success",
          Details = "No issues found",
          stringsAsFactors = FALSE
        )
      } else {
        do.call(rbind, checks)
      }
    }, options = list(
      pageLength = 8,
      scrollX = TRUE,
      scrollY = "300px",
      dom = 'frtip',
      autoWidth = TRUE
    ))
  })
}