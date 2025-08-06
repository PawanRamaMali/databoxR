mod_metadata_ui <- function(id) {
  ns <- NS(id)
  bs4Dash::box(
    title = "Metadata Explorer",
    status = "info", 
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    tabsetPanel(
      id = ns("metadata_tabs"),
      type = "pills",
      tabPanel(
        title = "Variables",
        value = "labels",
        br(),
        h6("Variable Information", class = "text-muted"),
        div(style = "height: 400px; overflow: auto; border: 1px solid #dee2e6; border-radius: 0.375rem;",
          DT::dataTableOutput(ns("variable_labels"))
        )
      ),
      tabPanel(
        title = "Dictionary", 
        value = "dictionary",
        br(),
        fluidRow(
          column(6,
            selectInput(ns("dict_variable"), "Select Variable:", 
                       choices = NULL, width = "100%")
          ),
          column(6,
            selectInput(ns("dict_type"), "Analysis Type:", 
                       choices = c("Summary" = "summary", "Frequency" = "frequency"),
                       width = "100%")
          )
        ),
        hr(),
        fluidRow(
          column(6,
            h6("Analysis Results", class = "text-muted"),
            div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 0.375rem; height: 200px; overflow-y: auto;",
              verbatimTextOutput(ns("variable_analysis"))
            )
          ),
          column(6,
            h6("Visual Distribution", class = "text-muted"),
            div(style = "border: 1px solid #dee2e6; border-radius: 0.375rem;",
              plotOutput(ns("variable_plot"), height = "200px")
            )
          )
        )
      ),
      tabPanel(
        title = "CDISC Checks",
        value = "cdisc",
        br(),
        h6("Compliance Validation", class = "text-muted"),
        div(style = "height: 400px; overflow: auto; border: 1px solid #dee2e6; border-radius: 0.375rem;",
          DT::dataTableOutput(ns("cdisc_checks"))
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
    
    # Variable plot
    output$variable_plot <- renderPlot({
      req(input$dict_variable, dataset())
      var_data <- dataset()[[input$dict_variable]]
      
      # Remove NA values for plotting
      var_data_clean <- var_data[!is.na(var_data)]
      
      # Check if there's any data to plot
      if (length(var_data_clean) == 0) {
        plot.new()
        text(0.5, 0.5, "No non-missing data available for plotting", cex = 1.2, col = "gray")
        return()
      }
      
      # Check if all values are the same
      if (length(unique(var_data_clean)) == 1) {
        plot.new()
        text(0.5, 0.5, paste("Constant value:", unique(var_data_clean)[1]), cex = 1.2, col = "gray")
        return()
      }
      
      tryCatch({
        if (is.numeric(var_data)) {
          hist(var_data_clean, main = paste("Distribution of", input$dict_variable),
               xlab = input$dict_variable, col = "lightblue", border = "white",
               breaks = min(20, length(unique(var_data_clean))))
        } else {
          # For categorical variables
          freq_table <- table(var_data_clean)
          if (length(freq_table) <= 15) {  # Only plot if not too many categories
            par(mar = c(8, 4, 4, 2))  # Increase bottom margin for labels
            barplot(freq_table, main = paste("Frequency of", input$dict_variable),
                    xlab = "", ylab = "Count", 
                    col = "lightgreen", las = 2, cex.names = 0.8)
          } else {
            plot.new()
            text(0.5, 0.5, paste("Too many categories (", length(freq_table), ") to display"), 
                 cex = 1.1, col = "gray")
          }
        }
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error creating plot:", e$message), cex = 1, col = "red")
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