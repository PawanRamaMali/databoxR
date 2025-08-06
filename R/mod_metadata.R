mod_metadata_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    bs4Dash::box(
      title = "Metadata Explorer",
      status = "info", 
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      tabsetPanel(
        id = ns("metadata_tabs"),
        tabPanel(
          title = "Variable Labels",
          value = "labels",
          br(),
          DT::dataTableOutput(ns("variable_labels"))
        ),
        tabPanel(
          title = "Data Dictionary", 
          value = "dictionary",
          br(),
          fluidRow(
            column(6, selectInput(ns("dict_variable"), "Select Variable:", choices = NULL)),
            column(6, selectInput(ns("dict_type"), "Analysis Type:", 
                                choices = c("Summary" = "summary", "Frequency" = "frequency")))
          ),
          verbatimTextOutput(ns("variable_analysis")),
          plotOutput(ns("variable_plot"))
        ),
        tabPanel(
          title = "CDISC Compliance",
          value = "cdisc",
          br(),
          h5("CDISC Standard Checks"),
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
    }, options = list(pageLength = 20, scrollX = TRUE))
    
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
      
      if (is.numeric(var_data)) {
        hist(var_data, main = paste("Distribution of", input$dict_variable),
             xlab = input$dict_variable, col = "lightblue", border = "white")
      } else {
        # For categorical variables
        freq_table <- table(var_data)
        if (length(freq_table) <= 20) {  # Only plot if not too many categories
          barplot(freq_table, main = paste("Frequency of", input$dict_variable),
                  xlab = input$dict_variable, ylab = "Count", 
                  col = "lightgreen", las = 2)
        }
      }
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
    }, options = list(pageLength = 10))
  })
}