mod_eda_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    bs4Dash::box(
      title = "Exploratory Data Analysis",
      status = "success",
      solidHeader = TRUE, 
      width = 12,
      collapsible = TRUE,
      tabsetPanel(
        id = ns("eda_tabs"),
        tabPanel(
          title = "Overview",
          value = "overview",
          br(),
          fluidRow(
            column(6,
              bs4Dash::valueBoxOutput(ns("total_rows"), width = 12),
              bs4Dash::valueBoxOutput(ns("total_cols"), width = 12)
            ),
            column(6,
              bs4Dash::valueBoxOutput(ns("missing_data"), width = 12),
              bs4Dash::valueBoxOutput(ns("numeric_vars"), width = 12)
            )
          ),
          br(),
          h5("Data Quality Summary"),
          verbatimTextOutput(ns("data_quality"))
        ),
        tabPanel(
          title = "Distributions",
          value = "distributions", 
          br(),
          fluidRow(
            column(4, selectInput(ns("dist_variable"), "Select Variable:", choices = NULL)),
            column(4, selectInput(ns("plot_type"), "Plot Type:", 
                                choices = c("Histogram" = "hist", "Boxplot" = "box", "Density" = "density"))),
            column(4, checkboxInput(ns("log_scale"), "Log Scale", value = FALSE))
          ),
          plotOutput(ns("distribution_plot"), height = "400px")
        ),
        tabPanel(
          title = "Correlations",
          value = "correlations",
          br(),
          fluidRow(
            column(6, 
              h5("Correlation Matrix"),
              plotOutput(ns("correlation_plot"))
            ),
            column(6,
              h5("Top Correlations"),
              DT::dataTableOutput(ns("correlation_table"))
            )
          )
        ),
        tabPanel(
          title = "Missing Data",
          value = "missing",
          br(),
          fluidRow(
            column(6, plotOutput(ns("missing_pattern"))),
            column(6, DT::dataTableOutput(ns("missing_summary")))
          )
        )
      )
    )
  )
}

mod_eda_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    # Value boxes for overview
    output$total_rows <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = ifelse(is.null(dataset()), 0, nrow(dataset())),
        subtitle = "Total Rows",
        icon = icon("table"),
        color = "primary"
      )
    })
    
    output$total_cols <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = ifelse(is.null(dataset()), 0, ncol(dataset())),
        subtitle = "Total Columns", 
        icon = icon("columns"),
        color = "info"
      )
    })
    
    output$missing_data <- bs4Dash::renderValueBox({
      missing_pct <- if (is.null(dataset())) {
        0
      } else {
        round(sum(is.na(dataset())) / (nrow(dataset()) * ncol(dataset())) * 100, 1)
      }
      bs4Dash::valueBox(
        value = paste0(missing_pct, "%"),
        subtitle = "Missing Data",
        icon = icon("exclamation-triangle"),
        color = if (missing_pct > 10) "warning" else "success"
      )
    })
    
    output$numeric_vars <- bs4Dash::renderValueBox({
      numeric_count <- if (is.null(dataset())) {
        0
      } else {
        sum(sapply(dataset(), is.numeric))
      }
      bs4Dash::valueBox(
        value = numeric_count,
        subtitle = "Numeric Variables",
        icon = icon("calculator"), 
        color = "secondary"
      )
    })
    
    # Data quality summary
    output$data_quality <- renderText({
      req(dataset())
      
      # Calculate various quality metrics
      data <- dataset()
      total_cells <- nrow(data) * ncol(data)
      missing_cells <- sum(is.na(data))
      
      # Check for duplicates
      duplicate_rows <- sum(duplicated(data))
      
      # Check for constant columns
      constant_cols <- sum(sapply(data, function(x) length(unique(x[!is.na(x)])) <= 1))
      
      paste(
        "Data Quality Assessment:",
        paste("Total cells:", total_cells),
        paste("Missing cells:", missing_cells, paste0("(", round(missing_cells/total_cells*100, 2), "%)")),
        paste("Duplicate rows:", duplicate_rows),
        paste("Constant columns:", constant_cols),
        "",
        "Recommendations:",
        if (missing_cells/total_cells > 0.1) "• High missing data - consider imputation strategies" else "• Missing data levels acceptable",
        if (duplicate_rows > 0) "• Remove duplicate rows for analysis" else "• No duplicate rows found",
        if (constant_cols > 0) "• Consider removing constant columns" else "• No constant columns detected",
        sep = "\n"
      )
    })
    
    # Update variable choices for distributions
    observe({
      req(dataset())
      numeric_vars <- names(dataset())[sapply(dataset(), is.numeric)]
      updateSelectInput(session, "dist_variable", choices = numeric_vars)
    })
    
    # Distribution plots
    output$distribution_plot <- renderPlot({
      req(input$dist_variable, dataset())
      
      var_data <- dataset()[[input$dist_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      if (input$log_scale && min(var_data) > 0) {
        var_data <- log10(var_data)
        x_label <- paste("log10(", input$dist_variable, ")")
      } else {
        x_label <- input$dist_variable
      }
      
      if (input$plot_type == "hist") {
        hist(var_data, main = paste("Distribution of", input$dist_variable),
             xlab = x_label, col = "lightblue", border = "white", breaks = 30)
      } else if (input$plot_type == "box") {
        boxplot(var_data, main = paste("Boxplot of", input$dist_variable),
                ylab = x_label, col = "lightgreen")
      } else if (input$plot_type == "density") {
        plot(density(var_data), main = paste("Density of", input$dist_variable),
             xlab = x_label, col = "darkblue", lwd = 2)
        polygon(density(var_data), col = "lightblue", border = "darkblue")
      }
    })
    
    # Correlation analysis
    output$correlation_plot <- renderPlot({
      req(dataset())
      
      numeric_data <- dataset()[sapply(dataset(), is.numeric)]
      if (ncol(numeric_data) >= 2) {
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        
        # Simple correlation heatmap
        image(1:ncol(cor_matrix), 1:nrow(cor_matrix), as.matrix(cor_matrix),
              col = colorRampPalette(c("blue", "white", "red"))(50),
              xlab = "", ylab = "", axes = FALSE, main = "Correlation Matrix")
        axis(1, at = 1:ncol(cor_matrix), labels = colnames(cor_matrix), las = 2)
        axis(2, at = 1:nrow(cor_matrix), labels = rownames(cor_matrix), las = 2)
      }
    })
    
    # Correlation table
    output$correlation_table <- DT::renderDataTable({
      req(dataset())
      
      numeric_data <- dataset()[sapply(dataset(), is.numeric)]
      if (ncol(numeric_data) >= 2) {
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        
        # Extract upper triangle correlations
        cor_pairs <- data.frame()
        for (i in 1:(ncol(cor_matrix)-1)) {
          for (j in (i+1):ncol(cor_matrix)) {
            cor_pairs <- rbind(cor_pairs, data.frame(
              Variable1 = colnames(cor_matrix)[i],
              Variable2 = colnames(cor_matrix)[j], 
              Correlation = round(cor_matrix[i, j], 3)
            ))
          }
        }
        
        # Sort by absolute correlation
        cor_pairs[order(abs(cor_pairs$Correlation), decreasing = TRUE), ]
      }
    }, options = list(pageLength = 10))
    
    # Missing data pattern
    output$missing_pattern <- renderPlot({
      req(dataset())
      
      missing_counts <- sapply(dataset(), function(x) sum(is.na(x)))
      missing_pct <- missing_counts / nrow(dataset()) * 100
      
      barplot(missing_pct, main = "Missing Data by Variable",
              xlab = "Variables", ylab = "Missing %", 
              col = "coral", las = 2)
      abline(h = 5, col = "red", lty = 2)  # 5% threshold line
    })
    
    # Missing data summary table  
    output$missing_summary <- DT::renderDataTable({
      req(dataset())
      
      missing_info <- data.frame(
        Variable = names(dataset()),
        Missing_Count = sapply(dataset(), function(x) sum(is.na(x))),
        Missing_Percent = round(sapply(dataset(), function(x) sum(is.na(x))/length(x) * 100), 2),
        stringsAsFactors = FALSE
      )
      
      missing_info[order(missing_info$Missing_Percent, decreasing = TRUE), ]
    }, options = list(pageLength = 15))
  })
}