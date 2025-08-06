mod_eda_ui <- function(id) {
  ns <- NS(id)
  bs4Dash::box(
    title = "Exploratory Data Analysis",
    status = "success",
    solidHeader = TRUE, 
    width = 12,
    collapsible = TRUE,
    tabsetPanel(
      id = ns("eda_tabs"),
      type = "pills",
      tabPanel(
        title = "Overview",
        value = "overview",
        br(),
        fluidRow(
          column(6, bs4Dash::valueBoxOutput(ns("total_rows"), width = 12)),
          column(6, bs4Dash::valueBoxOutput(ns("missing_data"), width = 12))
        ),
        br(),
        h6("Data Quality Assessment", class = "text-muted"),
        div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 0.375rem; height: 250px; overflow-y: auto;",
          verbatimTextOutput(ns("data_quality"))
        )
      ),
      tabPanel(
        title = "Distributions",
        value = "distributions", 
        br(),
        fluidRow(
          column(6,
            selectInput(ns("dist_variable"), "Select Variable:", choices = NULL, width = "100%")
          ),
          column(6,
            selectInput(ns("plot_type"), "Plot Type:", 
                       choices = c("Histogram" = "hist", "Boxplot" = "box"), width = "100%")
          )
        ),
        hr(),
        h6("Variable Distribution", class = "text-muted"),
        div(style = "border: 1px solid #dee2e6; border-radius: 0.375rem;",
          plotOutput(ns("distribution_plot"), height = "300px")
        )
      ),
      tabPanel(
        title = "Missing Data",
        value = "missing",
        br(),
        fluidRow(
          column(12,
            h6("Missing Data Pattern", class = "text-muted"),
            div(style = "border: 1px solid #dee2e6; border-radius: 0.375rem; margin-bottom: 15px;",
              plotOutput(ns("missing_pattern"), height = "200px")
            )
          )
        ),
        h6("Missing Data Summary", class = "text-muted"),
        div(style = "height: 200px; overflow: auto; border: 1px solid #dee2e6; border-radius: 0.375rem;",
          DT::dataTableOutput(ns("missing_summary"))
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
      
      if (length(numeric_vars) == 0) {
        updateSelectInput(session, "dist_variable", 
                         choices = list("No numeric variables found" = ""))
        showNotification("No numeric variables found for distribution analysis", 
                        type = "warning", duration = 5)
      } else {
        updateSelectInput(session, "dist_variable", choices = numeric_vars)
      }
    })
    
    # Distribution plots
    output$distribution_plot <- renderPlot({
      req(input$dist_variable, dataset())
      
      # Validate variable selection
      if (input$dist_variable == "" || input$dist_variable == "No numeric variables found") {
        plot.new()
        text(0.5, 0.5, "No numeric variables available", cex = 1.2, col = "gray")
        return()
      }
      
      var_data <- dataset()[[input$dist_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      # Check if there's enough data
      if (length(var_data) < 2) {
        plot.new()
        text(0.5, 0.5, "Insufficient data for plotting", cex = 1.2, col = "gray")
        return()
      }
      
      # Check for constant values
      if (length(unique(var_data)) == 1) {
        plot.new()
        text(0.5, 0.5, paste("Constant value:", unique(var_data)[1]), cex = 1.2, col = "gray")
        return()
      }
      
      x_label <- input$dist_variable
      
      tryCatch({
        if (input$plot_type == "hist") {
          hist(var_data, main = paste("Distribution of", input$dist_variable),
               xlab = x_label, col = "lightblue", border = "white", 
               breaks = min(30, length(unique(var_data))))
        } else if (input$plot_type == "box") {
          boxplot(var_data, main = paste("Boxplot of", input$dist_variable),
                  ylab = x_label, col = "lightgreen")
        }
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error creating plot:", e$message), cex = 1, col = "red")
      })
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