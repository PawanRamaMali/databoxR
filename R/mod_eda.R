mod_eda_ui <- function(id) {
  ns <- NS(id)
  bs4Dash::box(
    title = tags$div(
      icon("chart-line", class = "fa-fw"),
      "Exploratory Data Analysis",
      style = "color: #28a745;"
    ),
    status = "success",
    solidHeader = TRUE, 
    width = 12,
    collapsible = TRUE,
    headerBorder = FALSE,
    tabsetPanel(
      id = ns("eda_tabs"),
      type = "pills",
      tabPanel(
        title = tags$span(
          style = "display: flex; align-items: center; gap: 10px; padding: 8px 16px; margin-right: 12px; background: linear-gradient(135deg, #f8f9fa, #e9ecef); border-radius: 8px; border: 1px solid #dee2e6; font-weight: 500;",
          icon("tachometer-alt", class = "fa-sm"),
          "Dashboard"
        ),
        value = "overview",
        br(),
        # Key Metrics Row
        div(class = "row g-3 mb-4",
          div(class = "col-lg-3 col-md-6",
            shinycssloaders::withSpinner(
              bs4Dash::valueBoxOutput(ns("total_rows"), width = 12),
              type = 8, color = "#007bff"
            )
          ),
          div(class = "col-lg-3 col-md-6",
            shinycssloaders::withSpinner(
              bs4Dash::valueBoxOutput(ns("missing_data"), width = 12),
              type = 8, color = "#dc3545"
            )
          ),
          div(class = "col-lg-3 col-md-6",
            shinycssloaders::withSpinner(
              bs4Dash::valueBoxOutput(ns("total_vars"), width = 12),
              type = 8, color = "#28a745"
            )
          ),
          div(class = "col-lg-3 col-md-6",
            shinycssloaders::withSpinner(
              bs4Dash::valueBoxOutput(ns("numeric_vars"), width = 12),
              type = 8, color = "#17a2b8"
            )
          )
        ),
        # Data Quality Assessment Card
        div(class = "card border-0 shadow-sm",
          div(class = "card-header bg-gradient-primary text-white d-flex align-items-center",
            icon("shield-alt", class = "fa-2x me-3"),
            div(
              h4(class = "card-title mb-0", "Data Quality Assessment"),
              p(class = "card-text mb-0 opacity-75", "Comprehensive analysis of data integrity and completeness")
            )
          ),
          div(class = "card-body",
            div(style = "height: 320px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 0.9em; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); padding: 20px; border-radius: 0.375rem;",
              shinycssloaders::withSpinner(
                verbatimTextOutput(ns("data_quality")),
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
          "Distributions"
        ),
        value = "distributions", 
        br(),
        # Control Panel
        div(class = "row g-3 mb-4",
          div(class = "col-md-6",
            div(class = "card border-0 shadow-sm",
              div(class = "card-body",
                h6(class = "card-title text-success",
                  icon("filter", class = "fa-fw me-2"),
                  "Variable Selection"
                ),
                selectInput(ns("dist_variable"), 
                           label = NULL,
                           choices = NULL, 
                           width = "100%")
              )
            )
          ),
          div(class = "col-md-6",
            div(class = "card border-0 shadow-sm",
              div(class = "card-body",
                h6(class = "card-title text-success",
                  icon("chart-area", class = "fa-fw me-2"),
                  "Visualization Type"
                ),
                selectInput(ns("plot_type"), 
                           label = NULL,
                           choices = c(
                             "📊 Histogram" = "hist", 
                             "📦 Box Plot" = "box",
                             "📈 Density Plot" = "density",
                             "🎯 Violin Plot" = "violin"
                           ), 
                           width = "100%")
              )
            )
          )
        ),
        # Visualization Card
        div(class = "card border-0 shadow-lg",
          div(class = "card-header bg-gradient-success text-white",
            h5(class = "card-title mb-0 d-flex align-items-center",
              icon("chart-line", class = "fa-fw me-2"),
              "Variable Distribution Analysis",
              tags$span(class = "badge bg-light text-dark ms-auto", "Interactive")
            )
          ),
          div(class = "card-body p-4",
            shinycssloaders::withSpinner(
              plotlyOutput(ns("distribution_plot"), height = "400px"),
              type = 5, color = "#28a745"
            )
          )
        )
      ),
      tabPanel(
        title = tags$span(
          style = "display: flex; align-items: center; gap: 10px; padding: 8px 16px; margin-right: 12px; background: linear-gradient(135deg, #f8f9fa, #e9ecef); border-radius: 8px; border: 1px solid #dee2e6; font-weight: 500;",
          icon("exclamation-triangle", class = "fa-sm"),
          "Missing Data"
        ),
        value = "missing",
        br(),
        # Missing Data Pattern Visualization
        div(class = "card border-0 shadow-sm mb-4",
          div(class = "card-header bg-gradient-warning text-dark",
            h5(class = "card-title mb-0 d-flex align-items-center",
              icon("puzzle-piece", class = "fa-fw me-2"),
              "Missing Data Pattern Analysis",
              tags$span(class = "badge bg-dark ms-auto", "Pattern View")
            ),
            p(class = "card-text small mb-0 mt-2 opacity-75",
              "Visual representation of missing data patterns across variables")
          ),
          div(class = "card-body",
            shinycssloaders::withSpinner(
              plotOutput(ns("missing_pattern"), height = "250px"),
              type = 7, color = "#ffc107"
            )
          )
        ),
        # Missing Data Summary Table
        div(class = "card border-0 shadow-sm",
          div(class = "card-header bg-gradient-danger text-white",
            h5(class = "card-title mb-0",
              icon("table", class = "fa-fw me-2"),
              "Missing Data Summary"
            ),
            p(class = "card-text small mb-0 mt-2 opacity-75",
              "Detailed breakdown of missing values by variable")
          ),
          div(class = "card-body p-0",
            div(style = "height: 300px; overflow: auto;",
              shinycssloaders::withSpinner(
                DT::dataTableOutput(ns("missing_summary")),
                type = 4, color = "#dc3545"
              )
            )
          )
        )
      )
    )
  )
}

mod_eda_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    # Cached dataset info for performance
    dataset_info <- reactive({
      req(dataset())
      get_dataset_info(dataset(), detailed = TRUE)
    })
    
    # Value boxes for overview with enhanced information
    output$total_rows <- bs4Dash::renderValueBox({
      info <- dataset_info()
      bs4Dash::valueBox(
        value = if (is.null(info)) 0 else formatC(info$nrows, format = "f", digits = 0, big.mark = ","),
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
      info <- dataset_info()
      missing_pct <- if (is.null(info)) 0 else info$missing_rate %||% 0
      
      bs4Dash::valueBox(
        value = paste0(missing_pct, "%"),
        subtitle = "Missing Data",
        icon = icon("exclamation-triangle"),
        color = if (missing_pct > 15) "danger" else if (missing_pct > 5) "warning" else "success"
      )
    })
    
    output$total_vars <- bs4Dash::renderValueBox({
      total_vars <- if (is.null(dataset())) 0 else ncol(dataset())
      bs4Dash::valueBox(
        value = total_vars,
        subtitle = "Total Variables",
        icon = icon("list"),
        color = "success"
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
    
    # Enhanced data quality summary with cached info
    output$data_quality <- renderText({
      req(dataset())
      info <- dataset_info()
      
      if (is.null(info)) return("No data available for quality assessment.")
      
      # Use cached values for better performance
      total_cells <- info$nrows * info$ncols
      missing_cells <- info$total_missing %||% 0
      duplicate_rows <- info$duplicate_rows %||% 0
      constant_cols <- length(info$constant_columns %||% character(0))
      missing_rate <- info$missing_rate %||% 0
      
      # Memory usage information
      memory_mb <- round(as.numeric(info$memory_size %||% 0) / 1024^2, 1)
      
      quality_lines <- c(
        "=== DATA QUALITY ASSESSMENT ===",
        "",
        "Dataset Overview:",
        paste("  • Total cells:", formatC(total_cells, format = "f", digits = 0, big.mark = ",")),
        paste("  • Missing cells:", formatC(missing_cells, format = "f", digits = 0, big.mark = ","), 
              paste0("(", missing_rate, "%)")),
        paste("  • Memory usage:", memory_mb, "MB"),
        "",
        "Data Quality Issues:",
        paste("  • Duplicate rows:", formatC(duplicate_rows, format = "f", digits = 0, big.mark = ",")),
        paste("  • Constant columns:", constant_cols),
        if (length(info$high_missing_vars) > 0) 
          paste("  • High missing vars:", length(info$high_missing_vars)) else NULL,
        "",
        "Data Types:",
        paste("  • Numeric:", info$numeric_count %||% 0),
        paste("  • Character:", info$character_count %||% 0),
        paste("  • Factor:", info$factor_count %||% 0),
        paste("  • Date:", info$date_count %||% 0),
        "",
        "=== RECOMMENDATIONS ===",
        if (missing_rate > 15) "⚠️ HIGH missing data - investigate patterns" else 
        if (missing_rate > 5) "⚠️ MODERATE missing data - consider imputation" else 
        "✅ Missing data levels acceptable",
        if (duplicate_rows > 0) "⚠️ Remove duplicate rows before analysis" else "✅ No duplicate rows found",
        if (constant_cols > 0) "⚠️ Consider removing constant columns" else "✅ No constant columns detected",
        if (memory_mb > 100) "💾 Large dataset - consider data sampling for exploration" else NULL
      )
      
      paste(quality_lines[!is.null(quality_lines)], collapse = "\n")
    })
    
    # Update variable choices for distributions with better performance
    observe({
      req(dataset())
      info <- dataset_info()
      
      # Get numeric variables from cached info
      data_sample <- dataset()
      numeric_vars <- names(data_sample)[sapply(data_sample, is.numeric)]
      
      if (length(numeric_vars) == 0) {
        updateSelectInput(session, "dist_variable", 
                         choices = list("No numeric variables found" = ""))
        showNotification("No numeric variables found for distribution analysis", 
                        type = "warning", duration = 5)
      } else {
        # Sort by number of unique values for better UX
        var_info <- sapply(numeric_vars, function(var) {
          unique_count <- length(unique(data_sample[[var]][!is.na(data_sample[[var]])]))
          list(name = var, unique_count = unique_count)
        }, simplify = FALSE)
        
        # Sort by unique count (more variation first)
        sorted_vars <- names(var_info)[order(sapply(var_info, function(x) x$unique_count), decreasing = TRUE)]
        updateSelectInput(session, "dist_variable", choices = sorted_vars)
      }
    })
    
    # Enhanced distribution plots with plotly
    output$distribution_plot <- renderPlotly({
      req(input$dist_variable, dataset())
      
      # Validate variable selection
      if (input$dist_variable == "" || input$dist_variable == "No numeric variables found") {
        return(plot_ly() %>%
          layout(
            title = "No numeric variables available",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          ))
      }
      
      var_data <- dataset()[[input$dist_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      # Check if there's enough data
      if (length(var_data) < 2) {
        return(plot_ly() %>%
          layout(
            title = "Insufficient data for plotting",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          ))
      }
      
      # Check for constant values
      unique_vals <- unique(var_data)
      if (length(unique_vals) == 1) {
        return(plot_ly() %>%
          layout(
            title = paste("Constant value:", unique_vals[1]),
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          ))
      }
      
      # Sample large datasets for performance
      if (length(var_data) > 10000) {
        set.seed(42)  # Reproducible sampling
        var_data <- sample(var_data, 10000)
        plot_title_suffix <- " (10K sample)"
      } else {
        plot_title_suffix <- ""
      }
      
      tryCatch({
        if (input$plot_type == "hist") {
          # Histogram
          plot_ly(x = ~var_data, type = "histogram", 
                  marker = list(color = "steelblue", line = list(color = "white", width = 1))) %>%
            layout(
              title = paste("Distribution of", input$dist_variable, plot_title_suffix),
              xaxis = list(title = input$dist_variable),
              yaxis = list(title = "Frequency")
            )
          
        } else if (input$plot_type == "box") {
          # Box plot
          plot_ly(y = ~var_data, type = "box", 
                  marker = list(color = "lightgreen"),
                  line = list(color = "darkgreen")) %>%
            layout(
              title = paste("Boxplot of", input$dist_variable, plot_title_suffix),
              yaxis = list(title = input$dist_variable),
              xaxis = list(title = "", showticklabels = FALSE)
            )
          
        } else if (input$plot_type == "density") {
          # Density plot
          density_vals <- density(var_data)
          plot_ly(x = density_vals$x, y = density_vals$y, type = "scatter", mode = "lines",
                  fill = "tozeroy", line = list(color = "purple", width = 2)) %>%
            layout(
              title = paste("Density of", input$dist_variable, plot_title_suffix),
              xaxis = list(title = input$dist_variable),
              yaxis = list(title = "Density")
            )
          
        } else if (input$plot_type == "violin") {
          # Violin plot
          plot_ly(y = ~var_data, type = "violin",
                  box = list(visible = TRUE),
                  marker = list(color = "orange")) %>%
            layout(
              title = paste("Violin plot of", input$dist_variable, plot_title_suffix),
              yaxis = list(title = input$dist_variable),
              xaxis = list(title = "", showticklabels = FALSE)
            )
        } else {
          # Default to histogram
          plot_ly(x = ~var_data, type = "histogram", 
                  marker = list(color = "steelblue", line = list(color = "white", width = 1))) %>%
            layout(
              title = paste("Distribution of", input$dist_variable, plot_title_suffix),
              xaxis = list(title = input$dist_variable),
              yaxis = list(title = "Frequency")
            )
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
    
    # Enhanced missing data pattern with better visualization
    output$missing_pattern <- renderPlot({
      # Reset margins to default
      par(mar = c(8, 4, 4, 2) + 0.1)
      
      req(dataset())
      info <- dataset_info()
      
      if (is.null(info)) return()
      
      missing_pct <- info$missing_percent
      
      # Filter out variables with no missing data for cleaner plot
      vars_with_missing <- missing_pct[missing_pct > 0]
      
      if (length(vars_with_missing) == 0) {
        plot.new()
        text(0.5, 0.5, "No missing data found", cex = 1.5, col = "green")
        return()
      }
      
      # Sort by missing percentage for better visualization
      vars_with_missing <- sort(vars_with_missing, decreasing = TRUE)
      
      # Color code by severity
      colors <- ifelse(vars_with_missing > 50, "darkred",
                      ifelse(vars_with_missing > 20, "orange",
                            ifelse(vars_with_missing > 5, "yellow", "lightblue")))
      
      # Create enhanced barplot
      par(mar = c(8, 4, 4, 2))  # Increase bottom margin for labels
      bp <- barplot(vars_with_missing, 
                   main = "Missing Data Pattern (Variables with Missing Data Only)",
                   ylab = "Missing %", 
                   col = colors,
                   las = 2,
                   cex.names = 0.8,
                   ylim = c(0, max(vars_with_missing) * 1.1))
      
      # Add reference lines
      abline(h = c(5, 20, 50), col = c("green", "orange", "red"), lty = 2, alpha = 0.7)
      
      # Add percentage labels on bars
      text(bp, vars_with_missing + max(vars_with_missing) * 0.02, 
           paste0(round(vars_with_missing, 1), "%"), 
           cex = 0.7, srt = 90, adj = 0)
      
      # Add legend
      legend("topright", 
             legend = c("< 5%", "5-20%", "20-50%", "> 50%"),
             fill = c("lightblue", "yellow", "orange", "darkred"),
             title = "Missing Data Level",
             cex = 0.8)
    })
    
    # Enhanced missing data summary table with cached data
    output$missing_summary <- DT::renderDataTable({
      req(dataset())
      info <- dataset_info()
      
      if (is.null(info)) return(NULL)
      
      missing_info <- data.frame(
        Variable = info$variables,
        Type = as.character(info$variable_types),
        Missing_Count = formatC(info$missing_counts, format = "f", digits = 0, big.mark = ","),
        Missing_Percent = info$missing_percent,
        Total_Values = formatC(rep(info$nrows, length(info$variables)), format = "f", digits = 0, big.mark = ","),
        stringsAsFactors = FALSE
      )
      
      # Add severity classification
      missing_info$Severity <- ifelse(missing_info$Missing_Percent > 50, "Critical",
                                     ifelse(missing_info$Missing_Percent > 20, "High",
                                           ifelse(missing_info$Missing_Percent > 5, "Moderate", "Low")))
      
      # Order by missing percentage (descending)
      missing_info <- missing_info[order(missing_info$Missing_Percent, decreasing = TRUE), ]
      
      missing_info
    }, options = list(
      pageLength = 15,
      order = list(list(3, "desc")),  # Order by missing percent
      columnDefs = list(
        list(targets = c(2, 3, 4), className = "dt-center"),
        list(targets = 5, className = "dt-center")
      )
    ))
    
    # Performance monitoring - show when data is being processed
    observeEvent(dataset(), {
      if (!is.null(dataset())) {
        data_size_mb <- round(as.numeric(object.size(dataset())) / 1024^2, 1)
        if (data_size_mb > 50) {
          showNotification(
            paste("Processing large dataset (", data_size_mb, "MB). Some operations may take longer."),
            type = "message", duration = 5
          )
        }
      }
    })
    
  })
}