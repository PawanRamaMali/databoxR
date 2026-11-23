# Conformance module
#
# Surfaces the define.xml comparison and metadata-driven conformance checks
# (define_xml.R) in the app. The user uploads a define.xml, picks a loaded
# dataset and the define dataset to check it against, and sees a severity-ranked
# findings table plus a variable-by-variable comparison. Parsing a define needs
# the metacore package (Suggests); without it the tab explains how to enable it.
# The define functions are injectable so the module is testable without metacore.

mod_conformance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center mb-2",
      h3(class = "text-primary mb-0",
         icon("clipboard-check", class = "fa-fw me-2"), "Conformance")
    ),
    p(class = "text-muted",
      "Upload a define.xml and check a loaded dataset against it: missing or ",
      "unexpected variables, label mismatches, and values outside the ",
      "controlled terminology."),
    uiOutput(ns("availability")),
    fluidRow(
      column(width = 6,
        fileInput(ns("define"), "define.xml", accept = c(".xml"),
                  width = "100%")
      )
    ),
    uiOutput(ns("define_status")),
    conditionalPanel(
      condition = "output.define_ready", ns = ns,
      fluidRow(
        column(width = 6,
          selectInput(ns("data_ds"), "Loaded dataset", choices = NULL,
                      width = "100%")),
        column(width = 6,
          selectInput(ns("def_ds"), "Compare to define dataset", choices = NULL,
                      width = "100%"))
      ),
      uiOutput(ns("summary")),
      fluidRow(
        bs4Dash::box(
          title = "Conformance findings", status = "primary",
          solidHeader = TRUE, width = 12, collapsible = TRUE,
          DT::dataTableOutput(ns("findings"))
        )
      ),
      fluidRow(
        bs4Dash::box(
          title = "Variable comparison", status = "secondary",
          solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          DT::dataTableOutput(ns("comparison"))
        )
      ),
      fluidRow(
        bs4Dash::box(
          title = "Datasets in define", status = "info",
          solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          DT::dataTableOutput(ns("define_datasets"))
        )
      )
    )
  )
}

mod_conformance_server <- function(id, datasets, load_fn = NULL,
                                   check_fn = check_conformance,
                                   ds_fn = define_datasets,
                                   cmp_fn = compare_define_to_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Parsing a define needs metacore, unless a loader is injected (e.g. tests).
    have_define <- function() {
      !is.null(load_fn) || requireNamespace("metacore", quietly = TRUE)
    }
    loader <- function(path) (load_fn %||% load_define)(path)

    output$availability <- renderUI({
      if (have_define()) return(NULL)
      div(class = "alert alert-warning", role = "alert",
          icon("triangle-exclamation"),
          " Reading define.xml needs the ", tags$code("metacore"),
          " package (and ", tags$code("metatools"),
          " for controlled-terminology checks). Install them and restart the ",
          "app. The rest of databoxR works without them.")
    })

    # Parse the uploaded define into a metacore object (or an error marker).
    mc <- reactive({
      req(input$define)
      if (!have_define()) {
        return(structure("The metacore package is not installed.",
                         class = "conf_error"))
      }
      # load_define() checks the file extension, so hand it a .xml path.
      path <- input$define$datapath
      if (!grepl("\\.xml$", path, ignore.case = TRUE)) {
        xml_path <- tempfile(fileext = ".xml")
        file.copy(path, xml_path, overwrite = TRUE)
        path <- xml_path
      }
      tryCatch(
        loader(path),
        error = function(e) structure(conditionMessage(e), class = "conf_error")
      )
    })

    mc_ok <- reactive({
      m <- tryCatch(mc(), error = function(e) NULL)
      !is.null(m) && !inherits(m, "conf_error")
    })

    output$define_ready <- reactive(isTRUE(mc_ok()))
    outputOptions(output, "define_ready", suspendWhenHidden = FALSE)

    output$define_status <- renderUI({
      req(input$define)
      m <- mc()
      if (inherits(m, "conf_error")) {
        return(div(class = "alert alert-danger", role = "alert",
                   icon("circle-exclamation"),
                   " Could not read define.xml: ", as.character(m)))
      }
      NULL
    })

    define_ds_names <- reactive({
      req(mc_ok())
      df <- tryCatch(ds_fn(mc()), error = function(e) NULL)
      if (is.null(df) || !nrow(df)) character(0) else as.character(df$dataset)
    })

    # Populate the two pickers and default the define dataset to the one whose
    # name matches the chosen loaded dataset.
    observe({
      ds <- datasets()
      updateSelectInput(session, "data_ds",
                        choices = if (is.null(ds)) character(0) else names(ds))
    })

    observe({
      defs <- define_ds_names()
      sel <- defs[1]
      picked <- input$data_ds
      if (!is.null(picked) && nzchar(picked)) {
        hit <- defs[toupper(defs) == toupper(picked)]
        if (length(hit)) sel <- hit[1]
      }
      updateSelectInput(session, "def_ds", choices = defs, selected = sel)
    })

    findings <- reactive({
      req(mc_ok(), input$data_ds, input$def_ds)
      ds <- datasets()
      req(!is.null(ds), input$data_ds %in% names(ds))
      tryCatch(
        check_fn(ds[[input$data_ds]], mc(), input$def_ds),
        error = function(e) data.frame(
          Check = "Check failed", Variable = NA_character_,
          Severity = "Error", Details = conditionMessage(e),
          stringsAsFactors = FALSE)
      )
    })

    output$summary <- renderUI({
      f <- findings()
      counts <- table(factor(f$Severity,
                             levels = c("Error", "Warning", "Info", "Success")))
      badge <- function(label, n, cls) {
        if (n == 0 && label != "Error") return(NULL)
        tags$span(class = paste("badge", cls, "me-2"),
                  style = "font-size: 0.95rem;",
                  sprintf("%s: %d", label, n))
      }
      div(class = "mb-3",
        badge("Error", counts[["Error"]], "bg-danger"),
        badge("Warning", counts[["Warning"]], "bg-warning text-dark"),
        badge("Info", counts[["Info"]], "bg-info text-dark"),
        if (counts[["Success"]] > 0)
          tags$span(class = "badge bg-success", "All checks passed") else NULL
      )
    })

    output$findings <- DT::renderDataTable({
      f <- findings()
      dt <- DT::datatable(f, rownames = FALSE,
                          options = list(pageLength = 15, scrollX = TRUE))
      DT::formatStyle(
        dt, "Severity",
        backgroundColor = DT::styleEqual(
          c("Error", "Warning", "Info", "Success"),
          c("#f8d7da", "#fff3cd", "#cff4fc", "#d1e7dd")),
        fontWeight = "bold"
      )
    })

    output$comparison <- DT::renderDataTable({
      req(mc_ok(), input$data_ds, input$def_ds)
      ds <- datasets()
      req(!is.null(ds), input$data_ds %in% names(ds))
      cmp <- tryCatch(cmp_fn(ds[[input$data_ds]], mc(), input$def_ds),
                      error = function(e) NULL)
      req(!is.null(cmp))
      DT::datatable(cmp, rownames = FALSE,
                    options = list(pageLength = 15, scrollX = TRUE))
    })

    output$define_datasets <- DT::renderDataTable({
      req(mc_ok())
      df <- tryCatch(ds_fn(mc()), error = function(e) NULL)
      req(!is.null(df))
      DT::datatable(df, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    invisible(NULL)
  })
}
