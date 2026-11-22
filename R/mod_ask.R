# Ask module: natural-language query over the loaded domains.
#
# Thin Shiny wrapper over nl_query(). The user types a question; Claude proposes
# a structured plan; R validates and runs it. The tab always shows *what ran*
# (the plan) alongside the result and the reproducible code, so the answer is
# inspectable and never a black box. The deterministic core lives in nl_query.R.

# Example prompts offered as clickable chips.
.ask_examples <- c(
  "List the severe adverse events",
  "Show vital signs where the result is above 140",
  "How many serious adverse events are there"
)

mod_ask_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center mb-2",
      h3(class = "text-primary mb-0",
         icon("wand-magic-sparkles", class = "fa-fw me-2"), "Ask")
    ),
    p(class = "text-muted",
      "Ask a question in plain English. Claude proposes a query, and databoxR ",
      "validates and runs it against your data. You always see the plan it ran."),
    uiOutput(ns("availability")),
    fluidRow(
      column(width = 9,
        textAreaInput(ns("question"), label = NULL, width = "100%", rows = 2,
                      placeholder = "e.g. list the severe adverse events")
      ),
      column(width = 3,
        actionButton(ns("ask"), "Ask", icon = icon("paper-plane"),
                     class = "btn-primary w-100")
      )
    ),
    uiOutput(ns("examples")),
    uiOutput(ns("status")),
    conditionalPanel(
      condition = "output.has_result", ns = ns,
      fluidRow(
        bs4Dash::box(
          title = "What ran (query plan)", status = "info", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          uiOutput(ns("plan_summary"))
        )
      ),
      fluidRow(
        bs4Dash::box(
          title = "Result", status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          DT::dataTableOutput(ns("result"))
        )
      ),
      fluidRow(
        bs4Dash::box(
          title = "Reproducible code", status = "secondary", solidHeader = TRUE,
          width = 12, collapsible = TRUE, collapsed = TRUE,
          div(class = "mb-2",
            downloadButton(ns("dl_code"), "Download R script", class = "btn-sm")),
          verbatimTextOutput(ns("code"))
        )
      )
    )
  )
}

# Human-readable one-line summary of a plan (what the model decided to run).
#' @noRd
ask_plan_summary <- function(plan) {
  if (is.null(plan)) return("")
  action <- plan$action %||% "filter"
  if (action != "filter") {
    return(sprintf("%s for subject %s", action, plan$subject %||% "?"))
  }
  parts <- vapply(plan$filters %||% list(), function(f) {
    sprintf("%s %s %s", f$column %||% "?", f$op %||% "?", f$value %||% "?")
  }, character(1))
  cond <- if (length(parts))
    paste(parts, collapse = paste0(" ", tolower(plan$logic %||% "and"), " ")) else "(no filter)"
  sel <- plan$columns %||% character(0)
  sel <- sel[!is.na(sel) & nzchar(sel)]
  paste0("filter ", plan$domain %||% "?", " where ", cond,
         if (length(sel)) paste0("; keep ", paste(sel, collapse = ", ")) else "")
}

mod_ask_server <- function(id, datasets, propose = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Query is answerable only when a planner is available. A user-supplied
    # `propose` (e.g. tests) bypasses the ellmer/key requirement.
    have_llm <- function() {
      !is.null(propose) ||
        (requireNamespace("ellmer", quietly = TRUE) &&
           nzchar(Sys.getenv("ANTHROPIC_API_KEY")))
    }

    output$availability <- renderUI({
      if (have_llm()) return(NULL)
      div(class = "alert alert-warning", role = "alert",
          icon("triangle-exclamation"),
          " Natural-language queries need the ", tags$code("ellmer"),
          " package and an ", tags$code("ANTHROPIC_API_KEY"),
          " environment variable. Install ellmer and set the key, then restart ",
          "the app. The rest of databoxR works without them.")
    })

    output$examples <- renderUI({
      div(class = "mb-3",
        lapply(seq_along(.ask_examples), function(i) {
          actionButton(ns(paste0("ex_", i)), .ask_examples[i],
                       class = "btn-outline-secondary btn-sm me-2 mb-1")
        })
      )
    })

    # Wire example chips to fill the box.
    lapply(seq_along(.ask_examples), function(i) {
      observeEvent(input[[paste0("ex_", i)]], {
        shiny::updateTextAreaInput(session, "question", value = .ask_examples[i])
      }, ignoreInit = TRUE)
    })

    answer <- eventReactive(input$ask, {
      q <- trimws(input$question %||% "")
      if (!nzchar(q)) return(NULL)
      ds <- datasets()
      if (is.null(ds) || !length(ds)) {
        return(list(error = "Load one or more datasets first."))
      }
      if (!have_llm()) {
        return(list(error = "No planner available (see the notice above)."))
      }
      args <- list(question = q, datasets = ds)
      if (!is.null(propose)) args$propose <- propose
      shiny::withProgress(message = "Thinking...", value = 0.5, {
        do.call(nl_query, args)
      })
    })

    output$has_result <- reactive({
      a <- answer()
      !is.null(a) && !is.null(a$result)
    })
    outputOptions(output, "has_result", suspendWhenHidden = FALSE)

    output$status <- renderUI({
      a <- answer()
      if (is.null(a)) return(NULL)
      if (!is.null(a$error)) {
        return(div(class = "alert alert-danger", role = "alert",
                   icon("circle-exclamation"), " ", a$error))
      }
      if (!is.null(a$result) && nrow(a$result) == 0) {
        return(div(class = "alert alert-secondary", role = "alert",
                   icon("circle-info"), " The query ran but matched no rows."))
      }
      NULL
    })

    output$plan_summary <- renderUI({
      a <- answer()
      req(a, a$plan)
      warns <- a$valid$warnings %||% character(0)
      tagList(
        tags$code(ask_plan_summary(a$plan)),
        if (length(warns))
          div(class = "text-warning mt-2", icon("triangle-exclamation"), " ",
              paste(warns, collapse = " ")) else NULL
      )
    })

    output$result <- DT::renderDataTable({
      a <- answer()
      req(a, a$result)
      DT::datatable(a$result, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    output$code <- renderText({
      a <- answer()
      req(a, a$code)
      a$code
    })

    output$dl_code <- downloadHandler(
      filename = function() "databox_query.R",
      content = function(file) {
        a <- answer()
        writeLines(a$code %||% "# no code", file)
      }
    )

    invisible(NULL)
  })
}
