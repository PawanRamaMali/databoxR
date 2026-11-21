# Subject Profile module (Phase 4)
#
# Thin Shiny wrapper over the pure helpers in subject_profile.R. Takes a reactive
# that yields a named list of loaded domain data frames and lets the user pick a
# single subject to follow across every domain.

mod_subject_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center mb-2",
      h3(class = "text-primary mb-0",
         icon("user-md", class = "fa-fw me-2"), "Subject Profile")
    ),
    p(class = "text-muted",
      "Pick one subject and follow them across every loaded domain."),
    fluidRow(
      column(width = 5,
        selectizeInput(ns("subject"), "Subject (USUBJID)", choices = NULL,
                       width = "100%",
                       options = list(placeholder = "Select a subject..."))
      )
    ),
    uiOutput(ns("empty_msg")),
    conditionalPanel(
      condition = "input.subject != undefined && input.subject != ''", ns = ns,
      fluidRow(
        bs4Dash::box(
          title = "Demographics", status = "primary", solidHeader = TRUE,
          width = 6, collapsible = FALSE,
          DT::dataTableOutput(ns("demographics"))
        ),
        bs4Dash::box(
          title = "Records per domain", status = "info", solidHeader = TRUE,
          width = 6, collapsible = FALSE,
          DT::dataTableOutput(ns("overview"))
        )
      ),
      fluidRow(
        bs4Dash::box(
          title = "Cross-domain timeline", status = "success", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          DT::dataTableOutput(ns("timeline"))
        )
      ),
      fluidRow(
        bs4Dash::box(
          title = "Records by domain", status = "secondary", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          uiOutput(ns("domain_tables"))
        )
      )
    )
  )
}

mod_subject_server <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    subjects <- reactive({
      ds <- datasets()
      if (is.null(ds) || !length(ds)) return(character(0))
      list_subjects(ds)
    })

    observe({
      subs <- subjects()
      updateSelectizeInput(
        session, "subject",
        choices = subs,
        selected = if (length(subs)) subs[1] else "",
        server = TRUE
      )
    })

    output$empty_msg <- renderUI({
      ds <- datasets()
      if (is.null(ds) || !length(ds)) {
        div(class = "alert alert-info", role = "alert",
            icon("info-circle"),
            " Load one or more datasets (or launch with demo data) to explore subject profiles.")
      } else if (!length(subjects())) {
        div(class = "alert alert-warning", role = "alert",
            icon("exclamation-triangle"),
            " No subject identifier (USUBJID or SUBJID) was found in the loaded datasets.")
      } else {
        NULL
      }
    })

    current <- reactive({
      req(datasets())
      req(input$subject)
      input$subject
    })

    output$demographics <- DT::renderDataTable({
      demo <- subject_demographics(datasets(), current())
      DT::datatable(
        demo, rownames = FALSE, colnames = c("", ""),
        options = list(dom = "t", ordering = FALSE, paging = FALSE)
      )
    })

    output$overview <- DT::renderDataTable({
      ov <- subject_overview(datasets(), current())
      DT::datatable(
        ov, rownames = FALSE,
        options = list(dom = "t", paging = FALSE)
      )
    })

    output$timeline <- DT::renderDataTable({
      tl <- subject_timeline(datasets(), current())
      if (nrow(tl) == 0) {
        return(DT::datatable(
          data.frame(Message = "No dated events found for this subject."),
          rownames = FALSE, colnames = "",
          options = list(dom = "t", paging = FALSE)
        ))
      }
      tl$Date <- format(tl$Date, "%Y-%m-%d")
      DT::datatable(
        tl, rownames = FALSE,
        options = list(pageLength = 10, order = list(), scrollX = TRUE)
      )
    })

    output$domain_tables <- renderUI({
      recs <- subject_records(datasets(), current())
      if (!length(recs)) {
        return(div(class = "text-muted", "No records for this subject."))
      }
      tagList(lapply(names(recs), function(nm) {
        tags$details(
          open = NA,
          style = "margin-bottom: 10px;",
          tags$summary(
            style = "cursor: pointer; font-weight: 600; padding: 4px 0;",
            sprintf("%s  (%d record%s)", nm, nrow(recs[[nm]]),
                    if (nrow(recs[[nm]]) == 1) "" else "s")
          ),
          DT::dataTableOutput(ns(paste0("tbl_", nm)))
        )
      }))
    })

    # Render each domain's record table into its own dynamic output.
    observe({
      recs <- subject_records(datasets(), current())
      for (nm in names(recs)) {
        local({
          nm_local <- nm
          d_local <- utils::head(recs[[nm_local]], 100)
          output[[paste0("tbl_", nm_local)]] <- DT::renderDataTable({
            DT::datatable(
              d_local, rownames = FALSE,
              options = list(pageLength = 5, scrollX = TRUE)
            )
          })
        })
      }
    })

    invisible(NULL)
  })
}
