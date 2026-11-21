# Subject Profile module (Phase 4)
#
# Thin Shiny wrapper over the pure helpers in subject_profile.R. Takes a reactive
# that yields a named list of loaded domain data frames and lets the user pick a
# single subject to follow across every domain: KPI cards, demographics, a visual
# cross-domain timeline, highlighted adverse events, raw per-domain records, and
# a reproducible-code / CSV export.

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
      uiOutput(ns("kpi_cards")),
      fluidRow(
        bs4Dash::box(
          title = "Demographics", status = "primary", solidHeader = TRUE,
          width = 4, collapsible = FALSE,
          uiOutput(ns("demographics"))
        ),
        bs4Dash::box(
          title = "Cross-domain timeline", status = "success", solidHeader = TRUE,
          width = 8, collapsible = TRUE,
          plotly::plotlyOutput(ns("timeline_plot"), height = "320px")
        )
      ),
      fluidRow(
        bs4Dash::box(
          title = "Adverse events", status = "warning", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          uiOutput(ns("ae_panel"))
        )
      ),
      fluidRow(
        bs4Dash::box(
          title = "Timeline detail", status = "secondary", solidHeader = TRUE,
          width = 12, collapsible = TRUE, collapsed = TRUE,
          DT::dataTableOutput(ns("timeline_table"))
        )
      ),
      fluidRow(
        bs4Dash::box(
          title = "Records by domain", status = "secondary", solidHeader = TRUE,
          width = 12, collapsible = TRUE, collapsed = TRUE,
          uiOutput(ns("domain_tables"))
        )
      ),
      fluidRow(
        bs4Dash::box(
          title = "Reproducible code and export", status = "info",
          solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          div(class = "mb-2",
            downloadButton(ns("dl_timeline"), "Download timeline (CSV)",
                           class = "btn-sm"),
            downloadButton(ns("dl_code"), "Download R script", class = "btn-sm")
          ),
          verbatimTextOutput(ns("code_preview"))
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

    # KPI value boxes: demographics highlights on top, event counts below.
    output$kpi_cards <- renderUI({
      ds <- datasets()
      sid <- current()
      k <- subject_kpis(ds, sid)
      demo <- subject_demographics(ds, sid)
      kv <- stats::setNames(as.list(demo$Value), demo$Field)
      g <- function(nm) {
        v <- kv[[nm]]
        if (is.null(v) || !length(v) || is.na(v) || !nzchar(v)) "-" else v
      }
      vb <- function(value, subtitle, icon_name, color) {
        bs4Dash::valueBox(value = value, subtitle = subtitle,
                          icon = icon(icon_name), color = color, width = 3)
      }
      sev <- if (is.na(k$worst_ae_severity)) "None" else k$worst_ae_severity
      tagList(
        fluidRow(
          vb(g("Age"), "Age", "cake-candles", "primary"),
          vb(g("Sex"), "Sex", "venus-mars", "primary"),
          vb(g("Arm"), "Arm", "pills", "info"),
          vb(g("Site"), "Site", "hospital", "info")
        ),
        fluidRow(
          vb(k$n_ae, "Adverse events", "triangle-exclamation",
             if (k$n_ae > 0) "warning" else "success"),
          vb(k$n_serious_ae, "Serious AEs", "circle-exclamation",
             if (k$n_serious_ae > 0) "danger" else "success"),
          vb(sev, "Worst AE severity", "gauge-high",
             if (identical(sev, "SEVERE")) "danger"
             else if (identical(sev, "MODERATE")) "warning" else "secondary"),
          vb(if (is.na(k$n_visits)) "-" else k$n_visits, "Visits",
             "calendar-check", "secondary")
        )
      )
    })

    output$demographics <- renderUI({
      demo <- subject_demographics(datasets(), current())
      if (!nrow(demo)) {
        return(div(class = "text-muted", "No demographics available."))
      }
      rows <- lapply(seq_len(nrow(demo)), function(i) {
        tags$tr(
          tags$th(demo$Field[i],
                  style = "text-align:left;padding-right:12px;white-space:nowrap;color:#6c757d;font-weight:500;"),
          tags$td(demo$Value[i], style = "font-weight:600;")
        )
      })
      tags$table(class = "table table-sm mb-0", tags$tbody(rows))
    })

    output$timeline_plot <- plotly::renderPlotly({
      tl <- subject_timeline(datasets(), current())
      if (nrow(tl) == 0) {
        return(
          plotly::plotly_empty(type = "scatter", mode = "markers") |>
            plotly::layout(title = list(text = "No dated events for this subject"))
        )
      }
      tl$Domain <- factor(tl$Domain)
      hover <- paste0(
        "<b>", as.character(tl$Domain), "</b><br>", tl$Event,
        "<br>", format(tl$Date, "%Y-%m-%d"),
        ifelse(is.na(tl$Day), "", paste0("<br>Study day ", tl$Day))
      )
      plotly::plot_ly(
        tl, x = ~Date, y = ~Domain, color = ~Domain,
        type = "scatter", mode = "markers",
        marker = list(size = 11, opacity = 0.85),
        text = hover, hoverinfo = "text"
      ) |>
        plotly::layout(
          showlegend = FALSE,
          xaxis = list(title = "Date"),
          yaxis = list(title = "", automargin = TRUE),
          margin = list(l = 70)
        )
    })

    output$ae_panel <- renderUI({
      ae <- pick_domain(subject_records(datasets(), current()), "AE")
      if (is.null(ae) || nrow(ae) == 0) {
        return(div(class = "text-muted",
                   icon("circle-check"), " No adverse events recorded for this subject."))
      }
      DT::dataTableOutput(ns("ae_table"))
    })

    output$ae_table <- DT::renderDataTable({
      ae <- pick_domain(subject_records(datasets(), current()), "AE")
      req(!is.null(ae) && nrow(ae) > 0)
      cols <- intersect(
        c("AETERM", "AEDECOD", "AEBODSYS", "AESEV", "AESER", "AEREL", "AEOUT",
          "AESTDTC", "AEENDTC"),
        names(ae)
      )
      show <- if (length(cols)) ae[, cols, drop = FALSE] else ae
      dt <- DT::datatable(show, rownames = FALSE,
                          options = list(pageLength = 5, scrollX = TRUE))
      if ("AESEV" %in% names(show)) {
        dt <- DT::formatStyle(
          dt, "AESEV",
          backgroundColor = DT::styleEqual(
            c("MILD", "MODERATE", "SEVERE"),
            c("#d1e7dd", "#fff3cd", "#f8d7da")
          ),
          fontWeight = "bold"
        )
      }
      if ("AESER" %in% names(show)) {
        dt <- DT::formatStyle(
          dt, "AESER",
          color = DT::styleEqual(c("Y", "N"), c("#dc3545", "#6c757d")),
          fontWeight = DT::styleEqual(c("Y"), c("bold"))
        )
      }
      dt
    })

    output$timeline_table <- DT::renderDataTable({
      tl <- subject_timeline(datasets(), current())
      if (nrow(tl) == 0) {
        return(DT::datatable(
          data.frame(Message = "No dated events found for this subject."),
          rownames = FALSE, colnames = "",
          options = list(dom = "t", paging = FALSE)
        ))
      }
      tl$Date <- format(tl$Date, "%Y-%m-%d")
      DT::datatable(tl, rownames = FALSE,
                    options = list(pageLength = 10, order = list(), scrollX = TRUE))
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

    observe({
      recs <- subject_records(datasets(), current())
      for (nm in names(recs)) {
        local({
          nm_local <- nm
          d_local <- utils::head(recs[[nm_local]], 100)
          output[[paste0("tbl_", nm_local)]] <- DT::renderDataTable({
            DT::datatable(d_local, rownames = FALSE,
                          options = list(pageLength = 5, scrollX = TRUE))
          })
        })
      }
    })

    output$code_preview <- renderText({
      recs <- subject_records(datasets(), current())
      subject_profile_code(current(), names(recs))
    })

    output$dl_timeline <- downloadHandler(
      filename = function() paste0("subject_", current(), "_timeline.csv"),
      content = function(file) {
        utils::write.csv(subject_timeline(datasets(), current()), file,
                         row.names = FALSE)
      }
    )

    output$dl_code <- downloadHandler(
      filename = function() paste0("subject_", current(), "_profile.R"),
      content = function(file) {
        recs <- subject_records(datasets(), current())
        writeLines(subject_profile_code(current(), names(recs)), file)
      }
    )

    invisible(NULL)
  })
}
