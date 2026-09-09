# app.R
library(shiny)
library(callr)

# psm.tsv can be tens/hundreds of MB — raise Shiny's default 5 MB upload limit.
options(shiny.maxRequestSize = 500 * 1024^2)

ORGANISMS <- c(
  "Human (H. sapiens)"            = "human",
  "Mouse (M. musculus)"           = "mouse",
  "Rat (R. norvegicus)"           = "rat",
  "C. elegans"                    = "celegans",
  "Drosophila (D. melanogaster)"  = "drome",
  "Yeast (S. cerevisiae)"         = "yeast"
)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    #log_box {
      height: 550px;
      overflow-y: auto;
      background-color: #1e1e1e;
      color: #d4d4d4;
      padding: 10px;
      border-radius: 4px;
    }
    #log_box pre {
      color: inherit;
      background: none;
      border: none;
      margin: 0;
      white-space: pre-wrap;
      word-break: break-word;
    }
  "))),

  titlePanel("protflow"),
  sidebarLayout(
    sidebarPanel(

      h4("Experiment"),
      textInput("exp_id", "exp_id", placeholder = "MS_PR3_Cambridge"),
      selectInput("org_key", "Organism", choices = ORGANISMS),
      checkboxInput("astral", "Astral", value = FALSE),
      checkboxInput("use_unique", "use_unique", value = FALSE),
      textInput("covars", "Covariates (column name, optional)", value = ""),

      h4("Files"),
      fileInput("metadata_file", "metadata.txt",
                accept = c(".txt", ".tsv", ".csv")),
      fileInput("comparisons_file", "comparisons.txt",
                accept = c(".txt", ".tsv", ".csv")),
      fileInput("psm_file", "psm.tsv",
                accept = c(".tsv", ".txt")),

      selectInput("mode", "Mode",
                  choices = c("qc", "complete", "full", "both", "all")),
      actionButton("run", "Render", class = "btn-primary"),
      uiOutput("download_ui")
    ),
    mainPanel(
      textOutput("status"),
      br(),
      div(id = "log_box", verbatimTextOutput("log"))
    )
  )
)

server <- function(input, output, session) {
  source("render.R")

  rendered_files <- reactiveVal(character(0))
  job            <- reactiveVal(NULL)
  log_path       <- reactiveVal(NULL)
  log_text       <- reactiveVal("")
  job_running    <- reactiveVal(FALSE)
  current_job    <- reactiveVal(NULL)
  status_text    <- reactiveVal("")
  render_start   <- reactiveVal(NULL)

  observeEvent(input$run, {

    rendered_files(character(0))
    message("METADATA: " , input$metadata_file$datapat)
    setup <- tryCatch({
      req(input$exp_id, input$metadata_file, input$comparisons_file, input$psm_file)
      
      built <- build_uploaded_experiment(
        exp_id           = input$exp_id,
        org_key          = input$org_key,
        tmt              = "tmtpro",
        astral           = input$astral,
        use_unique       = input$use_unique,
        covars           = input$covars,
        metadata_path    = input$metadata_file$datapath,
        comparisons_path = input$comparisons_file$datapath,
        psm_path         = input$psm_file$datapath
      )
      
      list(exp_name = built$exp_name, params = built$params, mode = input$mode)
      
    }, error = function(e) {
      message(e)
      status_text(paste("Error:", conditionMessage(e)))
      NULL
    })

    if (is.null(setup)) return(invisible(NULL))

    dir.create("logs", showWarnings = FALSE)
    lp <- file.path("logs", paste0(setup$exp_name, ".log"))
    file.create(lp)
    log_path(lp)
    current_job(setup)
    render_start(Sys.time())

    proc <- callr::r_bg(
      func = function(params, mode, label) {
        source("render.R")
        render_with_params(params, mode = mode, label = label)
      },
      args   = list(params = setup$params, mode = setup$mode, label = setup$exp_name),
      stdout = lp,
      stderr = lp,
      wd     = getwd()
    )

    job(proc)
    job_running(TRUE)
    status_text("Running...")
  })

  observe({
    req(job_running())
    invalidateLater(750, session)

    lp <- log_path()
    if (!is.null(lp) && file.exists(lp)) {
      log_text(paste(readLines(lp, warn = FALSE), collapse = "\n"))
    }

    proc <- job()
    if (!is.null(proc) && !proc$is_alive()) {
      job_running(FALSE)
      cj <- current_job()
      exit_ok <- identical(proc$get_exit_status(), 0L)

      if (exit_ok) {
        status_text("Completed")

        report_files <- output_files_for_params(cj$params, cj$mode)

        results_files <- character(0)
        if (dir.exists("results")) {
          all_results <- list.files("results", full.names = TRUE)
          if (length(all_results) > 0) {
            mtimes        <- file.info(all_results)$mtime
            results_files <- all_results[mtimes >= render_start() - 2]
          }
        }

        rendered_files(c(report_files, results_files))
      } else {
        status_text(paste0("Failed (exit code ", proc$get_exit_status(), ")"))
      }
    }
  })

  output$status <- renderText(status_text())
  output$log    <- renderText(log_text())

  output$download_ui <- renderUI({
    files <- rendered_files()
    if (length(files) == 0 || !all(file.exists(files))) return(NULL)

    label <- if (length(files) == 1) {
      "Download"
    } else {
      "Download report + results (.zip)"
    }

    tagList(
      br(),
      downloadButton("download_report", label, class = "btn-success")
    )
  })

  output$download_report <- downloadHandler(
    filename = function() {
      files <- rendered_files()
      if (length(files) == 1) basename(files) else "protflow_reports.zip"
    },
    content = function(con) {
      files <- rendered_files()
      if (length(files) == 1) {
        file.copy(files, con, overwrite = TRUE)
      } else {
        zip::zip(con, files, mode = "cherry-pick")
      }
    }
  )
}

shinyApp(ui, server)
