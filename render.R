# render.R

library(yaml)
library(quarto)
library(purrr)
library(cli)

# Helpers ──────────────────────────────────────────────────────────────────────

load_config <- function(exp_name) {
  path <- file.path("experiments", paste0(exp_name, ".yml"))
  
  if (!file.exists(path)) {
    cli::cli_abort("Configuration not found for: {exp_name} ({path})")
  }
  
  config <- yaml::read_yaml(path)
  
  mods <- setNames(
    config$modules,
    paste0("mod_", names(config$modules))
  )
  
  c(config[names(config) != "modules"], mods)
}

do_render <- function(input, output_file, params) {
  cli::cli_alert_info("Rendering {input}...")
  cli::cli_alert_info("Rendering {output_file}...")
  cli::cli_alert_info("Rendering {params}...")
  
  quarto::quarto_render(
    input,
    output_file    = output_file,
    execute_params = params
  )
  
  cli::cli_alert_success("{output_file} completed")
}

# Report input/output targets for a given exp_id, independent of where the
# params came from (a yml file or an in-memory list) ──────────────────────────
report_targets <- function(exp_id) {
  list(
    qc = list(
      input  = "_qc.qmd",
      output = paste0(exp_id, "QC_report.html")
    ),
    complete = list(
      input  = "_results.qmd",
      output = paste0(exp_id, "Results_report.html")
    ),
    full = list(
      input  = "_full_report.qmd",
      output = paste0(exp_id, "Full_report.html")
    )
  )
}

modes_to_render <- function(mode) {
  switch(
    mode,
    qc       = "qc",
    complete = "complete",
    full     = "full",
    both     = c("qc", "complete"),
    all      = c("qc", "complete", "full")
  )
}

# Core rendering routine: takes a params list directly, with no dependency on
# any yml file on disk ──────────────────────────────────────────────────────
render_with_params <- function(p,
                               mode  = c("qc", "complete", "full", "both", "all"),
                               label = p$exp_id) {

  mode <- match.arg(mode)
  targets   <- report_targets(p$exp_id)
  to_render <- modes_to_render(mode)

  cli::cli_h1("Experiment: {label} [{mode}]")

  purrr::walk(to_render, function(r) {
    do_render(
      input       = targets[[r]]$input,
      output_file = targets[[r]]$output,
      params      = p
    )
  })

  cli::cli_alert_success("Experiment {label} completed successfully")

  invisible(p)
}

# Main rendering function, for experiments configured via yml files in
# experiments/ (used by render_all(), i.e. outside the app) ───────────────────

render_experiment <- function(exp_name,
                              mode = c("qc", "complete", "full", "both", "all")) {
  
  mode <- match.arg(mode)
  p <- load_config(exp_name)
  render_with_params(p, mode = mode, label = exp_name)
}

# Uploaded-file support (app.R) ─────────────────────────────────────────────────

# Builds an in-memory experiment (params list + data folder) entirely from
# app.R form inputs plus the three uploaded data files (metadata, comparisons,
# psm.tsv). No experiment.yml is written to disk: the resulting params list is
# passed straight into render_with_params(), fully driven by the app UI.
build_uploaded_experiment <- function(exp_id,
                                      org_key,
                                      tmt,
                                      astral,
                                      use_unique,
                                      covars,
                                      metadata_path,
                                      comparisons_path,
                                      psm_path,
                                      uploads_dir = "uploads") {

  if (is.null(exp_id) || trimws(exp_id) == "") {
    cli::cli_abort("'exp_id' is required")
  }

  exp_name <- paste0(exp_id, "_upload_", format(Sys.time(), "%Y%m%d%H%M%S"))

  message("EXPNAME: ", exp_name)
  
  message("UPLOADDIR: ",uploads_dir)
  
  data_dir <- file.path(uploads_dir, exp_name)
  
  message("DATADIR: ", data_dir)
  
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  
  
  file.copy(metadata_path,    file.path(data_dir, "metadata.txt"),    overwrite = TRUE)
  metadata_upload_path <- file.path(data_dir, "metadata.txt")
  
  file.copy(comparisons_path, file.path(data_dir, "comparisons.txt"), overwrite = TRUE)
  comparisons_upload_path <- file.path(data_dir, "comparisons.txt")
  
  file.copy(psm_path,         file.path(data_dir, "psm.tsv"),         overwrite = TRUE)
  psm_upload_path <- file.path(data_dir, "psm.tsv")
  
  p <- list(
    data_path  = data_dir,
    org_key    = org_key,
    exp_id     = exp_id,
    tmt        = tmt,
    astral     = isTRUE(astral),
    use_unique = isTRUE(use_unique),
    covars     = covars %||% ""
    # metadata_upload_path =  metadata_upload_path,
    # comparisons_upload_path = comparisons_upload_path,
    # psm_upload_path = psm_upload_path
  )

  list(exp_name = exp_name, params = p)
}

`%||%` <- function(x, y) if (is.null(x) || identical(x, "")) y else x

# Returns the report file(s) that render_with_params(p, mode) produces, so
# app.R knows what to offer for download.
output_files_for_params <- function(p, mode) {
  targets <- report_targets(p$exp_id)
  keys    <- modes_to_render(mode)
  unname(vapply(targets[keys], `[[`, character(1), "output"))
}

# Same as above, but for yml-configured experiments (experiments/*.yml).
output_files_for_mode <- function(exp_name, mode) {
  p <- load_config(exp_name)
  output_files_for_params(p, mode)
}

# Render all experiments ───────────────────────────────────────────────────────

render_all <- function(mode = "all", pattern = "\\.yml$") {
  
  exp_names <- tools::file_path_sans_ext(
    list.files("experiments", pattern = pattern)
  )
  
  if (length(exp_names) == 0) {
    cli::cli_alert_warning("No experiment configurations were found in 'experiments/'")
    return(invisible(NULL))
  }
  
  cli::cli_h1("Rendering {length(exp_names)} experiment(s) [{mode}]")
  
  purrr::walk(exp_names, ~render_experiment(.x, mode = mode))
  
  cli::cli_alert_success("All experiments completed successfully")
}