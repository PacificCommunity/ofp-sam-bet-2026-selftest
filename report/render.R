options(stringsAsFactors = FALSE, bitmapType = "cairo")

required <- c("ggplot2", "patchwork", "dplyr", "scales", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing public report dependencies: ", paste(missing, collapse = ", "), call. = FALSE)
}

source("report/lib/jitter-report.R", local = .GlobalEnv)
source("report/lib/selftest-report.R", local = .GlobalEnv)

# PNG optimization is optional; the report remains lossless and reproducible
# when pngquant/optipng are not installed in the execution image.
mfclshiny_report_optimize_png <- function(path, ...) invisible(path)

payload_file <- "data/diagnostic/selftest-report-payload.rds"
data <- readRDS(payload_file)
stopifnot(
  identical(data$audit$format, "bet2026.selftest.report-payload.v1"),
  identical(as.integer(data$audit$expected_replicates), 50L),
  identical(as.integer(data$audit$completed_replicates), 50L),
  identical(as.integer(data$audit$converged_replicates), 50L),
  identical(as.numeric(data$audit$tau), 2),
  identical(as.integer(data$audit$parest_flag_305), 1L),
  identical(as.numeric(data$audit$fish_pars_4), 0),
  identical(as.integer(data$audit$refits_with_parest_flag_305_equal_1), 50L)
)
data <- mfclshiny_selftest_add_dynamic_nofish(data)
stopifnot(
  sum(data$derived$metric == "spawning_potential_nofish") ==
    sum(data$derived$metric == "spawning_potential")
)

dir.create("results", recursive = TRUE, showWarnings = FALSE)
build_selftest_report(
  data = data,
  output_dir = "results",
  title = "BET 2026 Diagnostic model: self-test",
  recent_years = 2021:2024,
  formats = c("png", "pdf"),
  width = 11,
  height = 6.2,
  dpi = 300,
  render_html = TRUE
)

audit <- data.frame(
  item = c(
    "Expected replicates", "Completed refits", "Converged refits",
    "MGC criterion", "Fixed tag overdispersion", "Refit mode",
    "Model source MD5", "Merged self-test archive SHA-256"
  ),
  value = c(
    data$audit$expected_replicates,
    data$audit$completed_replicates,
    data$audit$converged_replicates,
    format(data$audit$convergence_threshold, scientific = TRUE),
    paste0("τ = ", data$audit$tau),
    data$audit$refit_mode,
    data$audit$model_source_md5,
    data$audit$merged_selftest_archive_sha256
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(audit, "results/selftest-audit.csv", row.names = FALSE)

cat("Rendered public Diagnostic model self-test report from 50 completed simulate-refit replicates.\n")
