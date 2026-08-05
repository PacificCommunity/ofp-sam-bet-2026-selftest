args <- commandArgs(trailingOnly = TRUE)
check_outputs <- "--outputs" %in% args

payload_file <- "data/diagnostic/selftest-report-payload.rds"
if (!file.exists(payload_file)) stop("Missing ", payload_file, call. = FALSE)
data <- readRDS(payload_file)
required <- c("runs", "derived", "parameters", "management", "simulation", "sources", "audit")
if (!all(required %in% names(data))) stop("Incomplete self-test report payload.", call. = FALSE)

runs <- data$runs
if (nrow(runs) != 50L) stop("Expected 50 self-test runs; found ", nrow(runs), call. = FALSE)
if (sum(runs$run_completed %in% TRUE) != 50L) stop("Not all refits completed.", call. = FALSE)
if (sum(runs$converged %in% TRUE) != 50L) stop("Not all refits met the archived convergence criterion.", call. = FALSE)
if (any(!is.finite(runs$max_grad)) || any(abs(runs$max_grad) > 1e-4)) {
  stop("At least one refit exceeds MGC <= 1e-4.", call. = FALSE)
}
if (!identical(as.numeric(data$audit$tau), 2) ||
    !identical(as.integer(data$audit$parest_flag_305), 1L) ||
    !identical(as.numeric(data$audit$fish_pars_4), 0) ||
    !identical(as.integer(data$audit$refits_with_parest_flag_305_equal_1), 50L)) {
  stop("The fixed-tau audit is incomplete.", call. = FALSE)
}

strings <- unlist(lapply(data, function(x) {
  if (is.data.frame(x)) unlist(x[vapply(x, is.character, logical(1))], use.names = FALSE)
  else if (is.list(x)) unlist(x, use.names = FALSE)
  else character()
}), use.names = FALSE)
private <- strings[grepl("(^|/)(home|tmp|var/lib/condor)/|KflowOutput|dir_[0-9]+", strings)]
if (length(private)) stop("Private execution path found in public payload.", call. = FALSE)

# The sixth annual panel is reconstructed without a new model assumption:
# SB(F=0,t) = SB(t) / [SB(t) / SB(F=0,t)].
keys <- c("scenario", "model_label", "replicate", "year")
spawning <- data$derived[data$derived$metric == "spawning_potential", c(keys, "truth", "estimate")]
depletion <- data$derived[data$derived$metric == "depletion", c(keys, "truth", "estimate")]
paired <- merge(
  spawning,
  depletion,
  by = keys,
  suffixes = c("_sb", "_depletion")
)
if (!nrow(paired) || any(paired$truth_depletion <= 0) || any(paired$estimate_depletion <= 0)) {
  stop("Dynamic no-fishing spawning potential cannot be reconstructed safely.", call. = FALSE)
}
nofish_truth <- paired$truth_sb / paired$truth_depletion
nofish_estimate <- paired$estimate_sb / paired$estimate_depletion
if (any(!is.finite(nofish_truth)) || any(!is.finite(nofish_estimate))) {
  stop("Dynamic no-fishing spawning-potential audit failed.", call. = FALSE)
}

if (check_outputs) {
  needed <- c(
    "results/selftest-report.html",
    "results/selftest-audit.csv",
    "results/figures/selftest-key-recovery-diagnostic.png",
    "results/figures/selftest-recovery-diagnostic.png",
    "results/figures/selftest-simulation-diagnostic.png"
  )
  missing <- needed[!file.exists(needed)]
  if (length(missing)) stop("Missing report output(s): ", paste(missing, collapse = ", "), call. = FALSE)
  html <- paste(readLines("results/selftest-report.html", warn = FALSE), collapse = "\n")
  forbidden <- c("S0.90-F2", "Job 21641", "Job 22974", "/home/", "/tmp/", "/var/lib/condor/")
  hit <- forbidden[vapply(forbidden, grepl, logical(1), x = html, fixed = TRUE)]
  if (length(hit)) stop("Public HTML contains forbidden provenance text: ", paste(hit, collapse = ", "), call. = FALSE)
  if (!grepl("Diagnostic model", html, fixed = TRUE)) stop("Diagnostic model label missing.", call. = FALSE)
  if (!grepl("τ was fixed at 2", html, fixed = TRUE)) stop("Fixed-tau method text missing.", call. = FALSE)
  if (!grepl("No-fishing spawning potential", html, fixed = TRUE)) {
    stop("Dynamic no-fishing spawning-potential panel is missing.", call. = FALSE)
  }
  if (grepl("Replicate convergence", html, fixed = TRUE) || grepl("Tag simulation contract", html, fixed = TRUE)) {
    stop("Internal ledger tables must not appear in the report.", call. = FALSE)
  }
}

cat("Validated 50/50 self-test refits, fixed τ = 2, public-data hygiene, and report outputs.\n")
