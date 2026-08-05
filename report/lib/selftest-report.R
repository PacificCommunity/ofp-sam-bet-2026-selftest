mfclshiny_selftest_bind_rows <- function(rows) {
  rows <- rows[vapply(rows, is.data.frame, logical(1L))]
  rows <- rows[vapply(rows, nrow, integer(1L)) > 0L]
  if (!length(rows)) return(data.frame())
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    for (column in setdiff(columns, names(x))) x[[column]] <- NA
    x[, columns, drop = FALSE]
  })
  do.call(rbind, rows)
}

mfclshiny_selftest_first_value <- function(x, default = NA) {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) default else x[[1L]]
}

mfclshiny_selftest_provenance_row <- function(provenance, selftest_root, model_hint) {
  if (is.null(provenance)) return(NULL)
  provenance <- as.data.frame(provenance, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(provenance)) return(NULL)
  path <- tolower(normalizePath(selftest_root, winslash = "/", mustWork = FALSE))
  score <- numeric(nrow(provenance))
  for (field in intersect(c("selftest_id", "selftest_job", "check_id", "check_job", "model_id", "model_job"), names(provenance))) {
    values <- tolower(trimws(as.character(provenance[[field]])))
    score <- score + ifelse(
      vapply(values, function(value) nzchar(value) && grepl(value, path, fixed = TRUE), logical(1L)),
      100,
      0
    )
  }
  if ("model_label" %in% names(provenance) && nzchar(model_hint)) {
    score <- score + ifelse(
      grepl(tolower(model_hint), tolower(as.character(provenance$model_label)), fixed = TRUE),
      10,
      0
    )
  }
  if (!any(score > 0) && nrow(provenance) != 1L) return(NULL)
  provenance[which.max(score), , drop = FALSE]
}

mfclshiny_selftest_rep_number <- function(path) {
  suppressWarnings(as.integer(sub("^rep_0*", "", basename(path))))
}

mfclshiny_selftest_metric_labels <- function() {
  c(
    depletion = "Depletion (SB/SB(F=0))",
    spawning_potential = "Spawning potential (SB)",
    spawning_potential_nofish = "No-fishing spawning potential (SB_F=0)",
    recruitment = "Recruitment (R)",
    fishing_mortality = "Annual fishing mortality (F)",
    relative_fishing_mortality = "Aggregate fishing mortality (F/FMSY)"
  )
}

mfclshiny_selftest_metric_units <- function() {
  c(
    depletion = "SB[t]/SB[list(F==0,t)]",
    spawning_potential = "SB[t]~~(10^3~t)",
    spawning_potential_nofish = "SB[list(F==0,t)]~~(10^3~t)",
    recruitment = "R[t]~~(millions)",
    fishing_mortality = "F[t]~~(year^{-1})",
    relative_fishing_mortality = "F[t]/F[MSY]"
  )
}

mfclshiny_selftest_add_dynamic_nofish <- function(data) {
  if (!is.list(data) || !is.data.frame(data$derived)) return(data)
  x <- data$derived
  if ("spawning_potential_nofish" %in% x$metric) return(data)
  keys <- c("scenario", "model_label", "replicate", "year")
  spawning <- x[x$metric == "spawning_potential", c(keys, "truth", "estimate"), drop = FALSE]
  depletion <- x[x$metric == "depletion", c(keys, "truth", "estimate"), drop = FALSE]
  names(spawning)[names(spawning) %in% c("truth", "estimate")] <- c("sb_truth", "sb_estimate")
  names(depletion)[names(depletion) %in% c("truth", "estimate")] <- c("dep_truth", "dep_estimate")
  nofish <- merge(spawning, depletion, by = keys, all = FALSE, sort = FALSE)
  valid <- is.finite(nofish$sb_truth) & is.finite(nofish$sb_estimate) &
    is.finite(nofish$dep_truth) & is.finite(nofish$dep_estimate) &
    nofish$dep_truth > .Machine$double.eps & nofish$dep_estimate > .Machine$double.eps
  nofish <- nofish[valid, , drop = FALSE]
  truth <- nofish$sb_truth / nofish$dep_truth
  estimate <- nofish$sb_estimate / nofish$dep_estimate
  addition <- data.frame(
    nofish[, keys, drop = FALSE],
    metric = "spawning_potential_nofish",
    quantity = "No-fishing spawning potential (SB_F=0)",
    truth = truth,
    estimate = estimate,
    relative_error = (estimate - truth) / abs(truth),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  data$derived <- mfclshiny_selftest_bind_rows(list(x, addition))
  data
}

mfclshiny_selftest_recent_quantity_labels <- function() {
  c(
    recent_depletion = "Recent depletion (SB_recent/SB_F=0)",
    sb_recent = "Recent spawning potential (SB_recent)",
    sbf0_recent = "No-fishing spawning potential (SB_F=0)",
    recruitment_recent = "Recent recruitment (R_recent)",
    f_diagnostic_recent = "Recent annual-F diagnostic (mean F)",
    aggregate_f_recent = "Recent aggregate fishing mortality (mean F/FMSY)"
  )
}

mfclshiny_selftest_native_management_labels <- function() {
  c(
    msy = "Maximum sustainable yield (MSY)",
    fmsy = "Fishing mortality at MSY (F_MSY)",
    sbmsy = "Equilibrium spawning biomass at MSY (B^S_MSY)",
    frecent_fmsy = "Recent fishing mortality relative to MSY (F_recent/F_MSY)"
  )
}

mfclshiny_selftest_report_quantities <- function() {
  c(
    "Recent depletion (SB_recent/SB_F=0)",
    "Recent spawning potential (SB_recent)",
    "No-fishing spawning potential (SB_F=0)",
    "Recent recruitment (R_recent)",
    "Maximum sustainable yield (MSY)",
    "Fishing mortality at MSY (F_MSY)",
    "Equilibrium spawning biomass at MSY (B^S_MSY)",
    "Recent fishing mortality relative to MSY (F_recent/F_MSY)"
  )
}

mfclshiny_selftest_parameter_labels <- function() {
  c(
    kappa = "von Bertalanffy growth rate (K)",
    L1 = "Mean length at youngest age (L1)",
    L2 = "Mean length at oldest age (L2)",
    s1 = "Mean SD of length-at-age (s1)",
    s2 = "Age trend in length-at-age SD (s2)",
    totpop = "Log recruitment scale (ln R0)"
  )
}

mfclshiny_selftest_key_parameter_labels <- function() {
  c(
    totpop = "Log recruitment scale (ln R0)",
    `sv(21)` = "Stock-recruit density-dependence coefficient",
    `vb_coff(1)` = "Mean length at youngest age (L1)",
    `vb_coff(2)` = "Mean length at oldest age (L2)",
    `vb_coff(3)` = "von Bertalanffy growth rate (K)",
    `var_coff(1)` = "Mean SD of length-at-age (s1)",
    `var_coff(2)` = "Age trend in length-at-age SD (s2)"
  )
}

mfclshiny_selftest_recent_years <- function(data, scenario, years = NULL, n_years = 4L) {
  available <- sort(unique(suppressWarnings(as.numeric(
    data$derived$year[data$derived$scenario == scenario]
  ))))
  available <- available[is.finite(available)]
  requested <- suppressWarnings(as.numeric(years))
  requested <- sort(unique(requested[is.finite(requested)]))
  if (length(requested)) {
    selected <- intersect(requested, available)
    if (length(selected)) return(selected)
  }
  n_years <- suppressWarnings(as.integer(n_years))
  if (!length(n_years) || !is.finite(n_years[[1L]]) || n_years[[1L]] < 1L) n_years <- 4L
  utils::tail(available, min(length(available), n_years[[1L]]))
}

mfclshiny_selftest_year_label <- function(years) {
  years <- sort(unique(suppressWarnings(as.numeric(years))))
  years <- years[is.finite(years)]
  if (!length(years)) return("recent period")
  if (length(years) == 1L) return(as.character(years[[1L]]))
  paste0(min(years), "\u2013", max(years))
}

mfclshiny_selftest_read_derived <- function(selftest_root, scenario, model_label) {
  files <- list.files(
    file.path(selftest_root, "recovery"),
    pattern = "^derived_recovery[.]csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  labels <- mfclshiny_selftest_metric_labels()
  rows <- list()
  for (file in files) {
    raw <- tryCatch(
      utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (!is.data.frame(raw) || !nrow(raw) || !"year" %in% names(raw)) next
    replicate <- mfclshiny_selftest_rep_number(dirname(file))
    for (metric in names(labels)) {
      truth <- paste0(metric, "_truth")
      estimate <- paste0(metric, "_estimate")
      if (!all(c(truth, estimate) %in% names(raw))) next
      truth_value <- suppressWarnings(as.numeric(raw[[truth]]))
      estimate_value <- suppressWarnings(as.numeric(raw[[estimate]]))
      denominator <- abs(truth_value)
      relative_error <- ifelse(
        is.finite(denominator) & denominator > .Machine$double.eps,
        (estimate_value - truth_value) / denominator,
        NA_real_
      )
      rows[[length(rows) + 1L]] <- data.frame(
        scenario = scenario,
        model_label = model_label,
        replicate = replicate,
        year = suppressWarnings(as.numeric(raw$year)),
        metric = metric,
        quantity = unname(labels[[metric]]),
        truth = truth_value,
        estimate = estimate_value,
        relative_error = relative_error,
        source_file = normalizePath(file, winslash = "/", mustWork = FALSE),
        stringsAsFactors = FALSE
      )
    }
  }
  out <- mfclshiny_selftest_bind_rows(rows)
  if (!nrow(out)) return(out)
  out[
    is.finite(out$replicate) & is.finite(out$year) &
      is.finite(out$truth) & is.finite(out$estimate),
    ,
    drop = FALSE
  ]
}

mfclshiny_selftest_read_parameters <- function(selftest_root, scenario, model_label) {
  key_files <- list.files(
    file.path(selftest_root, "recovery"),
    pattern = "^parameter_recovery[.]csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  key_labels <- mfclshiny_selftest_key_parameter_labels()
  key_rows <- lapply(key_files, function(file) {
    raw <- tryCatch(
      utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    required <- c("name", "truth", "estimate", "key_parameter")
    if (!is.data.frame(raw) || !nrow(raw) || !all(required %in% names(raw))) return(NULL)
    raw$name <- as.character(raw$name)
    raw <- raw[raw$key_parameter %in% TRUE & raw$name %in% names(key_labels), , drop = FALSE]
    if (!nrow(raw)) return(NULL)
    truth <- suppressWarnings(as.numeric(raw$truth))
    estimate <- suppressWarnings(as.numeric(raw$estimate))
    relative_error <- ifelse(
      is.finite(truth) & abs(truth) > .Machine$double.eps,
      (estimate - truth) / abs(truth),
      NA_real_
    )
    data.frame(
      scenario = scenario,
      model_label = model_label,
      replicate = mfclshiny_selftest_rep_number(dirname(file)),
      parameter = raw$name,
      parameter_label = unname(key_labels[raw$name]),
      index = suppressWarnings(as.integer(raw$index)),
      truth = truth,
      estimate = estimate,
      relative_error = relative_error,
      source_file = normalizePath(file, winslash = "/", mustWork = FALSE),
      stringsAsFactors = FALSE
    )
  })
  key_out <- mfclshiny_selftest_bind_rows(key_rows)
  if (nrow(key_out)) {
    return(key_out[
      is.finite(key_out$replicate) & is.finite(key_out$truth) &
        is.finite(key_out$estimate) & is.finite(key_out$relative_error),
      ,
      drop = FALSE
    ])
  }
  files <- list.files(
    file.path(selftest_root, "recovery"),
    pattern = "^profile_parameter_recovery[.]csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  labels <- mfclshiny_selftest_parameter_labels()
  rows <- lapply(files, function(file) {
    raw <- tryCatch(
      utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    required <- c("parameter", "index", "truth_value", "refit_value")
    if (!is.data.frame(raw) || !nrow(raw) || !all(required %in% names(raw))) return(NULL)
    raw$parameter <- as.character(raw$parameter)
    raw <- raw[raw$parameter %in% names(labels), , drop = FALSE]
    if (!nrow(raw)) return(NULL)
    truth <- suppressWarnings(as.numeric(raw$truth_value))
    estimate <- suppressWarnings(as.numeric(raw$refit_value))
    relative_error <- ifelse(
      is.finite(truth) & abs(truth) > .Machine$double.eps,
      (estimate - truth) / abs(truth),
      NA_real_
    )
    data.frame(
      scenario = scenario,
      model_label = model_label,
      replicate = mfclshiny_selftest_rep_number(dirname(file)),
      parameter = raw$parameter,
      parameter_label = unname(labels[raw$parameter]),
      index = suppressWarnings(as.integer(raw$index)),
      truth = truth,
      estimate = estimate,
      relative_error = relative_error,
      source_file = normalizePath(file, winslash = "/", mustWork = FALSE),
      stringsAsFactors = FALSE
    )
  })
  out <- mfclshiny_selftest_bind_rows(rows)
  if (!nrow(out)) return(out)
  out[
    is.finite(out$replicate) & is.finite(out$truth) &
      is.finite(out$estimate) & is.finite(out$relative_error),
    ,
    drop = FALSE
  ]
}

mfclshiny_selftest_read_management <- function(selftest_root, scenario, model_label) {
  files <- list.files(
    file.path(selftest_root, "recovery"),
    pattern = "^management_recovery[.]csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  labels <- mfclshiny_selftest_native_management_labels()
  rows <- lapply(files, function(file) {
    raw <- tryCatch(
      utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    required <- c("metric", "truth", "estimate")
    if (!is.data.frame(raw) || !nrow(raw) || !all(required %in% names(raw))) return(NULL)
    raw$metric <- as.character(raw$metric)
    raw <- raw[raw$metric %in% names(labels), , drop = FALSE]
    if (!nrow(raw)) return(NULL)
    truth <- suppressWarnings(as.numeric(raw$truth))
    estimate <- suppressWarnings(as.numeric(raw$estimate))
    data.frame(
      scenario = scenario,
      model_label = model_label,
      replicate = mfclshiny_selftest_rep_number(dirname(file)),
      metric = raw$metric,
      quantity = unname(labels[raw$metric]),
      truth = truth,
      estimate = estimate,
      relative_error = ifelse(
        is.finite(truth) & abs(truth) > .Machine$double.eps,
        (estimate - truth) / abs(truth),
        NA_real_
      ),
      native_source = if ("native_source" %in% names(raw)) {
        as.character(raw$native_source)
      } else "",
      source_file = normalizePath(file, winslash = "/", mustWork = FALSE),
      stringsAsFactors = FALSE
    )
  })
  out <- mfclshiny_selftest_bind_rows(rows)
  if (!nrow(out)) return(out)
  out[
    is.finite(out$replicate) & is.finite(out$truth) &
      is.finite(out$estimate) & is.finite(out$relative_error),
    ,
    drop = FALSE
  ]
}

mfclshiny_selftest_weighted_mean <- function(value, weight) {
  value <- suppressWarnings(as.numeric(value))
  weight <- suppressWarnings(as.numeric(weight))
  valid <- is.finite(value) & is.finite(weight) & weight > 0
  if (any(valid)) return(stats::weighted.mean(value[valid], weight[valid]))
  value <- value[is.finite(value)]
  if (length(value)) mean(value) else NA_real_
}

mfclshiny_selftest_read_simulation <- function(selftest_root, scenario, model_label) {
  files <- list.files(
    file.path(selftest_root, "inputs"),
    pattern = "^data_simulation_summary[.]rds$",
    recursive = TRUE,
    full.names = TRUE
  )
  selected <- c("CPUE", "length mean", "age-length mean age", "tag recaptures by region")
  rows <- list()
  for (file in files) {
    raw <- tryCatch(readRDS(file), error = function(e) NULL)
    required <- c("component", "year", "base_value", "pseudo_value")
    if (!is.data.frame(raw) || !nrow(raw) || !all(required %in% names(raw))) next
    raw <- raw[as.character(raw$component) %in% selected, , drop = FALSE]
    if (!nrow(raw)) next
    raw$year <- suppressWarnings(as.numeric(raw$year))
    raw <- raw[is.finite(raw$year), , drop = FALSE]
    groups <- split(seq_len(nrow(raw)), interaction(raw$component, raw$year, drop = TRUE))
    aggregate_rows <- lapply(groups, function(index) {
      x <- raw[index, , drop = FALSE]
      component <- as.character(x$component[[1L]])
      if (identical(component, "tag recaptures by region")) {
        base <- sum(suppressWarnings(as.numeric(x$base_value)), na.rm = TRUE)
        pseudo <- sum(suppressWarnings(as.numeric(x$pseudo_value)), na.rm = TRUE)
      } else {
        base_weight <- if (
          "base_aggregation_weight" %in% names(x) &&
            any(is.finite(suppressWarnings(as.numeric(x$base_aggregation_weight))) &
              suppressWarnings(as.numeric(x$base_aggregation_weight)) > 0)
        ) x$base_aggregation_weight else x$n
        pseudo_weight <- if (
          "pseudo_aggregation_weight" %in% names(x) &&
            any(is.finite(suppressWarnings(as.numeric(x$pseudo_aggregation_weight))) &
              suppressWarnings(as.numeric(x$pseudo_aggregation_weight)) > 0)
        ) x$pseudo_aggregation_weight else x$n
        base <- mfclshiny_selftest_weighted_mean(x$base_value, base_weight)
        pseudo <- mfclshiny_selftest_weighted_mean(x$pseudo_value, pseudo_weight)
      }
      provenance <- if ("reference_provenance" %in% names(x)) {
        paste(sort(unique(na.omit(as.character(x$reference_provenance)))), collapse = ", ")
      } else ""
      data.frame(
        scenario = scenario,
        model_label = model_label,
        replicate = mfclshiny_selftest_rep_number(dirname(file)),
        component = component,
        year = x$year[[1L]],
        reference = base,
        pseudo = pseudo,
        reference_provenance = provenance,
        stringsAsFactors = FALSE
      )
    })
    rows[[length(rows) + 1L]] <- mfclshiny_selftest_bind_rows(aggregate_rows)
  }
  out <- mfclshiny_selftest_bind_rows(rows)
  if (!nrow(out)) return(out)
  out[
    is.finite(out$replicate) & is.finite(out$year) &
      is.finite(out$reference) & is.finite(out$pseudo),
    ,
    drop = FALSE
  ]
}

mfclshiny_selftest_grad_reference <- function(selftest_root, runs) {
  refit_dirs <- list.dirs(file.path(selftest_root, "refit"), recursive = FALSE, full.names = TRUE)
  for (folder in refit_dirs) {
    info <- tryCatch(readRDS(file.path(folder, "model_info.rds")), error = function(e) NULL)
    value <- suppressWarnings(as.numeric(tryCatch(info$convergence_threshold, error = function(e) NA_real_)))
    if (length(value) && is.finite(value[[1L]]) && value[[1L]] > 0) return(value[[1L]])
    exponent <- suppressWarnings(as.numeric(tryCatch(info$convergence_exponent, error = function(e) NA_real_)))
    if (length(exponent) && is.finite(exponent[[1L]])) return(10^exponent[[1L]])
  }
  values <- suppressWarnings(as.numeric(runs$max_grad[runs$converged %in% TRUE]))
  values <- values[is.finite(values) & values > 0]
  if (length(values)) 10^ceiling(log10(max(values))) else NA_real_
}

mfclshiny_selftest_model_settings <- function(model_root) {
  reference <- tryCatch(
    mfclshiny_retro_reference(model_root),
    error = function(e) NULL
  )
  value <- function(name, fallback) {
    x <- suppressWarnings(as.numeric(tryCatch(attr(reference, name), error = function(e) NA_real_)))
    if (!length(x) || !is.finite(x[[1L]])) fallback else x[[1L]]
  }
  periods_per_year <- as.integer(value("periods_per_year", 4L))
  if (!is.finite(periods_per_year) || periods_per_year < 1L) periods_per_year <- 4L
  sb_recent_years_raw <- value("sb_recent_years", 0)
  sbf0_recent_years_raw <- value("sbf0_recent_years", 0)
  data.frame(
    periods_per_year = periods_per_year,
    sb_recent_years = if (sb_recent_years_raw > 0) {
      as.integer(sb_recent_years_raw)
    } else 4L,
    sbf0_recent_years = if (sbf0_recent_years_raw > 0) {
      as.integer(sbf0_recent_years_raw)
    } else 10L,
    sb_recent_uses_native_default = !is.finite(sb_recent_years_raw) ||
      sb_recent_years_raw <= 0,
    sbf0_recent_uses_native_default = !is.finite(sbf0_recent_years_raw) ||
      sbf0_recent_years_raw <= 0,
    f_recent_periods = as.integer(value("f_recent_periods", 5L * periods_per_year)),
    f_omit_periods = as.integer(value("f_omit_periods", periods_per_year)),
    stringsAsFactors = FALSE
  )
}

#' Collect MFCL self-test diagnostics
#'
#' Discovers merged `selftest` outputs, retaining the replicate convergence
#' ledger, derived-quantity recovery, key parameter recovery, pseudo-data
#' summaries and the recorded tag-likelihood contract.
#'
#' @param model_dir Root containing expanded Kflow model and self-test outputs.
#' @param provenance Optional Kflow model/self-test job mapping.
#' @return A list of normalized diagnostic tables.
#' @export
collect_selftest_diagnostics <- function(model_dir, provenance = NULL) {
  model_dir <- normalizePath(model_dir, winslash = "/", mustWork = TRUE)
  run_files <- list.files(
    model_dir,
    pattern = "^selftest_runs[.]rds$",
    recursive = TRUE,
    full.names = TRUE
  )
  run_files <- run_files[basename(dirname(run_files)) == "selftest"]
  if (!length(run_files)) {
    stop("No merged selftest/selftest_runs.rds output was found.", call. = FALSE)
  }
  run_rows <- derived_rows <- parameter_rows <- management_rows <-
    simulation_rows <- source_rows <- list()
  for (run_file in run_files) {
    selftest_root <- dirname(run_file)
    model_hint <- basename(dirname(selftest_root))
    prov <- mfclshiny_selftest_provenance_row(provenance, selftest_root, model_hint)
    model_job <- if (!is.null(prov) && "model_job" %in% names(prov)) {
      mfclshiny_jitter_first_text(prov$model_job, default = "")
    } else ""
    model_label <- if (!is.null(prov) && "model_label" %in% names(prov)) {
      mfclshiny_jitter_first_text(prov$model_label, model_hint)
    } else model_hint
    scenario <- if (nzchar(model_job)) paste0("model-", model_job) else model_hint
    runs <- tryCatch(readRDS(run_file), error = function(e) NULL)
    if (!is.data.frame(runs) || !nrow(runs)) next
    if (!"replicate" %in% names(runs)) runs$replicate <- runs$rep
    if (!"run_completed" %in% names(runs)) runs$run_completed <- runs$run_status == "completed"
    if (!"converged" %in% names(runs)) runs$converged <- FALSE
    threshold <- mfclshiny_selftest_grad_reference(selftest_root, runs)
    runs$scenario <- scenario
    runs$model_label <- model_label
    runs$model_job <- model_job
    runs$grad_reference <- threshold
    runs$included <- runs$run_completed %in% TRUE & runs$converged %in% TRUE
    runs$source_file <- normalizePath(run_file, winslash = "/", mustWork = FALSE)
    run_rows[[length(run_rows) + 1L]] <- runs
    derived_rows[[length(derived_rows) + 1L]] <- mfclshiny_selftest_read_derived(
      selftest_root, scenario, model_label
    )
    parameter_rows[[length(parameter_rows) + 1L]] <- mfclshiny_selftest_read_parameters(
      selftest_root, scenario, model_label
    )
    management_rows[[length(management_rows) + 1L]] <- mfclshiny_selftest_read_management(
      selftest_root, scenario, model_label
    )
    simulation_rows[[length(simulation_rows) + 1L]] <- mfclshiny_selftest_read_simulation(
      selftest_root, scenario, model_label
    )
    model_settings <- mfclshiny_selftest_model_settings(dirname(selftest_root))
    check_summary <- tryCatch(
      utils::read.csv(file.path(selftest_root, "check-summary.csv"), stringsAsFactors = FALSE),
      error = function(e) data.frame()
    )
    source_rows[[length(source_rows) + 1L]] <- data.frame(
      scenario = scenario,
      model_label = model_label,
      model_job = model_job,
      selftest_root = normalizePath(selftest_root, winslash = "/", mustWork = FALSE),
      merge_status = if (nrow(check_summary) && "merge_status" %in% names(check_summary)) {
        as.character(check_summary$merge_status[[1L]])
      } else "not recorded",
      expected_replicates = if (nrow(check_summary) && "n_expected_units" %in% names(check_summary)) {
        suppressWarnings(as.integer(check_summary$n_expected_units[[1L]]))
      } else nrow(runs),
      periods_per_year = model_settings$periods_per_year[[1L]],
      sb_recent_years = model_settings$sb_recent_years[[1L]],
      sbf0_recent_years = model_settings$sbf0_recent_years[[1L]],
      sb_recent_uses_native_default = model_settings$sb_recent_uses_native_default[[1L]],
      sbf0_recent_uses_native_default = model_settings$sbf0_recent_uses_native_default[[1L]],
      f_recent_periods = model_settings$f_recent_periods[[1L]],
      f_omit_periods = model_settings$f_omit_periods[[1L]],
      stringsAsFactors = FALSE
    )
  }
  data <- list(
    runs = mfclshiny_selftest_bind_rows(run_rows),
    derived = mfclshiny_selftest_bind_rows(derived_rows),
    parameters = mfclshiny_selftest_bind_rows(parameter_rows),
    management = mfclshiny_selftest_bind_rows(management_rows),
    simulation = mfclshiny_selftest_bind_rows(simulation_rows),
    sources = mfclshiny_selftest_bind_rows(source_rows)
  )
  if (!nrow(data$runs)) stop("No readable self-test replicate ledger was found.", call. = FALSE)
  data
}

mfclshiny_selftest_included_replicates <- function(data, scenario) {
  runs <- data$runs[data$runs$scenario == scenario & data$runs$included %in% TRUE, , drop = FALSE]
  unique(suppressWarnings(as.integer(runs$replicate)))
}

mfclshiny_selftest_quantiles <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) return(rep(NA_real_, 7L))
  stats::quantile(x, c(0.025, 0.10, 0.25, 0.50, 0.75, 0.90, 0.975), names = FALSE)
}

mfclshiny_selftest_derived_band <- function(data, scenario) {
  included <- mfclshiny_selftest_included_replicates(data, scenario)
  x <- data$derived[
    data$derived$scenario == scenario & data$derived$replicate %in% included,
    ,
    drop = FALSE
  ]
  groups <- split(seq_len(nrow(x)), interaction(x$metric, x$year, drop = TRUE))
  rows <- lapply(groups, function(index) {
    z <- x[index, , drop = FALSE]
    q <- mfclshiny_selftest_quantiles(z$estimate)
    data.frame(
      metric = z$metric[[1L]],
      quantity = z$quantity[[1L]],
      year = z$year[[1L]],
      truth = stats::median(z$truth, na.rm = TRUE),
      n = sum(is.finite(z$estimate)),
      q025 = q[[1L]], q10 = q[[2L]], q25 = q[[3L]], median = q[[4L]],
      q75 = q[[5L]], q90 = q[[6L]], q975 = q[[7L]],
      stringsAsFactors = FALSE
    )
  })
  mfclshiny_selftest_bind_rows(rows)
}

#' Plot self-test derived-quantity recovery
#'
#' @param data Output from `collect_selftest_diagnostics()`.
#' @param scenario Scenario key; defaults to the first scenario.
#' @return A ggplot object.
#' @export
plot_selftest_recovery <- function(data, scenario = NULL) {
  if (is.null(scenario)) scenario <- unique(data$runs$scenario)[[1L]]
  band <- mfclshiny_selftest_derived_band(data, scenario)
  if (!nrow(band)) stop("No included self-test recovery data were found.", call. = FALSE)
  units <- mfclshiny_selftest_metric_units()
  metric_order <- c(
    "depletion", "spawning_potential", "spawning_potential_nofish",
    "recruitment", "fishing_mortality",
    "relative_fishing_mortality"
  )
  plots <- lapply(seq_along(metric_order), function(index) {
    metric <- metric_order[[index]]
    z <- band[band$metric == metric, , drop = FALSE]
    ggplot2::ggplot(z, ggplot2::aes(x = year)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = q025, ymax = q975), fill = "#9ecae1", alpha = 0.55) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = q10, ymax = q90), fill = "#4292c6", alpha = 0.44) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = q25, ymax = q75), fill = "#08519c", alpha = 0.34) +
      ggplot2::geom_line(ggplot2::aes(y = median), colour = "#08306b", linewidth = 0.34) +
      ggplot2::geom_line(ggplot2::aes(y = truth), colour = "#c62828", linewidth = 0.36) +
      ggplot2::labs(
        x = if (index > length(metric_order) - 2L) "Year" else NULL,
        y = parse(text = units[[metric]])[[1L]]
      ) +
      ggplot2::theme_bw(base_size = 12.0, base_family = "serif") +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(colour = "#e5eaed", linewidth = 0.25),
        panel.border = ggplot2::element_rect(colour = "#263238", fill = NA, linewidth = 0.42),
        axis.title.y = ggplot2::element_text(size = 10.5),
        plot.margin = ggplot2::margin(6, 8, 6, 6)
      )
  })
  patchwork::wrap_plots(plots, ncol = 2)
}

mfclshiny_retro_recent_settings <- function(source = NULL) {
  value <- function(name, fallback) {
    x <- if (is.data.frame(source) && nrow(source) && name %in% names(source)) {
      suppressWarnings(as.numeric(source[[name]][[1L]]))
    } else NA_real_
    if (!is.finite(x)) fallback else x
  }
  logical_value <- function(name, fallback = FALSE) {
    x <- if (is.data.frame(source) && nrow(source) && name %in% names(source)) {
      source[[name]][[1L]]
    } else fallback
    isTRUE(as.logical(x))
  }
  periods_per_year <- as.integer(value("periods_per_year", 4L))
  if (periods_per_year < 1L) periods_per_year <- 4L
  sb_years <- as.integer(value("sb_recent_years", 4L))
  sbf0_years <- as.integer(value("sbf0_recent_years", 10L))
  if (sb_years < 1L) sb_years <- 4L
  if (sbf0_years < 1L) sbf0_years <- 10L
  f_recent_periods <- as.integer(value("f_recent_periods", 5L * periods_per_year))
  f_omit_periods <- as.integer(value("f_omit_periods", periods_per_year))
  f_years <- (f_recent_periods - f_omit_periods) / periods_per_year
  f_lag_years <- f_omit_periods / periods_per_year
  if (!is.finite(f_years) || !is.finite(f_lag_years) || f_years < 1 ||
      f_lag_years < 0 || f_years != floor(f_years) || f_lag_years != floor(f_lag_years)) {
    f_recent_periods <- 5L * periods_per_year
    f_omit_periods <- periods_per_year
    f_years <- 4L
    f_lag_years <- 1L
  }
  list(
    sb_years = sb_years,
    sbf0_years = sbf0_years,
    sb_uses_native_default = logical_value("sb_recent_uses_native_default"),
    sbf0_uses_native_default = logical_value("sbf0_recent_uses_native_default"),
    periods_per_year = periods_per_year,
    f_recent_periods = f_recent_periods,
    f_omit_periods = f_omit_periods,
    f_years = as.integer(f_years),
    f_lag_years = as.integer(f_lag_years),
    included_f_periods = as.integer(periods_per_year * f_years),
    f_start_offset = as.integer(f_lag_years + f_years - 1L),
    f_end_offset = as.integer(f_lag_years),
    f_period_label = if (periods_per_year == 4L) "quarters" else "model periods"
  )
}

mfclshiny_selftest_recent_settings <- function(data, scenario) {
  source <- if (is.data.frame(data$sources)) {
    data$sources[data$sources$scenario == scenario, , drop = FALSE]
  } else data.frame()
  mfclshiny_retro_recent_settings(source)
}

mfclshiny_selftest_recent_records <- function(data, scenario, years = NULL) {
  years <- mfclshiny_selftest_recent_years(data, scenario, years)
  terminal_year <- max(years)
  settings <- mfclshiny_selftest_recent_settings(data, scenario)
  sb_years <- seq.int(terminal_year - settings$sb_years + 1L, terminal_year)
  if (length(years)) sb_years <- years
  sbf0_years <- seq.int(
    terminal_year - settings$sbf0_years,
    terminal_year - 1L
  )
  f_years <- seq.int(
    terminal_year - settings$f_start_offset,
    terminal_year - settings$f_end_offset
  )
  included <- mfclshiny_selftest_included_replicates(data, scenario)
  x <- data$derived[
    data$derived$scenario == scenario & data$derived$replicate %in% included,
    ,
    drop = FALSE
  ]
  labels <- mfclshiny_selftest_recent_quantity_labels()
  has_aggregate_f <- "relative_fishing_mortality" %in% unique(x$metric)
  if (!has_aggregate_f) labels <- labels[names(labels) != "aggregate_f_recent"]
  groups <- split(seq_len(nrow(x)), x$replicate)
  mfclshiny_selftest_bind_rows(lapply(groups, function(index) {
    z <- x[index, , drop = FALSE]
    series <- function(metric, value, target_years) {
      values <- z[[value]][z$metric == metric & z$year %in% target_years]
      if (length(values) != length(target_years) || any(!is.finite(values))) {
        return(NA_real_)
      }
      mean(values)
    }
    summaries <- function(value) {
      sb <- series("spawning_potential", value, sb_years)
      sbf0_rows <- z[
        z$metric %in% c("spawning_potential", "depletion") &
          z$year %in% sbf0_years,
        c("year", "metric", value),
        drop = FALSE
      ]
      sbf0_wide <- reshape(
        sbf0_rows,
        idvar = "year",
        timevar = "metric",
        direction = "wide"
      )
      sb_col <- paste0(value, ".spawning_potential")
      depletion_col <- paste0(value, ".depletion")
      sbf0 <- if (
        nrow(sbf0_wide) == length(sbf0_years) &&
          all(c(sb_col, depletion_col) %in% names(sbf0_wide)) &&
          all(is.finite(sbf0_wide[[sb_col]])) &&
          all(is.finite(sbf0_wide[[depletion_col]])) &&
          all(sbf0_wide[[depletion_col]] > .Machine$double.eps)
      ) {
        mean(sbf0_wide[[sb_col]] / sbf0_wide[[depletion_col]])
      } else NA_real_
      out <- c(
        recent_depletion = sb / sbf0,
        sb_recent = sb,
        sbf0_recent = sbf0,
        recruitment_recent = series("recruitment", value, sb_years),
        f_diagnostic_recent = series("fishing_mortality", value, f_years)
      )
      if (has_aggregate_f) {
        out <- c(
          out,
          aggregate_f_recent = series(
            "relative_fishing_mortality", value, f_years
          )
        )
      }
      out
    }
    truth <- summaries("truth")
    estimate <- summaries("estimate")
    relative_error <- ifelse(
      is.finite(truth) & abs(truth) > .Machine$double.eps,
      (estimate - truth) / abs(truth),
      NA_real_
    )
    data.frame(
      replicate = z$replicate[[1L]],
      metric = names(labels),
      quantity = unname(labels),
      truth = unname(truth),
      estimate = unname(estimate),
      relative_error = unname(relative_error),
      sb_start = min(sb_years),
      sb_end = max(sb_years),
      sbf0_start = min(sbf0_years),
      sbf0_end = max(sbf0_years),
      f_start = min(f_years),
      f_end = max(f_years),
      stringsAsFactors = FALSE
    )
  }))
}

mfclshiny_selftest_management_records <- function(data, scenario, years = NULL) {
  recent <- mfclshiny_selftest_recent_records(data, scenario, years)
  included <- mfclshiny_selftest_included_replicates(data, scenario)
  native <- if (is.data.frame(data$management)) {
    data$management[
      data$management$scenario == scenario &
        data$management$replicate %in% included,
      ,
      drop = FALSE
    ]
  } else data.frame()
  mfclshiny_selftest_bind_rows(list(recent, native))
}

#' Plot recent-period self-test recovery bias
#'
#' @param data Output from `collect_selftest_diagnostics()`.
#' @param scenario Scenario key.
#' @param years Years averaged within each replicate.
#' @return A ggplot object.
#' @export
plot_selftest_recent_bias <- function(data, scenario = NULL, years = NULL) {
  if (is.null(scenario)) scenario <- unique(data$runs$scenario)[[1L]]
  x <- mfclshiny_selftest_management_records(data, scenario, years)
  quantity_order <- c(
    unname(mfclshiny_selftest_recent_quantity_labels()),
    unname(mfclshiny_selftest_native_management_labels())
  )
  x$quantity <- factor(
    x$quantity,
    levels = quantity_order
  )
  quantity_labels <- list(
    bquote(SB[recent]/SB[F == 0]),
    bquote(SB[recent]),
    bquote(SB[F == 0]),
    bquote(R[recent]),
    bquote(bar(F)),
    bquote("mean"~F/F[MSY]),
    "MSY",
    bquote(F[MSY]),
    bquote(B[MSY]^S),
    bquote(F[recent]/F[MSY])
  )
  names(quantity_labels) <- quantity_order
  ggplot2::ggplot(x, ggplot2::aes(x = quantity, y = 100 * relative_error)) +
    ggplot2::geom_hline(yintercept = c(-5, 5), colour = "#82919a", linewidth = 0.30, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, colour = "#c62828", linewidth = 0.38) +
    ggplot2::geom_boxplot(width = 0.56, outlier.shape = NA, fill = "#b9dce5", colour = "#123b5d") +
    ggplot2::geom_jitter(width = 0.12, height = 0, size = 1.45, alpha = 0.58, colour = "#087f8c") +
    ggplot2::scale_x_discrete(labels = quantity_labels) +
    ggplot2::labs(x = NULL, y = "Relative recovery error (%)") +
    ggplot2::theme_bw(base_size = 12.5, base_family = "serif") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 11),
      panel.border = ggplot2::element_rect(colour = "#263238", fill = NA, linewidth = 0.42)
    )
}

#' Plot key-parameter self-test recovery
#'
#' @param data Output from `collect_selftest_diagnostics()`.
#' @param scenario Scenario key.
#' @return A ggplot object.
#' @export
plot_selftest_parameter_recovery <- function(data, scenario = NULL) {
  if (is.null(scenario)) scenario <- unique(data$runs$scenario)[[1L]]
  included <- mfclshiny_selftest_included_replicates(data, scenario)
  x <- data$parameters[
    data$parameters$scenario == scenario & data$parameters$replicate %in% included,
    ,
    drop = FALSE
  ]
  if (!nrow(x)) stop("No key-parameter recovery data were found.", call. = FALSE)
  order <- unique(c(
    unname(mfclshiny_selftest_key_parameter_labels()),
    unname(mfclshiny_selftest_parameter_labels())
  ))
  x$parameter_label <- factor(x$parameter_label, levels = rev(order))
  ggplot2::ggplot(x, ggplot2::aes(x = 100 * relative_error, y = parameter_label)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#c62828", linewidth = 0.38) +
    ggplot2::geom_boxplot(width = 0.56, outlier.shape = NA, fill = "#c7dfcf", colour = "#123b5d") +
    ggplot2::geom_jitter(width = 0, height = 0.12, size = 1.35, alpha = 0.52, colour = "#24784f") +
    ggplot2::labs(x = "Relative parameter error (%)", y = NULL) +
    ggplot2::theme_bw(base_size = 12.2, base_family = "serif") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "#263238", fill = NA, linewidth = 0.42)
  )
}

#' Plot key self-test recovery for assessment quantities and parameters
#'
#' @param data Output from `collect_selftest_diagnostics()`.
#' @param scenario Scenario key.
#' @param years Years averaged for the assessment-quantity panel.
#' @return A ggplot object.
#' @export
plot_selftest_key_recovery <- function(data, scenario = NULL, years = NULL) {
  if (is.null(scenario)) scenario <- unique(data$runs$scenario)[[1L]]
  years <- mfclshiny_selftest_recent_years(data, scenario, years)
  included <- mfclshiny_selftest_included_replicates(data, scenario)

  quantities <- mfclshiny_selftest_management_records(data, scenario, years)
  quantities <- quantities[
    quantities$quantity %in% mfclshiny_selftest_report_quantities(),
    ,
    drop = FALSE
  ]
  quantities <- quantities[, c("replicate", "quantity", "relative_error"), drop = FALSE]
  names(quantities)[names(quantities) == "quantity"] <- "label"
  quantities$group <- "Management quantities"

  parameters <- data$parameters[
    data$parameters$scenario == scenario & data$parameters$replicate %in% included,
    c("replicate", "parameter_label", "relative_error"),
    drop = FALSE
  ]
  names(parameters)[names(parameters) == "parameter_label"] <- "label"
  parameters$group <- "Selected parameters"

  x <- mfclshiny_selftest_bind_rows(list(quantities, parameters))
  if (!nrow(x)) stop("No key recovery data were found.", call. = FALSE)
  quantity_order <- c(
    unname(mfclshiny_selftest_recent_quantity_labels()),
    unname(mfclshiny_selftest_native_management_labels())
  )
  parameter_order <- unique(c(
    unname(mfclshiny_selftest_key_parameter_labels()),
    unname(mfclshiny_selftest_parameter_labels())
  ))
  x$label <- factor(x$label, levels = rev(c(quantity_order, parameter_order)))
  x$group <- factor(
    x$group,
    levels = c("Management quantities", "Selected parameters")
  )
  compact_labels <- list(
    bquote(SB[recent]/SB[F == 0]),
    bquote(SB[recent]),
    bquote(SB[F == 0]),
    bquote(R[recent]),
    bquote(bar(F)),
    bquote("mean"~F/F[MSY]),
    "MSY",
    bquote(F[MSY]),
    bquote(B[MSY]^S),
    bquote(F[recent]/F[MSY]),
    bquote(ln(R[0])),
    "SR density dependence",
    bquote(L[1]~"(youngest)"),
    bquote(L[2]~"(oldest)"),
    bquote(K~"(growth)"),
    bquote(s[1]~"(mean SD)"),
    bquote(s[2]~"(SD age trend)")
  )
  names(compact_labels) <- c(
    quantity_order,
    unname(mfclshiny_selftest_key_parameter_labels())
  )

  ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = 100 * relative_error,
      y = label,
      fill = group,
      colour = group
    )
  ) +
    ggplot2::geom_vline(xintercept = c(-5, 5), colour = "#82919a", linewidth = 0.30, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, colour = "#c62828", linewidth = 0.38) +
    ggplot2::geom_boxplot(
      width = 0.56, outlier.shape = NA,
      linewidth = 0.42, alpha = 0.72
    ) +
    ggplot2::geom_jitter(
      width = 0, height = 0.11, size = 1.25,
      alpha = 0.5, show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = c("#b9dce5", "#c7dfcf"), guide = "none") +
    ggplot2::scale_colour_manual(values = c("#087f8c", "#24784f"), guide = "none") +
    ggplot2::scale_y_discrete(labels = compact_labels) +
    ggplot2::facet_wrap(
      ggplot2::vars(group),
      nrow = 1L,
      scales = "free_y",
      strip.position = "top"
    ) +
    ggplot2::labs(x = "Relative recovery error (%)", y = NULL) +
    ggplot2::theme_bw(base_size = 12.2, base_family = "serif") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.spacing.y = grid::unit(0.8, "lines"),
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "#263238", fill = NA, linewidth = 0.42)
    )
}

#' Plot self-test pseudo-data simulation checks
#'
#' @param data Output from `collect_selftest_diagnostics()`.
#' @param scenario Scenario key.
#' @return A ggplot object.
#' @export
plot_selftest_simulation <- function(data, scenario = NULL) {
  if (is.null(scenario)) scenario <- unique(data$runs$scenario)[[1L]]
  included <- mfclshiny_selftest_included_replicates(data, scenario)
  x <- data$simulation[
    data$simulation$scenario == scenario & data$simulation$replicate %in% included,
    ,
    drop = FALSE
  ]
  if (!nrow(x)) stop("No pseudo-data simulation summaries were found.", call. = FALSE)
  groups <- split(seq_len(nrow(x)), interaction(x$component, x$year, drop = TRUE))
  band <- mfclshiny_selftest_bind_rows(lapply(groups, function(index) {
    z <- x[index, , drop = FALSE]
    q <- mfclshiny_selftest_quantiles(z$pseudo)
    data.frame(
      component = z$component[[1L]],
      year = z$year[[1L]],
      reference = stats::median(z$reference, na.rm = TRUE),
      q025 = q[[1L]], median = q[[4L]], q975 = q[[7L]],
      stringsAsFactors = FALSE
    )
  }))
  component_labels <- c(
    CPUE = "CPUE~index",
    `length mean` = "Mean~length~(cm)",
    `age-length mean age` = "Mean~age~(years)",
    `tag recaptures by region` = "Tag~recaptures~(count)"
  )
  time_plots <- lapply(seq_along(component_labels), function(index) {
    component <- names(component_labels)[[index]]
    z <- band[band$component == component, , drop = FALSE]
    ggplot2::ggplot(z, ggplot2::aes(x = year)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = q025, ymax = q975), fill = "#9ecae1", alpha = 0.36) +
      ggplot2::geom_line(ggplot2::aes(y = median), colour = "#08306b", linewidth = 0.42) +
      ggplot2::geom_line(ggplot2::aes(y = reference), colour = "#c62828", linewidth = 0.32, linetype = "longdash") +
      ggplot2::labs(
        x = if (index == length(component_labels)) "Year" else NULL,
        y = parse(text = component_labels[[component]])[[1L]]
      ) +
      ggplot2::theme_bw(base_size = 11.5, base_family = "serif") +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(colour = "#e5eaed", linewidth = 0.25),
        panel.border = ggplot2::element_rect(colour = "#263238", fill = NA, linewidth = 0.42),
        axis.title.y = ggplot2::element_text(size = 9.8)
      )
  })
  time_plot <- patchwork::wrap_plots(time_plots, ncol = 1)
  bias_groups <- split(seq_len(nrow(x)), interaction(x$component, x$replicate, drop = TRUE))
  bias <- mfclshiny_selftest_bind_rows(lapply(bias_groups, function(index) {
    z <- x[index, , drop = FALSE]
    denominator <- sum(abs(z$reference), na.rm = TRUE)
    data.frame(
      component = z$component[[1L]],
      replicate = z$replicate[[1L]],
      bias = if (is.finite(denominator) && denominator > .Machine$double.eps) {
        100 * sum(z$pseudo - z$reference, na.rm = TRUE) / denominator
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  bias <- bias[is.finite(bias$bias), , drop = FALSE]
  bias$component <- factor(
    bias$component,
    levels = names(component_labels),
    labels = c("CPUE", "Mean length", "Mean age-at-length", "Post-mixing tags")
  )
  medians <- stats::aggregate(bias ~ component, bias, stats::median)
  medians$label <- paste0("Median ", sprintf("%+.2f%%", medians$bias))
  label_height <- max(bias$bias, na.rm = TRUE)
  medians$label_height <- label_height + max(0.25, diff(range(bias$bias, na.rm = TRUE)) * 0.08)
  bias_plot <- ggplot2::ggplot(bias, ggplot2::aes(x = component, y = bias)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#c62828", linewidth = 0.38) +
    ggplot2::geom_boxplot(width = 0.48, outlier.shape = NA, fill = "#c7dfcf", colour = "#123b5d") +
    ggplot2::geom_jitter(width = 0.11, height = 0, size = 1.25, alpha = 0.52, colour = "#24784f") +
    ggplot2::geom_label(
      data = medians,
      ggplot2::aes(x = component, y = label_height, label = label),
      inherit.aes = FALSE,
      family = "serif",
      size = 3.0,
      linewidth = 0.22,
      label.padding = grid::unit(0.14, "lines"),
      colour = "#123b5d",
      fill = "white"
    ) +
    ggplot2::labs(x = NULL, y = "Whole-series simulation bias (%)") +
    ggplot2::theme_bw(base_size = 12.2, base_family = "serif") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "#263238", fill = NA, linewidth = 0.42)
    )
  patchwork::wrap_plots(time_plot, bias_plot, ncol = 1, heights = c(3.2, 1.15))
}

mfclshiny_selftest_simulation_errors <- function(data, scenario) {
  included <- mfclshiny_selftest_included_replicates(data, scenario)
  x <- data$simulation[
    data$simulation$scenario == scenario & data$simulation$replicate %in% included,
    ,
    drop = FALSE
  ]
  if (!nrow(x)) return(data.frame())
  groups <- split(seq_len(nrow(x)), interaction(x$component, x$replicate, drop = TRUE))
  mfclshiny_selftest_bind_rows(lapply(groups, function(index) {
    z <- x[index, , drop = FALSE]
    denominator <- sum(abs(z$reference), na.rm = TRUE)
    data.frame(
      component = z$component[[1L]],
      replicate = z$replicate[[1L]],
      relative_error = if (is.finite(denominator) && denominator > .Machine$double.eps) {
        sum(z$pseudo - z$reference, na.rm = TRUE) / denominator
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

mfclshiny_selftest_simulation_table <- function(data, scenario) {
  x <- mfclshiny_selftest_simulation_errors(data, scenario)
  if (!nrow(x)) return(data.frame())
  labels <- c(
    CPUE = "CPUE",
    `length mean` = "Mean observed length",
    `age-length mean age` = "Mean age at length",
    `tag recaptures by region` = "Post-mixing tag recaptures"
  )
  groups <- split(seq_len(nrow(x)), x$component)
  out <- mfclshiny_selftest_bind_rows(lapply(groups, function(index) {
    z <- x[index, , drop = FALSE]
    error <- z$relative_error[is.finite(z$relative_error)]
    q <- if (length(error)) {
      stats::quantile(error, c(0.025, 0.5, 0.975), names = FALSE)
    } else rep(NA_real_, 3L)
    data.frame(
      Component = unname(labels[[z$component[[1L]]]]),
      Replicates = length(error),
      `Median error (%)` = round(100 * q[[2L]], 2),
      `95% empirical range (%)` = if (length(error)) {
        paste0(
          formatC(100 * q[[1L]], format = "f", digits = 2),
          " to ",
          formatC(100 * q[[3L]], format = "f", digits = 2)
        )
      } else "",
      `Relative RMSE (%)` = if (length(error)) {
        round(100 * sqrt(mean(error^2)), 2)
      } else NA_real_,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
  out[order(match(out$Component, unname(labels))), , drop = FALSE]
}

mfclshiny_selftest_recovery_envelope_summary <- function(data, scenario) {
  band <- mfclshiny_selftest_derived_band(data, scenario)
  if (!nrow(band)) {
    return(list(included = 0L, total = 0L, percent = NA_real_))
  }
  inside <- is.finite(band$truth) & is.finite(band$q025) & is.finite(band$q975) &
    band$truth >= band$q025 & band$truth <= band$q975
  eligible <- is.finite(band$truth) & is.finite(band$q025) & is.finite(band$q975)
  list(
    included = sum(inside & eligible),
    total = sum(eligible),
    percent = if (any(eligible)) 100 * mean(inside[eligible]) else NA_real_
  )
}

mfclshiny_selftest_run_table <- function(runs) {
  runs <- runs[order(suppressWarnings(as.integer(runs$replicate))), , drop = FALSE]
  data.frame(
    Replicate = suppressWarnings(as.integer(runs$replicate)),
    Seed = suppressWarnings(as.integer(runs$seed)),
    `Run status` = as.character(runs$run_status),
    Converged = ifelse(runs$converged %in% TRUE, "Yes", "No"),
    `Objective function value` = suppressWarnings(as.numeric(runs$obj_fun)),
    MGC = abs(suppressWarnings(as.numeric(runs$max_grad))),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

mfclshiny_selftest_display_number <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep("", length(x))
  ok <- is.finite(x)
  if (!any(ok)) return(out)
  digits <- ifelse(abs(x[ok]) < 1, 3L, ifelse(abs(x[ok]) < 1000, 1L, 0L))
  out[ok] <- mapply(
    function(value, n_digits) {
      formatC(value, format = "f", digits = n_digits, big.mark = ",", preserve.width = "none")
    },
    x[ok], digits,
    USE.NAMES = FALSE
  )
  out
}

mfclshiny_selftest_recovery_table <- function(data, scenario, years = NULL) {
  years <- mfclshiny_selftest_recent_years(data, scenario, years)
  x <- mfclshiny_selftest_management_records(data, scenario, years)
  retained <- mfclshiny_selftest_report_quantities()
  x <- x[x$quantity %in% retained, , drop = FALSE]
  groups <- split(seq_len(nrow(x)), x$quantity)
  rows <- lapply(groups, function(index) {
    z <- x[index, , drop = FALSE]
    error <- z$relative_error[is.finite(z$relative_error)]
    q <- if (length(error)) stats::quantile(error, c(0.025, 0.5, 0.975), names = FALSE) else rep(NA_real_, 3L)
    data.frame(
      Quantity = z$quantity[[1L]],
      Replicates = length(error),
      Truth = mfclshiny_selftest_display_number(stats::median(z$truth, na.rm = TRUE)),
      `Median refit` = mfclshiny_selftest_display_number(stats::median(z$estimate, na.rm = TRUE)),
      `Median relative bias (%)` = round(100 * q[[2L]], 1),
      `95% empirical range (%)` = if (length(error)) {
        paste0(
          formatC(100 * q[[1L]], format = "f", digits = 1),
          " to ",
          formatC(100 * q[[3L]], format = "f", digits = 1)
        )
      } else "",
      `Relative RMSE (%)` = round(100 * sqrt(mean(error^2)), 1),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- mfclshiny_selftest_bind_rows(rows)
  position <- match(
    out$Quantity,
    c(
      unname(mfclshiny_selftest_recent_quantity_labels()),
      unname(mfclshiny_selftest_native_management_labels())
    )
  )
  out[base::order(position), , drop = FALSE]
}

mfclshiny_selftest_parameter_table <- function(data, scenario) {
  included <- mfclshiny_selftest_included_replicates(data, scenario)
  x <- data$parameters[
    data$parameters$scenario == scenario & data$parameters$replicate %in% included,
    ,
    drop = FALSE
  ]
  groups <- split(seq_len(nrow(x)), x$parameter_label)
  out <- mfclshiny_selftest_bind_rows(lapply(groups, function(index) {
    error <- x$relative_error[index]
    error <- error[is.finite(error)]
    q <- stats::quantile(error, c(0.025, 0.5, 0.975), names = FALSE)
    truth <- suppressWarnings(as.numeric(x$truth[index]))
    estimate <- suppressWarnings(as.numeric(x$estimate[index]))
    data.frame(
      Parameter = x$parameter_label[index[[1L]]],
      Replicates = length(error),
      Truth = mfclshiny_selftest_display_number(stats::median(truth, na.rm = TRUE)),
      `Median refit` = mfclshiny_selftest_display_number(stats::median(estimate, na.rm = TRUE)),
      `Median relative error (%)` = round(100 * q[[2L]], 2),
      `95% empirical range (%)` = paste0(
        formatC(100 * q[[1L]], format = "f", digits = 2),
        " to ",
        formatC(100 * q[[3L]], format = "f", digits = 2)
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
  key_order <- unique(c(
    unname(mfclshiny_selftest_key_parameter_labels()),
    unname(mfclshiny_selftest_parameter_labels())
  ))
  out[order(match(out$Parameter, key_order)), , drop = FALSE]
}

mfclshiny_selftest_contract_table <- function(runs) {
  unique_text <- function(name, fallback = "not recorded") {
    if (!name %in% names(runs)) return(fallback)
    value <- unique(trimws(as.character(runs[[name]])))
    value <- value[!is.na(value) & nzchar(value)]
    if (length(value)) paste(value, collapse = "; ") else fallback
  }
  matched <- if ("tag_likelihood_matched" %in% names(runs)) {
    paste0(sum(runs$tag_likelihood_matched %in% TRUE), " / ", nrow(runs))
  } else "not recorded"
  simulation_mode <- unique_text("tag_simulation_mode")
  if (identical(simulation_mode, "conditional_postmixing")) {
    simulation_mode <- "Post-mixing recapture observations"
  }
  likelihood_family <- unique_text("tag_likelihood_family")
  if (identical(likelihood_family, "negative_binomial_mean_scaled_shape")) {
    likelihood_family <- "Negative binomial (mean-scaled shape)"
  }
  contract_status <- unique_text("tag_contract_status")
  if (identical(contract_status, "conditional_postmixing_refit_completed")) {
    contract_status <- "Conditional post-mixing refit completed"
  }
  bootstrap_scope <- unique_text("tag_bootstrap_scope")
  if (identical(bootstrap_scope, "latent_age_postmixing_conditional_on_empirical_premixing")) {
    bootstrap_scope <- "Post-mixing observations simulated; empirical pre-mixing observations conditioned"
  }
  tag_cells <- suppressWarnings(as.numeric(unique_text("tag_cell_count", fallback = NA_character_)))
  tag_cells <- if (is.finite(tag_cells)) {
    formatC(tag_cells, format = "f", digits = 0, big.mark = ",", preserve.width = "none")
  } else "not recorded"
  data.frame(
    Item = c(
      "Simulated tag observations", "Observation model", "Likelihood matched",
      "Validation status", "Pre-mixing treatment", "Simulated likelihood cells"
    ),
    Value = c(
      simulation_mode,
      likelihood_family,
      matched,
      contract_status,
      bootstrap_scope,
      tag_cells
    ),
    stringsAsFactors = FALSE
  )
}

mfclshiny_selftest_results_text <- function(data, scenario, years = NULL) {
  years <- mfclshiny_selftest_recent_years(data, scenario, years)
  terminal_year <- max(years)
  runs <- data$runs[data$runs$scenario == scenario, , drop = FALSE]
  included <- runs[runs$included %in% TRUE, , drop = FALSE]
  recent <- mfclshiny_selftest_recovery_table(data, scenario, years)
  mgc <- abs(suppressWarnings(as.numeric(included$max_grad)))
  mgc <- mgc[is.finite(mgc)]
  bias <- recent[["Median relative bias (%)"]]
  names(bias) <- tolower(as.character(recent$Quantity))
  simulation <- mfclshiny_selftest_simulation_table(data, scenario)
  simulation_sentence <- if (nrow(simulation)) {
    paste0(
      "Median whole-series pseudo-data error was ",
      paste0(
        tolower(simulation$Component), " ",
        formatC(simulation[["Median error (%)"]], format = "f", digits = 2), "%",
        collapse = "; "
      ),
      ". "
    )
  } else ""
  envelope <- mfclshiny_selftest_recovery_envelope_summary(data, scenario)
  envelope_sentence <- if (envelope$total > 0L) {
    paste0(
      "The generating truth was inside the pointwise central 95% empirical refit range at ",
      envelope$included, " of ", envelope$total, " annual quantity-year points (",
      formatC(envelope$percent, format = "f", digits = 1), "%). "
    )
  } else ""
  parameter_summary <- mfclshiny_selftest_parameter_table(data, scenario)
  parameter_error <- suppressWarnings(as.numeric(parameter_summary[["Median relative error (%)"]]))
  parameter_error <- parameter_error[is.finite(parameter_error)]
  parameter_sentence <- if (length(parameter_error)) {
    paste0(
      "Across the selected recruitment-scale, growth and stock-recruit parameters, median relative errors ranged from ",
      formatC(min(parameter_error), format = "f", digits = 2), "% to ",
      formatC(max(parameter_error), format = "f", digits = 2), "%. "
    )
  } else ""
  paste0(
    nrow(included), " of ", nrow(runs), " pseudo-data refits completed and met the recorded convergence criterion. ",
    if (length(mgc)) paste0(
      "MGC ranged from ", formatC(min(mgc), format = "e", digits = 2),
      " to ", formatC(max(mgc), format = "e", digits = 2), ". "
    ) else "",
    simulation_sentence,
    envelope_sentence,
    "For the recent management and assessment summaries ending in ", terminal_year,
    ", median relative recovery bias was ",
    paste0(names(bias), " ", formatC(bias, format = "f", digits = 1), "%", collapse = "; "),
    ". ",
    parameter_sentence,
    "These are empirical simulation-estimation diagnostics under the configured generator, not confidence intervals for the assessment."
  )
}

mfclshiny_selftest_figure_caption <- function(kind, model_name, included, total, years = NULL) {
  period_label <- mfclshiny_selftest_year_label(years)
  terminal_year <- if (length(years)) max(years) else "the terminal year"
  switch(
    kind,
    recovery = paste0(
      "Self-test recovery of annual assessment quantities for ", model_name, ". The red line is the generating truth; ",
      "the dark-blue line is the median refit across ", included, " successful replicates, and the nested blue bands are the ",
      "central 50%, 80%, and 95% empirical ranges. ", included, " of ", total,
      " replicates are included; unsuccessful or non-converged replicates are excluded. Dynamic depletion is ",
      "SB_t/SB_{F=0,t}, and the no-fishing spawning-potential panel shows its time-specific denominator."
    ),
    recent = paste0(
      "Relative recovery error for recent management and assessment summaries ending in ", terminal_year,
      " in ", model_name,
      ". Points are successful pseudo-data replicates, boxes show their empirical distribution, and the red line marks zero bias."
    ),
    parameters = paste0(
      "Relative recovery error for key estimated parameters in ", model_name,
      ". Points are successful pseudo-data refits; the red line marks exact recovery. Fixed natural mortality is not treated as an estimated recovery target."
    ),
    key = paste0(
      "Relative recovery error for recent management and assessment quantities ending in ", terminal_year,
      " and for selected estimated parameters in ", model_name,
      ". The left panel includes recent depletion, recent and no-fishing spawning potential, recent recruitment, ",
      "MSY, ", "fishing mortality and equilibrium spawning biomass at MSY, and recent fishing mortality relative to MSY. ",
      "Points are successful pseudo-data refits, boxes show their empirical distributions, and the red line marks exact recovery. ",
      "Grey dashed lines mark relative recovery errors of -5% and +5%. ",
      "Unavailable native quantities are omitted rather than approximated; fixed natural mortality and high-dimensional nuisance deviations are not included."
    ),
    simulation = paste0(
      "Pseudo-data generation checks for ", model_name,
      ". The dark-blue line is the median across successful pseudo-data replicates, the blue band is the central 95% empirical range, ",
      "and the dashed red line is the generating expectation. CPUE, mean length and mean age-at-length are sample-weighted across series; ",
      "post-mixing tag recaptures are summed across regions. The lower panel shows whole-series relative error for each replicate."
    )
  )
}

mfclshiny_selftest_html_math <- function(x) {
  replacements <- c(
    "SB_t/SB_{F=0,t}" = '<span class="math-inline"><i>SB</i><sub><i>t</i></sub>/<i>SB</i><sub><i>F</i>=0,<i>t</i></sub></span>',
    "Recent depletion (SB_recent/SB_F=0)" = 'Recent depletion (<span class="math-inline"><i>SB</i><sub>recent</sub>/<i>SB</i><sub><i>F</i>=0</sub></span>)',
    "Recent spawning potential (SB_recent)" = 'Recent spawning potential (<span class="math-inline"><i>SB</i><sub>recent</sub></span>)',
    "No-fishing spawning potential (SB_F=0)" = 'No-fishing spawning potential (<span class="math-inline"><i>SB</i><sub><i>F</i>=0</sub></span>)',
    "Recent recruitment (R_recent)" = 'Recent recruitment (<span class="math-inline"><i>R</i><sub>recent</sub></span>)',
    "Recent annual-F diagnostic (mean F)" = 'Recent annual-<i>F</i> diagnostic (<span class="math-inline"><span style="text-decoration:overline"><i>F</i></span></span>)',
    "Recent aggregate fishing mortality (mean F/FMSY)" = 'Recent aggregate fishing mortality (<span class="math-inline"><span style="text-decoration:overline"><i>F</i>/<i>F</i><sub>MSY</sub></span></span>)',
    "Maximum sustainable yield (MSY)" = 'Maximum sustainable yield (<span class="math-inline">MSY</span>)',
    "Fishing mortality at MSY (F_MSY)" = 'Fishing mortality at MSY (<span class="math-inline"><i>F</i><sub>MSY</sub></span>)',
    "Equilibrium spawning biomass at MSY (B^S_MSY)" = 'Equilibrium spawning biomass at MSY (<span class="math-inline"><i>B</i><sup>S</sup><sub>MSY</sub></span>)',
    "Recent fishing mortality relative to MSY (F_recent/F_MSY)" = 'Recent fishing mortality relative to MSY (<span class="math-inline"><i>F</i><sub>recent</sub>/<i>F</i><sub>MSY</sub></span>)',
    "SB_recent/SB_F=0" = '<span class="math-inline"><i>SB</i><sub>recent</sub>/<i>SB</i><sub><i>F</i>=0</sub></span>',
    "SB_recent" = '<span class="math-inline"><i>SB</i><sub>recent</sub></span>',
    "SB_F=0" = '<span class="math-inline"><i>SB</i><sub><i>F</i>=0</sub></span>',
    "R_recent" = '<span class="math-inline"><i>R</i><sub>recent</sub></span>',
    "Depletion (SB/SB(F=0))" = 'Depletion (<span class="math-inline"><i>SB</i>/<i>SB</i><sub><i>F</i>=0</sub></span>)',
    "Spawning potential (SB)" = 'Spawning potential (<span class="math-inline"><i>SB</i></span>)',
    "Recruitment (R)" = 'Recruitment (<span class="math-inline"><i>R</i></span>)',
    "Annual fishing mortality (F)" = 'Annual fishing mortality (<span class="math-inline"><i>F</i></span>)',
    "Aggregate fishing mortality (F/FMSY)" = 'Aggregate fishing mortality (<span class="math-inline"><i>F</i>/<i>F</i><sub>MSY</sub></span>)',
    "SB/SB(F=0)" = '<span class="math-inline"><i>SB</i>/<i>SB</i><sub><i>F</i>=0</sub></span>',
    "F_recent/F_MSY" = '<span class="math-inline"><i>F</i><sub>recent</sub>/<i>F</i><sub>MSY</sub></span>',
    "Log recruitment scale (ln R0)" = 'Log recruitment scale (ln <i>R</i><sub>0</sub>)',
    "Mean SD of length-at-age (s1)" = 'Mean SD of length-at-age (<i>s</i><sub>1</sub>)',
    "Age trend in length-at-age SD (s2)" = 'Age trend in length-at-age SD (<i>s</i><sub>2</sub>)',
    "(L1)" = '(<i>L</i><sub>1</sub>)',
    "(L2)" = '(<i>L</i><sub>2</sub>)',
    "(K)" = '(<i>K</i>)',
    "(s1)" = '(<i>s</i><sub>1</sub>)',
    "(s2)" = '(<i>s</i><sub>2</sub>)'
  )
  out <- as.character(x)
  for (key in names(replacements)) out <- gsub(key, replacements[[key]], out, fixed = TRUE)
  out
}

mfclshiny_selftest_latex_math <- function(x) {
  replacements <- c(
    "SB\\_t/SB\\_\\{F=0,t\\}" = "$\\mathrm{SB}_{t}/\\mathrm{SB}_{F=0,t}$",
    "Recent depletion (SB\\_recent/SB\\_F=0)" = "Recent depletion ($\\mathrm{SB}_{\\mathrm{recent}}/\\allowbreak\\mathrm{SB}_{F=0}$)",
    "Recent spawning potential (SB\\_recent)" = "Recent spawning potential ($\\mathrm{SB}_{\\mathrm{recent}}$)",
    "No-fishing spawning potential (SB\\_F=0)" = "No-fishing spawning potential ($\\mathrm{SB}_{F=0}$)",
    "Recent recruitment (R\\_recent)" = "Recent recruitment ($R_{\\mathrm{recent}}$)",
    "Recent annual-F diagnostic (mean F)" = "Recent annual-$F$ diagnostic ($\\overline{F}$)",
    "Recent aggregate fishing mortality (mean F/FMSY)" = "Recent aggregate fishing mortality ($\\overline{F/F_{\\mathrm{MSY}}}$)",
    "Maximum sustainable yield (MSY)" = "Maximum sustainable yield (MSY)",
    "Fishing mortality at MSY (F\\_MSY)" = "Fishing mortality at MSY ($F_{\\mathrm{MSY}}$)",
    "Equilibrium spawning biomass at MSY (B^S\\_MSY)" = "Equilibrium spawning biomass at MSY ($B^{S}_{\\mathrm{MSY}}$)",
    "Recent fishing mortality relative to MSY (F\\_recent/F\\_MSY)" = "Recent fishing mortality relative to MSY ($F_{\\mathrm{recent}}/F_{\\mathrm{MSY}}$)",
    "SB\\_recent/SB\\_F=0" = "$\\mathrm{SB}_{\\mathrm{recent}}/\\allowbreak\\mathrm{SB}_{F=0}$",
    "SB\\_recent" = "$\\mathrm{SB}_{\\mathrm{recent}}$",
    "SB\\_F=0" = "$\\mathrm{SB}_{F=0}$",
    "R\\_recent" = "$R_{\\mathrm{recent}}$",
    "Depletion (SB/SB(F=0))" = "Depletion ($\\mathrm{SB}/\\mathrm{SB}_{F=0}$)",
    "Spawning potential (SB)" = "Spawning potential ($\\mathrm{SB}$)",
    "Recruitment (R)" = "Recruitment ($R$)",
    "Annual fishing mortality (F)" = "Annual fishing mortality ($F$)",
    "Aggregate fishing mortality (F/FMSY)" = "Aggregate fishing mortality ($F/F_{\\mathrm{MSY}}$)",
    "SB/SB(F=0)" = "$\\mathrm{SB}/\\mathrm{SB}_{F=0}$",
    "F\\_recent/F\\_MSY" = "$F_{\\mathrm{recent}}/F_{\\mathrm{MSY}}$",
    "Log recruitment scale (ln R0)" = "Log recruitment scale ($\\ln R_0$)",
    "Mean SD of length-at-age (s1)" = "Mean SD of length-at-age ($s_1$)",
    "Age trend in length-at-age SD (s2)" = "Age trend in length-at-age SD ($s_2$)",
    "(L1)" = "($L_1$)",
    "(L2)" = "($L_2$)",
    "(K)" = "($K$)",
    "(s1)" = "($s_1$)",
    "(s2)" = "($s_2$)"
  )
  out <- as.character(x)
  for (key in names(replacements)) out <- gsub(key, replacements[[key]], out, fixed = TRUE)
  out
}

mfclshiny_selftest_html_section <- function(title, image_file, image_id, image_name, caption, latex_id) {
  caption_html <- mfclshiny_selftest_html_math(mfclshiny_jitter_html_escape(caption))
  caption_latex <- mfclshiny_selftest_latex_math(
    mfclshiny_jitter_latex_prose(caption)
  )
  paste0(
    '<div class="figure-block"><h3>', mfclshiny_jitter_html_escape(title), '</h3>',
    '<img id="', image_id, '" class="figure" src="', mfclshiny_jitter_image_data(image_file),
    '" alt="', mfclshiny_jitter_html_escape(title), '">',
    '<figcaption id="caption-', image_id, '"><strong>Figure <span class="figure-number" contenteditable="true" spellcheck="false">XX</span>.</strong> ',
    caption_html, '</figcaption>',
    '<pre id="', latex_id, '" class="copy-source">\\caption{',
    mfclshiny_jitter_html_escape(caption_latex),
    '}</pre><div class="actions"><button onclick="copyFigure(\'', image_id, '\',\'caption-', image_id,
    '\',this)">Copy figure for Word</button><button onclick="saveImage(\'', image_id, '\',\'',
    image_name, '\',this)">Save PNG</button><button onclick="copyText(\'', latex_id,
    '\',this)">Copy LaTeX caption</button></div></div>'
  )
}

mfclshiny_selftest_write_html <- function(file, data, images, table_dir, title, recent_years = NULL) {
  scenarios <- unique(data$runs$scenario)
  tab_buttons <- vapply(scenarios, function(scenario) {
    runs <- data$runs[data$runs$scenario == scenario, , drop = FALSE]
    model_name <- mfclshiny_jitter_first_text(runs$model_label, scenario)
    paste0(
      '<button class="tab-button" onclick="showTab(\'tab-', mfclshiny_jitter_slug(scenario),
      '\',this)">', mfclshiny_jitter_html_escape(model_name), '</button>'
    )
  }, character(1L))
  model_sections <- vapply(scenarios, function(scenario) {
    runs <- data$runs[data$runs$scenario == scenario, , drop = FALSE]
    model_name <- sub(
      " fitted model$", "",
      mfclshiny_jitter_first_text(runs$model_label, scenario),
      ignore.case = TRUE
    )
    slug <- mfclshiny_jitter_slug(scenario)
    included <- sum(runs$included %in% TRUE)
    total <- nrow(runs)
    years <- mfclshiny_selftest_recent_years(data, scenario, recent_years)
    settings <- mfclshiny_selftest_recent_settings(data, scenario)
    terminal_year <- max(years)
    window_text <- paste0(
      "SB_recent ", mfclshiny_selftest_year_label(years), "; ",
      "SB_F=0 ", mfclshiny_selftest_year_label(seq.int(
        terminal_year - settings$sbf0_years, terminal_year - 1L
      )), "; annual-F diagnostic ", mfclshiny_selftest_year_label(seq.int(
        terminal_year - settings$f_start_offset,
        terminal_year - settings$f_end_offset
      ))
    )
    run_table <- mfclshiny_selftest_run_table(runs)
    recovery_table <- mfclshiny_selftest_recovery_table(data, scenario, years)
    parameter_table <- mfclshiny_selftest_parameter_table(data, scenario)
    simulation_table <- mfclshiny_selftest_simulation_table(data, scenario)
    contract_table <- mfclshiny_selftest_contract_table(runs)
    table_block <- function(name, heading, table, caption, column_widths = NULL) {
      tex_file <- file.path(table_dir, paste0("selftest-", name, "-", slug, ".tex"))
      latex <- if (file.exists(tex_file)) paste(readLines(tex_file, warn = FALSE), collapse = "\n") else ""
      paste0(
        '<div class="format-block"><h3>', heading, '</h3><div class="actions"><button onclick="copyTable(\'table-',
        name, "-", slug, '\',this)">Copy table for Word</button><button onclick="copyText(\'latex-table-',
        name, "-", slug, '\',this)">Copy LaTeX</button></div>',
        mfclshiny_selftest_html_math(mfclshiny_jitter_html_table(
          table, paste0("table-", name, "-", slug), caption,
          column_widths = column_widths
        )),
        '<pre id="latex-table-', name, "-", slug, '" class="copy-source">',
        mfclshiny_jitter_html_escape(latex), '</pre></div>'
      )
    }
    figure_html <- c(
      mfclshiny_selftest_html_section(
        "Key recovery", images[[scenario]]$key, paste0("key-", slug),
        paste0("selftest-key-recovery-", slug, ".png"),
        mfclshiny_selftest_figure_caption("key", model_name, included, total, years),
        paste0("latex-key-", slug)
      ),
      mfclshiny_selftest_html_section(
        "Time-series recovery", images[[scenario]]$recovery, paste0("recovery-", slug),
        paste0("selftest-recovery-", slug, ".png"),
        mfclshiny_selftest_figure_caption("recovery", model_name, included, total, years),
        paste0("latex-recovery-", slug)
      )
    )
    if (!is.null(images[[scenario]]$simulation) && file.exists(images[[scenario]]$simulation)) {
      figure_html <- c(
        figure_html,
        mfclshiny_selftest_html_section(
          "Pseudo-data generator check", images[[scenario]]$simulation, paste0("simulation-", slug),
          paste0("selftest-simulation-", slug, ".png"),
          mfclshiny_selftest_figure_caption("simulation", model_name, included, total, years),
          paste0("latex-simulation-", slug)
        )
      )
    }
    paste0(
      '<section id="tab-', slug, '" class="model-card tab-panel"><h2>',
      mfclshiny_jitter_html_escape(model_name), '</h2>',
      paste(figure_html, collapse = "\n"),
      table_block(
        "recent-recovery", "Assessment-quantity recovery", recovery_table,
        paste0(
          "Recovery of recent and native management and assessment summaries ending in ",
          terminal_year, " across the ", included, " included self-test refits (",
          window_text, ")."
        ),
        column_widths = c(24, 9, 10, 11, 16, 19, 11)
      ),
      table_block(
        "parameter-recovery", "Selected estimated-parameter recovery", parameter_table,
        paste0("Recovery of selected recruitment-scale, growth and stock-recruit parameters across the ", included, " included self-test refits."),
        column_widths = c(29, 9, 11, 12, 17, 22)
      ),
      if (nrow(simulation_table)) table_block(
        "simulation-centering", "Pseudo-data centering", simulation_table,
        paste0("Whole-series pseudo-data errors across the ", included, " included self-test replicates."),
        column_widths = c(30, 10, 17, 27, 16)
      ) else "",
      '</section>'
    )
  }, character(1L))
  replicate_counts <- vapply(scenarios, function(scenario) {
    nrow(data$runs[data$runs$scenario == scenario, , drop = FALSE])
  }, integer(1L))
  replicate_text <- paste(sort(unique(replicate_counts)), collapse = " or ")
  thresholds <- suppressWarnings(as.numeric(unique(data$runs$grad_reference)))
  thresholds <- thresholds[is.finite(thresholds) & thresholds > 0]
  threshold_text <- if (length(thresholds)) {
    paste(formatC(sort(unique(thresholds)), format = "e", digits = 1), collapse = " or ")
  } else "the recorded threshold"
  parameter_text <- paste(unique(as.character(data$parameters$parameter_label)), collapse = ", ")
  has_native_management <- is.data.frame(data$management) && nrow(data$management) > 0L
  has_aggregate_f <- is.data.frame(data$derived) &&
    "relative_fishing_mortality" %in% unique(data$derived$metric)
  native_management_text <- if (has_native_management) {
    paste0(
      "Native MSY, F_MSY and equilibrium spawning biomass at MSY are read directly from each truth and refit plot report. ",
      "F_recent/F_MSY is the reciprocal of MFCL's native F multiplier at MSY. "
    )
  } else {
    paste0(
      "This archive does not retain a native yield result for every refit, so MSY, F_MSY, equilibrium spawning biomass at MSY and ",
      "F_recent/F_MSY are omitted rather than approximated. "
    )
  }
  aggregate_f_text <- if (has_aggregate_f) {
    "Recent aggregate F/F_MSY is the mean of MFCL's reported quarterly aggregate F/F_MSY series over the same recent-F calendar window. "
  } else ""
  window_specs <- vapply(scenarios, function(scenario) {
    settings <- mfclshiny_selftest_recent_settings(data, scenario)
    paste0(
      "SB_recent T-", settings$sb_years - 1L, " to T; ",
      "SB_F=0 T-", settings$sbf0_years, " to T-1; ",
      "annual-F diagnostic T-", settings$f_start_offset, " to T-",
      settings$f_end_offset
    )
  }, character(1L))
  method_items <- c(
    `Operating model` = paste0(
      "The fitted final parameter state of the Diagnostic model was treated as the generating truth for pseudo-data generation and recovery comparisons. ",
      "The negative-binomial tag overdispersion parameter τ was fixed at 2 in the generating model and in every refit."
    ),
    `Pseudo-data generation` = paste0(
      replicate_text,
      " independent pseudo-data sets per model were generated with replicate-specific seeds. CPUE, length composition, age-at-length and post-mixing tag observations were simulated under the archived model settings; catch and empirical pre-mixing tag observations remained conditioned."
    ),
    Refitting = paste0(
      "Each pseudo-data set was refitted independently with the recorded MFCL doitall schedule. Recovery figures include only completed refits marked converged under the archived MGC criterion (",
      threshold_text, ")."
    ),
    `Assessment quantities` = paste0(
      "Annual depletion (SB/SB(F=0)), spawning potential (SB), no-fishing spawning potential (SB_F=0), recruitment (R) and annual fishing mortality (F) are compared with their generating truth. ",
      "Recent management summaries use the MFCL windows read from each model: ",
      paste(unique(window_specs), collapse = "; "), ". ",
      "SB_recent is the mean spawning potential including terminal year T; SB_F=0 is the mean no-fishing spawning potential ending at T-1; recent depletion is their ratio. ",
      "Recent recruitment uses the SB_recent window. The annual-F series is the population-number-weighted harvest-rate diagnostic archived by the self-test and is averaged over the model's recent-F calendar window. ",
      "It is not the native equilibrium-yield quantity F_recent/F_MSY. ",
      aggregate_f_text,
      native_management_text
    ),
    `Estimated parameters` = paste0(
      "Parameter recovery is restricted to ", parameter_text,
      "; fixed natural mortality and high-dimensional nuisance deviations are excluded. ",
      "MFCL totpop is the fitted log recruitment scale (ln R0), which sets the population or mean-recruitment magnitude; it is not an untransformed recruitment count. ",
      "Mean length at the youngest age (L1) and mean length at the oldest age (L2) anchor the growth curve, while the von Bertalanffy growth rate (K) controls the transition between them. ",
      "Mean standard deviation of length-at-age (s1) sets the typical spread around mean length, and its age trend (s2) controls how that spread changes with age."
    ),
    `Error summaries` = paste0(
      "For each replicate, the period-average relative error is calculated against the same operating-model quantity. ",
      "Tables report the median error, the central 95% empirical range and the relative root-mean-square error across successful refits."
    ),
    `Tag treatment` = "Tag recaptures used the validated native conditional post-mixing likelihood contract. Empirical pre-mixing tag information remained conditioned in the dynamics and was not treated as a newly simulated likelihood contribution.",
    Interpretation = "Empirical recovery bands describe repeated pseudo-data performance under this configured generator. They are diagnostic distributions and are not parameter confidence intervals.",
    `Reporting basis` = "The diagnostic follows the native MFCL simulation-estimation runner, the WCPO bigeye assessment reporting focus used by Day et al. (2023, WCPFC-SC19-SA-WP-05), and the stock-status quantities identified for the 2026 SC22 review."
  )
  method_html <- paste0(
    '<ul class="method-list">',
    paste0(
      '<li><strong>', mfclshiny_jitter_html_escape(names(method_items)), '.</strong> ',
      mfclshiny_selftest_html_math(mfclshiny_jitter_html_escape(unname(method_items))), '</li>',
      collapse = ""
    ),
    '</ul>',
    '<div class="equation" aria-label="Relative recovery error equation">',
    '<span class="math-inline"><i>e</i><sub><i>r</i>,<i>X</i></sub> = 100 &times; ',
    '(<span style="text-decoration:overline"><i>X</i></span><sup>refit</sup><sub><i>r</i></sub> &minus; ',
    '<span style="text-decoration:overline"><i>X</i></span><sup>truth</sup>) / ',
    '|<span style="text-decoration:overline"><i>X</i></span><sup>truth</sup>|</span>',
    '</div><p class="formula-note">Here, <i>r</i> indexes a successful self-test replicate, <i>X</i> is an assessment quantity or selected estimated parameter, and the overbar denotes the mean over the model-specific recent period when a period average is reported. Relative RMSE is <span class="math-inline">&radic;[mean<sub><i>r</i></sub>(<i>e</i><sub><i>r</i>,<i>X</i></sub><sup>2</sup>)]</span>.</p>'
  )
  method_latex_items <- paste0(
    "\\item \\textbf{", mfclshiny_jitter_latex_escape(names(method_items)), ".} ",
    mfclshiny_selftest_latex_math(mfclshiny_jitter_latex_prose(unname(method_items)))
  )
  method_latex <- paste(
    "\\begin{itemize}",
    paste(method_latex_items, collapse = "\n"),
    "\\end{itemize}",
    "\\[",
    "e_{r,X}=100\\,\\frac{\\overline{X}^{\\mathrm{refit}}_{r}-\\overline{X}^{\\mathrm{truth}}}{\\left|\\overline{X}^{\\mathrm{truth}}\\right|},",
    "\\qquad",
    "\\mathrm{RRMSE}_{X}=\\sqrt{\\frac{1}{n}\\sum_{r=1}^{n} e_{r,X}^{2}}.",
    "\\]",
    "Here, $r$ indexes a successful self-test replicate, $X$ is an assessment quantity or selected estimated parameter, and the overbar denotes the mean over the model-specific recent period when a period average is reported.",
    sep = "\n"
  )
  results <- vapply(scenarios, function(scenario) {
    runs <- data$runs[data$runs$scenario == scenario, , drop = FALSE]
    model_name <- mfclshiny_jitter_first_text(runs$model_label, scenario)
    years <- mfclshiny_selftest_recent_years(data, scenario, recent_years)
    paste0(
      '<p><strong>', mfclshiny_jitter_html_escape(model_name), '.</strong> ',
      mfclshiny_selftest_html_math(mfclshiny_jitter_html_escape(
        mfclshiny_selftest_results_text(data, scenario, years)
      )), '</p>'
    )
  }, character(1L))
  results_latex <- paste(vapply(scenarios, function(scenario) {
    runs <- data$runs[data$runs$scenario == scenario, , drop = FALSE]
    model_name <- mfclshiny_jitter_first_text(runs$model_label, scenario)
    years <- mfclshiny_selftest_recent_years(data, scenario, recent_years)
    paste0(
      "\\textbf{", mfclshiny_jitter_latex_escape(model_name), ".} ",
      mfclshiny_selftest_latex_math(mfclshiny_jitter_latex_prose(
        mfclshiny_selftest_results_text(data, scenario, years)
      ))
    )
  }, character(1L)), collapse = "\n\n")
  reference_html <- paste0(
    '<h3 class="references-heading">References</h3><ol class="reference-list"><li>',
    'Day, J. et al. (2023). <em>Stock assessment of bigeye tuna in the western and central Pacific Ocean: 2023</em>. ',
    'WCPFC-SC19-SA-WP-05 (Rev. 2). ',
    '<a href="https://meetings.wcpfc.int/node/19353">WCPFC document page</a>.</li>',
    '<li>WCPFC (2026). <em>SC22 agenda item 4.3.2.1: Review of the 2026 bigeye tuna stock assessment</em>. ',
    '<a href="https://meetings.wcpfc.int/taxonomy/term/2845">WCPFC SC22 document page</a>.</li></ol>'
  )
  reference_bibtex <- paste(
    "@techreport{day2023bet,",
    "  author = {Day, J. and Magnusson, A. and Teears, T. and Hampton, J. and Davies, N. and Castillo-Jord{\\'a}n, C. and Peatman, T. and Scott, R. and Scutt Phillips, J. and McKechnie, S. and Scott, F. and Yao, N. and Natadra, R. and Pilling, G. and Williams, P. and Hamer, P.},",
    "  title = {Stock assessment of bigeye tuna in the western and central Pacific Ocean: 2023},",
    "  institution = {Western and Central Pacific Fisheries Commission},",
    "  number = {WCPFC-SC19-2023/SA-WP-05, Rev. 2},",
    "  year = {2023},",
    "  url = {https://meetings.wcpfc.int/node/19353}",
    "}",
    "",
    "@misc{wcpfc2026betreview,",
    "  author = {{Western and Central Pacific Fisheries Commission}},",
    "  title = {{SC22 agenda item 4.3.2.1: Review of the 2026 bigeye tuna stock assessment}},",
    "  year = {2026},",
    "  url = {https://meetings.wcpfc.int/taxonomy/term/2845}",
    "}",
    sep = "\n"
  )
  html <- paste0(
    '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
    '<title>', mfclshiny_jitter_html_escape(title), '</title><style>',
    ':root{--ink:#123b5d;--muted:#526979;--sea:#087f8c;--paper:#eef2f3;--card:#fff;--line:#c8d9df;--orange:#d97904}*{box-sizing:border-box}body{margin:0;background:var(--paper);color:#1d2f3a;font-family:"Aptos","Source Sans 3",sans-serif}header{padding:24px max(5vw,24px);background:var(--ink);color:#fff}header h1{font-family:Georgia,"Times New Roman",serif;font-size:clamp(1.7rem,3vw,2.5rem);margin:0}header p{margin:.6rem 0 0;color:#d6edf1}main{max-width:1240px;margin:auto;padding:28px max(3vw,18px) 70px}.tabs{display:flex;gap:8px;flex-wrap:wrap;margin-bottom:20px}.tab-button{background:#dce6e9;color:var(--ink);border:1px solid #b7cbd1}.tab-button.active{background:var(--ink);color:#fff}.tab-panel{display:none}.tab-panel.active{display:block}.overview,.model-card{background:#fff;border:1px solid var(--line);padding:clamp(18px,3vw,36px);margin-bottom:28px}.overview{border-top:5px solid var(--orange)}.model-card{border-top:5px solid var(--sea)}h2,h3{font-family:Georgia,"Times New Roman",serif;color:var(--ink)}h2{font-size:clamp(1.6rem,3vw,2.3rem);margin:.2rem 0 1.2rem}h3{margin:0 0 .4rem}.method-list{max-width:980px;margin:0;padding-left:1.25rem;color:#29495b;font-family:Georgia,"Times New Roman",serif;font-size:1rem;line-height:1.58}.method-list li{margin:0 0 .72rem;padding-left:.28rem}.results-copy,.formula-note{max-width:980px;color:#29495b;font-family:Georgia,"Times New Roman",serif;font-size:1rem;line-height:1.58}.math-inline{white-space:nowrap;font-family:Cambria,"Cambria Math",Georgia,serif}.equation{max-width:980px;margin:18px 0 8px;padding:16px;background:#f3f8f9;border-left:3px solid var(--sea);font-family:"Cambria Math",Cambria,Georgia,serif;font-size:1.12rem;text-align:center;overflow:auto}.references-heading{margin-top:1.6rem}.reference-list{max-width:980px;padding-left:1.35rem;font-family:Georgia,"Times New Roman",serif;font-size:.9rem;line-height:1.5}.reference-list a{color:var(--sea)}.figure-block{margin-top:26px}.figure{width:100%;height:auto;border:1px solid #d8e5e9;background:#fff}figcaption{margin-top:12px;padding:12px 15px;background:#f1f6f7;border-left:3px solid var(--sea);color:#29495b;font-family:Georgia,"Times New Roman",serif;font-size:.95rem;line-height:1.55}.figure-number,.table-number{display:inline-block;min-width:1.8em;padding:0 .15em;border-bottom:1px dotted var(--sea);color:var(--sea)}.format-block{margin-top:28px;padding-top:20px;border-top:1px solid var(--line)}.table-scroll{overflow:auto;max-height:680px;border:1px solid var(--line);margin-top:12px}table{border-collapse:collapse;width:100%;table-layout:fixed;font-size:.86rem}th,td{white-space:normal;overflow-wrap:anywhere}.actions{display:flex;gap:9px;flex-wrap:wrap;margin:18px 0 8px}button{border:0;background:var(--sea);color:#fff;padding:9px 14px;font-weight:700;cursor:pointer;transition:background .16s ease}button:hover{background:var(--ink)}button.done{background:#24784f}.copy-source{position:absolute!important;width:1px!important;height:1px!important;overflow:hidden!important;clip:rect(0 0 0 0)!important;white-space:pre!important;padding:0!important;border:0!important}.action-status{position:fixed;right:22px;bottom:22px;z-index:20;background:var(--ink);color:#fff;padding:10px 15px;opacity:0;transform:translateY(8px);pointer-events:none;transition:opacity .16s ease,transform .16s ease}.action-status.show{opacity:1;transform:translateY(0)}@media print{body{background:#fff}.tabs,.actions,.action-status{display:none}.tab-panel{display:block!important}.overview,.model-card{border:0;padding:0;margin:0 0 28px}}',
    '</style></head><body><header><h1>BET 2026 Diagnostic model</h1><p>',
    mfclshiny_jitter_html_escape(title), '</p></header><main><nav class="tabs">',
    '<button class="tab-button active" onclick="showTab(\'tab-overview\',this)">Overview</button>',
    paste(tab_buttons, collapse = ""), '</nav>',
    '<section id="tab-overview" class="overview tab-panel active"><h2>Self-test analysis</h2><div id="selftest-method">',
    method_html, '</div><pre id="latex-selftest-method" class="copy-source">', mfclshiny_jitter_html_escape(method_latex),
    '</pre><div class="actions"><button onclick="copySection(\'selftest-method\',this)">Copy analysis for Word</button><button onclick="copyText(\'latex-selftest-method\',this)">Copy analysis for LaTeX</button></div>',
    '<h2>Results</h2><div id="selftest-results" class="results-copy">', paste(results, collapse = ""),
    reference_html, '</div><pre id="latex-selftest-results" class="copy-source">', mfclshiny_jitter_html_escape(results_latex),
    '</pre><pre id="bibtex-selftest-reference" class="copy-source">', mfclshiny_jitter_html_escape(reference_bibtex),
    '</pre><div class="actions"><button onclick="copySection(\'selftest-results\',this)">Copy results for Word</button><button onclick="copyText(\'latex-selftest-results\',this)">Copy results for LaTeX</button><button onclick="copyText(\'bibtex-selftest-reference\',this)">Copy reference as BibTeX</button></div></section>',
    paste(model_sections, collapse = "\n"),
    '</main><div id="action-status" class="action-status"></div><script>',
    'function feedback(b,m){const s=document.getElementById("action-status");if(b){if(!b.dataset.label)b.dataset.label=b.textContent;b.textContent=m;b.classList.add("done");setTimeout(()=>{b.textContent=b.dataset.label;b.classList.remove("done")},1600)}if(s){s.textContent=m;s.classList.add("show");setTimeout(()=>s.classList.remove("show"),1800)}}',
    'async function copyText(id,b){const e=document.getElementById(id),t=e.innerText||e.textContent;try{await navigator.clipboard.writeText(t)}catch(x){const a=document.createElement("textarea");a.value=t;document.body.appendChild(a);a.select();document.execCommand("copy");a.remove()}feedback(b,"Copied")}',
    'async function copySection(id,b){const e=document.getElementById(id).cloneNode(true),h=`<div style="font-family:Cambria,Georgia,serif;font-size:10.5pt;line-height:1.4;max-width:6.2in">${e.innerHTML}</div>`,t=e.innerText||e.textContent;try{await navigator.clipboard.write([new ClipboardItem({"text/html":new Blob([h],{type:"text/html"}),"text/plain":new Blob([t],{type:"text/plain"})})])}catch(x){const d=document.createElement("div");d.innerHTML=h;d.style.position="fixed";d.style.left="-10000px";document.body.appendChild(d);const r=document.createRange();r.selectNode(d);const s=window.getSelection();s.removeAllRanges();s.addRange(r);document.execCommand("copy");s.removeAllRanges();d.remove()}feedback(b,"Copied for Word")}',
    'async function copyTable(id,b){const e=document.getElementById(id),c=e.cloneNode(true);c.querySelectorAll("[contenteditable]").forEach(n=>{n.removeAttribute("contenteditable");n.removeAttribute("spellcheck");n.removeAttribute("title");n.removeAttribute("class");n.removeAttribute("style")});c.removeAttribute("style");c.removeAttribute("width");c.setAttribute("width","100%");c.style.cssText="border-collapse:collapse;width:100%;max-width:6.2in;table-layout:fixed;font-family:Cambria,Georgia,serif;font-size:8.5pt;line-height:1.18";c.querySelectorAll("th,td").forEach(n=>{n.removeAttribute("width");n.style.cssText="padding:3pt 4pt;white-space:normal;overflow-wrap:break-word;word-wrap:break-word;vertical-align:top"});const h=`<div style="width:6.2in;max-width:100%;overflow:hidden">${c.outerHTML}</div>`,t=Array.from(c.rows).map(r=>Array.from(r.cells).map(x=>x.innerText).join("\\t")).join("\\n");try{await navigator.clipboard.write([new ClipboardItem({"text/html":new Blob([h],{type:"text/html"}),"text/plain":new Blob([t],{type:"text/plain"})})])}catch(x){const d=document.createElement("div");d.innerHTML=h;d.style.position="fixed";d.style.left="-10000px";document.body.appendChild(d);const r=document.createRange();r.selectNode(d);const s=window.getSelection();s.removeAllRanges();s.addRange(r);document.execCommand("copy");s.removeAllRanges();d.remove()}feedback(b,"Copied")}',
    'async function copyFigure(i,c,b){const e=document.getElementById(i),p=document.getElementById(c).cloneNode(true);p.querySelectorAll("[contenteditable]").forEach(n=>{n.removeAttribute("contenteditable");n.removeAttribute("spellcheck");n.removeAttribute("title");n.removeAttribute("class");n.removeAttribute("style")});const h=`<div style="font-family:Cambria,Georgia,serif"><img src="${e.src}" style="display:block;width:100%;height:auto"><p style="font-size:10.5pt;line-height:1.3;margin:8pt 0 0">${p.innerHTML}</p></div>`;try{await navigator.clipboard.write([new ClipboardItem({"text/html":new Blob([h],{type:"text/html"}),"text/plain":new Blob([p.innerText],{type:"text/plain"})})])}catch(x){const d=document.createElement("div");d.innerHTML=h;d.style.position="fixed";d.style.left="-10000px";document.body.appendChild(d);const r=document.createRange();r.selectNode(d);const s=window.getSelection();s.removeAllRanges();s.addRange(r);document.execCommand("copy");s.removeAllRanges();d.remove()}feedback(b,"Copied for Word")}',
    'function saveImage(id,name,b){const a=document.createElement("a");a.href=document.getElementById(id).src;a.download=name;document.body.appendChild(a);a.click();a.remove();feedback(b,"Download started")}',
    'function showTab(id,b){document.querySelectorAll(".tab-panel").forEach(x=>x.classList.remove("active"));document.querySelectorAll(".tab-button").forEach(x=>x.classList.remove("active"));document.getElementById(id).classList.add("active");b.classList.add("active")}',
    '</script></body></html>'
  )
  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}

#' Build a portable report-ready MFCL self-test bundle
#'
#' Creates self-test pseudo-data, recovery, convergence and tag-contract
#' diagnostics as publication figures, LaTeX tables and a self-contained HTML
#' review in the same style as the jitter and retrospective reports.
#'
#' @param model_dir Root containing expanded model and self-test job inputs.
#' @param output_dir Output directory.
#' @param title Report title.
#' @param provenance Optional Kflow model/self-test mapping.
#' @param data Optional normalized output from `collect_selftest_diagnostics()`.
#' @param recent_years Optional years averaged for the recent-period recovery
#'   diagnostic. The terminal four available years are used by default.
#' @param formats Figure formats (`png` and `pdf`).
#' @param width,height Figure dimensions in inches.
#' @param dpi PNG resolution.
#' @param render_html Write a self-contained HTML report.
#' @return Invisibly returns normalized data, plots, tables and output paths.
#' @export
build_selftest_report <- function(model_dir = NULL,
                                  output_dir = "selftest",
                                  title = "Self-test model checks",
                                  provenance = NULL,
                                  data = NULL,
                                  recent_years = NULL,
                                  formats = c("png", "pdf"),
                                  width = 11,
                                  height = 6.2,
                                  dpi = 300,
                                  render_html = TRUE) {
  if (is.null(data)) {
    if (is.null(model_dir)) stop("Provide model_dir or data.", call. = FALSE)
    data <- collect_selftest_diagnostics(model_dir, provenance = provenance)
  }
  if (!is.list(data) || !all(c("runs", "derived", "parameters", "simulation") %in% names(data))) {
    stop("data must contain self-test runs, derived, parameters and simulation tables.", call. = FALSE)
  }
  formats <- intersect(unique(tolower(formats)), c("png", "pdf"))
  if (!length(formats)) formats <- "png"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  figure_dir <- file.path(output_dir, "figures")
  table_dir <- file.path(output_dir, "tables")
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  scenarios <- unique(data$runs$scenario)
  plots <- images <- list()
  figure_rows <- table_rows <- list()
  save_plot <- function(plot, stem, plot_width = width, plot_height = height) {
    paths <- character()
    for (format in formats) {
      path <- file.path(figure_dir, paste0(stem, ".", format))
      ggplot2::ggsave(
        path, plot = plot, width = plot_width, height = plot_height,
        dpi = dpi, units = "in", bg = "white"
      )
      if (identical(format, "png")) {
        mfclshiny_report_optimize_png(path, optimize_figures = TRUE, lossless_only = TRUE)
      }
      figure_rows[[length(figure_rows) + 1L]] <<- data.frame(
        figure = stem, format = format,
        file = normalizePath(path, winslash = "/", mustWork = FALSE),
        width = plot_width, height = plot_height,
        dpi = if (identical(format, "png")) as.integer(dpi) else NA_integer_,
        stringsAsFactors = FALSE
      )
      paths[[format]] <- path
    }
    if (is.null(paths[["png"]])) {
      path <- file.path(figure_dir, paste0(stem, ".png"))
      ggplot2::ggsave(path, plot = plot, width = plot_width, height = plot_height, dpi = dpi, units = "in", bg = "white")
      mfclshiny_report_optimize_png(path, optimize_figures = TRUE, lossless_only = TRUE)
      paths[["png"]] <- path
    }
    normalizePath(paths[["png"]], winslash = "/", mustWork = FALSE)
  }
  for (scenario in scenarios) {
    slug <- mfclshiny_jitter_slug(scenario)
    years <- mfclshiny_selftest_recent_years(data, scenario, recent_years)
    settings <- mfclshiny_selftest_recent_settings(data, scenario)
    terminal_year <- max(years)
    window_text <- paste0(
      "SB_recent ", mfclshiny_selftest_year_label(years), "; ",
      "SB_F=0 ", mfclshiny_selftest_year_label(seq.int(
        terminal_year - settings$sbf0_years, terminal_year - 1L
      )), "; annual-F diagnostic ", mfclshiny_selftest_year_label(seq.int(
        terminal_year - settings$f_start_offset,
        terminal_year - settings$f_end_offset
      ))
    )
    recovery_plot <- plot_selftest_recovery(data, scenario)
    key_plot <- plot_selftest_key_recovery(data, scenario, years)
    simulation_plot <- if (nrow(data$simulation[data$simulation$scenario == scenario, , drop = FALSE])) {
      plot_selftest_simulation(data, scenario)
    } else NULL
    plots[[scenario]] <- list(
      recovery = recovery_plot,
      key = key_plot,
      simulation = simulation_plot
    )
    images[[scenario]] <- list(
      recovery = save_plot(recovery_plot, paste0("selftest-recovery-", slug)),
      key = save_plot(
        key_plot,
        paste0("selftest-key-recovery-", slug),
        plot_height = if (
          (is.data.frame(data$management) &&
            nrow(data$management[data$management$scenario == scenario, , drop = FALSE])) ||
            "relative_fishing_mortality" %in%
              data$derived$metric[data$derived$scenario == scenario]
        ) 7.4 else 5.7
      ),
      simulation = if (!is.null(simulation_plot)) {
        save_plot(simulation_plot, paste0("selftest-simulation-", slug), plot_height = 10.0)
      } else NULL
    )
    runs <- data$runs[data$runs$scenario == scenario, , drop = FALSE]
    included <- sum(runs$included %in% TRUE)
    tables <- list(
      `recent-recovery` = list(
        value = mfclshiny_selftest_recovery_table(data, scenario, years),
        caption = paste0(
          "Recovery of recent and native management and assessment summaries ending in ",
          terminal_year, " across ", included, " included self-test refits (",
          window_text, ")."
        )
      ),
      `parameter-recovery` = list(
        value = mfclshiny_selftest_parameter_table(data, scenario),
        caption = paste0("Recovery of selected recruitment-scale, growth and stock-recruit parameters across ", included, " included self-test refits.")
      ),
      `simulation-centering` = list(
        value = mfclshiny_selftest_simulation_table(data, scenario),
        caption = paste0("Whole-series pseudo-data errors across ", included, " included self-test replicates.")
      )
    )
    for (name in names(tables)) {
      if (!nrow(tables[[name]]$value)) next
      bundle <- mfclshiny_jitter_write_table_bundle(
        tables[[name]]$value,
        paste0("selftest-", name, "-", slug),
        table_dir,
        tables[[name]]$caption
      )
      tex_file <- as.character(bundle$file[[1L]])
      if (file.exists(tex_file)) {
        tex_lines <- readLines(tex_file, warn = FALSE)
        writeLines(mfclshiny_selftest_latex_math(tex_lines), tex_file, useBytes = TRUE)
      }
      table_rows[[length(table_rows) + 1L]] <- bundle
    }
  }
  html_file <- file.path(output_dir, "selftest-report.html")
  if (isTRUE(render_html)) {
    mfclshiny_selftest_write_html(
      html_file, data, images, table_dir, title,
      recent_years = recent_years
    )
  }
  invisible(list(
    data = data,
    plots = plots,
    figures = mfclshiny_selftest_bind_rows(figure_rows),
    tables = mfclshiny_selftest_bind_rows(table_rows),
    html = if (isTRUE(render_html)) normalizePath(html_file, winslash = "/", mustWork = FALSE) else "",
    html_image_dpi = as.integer(dpi),
    html_uses_publication_png = TRUE,
    recent_periods = mfclshiny_selftest_bind_rows(lapply(scenarios, function(scenario) {
      years <- mfclshiny_selftest_recent_years(data, scenario, recent_years)
      data.frame(
        scenario = scenario,
        start_year = if (length(years)) min(years) else NA_real_,
        end_year = if (length(years)) max(years) else NA_real_,
        years = paste(years, collapse = ","),
        stringsAsFactors = FALSE
      )
    })),
    title = title,
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  ))
}
