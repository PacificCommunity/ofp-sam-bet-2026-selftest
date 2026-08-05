mfclshiny_jitter_pluck <- function(x, ...) {
  keys <- list(...)
  out <- x
  for (key in keys) {
    if (!is.list(out) || is.null(out[[key]])) return(NULL)
    out <- out[[key]]
  }
  out
}

mfclshiny_jitter_first_number <- function(...) {
  values <- list(...)
  for (value in values) {
    number <- suppressWarnings(as.numeric(value))
    if (length(number) && is.finite(number[[1L]])) return(number[[1L]])
  }
  NA_real_
}

mfclshiny_jitter_first_text <- function(..., default = "") {
  values <- list(...)
  for (value in values) {
    text <- suppressWarnings(as.character(value))
    if (length(text) && !is.na(text[[1L]]) && nzchar(trimws(text[[1L]]))) {
      return(trimws(text[[1L]]))
    }
  }
  default
}

mfclshiny_jitter_first_logical <- function(...) {
  values <- list(...)
  for (value in values) {
    logical_value <- suppressWarnings(as.logical(value))
    if (length(logical_value) && !is.na(logical_value[[1L]])) return(logical_value[[1L]])
  }
  NA
}

mfclshiny_jitter_is_base_reference <- function(payload, info = NULL) {
  flags <- suppressWarnings(as.logical(unlist(c(
    mfclshiny_jitter_pluck(payload, "is_base_fit_reference"),
    mfclshiny_jitter_pluck(info, "is_base_fit_reference")
  ), use.names = FALSE)))
  if (any(flags %in% TRUE, na.rm = TRUE)) return(TRUE)
  roles <- tolower(trimws(as.character(unlist(c(
    mfclshiny_jitter_pluck(payload, "run_role"),
    mfclshiny_jitter_pluck(info, "run_role")
  ), use.names = FALSE))))
  any(roles == "base_fit_reference", na.rm = TRUE)
}

mfclshiny_jitter_has_rep_recovery <- function(payload) {
  if (!is.list(payload)) return(FALSE)
  direct <- tryCatch(payload$data$RepOut, error = function(e) NULL)
  cached <- tryCatch(payload$object_cache$objects$RepOut, error = function(e) NULL)
  artifact <- tryCatch(payload$artifacts$files$rep$bytes, error = function(e) NULL)
  !is.null(direct) || !is.null(cached) || (is.raw(artifact) && length(artifact) > 0L)
}

mfclshiny_jitter_counted_data <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!"is_base_fit_reference" %in% names(data)) return(data)
  data[!data$is_base_fit_reference %in% TRUE, , drop = FALSE]
}

mfclshiny_jitter_valid_threshold <- function(...) {
  value <- mfclshiny_jitter_first_number(...)
  if (is.finite(value) && value > 0) value else NA_real_
}

mfclshiny_jitter_data_threshold <- function(data, grad_reference = NULL) {
  override <- mfclshiny_jitter_valid_threshold(grad_reference)
  if (is.finite(override)) return(override)
  if (!is.data.frame(data) || !"grad_reference" %in% names(data)) return(NA_real_)
  values <- suppressWarnings(as.numeric(data$grad_reference))
  values <- unique(values[is.finite(values) & values > 0])
  if (length(values)) values[[1L]] else NA_real_
}

mfclshiny_jitter_slug <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("(^-+|-+$)", "", x)
  ifelse(nzchar(x), x, "model")
}

mfclshiny_jitter_html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

mfclshiny_jitter_reference_row <- function(file) {
  payload <- tryCatch(readRDS(file), error = function(e) NULL)
  if (!is.list(payload)) return(NULL)
  info <- mfclshiny_jitter_pluck(payload, "data", "info")
  registry <- mfclshiny_jitter_pluck(info, "registry")
  label <- mfclshiny_jitter_first_text(
    mfclshiny_jitter_pluck(registry, "plot_label"),
    mfclshiny_jitter_pluck(registry, "model_label"),
    mfclshiny_jitter_pluck(registry, "model_token"),
    mfclshiny_jitter_pluck(info, "plot_label"),
    mfclshiny_jitter_pluck(info, "model_label"),
    mfclshiny_jitter_pluck(info, "model_token"),
    basename(dirname(file))
  )
  data.frame(
    model_label = label,
    ref_obj = mfclshiny_jitter_first_number(
      payload$obj_fun,
      mfclshiny_jitter_pluck(info, "payload_obj_fun"),
      mfclshiny_jitter_pluck(info, "obj_fun")
    ),
    ref_grad = mfclshiny_jitter_first_number(
      payload$max_grad,
      mfclshiny_jitter_pluck(info, "payload_max_grad"),
      mfclshiny_jitter_pluck(info, "max_grad")
    ),
    source_file = normalizePath(file, winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE
  )
}

mfclshiny_jitter_normalize_provenance <- function(provenance) {
  if (is.null(provenance)) return(data.frame())
  provenance <- as.data.frame(provenance, stringsAsFactors = FALSE, check.names = FALSE)
  aliases <- list(
    model_job = c("model_job", "model_job_number", "source_job"),
    model_id = c("model_id", "model_job_id", "source_job_id"),
    jitter_job = c("jitter_job", "diagnostic_job", "check_job"),
    jitter_id = c("jitter_id", "diagnostic_id", "check_job_id"),
    model_label = c("model_label", "label", "plot_label")
  )
  for (target in names(aliases)) {
    source <- aliases[[target]][aliases[[target]] %in% names(provenance)][1L]
    if (length(source) && !is.na(source)) {
      provenance[[target]] <- as.character(provenance[[source]])
    } else if (!target %in% names(provenance)) {
      provenance[[target]] <- ""
    }
    provenance[[target]][is.na(provenance[[target]])] <- ""
  }
  provenance
}

mfclshiny_jitter_path_match <- function(path, provenance, kind = c("jitter", "model")) {
  kind <- match.arg(kind)
  if (!is.data.frame(provenance) || !nrow(provenance)) return(integer())
  columns <- if (identical(kind, "jitter")) c("jitter_id", "jitter_job") else c("model_id", "model_job")
  which(vapply(seq_len(nrow(provenance)), function(i) {
    patterns <- unlist(provenance[i, intersect(columns, names(provenance)), drop = FALSE], use.names = FALSE)
    patterns <- as.character(patterns)
    patterns <- patterns[nzchar(patterns) & !is.na(patterns)]
    any(vapply(patterns, function(pattern) grepl(pattern, path, fixed = TRUE), logical(1)))
  }, logical(1)))
}

mfclshiny_jitter_infer_model_label <- function(file) {
  parts <- strsplit(normalizePath(file, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1L]]
  jitter_index <- which(tolower(parts) == "jitter")
  if (length(jitter_index) && jitter_index[[1L]] > 1L) return(parts[[jitter_index[[1L]] - 1L]])
  seed_dir <- basename(dirname(file))
  parent <- basename(dirname(dirname(file)))
  if (grepl("^(jitter[_-]?seed|seed)[_-]?[0-9]+$", seed_dir, ignore.case = TRUE)) parent else seed_dir
}

mfclshiny_jitter_reference_for <- function(file, label, provenance_row, references) {
  if (!is.data.frame(references) || !nrow(references)) {
    return(list(ref_obj = NA_real_, ref_grad = NA_real_, model_label = label, source_file = ""))
  }
  candidates <- seq_len(nrow(references))
  if (is.data.frame(provenance_row) && nrow(provenance_row)) {
    patterns <- c(provenance_row$model_id, provenance_row$model_job)
    patterns <- patterns[nzchar(patterns) & !is.na(patterns)]
    if (length(patterns)) {
      matched <- which(vapply(references$source_file, function(path) {
        any(vapply(patterns, function(pattern) grepl(pattern, path, fixed = TRUE), logical(1)))
      }, logical(1)))
      if (length(matched)) candidates <- matched
    }
  }
  label_match <- candidates[tolower(references$model_label[candidates]) == tolower(label)]
  if (length(label_match)) candidates <- label_match
  if (length(candidates) > 1L) {
    seed_parent <- normalizePath(dirname(file), winslash = "/", mustWork = FALSE)
    prefix_score <- vapply(references$source_file[candidates], function(path) {
      ref_parent <- dirname(path)
      common <- 0L
      limit <- min(nchar(seed_parent), nchar(ref_parent))
      while (common < limit && substr(seed_parent, common + 1L, common + 1L) == substr(ref_parent, common + 1L, common + 1L)) {
        common <- common + 1L
      }
      common
    }, integer(1))
    candidates <- candidates[order(prefix_score, decreasing = TRUE)]
  }
  hit <- references[candidates[[1L]], , drop = FALSE]
  list(
    ref_obj = hit$ref_obj[[1L]],
    ref_grad = hit$ref_grad[[1L]],
    model_label = hit$model_label[[1L]],
    source_file = hit$source_file[[1L]]
  )
}

mfclshiny_jitter_display_table <- function(data) {
  data <- mfclshiny_jitter_counted_data(data)
  data <- data[data$converged %in% TRUE, , drop = FALSE]
  data.frame(
    Run = seq_len(nrow(data)),
    `Objective function value` = data$obj_fun,
    `Δ objective` = data$delta_obj,
    MGC = data$max_grad,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

mfclshiny_jitter_model_caption <- function(data, grad_reference) {
  data <- mfclshiny_jitter_counted_data(data)
  converged <- sum(data$converged %in% TRUE)
  total <- nrow(data)
  reference_objective <- formatC(
    mfclshiny_jitter_first_number(data$ref_obj),
    format = "f",
    digits = 1,
    big.mark = ","
  )
  reference_mgc <- formatC(
    mfclshiny_jitter_first_number(data$ref_grad),
    format = "e",
    digits = 2
  )
  threshold_value <- mfclshiny_jitter_data_threshold(data, grad_reference)
  threshold_text <- if (is.finite(threshold_value)) {
    paste0(
      " using a maximum gradient component (MGC) threshold of ",
      formatC(threshold_value, format = "g", digits = 3)
    )
  } else {
    " using the convergence status recorded by the jitter run"
  }
  paste0(
    "Of ", total, " jitter runs, ", converged,
    " converged", threshold_text,
    ". The reference model had an objective function value of ",
    reference_objective, " and an MGC of ", reference_mgc, "."
  )
}

#' Collect MFCL jitter results for plotting and reporting
#'
#' Reads `jitter_result.rds` files below a local model directory or an expanded
#' Kflow input directory. Reference objective and gradient values are read from
#' nearby `model_payload.rds` files when available.
#'
#' @param model_dir Root directory containing model and jitter outputs.
#' @param provenance Optional data frame mapping Kflow model and jitter job ids
#'   or numbers to model labels.
#' @param grad_reference Maximum absolute gradient used to classify a completed
#'   jitter fit as converged.
#' @param recursive Search recursively below `model_dir`.
#' @return A normalized data frame with one row per unique jitter seed.
#' @export
mfclshiny_jitter_count_doitall_phases <- function(file) {
  if (!length(file) || is.na(file[[1L]]) || !file.exists(file[[1L]])) return(NA_integer_)
  lines <- trimws(readLines(file[[1L]], warn = FALSE))
  active <- nzchar(lines) & !grepl("^#", lines)
  phase_command <- grepl("<<", lines, fixed = TRUE) &
    grepl("[.]par[0-9]*[[:space:]]+[^[:space:]#]+[.]par[0-9]*([[:space:]]|$)", lines)
  count <- sum(active & phase_command)
  if (count > 0L) as.integer(count) else NA_integer_
}

mfclshiny_jitter_phase_count <- function(source_file, reference_file = NA_character_, payload = NULL, info = NULL) {
  recorded <- c(
    mfclshiny_jitter_pluck(payload, "n_estimation_phases"),
    mfclshiny_jitter_pluck(payload, "phase_count"),
    mfclshiny_jitter_pluck(info, "n_estimation_phases"),
    mfclshiny_jitter_pluck(info, "phase_count")
  )
  recorded <- suppressWarnings(as.integer(unlist(recorded, use.names = FALSE)))
  recorded <- recorded[is.finite(recorded) & recorded > 0L]
  if (length(recorded)) return(recorded[[1L]])

  sources <- unique(as.character(c(source_file, reference_file)))
  sources <- sources[!is.na(sources) & nzchar(sources)]
  candidates <- character()
  for (source in sources) {
    directory <- if (dir.exists(source)) source else dirname(source)
    for (level in 0:5) {
      candidates <- c(
        candidates,
        file.path(directory, "doitall.sh"),
        file.path(directory, "mfcl-inputs", "doitall.sh")
      )
      parent <- dirname(directory)
      if (identical(parent, directory)) break
      directory <- parent
    }
  }
  candidates <- unique(candidates[file.exists(candidates)])
  if (!length(candidates)) return(NA_integer_)
  counts <- vapply(candidates, mfclshiny_jitter_count_doitall_phases, integer(1L))
  counts <- counts[is.finite(counts) & counts > 0L]
  if (length(counts)) counts[[1L]] else NA_integer_
}

collect_jitter_diagnostics <- function(model_dir,
                                       provenance = NULL,
                                       grad_reference = NULL,
                                       recursive = TRUE) {
  if (!dir.exists(model_dir)) stop("model_dir does not exist: ", model_dir, call. = FALSE)
  grad_override <- mfclshiny_jitter_valid_threshold(grad_reference)
  provenance <- mfclshiny_jitter_normalize_provenance(provenance)
  result_files <- list.files(
    model_dir,
    pattern = "^jitter_result[.]rds$",
    recursive = isTRUE(recursive),
    full.names = TRUE
  )
  result_files <- sort(unique(normalizePath(result_files, winslash = "/", mustWork = FALSE)))
  if (!length(result_files)) return(data.frame())

  reference_files <- list.files(
    model_dir,
    pattern = "^model_payload[.]rds$",
    recursive = isTRUE(recursive),
    full.names = TRUE
  )
  reference_rows <- lapply(reference_files, mfclshiny_jitter_reference_row)
  reference_rows <- Filter(Negate(is.null), reference_rows)
  references <- if (length(reference_rows)) do.call(rbind, reference_rows) else data.frame()

  rows <- lapply(result_files, function(file) {
    payload <- tryCatch(readRDS(file), error = function(e) NULL)
    if (!is.list(payload)) return(NULL)
    info_file <- file.path(dirname(file), "jitter_info.rds")
    info <- if (file.exists(info_file)) tryCatch(readRDS(info_file), error = function(e) NULL) else NULL
    provenance_hit <- mfclshiny_jitter_path_match(file, provenance, "jitter")
    provenance_row <- if (length(provenance_hit)) provenance[provenance_hit[[1L]], , drop = FALSE] else data.frame()
    inferred_label <- mfclshiny_jitter_infer_model_label(file)
    label <- mfclshiny_jitter_first_text(
      if (nrow(provenance_row)) provenance_row$model_label else NULL,
      inferred_label
    )
    reference <- mfclshiny_jitter_reference_for(file, label, provenance_row, references)
    if ((!nzchar(label) || identical(label, inferred_label)) && nzchar(reference$model_label)) {
      label <- reference$model_label
    }
    model_job <- if (nrow(provenance_row)) provenance_row$model_job[[1L]] else ""
    jitter_job <- if (nrow(provenance_row)) provenance_row$jitter_job[[1L]] else ""
    scenario <- if (nzchar(model_job)) {
      if (grepl(paste0("job[[:space:]#]*", model_job, "($|[^0-9])"), label, ignore.case = TRUE)) label else paste0(label, " (Job ", model_job, ")")
    } else {
      label
    }
    seed <- mfclshiny_jitter_first_number(
      payload$seed,
      sub(".*?([0-9]+)$", "\\1", basename(dirname(file)))
    )
    obj_fun <- mfclshiny_jitter_first_number(payload$obj_fun, mfclshiny_jitter_pluck(payload, "state", "obj_fun"))
    max_grad <- mfclshiny_jitter_first_number(payload$max_grad, mfclshiny_jitter_pluck(payload, "state", "max_grad"))
    ref_obj <- mfclshiny_jitter_first_number(
      reference$ref_obj,
      payload$ref_obj,
      payload$reference_obj_fun,
      payload$source_obj_fun,
      mfclshiny_jitter_pluck(info, "reference_obj_fun"),
      mfclshiny_jitter_pluck(info, "source_obj_fun")
    )
    ref_grad <- mfclshiny_jitter_first_number(
      reference$ref_grad,
      payload$ref_grad,
      payload$reference_max_grad,
      payload$source_max_grad,
      mfclshiny_jitter_pluck(info, "reference_max_grad"),
      mfclshiny_jitter_pluck(info, "source_max_grad")
    )
    completed <- mfclshiny_jitter_first_logical(
      payload$run_completed,
      mfclshiny_jitter_pluck(payload, "state", "run_completed"),
      payload$success
    )
    stored_converged <- mfclshiny_jitter_first_logical(
      payload$converged,
      mfclshiny_jitter_pluck(payload, "state", "converged")
    )
    row_grad_reference <- mfclshiny_jitter_valid_threshold(
      grad_override,
      if (nrow(provenance_row) && "grad_reference" %in% names(provenance_row)) provenance_row$grad_reference else NULL,
      payload$grad_reference,
      payload$convergence_threshold,
      mfclshiny_jitter_pluck(payload, "state", "grad_reference"),
      mfclshiny_jitter_pluck(payload, "state", "convergence_threshold"),
      info$grad_reference,
      info$convergence_threshold
    )
    converged <- isTRUE(completed) && !identical(stored_converged, FALSE) &&
      if (is.finite(row_grad_reference)) {
        is.finite(max_grad) && abs(max_grad) <= row_grad_reference
      } else {
        isTRUE(stored_converged)
      }
    delta_obj <- if (is.finite(obj_fun) && is.finite(ref_obj)) obj_fun - ref_obj else NA_real_
    rel_diff_pct <- if (is.finite(delta_obj) && is.finite(ref_obj) && abs(ref_obj) > 1e-12) 100 * delta_obj / abs(ref_obj) else NA_real_
    negative <- mfclshiny_jitter_first_number(
      mfclshiny_jitter_pluck(payload, "hessian_info", "n_negative_eigenvalues"),
      mfclshiny_jitter_pluck(payload, "hessian", "n_negative_eigenvalues")
    )
    is_base_reference <- mfclshiny_jitter_is_base_reference(payload, info)
    data.frame(
      scenario = scenario,
      model_label = label,
      model_job = model_job,
      jitter_job = jitter_job,
      seed = as.integer(seed),
      jitter_cv = mfclshiny_jitter_first_number(payload$jitter_cv, info$jitter_cv, mfclshiny_jitter_pluck(payload, "state", "jitter_cv")),
      phase_count = mfclshiny_jitter_phase_count(file, reference$source_file, payload, info),
      jitter_design = mfclshiny_jitter_first_text(payload$jitter_design, info$jitter_design),
      jitter_reference_stage = mfclshiny_jitter_first_text(payload$jitter_reference_stage, info$jitter_reference_stage),
      jitter_parameter_scope = mfclshiny_jitter_first_text(payload$jitter_parameter_scope, info$jitter_parameter_scope),
      run_status = mfclshiny_jitter_first_text(payload$run_status, mfclshiny_jitter_pluck(payload, "state", "run_status"), if (isTRUE(completed)) "completed" else "unknown"),
      run_completed = isTRUE(completed),
      converged = isTRUE(converged),
      ref_obj = ref_obj,
      ref_grad = ref_grad,
      obj_fun = obj_fun,
      max_grad = max_grad,
      grad_reference = row_grad_reference,
      delta_obj = delta_obj,
      rel_diff_pct = rel_diff_pct,
      hessian_status = mfclshiny_jitter_first_text(
        mfclshiny_jitter_pluck(payload, "hessian_info", "hessian_status"),
        mfclshiny_jitter_pluck(payload, "hessian", "hessian_status")
      ),
      negative_eigenvalues = negative,
      run_role = if (is_base_reference) "base_fit_reference" else "jitter",
      is_base_fit_reference = is_base_reference,
      count_as_jitter = !is_base_reference,
      display_label = mfclshiny_jitter_first_text(
        payload$display_label,
        info$display_label,
        if (is_base_reference) "Base fit reference" else paste("Seed", as.integer(seed))
      ),
      has_rep_recovery = mfclshiny_jitter_has_rep_recovery(payload),
      reference_source_file = mfclshiny_jitter_first_text(reference$source_file),
      source_file = file,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  data <- do.call(rbind, rows)
  fingerprint <- paste(
    data$scenario,
    data$seed,
    format(data$jitter_cv, digits = 15),
    format(data$obj_fun, digits = 15),
    format(data$max_grad, digits = 15),
    sep = "\r"
  )
  quality <- rowSums(cbind(
    is.finite(data$ref_obj),
    is.finite(data$ref_grad),
    data$run_completed,
    nzchar(data$jitter_job),
    4L * data$has_rep_recovery
  ))
  order_index <- order(fingerprint, -quality, data$source_file)
  data <- data[order_index, , drop = FALSE]
  fingerprint <- fingerprint[order_index]
  data <- data[!duplicated(fingerprint), , drop = FALSE]
  data <- data[order(data$scenario, data$seed, data$jitter_job), , drop = FALSE]
  data$jitter_id <- ave(seq_len(nrow(data)), data$scenario, FUN = seq_along)
  rownames(data) <- NULL
  data
}

#' Plot MFCL jitter objective and gradient diagnostics
#'
#' This is the shared plot builder used by both the mfclshiny Jitter tab and
#' standalone report generation.
#'
#' @param data Normalized jitter data returned by `collect_jitter_diagnostics()`
#'   or equivalent data containing `scenario`, `obj_fun`, `ref_obj`,
#'   `max_grad`, `ref_grad`, and `jitter_id`.
#' @param grad_reference Maximum-gradient reference line.
#' @param rel_diff_threshold Symmetric objective-difference display threshold,
#'   in percent.
#' @param facet_ncol Number of model facets per row.
#' @param converged_only Whether the input has been filtered to converged runs.
#' @param convergence_counts Optional text appended to the subtitle.
#' @param title Figure title.
#' @param show_facet_labels Show model labels above plot facets.
#' @param point_style Colour jitter runs by run number (`"run_colour"`) or show
#'   all runs in grey with optional highlighted comparison fits (`"highlight"`).
#' @param comparison_data Optional data frame containing a separately attached
#'   fit to highlight when `point_style = "highlight"`.
#' @param reference_label,comparison_label Legend labels for highlighted fits.
#' @param reference_colour,comparison_colour Colours for highlighted fits.
#' @param show_reference_line Draw the horizontal reference-objective line.
#' @return A ggplot object.
#' @export
plot_jitter_diagnostics <- function(data,
                                    grad_reference = NULL,
                                    rel_diff_threshold = 10,
                                    facet_ncol = NULL,
                                    converged_only = FALSE,
                                    convergence_counts = NULL,
                                    title = "Jitter Analysis: Convergence Diagnostics",
                                    show_facet_labels = TRUE,
                                    point_style = c("run_colour", "highlight"),
                                    comparison_data = NULL,
                                    reference_label = "Reference model",
                                    comparison_label = "Attached base fit",
                                    reference_colour = "#B2182B",
                                    comparison_colour = "#111827",
                                    show_reference_line = TRUE) {
  point_style <- match.arg(point_style)
  data <- mfclshiny_jitter_counted_data(data)
  if (!nrow(data)) stop("No counted jitter runs were provided.", call. = FALSE)
  required <- c("scenario", "obj_fun", "ref_obj", "max_grad", "ref_grad")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Missing jitter plot column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  if (!"jitter_id" %in% names(data)) data$jitter_id <- ave(seq_len(nrow(data)), data$scenario, FUN = seq_along)
  grad_override <- mfclshiny_jitter_valid_threshold(grad_reference)
  if (is.finite(grad_override)) {
    data$grad_reference <- grad_override
  } else if (!"grad_reference" %in% names(data)) {
    data$grad_reference <- NA_real_
  }
  data$grad_reference <- suppressWarnings(as.numeric(data$grad_reference))
  rel_diff_threshold <- suppressWarnings(as.numeric(rel_diff_threshold[[1L]]))
  if (!is.finite(rel_diff_threshold) || rel_diff_threshold <= 0) rel_diff_threshold <- 10
  if (is.null(facet_ncol)) facet_ncol <- min(2L, max(1L, length(unique(data$scenario))))
  facet_ncol <- max(1L, as.integer(facet_ncol[[1L]]))
  if (is.null(convergence_counts) && all(c("converged", "scenario") %in% names(data))) {
    counts <- lapply(split(data, data$scenario), function(x) paste0(sum(x$converged, na.rm = TRUE), "/", nrow(x)))
    convergence_counts <- paste(paste(names(counts), unlist(counts)), collapse = " | ")
  }

  plot_df <- data
  plot_df$max_grad <- ifelse(plot_df$max_grad > 0, plot_df$max_grad, NA_real_)
  plot_df$delta_obj <- plot_df$obj_fun - plot_df$ref_obj
  plot_df$rel_diff <- ifelse(
    is.finite(plot_df$ref_obj) & abs(plot_df$ref_obj) > 1e-12,
    plot_df$delta_obj / abs(plot_df$ref_obj),
    NA_real_
  )
  plot_df$rel_diff_pct <- 100 * plot_df$rel_diff
  plot_df$fit_status <- factor(
    ifelse(
      is.finite(plot_df$grad_reference),
      ifelse(plot_df$max_grad <= plot_df$grad_reference, "MGC at or below threshold", "MGC above threshold"),
      "MGC threshold unavailable"
    ),
    levels = c("MGC at or below threshold", "MGC above threshold", "MGC threshold unavailable")
  )
  lower_outlier <- -rel_diff_threshold
  upper_outlier <- rel_diff_threshold
  y_pad <- max(rel_diff_threshold * 0.2, 0.2)
  y_min <- lower_outlier - y_pad
  y_max <- upper_outlier + y_pad
  plot_df$is_outlier <- plot_df$rel_diff_pct < lower_outlier | plot_df$rel_diff_pct > upper_outlier
  plot_df$outlier_direction <- ifelse(plot_df$is_outlier & plot_df$rel_diff_pct < 0, "below", ifelse(plot_df$is_outlier, "above", "none"))

  ref_df <- dplyr::summarise(
    dplyr::group_by(plot_df, .data$scenario),
    ref_grad = dplyr::first(.data$ref_grad),
    ref_obj = dplyr::first(.data$ref_obj),
    .groups = "drop"
  )
  ref_df$ref_grad <- ifelse(ref_df$ref_grad > 0, ref_df$ref_grad, NA_real_)
  reference_line_layer <- if (isTRUE(show_reference_line)) {
    ggplot2::geom_hline(
      data = ref_df,
      ggplot2::aes(yintercept = .data$ref_obj),
      inherit.aes = FALSE,
      linetype = "dashed",
      color = if (identical(point_style, "highlight")) reference_colour else "#555555",
      linewidth = 0.5
    )
  } else {
    NULL
  }
  threshold_df <- unique(
    plot_df[
      is.finite(plot_df$grad_reference) & plot_df$grad_reference > 0,
      c("scenario", "grad_reference"),
      drop = FALSE
    ]
  )
  point_df <- plot_df[is.finite(plot_df$max_grad) & plot_df$max_grad > 0 & is.finite(plot_df$obj_fun), , drop = FALSE]
  if (!nrow(point_df)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No positive finite MGC and reference objective values are available.", size = 5, color = "#777777") +
        ggplot2::theme_void()
    )
  }
  outlier_df <- point_df[point_df$is_outlier, , drop = FALSE]
  clipped_corner_df <- if (nrow(outlier_df)) {
    dplyr::summarise(
      dplyr::group_by(outlier_df, .data$scenario),
      corner_x = max(.data$max_grad, na.rm = TRUE),
      .groups = "drop"
    )
  } else {
    data.frame(scenario = character(), corner_x = numeric())
  }
  clipped_corner_df$corner_y <- rep(y_max - 0.04 * (y_max - y_min), nrow(clipped_corner_df))
  outside_count <- nrow(outlier_df)
  total_count <- nrow(point_df)
  point_df$run_number <- suppressWarnings(as.numeric(point_df$jitter_id))
  if (any(!is.finite(point_df$run_number))) {
    point_df$run_number <- ave(seq_len(nrow(point_df)), point_df$scenario, FUN = seq_along)
  }

  if (identical(point_style, "highlight")) {
    comparison_df <- as.data.frame(comparison_data, stringsAsFactors = FALSE)
    if (nrow(comparison_df)) {
      required_comparison <- c("scenario", "max_grad", "obj_fun")
      if (!all(required_comparison %in% names(comparison_df))) {
        stop(
          "comparison_data is missing: ",
          paste(setdiff(required_comparison, names(comparison_df)), collapse = ", "),
          call. = FALSE
        )
      }
      comparison_df <- comparison_df[
        is.finite(comparison_df$max_grad) & comparison_df$max_grad > 0 &
          is.finite(comparison_df$obj_fun),
        , drop = FALSE
      ]
      comparison_df <- comparison_df[!duplicated(comparison_df$scenario), , drop = FALSE]
    }
    ref_df$highlight <- reference_label
    comparison_df$highlight <- comparison_label
    highlight_breaks <- character()
    highlight_values <- character()
    highlight_shapes <- numeric()
    if (nrow(comparison_df)) {
      highlight_breaks <- c(highlight_breaks, comparison_label)
      highlight_values <- c(highlight_values, stats::setNames(comparison_colour, comparison_label))
      highlight_shapes <- c(highlight_shapes, stats::setNames(18, comparison_label))
    }
    if (nrow(ref_df)) {
      highlight_breaks <- c(highlight_breaks, reference_label)
      highlight_values <- c(highlight_values, stats::setNames(reference_colour, reference_label))
      highlight_shapes <- c(highlight_shapes, stats::setNames(17, reference_label))
    }
    return(
      ggplot2::ggplot(point_df, ggplot2::aes(x = .data$max_grad, y = .data$obj_fun)) +
        ggplot2::geom_point(
          colour = "#737C82", size = 2.5, alpha = 0.82, na.rm = TRUE
        ) +
        reference_line_layer +
        ggplot2::geom_vline(
          data = threshold_df,
          ggplot2::aes(xintercept = .data$grad_reference),
          inherit.aes = FALSE,
          linetype = "dotted", color = "gray50", linewidth = 0.6
        ) +
        ggplot2::geom_point(
          data = ref_df,
          ggplot2::aes(
            x = .data$ref_grad, y = .data$ref_obj,
            colour = .data$highlight, shape = .data$highlight
          ),
          inherit.aes = FALSE, size = 4.1, na.rm = TRUE
        ) +
        ggplot2::geom_point(
          data = comparison_df,
          ggplot2::aes(
            x = .data$max_grad, y = .data$obj_fun,
            colour = .data$highlight, shape = .data$highlight
          ),
          inherit.aes = FALSE, size = 4.3, na.rm = TRUE
        ) +
        ggplot2::scale_colour_manual(
          values = highlight_values, breaks = highlight_breaks, name = NULL
        ) +
        ggplot2::scale_shape_manual(
          values = highlight_shapes, breaks = highlight_breaks, name = NULL
        ) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.06, 0.08))) +
        ggplot2::facet_wrap(~scenario, scales = "free_x", ncol = facet_ncol) +
        ggplot2::labs(
          x = "Maximum gradient component (MGC)",
          y = "Objective function value",
          title = title
        ) +
        ggplot2::theme_classic(base_size = 12.5, base_family = "serif") +
        ggplot2::theme(
          legend.position = "bottom",
          legend.text = ggplot2::element_text(size = 9.5),
          legend.margin = ggplot2::margin(t = -2),
          legend.key.height = grid::unit(0.42, "cm"),
          plot.title = ggplot2::element_text(face = "bold", size = 13),
          strip.text = if (isTRUE(show_facet_labels)) ggplot2::element_text(face = "bold", size = 11) else ggplot2::element_blank(),
          strip.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.45),
          axis.title = ggplot2::element_text(size = 11.5),
          axis.text = ggplot2::element_text(size = 10.5, color = "black"),
          axis.ticks = ggplot2::element_line(linewidth = 0.45, colour = "black"),
          plot.margin = ggplot2::margin(8, 10, 8, 8)
        )
    )
  }

  ggplot2::ggplot(point_df, ggplot2::aes(x = .data$max_grad, y = .data$obj_fun)) +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data$run_number),
      size = 2.6,
      alpha = 0.88,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = ref_df,
      ggplot2::aes(x = .data$ref_grad, y = .data$ref_obj),
      inherit.aes = FALSE, color = "#B2182B", size = 4.2, shape = 18, na.rm = TRUE
    ) +
    reference_line_layer +
    ggplot2::geom_vline(
      data = threshold_df,
      ggplot2::aes(xintercept = .data$grad_reference),
      inherit.aes = FALSE,
      linetype = "dotted",
      color = "gray50",
      linewidth = 0.6
    ) +
    ggplot2::scale_colour_viridis_c(
      option = "D",
      begin = 0.08,
      end = 0.92,
      name = "Run",
      breaks = scales::breaks_pretty(n = 5),
      guide = ggplot2::guide_colourbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(6.0, "cm"),
        barheight = grid::unit(0.36, "cm")
      )
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.06, 0.08))) +
    ggplot2::facet_wrap(~scenario, scales = "free_x", ncol = facet_ncol) +
    ggplot2::labs(
      x = "Maximum gradient component (MGC)",
      y = "Objective function value",
      title = title
    ) +
    ggplot2::theme_classic(base_size = 12.5, base_family = "serif") +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 10.5, face = "bold"),
      legend.text = ggplot2::element_text(size = 9.5),
      legend.margin = ggplot2::margin(t = -2),
      legend.key.height = grid::unit(0.42, "cm"),
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      strip.text = if (isTRUE(show_facet_labels)) ggplot2::element_text(face = "bold", size = 11) else ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.45),
      axis.title = ggplot2::element_text(size = 11.5),
      axis.text = ggplot2::element_text(size = 10.5, color = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.45, colour = "black"),
      plot.margin = ggplot2::margin(8, 10, 8, 8)
    )
}

mfclshiny_jitter_summary <- function(data) {
  data <- mfclshiny_jitter_counted_data(data)
  rows <- lapply(split(data, data$scenario), function(x) {
    finite_obj <- x$obj_fun[is.finite(x$obj_fun)]
    finite_grad <- abs(x$max_grad[is.finite(x$max_grad)])
    best_index <- if (length(finite_obj)) which.min(ifelse(is.finite(x$obj_fun), x$obj_fun, Inf)) else NA_integer_
    data.frame(
      Species = sub(" fitted model$", "", mfclshiny_jitter_first_text(x$model_label, x$scenario), ignore.case = TRUE),
      `Converged runs` = paste0(sum(x$converged, na.rm = TRUE), " / ", nrow(x)),
      `Reference objective function value` = mfclshiny_jitter_first_number(x$ref_obj),
      `Best objective function value` = if (length(finite_obj)) min(finite_obj) else NA_real_,
      `Median objective function value` = if (length(finite_obj)) stats::median(finite_obj) else NA_real_,
      `Minimum MGC` = if (length(finite_grad)) min(finite_grad) else NA_real_,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

mfclshiny_jitter_latex_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  backslash_placeholder <- "@@MFCLSHINY_BACKSLASH@@"
  x <- gsub("\\", backslash_placeholder, x, fixed = TRUE)
  replacements <- c("&" = "\\&", "%" = "\\%", "$" = "\\$", "#" = "\\#", "_" = "\\_", "{" = "\\{", "}" = "\\}")
  for (key in names(replacements)) x <- gsub(key, replacements[[key]], x, fixed = TRUE)
  x <- gsub(backslash_placeholder, "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("<", "\\textless{}", x, fixed = TRUE)
  x <- gsub(">", "\\textgreater{}", x, fixed = TRUE)
  x
}

mfclshiny_jitter_format_cell <- function(x) {
  if (is.integer(x)) {
    out <- as.character(x)
    out[is.na(x)] <- ""
    return(out)
  }
  if (is.numeric(x)) {
    out <- rep("", length(x))
    ok <- is.finite(x)
    out[ok] <- ifelse(abs(x[ok]) > 0 & (abs(x[ok]) < 1e-3 | abs(x[ok]) >= 1e6), formatC(x[ok], format = "e", digits = 3), formatC(x[ok], format = "fg", digits = 7))
    return(out)
  }
  out <- as.character(x)
  out[is.na(out)] <- ""
  out
}

mfclshiny_jitter_format_table <- function(table) {
  formatted <- as.data.frame(
    lapply(table, mfclshiny_jitter_format_cell),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  fixed_numeric <- function(x, format, digits, big.mark = "") {
    out <- rep("", length(x))
    ok <- is.finite(x)
    out[ok] <- formatC(
      x[ok], format = format, digits = digits,
      big.mark = big.mark, preserve.width = "none"
    )
    out
  }
  if ("Run" %in% names(table)) {
    formatted$Run <- ifelse(is.finite(table$Run), as.character(as.integer(table$Run)), "")
  }
  if ("Objective function value" %in% names(table)) {
    formatted[["Objective function value"]] <- fixed_numeric(
      table[["Objective function value"]], "f", 1L, ","
    )
  }
  if ("Δ objective" %in% names(table)) {
    formatted[["Δ objective"]] <- fixed_numeric(table[["Δ objective"]], "f", 1L, ",")
  }
  if ("MGC" %in% names(table)) {
    formatted$MGC <- fixed_numeric(table$MGC, "e", 2L)
  }
  formatted
}

mfclshiny_jitter_write_latex <- function(table, file, caption, label) {
  formatted <- mfclshiny_jitter_format_table(table)
  formatted[] <- lapply(formatted, mfclshiny_jitter_latex_escape)
  headers <- mfclshiny_jitter_latex_escape(names(formatted))
  headers <- gsub("Δ", "\\ensuremath{\\Delta}", headers, fixed = TRUE)
  headers <- paste0("\\textbf{", headers, "}")
  font_size <- if (ncol(formatted) >= 9L) {
    "\\scriptsize"
  } else if (ncol(formatted) >= 7L) {
    "\\footnotesize"
  } else {
    "\\small"
  }
  column_padding <- if (ncol(formatted) >= 9L) {
    "2.5pt"
  } else if (ncol(formatted) >= 7L) {
    "4pt"
  } else {
    "7pt"
  }
  first_column_width <- max(
    nchar(c(names(formatted)[[1L]], as.character(formatted[[1L]]))),
    na.rm = TRUE
  )
  first_alignment <- if (is.finite(first_column_width) &&
      first_column_width > 18L) {
    ">{\\raggedright\\arraybackslash}X"
  } else {
    "r"
  }
  alignment <- paste0(
    "@{}",
    first_alignment,
    paste(rep(">{\\raggedleft\\arraybackslash}X", max(0L, ncol(formatted) - 1L)), collapse = ""),
    "@{}"
  )
  body <- if (nrow(formatted)) apply(formatted, 1L, paste, collapse = " & ") else character()
  lines <- c(
    "% Requires \\usepackage{booktabs,tabularx,array}",
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", mfclshiny_jitter_latex_escape(caption), "}"),
    paste0("\\label{", label, "}"),
    font_size,
    paste0("\\setlength{\\tabcolsep}{", column_padding, "}"),
    "\\renewcommand{\\arraystretch}{1.08}",
    paste0("\\begin{tabularx}{\\textwidth}{", alignment, "}"),
    "\\toprule",
    paste0(paste(headers, collapse = " & "), " \\\\"),
    "\\midrule",
    if (length(body)) paste0(body, " \\\\") else character(),
    "\\bottomrule",
    "\\end{tabularx}",
    "\\end{table}"
  )
  writeLines(lines, file, useBytes = TRUE)
  invisible(file)
}

mfclshiny_jitter_write_table_bundle <- function(table, stem, table_dir, caption) {
  tex <- file.path(table_dir, paste0(stem, ".tex"))
  mfclshiny_jitter_write_latex(table, tex, caption, paste0("tab:", gsub("[^a-z0-9-]", "-", stem)))
  data.frame(
    table = stem,
    format = "tex",
    file = normalizePath(tex, winslash = "/", mustWork = FALSE),
    rows = nrow(table),
    columns = ncol(table),
    stringsAsFactors = FALSE
  )
}

mfclshiny_jitter_html_table <- function(table,
                                        id,
                                        caption = "",
                                        column_widths = NULL) {
  formatted <- mfclshiny_jitter_format_table(table)
  column_count <- ncol(formatted)
  widths <- suppressWarnings(as.numeric(column_widths))
  if (
    length(widths) != column_count ||
      any(!is.finite(widths)) ||
      any(widths <= 0)
  ) {
    widths <- if (column_count == 4L) {
      c(12, 32, 28, 28)
    } else if (column_count > 1L) {
      c(14, rep(86 / (column_count - 1L), column_count - 1L))
    } else {
      100
    }
  } else {
    widths <- 100 * widths / sum(widths)
  }
  alignments <- c("center", rep("right", max(0L, column_count - 1L)))
  colgroup <- paste0(
    "<colgroup>",
    paste0("<col style='width:", format(widths, trim = TRUE), "%;'>", collapse = ""),
    "</colgroup>"
  )
  header_style <- function(alignment) paste0(
    "font-family:Cambria,Georgia,serif;font-size:10.5pt;font-weight:700;color:#172B3A;",
    "background:#FFFFFF;",
    "text-align:", alignment, ";padding:5pt 7pt;border-top:1.5pt solid #172B3A;",
    "border-bottom:0.75pt solid #172B3A;vertical-align:bottom;",
    "white-space:normal;overflow-wrap:anywhere;line-height:1.15;"
  )
  cell_style <- function(alignment, last_row = FALSE) paste0(
    "font-family:Cambria,Georgia,serif;font-size:10.5pt;color:#172B3A;",
    "text-align:", alignment, ";padding:3.5pt 7pt;font-variant-numeric:tabular-nums;",
    if (isTRUE(last_row)) "border-bottom:1.5pt solid #172B3A;" else "border-bottom:0;",
    "vertical-align:middle;"
  )
  head <- paste0(
    "<tr>",
    paste0(
      "<th style='", vapply(alignments, header_style, character(1)), "'>",
      mfclshiny_jitter_html_escape(names(formatted)), "</th>",
      collapse = ""
    ),
    "</tr>"
  )
  body <- if (nrow(formatted)) {
    vapply(seq_len(nrow(formatted)), function(row_index) {
      values <- unlist(formatted[row_index, , drop = FALSE], use.names = FALSE)
      paste0(
        "<tr>",
        paste0(
          "<td style='", vapply(alignments, cell_style, character(1), last_row = row_index == nrow(formatted)), "'>",
          mfclshiny_jitter_html_escape(values), "</td>",
          collapse = ""
        ),
        "</tr>"
      )
    }, character(1))
  } else {
    character()
  }
  caption_html <- if (nzchar(caption)) paste0(
    "<caption style='caption-side:top;text-align:left;font-family:Cambria,Georgia,serif;",
    "font-size:10.5pt;font-weight:400;line-height:1.3;color:#172B3A;padding:0 0 7pt 0;'>",
    "<strong>Table <span class='table-number' contenteditable='true' spellcheck='false' style='display:inline-block;min-width:1.8em;padding:0 0.15em;border-bottom:1px dotted #087f8c;color:#087f8c;' ",
    "title='Click to edit the table number'>XX</span>.</strong> ",
    mfclshiny_jitter_html_escape(caption),
    "</caption>"
  ) else ""
  paste0(
    '<div class="table-scroll"><table id="', id,
    '" style="border-collapse:collapse;width:100%;table-layout:fixed;margin:0;">',
    caption_html, colgroup, "<thead>", head, "</thead><tbody>",
    paste(body, collapse = ""), "</tbody></table></div>"
  )
}

mfclshiny_jitter_collect_derived <- function(data) {
  metric_labels <- c(
    depletion = "Depletion",
    fishing_mortality = "Fishing mortality",
    recruitment = "Recruitment",
    spawning_potential = "Spawning potential"
  )
  rows <- list()
  if (!"is_base_fit_reference" %in% names(data)) data$is_base_fit_reference <- FALSE
  converged <- data[
    data$converged %in% TRUE | data$is_base_fit_reference %in% TRUE,
    ,
    drop = FALSE
  ]
  for (scenario in unique(converged$scenario)) {
    model_data <- converged[converged$scenario == scenario, , drop = FALSE]
    for (run in seq_len(nrow(model_data))) {
      payload <- tryCatch(readRDS(model_data$source_file[[run]]), error = function(e) NULL)
      derived <- mfclshiny_jitter_payload_derived(payload, model_data$source_file[[run]])
      if (!is.data.frame(derived) || !"year" %in% names(derived)) next
      metrics <- intersect(names(metric_labels), names(derived))
      for (metric in metrics) {
        rows[[length(rows) + 1L]] <- data.frame(
          scenario = scenario,
          model_label = model_data$model_label[[run]],
          run = model_data$jitter_id[[run]],
          year = as.numeric(derived$year),
          quantity = unname(metric_labels[[metric]]),
          value = as.numeric(derived[[metric]]),
          is_reference = FALSE,
          is_base_fit_reference = model_data$is_base_fit_reference[[run]] %in% TRUE,
          display_label = mfclshiny_jitter_first_text(
            model_data$display_label[[run]],
            if (model_data$is_base_fit_reference[[run]] %in% TRUE) "Base fit reference" else paste("Seed", model_data$jitter_id[[run]])
          ),
          stringsAsFactors = FALSE
        )
      }
    }
    reference_files <- if ("reference_source_file" %in% names(model_data)) {
      unique(model_data$reference_source_file[nzchar(model_data$reference_source_file)])
    } else {
      character()
    }
    reference_file <- reference_files[file.exists(reference_files)][1L]
    if (length(reference_file) && !is.na(reference_file)) {
      payload <- tryCatch(readRDS(reference_file), error = function(e) NULL)
      derived <- mfclshiny_jitter_payload_derived(payload, reference_file)
      if (is.data.frame(derived) && "year" %in% names(derived)) {
        metrics <- intersect(names(metric_labels), names(derived))
        for (metric in metrics) {
          rows[[length(rows) + 1L]] <- data.frame(
            scenario = scenario,
            model_label = model_data$model_label[[1L]],
            run = NA_integer_,
            year = as.numeric(derived$year),
            quantity = unname(metric_labels[[metric]]),
            value = as.numeric(derived[[metric]]),
            is_reference = TRUE,
            is_base_fit_reference = FALSE,
            display_label = "Reference model",
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (!length(rows)) return(data.frame())
  result <- do.call(rbind, rows)
  result[is.finite(result$year) & is.finite(result$value), , drop = FALSE]
}

mfclshiny_jitter_payload_derived <- function(payload, source_file = "") {
  if (!is.list(payload)) return(NULL)
  direct <- payload$derived_quantities
  if (is.data.frame(direct) && "year" %in% names(direct)) return(direct)

  folder <- if (nzchar(source_file)) dirname(source_file) else NULL
  hydrated <- tryCatch(
    mfclshiny_hydrate_model_payload(payload, folder = folder, roles = "RepOut"),
    error = function(e) payload
  )
  rep_obj <- tryCatch(hydrated$data$RepOut, error = function(e) NULL)
  if (is.null(rep_obj)) {
    rep_obj <- tryCatch(payload$object_cache$objects$RepOut, error = function(e) NULL)
  }
  if (is.null(rep_obj)) return(NULL)
  derived <- tryCatch(
    mfclshiny_report_extract_rep_timeseries(rep_obj),
    error = function(e) NULL
  )
  if (!is.data.frame(derived) || !"year" %in% names(derived)) return(NULL)
  derived
}

mfclshiny_jitter_extract_regional_timeseries <- function(rep_obj) {
  if (is.null(rep_obj)) return(data.frame())
  slot_df <- function(name) {
    value <- tryCatch(methods::slot(rep_obj, name), error = function(e) NULL)
    if (is.null(value)) data.frame() else mfclshiny_report_array_to_df(value)
  }
  normalize <- function(data) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    for (column in intersect(c("year", "season", "area", "data"), names(data))) {
      data[[column]] <- suppressWarnings(as.numeric(data[[column]]))
    }
    data
  }
  annual_mean_by_region <- function(data, value_name) {
    data <- normalize(data)
    required <- c("year", "season", "area", "data")
    if (!nrow(data) || !all(required %in% names(data))) return(data.frame())
    data <- data[
      is.finite(data$year) & is.finite(data$season) &
        is.finite(data$area) & is.finite(data$data),
      , drop = FALSE
    ]
    if (!nrow(data)) return(data.frame())
    seasonal <- stats::aggregate(data ~ year + season + area, data = data, FUN = sum, na.rm = TRUE)
    annual <- stats::aggregate(data ~ year + area, data = seasonal, FUN = mean, na.rm = TRUE)
    names(annual)[names(annual) == "data"] <- value_name
    annual
  }

  biomass <- annual_mean_by_region(slot_df("adultBiomass"), "biomass_fished")
  biomass_nofish <- annual_mean_by_region(slot_df("adultBiomass_nofish"), "biomass_nofish")
  depletion <- data.frame()
  if (nrow(biomass) && nrow(biomass_nofish)) {
    depletion <- merge(biomass, biomass_nofish, by = c("year", "area"), all = FALSE)
    depletion$value <- depletion$biomass_fished /
      pmax(depletion$biomass_nofish, .Machine$double.eps)
    depletion$quantity <- "Regional depletion"
    depletion <- depletion[, c("year", "area", "quantity", "value"), drop = FALSE]
  }

  recruitment <- normalize(slot_df("rec_region"))
  required <- c("year", "area", "data")
  if (nrow(recruitment) && all(required %in% names(recruitment))) {
    recruitment <- recruitment[
      is.finite(recruitment$year) & is.finite(recruitment$area) & is.finite(recruitment$data),
      , drop = FALSE
    ]
    recruitment <- stats::aggregate(data ~ year + area, data = recruitment, FUN = sum, na.rm = TRUE)
    names(recruitment)[names(recruitment) == "data"] <- "value"
    recruitment$value <- recruitment$value / 1e6
    recruitment$quantity <- "Regional recruitment"
    recruitment <- recruitment[, c("year", "area", "quantity", "value"), drop = FALSE]
  } else {
    recruitment <- data.frame()
  }

  pieces <- Filter(function(x) is.data.frame(x) && nrow(x), list(depletion, recruitment))
  if (!length(pieces)) return(data.frame())
  out <- do.call(rbind, pieces)
  out$region <- paste("Region", format(out$area, trim = TRUE, scientific = FALSE))
  out <- out[is.finite(out$year) & is.finite(out$value), , drop = FALSE]
  rownames(out) <- NULL
  out[, c("year", "region", "quantity", "value"), drop = FALSE]
}

mfclshiny_jitter_payload_regional <- function(payload, source_file = "") {
  if (!is.list(payload)) return(data.frame())
  direct <- payload$regional_quantities
  if (is.data.frame(direct) && all(c("year", "region", "quantity", "value") %in% names(direct))) {
    return(direct)
  }
  folder <- if (nzchar(source_file)) dirname(source_file) else NULL
  hydrated <- tryCatch(
    mfclshiny_hydrate_model_payload(payload, folder = folder, roles = "RepOut"),
    error = function(e) payload
  )
  rep_obj <- tryCatch(hydrated$data$RepOut, error = function(e) NULL)
  if (is.null(rep_obj)) {
    rep_obj <- tryCatch(payload$object_cache$objects$RepOut, error = function(e) NULL)
  }
  mfclshiny_jitter_extract_regional_timeseries(rep_obj)
}

mfclshiny_jitter_collect_regional <- function(data) {
  if (!is.data.frame(data) || !nrow(data)) return(data.frame())
  if (!"is_base_fit_reference" %in% names(data)) data$is_base_fit_reference <- FALSE
  selected <- data[data$converged %in% TRUE | data$is_base_fit_reference %in% TRUE, , drop = FALSE]
  rows <- list()
  for (scenario in unique(selected$scenario)) {
    model_data <- selected[selected$scenario == scenario, , drop = FALSE]
    for (run_index in seq_len(nrow(model_data))) {
      source_file <- model_data$source_file[[run_index]]
      payload <- tryCatch(readRDS(source_file), error = function(e) NULL)
      regional <- mfclshiny_jitter_payload_regional(payload, source_file)
      if (!nrow(regional)) next
      regional$scenario <- scenario
      regional$model_label <- model_data$model_label[[run_index]]
      regional$run <- model_data$jitter_id[[run_index]]
      regional$seed <- model_data$seed[[run_index]]
      regional$is_reference <- FALSE
      regional$is_base_fit_reference <- model_data$is_base_fit_reference[[run_index]] %in% TRUE
      regional$display_label <- mfclshiny_jitter_first_text(
        model_data$display_label[[run_index]],
        if (regional$is_base_fit_reference[[1L]]) "Base fit reference" else paste("Seed", model_data$seed[[run_index]])
      )
      rows[[length(rows) + 1L]] <- regional
    }
    reference_files <- if ("reference_source_file" %in% names(model_data)) {
      unique(model_data$reference_source_file[nzchar(model_data$reference_source_file)])
    } else {
      character()
    }
    reference_file <- reference_files[file.exists(reference_files)][1L]
    if (length(reference_file) && !is.na(reference_file)) {
      payload <- tryCatch(readRDS(reference_file), error = function(e) NULL)
      regional <- mfclshiny_jitter_payload_regional(payload, reference_file)
      if (nrow(regional)) {
        regional$scenario <- scenario
        regional$model_label <- model_data$model_label[[1L]]
        regional$run <- NA_integer_
        regional$seed <- NA_integer_
        regional$is_reference <- TRUE
        regional$is_base_fit_reference <- FALSE
        regional$display_label <- "Reference model"
        rows[[length(rows) + 1L]] <- regional
      }
    }
  }
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  out <- out[is.finite(out$year) & is.finite(out$value), , drop = FALSE]
  rownames(out) <- NULL
  out
}

mfclshiny_jitter_summarise_derived <- function(data) {
  if (!is.data.frame(data) || !nrow(data)) return(data.frame())
  if (!"is_reference" %in% names(data)) data$is_reference <- FALSE
  data <- data[!data$is_reference %in% TRUE & is.finite(data$value), , drop = FALSE]
  if (!nrow(data)) return(data.frame())
  group_columns <- intersect(
    c("scenario", "model_label", "quantity", "region", "year"),
    names(data)
  )
  groups <- split(
    seq_len(nrow(data)),
    interaction(data[group_columns], drop = TRUE, lex.order = TRUE)
  )
  rows <- lapply(groups, function(index) {
    values <- data$value[index]
    probabilities <- stats::quantile(
      values,
      probs = c(0.025, 0.10, 0.25, 0.50, 0.75, 0.90, 0.975),
      names = FALSE,
      type = 7,
      na.rm = TRUE
    )
    row <- data[index[[1L]], group_columns, drop = FALSE]
    row$q025 <- probabilities[[1L]]
    row$q10 <- probabilities[[2L]]
    row$q25 <- probabilities[[3L]]
    row$q50 <- probabilities[[4L]]
    row$q75 <- probabilities[[5L]]
    row$q90 <- probabilities[[6L]]
    row$q975 <- probabilities[[7L]]
    row$n_runs <- length(unique(data$run[index]))
    row
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result[order(result$quantity, result$year), , drop = FALSE]
}

mfclshiny_jitter_plot_derived_individual <- function(data,
                                                      reference_label,
                                                      base_label,
                                                      reference_colour,
                                                      base_colour) {
  quantity_levels <- c(
    "Depletion",
    "Fishing mortality",
    "Recruitment",
    "Spawning potential"
  )
  quantity_labeller <- ggplot2::as_labeller(
    c(
      "Depletion" = "bold(SB / SB[plain(F == 0)])",
      "Fishing mortality" = "bolditalic(F)~plain('(year')^{-1}*plain(')')",
      "Recruitment" = "bold(Recruitment)~plain('(millions of fish)')",
      "Spawning potential" = "bold('Spawning potential')~group('(', 10^3~plain(MT), ')')"
    ),
    default = ggplot2::label_parsed
  )
  data$quantity <- factor(as.character(data$quantity), levels = quantity_levels)
  if (!"is_reference" %in% names(data)) data$is_reference <- FALSE
  if (!"is_base_fit_reference" %in% names(data)) data$is_base_fit_reference <- FALSE
  reference_data <- data[data$is_reference %in% TRUE, , drop = FALSE]
  base_reference_data <- data[
    !data$is_reference %in% TRUE & data$is_base_fit_reference %in% TRUE,
    , drop = FALSE
  ]
  jitter_data <- data[
    !data$is_reference %in% TRUE & !data$is_base_fit_reference %in% TRUE,
    , drop = FALSE
  ]
  if (!nrow(jitter_data)) {
    stop("No converged jitter trajectories were available.", call. = FALSE)
  }
  jitter_data$run_id <- factor(as.character(jitter_data$run))
  reference_data$highlight <- reference_label
  base_reference_data$highlight <- base_label
  present_quantities <- quantity_levels[quantity_levels %in% as.character(data$quantity)]
  axis_years <- data$year[is.finite(data$year)]
  axis_data <- data.frame(
    quantity = factor(present_quantities, levels = quantity_levels),
    year = min(axis_years, na.rm = TRUE),
    value = 0,
    stringsAsFactors = FALSE
  )
  if (quantity_levels[[1L]] %in% present_quantities) {
    depletion_max <- max(
      data$value[data$quantity == quantity_levels[[1L]]],
      na.rm = TRUE
    )
    axis_data <- rbind(
      axis_data,
      data.frame(
        quantity = factor(quantity_levels[[1L]], levels = quantity_levels),
        year = min(axis_years, na.rm = TRUE),
        value = max(1, depletion_max),
        stringsAsFactors = FALSE
      )
    )
  }
  depletion_reference <- data.frame(
    quantity = factor(rep(quantity_levels[[1L]], 2L), levels = quantity_levels),
    yintercept = c(0.2, 0.5)
  )
  highlight_breaks <- c(base_label, reference_label)
  highlight_values <- stats::setNames(c(base_colour, reference_colour), highlight_breaks)

  ggplot2::ggplot() +
    ggplot2::geom_blank(
      data = axis_data,
      ggplot2::aes(x = .data$year, y = .data$value),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      data = depletion_reference[1L, , drop = FALSE],
      ggplot2::aes(yintercept = .data$yintercept),
      inherit.aes = FALSE,
      colour = "#B73E3E", linetype = "dashed", linewidth = 0.42
    ) +
    ggplot2::geom_hline(
      data = depletion_reference[2L, , drop = FALSE],
      ggplot2::aes(yintercept = .data$yintercept),
      inherit.aes = FALSE,
      colour = "#3F8F53", linetype = "dashed", linewidth = 0.42
    ) +
    ggplot2::geom_line(
      data = jitter_data,
      ggplot2::aes(x = .data$year, y = .data$value, group = .data$run_id),
      colour = "#70797F", linewidth = 0.42, alpha = 0.74,
      lineend = "round", inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = reference_data,
      ggplot2::aes(
        x = .data$year, y = .data$value, group = 1,
        colour = .data$highlight
      ),
      linewidth = 0.70, lineend = "round", inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = base_reference_data,
      ggplot2::aes(
        x = .data$year, y = .data$value,
        group = interaction(.data$run, .data$quantity),
        colour = .data$highlight
      ),
      linewidth = 0.74, lineend = "round", inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::scale_colour_manual(
      values = highlight_values,
      breaks = highlight_breaks,
      name = NULL,
      guide = ggplot2::guide_legend(override.aes = list(linewidth = c(0.74, 0.70)))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(values) {
        vapply(
          values,
          function(value) format(signif(value, 4L), trim = TRUE, scientific = FALSE, big.mark = ","),
          character(1)
        )
      },
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.045))
    ) +
    ggplot2::facet_wrap(
      ~quantity,
      scales = "free_y",
      ncol = 2,
      strip.position = "left",
      labeller = ggplot2::labeller(quantity = quantity_labeller)
    ) +
    ggplot2::labs(x = "Year", y = NULL) +
    ggplot2::theme_bw(base_size = 12.5, base_family = "serif") +
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        size = 9.5, colour = "#172B3A",
        margin = ggplot2::margin(t = 5, r = 8, b = 5, l = 3)
      ),
      strip.text.y.left = ggplot2::element_text(angle = 90),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "#263238", fill = NA, linewidth = 0.42),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E4E7EB", linewidth = 0.24),
      axis.text = ggplot2::element_text(size = 10.5, color = "#263238"),
      axis.title = ggplot2::element_text(size = 11.5),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 7)),
      axis.ticks = ggplot2::element_line(linewidth = 0.42, colour = "#263238"),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 9.5),
      legend.margin = ggplot2::margin(t = -2),
      legend.key.height = grid::unit(0.42, "cm"),
      panel.spacing = grid::unit(1.1, "lines"),
      aspect.ratio = 0.47,
      plot.margin = ggplot2::margin(8, 12, 8, 10)
    )
}

mfclshiny_jitter_plot_derived <- function(data,
                                          trajectory_style = c("distribution", "individual"),
                                          reference_label = "Reference model",
                                          base_label = "Attached base fit",
                                          reference_colour = "#C62828",
                                          base_colour = "#111827") {
  trajectory_style <- match.arg(trajectory_style)
  if (identical(trajectory_style, "individual")) {
    return(mfclshiny_jitter_plot_derived_individual(
      data,
      reference_label = reference_label,
      base_label = base_label,
      reference_colour = reference_colour,
      base_colour = base_colour
    ))
  }
  quantity_levels <- c(
    "Depletion",
    "Fishing mortality",
    "Recruitment",
    "Spawning potential"
  )
  quantity_labeller <- ggplot2::as_labeller(
    c(
      "Depletion" = "bold(SB / SB[plain(F == 0)])",
      "Fishing mortality" = "bolditalic(F)~plain('(year')^{-1}*plain(')')",
      "Recruitment" = "bold(Recruitment)~plain('(millions of fish)')",
      "Spawning potential" = "bold('Spawning potential')~group('(', 10^3~plain(MT), ')')"
    ),
    default = ggplot2::label_parsed
  )
  data$quantity <- factor(as.character(data$quantity), levels = quantity_levels)
  if (!"is_reference" %in% names(data)) data$is_reference <- FALSE
  if (!"is_base_fit_reference" %in% names(data)) data$is_base_fit_reference <- FALSE
  reference_data <- data[data$is_reference %in% TRUE, , drop = FALSE]
  base_reference_data <- data[
    !data$is_reference %in% TRUE & data$is_base_fit_reference %in% TRUE,
    ,
    drop = FALSE
  ]
  jitter_data <- data[
    !data$is_reference %in% TRUE & !data$is_base_fit_reference %in% TRUE,
    ,
    drop = FALSE
  ]
  jitter_data$run_id <- factor(as.character(jitter_data$run))
  summary_data <- mfclshiny_jitter_summarise_derived(jitter_data)
  summary_data$quantity <- factor(as.character(summary_data$quantity), levels = quantity_levels)
  present_quantities <- quantity_levels[quantity_levels %in% as.character(data$quantity)]
  axis_values <- c(summary_data$q975, reference_data$value, base_reference_data$value)
  axis_years <- c(summary_data$year, reference_data$year, base_reference_data$year)
  axis_data <- data.frame(
    quantity = factor(present_quantities, levels = quantity_levels),
    year = min(axis_years[is.finite(axis_years)], na.rm = TRUE),
    value = 0,
    stringsAsFactors = FALSE
  )
  if (quantity_levels[[1L]] %in% present_quantities) {
    depletion_max <- max(
      c(
        summary_data$q975[summary_data$quantity == quantity_levels[[1L]]],
        reference_data$value[reference_data$quantity == quantity_levels[[1L]]],
        base_reference_data$value[base_reference_data$quantity == quantity_levels[[1L]]]
      ),
      na.rm = TRUE
    )
    axis_data <- rbind(
      axis_data,
      data.frame(
        quantity = factor(quantity_levels[[1L]], levels = quantity_levels),
        year = min(axis_years[is.finite(axis_years)], na.rm = TRUE),
        value = max(1, depletion_max),
        stringsAsFactors = FALSE
      )
    )
  }
  depletion_reference <- data.frame(
    quantity = factor(rep(quantity_levels[[1L]], 2L), levels = quantity_levels),
    yintercept = c(0.2, 0.5)
  )
  axis_labels <- function(values) {
    vapply(
      values,
      function(value) format(signif(value, 4L), trim = TRUE, scientific = FALSE, big.mark = ","),
      character(1)
    )
  }
  colour_breaks <- c("Jitter median", "Reference model")
  if (nrow(base_reference_data)) colour_breaks <- c(colour_breaks, "Attached base fit")
  colour_widths <- c("Jitter median" = 0.72, "Reference model" = 0.86, "Attached base fit" = 0.9)
  colour_linetypes <- c("Jitter median" = "solid", "Reference model" = "solid", "Attached base fit" = "longdash")

  ggplot2::ggplot() +
    ggplot2::geom_blank(
      data = axis_data,
      ggplot2::aes(x = .data$year, y = .data$value),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      data = depletion_reference[1L, , drop = FALSE],
      ggplot2::aes(yintercept = .data$yintercept),
      inherit.aes = FALSE,
      colour = "#B73E3E",
      linetype = "dashed",
      linewidth = 0.42
    ) +
    ggplot2::geom_hline(
      data = depletion_reference[2L, , drop = FALSE],
      ggplot2::aes(yintercept = .data$yintercept),
      inherit.aes = FALSE,
      colour = "#3F8F53",
      linetype = "dashed",
      linewidth = 0.42
    ) +
    ggplot2::geom_ribbon(
      data = summary_data,
      ggplot2::aes(
        x = .data$year,
        ymin = .data$q025,
        ymax = .data$q975,
        fill = "Central 95%"
      ),
      linewidth = 0,
      inherit.aes = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::geom_ribbon(
      data = summary_data,
      ggplot2::aes(
        x = .data$year,
        ymin = .data$q10,
        ymax = .data$q90,
        fill = "Central 80%"
      ),
      linewidth = 0,
      inherit.aes = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::geom_ribbon(
      data = summary_data,
      ggplot2::aes(
        x = .data$year,
        ymin = .data$q25,
        ymax = .data$q75,
        fill = "Central 50%"
      ),
      linewidth = 0,
      inherit.aes = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = jitter_data,
      ggplot2::aes(
        x = .data$year,
        y = .data$value,
        group = .data$run_id
      ),
      colour = "#425B6D",
      linewidth = 0.27,
      alpha = 0.12,
      lineend = "round",
      inherit.aes = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = summary_data,
      ggplot2::aes(
        x = .data$year,
        y = .data$q50,
        colour = "Jitter median"
      ),
      linewidth = 0.72,
      lineend = "round",
      inherit.aes = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = reference_data,
      ggplot2::aes(
        x = .data$year,
        y = .data$value,
        group = 1,
        colour = "Reference model"
      ),
      linewidth = 0.86,
      lineend = "round",
      inherit.aes = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = base_reference_data,
      ggplot2::aes(
        x = .data$year,
        y = .data$value,
        group = interaction(.data$run, .data$quantity),
        colour = "Attached base fit"
      ),
      linewidth = 0.9,
      linetype = "longdash",
      lineend = "round",
      inherit.aes = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Central 95%" = "#DCEAF2",
        "Central 80%" = "#AACBDD",
        "Central 50%" = "#70A4BE"
      ),
      breaks = c("Central 50%", "Central 80%", "Central 95%"),
      name = "Pointwise jitter distribution",
      guide = ggplot2::guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 1,
        byrow = TRUE
      )
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "Jitter median" = "#173F5F",
        "Reference model" = "#C62828",
        "Attached base fit" = "#111827"
      ),
      breaks = colour_breaks,
      name = NULL,
      guide = ggplot2::guide_legend(
        override.aes = list(
          linewidth = unname(colour_widths[colour_breaks]),
          linetype = unname(colour_linetypes[colour_breaks])
        )
      )
    ) +
    ggplot2::scale_y_continuous(
      labels = axis_labels,
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.045))
    ) +
    ggplot2::facet_wrap(
      ~quantity,
      scales = "free_y",
      ncol = 2,
      strip.position = "left",
      labeller = ggplot2::labeller(quantity = quantity_labeller)
    ) +
    ggplot2::labs(x = "Year", y = NULL) +
    ggplot2::theme_bw(base_size = 12.5, base_family = "serif") +
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        size = 9.5,
        colour = "#172B3A",
        margin = ggplot2::margin(t = 5, r = 8, b = 5, l = 3)
      ),
      strip.text.y.left = ggplot2::element_text(angle = 90),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "#263238", fill = NA, linewidth = 0.42),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E4E7EB", linewidth = 0.24),
      axis.text = ggplot2::element_text(size = 10.5, color = "#263238"),
      axis.title = ggplot2::element_text(size = 11.5),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 7)),
      axis.ticks = ggplot2::element_line(linewidth = 0.42, colour = "#263238"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 10.5, face = "bold"),
      legend.text = ggplot2::element_text(size = 9.5),
      legend.margin = ggplot2::margin(t = -2),
      legend.box = "vertical",
      legend.key.height = grid::unit(0.42, "cm"),
      panel.spacing = grid::unit(1.1, "lines"),
      aspect.ratio = 0.47,
      plot.margin = ggplot2::margin(8, 12, 8, 10)
    )
}

mfclshiny_jitter_plot_regional_individual <- function(data,
                                                       quantity,
                                                       reference_label,
                                                       base_label,
                                                       reference_colour,
                                                       base_colour) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data <- data[data$quantity == quantity & is.finite(data$value), , drop = FALSE]
  if (!nrow(data)) stop("No ", tolower(quantity), " data were provided.", call. = FALSE)
  if (!"is_reference" %in% names(data)) data$is_reference <- FALSE
  if (!"is_base_fit_reference" %in% names(data)) data$is_base_fit_reference <- FALSE
  region_levels <- unique(as.character(data$region))
  region_numbers <- suppressWarnings(as.numeric(sub(".*?([0-9]+)$", "\\1", region_levels)))
  data$region <- factor(
    as.character(data$region),
    levels = region_levels[order(!is.finite(region_numbers), region_numbers, region_levels)]
  )
  reference_data <- data[data$is_reference %in% TRUE, , drop = FALSE]
  base_reference_data <- data[
    !data$is_reference %in% TRUE & data$is_base_fit_reference %in% TRUE,
    , drop = FALSE
  ]
  jitter_data <- data[
    !data$is_reference %in% TRUE & !data$is_base_fit_reference %in% TRUE,
    , drop = FALSE
  ]
  if (!nrow(jitter_data)) {
    stop("No converged jitter trajectories were available for ", tolower(quantity), ".", call. = FALSE)
  }
  jitter_data$run_id <- factor(as.character(jitter_data$run))
  reference_data$highlight <- reference_label
  base_reference_data$highlight <- base_label
  depletion <- identical(quantity, "Regional depletion")
  y_label <- if (depletion) bquote(bold(SB / SB[plain(F == 0)])) else "Recruitment (Millions)"
  highlight_breaks <- c(base_label, reference_label)
  highlight_values <- stats::setNames(c(base_colour, reference_colour), highlight_breaks)

  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = jitter_data,
      ggplot2::aes(
        x = .data$year, y = .data$value,
        group = interaction(.data$run_id, .data$region)
      ),
      colour = "#70797F", linewidth = 0.42, alpha = 0.74,
      lineend = "round", inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = reference_data,
      ggplot2::aes(
        x = .data$year, y = .data$value, group = .data$region,
        colour = .data$highlight
      ),
      linewidth = 0.70, lineend = "round", inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = base_reference_data,
      ggplot2::aes(
        x = .data$year, y = .data$value, group = .data$region,
        colour = .data$highlight
      ),
      linewidth = 0.74, lineend = "round", inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::scale_colour_manual(
      values = highlight_values,
      breaks = highlight_breaks,
      name = NULL,
      guide = ggplot2::guide_legend(override.aes = list(linewidth = c(0.74, 0.70)))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      labels = function(values) format(signif(values, 4L), trim = TRUE, scientific = FALSE, big.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.045))
    ) +
    ggplot2::facet_wrap(~region, scales = if (depletion) "fixed" else "free_y", ncol = 3) +
    ggplot2::labs(x = "Year", y = y_label) +
    ggplot2::theme_bw(base_size = 12.5, base_family = "serif") +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 10.5, face = "bold", colour = "#172B3A"),
      strip.background = ggplot2::element_rect(fill = "#EAF2F5", colour = "#B9CBD2", linewidth = 0.35),
      panel.border = ggplot2::element_rect(color = "#263238", fill = NA, linewidth = 0.42),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E4E7EB", linewidth = 0.24),
      axis.text = ggplot2::element_text(size = 9.5, color = "#263238"),
      axis.title = ggplot2::element_text(size = 11.5),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 9.5),
      panel.spacing = grid::unit(0.85, "lines"),
      plot.margin = ggplot2::margin(8, 12, 8, 10)
    )
  if (depletion) {
    plot <- plot +
      ggplot2::geom_hline(yintercept = 0.2, colour = "#B73E3E", linetype = "dashed", linewidth = 0.42) +
      ggplot2::geom_hline(yintercept = 0.5, colour = "#3F8F53", linetype = "dashed", linewidth = 0.42) +
      ggplot2::coord_cartesian(ylim = c(0, max(1, data$value, na.rm = TRUE)))
  }
  plot
}

mfclshiny_jitter_plot_regional <- function(data,
                                           quantity = c("Regional depletion", "Regional recruitment"),
                                           trajectory_style = c("distribution", "individual"),
                                           reference_label = "Reference model",
                                           base_label = "Attached base fit",
                                           reference_colour = "#C62828",
                                           base_colour = "#111827") {
  quantity <- match.arg(quantity)
  trajectory_style <- match.arg(trajectory_style)
  if (identical(trajectory_style, "individual")) {
    return(mfclshiny_jitter_plot_regional_individual(
      data,
      quantity = quantity,
      reference_label = reference_label,
      base_label = base_label,
      reference_colour = reference_colour,
      base_colour = base_colour
    ))
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data <- data[data$quantity == quantity & is.finite(data$value), , drop = FALSE]
  if (!nrow(data)) stop("No ", tolower(quantity), " data were provided.", call. = FALSE)
  if (!"is_reference" %in% names(data)) data$is_reference <- FALSE
  if (!"is_base_fit_reference" %in% names(data)) data$is_base_fit_reference <- FALSE
  region_levels <- unique(as.character(data$region))
  region_numbers <- suppressWarnings(as.numeric(sub(".*?([0-9]+)$", "\\1", region_levels)))
  data$region <- factor(
    as.character(data$region),
    levels = region_levels[order(!is.finite(region_numbers), region_numbers, region_levels)]
  )
  reference_data <- data[data$is_reference %in% TRUE, , drop = FALSE]
  base_reference_data <- data[
    !data$is_reference %in% TRUE & data$is_base_fit_reference %in% TRUE,
    , drop = FALSE
  ]
  jitter_data <- data[
    !data$is_reference %in% TRUE & !data$is_base_fit_reference %in% TRUE,
    , drop = FALSE
  ]
  if (!nrow(jitter_data)) stop("No converged jitter trajectories were available for ", tolower(quantity), ".", call. = FALSE)
  jitter_data$run_id <- factor(as.character(jitter_data$run))
  summary_data <- mfclshiny_jitter_summarise_derived(jitter_data)
  summary_data$region <- factor(as.character(summary_data$region), levels = levels(data$region))
  depletion <- identical(quantity, "Regional depletion")
  y_label <- if (depletion) bquote(bold(SB / SB[plain(F == 0)])) else "Recruitment (Millions)"
  colour_breaks <- c("Jitter median", "Reference model")
  if (nrow(base_reference_data)) colour_breaks <- c(colour_breaks, "Attached base fit")
  colour_widths <- c("Jitter median" = 0.72, "Reference model" = 0.86, "Attached base fit" = 0.9)
  colour_linetypes <- c("Jitter median" = "solid", "Reference model" = "solid", "Attached base fit" = "longdash")

  plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = summary_data,
      ggplot2::aes(x = .data$year, ymin = .data$q025, ymax = .data$q975, fill = "Central 95%"),
      linewidth = 0, inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_ribbon(
      data = summary_data,
      ggplot2::aes(x = .data$year, ymin = .data$q10, ymax = .data$q90, fill = "Central 80%"),
      linewidth = 0, inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_ribbon(
      data = summary_data,
      ggplot2::aes(x = .data$year, ymin = .data$q25, ymax = .data$q75, fill = "Central 50%"),
      linewidth = 0, inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = jitter_data,
      ggplot2::aes(x = .data$year, y = .data$value, group = interaction(.data$run_id, .data$region)),
      colour = "#425B6D", linewidth = 0.25, alpha = 0.10,
      lineend = "round", inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = summary_data,
      ggplot2::aes(x = .data$year, y = .data$q50, colour = "Jitter median"),
      linewidth = 0.72, lineend = "round", inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = reference_data,
      ggplot2::aes(x = .data$year, y = .data$value, group = .data$region, colour = "Reference model"),
      linewidth = 0.86, lineend = "round", inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = base_reference_data,
      ggplot2::aes(x = .data$year, y = .data$value, group = .data$region, colour = "Attached base fit"),
      linewidth = 0.9, linetype = "longdash", lineend = "round",
      inherit.aes = FALSE, na.rm = TRUE
    ) +
    ggplot2::scale_fill_manual(
      values = c("Central 95%" = "#DCEAF2", "Central 80%" = "#AACBDD", "Central 50%" = "#70A4BE"),
      breaks = c("Central 50%", "Central 80%", "Central 95%"),
      name = "Pointwise jitter distribution",
      guide = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1, byrow = TRUE)
    ) +
    ggplot2::scale_colour_manual(
      values = c("Jitter median" = "#173F5F", "Reference model" = "#C62828", "Attached base fit" = "#111827"),
      breaks = colour_breaks,
      name = NULL,
      guide = ggplot2::guide_legend(
        override.aes = list(
          linewidth = unname(colour_widths[colour_breaks]),
          linetype = unname(colour_linetypes[colour_breaks])
        )
      )
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      labels = function(values) format(signif(values, 4L), trim = TRUE, scientific = FALSE, big.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.045))
    ) +
    ggplot2::facet_wrap(~region, scales = if (depletion) "fixed" else "free_y", ncol = 3) +
    ggplot2::labs(x = "Year", y = y_label) +
    ggplot2::theme_bw(base_size = 12.5, base_family = "serif") +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 10.5, face = "bold", colour = "#172B3A"),
      strip.background = ggplot2::element_rect(fill = "#EAF2F5", colour = "#B9CBD2", linewidth = 0.35),
      panel.border = ggplot2::element_rect(color = "#263238", fill = NA, linewidth = 0.42),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E4E7EB", linewidth = 0.24),
      axis.text = ggplot2::element_text(size = 9.5, color = "#263238"),
      axis.title = ggplot2::element_text(size = 11.5),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 10.5, face = "bold"),
      legend.text = ggplot2::element_text(size = 9.5),
      legend.box = "vertical",
      panel.spacing = grid::unit(0.85, "lines"),
      plot.margin = ggplot2::margin(8, 12, 8, 10)
    )
  if (depletion) {
    plot <- plot +
      ggplot2::geom_hline(yintercept = 0.2, colour = "#B73E3E", linetype = "dashed", linewidth = 0.42) +
      ggplot2::geom_hline(yintercept = 0.5, colour = "#3F8F53", linetype = "dashed", linewidth = 0.42) +
      ggplot2::coord_cartesian(ylim = c(0, max(1, data$value, na.rm = TRUE)))
  }
  plot
}

mfclshiny_jitter_image_data <- function(file) {
  size <- file.info(file)$size
  raw <- readBin(file, what = "raw", n = size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw))
}

mfclshiny_jitter_method_items <- function(data, grad_reference) {
  data <- mfclshiny_jitter_counted_data(data)
  scenarios <- unique(data$scenario)
  specifications <- vapply(scenarios, function(scenario) {
    model_data <- data[data$scenario == scenario, , drop = FALSE]
    model_name <- mfclshiny_jitter_first_text(model_data$model_label, scenario)
    model_name <- sub(" fitted model$", "", model_name, ignore.case = TRUE)
    cv <- sort(unique(model_data$jitter_cv[is.finite(model_data$jitter_cv)]))
    cv_text <- if (!length(cv)) {
      "CV not recorded"
    } else if (length(cv) == 1L) {
      paste0("CV = ", format(signif(cv, 3L), trim = TRUE, scientific = FALSE))
    } else {
      paste0(
        "CV = ", format(signif(min(cv), 3L), trim = TRUE, scientific = FALSE),
        " to ", format(signif(max(cv), 3L), trim = TRUE, scientific = FALSE)
      )
    }
    phases <- if ("phase_count" %in% names(model_data)) {
      sort(unique(model_data$phase_count[is.finite(model_data$phase_count)]))
    } else {
      integer()
    }
    phase_text <- if (!length(phases)) {
      "configured phase count not recorded"
    } else if (length(phases) == 1L) {
      paste0(phases[[1L]], " estimation phases")
    } else {
      paste0(min(phases), " to ", max(phases), " estimation phases")
    }
    paste0(model_name, ": ", nrow(model_data), " runs, ", cv_text, ", ", phase_text)
  }, character(1), USE.NAMES = FALSE)
  threshold_values <- if ("grad_reference" %in% names(data)) {
    suppressWarnings(as.numeric(data$grad_reference))
  } else {
    numeric()
  }
  override <- mfclshiny_jitter_valid_threshold(grad_reference)
  if (is.finite(override)) threshold_values <- override
  threshold_values <- sort(unique(threshold_values[is.finite(threshold_values) & threshold_values > 0]))
  threshold_text <- if (length(threshold_values) == 1L) {
    paste0(
      "Runs with MGC <= ",
      formatC(threshold_values[[1L]], format = "g", digits = 3),
      " were retained"
    )
  } else if (length(threshold_values) > 1L) {
    "Runs satisfying their model-specific recorded MGC threshold were retained"
  } else {
    "Runs carrying a successful recorded convergence status were retained"
  }
  metadata_values <- function(name) {
    if (!name %in% names(data)) return(character())
    values <- trimws(as.character(data[[name]]))
    unique(values[!is.na(values) & nzchar(values)])
  }
  reference_stages <- metadata_values("jitter_reference_stage")
  parameter_scopes <- metadata_values("jitter_parameter_scope")
  designs <- metadata_values("jitter_design")
  phase1_workflow <- length(reference_stages) > 0L &&
    all(reference_stages == "post_phase1")
  fitted_workflow <- length(reference_stages) > 0L &&
    all(reference_stages == "fitted_estimate")
  active_parameters <- length(parameter_scopes) > 0L &&
    all(parameter_scopes == "active_independent_variables")
  structure_aware <- length(designs) > 0L &&
    all(grepl("^structure_aware", designs))
  scope_text <- if (active_parameters) {
    " Only parameters estimated in the completed reference model were included."
  } else {
    ""
  }
  schedule_text <- if (phase1_workflow) {
    paste0(
      "An estimation phase is one step in MFCL's staged fitting schedule, during which additional parameters can become estimable. After Phase 1, values for parameters already available and estimated in the completed reference model were perturbed. As each remaining phase began, parameters appearing for the first time were also perturbed before optimisation. Each run then completed the same full phase schedule as its reference model."
    )
  } else if (fitted_workflow) {
    paste0(
      "Starting values were perturbed around the fitted reference estimates, and each run was optimised using its recorded fitting configuration."
    )
  } else {
    paste0(
      "Each perturbed set was fitted using the estimation configuration recorded with its reference model."
    )
  }
  scale_text <- if (structure_aware) {
    paste0(
      "The stated CV controlled perturbation size on a parameter-appropriate scale: mean-preserving proportional changes for positive parameters, additive normal changes for unconstrained or near-zero parameters based on their parameter-family scale, and bound-preserving changes for bounded parameters."
    )
  } else {
    paste0(
      "The stated CV controlled perturbation size according to the jitter design recorded with each run."
    )
  }
  c(
    Design = paste0(
      "Starting values were randomly perturbed to test solution stability (",
      paste(specifications, collapse = "; "), ").", scope_text
    ),
    `Fitting schedule` = schedule_text,
    `Perturbation scale` = scale_text,
    Evaluation = paste0(
      "The maximum gradient component (MGC) is the largest absolute component of the objective-function gradient with respect to estimated parameters; smaller values indicate closer approach to a stationary point. ",
      threshold_text,
      ", and their objective function values and key derived quantities were compared with the unperturbed reference model to assess solution stability and identify alternative minima."
    )
  )
}

mfclshiny_jitter_method_text <- function(data, grad_reference) {
  paste(unname(mfclshiny_jitter_method_items(data, grad_reference)), collapse = " ")
}

mfclshiny_jitter_results_records <- function(data) {
  data <- mfclshiny_jitter_counted_data(data)
  records <- lapply(unique(data$scenario), function(scenario) {
    model_data <- data[data$scenario == scenario, , drop = FALSE]
    model_name <- mfclshiny_jitter_first_text(model_data$model_label, scenario)
    model_name <- sub(" fitted model$", "", model_name, ignore.case = TRUE)
    converged <- model_data[model_data$converged %in% TRUE, , drop = FALSE]
    total <- nrow(model_data)
    n_converged <- nrow(converged)
    rate <- if (total) round(100 * n_converged / total) else 0
    lead <- paste0(n_converged, " of ", total, " runs converged (", rate, "%).")
    finite <- converged[is.finite(converged$obj_fun), , drop = FALSE]
    if (!nrow(finite)) {
      return(data.frame(model = model_name, text = lead, stringsAsFactors = FALSE))
    }
    best <- min(finite$obj_fun)
    reference <- mfclshiny_jitter_first_number(model_data$ref_obj)
    finite_mgc <- abs(finite$max_grad[is.finite(finite$max_grad)])
    minimum_mgc <- if (length(finite_mgc)) min(finite_mgc) else NA_real_
    objective_text <- paste0(
      " The lowest objective function value among converged runs was ",
      formatC(best, format = "f", digits = 1, big.mark = ",")
    )
    if (is.finite(reference)) {
      delta <- best - reference
      tolerance <- 0.05
      improved <- sum(is.finite(finite$obj_fun) & finite$obj_fun < reference - tolerance)
      comparison <- if (delta < -tolerance) {
        paste0(
          ", ", formatC(abs(delta), format = "f", digits = 1, big.mark = ","),
          " units lower than the reference model; ", improved, " converged ",
          if (improved == 1L) "run improved" else "runs improved", " on the reference."
        )
      } else if (delta > tolerance) {
        paste0(
          ", ", formatC(delta, format = "f", digits = 1, big.mark = ","),
          " units higher than the reference model; no converged run improved on the reference."
        )
      } else {
        ", effectively matching the reference model."
      }
      objective_text <- paste0(objective_text, comparison)
    } else {
      objective_text <- paste0(objective_text, ".")
    }
    mgc_text <- if (is.finite(minimum_mgc)) {
      paste0(" The minimum MGC was ", formatC(minimum_mgc, format = "e", digits = 2), ".")
    } else ""
    data.frame(
      model = model_name,
      text = paste0(lead, objective_text, mgc_text),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, records)
}

mfclshiny_jitter_interpretation_records <- function(data) {
  data <- mfclshiny_jitter_counted_data(data)
  records <- lapply(unique(data$scenario), function(scenario) {
    model_data <- data[data$scenario == scenario, , drop = FALSE]
    model_name <- mfclshiny_jitter_first_text(model_data$model_label, scenario)
    model_name <- sub(" fitted model$", "", model_name, ignore.case = TRUE)
    converged <- model_data[model_data$converged %in% TRUE, , drop = FALSE]
    total <- nrow(model_data)
    n_converged <- nrow(converged)
    n_excluded <- total - n_converged
    finite <- converged[
      is.finite(converged$obj_fun) & is.finite(converged$ref_obj),
      , drop = FALSE
    ]
    if (!n_converged) {
      text <- paste0(
        "No run met the convergence criterion, so sensitivity to the tested starting-value perturbations could not be evaluated from this set."
      )
    } else if (!nrow(finite)) {
      text <- paste0(
        "Converged runs were available, but finite objective-function comparisons with the unperturbed fit were not available."
      )
    } else {
      n_improved <- sum(finite$obj_fun < finite$ref_obj)
      if (n_improved > 0L) {
        text <- paste0(
          n_improved, " of ", nrow(finite), " converged ",
          if (nrow(finite) == 1L) "run" else "runs",
          " with finite objective-function values attained a lower value than the unperturbed fit. ",
          "At least one perturbed starting point therefore reached a lower objective-function minimum; the associated parameter estimates and derived quantities should be examined."
        )
      } else {
        text <- paste0(
          "None of the ", nrow(finite), " converged ",
          if (nrow(finite) == 1L) "run" else "runs",
          " with finite objective-function values improved on the unperturbed fit. ",
          "This supports numerical stability over the tested starting-value perturbations, but does not demonstrate that the global minimum was found."
        )
      }
    }
    if (n_excluded > 0L) {
      text <- paste0(
        text, " ", n_excluded, " ", if (n_excluded == 1L) "run" else "runs",
        " did not meet the convergence criterion and ",
        if (n_excluded == 1L) "was" else "were",
        " excluded from objective-function and derived-quantity comparisons."
      )
    }
    data.frame(model = model_name, text = text, stringsAsFactors = FALSE)
  })
  do.call(rbind, records)
}

mfclshiny_jitter_references_html <- function() {
  paste0(
    '<p>Carvalho, F. et al. (2021). A cookbook for using model diagnostics in integrated stock assessments. ',
    '<em>Fisheries Research</em>, 240, 105959. ',
    '<a href="https://doi.org/10.1016/j.fishres.2021.105959">https://doi.org/10.1016/j.fishres.2021.105959</a></p>',
    '<p>Subbey, S. (2018). Parameter estimation in stock assessment modelling: caveats with gradient-based algorithms. ',
    '<em>ICES Journal of Marine Science</em>, 75, 1553-1559. ',
    '<a href="https://doi.org/10.1093/icesjms/fsy044">https://doi.org/10.1093/icesjms/fsy044</a></p>'
  )
}

mfclshiny_jitter_references_latex <- function() {
  paste0(
    "\\paragraph{References.}\n\n",
    "Carvalho, F. et al. (2021). A cookbook for using model diagnostics in integrated stock assessments. ",
    "\\textit{Fisheries Research}, 240, 105959. ",
    "\\url{https://doi.org/10.1016/j.fishres.2021.105959}\n\n",
    "Subbey, S. (2018). Parameter estimation in stock assessment modelling: caveats with gradient-based algorithms. ",
    "\\textit{ICES Journal of Marine Science}, 75, 1553--1559. ",
    "\\url{https://doi.org/10.1093/icesjms/fsy044}"
  )
}

mfclshiny_jitter_references_bibtex <- function() {
  paste0(
    "@article{CarvalhoEtAl2021,\n",
    "  author = {Carvalho, Felipe and Winker, Henning and Courtney, Dean and Kapur, Maia and Kell, Laurence and Cardinale, Massimiliano and Schirripa, Michael and Kitakado, Toshihide and Yemane, Dawit and Piner, Kevin R. and Maunder, Mark N. and Taylor, Ian G. and Wetzel, Chantel R. and Doering, Kathryn and Johnson, Kelli F. and Methot, Richard D.},\n",
    "  title = {A cookbook for using model diagnostics in integrated stock assessments},\n",
    "  journal = {Fisheries Research},\n",
    "  year = {2021}, volume = {240}, pages = {105959},\n",
    "  doi = {10.1016/j.fishres.2021.105959}\n",
    "}\n\n",
    "@article{Subbey2018,\n",
    "  author = {Subbey, Sam},\n",
    "  title = {Parameter estimation in stock assessment modelling: caveats with gradient-based algorithms},\n",
    "  journal = {ICES Journal of Marine Science},\n",
    "  year = {2018}, volume = {75}, pages = {1553--1559},\n",
    "  doi = {10.1093/icesjms/fsy044}\n",
    "}"
  )
}

mfclshiny_jitter_results_html <- function(data) {
  records <- mfclshiny_jitter_results_records(data)
  interpretation <- mfclshiny_jitter_interpretation_records(data)
  interpretation_note <- paste0(
    "Multiple starting values diagnose sensitivity to local minima, but do not by themselves identify the global minimum. ",
    "The findings should be considered with convergence, fit to the data and other model diagnostics ",
    "(Subbey, 2018; Carvalho et al., 2021)."
  )
  paragraphs <- paste0(
    "<p><strong>", mfclshiny_jitter_html_escape(records$model),
    ".</strong> ", mfclshiny_jitter_html_escape(records$text), "</p>"
  )
  interpretation_paragraphs <- paste0(
    "<p><strong>", mfclshiny_jitter_html_escape(interpretation$model),
    ".</strong> ", mfclshiny_jitter_html_escape(interpretation$text), "</p>"
  )
  paste0(
    paste(paragraphs, collapse = ""),
    "<h3>Interpretation</h3>", paste(interpretation_paragraphs, collapse = ""),
    "<p class='interpretation-note'>", mfclshiny_jitter_html_escape(interpretation_note), "</p>",
    "<h3>References</h3>", mfclshiny_jitter_references_html()
  )
}

mfclshiny_jitter_latex_prose <- function(x) {
  pieces <- strsplit(as.character(x), "MGC <= ", fixed = TRUE)
  vapply(pieces, function(part) {
    paste(
      vapply(part, mfclshiny_jitter_latex_escape, character(1L), USE.NAMES = FALSE),
      collapse = "MGC $\\leq$ "
    )
  }, character(1L), USE.NAMES = FALSE)
}

mfclshiny_jitter_results_latex <- function(data) {
  records <- mfclshiny_jitter_results_records(data)
  interpretation <- mfclshiny_jitter_interpretation_records(data)
  interpretation_note <- paste0(
    "Multiple starting values diagnose sensitivity to local minima, but do not by themselves identify the global minimum. ",
    "The findings should be considered with convergence, fit to the data and other model diagnostics ",
    "(Subbey, 2018; Carvalho et al., 2021)."
  )
  paste0(
    "\\paragraph{Results.}\n\n",
    paste0(
      "\\textbf{", mfclshiny_jitter_latex_escape(records$model), ".} ",
      mfclshiny_jitter_latex_prose(records$text),
      collapse = "\n\n"
    ),
    "\n\n\\paragraph{Interpretation.}\n\n",
    paste0(
      "\\textbf{", mfclshiny_jitter_latex_escape(interpretation$model), ".} ",
      mfclshiny_jitter_latex_prose(interpretation$text),
      collapse = "\n\n"
    ),
    "\n\n", mfclshiny_jitter_latex_escape(interpretation_note),
    "\n\n", mfclshiny_jitter_references_latex()
  )
}

mfclshiny_jitter_regional_html_block <- function(file,
                                                 model_name,
                                                 slug,
                                                 metric,
                                                 inclusion_caption,
                                                 trajectory_style,
                                                 reference_label,
                                                 base_label) {
  if (is.null(file) || !length(file) || !file.exists(file)) return("")
  depletion <- identical(metric, "depletion")
  heading <- if (depletion) "Depletion by region" else "Recruitment by region"
  id <- paste0("regional-", metric, "-", slug)
  name <- paste0("jitter-regional-", metric, "-", slug, ".png")
  caption_id <- paste0("caption-regional-", metric, "-", slug)
  latex_id <- paste0("latex-caption-regional-", metric, "-", slug)
  short_name <- sub(" fitted model$", "", model_name, ignore.case = TRUE)
  definition <- if (depletion) {
    paste0(
      "Regional depletion is annual mean spawning potential in each model region under fitted fishing divided by the corresponding regional no-fishing value. ",
      "Dashed lines mark 0.2 and 0.5."
    )
  } else {
    "Regional recruitment is the annual sum across recruitment seasons within each model region, in millions of fish."
  }
  trajectory_caption <- if (identical(trajectory_style, "individual")) {
    paste0(
      "All included jitter trajectories are shown individually as grey lines. The ",
      base_label, " and ", reference_label,
      " are highlighted separately as identified in the legend. "
    )
  } else {
    paste0(
      "Bands show the pointwise central 50%, 80%, and 95% empirical ranges; ",
      "the dark-blue line is the jitter median, faint lines are individual included fits, ",
      "the red line is the reference model, and the black long-dashed line is the separately attached base fit. "
    )
  }
  caption <- paste0(
    heading, " across converged jitter runs for ", short_name, ". ", inclusion_caption, " ",
    trajectory_caption, definition
  )
  latex <- paste0(
    "\\caption{", mfclshiny_jitter_latex_prose(caption), "}\n",
    "\\label{fig:jitter-regional-", metric, "-", slug, "}"
  )
  paste0(
    '<div class="figure-block"><h4>', heading, '</h4><img id="', id,
    '" class="figure regional-figure" alt="', mfclshiny_jitter_html_escape(heading),
    ' for ', mfclshiny_jitter_html_escape(model_name), '" src="', mfclshiny_jitter_image_data(file),
    '"><figcaption id="', caption_id,
    '"><strong>Figure <span class="figure-number" contenteditable="true" spellcheck="false" title="Click to edit the figure number">XX</span>.</strong> ',
    mfclshiny_jitter_html_escape(caption), '</figcaption><pre id="', latex_id,
    '" class="copy-source">', mfclshiny_jitter_html_escape(latex),
    '</pre><div class="actions"><button onclick="copyFigure(\'', id, '\',\'', caption_id,
    '\',this)">Copy figure for Word</button><button onclick="saveImage(\'', id, '\',\'', name,
    '\',this)">Save PNG</button><button onclick="copyText(\'', latex_id,
    '\',this)">Copy LaTeX caption</button></div></div>'
  )
}

mfclshiny_jitter_write_html <- function(file,
                                        data,
                                        summary,
                                        model_pngs,
                                        derived_pngs,
                                        regional_pngs,
                                        table_dir,
                                        grad_reference,
                                        title = "Jitter diagnostics",
                                        trajectory_style = c("distribution", "individual"),
                                        reference_label = "Reference model",
                                        base_label = "Attached base fit",
                                        show_objective_reference_line = TRUE) {
  trajectory_style <- match.arg(trajectory_style)
  method_items <- mfclshiny_jitter_method_items(data, grad_reference)
  method_text <- paste(unname(method_items), collapse = " ")
  method_html <- paste0(
    '<ul class="method-list">',
    paste0(
      "<li><strong>", mfclshiny_jitter_html_escape(names(method_items)),
      ".</strong> ", mfclshiny_jitter_html_escape(unname(method_items)), "</li>",
      collapse = ""
    ),
    "</ul>"
  )
  results_html <- mfclshiny_jitter_results_html(data)
  method_latex <- paste0(
    "\\paragraph{Jitter analysis.}\n\n\\begin{itemize}\n",
    paste0(
      "\\item \\textbf{", mfclshiny_jitter_latex_escape(names(method_items)),
      ".} ", mfclshiny_jitter_latex_prose(unname(method_items)),
      collapse = "\n"
    ),
    "\n\\end{itemize}"
  )
  results_latex <- mfclshiny_jitter_results_latex(data)
  references_bibtex <- mfclshiny_jitter_references_bibtex()
  tab_buttons <- vapply(unique(data$scenario), function(model) {
    model_data <- data[data$scenario == model, , drop = FALSE]
    model_name <- mfclshiny_jitter_first_text(model_data$model_label, model)
    tab_id <- paste0("tab-model-", mfclshiny_jitter_slug(model))
    paste0(
      '<button class="tab-button" role="tab" onclick="showTab(\'', tab_id, '\',this)">',
      mfclshiny_jitter_html_escape(model_name), "</button>"
    )
  }, character(1), USE.NAMES = FALSE)
  model_sections <- vapply(unique(data$scenario), function(model) {
    model_data <- data[data$scenario == model, , drop = FALSE]
    model_name <- mfclshiny_jitter_first_text(model_data$model_label, model)
    display <- mfclshiny_jitter_display_table(model_data)
    table_caption <- mfclshiny_jitter_model_caption(model_data, grad_reference)
    image_file <- model_pngs[[model]]
    derived_file <- derived_pngs[[model]]
    model_regional_pngs <- regional_pngs[[model]]
    slug <- mfclshiny_jitter_slug(model)
    id <- paste0("table-", slug)
    image_id <- paste0("figure-", slug)
    image_name <- paste0("jitter-diagnostics-", slug, ".png")
    objective_caption_id <- paste0("caption-objective-", slug)
    objective_latex_caption_id <- paste0("latex-caption-objective-", slug)
    derived_id <- paste0("derived-", slug)
    derived_name <- paste0("jitter-derived-", slug, ".png")
    derived_caption_id <- paste0("caption-derived-", slug)
    derived_latex_caption_id <- paste0("latex-caption-derived-", slug)
    short_name <- sub(" fitted model$", "", model_name, ignore.case = TRUE)
    model_grad_reference <- mfclshiny_jitter_data_threshold(model_data, grad_reference)
    threshold_caption <- if (is.finite(model_grad_reference)) {
      paste0(
        "; the dotted vertical line marks the diagnostic MGC threshold (MGC <= ",
        mfclshiny_jitter_format_cell(model_grad_reference),
        ")"
      )
    } else {
      ""
    }
    included_runs <- sum(model_data$converged %in% TRUE)
    excluded_runs <- nrow(model_data) - included_runs
    inclusion_caption <- if (is.finite(model_grad_reference)) {
      paste0(
        "Only the ", included_runs, " successful run", if (included_runs == 1L) "" else "s",
        " meeting MGC <= ",
        mfclshiny_jitter_format_cell(model_grad_reference),
        " are included; ", excluded_runs,
        " unsuccessful or non-converged run", if (excluded_runs == 1L) " was" else "s were",
        " excluded."
      )
    } else {
      paste0(
        "Only the ", included_runs, " run", if (included_runs == 1L) "" else "s",
        " carrying a successful recorded convergence status ",
        if (included_runs == 1L) "is" else "are", " included; ",
        excluded_runs, " unsuccessful or non-converged run",
        if (excluded_runs == 1L) " was" else "s were", " excluded."
      )
    }
    objective_role_caption <- if (identical(trajectory_style, "individual")) {
      paste0(
        "Grey points are individual jitter runs. The highlighted points identify ",
        base_label, " and ", reference_label, " as shown in the legend",
        if (isTRUE(show_objective_reference_line)) "; the horizontal line marks the reference objective function value" else ""
      )
    } else {
      paste0(
        "Each point represents one jitter run. The dashed horizontal line and red diamond indicate ",
        "the reference objective function value and reference model, respectively"
      )
    }
    objective_caption <- paste0(
      "Jitter convergence diagnostics for ", short_name, ". ", objective_role_caption,
      threshold_caption, ". Lower objective function values indicate improved fit."
    )
    derived_trajectory_caption <- if (identical(trajectory_style, "individual")) {
      paste0(
        " All included jitter trajectories are shown individually as grey lines. The ",
        base_label, " and ", reference_label,
        " are highlighted separately as identified in the legend."
      )
    } else {
      paste0(
        " Bands show the pointwise empirical distribution across those equally weighted runs: ",
        "central 50%, 80%, and 95% ranges, with the dark-blue median line. Very faint grey-blue ",
        "lines show the included individual trajectories without giving a small number of extreme ",
        "runs the same visual weight as the central distribution. The red line shows the reference model."
      )
    }
    derived_caption <- paste0(
      "Derived quantities across converged jitter runs for ", short_name,
      ". ", inclusion_caption, derived_trajectory_caption,
      " Dashed lines in the depletion panel mark 0.2 and 0.5. Fishing mortality is expressed as an annual instantaneous rate, recruitment in millions of fish, and spawning potential in thousands of metric tonnes."
    )
    objective_latex_caption <- paste0(
      "\\caption{", mfclshiny_jitter_latex_prose(objective_caption), "}\n",
      "\\label{fig:jitter-objective-", slug, "}"
    )
    derived_latex_caption <- paste0(
      "\\caption{", mfclshiny_jitter_latex_prose(derived_caption), "}\n",
      "\\label{fig:jitter-derived-", slug, "}"
    )
    tex_file <- file.path(table_dir, paste0("jitter-", slug, ".tex"))
    latex <- if (file.exists(tex_file)) paste(readLines(tex_file, warn = FALSE), collapse = "\n") else ""
    derived_html <- if (!is.null(derived_file) && file.exists(derived_file)) paste0(
      '<div class="figure-block"><h3>Derived quantities</h3><img id="', derived_id,
      '" class="figure derived-figure" alt="Jitter derived quantities for ', mfclshiny_jitter_html_escape(model_name),
      '" src="', mfclshiny_jitter_image_data(derived_file), '"><figcaption id="', derived_caption_id, '"><strong>Figure <span class="figure-number" contenteditable="true" spellcheck="false" title="Click to edit the figure number">XX</span>.</strong> ',
      mfclshiny_jitter_html_escape(derived_caption), '</figcaption><pre id="', derived_latex_caption_id, '" class="copy-source">', mfclshiny_jitter_html_escape(derived_latex_caption), '</pre><div class="actions"><button onclick="copyFigure(\'',
      derived_id, '\',\'', derived_caption_id, '\',this)">Copy figure for Word</button><button onclick="saveImage(\'', derived_id, '\',\'', derived_name,
      '\',this)">Save PNG</button><button onclick="copyText(\'', derived_latex_caption_id, '\',this)">Copy LaTeX caption</button></div></div>'
    ) else ""
    regional_depletion_html <- mfclshiny_jitter_regional_html_block(
      model_regional_pngs[["depletion"]], model_name, slug, "depletion", inclusion_caption,
      trajectory_style, reference_label, base_label
    )
    regional_recruitment_html <- mfclshiny_jitter_regional_html_block(
      model_regional_pngs[["recruitment"]], model_name, slug, "recruitment", inclusion_caption,
      trajectory_style, reference_label, base_label
    )
    regional_html <- if (nzchar(regional_depletion_html) || nzchar(regional_recruitment_html)) {
      paste0(
        '<div class="format-block regional-diagnostics"><h3>Regional diagnostics</h3>',
        '<p class="note">These spatial plots are an additional jitter view and do not replace the aggregate derived-quantity diagnostics above.</p>',
        regional_depletion_html, regional_recruitment_html, '</div>'
      )
    } else ""
    paste0(
      '<section id="tab-model-', slug, '" class="model-card tab-panel"><h2>', mfclshiny_jitter_html_escape(model_name), '</h2><div class="figure-block"><h3>Objective function</h3>',
      '<img id="', image_id, '" class="figure" alt="Jitter convergence diagnostics for ', mfclshiny_jitter_html_escape(model_name), '" src="', mfclshiny_jitter_image_data(image_file), '">',
      '<figcaption id="', objective_caption_id, '"><strong>Figure <span class="figure-number" contenteditable="true" spellcheck="false" title="Click to edit the figure number">XX</span>.</strong> ', mfclshiny_jitter_html_escape(objective_caption), '</figcaption><pre id="', objective_latex_caption_id, '" class="copy-source">', mfclshiny_jitter_html_escape(objective_latex_caption), '</pre><div class="actions"><button onclick="copyFigure(\'', image_id, '\',\'', objective_caption_id, '\',this)">Copy figure for Word</button><button onclick="saveImage(\'', image_id, '\',\'', image_name, '\',this)">Save PNG</button><button onclick="copyText(\'', objective_latex_caption_id, '\',this)">Copy LaTeX caption</button></div></div>', derived_html, regional_html,
      '<div class="format-block"><h3>Word-ready table</h3><p class="note">Use the button to paste this formatted table directly into Word.</p>',
      '<div class="actions"><button onclick="copyTable(\'', id, '\',this)">Copy table for Word</button></div>', mfclshiny_jitter_html_table(display, id, table_caption), "</div>",
      '<div class="format-block"><h3>LaTeX table</h3><p class="note">Copy the complete booktabs table into a LaTeX document.</p>',
      '<div class="actions"><button onclick="copyText(\'latex-', id, '\',this)">Copy LaTeX</button></div>',
      '<pre id="latex-', id, '">', mfclshiny_jitter_html_escape(latex), "</pre></div></section>"
    )
  }, character(1), USE.NAMES = TRUE)
  html <- paste0(
    '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
    "<title>", mfclshiny_jitter_html_escape(title), "</title><style>",
    ':root{--ink:#123b5d;--muted:#526979;--sea:#087f8c;--paper:#f5f1e8;--card:#fff;--line:#c8d9df;--orange:#d97904}*{box-sizing:border-box}body{margin:0;background:#eef2f3;color:#1d2f3a;font-family:"Aptos","Source Sans 3",sans-serif}header{padding:48px max(5vw,24px) 34px;background:#123b5d;color:white}header .eyebrow,.section-kicker{font-size:.72rem;letter-spacing:.18em;font-weight:800}header h1{font-family:"Georgia","Times New Roman",serif;font-size:clamp(2rem,4vw,3.7rem);line-height:1.08;margin:.35rem 0 1rem}header p{max-width:850px;font-size:1.02rem;color:#d6edf1}main{max-width:1240px;margin:auto;padding:28px max(3vw,18px) 70px}.tabs{display:flex;gap:8px;flex-wrap:wrap;margin-bottom:20px}.tab-button{background:#dce6e9;color:var(--ink);border:1px solid #b7cbd1}.tab-button.active{background:var(--ink);color:white}.tab-panel{display:none}.tab-panel.active{display:block}.overview,.model-card{background:#fff;border:1px solid var(--line);padding:clamp(18px,3vw,36px);margin-bottom:28px}.overview{border-top:5px solid var(--orange)}.method-copy{max-width:920px;margin:0;color:#29495b;font-family:"Georgia","Times New Roman",serif;font-size:1rem;line-height:1.65}.method-copy p{margin:.35rem 0 .8rem}.method-list{max-width:960px;margin:0;padding-left:1.25rem;color:#29495b;font-family:"Georgia","Times New Roman",serif;font-size:1rem;line-height:1.58}.method-list li{margin:0 0 .72rem;padding-left:.28rem}.results-heading{margin-top:1.9rem}.model-card{border-top:5px solid var(--sea)}h2{font-family:"Georgia","Times New Roman",serif;color:var(--ink);font-size:clamp(1.6rem,3vw,2.3rem);margin:.2rem 0 1.2rem}h3{font-family:"Georgia","Times New Roman",serif;color:var(--ink);margin:0 0 .4rem}.figure{width:100%;height:auto;border:1px solid #d8e5e9;background:white}.derived-figure{aspect-ratio:11/6.2;object-fit:contain}.copy-source{position:absolute!important;width:1px!important;height:1px!important;overflow:hidden!important;clip:rect(0 0 0 0)!important;white-space:pre!important;padding:0!important;border:0!important}.format-block{margin-top:26px;padding-top:20px;border-top:1px solid var(--line)}.table-scroll{overflow:auto;max-height:680px;border:1px solid var(--line);margin-top:12px}table{border-collapse:collapse;width:max-content;min-width:100%;font-size:.86rem}th{position:sticky;top:0;background:var(--ink);color:#fff;text-align:left;z-index:1}th,td{padding:9px 11px;border-bottom:1px solid #dce7ea;white-space:nowrap}tbody tr:nth-child(even){background:#f3f8f9}.actions{display:flex;gap:9px;flex-wrap:wrap;margin:18px 0 8px}button{border:0;background:var(--sea);color:white;padding:9px 14px;font-weight:700;cursor:pointer;transition:background .16s ease}button:hover{background:var(--ink)}button.done{background:#24784f}.action-status{position:fixed;right:22px;bottom:22px;z-index:20;background:var(--ink);color:#fff;padding:10px 15px;box-shadow:0 8px 24px rgba(18,59,93,.2);opacity:0;transform:translateY(8px);pointer-events:none;transition:opacity .16s ease,transform .16s ease}.action-status.show{opacity:1;transform:translateY(0)}pre{white-space:pre;overflow:auto;background:#102f43;color:#e9f5f7;padding:18px;font-size:.78rem}.note{color:var(--muted);line-height:1.55}footer{padding:24px;text-align:center;color:var(--muted)}@media print{body{background:white}.tabs,.actions,.action-status{display:none}.tab-panel{display:block!important}.overview,.model-card{border:0;box-shadow:none;break-inside:avoid;padding:0;margin:0 0 28px}}',
    'header{padding:24px max(5vw,24px)}header h1{font-size:clamp(1.7rem,3vw,2.5rem);margin:0}.figure-block{margin-top:24px}.regional-diagnostics h4{font-family:"Georgia","Times New Roman",serif;color:var(--ink);font-size:1.2rem;margin:1.4rem 0 .45rem}.regional-figure{aspect-ratio:11/7.5;object-fit:contain}figcaption{margin-top:12px;padding:12px 15px;background:#f1f6f7;border-left:3px solid var(--sea);color:#29495b;font-family:"Georgia","Times New Roman",serif;font-size:.95rem;line-height:1.55}.figure-number{display:inline-block;min-width:1.8em;padding:0 .15em;border-bottom:1px dotted var(--sea);color:var(--sea)}caption{caption-side:top;text-align:left;padding:12px 2px;font-family:"Georgia","Times New Roman",serif;font-weight:700;color:var(--ink);line-height:1.45}',
    "</style></head><body><header><h1>", mfclshiny_jitter_html_escape(title), "</h1></header><main>",
    '<nav class="tabs" role="tablist"><button class="tab-button active" role="tab" onclick="showTab(\'tab-overview\',this)">Overview</button>', paste(tab_buttons, collapse = ""), "</nav>",
    '<section id="tab-overview" class="overview tab-panel active"><h2>Jitter analysis</h2><div id="jitter-method">', method_html, '</div><pre id="latex-jitter-method" class="copy-source">', mfclshiny_jitter_html_escape(method_latex), '</pre><div class="actions"><button onclick="copySection(\'jitter-method\',this)">Copy analysis for Word</button><button onclick="copyText(\'latex-jitter-method\',this)">Copy analysis for LaTeX</button></div><h2 class="results-heading">Results</h2><div id="jitter-results" class="method-copy">', results_html, '</div><pre id="latex-jitter-results" class="copy-source">', mfclshiny_jitter_html_escape(results_latex), '</pre><pre id="bibtex-jitter-references" class="copy-source">', mfclshiny_jitter_html_escape(references_bibtex), '</pre><div class="actions"><button onclick="copySection(\'jitter-results\',this)">Copy results for Word</button><button onclick="copyText(\'latex-jitter-results\',this)">Copy results for LaTeX</button><button onclick="copyText(\'bibtex-jitter-references\',this)">Copy references as BibTeX</button></div></section>',
    paste(model_sections, collapse = "\n"),
    '</main><div id="action-status" class="action-status" role="status" aria-live="polite"></div><script>',
    'function actionFeedback(button,message){const status=document.getElementById("action-status");if(button){if(!button.dataset.label)button.dataset.label=button.textContent;button.textContent=message;button.classList.add("done");clearTimeout(button._feedbackTimer);button._feedbackTimer=setTimeout(()=>{button.textContent=button.dataset.label;button.classList.remove("done")},1600)}if(status){status.textContent=message;status.classList.add("show");clearTimeout(status._feedbackTimer);status._feedbackTimer=setTimeout(()=>status.classList.remove("show"),1800)}}',
    'async function copyText(id,button){const element=document.getElementById(id);const text=element.innerText||element.textContent;try{await navigator.clipboard.writeText(text)}catch(e){const t=document.createElement("textarea");t.value=text;document.body.appendChild(t);t.select();document.execCommand("copy");t.remove()}actionFeedback(button,"Copied")}',
    'async function copySection(id,button){const source=document.getElementById(id);const clone=source.cloneNode(true);const html=`<div style="font-family:Cambria,Georgia,serif;font-size:10.5pt;line-height:1.4">${clone.innerHTML}</div>`;const text=clone.innerText||clone.textContent;try{if(window.ClipboardItem){await navigator.clipboard.write([new ClipboardItem({"text/html":new Blob([html],{type:"text/html"}),"text/plain":new Blob([text],{type:"text/plain"})})])}else{await navigator.clipboard.writeText(text)}}catch(e){const holder=document.createElement("div");holder.innerHTML=html;holder.style.position="fixed";holder.style.left="-10000px";document.body.appendChild(holder);const range=document.createRange();range.selectNode(holder);const selection=window.getSelection();selection.removeAllRanges();selection.addRange(range);document.execCommand("copy");selection.removeAllRanges();holder.remove()}actionFeedback(button,"Copied for Word")}',
    'async function copyTable(id,button){const table=document.getElementById(id);const clone=table.cloneNode(true);clone.querySelectorAll("[contenteditable]").forEach(node=>{node.removeAttribute("contenteditable");node.removeAttribute("spellcheck");node.removeAttribute("title");node.removeAttribute("class");node.removeAttribute("style")});const html=clone.outerHTML;const caption=clone.querySelector("caption");const rows=Array.from(clone.rows).map(r=>Array.from(r.cells).map(c=>c.innerText).join("\\t")).join("\\n");const text=(caption?caption.innerText+"\\n":"")+rows;try{if(window.ClipboardItem){await navigator.clipboard.write([new ClipboardItem({"text/html":new Blob([html],{type:"text/html"}),"text/plain":new Blob([text],{type:"text/plain"})})])}else{await navigator.clipboard.writeText(text)}}catch(e){const range=document.createRange();range.selectNode(table);const selection=window.getSelection();selection.removeAllRanges();selection.addRange(range);document.execCommand("copy");selection.removeAllRanges()}actionFeedback(button,"Copied")}',
    'async function copyFigure(imageId,captionId,button){const source=document.getElementById(imageId);const caption=document.getElementById(captionId).cloneNode(true);caption.querySelectorAll("[contenteditable]").forEach(node=>{node.removeAttribute("contenteditable");node.removeAttribute("spellcheck");node.removeAttribute("title");node.removeAttribute("class");node.removeAttribute("style")});const html=`<div style="font-family:Cambria,Georgia,serif"><img src="${source.src}" style="display:block;width:100%;height:auto"><p style="font-size:10.5pt;line-height:1.3;margin:8pt 0 0 0">${caption.innerHTML}</p></div>`;const text=caption.innerText;try{if(window.ClipboardItem){await navigator.clipboard.write([new ClipboardItem({"text/html":new Blob([html],{type:"text/html"}),"text/plain":new Blob([text],{type:"text/plain"})})])}else{throw new Error("HTML clipboard unavailable")}}catch(e){const holder=document.createElement("div");holder.innerHTML=html;holder.style.position="fixed";holder.style.left="-10000px";holder.style.top="0";document.body.appendChild(holder);const range=document.createRange();range.selectNode(holder);const selection=window.getSelection();selection.removeAllRanges();selection.addRange(range);document.execCommand("copy");selection.removeAllRanges();holder.remove()}actionFeedback(button,"Copied for Word")}',
    'async function copyImage(id,button){const image=document.getElementById(id);try{const blob=await(await fetch(image.src)).blob();await navigator.clipboard.write([new ClipboardItem({"image/png":blob})]);actionFeedback(button,"Copied")}catch(e){saveImage(id,"jitter-figure.png",button)}}',
    'function saveImage(id,name,button){const link=document.createElement("a");link.href=document.getElementById(id).src;link.download=name;document.body.appendChild(link);link.click();link.remove();actionFeedback(button,"Download started")}',
    'function showTab(id,button){document.querySelectorAll(".tab-panel").forEach(panel=>panel.classList.remove("active"));document.querySelectorAll(".tab-button").forEach(tab=>tab.classList.remove("active"));document.getElementById(id).classList.add("active");button.classList.add("active")}',
    "</script></body></html>"
  )
  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}

#' Build a portable report-ready MFCL jitter bundle
#'
#' Creates the same objective-function versus maximum-gradient diagnostic used
#' by the mfclshiny Jitter tab, together with model and seed tables suitable for
#' Word, LaTeX, spreadsheets, and a self-contained HTML review.
#'
#' @param model_dir Root directory containing local model outputs or expanded
#'   Kflow archives.
#' @param output_dir Output directory, conventionally `jitter`.
#' @param title Report title.
#' @param provenance Optional Kflow job mapping passed to
#'   `collect_jitter_diagnostics()`.
#' @param data Optional already-normalized jitter data.
#' @param regional Include separate region-specific depletion and recruitment
#'   diagnostics. These require recoverable MFCL plot-report objects in the
#'   selected jitter payloads.
#' @param regional_data Optional already-normalized regional jitter time series.
#'   When supplied, it must contain `scenario`, `region`, `year`, `quantity`,
#'   `value`, `run`, `is_reference`, and `is_base_fit_reference` columns.
#' @param trajectory_style Show the pointwise jitter distribution or every
#'   included trajectory separately. Comparison labels and colours are generic
#'   report inputs; assessment-specific names are never inferred.
#' @param reference_label,base_label Labels for the reference fit and separately
#'   attached base fit.
#' @param reference_colour,base_colour Colours for those highlighted fits.
#' @param show_objective_reference_line Draw the horizontal reference-objective
#'   line in the objective-function diagnostic.
#' @param grad_reference Maximum gradient used for convergence classification.
#' @param rel_diff_threshold Symmetric objective-difference threshold in percent.
#' @param formats Figure formats; `png` and `pdf` are supported.
#' @param width,height Figure dimensions in inches.
#' @param dpi PNG resolution.
#' @param render_html Write a self-contained HTML report.
#' @return Invisibly returns data, plots, tables, indexes, and output paths.
#' @export
build_jitter_report <- function(model_dir = NULL,
                                output_dir = "jitter",
                                title = "Jitter model checks",
                                provenance = NULL,
                                data = NULL,
                                regional = FALSE,
                                regional_data = NULL,
                                trajectory_style = c("distribution", "individual"),
                                reference_label = "Reference model",
                                base_label = "Attached base fit",
                                reference_colour = "#C62828",
                                base_colour = "#111827",
                                show_objective_reference_line = TRUE,
                                grad_reference = NULL,
                                rel_diff_threshold = 10,
                                formats = c("png", "pdf"),
                                width = 11,
                                height = 7,
                                dpi = 300,
                                render_html = TRUE) {
  trajectory_style <- match.arg(trajectory_style)
  if (is.null(data)) {
    if (is.null(model_dir)) stop("Provide model_dir or data.", call. = FALSE)
    data <- collect_jitter_diagnostics(
      model_dir = model_dir,
      provenance = provenance,
      grad_reference = grad_reference
    )
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!nrow(data)) stop("No jitter_result.rds records were found.", call. = FALSE)
  if (!"is_base_fit_reference" %in% names(data)) data$is_base_fit_reference <- FALSE
  grad_override <- mfclshiny_jitter_valid_threshold(grad_reference)
  if (is.finite(grad_override)) {
    data$grad_reference <- grad_override
    data$converged <- data$run_completed %in% TRUE &
      is.finite(data$max_grad) &
      abs(data$max_grad) <= grad_override
  }
  all_data <- data
  if (isTRUE(regional) && is.null(regional_data)) {
    regional_data <- mfclshiny_jitter_collect_regional(all_data)
  }
  if (isTRUE(regional)) {
    regional_data <- as.data.frame(regional_data, stringsAsFactors = FALSE)
    required_regional <- c(
      "scenario", "region", "year", "quantity", "value", "run",
      "is_reference", "is_base_fit_reference"
    )
    missing_regional <- setdiff(required_regional, names(regional_data))
    if (!nrow(regional_data) || length(missing_regional)) {
      stop(
        "Regional jitter diagnostics were requested, but recoverable region-specific plot-report data were unavailable",
        if (length(missing_regional)) paste0(" (missing: ", paste(missing_regional, collapse = ", "), ")") else "",
        ".",
        call. = FALSE
      )
    }
  } else {
    regional_data <- data.frame()
  }
  data <- data[!data$is_base_fit_reference %in% TRUE, , drop = FALSE]
  if (!nrow(data)) stop("No counted jitter runs were found after excluding base-fit references.", call. = FALSE)
  formats <- intersect(unique(tolower(formats)), c("png", "pdf"))
  if (!length(formats)) formats <- "png"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  figure_dir <- file.path(output_dir, "figures")
  table_dir <- file.path(output_dir, "tables")
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

  scenarios <- unique(data$scenario)
  combined_plot <- plot_jitter_diagnostics(
    data,
    grad_reference = grad_reference,
    rel_diff_threshold = rel_diff_threshold,
    facet_ncol = min(2L, length(scenarios)),
    title = "Jitter Analysis: Model Comparison",
    point_style = if (identical(trajectory_style, "individual")) "highlight" else "run_colour",
    comparison_data = all_data[all_data$is_base_fit_reference %in% TRUE, , drop = FALSE],
    reference_label = reference_label,
    comparison_label = base_label,
    reference_colour = reference_colour,
    comparison_colour = base_colour,
    show_reference_line = show_objective_reference_line
  )
  figure_rows <- list()
  save_plot <- function(plot, stem, plot_width, plot_height, model = "All models") {
    rows <- lapply(formats, function(format) {
      file <- file.path(figure_dir, paste0(stem, ".", format))
      ggplot2::ggsave(file, plot = plot, width = plot_width, height = plot_height, dpi = dpi, units = "in", bg = "white")
      if (identical(format, "png")) {
        mfclshiny_report_optimize_png(file, optimize_figures = TRUE, lossless_only = TRUE)
      }
      data.frame(
        figure = stem,
        model = model,
        format = format,
        file = normalizePath(file, winslash = "/", mustWork = FALSE),
        width = plot_width,
        height = plot_height,
        dpi = if (identical(format, "png")) dpi else NA_real_,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }
  model_plots <- list()
  model_pngs <- list()
  derived_data <- mfclshiny_jitter_collect_derived(all_data)
  derived_plots <- list()
  derived_pngs <- list()
  regional_plots <- list()
  regional_pngs <- list()
  if (nrow(regional_data)) {
    utils::write.csv(
      regional_data,
      file.path(table_dir, "jitter-regional-timeseries.csv"),
      row.names = FALSE
    )
    regional_summary <- mfclshiny_jitter_summarise_derived(
      regional_data[
        !regional_data$is_reference %in% TRUE &
          !regional_data$is_base_fit_reference %in% TRUE,
        , drop = FALSE
      ]
    )
    utils::write.csv(
      regional_summary,
      file.path(table_dir, "jitter-regional-pointwise-summary.csv"),
      row.names = FALSE
    )
  } else {
    regional_summary <- data.frame()
  }
  for (model in scenarios) {
    model_data <- data[data$scenario == model, , drop = FALSE]
    plot_data <- model_data
    plot_data$scenario <- mfclshiny_jitter_first_text(model_data$model_label, model)
    model_comparison_data <- all_data[
      all_data$scenario == model & all_data$is_base_fit_reference %in% TRUE,
      , drop = FALSE
    ]
    if (nrow(model_comparison_data)) {
      model_comparison_data$scenario <- plot_data$scenario[[1L]]
    }
    plot <- plot_jitter_diagnostics(
      plot_data,
      grad_reference = grad_reference,
      rel_diff_threshold = rel_diff_threshold,
      facet_ncol = 1L,
      title = NULL,
      show_facet_labels = FALSE,
      point_style = if (identical(trajectory_style, "individual")) "highlight" else "run_colour",
      comparison_data = model_comparison_data,
      reference_label = reference_label,
      comparison_label = base_label,
      reference_colour = reference_colour,
      comparison_colour = base_colour,
      show_reference_line = show_objective_reference_line
    )
    stem <- paste0("jitter-diagnostics-", mfclshiny_jitter_slug(model))
    model_plots[[model]] <- plot
    figure_rows[[length(figure_rows) + 1L]] <- save_plot(plot, stem, 7.2, 5.2, model)
    png <- file.path(figure_dir, paste0(stem, ".png"))
    if (!file.exists(png)) {
      ggplot2::ggsave(png, plot = plot, width = 7.2, height = 5.2, dpi = dpi, units = "in", bg = "white")
      mfclshiny_report_optimize_png(png, optimize_figures = TRUE, lossless_only = TRUE)
    }
    model_pngs[[model]] <- png
    model_derived <- derived_data[derived_data$scenario == model, , drop = FALSE]
    if (nrow(model_derived)) {
      derived_plot <- mfclshiny_jitter_plot_derived(
        model_derived,
        trajectory_style = trajectory_style,
        reference_label = reference_label,
        base_label = base_label,
        reference_colour = reference_colour,
        base_colour = base_colour
      )
      derived_stem <- paste0("jitter-derived-", mfclshiny_jitter_slug(model))
      derived_plots[[model]] <- derived_plot
      figure_rows[[length(figure_rows) + 1L]] <- save_plot(derived_plot, derived_stem, 11, 6.2, model)
      derived_png <- file.path(figure_dir, paste0(derived_stem, ".png"))
      if (!file.exists(derived_png)) {
        ggplot2::ggsave(derived_png, plot = derived_plot, width = 11, height = 6.2, dpi = dpi, units = "in", bg = "white")
        mfclshiny_report_optimize_png(derived_png, optimize_figures = TRUE, lossless_only = TRUE)
      }
      derived_pngs[[model]] <- derived_png
    }
    model_regional <- regional_data[regional_data$scenario == model, , drop = FALSE]
    if (nrow(model_regional)) {
      regional_plots[[model]] <- list()
      regional_pngs[[model]] <- list()
      for (metric in c("depletion", "recruitment")) {
        quantity <- if (identical(metric, "depletion")) "Regional depletion" else "Regional recruitment"
        metric_data <- model_regional[model_regional$quantity == quantity, , drop = FALSE]
        if (!nrow(metric_data)) next
        regional_plot <- mfclshiny_jitter_plot_regional(
          metric_data,
          quantity = quantity,
          trajectory_style = trajectory_style,
          reference_label = reference_label,
          base_label = base_label,
          reference_colour = reference_colour,
          base_colour = base_colour
        )
        regional_stem <- paste0("jitter-regional-", metric, "-", mfclshiny_jitter_slug(model))
        regional_plots[[model]][[metric]] <- regional_plot
        figure_rows[[length(figure_rows) + 1L]] <- save_plot(
          regional_plot, regional_stem, 11, 7.5, model
        )
        regional_png <- file.path(figure_dir, paste0(regional_stem, ".png"))
        if (!file.exists(regional_png)) {
          ggplot2::ggsave(
            regional_png, plot = regional_plot, width = 11, height = 7.5,
            dpi = dpi, units = "in", bg = "white"
          )
          mfclshiny_report_optimize_png(regional_png, optimize_figures = TRUE, lossless_only = TRUE)
        }
        regional_pngs[[model]][[metric]] <- regional_png
      }
    }
  }
  figure_index <- do.call(rbind, figure_rows)

  summary <- mfclshiny_jitter_summary(data)
  seed_table <- mfclshiny_jitter_display_table(data)
  table_rows <- list()
  for (model in scenarios) {
    model_data <- data[data$scenario == model, , drop = FALSE]
    model_name <- mfclshiny_jitter_first_text(model_data$model_label, model)
    model_table <- mfclshiny_jitter_display_table(model_data)
    table_caption <- mfclshiny_jitter_model_caption(model_data, grad_reference)
    table_rows[[length(table_rows) + 1L]] <- mfclshiny_jitter_write_table_bundle(
      model_table,
      paste0("jitter-", mfclshiny_jitter_slug(model)),
      table_dir,
      paste0("MFCL jitter results for ", model_name, ". ", table_caption)
    )
  }
  table_index <- do.call(rbind, table_rows)

  html_file <- file.path(output_dir, "jitter-report.html")
  if (isTRUE(render_html)) {
    mfclshiny_jitter_write_html(
      html_file,
      data,
      summary,
      model_pngs,
      derived_pngs,
      regional_pngs,
      table_dir,
      grad_reference,
      title = title,
      trajectory_style = trajectory_style,
      reference_label = reference_label,
      base_label = base_label,
      show_objective_reference_line = show_objective_reference_line
    )
  }
  invisible(list(
    data = data,
    plot = combined_plot,
    model_plots = model_plots,
    derived_plots = derived_plots,
    regional_data = regional_data,
    regional_summary = regional_summary,
    regional_plots = regional_plots,
    model_summary = summary,
    seed_table = seed_table,
    figures = figure_index,
    tables = table_index,
    html = if (isTRUE(render_html)) normalizePath(html_file, winslash = "/", mustWork = FALSE) else "",
    html_image_dpi = as.integer(dpi),
    html_uses_publication_png = TRUE,
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  ))
}
