# ============================================================
# v2.4 level-2 method 02
# Modal-locked adaptive repeat walk + EI sensitivity panel
# ============================================================

walk_adaptive_tail <- function(
    sample_peaks,
    modal_size,
    modal_signal,
    max_delta = 10,
    nominal_repeat_size = 3,
    tolerance_bp = 0.8,
    spacing_bounds_bp = c(2.5, 3.2)
) {
  if (!is.finite(modal_size) || !is.finite(modal_signal)) {
    return(list(selected = data.frame(), audit = data.frame()))
  }

  x <- sample_peaks[
    is.finite(sample_peaks$Size) & is.finite(sample_peaks$Height) & sample_peaks$Height > 0,
    , drop = FALSE
  ]
  x <- x[order(x$Size), , drop = FALSE]
  x$Physical_row_id <- seq_len(nrow(x))

  modal_rows <- which(abs(x$Size - modal_size) < 1e-8)
  if (length(modal_rows) != 1L) stop("Expected exactly one physical modal row.")
  modal_id <- modal_rows[[1]]
  if (x$Height[modal_id] != modal_signal) stop("Modal signal mismatch.")

  selected <- x[modal_id, , drop = FALSE]
  selected$Delta_CAG <- 0L
  selected$Expected_size_bp <- modal_size
  selected$Local_repeat_spacing_bp <- nominal_repeat_size
  selected$Walk_error_bp <- 0
  selected$n_candidates_in_walk_window <- 1L

  audit <- x
  audit$Selected_repeat_peak <- audit$Physical_row_id == modal_id
  audit$Assigned_Delta_CAG <- ifelse(audit$Selected_repeat_peak, 0L, NA_integer_)
  audit$Expected_size_bp <- ifelse(audit$Selected_repeat_peak, modal_size, NA_real_)
  audit$Walk_error_bp <- ifelse(audit$Selected_repeat_peak, 0, NA_real_)

  accepted_delta <- 0
  accepted_size <- modal_size
  used_rows <- modal_id

  for (delta in seq_len(max_delta)) {
    if (length(accepted_delta) >= 2L) {
      dx <- accepted_delta[-1]
      dy <- accepted_size[-1] - modal_size
      spacing <- sum(dx * dy) / sum(dx^2)
      spacing <- max(spacing_bounds_bp[1], min(spacing_bounds_bp[2], spacing))
    } else {
      spacing <- nominal_repeat_size
    }

    expected_size <- modal_size + delta * spacing
    candidate_ids <- which(
      !x$Physical_row_id %in% used_rows &
        abs(x$Size - expected_size) <= tolerance_bp
    )

    if (!length(candidate_ids)) next

    z <- x[candidate_ids, , drop = FALSE]
    z$Abs_walk_error_bp <- abs(z$Size - expected_size)
    z <- z[order(-z$Height, z$Abs_walk_error_bp, z$Size), , drop = FALSE]
    chosen <- z[1, , drop = FALSE]
    chosen$Abs_walk_error_bp <- NULL

    chosen$Delta_CAG <- as.integer(delta)
    chosen$Expected_size_bp <- expected_size
    chosen$Local_repeat_spacing_bp <- spacing
    chosen$Walk_error_bp <- chosen$Size - expected_size
    chosen$n_candidates_in_walk_window <- length(candidate_ids)

    selected <- rbind(selected, chosen)

    row_id <- chosen$Physical_row_id
    audit$Selected_repeat_peak[audit$Physical_row_id == row_id] <- TRUE
    audit$Assigned_Delta_CAG[audit$Physical_row_id == row_id] <- as.integer(delta)
    audit$Expected_size_bp[audit$Physical_row_id == row_id] <- expected_size
    audit$Walk_error_bp[audit$Physical_row_id == row_id] <- chosen$Size - expected_size

    used_rows <- c(used_rows, row_id)
    accepted_delta <- c(accepted_delta, delta)
    accepted_size <- c(accepted_size, chosen$Size)
  }

  selected <- selected[order(selected$Delta_CAG), , drop = FALSE]
  rownames(selected) <- NULL
  list(selected = selected, audit = audit)
}


if (!isTRUE(getOption("v2.4_method_define_only"))) {
  suppressPackageStartupMessages(library(dplyr))

  project_directory <- "D:/R/Summer_Pipeline_to_UI/development/v2_4"
  core_output_directory <- file.path(project_directory, "SCA2_brain_pipeline_outputs_v2_4")
  output_directory <- file.path(core_output_directory, "method02_adaptive_walk")
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  old_opt <- getOption("v2.4_define_only")
  options(v2.4_define_only = TRUE)
  source(file.path(project_directory, "repeat_peak.v2.4_core.R"))
  options(v2.4_define_only = old_opt)

  core <- readRDS(file.path(core_output_directory, "SCA2_v2_4_core_objects.rds"))
  brain_peaks <- core$brain_peaks
  refs <- core$sample_reference_table
  tail_width <- core$parameters$ei_tail_width
  pct_thresholds <- core$parameters$ei_pct_thresholds
  repeat_size_bp <- core$parameters$repeat_size_bp

  tolerance_bp <- 0.80
  spacing_bounds_bp <- c(2.5, 3.2)

  selected_all <- data.frame()
  audit_all <- data.frame()

  eligible <- refs %>% filter(Expanded_EI_eligible, is.finite(Expanded_modal_size_bp))

  for (sample_file in eligible$Sample.File.Name) {
    ref <- eligible %>% filter(Sample.File.Name == sample_file) %>% slice(1)
    peaks <- brain_peaks %>% filter(Sample.File.Name == sample_file)

    walked <- walk_adaptive_tail(
      sample_peaks = peaks,
      modal_size = ref$Expanded_modal_size_bp,
      modal_signal = ref$Expanded_modal_signal,
      max_delta = tail_width,
      nominal_repeat_size = repeat_size_bp,
      tolerance_bp = tolerance_bp,
      spacing_bounds_bp = spacing_bounds_bp
    )

    selected_all <- bind_rows(selected_all, walked$selected)
    audit_all <- bind_rows(audit_all, walked$audit)
  }

  modal_qc <- eligible %>%
    select(Sample.File.Name, Expanded_modal_size_bp, Expanded_modal_signal) %>%
    left_join(
      selected_all %>%
        filter(Delta_CAG == 0) %>%
        select(Sample.File.Name, Selected_modal_size_bp = Size, Selected_modal_signal = Height),
      by = "Sample.File.Name"
    ) %>%
    mutate(
      Modal_size_preserved = abs(Selected_modal_size_bp - Expanded_modal_size_bp) < 1e-8,
      Modal_signal_preserved = Selected_modal_signal == Expanded_modal_signal
    )

  if (any(!modal_qc$Modal_size_preserved) || any(!modal_qc$Modal_signal_preserved)) {
    stop("Method 02 modal-lock QC failed.")
  }

  ei_results <- bind_rows(lapply(
    split(selected_all, selected_all$Sample.File.Name),
    function(x) calculate_ei_variants(
      selected_peaks = x,
      expanded_eligible = TRUE,
      tail_width = tail_width,
      pct_thresholds = pct_thresholds,
      include_historical_full_3pct = FALSE
    )
  )) %>%
    select(-Expanded_EI_eligible)

  sample_results <- refs %>%
    left_join(ei_results, by = "Sample.File.Name") %>%
    mutate(
      Repeat_assignment_method = "adaptive_repeat_walk",
      Repeat_walk_tolerance_bp = tolerance_bp,
      Repeat_walk_spacing_bounds_bp = paste(spacing_bounds_bp, collapse = ":"),
      EI_primary_definition = "walk-selected Delta_CAG 0:+10",
      EI_hybrid_definition = "within Delta_CAG 0:+10, retain peaks >= x% of modal height"
    ) %>%
    arrange(Group, Brain_region)

  write.csv(selected_all, file.path(output_directory, "adaptive_walk_selected_peaks.csv"), row.names = FALSE, na = "")
  write.csv(audit_all, file.path(output_directory, "adaptive_walk_all_physical_peaks_audit.csv"), row.names = FALSE, na = "")
  write.csv(modal_qc, file.path(output_directory, "adaptive_walk_modal_lock_QC.csv"), row.names = FALSE, na = "")
  write.csv(sample_results, file.path(output_directory, "adaptive_walk_EI_results.csv"), row.names = FALSE, na = "")

  saveRDS(
    list(selected_peaks = selected_all, audit = audit_all, sample_results = sample_results),
    file.path(output_directory, "method02_adaptive_walk_objects.rds")
  )

  cat("\nMethod 02 done: adaptive repeat walk.\n")
}
