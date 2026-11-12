# ============================================================
# SCA2 brain ABI pipeline — v2.4 CORE
# Common preprocessing shared by all downstream method tests
#
# Purpose of the core:
#   1. Read GeneMapper peak table.
#   2. Match peaks to the user-supplied long plate map.
#   3. Scan the actual 3730XL run folder for .fsa files.
#   4. Keep TempEPTData as run-level audit only.
#   5. Keep all physical GeneMapper peaks for audit.
#   6. Retain Size >= 103 bp as theoretical SCA2 candidates.
#   7. Apply the agreed Case-level biological interpretation.
#   8. Call a physical modal reference, but DO NOT assign repeat bins.
#   9. Provide one generic EI calculator for downstream methods.
#
# Important separation of responsibilities:
#   - GeneMapper supplies the production Size + Height peak table.
#   - Raw .fsa files are retained for the independent TRACE sub-test.
#   - TempEPTData is NOT used for peak sizing or EI.
#   - Repeat assignment is deliberately NOT performed in this file.
#   - Case_2 is treated as normal / non-expanded, so expanded EI is NA.
# ============================================================


# ============================================================
# 0. Small reusable functions
#    These use base R so they can be unit-tested independently.
# ============================================================

extract_well_from_filename <- function(x) {
  x <- toupper(basename(as.character(x)))
  out <- rep(NA_character_, length(x))
  hit <- grepl("^[A-H](0[1-9]|1[0-2])", x, perl = TRUE)
  out[hit] <- sub("^([A-H](0[1-9]|1[0-2])).*$", "\\1", x[hit], perl = TRUE)
  if (length(out) == 1L) out[[1]] else out
}


build_fsa_manifest <- function(run_directory) {
  if (!dir.exists(run_directory)) {
    stop("Run directory does not exist: ", run_directory)
  }

  paths <- list.files(
    run_directory,
    pattern = "\\.fsa$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = FALSE
  )

  if (!length(paths)) {
    return(data.frame(
      FSA_file_name = character(),
      Well = character(),
      FSA_path = character(),
      FSA_size_bytes = numeric(),
      FSA_exists = logical(),
      stringsAsFactors = FALSE
    ))
  }

  info <- file.info(paths)

  data.frame(
    FSA_file_name = basename(paths),
    Well = extract_well_from_filename(basename(paths)),
    FSA_path = normalizePath(paths, winslash = "/", mustWork = FALSE),
    FSA_size_bytes = unname(info$size),
    FSA_exists = file.exists(paths),
    stringsAsFactors = FALSE
  )[order(extract_well_from_filename(basename(paths))), ]
}


normalize_plate_map_schema <- function(plate_map) {
  aliases <- list(
    Row = c("Row", "Plate_row"),
    Column = c("Column", "Plate_column"),
    Well = c("Well"),
    Sample_ID = c("Sample_ID", "Plate_sample_ID"),
    Sample_Type = c("Sample_Type", "Sample_type"),
    Group = c("Group", "Patient_ID"),
    Brain_region = c("Brain_region"),
    Control_type = c("Control_type"),
    Is_water = c("Is_water")
  )

  out <- plate_map

  for (canonical_name in names(aliases)) {
    if (!canonical_name %in% names(out)) {
      available_aliases <- aliases[[canonical_name]][
        aliases[[canonical_name]] %in% names(out)
      ]

      if (length(available_aliases)) {
        names(out)[names(out) == available_aliases[1]] <- canonical_name
      }
    }
  }

  required <- names(aliases)
  missing <- setdiff(required, names(out))

  if (length(missing)) {
    stop(
      "Plate map missing columns after schema normalisation: ",
      paste(missing, collapse = ", "),
      ". Available columns: ",
      paste(names(plate_map), collapse = ", ")
    )
  }

  out
}


make_case_analysis_plan <- function() {
  data.frame(
    Group = c("Case_1", "Case_2", "Case_3"),
    Genotype_interpretation = c(
      "expanded_single_resolved_cluster",
      "normal_non_expanded",
      "normal_plus_expanded_two_resolved_clusters"
    ),
    Expected_resolved_clusters = c(1L, 1L, 2L),
    Expanded_EI_eligible = c(TRUE, FALSE, TRUE),
    Expanded_reference_rule = c(
      "single_resolved_cluster_modal",
      "none_normal_genotype",
      "larger_size_of_two_resolved_clusters"
    ),
    stringsAsFactors = FALSE
  )
}


find_modal_candidates <- function(
    sample_peaks,
    gap_bp = 6,
    signal_multiplier = 1
) {
  required <- c("Size", "Height")
  missing <- setdiff(required, names(sample_peaks))
  if (length(missing)) {
    stop("sample_peaks missing columns: ", paste(missing, collapse = ", "))
  }

  x <- sample_peaks[
    is.finite(sample_peaks$Size) &
      is.finite(sample_peaks$Height) &
      sample_peaks$Height > 0,
    ,
    drop = FALSE
  ]

  x <- x[order(x$Size), , drop = FALSE]
  if (!nrow(x)) return(data.frame())

  threshold <- mean(x$Height) * signal_multiplier
  candidates <- x[x$Height > threshold, , drop = FALSE]
  if (!nrow(candidates)) return(data.frame())

  candidates$Cluster_ID <- cumsum(c(TRUE, diff(candidates$Size) > gap_bp))

  cluster_ids <- unique(candidates$Cluster_ID)
  modal_rows <- lapply(cluster_ids, function(cluster_id) {
    z <- candidates[candidates$Cluster_ID == cluster_id, , drop = FALSE]
    z <- z[order(-z$Height, z$Size), , drop = FALSE]
    z <- z[1, , drop = FALSE]
    data.frame(
      Cluster_ID = cluster_id,
      Modal_size_bp = z$Size,
      Modal_signal = z$Height,
      Signal_threshold = threshold,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, modal_rows)
  rownames(out) <- NULL
  out[order(out$Modal_size_bp), , drop = FALSE]
}


call_sample_modal <- function(
    sample_peaks,
    group,
    case_plan = make_case_analysis_plan(),
    gap_bp = 6,
    signal_multiplier = 1
) {
  if (!"Sample.File.Name" %in% names(sample_peaks)) {
    stop("sample_peaks must contain Sample.File.Name")
  }

  sample_ids <- unique(as.character(sample_peaks$Sample.File.Name))
  if (length(sample_ids) != 1L) {
    stop("call_sample_modal() requires exactly one sample.")
  }

  plan <- case_plan[case_plan$Group == group, , drop = FALSE]
  if (nrow(plan) != 1L) {
    stop("No unique Case rule found for: ", group)
  }

  candidates <- find_modal_candidates(
    sample_peaks = sample_peaks,
    gap_bp = gap_bp,
    signal_multiplier = signal_multiplier
  )

  n_candidates <- nrow(candidates)

  reference_size <- NA_real_
  reference_signal <- NA_real_
  normal_size <- NA_real_
  normal_signal <- NA_real_
  expanded_size <- NA_real_
  expanded_signal <- NA_real_
  status <- "unresolved"

  if (group == "Case_1") {
    if (n_candidates >= 1L) {
      chosen <- candidates[order(-candidates$Modal_signal), , drop = FALSE][1, ]
      reference_size <- chosen$Modal_size_bp
      reference_signal <- chosen$Modal_signal
      expanded_size <- reference_size
      expanded_signal <- reference_signal
      status <- if (n_candidates == 1L) {
        "expanded_single_cluster_resolved"
      } else {
        paste0("expanded_selected_strongest_of_", n_candidates, "_candidate_clusters")
      }
    }
  }

  if (group == "Case_2") {
    if (n_candidates >= 1L) {
      chosen <- candidates[order(-candidates$Modal_signal), , drop = FALSE][1, ]
      reference_size <- chosen$Modal_size_bp
      reference_signal <- chosen$Modal_signal
      normal_size <- reference_size
      normal_signal <- reference_signal
      status <- if (n_candidates == 1L) {
        "normal_single_cluster_resolved_no_expanded_EI"
      } else {
        paste0("normal_selected_strongest_of_", n_candidates, "_candidate_clusters_no_expanded_EI")
      }
    }
  }

  if (group == "Case_3") {
    if (n_candidates >= 2L) {
      two <- candidates[order(-candidates$Modal_signal), , drop = FALSE][1:2, ]
      two <- two[order(two$Modal_size_bp), , drop = FALSE]

      normal_size <- two$Modal_size_bp[1]
      normal_signal <- two$Modal_signal[1]
      expanded_size <- two$Modal_size_bp[2]
      expanded_signal <- two$Modal_signal[2]
      reference_size <- expanded_size
      reference_signal <- expanded_signal

      status <- if (n_candidates == 2L) {
        "two_clusters_resolved_larger_is_expanded"
      } else {
        paste0("strongest_two_of_", n_candidates, "_candidate_clusters_larger_is_expanded")
      }
    } else {
      status <- paste0("expected_two_clusters_but_resolved_", n_candidates)
    }
  }

  data.frame(
    Sample.File.Name = sample_ids,
    Group = group,
    Genotype_interpretation = plan$Genotype_interpretation,
    Expected_resolved_clusters = plan$Expected_resolved_clusters,
    Expanded_EI_eligible = plan$Expanded_EI_eligible,
    Resolved_candidate_clusters = n_candidates,
    Reference_modal_size_bp = reference_size,
    Reference_modal_signal = reference_signal,
    Normal_modal_size_bp = normal_size,
    Normal_modal_signal = normal_signal,
    Expanded_modal_size_bp = expanded_size,
    Expanded_modal_signal = expanded_signal,
    Modal_call_status = status,
    Modal_candidate_gap_bp = gap_bp,
    Modal_candidate_signal_multiplier = signal_multiplier,
    stringsAsFactors = FALSE
  )
}


calculate_ei_variants <- function(
    selected_peaks,
    expanded_eligible = TRUE,
    tail_width = 10,
    pct_thresholds = c(0.03, 0.10, 0.20),
    include_historical_full_3pct = TRUE
) {
  required <- c("Delta_CAG", "Height")
  missing <- setdiff(required, names(selected_peaks))
  if (length(missing)) {
    stop("selected_peaks missing columns: ", paste(missing, collapse = ", "))
  }

  sample_id <- if ("Sample.File.Name" %in% names(selected_peaks)) {
    ids <- unique(as.character(selected_peaks$Sample.File.Name))
    if (length(ids) != 1L) stop("EI calculation requires exactly one sample.")
    ids
  } else {
    NA_character_
  }

  base_result <- list(
    Sample.File.Name = sample_id,
    Expanded_EI_eligible = isTRUE(expanded_eligible),
    EI_tail10 = NA_real_,
    Tail10_signal_denominator = NA_real_,
    Tail10_n_detected_bins = 0L,
    Tail10_missing_delta_bins = paste(0:tail_width, collapse = ";")
  )

  for (p in pct_thresholds) {
    label <- paste0(as.integer(round(p * 100)), "pct")
    base_result[[paste0("EI_", label, "_tail10")]] <- NA_real_
    base_result[[paste0("Tail10_", label, "_threshold_RFU")]] <- NA_real_
    base_result[[paste0("Tail10_", label, "_signal_denominator")]] <- NA_real_
    base_result[[paste0("Tail10_", label, "_n_included_bins")]] <- 0L
  }

  if (include_historical_full_3pct) {
    base_result$EI_3pct_full_tail <- NA_real_
    base_result$Full_3pct_signal_denominator <- NA_real_
    base_result$Full_3pct_n_included_bins <- 0L
  }

  if (!isTRUE(expanded_eligible)) {
    base_result$EI_QC <- "Not_calculated_normal_non_expanded"
    return(as.data.frame(base_result, stringsAsFactors = FALSE))
  }

  x <- selected_peaks[
    is.finite(selected_peaks$Delta_CAG) &
      is.finite(selected_peaks$Height) &
      selected_peaks$Height > 0,
    ,
    drop = FALSE
  ]

  if (!nrow(x)) {
    base_result$EI_QC <- "No_selected_repeat_peaks"
    return(as.data.frame(base_result, stringsAsFactors = FALSE))
  }

  if (anyDuplicated(x$Delta_CAG)) {
    stop("selected_peaks must contain at most one peak per Delta_CAG.")
  }

  modal <- x[x$Delta_CAG == 0, , drop = FALSE]
  if (nrow(modal) != 1L) {
    base_result$EI_QC <- "Missing_or_duplicated_Delta0_modal"
    return(as.data.frame(base_result, stringsAsFactors = FALSE))
  }

  modal_signal <- modal$Height[1]

  tail <- x[x$Delta_CAG >= 0 & x$Delta_CAG <= tail_width, , drop = FALSE]
  tail <- tail[order(tail$Delta_CAG), , drop = FALSE]

  denominator <- sum(tail$Height)
  base_result$Tail10_signal_denominator <- denominator
  base_result$Tail10_n_detected_bins <- nrow(tail)

  expected_bins <- 0:tail_width
  missing_bins <- setdiff(expected_bins, tail$Delta_CAG)
  base_result$Tail10_missing_delta_bins <- if (length(missing_bins)) {
    paste(missing_bins, collapse = ";")
  } else {
    ""
  }

  if (denominator > 0) {
    base_result$EI_tail10 <- sum(tail$Height * tail$Delta_CAG) / denominator
  }

  for (p in pct_thresholds) {
    label <- paste0(as.integer(round(p * 100)), "pct")
    threshold_rfu <- modal_signal * p
    filtered <- tail[tail$Height >= threshold_rfu, , drop = FALSE]
    filtered_denominator <- sum(filtered$Height)

    base_result[[paste0("Tail10_", label, "_threshold_RFU")]] <- threshold_rfu
    base_result[[paste0("Tail10_", label, "_signal_denominator")]] <- filtered_denominator
    base_result[[paste0("Tail10_", label, "_n_included_bins")]] <- nrow(filtered)

    if (filtered_denominator > 0) {
      base_result[[paste0("EI_", label, "_tail10")]] <-
        sum(filtered$Height * filtered$Delta_CAG) / filtered_denominator
    }
  }

  if (include_historical_full_3pct) {
    threshold_rfu <- modal_signal * 0.03
    full_3pct <- x[x$Delta_CAG >= 0 & x$Height >= threshold_rfu, , drop = FALSE]
    full_denominator <- sum(full_3pct$Height)

    base_result$Full_3pct_signal_denominator <- full_denominator
    base_result$Full_3pct_n_included_bins <- nrow(full_3pct)

    if (full_denominator > 0) {
      base_result$EI_3pct_full_tail <-
        sum(full_3pct$Height * full_3pct$Delta_CAG) / full_denominator
    }
  }

  base_result$EI_QC <- "OK"
  as.data.frame(base_result, stringsAsFactors = FALSE)
}


# ============================================================
# 1. Run the core pipeline unless sourced in define-only mode
# ============================================================

if (!isTRUE(getOption("v2.4_define_only"))) {

  suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
    library(stringr)
    library(ggplot2)
  })

  # ----------------------------------------------------------
  # Paths: update only this block if the local structure moves.
  # ----------------------------------------------------------

  data_root <- "D:/R/Summer_Pipeline_to_UI/private_inputs"
  project_directory <- "D:/R/Summer_Pipeline_to_UI/development/v2_4"

  run_directory <- file.path(
    "D:/R/Summer_Pipeline_to_UI",
    "Run_3730XL_2026-07-13_11-24_0219_Luis_SCA2"
  )

  temp_ept_directory <- file.path(run_directory, "TempEPTData")

  input_file <- file.path(
    data_root,
    "sizing_sca2_brain_7.13.txt"
  )

  plate_map_file <- file.path(
    data_root,

    "SCA2_brain_plate_map_96wells_long.csv"
  )

  output_directory <- file.path(
    project_directory,
    "SCA2_brain_pipeline_outputs_v2_4"
  )

  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  # ----------------------------------------------------------
  # Shared biological / assay parameters
  # ----------------------------------------------------------

  target_dye_channel <- "B"
  fixed_bp <- 103
  minimum_size_bp <- 103
  repeat_size_bp <- 3

  # Common modal-candidate rule inherited from the current workflow.
  # This identifies the physical modal reference only.
  # Repeat assignment is tested separately in the level-2 scripts.
  modal_candidate_gap_bp <- 6
  modal_candidate_signal_multiplier <- 1

  # EI definitions shared by every level-2 repeat-assignment method.
  ei_tail_width <- 10
  ei_pct_thresholds <- c(0.03, 0.10, 0.20)

  case_analysis_plan <- make_case_analysis_plan()

  write_out <- function(x, filename) {
    write.csv(
      x,
      file.path(output_directory, filename),
      row.names = FALSE,
      na = ""
    )
  }

  # ==========================================================
  # 2. Validate input paths
  # ==========================================================

  cat(
    "\nResolved v2.4 inputs:\n",
    "GeneMapper sizing table: ", input_file, "\n",
    "Plate map: ", plate_map_file, "\n",
    "3730XL run directory: ", run_directory, "\n\n",
    sep = ""
  )

  if (!dir.exists(run_directory)) {
    stop("Missing 3730XL run directory: ", run_directory)
  }
  if (!file.exists(input_file)) {
    stop("Missing GeneMapper sizing table: ", input_file)
  }
  if (!file.exists(plate_map_file)) {
    stop("Missing long-format plate map: ", plate_map_file)
  }

  # ==========================================================
  # 3. Scan raw .fsa files + TempEPTData
  # ==========================================================

  fsa_manifest <- build_fsa_manifest(run_directory)

  invalid_fsa_names <- fsa_manifest[is.na(fsa_manifest$Well), , drop = FALSE]
  if (nrow(invalid_fsa_names)) {
    warning(
      "Some .fsa filenames do not start with a valid 96-well coordinate: ",
      paste(invalid_fsa_names$FSA_file_name, collapse = ", ")
    )
  }

  if (dir.exists(temp_ept_directory)) {
    temp_paths <- list.files(
      temp_ept_directory,
      full.names = TRUE,
      recursive = FALSE
    )
    temp_info <- file.info(temp_paths)

    temp_ept_manifest <- data.frame(
      File_name = basename(temp_paths),
      File_path = normalizePath(temp_paths, winslash = "/", mustWork = FALSE),
      Size_bytes = unname(temp_info$size),
      Interpretation = "3730XL run telemetry; audit only; not used for sizing or EI",
      stringsAsFactors = FALSE
    )
  } else {
    temp_ept_manifest <- data.frame(
      File_name = character(),
      File_path = character(),
      Size_bytes = numeric(),
      Interpretation = character(),
      stringsAsFactors = FALSE
    )
    warning("TempEPTData directory was not found; this does not block peak analysis.")
  }

  # ==========================================================
  # 4. Import and validate long-format plate map
  # ==========================================================

  plate_map <- read.csv(
    plate_map_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ) %>%
    normalize_plate_map_schema() %>%
    mutate(
      Well = str_to_upper(str_squish(as.character(Well))),
      Is_water = str_to_lower(str_squish(as.character(Is_water))) %in%
        c("true", "t", "1", "yes", "y")
    )

  if (anyDuplicated(plate_map$Well)) {
    stop("Duplicated wells in plate map.")
  }

  if (any(!str_detect(plate_map$Well, "^[A-H](0[1-9]|1[0-2])$"))) {
    stop("Invalid well coordinate in plate map.")
  }

  unknown_groups <- setdiff(
    unique(plate_map$Group[!plate_map$Is_water & !is.na(plate_map$Group)]),
    case_analysis_plan$Group
  )
  if (length(unknown_groups)) {
    stop("No Case interpretation defined for: ", paste(unknown_groups, collapse = ", "))
  }

  # ==========================================================
  # 5. Import GeneMapper peak table
  # ==========================================================

  gm_raw <- read.delim(
    input_file,
    stringsAsFactors = FALSE,
    check.names = TRUE
  )

  required_gm_cols <- c(
    "Dye.Sample.Peak", "Sample.File.Name", "Allele", "Size", "Height"
  )

  missing_gm_cols <- setdiff(required_gm_cols, names(gm_raw))
  if (length(missing_gm_cols)) {
    stop("GeneMapper table missing columns: ", paste(missing_gm_cols, collapse = ", "))
  }

  for (nm in c("Area", "Data.Point")) {
    if (!nm %in% names(gm_raw)) gm_raw[[nm]] <- NA_real_
  }

  gm_sample_file_lookup <- gm_raw %>%
    transmute(
      Sample.File.Name = str_squish(as.character(Sample.File.Name)),
      Well = extract_well_from_filename(Sample.File.Name)
    ) %>%
    filter(!is.na(Sample.File.Name), Sample.File.Name != "") %>%
    distinct()

  gm_clean <- gm_raw %>%
    mutate(
      Dye.Sample.Peak = str_squish(as.character(Dye.Sample.Peak)),
      Sample.File.Name = str_squish(as.character(Sample.File.Name)),
      Allele = as.character(Allele),
      Size = suppressWarnings(as.numeric(Size)),
      Height = suppressWarnings(as.numeric(Height)),
      Area = suppressWarnings(as.numeric(Area)),
      Data.Point = suppressWarnings(as.numeric(Data.Point)),
      Dye_channel = str_sub(Dye.Sample.Peak, 1, 1),
      Well = extract_well_from_filename(Sample.File.Name)
    ) %>%
    filter(
      Dye_channel == target_dye_channel,
      !is.na(Well),
      is.finite(Size),
      is.finite(Height),
      Height > 0
    ) %>%
    left_join(plate_map, by = "Well") %>%
    arrange(Well, Size)

  if (any(is.na(gm_clean$Sample_Type))) {
    bad <- unique(gm_clean$Well[is.na(gm_clean$Sample_Type)])
    stop("GeneMapper wells missing from plate map: ", paste(bad, collapse = ", "))
  }

  # ==========================================================
  # 6. Cross-check GeneMapper filenames against actual .fsa files
  # ==========================================================

  fsa_lookup <- fsa_manifest %>%
    transmute(
      Sample.File.Name = FSA_file_name,
      FSA_Well = Well,
      FSA_path,
      FSA_size_bytes,
      FSA_in_run = TRUE
    )

  gm_lookup <- gm_sample_file_lookup %>%
    mutate(GeneMapper_in_table = TRUE)

  fsa_genemapper_crosscheck <- full_join(
    gm_lookup,
    fsa_lookup,
    by = "Sample.File.Name"
  ) %>%
    mutate(
      GeneMapper_in_table = coalesce(GeneMapper_in_table, FALSE),
      FSA_in_run = coalesce(FSA_in_run, FALSE),
      Filename_match_status = case_when(
        GeneMapper_in_table & FSA_in_run ~ "Matched",
        GeneMapper_in_table & !FSA_in_run ~ "GeneMapper_only_missing_FSA",
        !GeneMapper_in_table & FSA_in_run ~ "FSA_only_not_in_GeneMapper",
        TRUE ~ "Unexpected"
      )
    ) %>%
    arrange(coalesce(Well, FSA_Well), Sample.File.Name)

  missing_fsa_for_gm <- fsa_genemapper_crosscheck %>%
    filter(Filename_match_status == "GeneMapper_only_missing_FSA")

  if (nrow(missing_fsa_for_gm)) {
    warning(
      "GeneMapper sample(s) without matching raw .fsa file: ",
      paste(missing_fsa_for_gm$Sample.File.Name, collapse = ", ")
    )
  }

  # ==========================================================
  # 7. Annotate theoretical SCA2 fragment size
  # ==========================================================

  gm_annotated <- gm_clean %>%
    mutate(
      Passes_theoretical_size = Size >= minimum_size_bp,
      Repeat_units_raw = if_else(
        Passes_theoretical_size,
        (Size - fixed_bp) / repeat_size_bp,
        NA_real_
      ),
      Approx_CAG_from_GeneMapper_size = if_else(
        Passes_theoretical_size,
        floor(Repeat_units_raw + 0.5),
        NA_real_
      )
    ) %>%
    arrange(Group, Brain_region, Sample.File.Name, Size)

  brain_peaks <- gm_annotated %>%
    filter(!Is_water, Passes_theoretical_size)

  brain_peaks_below_103 <- gm_annotated %>%
    filter(!Is_water, !Passes_theoretical_size)

  water_peak_records <- gm_annotated %>%
    filter(Is_water)

  if (!nrow(brain_peaks)) {
    stop("No brain peaks remain at Size >= 103 bp.")
  }

  brain_metadata <- brain_peaks %>%
    distinct(
      Sample.File.Name,
      Well,
      Sample_ID,
      Group,
      Brain_region,
      Sample_Type,
      Control_type,
      Is_water
    ) %>%
    left_join(case_analysis_plan, by = "Group") %>%
    arrange(Group, Brain_region)

  # ==========================================================
  # 8. Common physical modal reference calling
  #
  # This is deliberately upstream of the four repeat-assignment tests.
  # It selects the physical modal Size + Height only.
  # It does NOT create Delta_CAG bins.
  # ==========================================================

  split_peaks <- split(brain_peaks, brain_peaks$Sample.File.Name)

  modal_calls <- bind_rows(lapply(split_peaks, function(sample_peaks) {
    group <- unique(sample_peaks$Group)
    if (length(group) != 1L) {
      stop("One sample was assigned to more than one Group: ", unique(sample_peaks$Sample.File.Name))
    }

    call_sample_modal(
      sample_peaks = sample_peaks,
      group = group,
      case_plan = case_analysis_plan,
      gap_bp = modal_candidate_gap_bp,
      signal_multiplier = modal_candidate_signal_multiplier
    )
  })) %>%
    mutate(
      Reference_modal_repeat_raw = (Reference_modal_size_bp - fixed_bp) / repeat_size_bp,
      Reference_modal_CAG = floor(Reference_modal_repeat_raw + 0.5),

      Normal_modal_repeat_raw = (Normal_modal_size_bp - fixed_bp) / repeat_size_bp,
      Normal_modal_CAG = floor(Normal_modal_repeat_raw + 0.5),

      Expanded_modal_repeat_raw = (Expanded_modal_size_bp - fixed_bp) / repeat_size_bp,
      Expanded_modal_CAG = floor(Expanded_modal_repeat_raw + 0.5)
    )

  sample_reference_table <- brain_metadata %>%
    left_join(modal_calls, by = c("Sample.File.Name", "Group", "Genotype_interpretation",
                                  "Expected_resolved_clusters", "Expanded_EI_eligible")) %>%
    arrange(Group, Brain_region)

  unresolved_expanded_samples <- sample_reference_table %>%
    filter(Expanded_EI_eligible, !is.finite(Expanded_modal_size_bp))

  if (nrow(unresolved_expanded_samples)) {
    warning(
      "Expanded modal unresolved for: ",
      paste(unresolved_expanded_samples$Sample.File.Name, collapse = ", ")
    )
  }

  # ==========================================================
  # 9. Compact sample QC
  # ==========================================================

  sample_qc <- gm_annotated %>%
    group_by(
      Sample.File.Name,
      Well,
      Group,
      Brain_region,
      Sample_Type,
      Is_water
    ) %>%
    summarise(
      n_called_B_channel_peaks = n(),
      n_peaks_ge_103 = sum(Passes_theoretical_size),
      min_size_bp = min(Size),
      max_size_bp = max(Size),
      max_height_RFU = max(Height),
      total_height_RFU = sum(Height),
      .groups = "drop"
    ) %>%
    arrange(Well)

  # ==========================================================
  # 10. Physical peak profile plot
  # ==========================================================

  plot_case_physical_profiles <- function(case_id) {
    pdat <- brain_peaks %>%
      filter(Group == case_id) %>%
      group_by(Sample.File.Name) %>%
      mutate(Relative_height = Height / max(Height)) %>%
      ungroup()

    modal_lines <- sample_reference_table %>%
      filter(Group == case_id, is.finite(Reference_modal_CAG)) %>%
      select(Sample.File.Name, Brain_region, Reference_modal_CAG)

    ggplot(pdat, aes(Approx_CAG_from_GeneMapper_size, Relative_height)) +
      geom_segment(aes(xend = Approx_CAG_from_GeneMapper_size, y = 0, yend = Relative_height)) +
      geom_point(size = 1) +
      geom_vline(
        data = modal_lines,
        aes(xintercept = Reference_modal_CAG),
        linetype = "dashed"
      ) +
      facet_wrap(~Brain_region, ncol = 4) +
      labs(
        title = paste(case_id, "physical GeneMapper peaks — v2.4 core"),
        subtitle = "Dashed line = common physical reference modal; repeat assignment not yet applied",
        x = "Approximate CAG from GeneMapper Size ((Size - 103) / 3)",
        y = "Peak height / sample maximum"
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }

  for (case_id in intersect(case_analysis_plan$Group, unique(brain_metadata$Group))) {
    p <- plot_case_physical_profiles(case_id)
    ggsave(
      file.path(output_directory, paste0(case_id, "_physical_profiles_v2_4_core.png")),
      p,
      width = 12,
      height = 8,
      dpi = 180
    )
  }

  # ==========================================================
  # 11. Record analysis parameters
  # ==========================================================

  analysis_parameters <- data.frame(
    Parameter = c(
      "Pipeline_version",
      "Project_directory",
      "Run_directory",
      "GeneMapper_input_file",
      "Plate_map_file",
      "Target_dye_channel",
      "Fixed_non_repeat_bp",
      "Minimum_fragment_size_bp",
      "Nominal_repeat_size_bp",
      "Modal_candidate_gap_bp",
      "Modal_candidate_signal_multiplier",
      "Primary_EI_window",
      "Hybrid_EI_thresholds",
      "Water_handling",
      "TempEPTData_handling",
      "Case_2_interpretation"
    ),
    Value = c(
      "v2.4_core_common_preprocessing",
      project_directory,
      run_directory,
      input_file,
      plate_map_file,
      target_dye_channel,
      fixed_bp,
      minimum_size_bp,
      repeat_size_bp,
      modal_candidate_gap_bp,
      modal_candidate_signal_multiplier,
      paste0("Delta_CAG 0:+", ei_tail_width),
      paste0(ei_pct_thresholds * 100, "% within Delta_CAG 0:+", ei_tail_width, collapse = "; "),
      "Water-only wells reported for audit; never used to filter brain peaks",
      "Run telemetry only; not used for peak sizing, repeat assignment, or EI",
      "normal_non_expanded; expanded EI not calculated"
    ),
    stringsAsFactors = FALSE
  )

  # ==========================================================
  # 12. Save common outputs
  # ==========================================================

  write_out(case_analysis_plan, "SCA2_case_analysis_plan_v2_4.csv")
  write_out(fsa_manifest, "SCA2_FSA_manifest_v2_4.csv")
  write_out(fsa_genemapper_crosscheck, "SCA2_FSA_vs_GeneMapper_crosscheck_v2_4.csv")
  write_out(temp_ept_manifest, "SCA2_TempEPTData_manifest_v2_4.csv")
  write_out(gm_clean, "SCA2_GeneMapper_cleaned_B_channel_v2_4.csv")
  write_out(gm_annotated, "SCA2_all_physical_peaks_annotated_v2_4.csv")
  write_out(brain_peaks, "SCA2_brain_physical_peaks_ge_103_v2_4.csv")
  write_out(brain_peaks_below_103, "SCA2_brain_peaks_below_103_audit_v2_4.csv")
  write_out(water_peak_records, "SCA2_water_only_records_audit_v2_4.csv")
  write_out(brain_metadata, "SCA2_brain_metadata_v2_4.csv")
  write_out(modal_calls, "SCA2_physical_modal_calls_v2_4.csv")
  write_out(sample_reference_table, "SCA2_sample_reference_table_v2_4.csv")
  write_out(sample_qc, "SCA2_sample_QC_v2_4.csv")
  write_out(analysis_parameters, "SCA2_analysis_parameters_v2_4.csv")

  core_bundle <- list(
    paths = list(
      project_directory = project_directory,
      run_directory = run_directory,
      temp_ept_directory = temp_ept_directory,
      input_file = input_file,
      plate_map_file = plate_map_file,
      output_directory = output_directory
    ),
    parameters = list(
      target_dye_channel = target_dye_channel,
      fixed_bp = fixed_bp,
      minimum_size_bp = minimum_size_bp,
      repeat_size_bp = repeat_size_bp,
      modal_candidate_gap_bp = modal_candidate_gap_bp,
      modal_candidate_signal_multiplier = modal_candidate_signal_multiplier,
      ei_tail_width = ei_tail_width,
      ei_pct_thresholds = ei_pct_thresholds
    ),
    case_analysis_plan = case_analysis_plan,
    fsa_manifest = fsa_manifest,
    fsa_genemapper_crosscheck = fsa_genemapper_crosscheck,
    temp_ept_manifest = temp_ept_manifest,
    plate_map = plate_map,
    gm_annotated = gm_annotated,
    brain_peaks = brain_peaks,
    brain_peaks_below_103 = brain_peaks_below_103,
    water_peak_records = water_peak_records,
    brain_metadata = brain_metadata,
    modal_calls = modal_calls,
    sample_reference_table = sample_reference_table,
    sample_qc = sample_qc
  )

  saveRDS(
    core_bundle,
    file.path(output_directory, "SCA2_v2_4_core_objects.rds")
  )

  cat(
    "\nDone: SCA2 ABI v2.4 CORE.\n",
    "Production peak source: GeneMapper Size + Height.\n",
    "Raw FSA files: indexed for independent TRACE validation.\n",
    "TempEPTData: audit only.\n",
    "Case_2: normal/non-expanded; expanded EI disabled.\n",
    "No repeat-assignment method has been applied yet.\n",
    "Next: run one or more v2.4 level-2 method scripts.\n",
    "Core object: ", file.path(output_directory, "SCA2_v2_4_core_objects.rds"), "\n",
    sep = ""
  )
}
