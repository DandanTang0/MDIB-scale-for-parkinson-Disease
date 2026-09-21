# ---------------------------------------------------------------------------- #
# Run Exploratory Factor Analysis for the PD paper
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running this script:
# 1. Restart R.
# 2. Set the working directory to the project parent folder.
# 3. Make sure the PD data preparation scripts export PD-labeled objects.
# ---------------------------------------------------------------------------- #
# Check R version and load packages ----
# ---------------------------------------------------------------------------- #

source("./code/1a_define_functions.R")

groundhog_day <- version_control()

pkgs <- c("psych", "lavaan")
groundhog.library(pkgs, groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Define helper functions ----
# ---------------------------------------------------------------------------- #

# Create directory if it does not already exist.
make_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
# Short labels used only in output folder/file names. 
short_item_name <- function(item) {
  item_map <- c(
    mdib_neg_int_remember_1b = "1b",
    mdib_neg_ext_server_2a   = "2a",
    mdib_neg_int_email_6b    = "6b"
  )

  if (item %in% names(item_map)) {
    unname(item_map[[item]])
  } else {
    item
  }
}

# Export lavaan EFA summaries, detailed output, loadings, and the fitted object.
export_efa_res <- function(fit, path, filename_stem) {
  make_dir(path)

  sink(file.path(path, paste0(filename_stem, ".txt")))
  print(summary(fit))
  sink()

  sink(file.path(path, paste0(filename_stem, "_detail.txt")))
  print(summary(fit, se = TRUE, zstat = TRUE, pvalue = TRUE))
  sink()

  sink(file.path(path, paste0(filename_stem, "_loadings.csv")))
  print(fit$loadings)
  sink()

  saveRDS(fit, file.path(path, paste0(filename_stem, ".rds")))
}

# Export ordinal item distributions as counts and percentages.
export_item_distributions <- function(df, path, filename_stem) {
  make_dir(path)

  dist_list <- lapply(names(df), function(item) {
    tab <- table(df[[item]], useNA = "ifany")
    data.frame(
      item = item,
      response = names(tab),
      n = as.integer(tab),
      percent = round(100 * as.integer(tab) / nrow(df), 2),
      stringsAsFactors = FALSE
    )
  })

  dist_df <- do.call(rbind, dist_list)
  write.csv(
    dist_df,
    file.path(path, paste0(filename_stem, "_item_distributions.csv")),
    row.names = FALSE
  )

  invisible(dist_df)
}

# Plot item histograms for visual inspection.
# The x-axis is fixed to the 0 to 4 MDIB response scale.
plot_item_hists <- function(df, path, filename_stem, n_per_page = 6) {
  make_dir(path)

  pdf(file.path(path, paste0(filename_stem, "_hists.pdf")), height = 6, width = 6)

  for (start_col in seq(1, ncol(df), by = n_per_page)) {
    end_col <- min(start_col + n_per_page - 1, ncol(df))
    cols <- start_col:end_col

    par(mfrow = c(3, 2))
    for (i in cols) {
      hist(
        df[[i]],
        main = names(df)[i],
        xlab = "",
        breaks = seq(-0.5, 4.5, by = 1),
        xaxt = "n"
      )
      axis(1, at = 0:4)
    }
  }

  dev.off()
}

# Convert MDIB items to ordered factors for categorical analyses.
# Script 2 restricts the EFA sample to complete baseline MDIB data, so this function 
# should receive item data with no missing responses.

make_ordered_mdib <- function(df) {
  stopifnot(sum(is.na(df)) == 0)
  stopifnot(sum(df == 99, na.rm = TRUE) == 0)
  
  df_ord <- as.data.frame(lapply(df, function(x) {
    factor(x, levels = sort(unique(x)), ordered = TRUE)
  }))
  
  stopifnot(ncol(df_ord) == ncol(df))
  stopifnot(all(names(df_ord) == names(df)))
  
  df_ord
}

# Run parallel analysis based on principal components and polychoric correlations.
run_pa_poly <- function(df, path, filename_stem, fm = "minres", n_iter = 100) {
  make_dir(path)

  pdf(file.path(path, paste0(filename_stem, "_pa_poly_", fm, ".pdf")),
      height = 6, width = 6)

  result <- psych::fa.parallel(
    df,
    fa = "pc",
    fm = fm,
    n.iter = n_iter,
    correct = 0,
    cor = "poly"
  )

  dev.off()

  n_random_mean <- sum(result$pc.values > result$pc.sim)
  n_resample_mean <- sum(result$pc.values > result$pc.simr)

  summary_df <- data.frame(
    filename_stem = filename_stem,
    fm = fm,
    n_iter = n_iter,
    threshold = c("random_data_mean", "resampled_data_mean"),
    n_components = c(n_random_mean, n_resample_mean),
    stringsAsFactors = FALSE
  )

  write.csv(
    summary_df,
    file.path(path, paste0(filename_stem, "_pa_poly_", fm, "_summary.csv")),
    row.names = FALSE
  )

  saveRDS(
    result,
    file.path(path, paste0(filename_stem, "_pa_poly_", fm, ".rds"))
  )

  list(result = result, summary = summary_df)
}

# Summarize two polychoric parallel-analysis runs and define candidate factor counts.
# If the random-data and resampled-data thresholds differ, the larger number is
# treated as the upper bound. Candidate EFA solutions then follow the preregistered
# +/- 1 approach.
summarize_pa_decision <- function(pa_minres, pa_ml, path, filename_stem) {
  make_dir(path)

  pa_summary <- rbind(pa_minres$summary, pa_ml$summary)

  upper_bound <- max(pa_summary$n_components, na.rm = TRUE)
  candidate_nfactors <- seq(max(1, upper_bound - 1), upper_bound + 1)

  decision_df <- data.frame(
    filename_stem = filename_stem,
    upper_bound = upper_bound,
    candidate_nfactors = paste(candidate_nfactors, collapse = ", "),
    stringsAsFactors = FALSE
  )

  write.csv(
    pa_summary,
    file.path(path, paste0(filename_stem, "_pa_poly_combined_summary.csv")),
    row.names = FALSE
  )

  write.csv(
    decision_df,
    file.path(path, paste0(filename_stem, "_pa_decision.csv")),
    row.names = FALSE
  )

  list(
    pa_summary = pa_summary,
    decision = decision_df,
    candidate_nfactors = candidate_nfactors
  )
}

# Run WLSMV EFAs with primary and sensitivity rotations.
# Oblimin is the primary rotation. Geomin and promax are used to evaluate whether
# the substantive loading pattern is robust to rotation choice.
run_wlsmv_efas <- function(df_ord, nfactors, path, filename_stem) {
  make_dir(path)

  rotations <- c("oblimin", "geomin", "promax")
  fits <- list()

  for (rotation in rotations) {
    set.seed(1234)

    fit <- lavaan::efa(
      data = df_ord,
      nfactors = nfactors,
      rotation = rotation,
      estimator = "WLSMV",
      ordered = names(df_ord),
      check.vcov = FALSE
    )

    fits[[rotation]] <- fit

    export_efa_res(
      fit = fit,
      path = path,
      filename_stem = paste0(filename_stem, "_", rotation, "_wlsmv")
    )
  }

  fits
}

# Run one item-removal sequence.
# Each step removes one additional item, reruns polychoric parallel analysis, and
# reruns WLSMV EFAs. These results are used to compare strategically chosen
# removal paths, not to exhaustively search every possible item subset.
run_removal_sequence <- function(df, sequence_name, removal_order, base_path) {
  sequence_path <- file.path(base_path, sequence_name)
  make_dir(sequence_path)

  current_df <- df
  sequence_log <- data.frame(
    step = integer(),
    removed_item = character(),
    retained_n_items = integer(),
    retained_items = character(),
    stringsAsFactors = FALSE
  )

  for (step in seq_along(removal_order)) {
    item_to_remove <- removal_order[step]

    if (!item_to_remove %in% names(current_df)) {
      stop(paste0("Item not found in current data: ", item_to_remove))
    }

    current_df <- current_df[, names(current_df) != item_to_remove, drop = FALSE]

    step_name <- paste0("s", sprintf("%02d", step), "_minus_", short_item_name(item_to_remove))
    step_path <- file.path(sequence_path, step_name)

    # Parallel analysis is rerun after each item removal because removing one
    # item can change the observed and simulated eigenvalue comparison.
    pa_minres <- run_pa_poly(
      current_df,
      path = file.path(step_path, "pa"),
      filename_stem = step_name,
      fm = "minres",
      n_iter = 100
    )

    pa_ml <- run_pa_poly(
      current_df,
      path = file.path(step_path, "pa"),
      filename_stem = step_name,
      fm = "ml",
      n_iter = 100
    )

    pa_decision <- summarize_pa_decision(
      pa_minres,
      pa_ml,
      path = file.path(step_path, "pa"),
      filename_stem = step_name
    )

    df_ord <- make_ordered_mdib(current_df)

    run_wlsmv_efas(
      df_ord = df_ord,
      nfactors = pa_decision$candidate_nfactors,
      path = file.path(step_path, "efa"),
      filename_stem = step_name
    )

    sequence_log <- rbind(
      sequence_log,
      data.frame(
        step = step,
        removed_item = item_to_remove,
        retained_n_items = ncol(current_df),
        retained_items = paste(names(current_df), collapse = ", "),
        stringsAsFactors = FALSE
      )
    )

    save(
      current_df,
      file = file.path(step_path, paste0(step_name, "_numeric.RData"))
    )

    save(
      df_ord,
      file = file.path(step_path, paste0(step_name, "_ordered.RData"))
    )
  }

  write.csv(
    sequence_log,
    file.path(sequence_path, paste0(sequence_name, "_sequence_log.csv")),
    row.names = FALSE
  )

  invisible(sequence_log)
}

# ---------------------------------------------------------------------------- #
# Import PD data ----
# ---------------------------------------------------------------------------- #
load("./data/further_clean/mdib_pd_dat.RData")
load("./data/helper/mdib_dat_items.RData")
load("./data/helper/mdib_item_map.RData")

stopifnot(exists("mdib_pd_dat"))
stopifnot(exists("mdib_dat_items"))
stopifnot(exists("mdib_item_map"))

# ---------------------------------------------------------------------------- #
# Prepare baseline MDIB item data ----
# ---------------------------------------------------------------------------- #

mdib_items <- c(mdib_dat_items$mdib_ben, mdib_dat_items$mdib_neg)

mdib_bl <- mdib_pd_dat[
  mdib_pd_dat$redcap_event_name == "baseline_arm_1",
  mdib_items
]

# Order columns by meaning and then domain, using the predefined item map.
mdib_item_map <- mdib_item_map[order(mdib_item_map$meaning, mdib_item_map$domain), ]

mdib_bl <- mdib_bl[
  match(mdib_item_map$items_rename, names(mdib_bl))
]

stopifnot(ncol(mdib_bl) == 36)
stopifnot(all(names(mdib_bl) == mdib_item_map$items_rename))

# Confirm that the imported baseline MDIB item data are complete.

# Confirm the expected EFA analysis sample size for the current cleaned PD data
# export.
expected_efa_n <- 82

stopifnot(nrow(mdib_bl) == length(unique(mdib_pd_dat$record_id)))
stopifnot(sum(is.na(mdib_bl)) == 0)
stopifnot(sum(mdib_bl == 99, na.rm = TRUE) == 0)
stopifnot(nrow(mdib_bl) == expected_efa_n)


# ---------------------------------------------------------------------------- #
# Define output paths ----
# ---------------------------------------------------------------------------- #

efa_path <- "./results/efa_pd/"
make_dir(efa_path)

# ---------------------------------------------------------------------------- #
# Step 1: Inspect item distributions ----
# ---------------------------------------------------------------------------- #

dist_path <- file.path(efa_path, "dist")
make_dir(dist_path)

export_item_distributions(
  df = mdib_bl,
  path = dist_path,
  filename_stem = "all36"
)

plot_item_hists(
  df = mdib_bl,
  path = dist_path,
  filename_stem = "all36"
)

# ---------------------------------------------------------------------------- #
# Step 2: Parallel analysis for all 36 MDIB items ----
# ---------------------------------------------------------------------------- #

all_items_path <- file.path(efa_path, "all36")
all_pa_path <- file.path(all_items_path, "pa")

pa_all_minres <- run_pa_poly(
  df = mdib_bl,
  path = all_pa_path,
  filename_stem = "all36",
  fm = "minres",
  n_iter = 100
)

pa_all_ml <- run_pa_poly(
  df = mdib_bl,
  path = all_pa_path,
  filename_stem = "all36",
  fm = "ml",
  n_iter = 100
)

pa_all_decision <- summarize_pa_decision(
  pa_minres = pa_all_minres,
  pa_ml = pa_all_ml,
  path = all_pa_path,
  filename_stem = "all36"
)

# ---------------------------------------------------------------------------- #
# Step 3: WLSMV EFAs for all 36 MDIB items ----
# ---------------------------------------------------------------------------- #

mdib_bl_ord <- make_ordered_mdib(mdib_bl)

fits_all_36 <- run_wlsmv_efas(
  df_ord = mdib_bl_ord,
  nfactors = pa_all_decision$candidate_nfactors,
  path = file.path(all_items_path, "efa"),
  filename_stem = "all36"
)

# ---------------------------------------------------------------------------- #
# Step 4: Restrict to the 12 theorized negative bias items ----
# ---------------------------------------------------------------------------- #

mdib_bl_neg_12 <- mdib_bl[, grepl("^mdib_neg", names(mdib_bl)), drop = FALSE]

stopifnot(ncol(mdib_bl_neg_12) == 12)

# The 12 negative items are a subset of the complete baseline MDIB item data.
stopifnot(sum(is.na(mdib_bl_neg_12)) == 0)
stopifnot(sum(mdib_bl_neg_12 == 99, na.rm = TRUE) == 0)
stopifnot(nrow(mdib_bl_neg_12) == expected_efa_n)


neg_12_path <- file.path(efa_path, "neg12")
neg_12_dist_path <- file.path(neg_12_path, "dist")

export_item_distributions(
  df = mdib_bl_neg_12,
  path = neg_12_dist_path,
  filename_stem = "neg12"
)

plot_item_hists(
  df = mdib_bl_neg_12,
  path = neg_12_dist_path,
  filename_stem = "neg12"
)

# ---------------------------------------------------------------------------- #
# Step 5: Parallel analysis for the 12 negative bias items ----
# ---------------------------------------------------------------------------- #

neg_12_pa_path <- file.path(neg_12_path, "pa")

pa_neg_12_minres <- run_pa_poly(
  df = mdib_bl_neg_12,
  path = neg_12_pa_path,
  filename_stem = "neg12",
  fm = "minres",
  n_iter = 100
)

pa_neg_12_ml <- run_pa_poly(
  df = mdib_bl_neg_12,
  path = neg_12_pa_path,
  filename_stem = "neg12",
  fm = "ml",
  n_iter = 100
)

pa_neg_12_decision <- summarize_pa_decision(
  pa_minres = pa_neg_12_minres,
  pa_ml = pa_neg_12_ml,
  path = neg_12_pa_path,
  filename_stem = "neg12"
)

# ---------------------------------------------------------------------------- #
# Step 6: WLSMV EFAs for the 12 negative bias items ----
# ---------------------------------------------------------------------------- #

mdib_bl_neg_12_ord <- make_ordered_mdib(mdib_bl_neg_12)

fits_neg_12 <- run_wlsmv_efas(
  df_ord = mdib_bl_neg_12_ord,
  nfactors = pa_neg_12_decision$candidate_nfactors,
  path = file.path(neg_12_path, "efa"),
  filename_stem = "neg12"
)

# Save the complete baseline 12-negative-item data used in the EFA. These
# objects should reflect the preregistered analysis sample restriction.

stopifnot(nrow(mdib_bl_neg_12) == expected_efa_n)
stopifnot(sum(is.na(mdib_bl_neg_12)) == 0)
stopifnot(sum(mdib_bl_neg_12 == 99, na.rm = TRUE) == 0)


save(
  mdib_bl_neg_12,
  file = "./data/further_clean/mdib_bl_neg_12_pd.RData"
)

save(
  mdib_bl_neg_12_ord,
  file = "./data/further_clean/mdib_bl_neg_12_ord_pd.RData"
)

# ---------------------------------------------------------------------------- #
# Step 7: item-removal sequence
# ---------------------------------------------------------------------------- #
# The sequence starts with the two most consistently problematic items:
# - mdib_neg_ext_server_2a: external item that repeatedly loaded with internal items.
# - mdib_neg_int_remember_1b: nonsalient loading and very low communality.

#
# Step 2 of this sequence gives the balanced 10-item candidate:
# remove mdib_neg_ext_server_2a, and mdib_neg_int_remember_1b retain
# mdib_neg_int_email_6b.
#
# Step 3 gives the stricter 9-item candidate:
# also remove mdib_neg_int_email_6b.

removal_sequences <- list(
  focused = c(
    "mdib_neg_ext_server_2a",
    "mdib_neg_int_remember_1b",
    "mdib_neg_int_email_6b"
  )
)

removal_path <- file.path(efa_path, "orginal_EFA")

for (sequence_name in names(removal_sequences)) {
  run_removal_sequence(
    df = mdib_bl_neg_12,
    sequence_name = sequence_name,
    removal_order = removal_sequences[[sequence_name]],
    base_path = removal_path
  )
}


# ---------------------------------------------------------------------------- #
# Step 8: Sensitivity analysis about choosing item-removal sequences for negative items 
# ---------------------------------------------------------------------------- #

# These sequences are not exhaustive. They are chosen to reflect decision rules
# that can be described in the paper:
#
# Sequence A: Start with the item that did not load saliently on either factor,
# then remove the internal item with a mild cross-loading and the external item
# with a theory-inconsistent loading.
#
# Sequence B: Start with the external item that loaded with the internal factor,
# then remove the internal item with a mild cross-loading, and then remove the
# nonsalient item if still needed.
#
# Sequence C: Start with the internal item with a mild cross-loading, then remove
# the external item that loaded with the internal factor, and then remove the
# nonsalient item if still needed.
#

removal_sequences <- list(
  ns_int_ext = c(
    "mdib_neg_int_remember_1b",
    "mdib_neg_int_email_6b",
    "mdib_neg_ext_server_2a"
  ),
  ext_first = c(
    "mdib_neg_ext_server_2a",
    "mdib_neg_int_email_6b",
    "mdib_neg_int_remember_1b"
  ),
  int_first = c(
    "mdib_neg_int_email_6b",
    "mdib_neg_ext_server_2a",
    "mdib_neg_int_remember_1b"
  )
)

removal_path <- file.path(efa_path, "rm_seq")

for (sequence_name in names(removal_sequences)) {
  run_removal_sequence(
    df = mdib_bl_neg_12,
    sequence_name = sequence_name,
    removal_order = removal_sequences[[sequence_name]],
    base_path = removal_path
  )
}

# ---------------------------------------------------------------------------- #
# Step 9: Sensitivity analysis for additional sequence 
# ---------------------------------------------------------------------------- #

# Sequence D: Start with the item that did not load saliently on either factor,
# then remove the external item with a theory-inconsistent loading and the internal 
#item with a mild cross-loading.


removal_sequences <- list(
  focused = c(
    "mdib_neg_int_remember_1b",
    "mdib_neg_ext_server_2a",
    "mdib_neg_int_email_6b"
  )
)

removal_path <- file.path(efa_path, "rm_Seq_D")

for (sequence_name in names(removal_sequences)) {
  run_removal_sequence(
    df = mdib_bl_neg_12,
    sequence_name = sequence_name,
    removal_order = removal_sequences[[sequence_name]],
    base_path = removal_path
  )
}

# ---------------------------------------------------------------------------- #
# End of script ----
# ---------------------------------------------------------------------------- #
