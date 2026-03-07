

# ============================================================
# STEP 0: Libraries
# ============================================================
library(dplyr)
library(tidyr)
library(psych)       # fa(), KMO(), cortest.bartlett()
library(GPArotation) # oblimin / varimax rotation
library(ggplot2)
library(forcats)

# ============================================================
# STEP 1: Define the candidate indicator columns for EFA
# ============================================================
# These are neighbourhood-level aggregates joined onto master_merged.
# Exclude identifiers, address fields, raw assessment values, and
# sparse multi-class columns (property_class_2 etc.).

indicator_cols <- c(
  # Housing stock
  "total_living_area",
  "year_built",
  "rooms",
  # Amenity / green space
  "parks_count",
  "parks_total_area_ha",
  "tree_count",
  # Safety / disorder
  "substance_incidents_total",
  "naloxone_incidents_total",
  "naloxone_doses_total",
  "wfps_incidents_total",
  "wfps_median_response_mins",
  # Built environment activity
  "trade_permits_total",
  "bldg_permits_total",
  "bldg_value_total",
  "bldg_major_projects",
  # Social vulnerability
  "rooming_complaints_total",
  "rooming_proactive_total",
  "population_total",
  "population_in_mbm",
  "land_area_in_square_km"
)

# ============================================================
# STEP 2: Missing value audit — flag columns > 20% NA
# ============================================================
na_audit <- master_merged |>
  dplyr::select(dplyr::all_of(indicator_cols)) |>
  dplyr::summarise(dplyr::across(
    dplyr::everything(),
    \(x) round(mean(is.na(x)) * 100, 1)
  )) |>
  tidyr::pivot_longer(
    cols      = dplyr::everything(),
    names_to  = "column",
    values_to = "pct_na"
  ) |>
  dplyr::mutate(
    flag = dplyr::case_when(
      pct_na > 20 ~ "REMOVE  (>20% NA)",
      pct_na > 5  ~ "REVIEW  (5–20% NA)",
      TRUE        ~ "OK"
    )
  ) |>
  dplyr::arrange(dplyr::desc(pct_na))

print(na_audit, n = Inf)

# ============================================================
# STEP 3: Drop flagged columns; keep only REMOVE-free indicators
# ============================================================
cols_to_remove <- na_audit |>
  dplyr::filter(flag == "REMOVE  (>20% NA)") |>
  dplyr::pull(column)

message("\nColumns removed (>20% NA): ", paste(cols_to_remove, collapse = ", "))

cols_for_efa <- setdiff(indicator_cols, cols_to_remove)

# ============================================================
# STEP 4: Build neighbourhood-level summary table for EFA
# (one row per neighbourhood — EFA works on aggregate, not parcel)
# ============================================================
neigh_efa <- master_merged |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(cols_for_efa),
      \(x) mean(x, na.rm = TRUE)   # parcel → neighbourhood mean
    ),
    .groups = "drop"
  ) |>
  # Drop neighbourhoods still missing >50% of retained indicators
  dplyr::filter(
    rowMeans(is.na(dplyr::across(dplyr::all_of(cols_for_efa)))) < 0.5
  )

message("Neighbourhoods available for EFA: ", nrow(neigh_efa))

# ============================================================
# STEP 5: Scale indicators (z-score) and remove near-zero variance
# ============================================================
efa_matrix <- neigh_efa |>
  dplyr::select(dplyr::all_of(cols_for_efa)) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) |>
  # Impute remaining NAs with column median before scaling
  dplyr::mutate(dplyr::across(
    dplyr::everything(),
    \(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)
  )) |>
  scale() |>
  as.data.frame()

# Remove near-zero variance columns (sd ≈ 0 after scaling means constant)
nzv_check <- sapply(efa_matrix, sd, na.rm = TRUE)
efa_matrix <- efa_matrix[, nzv_check > 0.01]

message("Indicators entering EFA: ", ncol(efa_matrix))

# ============================================================
# STEP 6: EFA pre-checks — KMO & Bartlett's test
# ============================================================
cor_matrix <- cor(efa_matrix, use = "pairwise.complete.obs")

kmo_result <- psych::KMO(cor_matrix)
cat("\n── KMO Measure of Sampling Adequacy ──\n")
print(kmo_result$MSA)          # Overall: want > 0.60
print(kmo_result$MSAi)         # Per-variable: flag < 0.50

bart_result <- psych::cortest.bartlett(cor_matrix, n = nrow(efa_matrix))
cat("\n── Bartlett's Test of Sphericity ──\n")
cat("Chi-sq =", round(bart_result$chisq, 2),
    "  df =", bart_result$df,
    "  p =", format.pval(bart_result$p.value, digits = 3), "\n")
# Want p < 0.05 to confirm correlations exist

# ============================================================
# STEP 7: Determine number of factors — parallel analysis + scree
# ============================================================
psych::fa.parallel(
  efa_matrix,
  fm      = "ml",      # maximum likelihood
  fa      = "fa",
  n.iter  = 100,
  main    = "Parallel Analysis Scree Plot"
)
# Read suggested nfactors from console output

# ---- Set n_factors based on parallel analysis result ----
n_factors <- 4   # <-- update after inspecting parallel analysis output

# ============================================================
# STEP 8: Run EFA
# ============================================================
efa_result <- psych::fa(
  r        = efa_matrix,
  nfactors = n_factors,
  rotate   = "oblimin",   # oblique — factors likely correlated
  fm       = "ml",
  scores   = "regression"
)

cat("\n── EFA Summary ──\n")
print(efa_result$loadings, cutoff = 0.30, sort = TRUE)

cat("\n── Variance Explained ──\n")
print(efa_result$Vaccounted)

cat("\n── Factor Intercorrelations ──\n")
print(round(efa_result$Phi, 3))

# ============================================================
# STEP 9: Extract factor scores → attach to neighbourhood table
# ============================================================
factor_scores <- as.data.frame(efa_result$scores)

# Name factors based on loadings (edit labels after inspection)
factor_labels <- c(
  "F1_GreenAmenity",      # e.g. parks, trees
  "F2_SafetyDisorder",    # e.g. substance, naloxone, wfps
  "F3_BuildtActivity",    # e.g. permits, construction value
  "F4_SocialVulnerability" # e.g. poverty, rooming
)
names(factor_scores) <- factor_labels[seq_len(n_factors)]

neigh_scores <- dplyr::bind_cols(
  neigh_efa |> dplyr::select(neighbourhood_area),
  factor_scores
)

# ============================================================
# STEP 10: Compute composite Liveability Index
# (equal-weighted average of factors; flip sign of negative factors)
# ============================================================
# Convention: higher score = better liveability
# Flip factors where high score = BAD (safety/disorder, vulnerability)
neigh_scores <- neigh_scores |>
  dplyr::mutate(
    # Negate disorder/vulnerability factors so direction is consistent
    F2_SafetyDisorder_inv    = -F2_SafetyDisorder,
    F4_SocialVulnerability_inv = -F4_SocialVulnerability,
    
    liveability_index = rowMeans(dplyr::pick(
      F1_GreenAmenity,
      F2_SafetyDisorder_inv,
      F3_BuildtActivity,
      F4_SocialVulnerability_inv
    ), na.rm = TRUE),
    
    # Rescale to 0–100 for interpretability
    liveability_score = scales::rescale(liveability_index, to = c(0, 100))
  ) |>
  dplyr::arrange(dplyr::desc(liveability_score))

# ============================================================
# STEP 11: Visualise — top / bottom 20 neighbourhoods
# ============================================================
top_bottom_20 <- dplyr::bind_rows(
  head(neigh_scores, 20) |> dplyr::mutate(group = "Top 20"),
  tail(neigh_scores, 20) |> dplyr::mutate(group = "Bottom 20")
)

ggplot2::ggplot(
  top_bottom_20,
  ggplot2::aes(
    x    = liveability_score,
    y    = forcats::fct_reorder(neighbourhood_area, liveability_score),
    fill = group
  )
) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(values = c("Top 20" = "#2ecc71", "Bottom 20" = "#e74c3c")) +
  ggplot2::labs(
    title    = "Winnipeg Neighbourhood Liveability Index",
    subtitle = paste0("EFA-derived composite (", n_factors, " factors) | 0–100 scale"),
    x        = "Liveability Score",
    y        = NULL,
    fill     = NULL,
    caption  = "Source: City of Winnipeg Open Data · Assessment parcels anchor · 2021–2023 indicators"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "top")

# ============================================================
# STEP 12: Save outputs
# ============================================================
readr::write_csv(neigh_scores,   "winnipeg_liveability_scores.csv")
readr::write_csv(na_audit,       "na_audit_indicators.csv")

# Full factor loading table for reporting
loadings_df <- as.data.frame(unclass(efa_result$loadings)) |>
  tibble::rownames_to_column("indicator")
readr::write_csv(loadings_df, "efa_factor_loadings.csv")

message("Done. Outputs written to working directory.")

