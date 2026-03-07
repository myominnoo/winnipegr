library(tidyverse)
library(arrow)

# ── 1. Pull data ──────────────────────────────────────────────────────────────
raw_assessment_parcels <- read_parquet("data/raw_assessment_parcels.parquet")

# write_parquet(raw |> select(-geometry), "data/raw_assessment_parcels.parquet")
# ── 2. Classify columns ───────────────────────────────────────────────────────

# Columns to skip entirely (identifiers, URLs, geometry, constant fields)
skip_cols <- c(
  "roll_number", "unit_number", "street_suffix", "full_address", "street_name",
  "detail_url", "geometry", "assessment_date", "proposed_assessment_date",
  "current_assessment_year", "proposed_assessment_year",
  # proposed / secondary assessment classes are very sparse – keep primary only
  "proposed_property_class_1", "proposed_property_class_2",
  "proposed_property_class_3", "proposed_property_class_4",
  "proposed_property_class_5",
  "proposed_status_1", "proposed_status_2", "proposed_status_3",
  "proposed_status_4", "proposed_status_5",
  "proposed_assessment_value_1", "proposed_assessment_value_2",
  "proposed_assessment_value_3", "proposed_assessment_value_4",
  "proposed_assessment_value_5",
  "property_class_2", "property_class_3", "property_class_4", "property_class_5",
  "status_2", "status_3", "status_4", "status_5",
  "assessed_value_2", "assessed_value_3", "assessed_value_4", "assessed_value_5",
  "total_proposed_assessment_value",  # redundant with proposed_assessment_value_1
  "neighbourhood_area"                # grouping key – exclude from summaries
)

group_col  <- "neighbourhood_area"

# Coerce known numeric columns (JSON may read some as character)
numeric_cols <- c(
  "street_number", "total_living_area", "year_built", "rooms",
  "number_floors_condo", "assessed_land_area", "water_frontage_measurement",
  "sewer_frontage_measurement", "total_assessed_value", "assessed_value_1",
  "centroid_lat", "centroid_lon", "gisid", "dwelling_units"
)

# Only coerce columns that actually exist in the pulled data
numeric_cols <- intersect(numeric_cols, names(raw_assessment_parcels))

raw_assessment_parcels <- raw_assessment_parcels |>
  mutate(across(all_of(numeric_cols), as.numeric))

# Categorical cols = everything not in skip_cols and not numeric
cat_cols <- setdiff(
  names(raw_assessment_parcels),
  c(skip_cols, numeric_cols, group_col)
)

# ── 3. Numeric summary table ──────────────────────────────────────────────────

numeric_summary <- raw_assessment_parcels |>
  group_by(neighbourhood_area) |>
  summarise(
    n_properties = n(),
    across(
      all_of(numeric_cols),
      list(
        min    = \(x) min(x,                  na.rm = TRUE),
        q25    = \(x) quantile(x, 0.25,       na.rm = TRUE),
        median = \(x) median(x,               na.rm = TRUE),
        mean   = \(x) mean(x,                 na.rm = TRUE),
        q75    = \(x) quantile(x, 0.75,       na.rm = TRUE),
        max    = \(x) max(x,                  na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  )

# ── 4. Categorical count tables, one per column ───────────────────────────────
#
# For each categorical column produce a wide pivot:
#   neighbourhood_area | <col>__<value1> | <col>__<value2> | …
#
# Then join them all into one wide table.

make_cat_wide <- function(df, col) {
  df |>
    filter(!is.na(.data[[col]]), .data[[col]] != "") |>
    count(neighbourhood_area, value = .data[[col]]) |>
    mutate(value = paste0(col, "__", value)) |>
    pivot_wider(
      names_from  = value,
      values_from = n,
      values_fill = 0L
    )
}

cat_wide_list <- map(cat_cols, \(col) make_cat_wide(raw_assessment_parcels, col))

cat_summary <- reduce(
  cat_wide_list,
  full_join,
  by = "neighbourhood_area"
)

# ── 5. Final neighbourhood table ──────────────────────────────────────────────

neighbourhood_summary <- numeric_summary |>
  left_join(cat_summary, by = "neighbourhood_area") |>
  arrange(neighbourhood_area)

neighbourhood_summary
