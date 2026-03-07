# Inspect column types, names, and sample values per dataset ----
glimpse_all(data_raw_list)



# Verify dimensions — check for truncated pulls against expected row counts ----
purrr::map(data_raw_list, base::dim)
purrr::map(data_raw_list, base::names)

# Identify available join keys for merging datasets later ----
purrr::map(data_raw_list, \(df) base::grep(
  pattern     = "neighbourhood|neigh|id|ward",
  x           = base::names(df),
  value       = TRUE,
  ignore.case = TRUE
))


# Merge Strategy ----

# Step 1: Prepare master — normalise neighbourhood key ----
# Assessment parcels has no date — use full dataset as parcel-level anchor
master <- data_raw_list$assessment_parcels |>
  dplyr::mutate(
    neighbourhood_area = clean_neighbourhood(neighbourhood_area)
  )


# Step 2: Aggregate parks by neighbourhood ----
# Parks is a static dataset with no date column — use full dataset

# 2a: Neighbourhood-level totals ----
parks_totals <- data_raw_list$parks_open_space |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood)) |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    parks_count         = dplyr::n(),
    parks_total_area_ha = base::sum(base::as.numeric(total_area_in_hectares), na.rm = TRUE),
    parks_land_area_ha  = base::sum(base::as.numeric(land_area_in_hectares),  na.rm = TRUE),
    parks_water_area_ha = base::sum(base::as.numeric(water_area_in_hectares), na.rm = TRUE),
    .groups = "drop"
  )

# 2b: Neighbourhood-level totals by park category (wide format) ----
parks_by_category <- data_raw_list$parks_open_space |>
  dplyr::mutate(
    neighbourhood_area = clean_neighbourhood(neighbourhood),
    park_category      = clean_neighbourhood(park_category)
  ) |>
  dplyr::group_by(neighbourhood_area, park_category) |>
  dplyr::summarise(
    parks_count         = dplyr::n(),
    parks_total_area_ha = base::sum(base::as.numeric(total_area_in_hectares), na.rm = TRUE),
    parks_land_area_ha  = base::sum(base::as.numeric(land_area_in_hectares),  na.rm = TRUE),
    parks_water_area_ha = base::sum(base::as.numeric(water_area_in_hectares), na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    id_cols     = neighbourhood_area,
    names_from  = park_category,
    values_from = c(parks_count, parks_total_area_ha, parks_land_area_ha, parks_water_area_ha),
    values_fill = 0,
    names_glue  = "{.value}__{park_category}" |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("\\s+|-", "_")
  )

# 2c: Join totals + by-category into one parks table — one row per neighbourhood ----
parks_agg <- parks_totals |>
  dplyr::left_join(parks_by_category, by = "neighbourhood_area")



# Step 3: Aggregate substance use by neighbourhood (2021–2023) ----
substance_agg <- data_raw_list$substance_use |>
  dplyr::filter(dplyr::between(
    base::as.Date(dispatch_date), INDEX_START, INDEX_END
  )) |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood)) |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    substance_incidents_total = dplyr::n(),
    substance_alcohol         = base::sum(substance == "Alcohol",      na.rm = TRUE),
    substance_drug            = base::sum(substance != "Alcohol",      na.rm = TRUE),
    substance_cocaine         = base::sum(substance == "Cocaine",      na.rm = TRUE),
    substance_opioids         = base::sum(substance == "Opioids",      na.rm = TRUE),
    substance_marijuana       = base::sum(substance == "Marijuana",    na.rm = TRUE),
    substance_meth            = base::sum(substance == "Crystal Meth", na.rm = TRUE),
    .groups = "drop"
  )

# Step 4: Aggregate naloxone by neighbourhood (2021–2023, drop NA neighbourhoods) ----
naloxone_agg <- data_raw_list$naloxone_administrations |>
  dplyr::filter(
    !is.na(neighbourhood),
    dplyr::between(base::as.Date(dispatch_date), INDEX_START, INDEX_END)
  ) |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood)) |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    naloxone_incidents_total = dplyr::n(),
    naloxone_doses_total     = base::sum(
      base::as.numeric(naxolone_administrations), na.rm = TRUE
    ),
    .groups = "drop"
  )
# 
# # Step 5: Aggregate 311 requests (2021–2023, drop blank neighbourhood rows) ----
# requests_311_agg <- data_raw_list$requests_311 |>
#   dplyr::filter(
#     neighbourhood != "",
#     dplyr::between(base::as.Date(open_date), INDEX_START, INDEX_END)
#   ) |>
#   dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood)) |>
#   dplyr::group_by(neighbourhood_area) |>
#   dplyr::summarise(
#     requests_311_total  = dplyr::n(),
#     requests_311_open   = base::sum(case_status == "Open",   na.rm = TRUE),
#     requests_311_closed = base::sum(case_status == "Closed", na.rm = TRUE),
#     .groups = "drop"
#   )

# Step 6: Aggregate trade permits by neighbourhood (2021–2023) ----
trade_permits_agg <- data_raw_list$trade_permits |>
  dplyr::filter(
    dplyr::between(base::as.Date(issue_date), INDEX_START, INDEX_END)
  ) |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood_name)) |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    trade_permits_total  = dplyr::n(),
    trade_permits_closed = base::sum(status == "Closed", na.rm = TRUE),
    trade_permits_issued = base::sum(status == "Issued", na.rm = TRUE),
    .groups = "drop"
  )

# Step 7: Aggregate rooming house enforcement by neighbourhood (2021–2023) ----
rooming_house_agg <- data_raw_list$rooming_house_enforcement |>
  dplyr::filter(
    lubridate::year(year) >= lubridate::year(INDEX_START) &
      lubridate::year(year) <= lubridate::year(INDEX_END)
  ) |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood)) |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    rooming_complaints_total = base::sum(base::as.numeric(complaint_driven),     na.rm = TRUE),
    rooming_proactive_total  = base::sum(base::as.numeric(proactive_enforcement), na.rm = TRUE),
    rooming_completed_total  = base::sum(base::as.numeric(completed),             na.rm = TRUE),
    .groups = "drop"
  )

# Step 8: Clean higher poverty areas — filter to 2021 Census ----
# 1,144 rows = multiple census years — keep 2021 Census rows only
higher_poverty_clean <- data_raw_list$higher_poverty_areas |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood_name)) |>
  dplyr::arrange(neighbourhood_area, dplyr::desc(id)) |>
  dplyr::distinct(neighbourhood_area, .keep_all = TRUE) |>
  dplyr::select(
    neighbourhood_area,
    high_poverty_status,
    population_total,
    population_in_mbm,
    land_area_in_square_km
  )

# Step 9: Aggregate short term rentals — ward level, 2021–2023 licences ----
rentals_agg <- data_raw_list$short_term_rentals |>
  dplyr::filter(
    !is.na(electoral_ward),
    dplyr::between(base::as.Date(date_licence_was_issued), INDEX_START, INDEX_END)
  ) |>
  dplyr::group_by(electoral_ward) |>
  dplyr::summarise(
    rentals_total       = dplyr::n(),
    rentals_primary     = base::sum(primary_non_primary == "Primary",     na.rm = TRUE),
    rentals_non_primary = base::sum(primary_non_primary == "Non Primary", na.rm = TRUE),
    .groups = "drop"
  )


# Step 9b: Aggregate building permits by neighbourhood (2021–2023) ----
# Residential vs non-residential breakdown; value in declared construction $
building_permits_agg <- data_raw_list$aggregate_building_permits |>
  dplyr::filter(
    as.integer(year) >= lubridate::year(INDEX_START),
    as.integer(year) <= lubridate::year(INDEX_END)
  ) |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood)) |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    bldg_permits_total       = base::sum(base::as.numeric(total_permits),                    na.rm = TRUE),
    bldg_permits_residential = base::sum(base::as.numeric(total_permits[permit_group == "Residential"]),     na.rm = TRUE),
    bldg_permits_nonres      = base::sum(base::as.numeric(total_permits[permit_group == "Non-Residential"]), na.rm = TRUE),
    bldg_value_total         = base::sum(base::as.numeric(total_declared_construction_value), na.rm = TRUE),
    bldg_major_projects      = base::sum(base::as.numeric(major_projects_count),              na.rm = TRUE),
    .groups = "drop"
  )

# Step 9c: Tree inventory — one row per neighbourhood, already aggregated ----
tree_agg <- data_raw_list$tree_inventory |>
  dplyr::mutate(
    neighbourhood_area = clean_neighbourhood(neighbourhood),
    tree_count         = base::as.numeric(count)
  ) |>
  dplyr::select(neighbourhood_area, tree_count)


# Step 9d: WFPS call logs — response time by neighbourhood (2021–2023) ----
# Call Time / Closed Time are chr: "2022 Apr 05 04:17:33 AM"
wfps_agg <- data_raw_list$wfps_call_logs |>
  dplyr::rename(
    neighbourhood_area = Neighbourhood,
    incident_type      = `Incident Type`,
    call_time          = `Call Time`,
    closed_time        = `Closed Time`
  ) |>
  dplyr::mutate(
    neighbourhood_area = clean_neighbourhood(neighbourhood_area),
    call_dt   = lubridate::parse_date_time(call_time,   orders = "Y b d I:M:S p"),
    closed_dt = lubridate::parse_date_time(closed_time, orders = "Y b d I:M:S p"),
    response_mins = base::as.numeric(difftime(closed_dt, call_dt, units = "mins"))
  ) |>
  dplyr::filter(
    dplyr::between(base::as.Date(call_dt), INDEX_START, INDEX_END),
    response_mins >= 0   # drop negative / malformed times
  ) |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    wfps_incidents_total    = dplyr::n(),
    wfps_median_response_mins = stats::median(response_mins, na.rm = TRUE),
    wfps_mean_response_mins   = base::mean(response_mins,   na.rm = TRUE),
    .groups = "drop"
  )

# Step 10: Join audit helper ----
audit_join <- function(master, lookup, by, dataset_name) {
  
  join_key     <- base::unname(by)
  master_keys  <- master[[join_key]]
  lookup_keys  <- lookup[[join_key]]
  
  matched      <- base::sum(master_keys %in% lookup_keys)
  unmatched    <- base::sum(!master_keys %in% lookup_keys)
  unmatched_keys <- base::unique(master_keys[!master_keys %in% lookup_keys])
  
  base::message(
    "\n── ", dataset_name, " (joined on '", join_key, "') ",
    base::strrep("─", 30 - base::nchar(dataset_name)),
    "\n   matched   : ", matched, " / ", base::nrow(master), " rows",
    "\n   unmatched : ", unmatched, " rows",
    if (unmatched > 0) base::paste0(
      "\n   missing   : ",
      base::paste(utils::head(unmatched_keys, 10), collapse = ", "),
      if (base::length(unmatched_keys) > 10)
        base::paste0(" ... +", base::length(unmatched_keys) - 10, " more")
    )
  )
  
  # Return lookup unchanged — used inside the pipe ----
  base::invisible(lookup)
}


# Step 10: Left join all onto parcel-level master with audit ----


datasets_to_join <- list(
  list(data = parks_agg,             by = "neighbourhood_area", name = "parks_agg"),
  list(data = tree_agg,              by = "neighbourhood_area", name = "tree_agg"),
  list(data = substance_agg,         by = "neighbourhood_area", name = "substance_agg"),
  list(data = naloxone_agg,          by = "neighbourhood_area", name = "naloxone_agg"),
  list(data = wfps_agg,              by = "neighbourhood_area", name = "wfps_agg"),
  # list(data = requests_311_agg,    by = "neighbourhood_area", name = "requests_311_agg"),
  list(data = trade_permits_agg,     by = "neighbourhood_area", name = "trade_permits_agg"),
  list(data = building_permits_agg,  by = "neighbourhood_area", name = "building_permits_agg"),
  list(data = rooming_house_agg,     by = "neighbourhood_area", name = "rooming_house_agg"),
  list(data = higher_poverty_clean,  by = "neighbourhood_area", name = "higher_poverty_clean")
  # list(data = rentals_agg,         by = "electoral_ward",     name = "rentals_agg")
)


base::message("\n", base::strrep("═", 60))
base::message("Join Audit — master: ", base::nrow(master), " parcels")
base::message(base::strrep("═", 60))

master_merged <- purrr::reduce(
  datasets_to_join,
  \(acc, item) {
    audit_join(acc, item$data, item$by, item$name)
    dplyr::left_join(acc, item$data, by = item$by)
  },
  .init = master
)

base::message("\n", base::strrep("═", 60))
base::message("Final master_merged: ",
              base::nrow(master_merged), " rows × ",
              base::ncol(master_merged), " cols")
base::message(base::strrep("═", 60))
# ```
# 
# This produces a console report like:
#   ```
# ══════════════════════════════════════════════════════════════
# Join Audit — master: 50000 parcels
# ══════════════════════════════════════════════════════════════
# 
# ── parks_agg (joined on 'neighbourhood_area') ──────────────
# matched   : 48231 / 50000 rows
# unmatched : 1769 rows
# missing   : WAVERLEY WEST A, WAVERLEY WEST B, SAGE CREEK ...
# 
# ── substance_agg (joined on 'neighbourhood_area') ──────────
# matched   : 49105 / 50000 rows
# unmatched : 895 rows
# missing   : ST. JAMES INDUSTRIAL, POLO PARK ...
# ...
# 
# ══════════════════════════════════════════════════════════════
# Final master_merged: 50000 rows × 134 cols
# ══════════════════════════════════════════════════════════════






# Step 11: Audit join quality — % NA per joined column ----
master_merged |>
  dplyr::glimpse() |> 
  # dplyr::select(
  #   parks_count, substance_incidents_total, naloxone_incidents_total,
  #   requests_311_total, trade_permits_total, rooming_complaints_total,
  #   high_poverty_status
  # ) |>
  dplyr::summarise(dplyr::across(
    dplyr::everything(),
    \(x) base::round(base::mean(base::is.na(x)) * 100, 1)
  )) |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to  = "column",
                      values_to = "pct_na") |>
  dplyr::arrange(dplyr::desc(pct_na)) 
