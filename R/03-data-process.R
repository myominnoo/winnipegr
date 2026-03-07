
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

# Cache raw data locally — avoids repeat API calls across sessions ----
base::save(data_raw_list, file = "data/data_raw_list.RData")

# Inspect column types, names, and sample values per dataset ----
glimpse_all(data_raw_list)



# Merge Strategy ----------------------------------------------------------

# Helper: uppercase + trim for consistent neighbourhood matching ----
clean_neighbourhood <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringr::str_squish()
}

# Step 1: Prepare master — normalise neighbourhood key ----
master <- data_raw_list$assessment_parcels |>
  dplyr::mutate(
    neighbourhood_area = clean_neighbourhood(neighbourhood_area)
  )

# Step 2: Aggregate parks by neighbourhood ----
parks_agg <- data_raw_list$parks_open_space |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood)) |>
  dplyr::group_by(neighbourhood_area, park_category) |>
  dplyr::summarise(
    parks_count            = dplyr::n(),
    parks_total_area_ha    = base::sum(base::as.numeric(total_area_in_hectares), na.rm = TRUE),
    parks_land_area_ha     = base::sum(base::as.numeric(land_area_in_hectares),  na.rm = TRUE),
    parks_water_area_ha    = base::sum(base::as.numeric(water_area_in_hectares), na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Aggregate substance use by neighbourhood ----
substance_agg <- data_raw_list$substance_use |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood)) |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    substance_incidents_total = dplyr::n(),
    substance_alcohol         = base::sum(substance == "Alcohol",  na.rm = TRUE),
    substance_drug            = base::sum(substance != "Alcohol",  na.rm = TRUE),
    .groups = "drop"
  )

# Step 4: Aggregate naloxone by neighbourhood (drop NA neighbourhoods) ----
naloxone_agg <- data_raw_list$naloxone_administrations |>
  dplyr::filter(!is.na(neighbourhood)) |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood)) |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    naloxone_incidents_total = dplyr::n(),
    naloxone_doses_total     = base::sum(
      base::as.numeric(naxolone_administrations), na.rm = TRUE
    ),
    .groups = "drop"
  )

# Step 5: Aggregate 311 requests (drop blank neighbourhood rows) ----
requests_311_agg <- data_raw_list$requests_311 |>
  dplyr::filter(neighbourhood != "") |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood)) |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    requests_311_total  = dplyr::n(),
    requests_311_open   = base::sum(case_status == "Open",   na.rm = TRUE),
    requests_311_closed = base::sum(case_status == "Closed", na.rm = TRUE),
    .groups = "drop"
  )

# Step 6: Aggregate trade permits by neighbourhood ----
trade_permits_agg <- data_raw_list$trade_permits |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood_name)) |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    trade_permits_total    = dplyr::n(),
    trade_permits_closed   = base::sum(status == "Closed", na.rm = TRUE),
    trade_permits_issued   = base::sum(status == "Issued", na.rm = TRUE),
    .groups = "drop"
  )

# Step 7: Aggregate rooming house enforcement by neighbourhood ----
rooming_house_agg <- data_raw_list$rooming_house_enforcement |>
  dplyr::mutate(neighbourhood_area = clean_neighbourhood(neighbourhood)) |>
  dplyr::group_by(neighbourhood_area) |>
  dplyr::summarise(
    rooming_complaints_total  = base::sum(base::as.numeric(complaint_driven),      na.rm = TRUE),
    rooming_proactive_total   = base::sum(base::as.numeric(proactive_enforcement),  na.rm = TRUE),
    rooming_completed_total   = base::sum(base::as.numeric(completed),              na.rm = TRUE),
    .groups = "drop"
  )

# Step 8: Clean higher poverty areas ----
# 1,144 rows likely = multiple census years — keep most recent per neighbourhood
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

# Step 9: Aggregate short term rentals — ward level only ----
rentals_agg <- data_raw_list$short_term_rentals |>
  dplyr::filter(!is.na(electoral_ward)) |>
  dplyr::group_by(electoral_ward) |>
  dplyr::summarise(
    rentals_total         = dplyr::n(),
    rentals_primary       = base::sum(primary_non_primary == "Primary",     na.rm = TRUE),
    rentals_non_primary   = base::sum(primary_non_primary == "Non Primary", na.rm = TRUE),
    .groups = "drop"
  )

# Step 10: Left join all onto parcel-level master ----
master_merged <- master |>
  dplyr::left_join(parks_agg,          by = "neighbourhood_area") |>
  dplyr::left_join(substance_agg,      by = "neighbourhood_area") |>
  dplyr::left_join(naloxone_agg,       by = "neighbourhood_area") |>
  dplyr::left_join(requests_311_agg,   by = "neighbourhood_area") |>
  dplyr::left_join(trade_permits_agg,  by = "neighbourhood_area") |>
  dplyr::left_join(rooming_house_agg,  by = "neighbourhood_area") |>
  dplyr::left_join(higher_poverty_clean, by = "neighbourhood_area") |>
  dplyr::left_join(rentals_agg,        by = "electoral_ward")   # ward-level only

# Step 11: Audit join quality — % NA per joined column ----
master_merged |>
  dplyr::select(
    parks_count, substance_incidents_total, naloxone_incidents_total,
    requests_311_total, trade_permits_total, rooming_complaints_total,
    high_poverty_status, rentals_total
  ) |>
  dplyr::summarise(dplyr::across(
    dplyr::everything(),
    \(x) base::round(base::mean(base::is.na(x)) * 100, 1),
  )) |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to  = "column",
                      values_to = "pct_na") |>
  dplyr::arrange(dplyr::desc(pct_na))   # highest NA% first — flag problem joins