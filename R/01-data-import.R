# Fetch all datasets from Winnipeg Open Data portal ----
data_raw_list <- fetch_all_socrata(api_list, api_limits)

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