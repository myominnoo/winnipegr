
# ── API endpoint list ────────────────────────────────────────────────────────── -----

api_list <- list(
  assessment_parcels          = "https://data.winnipeg.ca/api/odata/v4/tx3d-pfxq",
  parks_open_space            = "https://data.winnipeg.ca/resource/tx3d-pfxq.json",
  substance_use               = "https://data.winnipeg.ca/resource/6x82-bz5y.json",
  naloxone_administrations    = "https://data.winnipeg.ca/resource/qd6b-q49i.json",
  transit_passups             = "https://data.winnipeg.ca/resource/mer2-irmb.json",  # no neighbourhood — join manually
  census_households           = "https://data.winnipeg.ca/resource/nmk5-uwfw.json",
  requests_311                = "https://data.winnipeg.ca/api/odata/v4/u7f6-5326",
  higher_poverty_areas        = "https://data.winnipeg.ca/api/odata/v4/gg4w-peq6",
  trade_permits               = "https://data.winnipeg.ca/resource/urbd-qygv.json",
  rooming_house_enforcement   = "https://data.winnipeg.ca/resource/vk2f-xwp7.json",
  short_term_rentals          = "https://data.winnipeg.ca/resource/74hr-f8ai.json"   # no neighbourhood — join manually
)


# Per-dataset row limits (NULL = fetch all rows via auto-pagination) ----
api_limits <- list(
  assessment_parcels        = NULL,
  parks_open_space          = NULL,
  substance_use             = NULL,
  naloxone_administrations  = NULL,
  transit_passups           = 50000,
  census_households         = NULL,
  requests_311              = 50000,
  higher_poverty_areas      = NULL,
  trade_permits             = 50000,
  rooming_house_enforcement = NULL,
  short_term_rentals        = 5000
)

# Module 1: Build URL with optional limit ----
build_url <- function(url, limit = NULL) {
  if (!base::is.null(limit)) {
    base::paste0(url, "?$limit=", limit)
  } else {
    url  # no limit → RSocrata auto-paginates all rows
  }
}

# Module 2: Fetch a single dataset with error handling ----
fetch_socrata <- function(url, limit = NULL) {
  final_url <- build_url(url, limit)
  base::message("Fetching: ", final_url)
  tryCatch(
    expr    = RSocrata::read.socrata(final_url),
    error   = \(e) {
      base::message("ERROR fetching ", final_url, "\n  → ", base::conditionMessage(e))
      NULL
    },
    warning = \(w) {
      base::message("WARNING fetching ", final_url, "\n  → ", base::conditionMessage(w))
      RSocrata::read.socrata(final_url)  # retry on warning
    }
  )
}

# Module 3: Report failed datasets after loading ----
report_failures <- function(data_list) {
  failed <- base::names(purrr::keep(data_list, base::is.null))
  if (base::length(failed) > 0) {
    base::message("\nFailed to load: ", base::paste(failed, collapse = ", "))
  } else {
    base::message("\nAll datasets loaded successfully.")
  }
  base::invisible(failed)
}

# Module 4: Fetch all datasets with per-dataset limits ----
fetch_all_socrata <- function(api_list, api_limits = NULL) {
  
  # if no limits provided, fetch all rows for every dataset
  if (base::is.null(api_limits)) {
    data_list <- purrr::map(api_list, fetch_socrata)
    
    # if limits provided, apply per-dataset limits via map2  
  } else {
    data_list <- purrr::map2(
      api_list,
      api_limits,
      \(url, lim) fetch_socrata(url, limit = lim)
    )
  }
  
  report_failures(data_list)
  data_list
}



# Run ----
data_raw_list <- fetch_all_socrata(api_list, api_limits)


# Check structure ----
purrr::map(data_raw_list, base::dim)    # rows × cols per dataset
purrr::map(data_raw_list, base::names)  # column names

# Scan for likely join key columns ----
purrr::map(data_raw_list, \(df) base::grep(
  pattern     = "neighbourhood|neigh|id|ward",
  x           = base::names(df),
  value       = TRUE,
  ignore.case = TRUE
))