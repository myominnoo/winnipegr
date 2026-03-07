# ── Time windows ──────────────────────────────────────────────────────────────
INDEX_START       <- as.Date("2021-01-01")
INDEX_END         <- Sys.Date()
INDEX_CENSUS_YEAR <- "2021"

# Tax-assessment lag year (2025 full calendar year)
lag_year_start <- "2025-01-01T00:00:00"
lag_year_end   <- "2025-12-31T23:59:59"

threeyearsback <- (lubridate::now() - lubridate::days(365 * 3)) |>
  lubridate::format_ISO8601()

# ── API endpoint list ─────────────────────────────────────────────────────────
# requests_311 and wfps_call_logs: $where only — fetch_all_pages appends
# $limit / $offset / $order itself, so no $group or $select here.
api_list <- list(
  # assessment_parcels          = "https://data.winnipeg.ca/api/odata/v4/d4mq-wa44",
  parks_open_space            = "https://data.winnipeg.ca/resource/tx3d-pfxq.json",
  substance_use               = "https://data.winnipeg.ca/resource/6x82-bz5y.json",
  naloxone_administrations    = "https://data.winnipeg.ca/resource/qd6b-q49i.json",
  transit_passups             = "https://data.winnipeg.ca/resource/mer2-irmb.json",  # no neighbourhood — join manually
  census_households           = "https://data.winnipeg.ca/resource/nmk5-uwfw.json",
  # requests_311                = paste0(                                               # httr2 paginated
  #   "https://data.winnipeg.ca/resource/u7f6-5326.json",
  #   "?$where=open_date>='", lag_year_start, "'",
  #   " AND open_date<='",    lag_year_end,   "'"
  # ),
  higher_poverty_areas        = "https://data.winnipeg.ca/api/odata/v4/gg4w-peq6",
  trade_permits               = "https://data.winnipeg.ca/resource/urbd-qygv.json",
  rooming_house_enforcement   = "https://data.winnipeg.ca/resource/vk2f-xwp7.json",
  short_term_rentals          = "https://data.winnipeg.ca/resource/74hr-f8ai.json"  # no neighbourhood — join manually
  # wfps_call_logs              = paste0(                                               # httr2 paginated
  #   "https://data.winnipeg.ca/resource/yg42-q284.json",
  #   "?$where=call_time>='", lag_year_start, "'",
  #   " AND call_time<='",    lag_year_end,   "'"
  # )
)

# ── Per-dataset row limits ────────────────────────────────────────────────────
# NULL = fetch all via RSocrata auto-pagination.
# httr2_keys datasets (requests_311, wfps_call_logs) ignore this — fetch_all_pages
# controls page size internally (default 1000L).
api_limits <- list(
  # assessment_parcels          = NULL,
  parks_open_space            = NULL,
  substance_use               = NULL,
  naloxone_administrations    = NULL,
  transit_passups             = NULL,
  census_households           = NULL,
  requests_311                = NULL,   # ignored — routed to fetch_all_pages
  higher_poverty_areas        = NULL,
  trade_permits               = NULL,
  rooming_house_enforcement   = NULL,
  short_term_rentals          = NULL,
  wfps_call_logs              = NULL    # ignored — routed to fetch_all_pages
)