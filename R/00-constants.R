
# ── API endpoint list ────────────────────────────────────────────────────────── -----

api_list <- list(
  assessment_parcels          = "https://data.winnipeg.ca/api/odata/v4/d4mq-wa44",
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
  assessment_parcels        = 50000,
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