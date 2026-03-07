source("R/00-helpers.R")

# ── API endpoint list ────────────────────────────────────────────────────────── -----

onemonthback <- (now() - days(30)) |> lubridate::format_ISO8601()
threeyearsback <- (now() - days(365 * 3)) |> lubridate::format_ISO8601()

api_list <- list(
  wfps_call_logs              = paste0("https://data.winnipeg.ca/resource/yg42-q284.json?$where=call_time>'", onemonthback, "'&$group=neighbourhood,incident_type&$select=neighbourhood,incident_type,count(*)"),
  aggregate_building_permits  = paste0("https://data.winnipeg.ca/resource/p5sy-gt7y.json?$where=year>=", year(threeyearsback), "&$group=neighbourhood,permit_group&$select=neighbourhood,permit_group,sum(total_declared_construction_value),sum(total_permits),sum(major_projects_count)"),
  tree_inventory              = "https://data.winnipeg.ca/resource/hfwk-jp4h.json?$group=neighbourhood&$select=neighbourhood,count(*)",
  # mosquito_trap               = "https://data.winnipeg.ca/resource/du7c-8488.json",
  request311                 = paste0("https://data.winnipeg.ca/resource/u7f6-5326.json?$where=open_date>'", onemonthback, "'&$group=neighbourhood,type&$select=neighbourhood,type,count(*)")
)

wfps_call_logs <- fetch_one_page(api_list$wfps_call_logs)
aggregate_building_permits <- fetch_one_page(api_list$aggregate_building_permits)
tree_inventory <- fetch_one_page(api_list$tree_inventory)
request311 = fetch_one_page(api_list$request311)

# Takes ~3 minutes to run
# raw_assessment_parcels <- fetch_all_pages("https://data.winnipeg.ca/resource/d4mq-wa44.json", 1000L)
# write_parquet(raw_assessment_parcels |> select(-geometry), "data/raw_assessment_parcels.parquet")
