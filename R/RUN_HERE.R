# RUN_HERE.R
# Master run script — source all pipeline scripts in order ----
# Run this file to execute the full pipeline before knitting the RMD

# 0. Constants & helpers (no side effects — must load first) ----
source("R/00-helpers.R")
source("R/00-data-constants.R")

# 1. Import data from Winnipeg Open Data Portal ----
# source("R/01-data-import.R")

# Restore cached data (skips API fetch if already pulled) ----
base::load("data/data_raw_list.RData")





data_raw_list$assessment_parcels <- arrow::read_parquet(here::here("data/raw_assessment_parcels.parquet"))
data_raw_list$wfps_call_logs <- readr::read_csv(here::here("data/WFPS_Call_Logs_20260307.csv"))



onemonthback <- (lubridate::now() - days(30)) |> lubridate::format_ISO8601()
threeyearsback <- (lubridate::now() - days(365 * 3)) |> lubridate::format_ISO8601()

api_list <- list(
  tree_inventory              = "https://data.winnipeg.ca/resource/hfwk-jp4h.json?$group=neighbourhood&$select=neighbourhood,count(*)"
)

data_raw_list$aggregate_building_permits <- RSocrata::read.socrata("https://data.winnipeg.ca/resource/p5sy-gt7y.json")
data_raw_list$tree_inventory <- fetch_one_page(api_list$tree_inventory)


# data_raw_list$requests_311 <- readxl::read_excel(here::here("data/311_Requests_20260307.xlsx"))


# Cache raw data locally — avoids repeat API calls across sessions ----
base::save(data_raw_list, file = "data/data_raw_list_all.RData")


# 2. Run column diagnostics across all datasets ----
source("R/02-diagnostics.R")

# 3. Clean and process data for merging ----
source("R/03-data-process.R")
source("R/04-efa.R")

# 4. Knit the report (runs after all objects are in environment) ----
rmarkdown::render(
  input         = "output/01-data-import-notes.Rmd",
  output_file   = "01-data-import-notes.html",
  output_dir    = "output/",
  knit_root_dir = here::here(".")   # assumes RUN_HERE.R is run from project root
)
