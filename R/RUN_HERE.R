# RUN_HERE.R
# Master run script — source all pipeline scripts in order ----
# Run this file to execute the full pipeline before knitting the RMD

# 0. Constants & helpers (no side effects — must load first) ----
source("R/00-constants.R")
source("R/00-helpers.R")

# 1. Import data from Winnipeg Open Data Portal ----
# source("R/01-data-import.R")

# Restore cached data (skips API fetch if already pulled) ----
base::load("data/data_raw_list.RData")


# 2. Run column diagnostics across all datasets ----
source("R/02-diagnostics.R")

# 3. Clean and process data for merging ----
# source("R/03-data-process.R")

# 4. Knit the report (runs after all objects are in environment) ----
rmarkdown::render(
  input         = "output/01-data-import-notes.Rmd",
  output_file   = "01-data-import-notes.html",
  output_dir    = "output/",
  knit_root_dir = here::here(".")   # assumes RUN_HERE.R is run from project root
)
