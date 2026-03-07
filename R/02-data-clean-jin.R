source("R/01-data-import-jin.R")

library(tidyverse)

wfps_call_logs <- wfps_call_logs |> 
  pivot_wider(names_from=incident_type, values_from=count)

aggregate_building_permits <- aggregate_building_permits |> 
  pivot_wider(names_from=permit_group, values_from =matches("sum"))

request311 <- request311 |> 
  pivot_wider(names_from = type, values_from=count)
