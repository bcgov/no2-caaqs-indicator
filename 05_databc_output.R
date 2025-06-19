# Copyright 2025 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and 
# limitations under the License.


# DataBC 

# Create output files for the BC data catalogue, combining with previous years' data

source("00_setup.R")

library("readr")
library("dplyr")
library("stringr")
library("tidyr")
library("purrr")
library("lubridate")
library("assertr")

library("rcaaqs")

library("bcdata")

# Join old and new ------------------------
# Load Data ----------------------
no2_3yr_mgmt <- read_rds("data/datasets/no2_3yr_mgmt.rds")
no2_1yr_mgmt <- read_rds("data/datasets/no2_1yr_mgmt.rds")
stations_clean <- read_rds("data/datasets/stations_clean.rds")

# Station results -----------------------------------------
# Filter by or add n_years
no2_3yr <- get_caaqs(no2_3yr_mgmt) %>%
  filter(n_years > 1)

no2_1yr <- get_caaqs(no2_1yr_mgmt) %>%
  mutate(n_years = 1)


no2_results <- bind_rows(no2_3yr, no2_1yr) %>%
  ungroup() %>%
  left_join(stations_clean, by = "site") %>%
  # Ensure only 1 analysis per site
  add_count(site, caaqs_year, metric) %>%
  assert(in_set(1), n) %>%
  # Clean up
  select(caaqs_year, airzone, 
         station_name = site, 
         region, 
         latitude = lat, 
         longitude = lon, 
         metric,
         n_years,	
         min_year,	
         max_year,
         metric_value_ambient,
         caaqs_ambient,
         excluded,
         metric_value_mgmt,
         mgmt_level,
         everything(), -n, -flag_daily_incomplete, -flag_yearly_incomplete) %>% 
  arrange(caaqs_year, airzone)

write_csv(no2_results, "out/databc/no2_stations_summary.csv", na = "")

# Airzone results ---------------------------------------------------------
# Get airzone results by metric
az_ambient_year <- no2_results %>%
  nest(data = c(-metric, -caaqs_year)) %>%
  mutate(data = map(data, ~airzone_metric(., keep = "station_name", station_id = "station_name"))) %>%
  unnest(data) %>%
  select(airzone, metric, caaqs_year, everything())

az_mgmt_year <- az_ambient_year %>% 
  group_by(airzone, caaqs_year, metric) %>%   
  # Get which ever metric is worst (one per airzone)
  slice_max(mgmt_level, with_ties = FALSE) %>% 
  ungroup() %>%
  select(caaqs_year, airzone, 
         metric, n_years_ambient,
         metric_value_ambient,
         caaqs_ambient,
         rep_stn_name_ambient = rep_stn_id_ambient,
         rep_stn_id_ambient, 
         excluded,
         n_years_mgmt,
         metric_value_mgmt,
         mgmt_level,  
         rep_stn_name_mgmt = rep_stn_id_mgmt,
         rep_stn_id_mgmt
         ) %>%
  arrange(caaqs_year, airzone)

write_csv(az_mgmt_year, "out/databc/no2_airzones_summary.csv", na = "")

## Stations ---------------------

#stations_summary <- read_rds("data/datasets/no2_results.rds") %>%
#  select(-flag_yearly_incomplete, -flag_two_of_three_years, 
#         -flag_daily_incomplete, -region) %>%
#  rename(station_name = site, latitude = lat, longitude = lon) %>%
#  mutate(caaqs_ambient = as.character(caaqs_ambient),
#         mgmt_level = as.character(mgmt_level),
#         station_id = station_name)

# In future bind_rows() with older data...

#stations_summary %>%
#  select(caaqs_year, airzone, station_name, station_id, 
#         latitude, longitude, metric, n_years, min_year, max_year, 
#         metric_value_ambient, caaqs_ambient,
#         excluded, metric_value_mgmt, mgmt_level) %>%
#  arrange(caaqs_year, airzone, station_name) %>%
#  write_csv("out/databc/no2_stations_summary.csv", na = "")


## Airzones -------------------

#airzones_summary <- read_rds("data/datasets/az_ambient.rds") %>%
#  mutate(rep_stn_name_ambient = rep_stn_id_ambient,
#         rep_stn_name_mgmt = rep_stn_id_mgmt, 
#         caaqs_ambient = as.character(caaqs_ambient), 
#         mgmt_level = as.character(mgmt_level),
#         caaqs_year = .env$rep_year)

# In future bind_rows() with older data...

#airzones_summary %>%
#  select(caaqs_year, airzone, metric, 
#         n_years_ambient, metric_value_ambient, caaqs_ambient, 
#         rep_stn_name_ambient, rep_stn_id_ambient,
#         excluded, n_years_mgmt, metric_value_mgmt, mgmt_level, 
#         rep_stn_name_mgmt, rep_stn_id_mgmt) %>%
#  arrange(caaqs_year, airzone) %>% 
#  write_csv("out/databc/no2_airzones_summary.csv", na = "")
