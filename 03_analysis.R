# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

source("00_setup.R")

library("readr")
library("dplyr")
library("tidyr")
library("purrr")
library("lubridate")
library("assertr")

library("rcaaqs")

# Load Data ----------------------
no2_clean <- read_rds("data/datasets/no2_clean.rds")
stations_clean <- read_rds("data/datasets/stations_clean.rds")

# Calculate CAAQs --------------------------------------

# NO2 1-h (3-yr average)
no2_3yr_caaqs <- no2_3yr_caaqs(no2_clean, by = "site")
no2_3yr_mgmt <- caaqs_management(no2_3yr_caaqs)

# NO2 Annual (1-yr)
no2_1yr_caaqs <- no2_1yr_caaqs(no2_clean, by = "site")
no2_1yr_mgmt <- caaqs_management(no2_1yr_caaqs)

# Station results -----------------------------------------
# Filter by or add n_years
no2_3yr <- get_caaqs(no2_3yr_mgmt) %>%
  filter(n_years > 1)

no2_1yr <- get_caaqs(no2_1yr_mgmt) %>%
  mutate(n_years = 1)

#check for double entrues
# Combine and filter
no2_results <- bind_rows(no2_3yr, no2_1yr) %>%
  ungroup() %>%
  filter(caaqs_year == .env$rep_year) %>% 
  left_join(stations_clean, by = "site") %>%
  # Ensure only 1 analysis per site
  add_count(site, metric) %>%
  assert(in_set(1), n) %>%
  # Clean up
  select(airzone, site, region, lat, lon, everything(), -n)

# Airzone results ---------------------------------------------------------
# Get airzone results by metric
az_ambient <- no2_results %>%
  nest(data = c(-metric)) %>%
  mutate(data = map(data, ~airzone_metric(., keep = "site", station_id = "site"))) %>%
  unnest(data) %>%
  select(airzone, metric, everything())

az_mgmt <- az_ambient %>% 
  group_by(airzone) %>%   
  # Get which ever metric is worst (one per airzone)
  slice_max(mgmt_level, with_ties = FALSE) %>% 
  mutate(caaqs_year = .env$rep_year) %>% 
  ungroup() %>%
  select(caaqs_year, airzone, mgmt_level, metric, 
         metric_value_mgmt,
         rep_stn_id = rep_stn_id_mgmt, n_years = n_years_mgmt, 
         caaqs_ambient)


write_rds(no2_results, "data/datasets/no2_results.rds")
write_rds(az_ambient, "data/datasets/az_ambient.rds")
write_rds(az_mgmt, "data/datasets/az_mgmt.rds")
write_rds(no2_3yr_mgmt, "data/datasets/no2_3yr_mgmt.rds")
write_rds(no2_1yr_mgmt, "data/datasets/no2_1yr_mgmt.rds")

write_csv(no2_results, "out/no2_caaqs_combined_results.csv", na = "")
write_csv(az_ambient, "out/no2_airzone_results.csv" , na = "")
write_csv(az_mgmt, "out/no2_airzone_management_levels.csv", na = "")

