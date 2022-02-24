# Copyright 2022 Province of British Columbia
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

library("bcdata")

# Join old and new ------------------------

## Stations

stations_summary <- read_rds("data/datasets/no2_results.rds") %>%
  select(-c(flag_yearly_incomplete, flag_two_of_three_years,flag_daily_incomplete)) %>%
  rename(station_name = site, latitude = lat, longitude = lon) %>%
  mutate(caaqs_ambient = as.character(caaqs_ambient),
         mgmt_level = as.character(mgmt_level))

stations_summary %>%
  arrange(caaqs_year) %>% 
  write_csv("out/databc/no2_site_summary.csv", na = "")


## Ambient CAAQs by airzone 

az_ambient <- read_rds("data/datasets/az_ambient.rds") %>%
  select(c(airzone, metric, n_years_ambient, metric_value_ambient, 
         caaqs_ambient, rep_stn_id_ambient)) %>%
  rename_with(.cols = ends_with("_ambient"), ~ str_remove(., "_ambient")) %>%
  mutate(caaqs = as.character(caaqs), 
         caaqs_year = .env$rep_year)

az_ambient %>%
  arrange(caaqs_year) %>% 
  write_csv("out/databc/no2_airzone_ambient_summary.csv", na = "")


# air management zone 

az_mgmt <- read_rds("data/datasets/az_mgmt.rds") %>%
  mutate(mgmt_level = as.character(mgmt_level))

az_mgmt %>%
  arrange(caaqs_year)%>% 
  write_csv("out/databc/no2_airzone_management_summary.csv", na = "")

