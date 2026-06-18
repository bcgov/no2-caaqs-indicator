# Copyright 2026 Province of British Columbia
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


# Load Data ----------------------
no2_3yr_mgmt <- read_rds("data/datasets/rep_year_2024/no2_3yr_mgmt.rds")
no2_1yr_mgmt <- read_rds("data/datasets/rep_year_2024/no2_1yr_mgmt.rds")
stations_clean <- read_rds("data/datasets/rep_year_2024/stations_clean.rds")
print_summary <- read_rds("data/datasets/rep_year_2024/print_summary.rds")

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


# Compare 2021-2023 and 2022-2024 results for consistency
year_prev <- 2023
year_curr <- 2024

yrs_present <- sort(unique(no2_results$caaqs_year))
summarise_year <- function(df, y) {
  az_achieved <- df %>%
    filter(caaqs_year == y) %>%
    nest(data = c(-metric)) %>%
    mutate(data = map(data, ~airzone_metric(., keep = "station_name", station_id = "station_name"))) %>%
    unnest(data) %>%
    select(airzone, metric, everything()) %>%
    group_by(metric) %>%
    summarise(az_achieved = sum(caaqs_ambient == "Achieved", na.rm = TRUE))
  df %>%
    filter(caaqs_year == y) %>%
    group_by(metric) %>%
    filter(!is.na(metric_value_ambient)) %>%
    summarise(
      year = y,
      n_stations = n(),
      n_achieved = sum(caaqs_ambient == "Achieved", na.rm = TRUE),
      pct_achieved = round(100 * n_achieved / n_stations),
      .groups = "drop"
      ) %>%
      left_join(az_achieved, by = "metric")
  }
  
summary_prev <- summarise_year(no2_results, year_prev)
summary_curr <- summarise_year(no2_results, year_curr)
  
# Combine for side-by-side comparison
summary_compare <- full_join(
  summary_prev, summary_curr,
  by = "metric",
  suffix = c("_prev", "_curr")
) %>%
  mutate(
    delta_n = n_stations_curr - n_stations_prev,
    delta_achieved = n_achieved_curr - n_achieved_prev,
    delta_pct = pct_achieved_curr - pct_achieved_prev
)
  
print(summary_compare)
  
# diagnose which stations appear/disappear by metric
coverage_by_metric <- function(df, y) {
  df %>%
    filter(caaqs_year == y, !is.na(metric_value_ambient)) %>%
    distinct(metric, station_name)
}
  
cov_prev <- coverage_by_metric(no2_results, year_prev)
cov_curr <- coverage_by_metric(no2_results, year_curr)
  
added_stations <- anti_join(cov_curr, cov_prev, by = c("metric", "station_name")) %>%
  arrange(metric, station_name)
dropped_stations <- anti_join(cov_prev, cov_curr, by = c("metric", "station_name")) %>%
  arrange(metric, station_name)
  
print(added_stations)

print(dropped_stations)

# Station-level comparisons for stations present in BOTH years (by metric)
station_compare <- no2_results %>%
  filter(caaqs_year %in% c(year_prev, year_curr)) %>%
  filter(!is.na(metric_value_ambient)) %>%
  select(caaqs_year, metric, station_name,
         metric_value_ambient, caaqs_ambient,
         metric_value_mgmt, mgmt_level,
         min_year, max_year, n_years) %>%
  mutate(caaqs_ambient = as.character(caaqs_ambient),
         mgmt_level = as.character(mgmt_level)) %>%
  pivot_wider(
    names_from = caaqs_year,
    values_from = c(metric_value_ambient, caaqs_ambient,
                    metric_value_mgmt, mgmt_level,
                    min_year, max_year, n_years),
    names_sep = "_"
  ) %>%
  # keep only those with BOTH years
  filter(
    !is.na(.data[[paste0("metric_value_ambient_", year_prev)]]) &
    !is.na(.data[[paste0("metric_value_ambient_", year_curr)]])
    )
  
  
# Flag any changes in achievement status or management level
changes <- station_compare %>%
  mutate(
    achievement_changed =
      .data[[paste0("caaqs_ambient_", year_prev)]] != .data[[paste0("caaqs_ambient_", year_curr)]],
    mgmt_changed =
      .data[[paste0("mgmt_level_", year_prev)]] != .data[[paste0("mgmt_level_", year_curr)]]) %>%
  filter(achievement_changed | mgmt_changed) %>%
  print(n = Inf, width = Inf)
