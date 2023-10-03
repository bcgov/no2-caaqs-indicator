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
library("ggplot2")
library("ggtext")
library("stringr")

library("sf")
library("bcmaps")

library("rcaaqs")
library("envreportutils")

# Load Data --------------------------------------------------
no2_results <- read_rds("data/datasets/no2_results.rds")
no2_3yr_mgmt <- read_rds("data/datasets/no2_3yr_mgmt.rds")
no2_1yr_mgmt <- read_rds("data/datasets/no2_1yr_mgmt.rds")

az_ambient <- read_rds("data/datasets/az_ambient.rds")
az_mgmt <- read_rds("data/datasets/az_mgmt.rds")

# Let's save plots for the print version
print_plots <- list()

# Spatial data summaries --------------------------------------------
az <- airzones() %>%
  st_make_valid() %>%
  st_transform(st_crs(bc_bound())) %>%
  st_intersection(st_geometry(bc_bound())) %>% 
  group_by(airzone = Airzone) %>% 
  summarize()

az_ambient_sf <- az_ambient %>% 
  complete(airzone = az$airzone, metric) %>% # Ensure ALL airzones
  left_join(az, ., by = "airzone") %>% 
  mutate(caaqs_ambient = replace_na(caaqs_ambient, levels(caaqs_ambient)[1]))

az_mgmt_sf <- az_mgmt %>%
  complete(airzone = az$airzone, metric, caaqs_year) %>% # Ensure ALL airzones
  left_join(az, ., by = "airzone") %>% 
  mutate(mgmt_level = replace_na(mgmt_level, levels(mgmt_level)[1]),
         caaqs_ambient = replace_na(caaqs_ambient, levels(caaqs_ambient)[1]))

stations_sf <- no2_results %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  transform_bc_albers()

# Numbers for print version 
print_summary <- stations_sf %>%
  group_by(metric) %>%
  summarise(n = n(), 
            n_achieved = sum(caaqs_ambient == "Achieved", na.rm = TRUE), 
            percent_achieved = round(n_achieved / n * 100))

# Spatial files for leaflet maps ---------------------------------------

v <- no2_results %>%
  select("site", "metric", "metric_value_mgmt") %>%
  mutate(name = paste0("metric_value_mgmt_", str_remove(metric, "no2_"))) %>%
  select(-metric) %>%
  pivot_wider(names_from = name, values_from = metric_value_mgmt)

# Airzones by Management CAAQS
# - Arranged by worst Management level, 
# - BUT includes Management values for both metrics for the Representative station
leaf_az_mgmt <- az_mgmt_sf %>%
  select(airzone, caaqs_ambient, 
         mgmt_level, rep_stn_id, n_years, geometry) %>%
  group_by(airzone) %>% 
  slice_max(mgmt_level, with_ties = FALSE) %>%
  left_join(v, by = c("rep_stn_id" = "site"))

# Stations by management CAAQS
# - Arranged by worst CAAQS management, 
# - BUT include Management values for both metrics 
leaf_stations_mgmt <- stations_sf %>%
  select(airzone, site, mgmt_level, n_years) %>%
  group_by(site) %>% 
  slice_max(mgmt_level, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(v, by = "site")


# Individual Station Plots ------------------------------------------------
# - For print version and leaflet maps
# - Using management data because contains differences between the non-tfee data
#   and tfee-adjusted data

sites <- no2_results %>%
  arrange(airzone, site, metric) %>%
  pull(site) %>%
  unique()

stn_plots <- list()

for(s in sites) {
  message("Creating plots for ", s)
  
  g1 <- plot_caaqs(no2_3yr_mgmt, id = s, id_col = "site", year_min = 2013)
  g2 <- plot_caaqs(no2_1yr_mgmt, id = s, id_col = "site", year_min = 2013)
  
  # Save for print version
  stn_plots[[s]][["3yr"]] <- g1
  stn_plots[[s]][["1yr"]] <- g2
  
  # Save svg for leaflet maps (save each that exists)
  if(!is.null(g1)) {
    ggsave(paste0("leaflet_map/station_plots/", s, "_3yr.svg"), g1, 
           width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  }
  if(!is.null(g2)) {
    ggsave(paste0("leaflet_map/station_plots/", s, "_1yr.svg"), g2, 
           width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  }
}

# Summary plot -------------------------------------------------------------
# - For print version

g <- summary_plot(no2_results, 
                  metric_val = "metric_value_ambient", 
                  airzone = "airzone", station = "site", 
                  parameter = "metric", pt_size = 2, 
                  az_labeller = label_wrap_gen(10)) + 
  theme(strip.text.y = element_text(angle = 0))
print_plots[["no2_ambient_summary_plot"]]<- g

# Maps -------------------------------------------------------------
# - For print version

g <- achievement_map(
  az_data = filter(az_ambient_sf, metric == "no2_3yr"),
  stn_data = filter(stations_sf, metric == "no2_3yr"),
  az_labs = "**Airzones:**<br>NO<sub>2</sub> 1-Hr Air Quality Standard",
  stn_labs = "**Monitoring Stations:**<br>NO<sub>2</sub> 1-Hr Metric (ppb)")
print_plots[["achievement_map_3yr"]] <- g


g <- achievement_map(
  az_data = filter(az_ambient_sf, metric == "no2_1yr"),
  stn_data = filter(stations_sf, metric == "no2_1yr"),
  az_labs = "**Airzones:**<br>NO<sub>2</sub> Annual Air Quality Standard",
  stn_labs = "**Monitoring Stations:**<br>NO<sub>2</sub> Annual Metric (ppb)")
print_plots[["achievement_map_1yr"]] <- g

# Management figures --------------------------------------------
# - For print version

## Air Zone Map --------------

colrs <- get_colours("management", drop_na = FALSE)

labels_df <-  data.frame(
  x = c(680000, 1150000, 780000, 1150000, 1550000, 1150000, 1500000),
  y = c(950000, 1550000, 1500000, 950000, 600000, 325000, 410000), 
  airzone_name = c("Coastal", "Northeast", "Northwest", 
                   "Central\nInterior", "Southern\nInterior", 
                   "Georgia Strait", "Lower Fraser Valley"))

g <- ggplot(az_mgmt_sf) +   
  geom_sf(aes(fill = mgmt_level), colour = "white") + 
  coord_sf(datum = NA) + 
  theme_minimal() + 
  scale_fill_manual(values = colrs, 
                    drop = FALSE, 
                    name = "Air Zone Management Levels", 
                    guide = guide_legend(reverse = TRUE)) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.25, 0.12), legend.direction = "vertical",
        plot.margin = unit(c(0,0,0,0),"mm")) +
  geom_text(data = labels_df, aes(x = x, y = y, label = airzone_name), 
            colour = "black", size = 6)

print_plots[["no2_mgmt_map"]] <- g

# SVG of airzone CAAQS mgmt level map
ggsave("out/no2_caaqs_mgmt_map.svg", plot = g, dpi = 72,
       width = 500, height = 450, units = "px", bg = "white")

## Bar Chart --------------

g <- ggplot(data = no2_results, aes(x = metric, fill = mgmt_level)) + 
  geom_bar(alpha = 1, width = 0.8) +
  facet_wrap(~ airzone, ncol = 1) +
  xlab("") + ylab("Number of Reporting Stations") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = colrs, 
                    drop = TRUE, 
                    name = "Air Zone Management Levels", 
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = c("no2_3yr" = "3-yr", "no2_1yr" = "1-yr")) +
  theme_soe_facet() +
  theme(panel.grid.major.y = (element_blank()),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.spacing = unit(5,"mm"),
        plot.margin = unit(c(10,0,1,0),"mm"),
        strip.text = element_text(size = 13))

print_plots[["no2_mgmt_chart"]] <- g

# SVG of airzone/station CAAQS mgmt achievement chart
ggsave("out/no2_caaqs_mgmt_chart.svg", dpi = 72,
       width = 500, height = 600, units = "px", bg = "white")

# Output data ------------------------------------------------

# For print version
write_rds(print_plots, "data/datasets/print_plots.rds")
write_rds(stn_plots, "data/datasets/print_stn_plots.rds")
write_rds(print_summary, "data/datasets/print_summary.rds")

# For leaflet maps
filter(leaf_stations_mgmt) %>%
  st_transform(4326) %>% 
  st_write("out/no2_stations_mgmt.geojson", delete_dsn = TRUE)

filter(leaf_az_mgmt) %>%
  st_transform(4326) %>% 
  st_write("out/no2_airzones_mgmt.geojson", delete_dsn = TRUE)

