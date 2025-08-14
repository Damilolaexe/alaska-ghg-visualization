
---
  
  ## 📄 `1_project_visualization_exploration/exploration_visualization.R`
  ```r
# =============================================
# Project 1: Data Exploration & Visualization
# Chapter 1 Exploratory Methods
# =============================================

# Load packages
library(tidyverse)
library(scales)

# -------------------------------
# 1. Load Data
# -------------------------------
data_path <- "data/alaska_ghg_master.csv" # <-- change if needed
ghg_data <- read_csv(data_path)

# Expected columns:
# year, sector, gas, emissions_MtCO2e
# Example: 1990, Transportation, CO2, 5.23

# -------------------------------
# 2. Summary Statistics
# -------------------------------

# Mean emissions by sector (full period)
mean_by_sector <- ghg_data %>%
  group_by(sector) %>%
  summarise(mean_emissions = mean(emissions_MtCO2e, na.rm = TRUE)) %>%
  arrange(desc(mean_emissions))

write_csv(mean_by_sector, "output/mean_by_sector.csv")

# Total emissions by year
total_by_year <- ghg_data %>%
  group_by(year) %>%
  summarise(total_emissions = sum(emissions_MtCO2e, na.rm = TRUE))

write_csv(total_by_year, "output/total_by_year.csv")

# Mean emissions by gas
mean_by_gas <- ghg_data %>%
  group_by(gas) %>%
  summarise(mean_emissions = mean(emissions_MtCO2e, na.rm = TRUE)) %>%
  arrange(desc(mean_emissions))

write_csv(mean_by_gas, "output/mean_by_gas.csv")

# -------------------------------
# 3. Visualizations
# -------------------------------

# Line plot: total emissions over time
ggplot(total_by_year, aes(x = year, y = total_emissions)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(color = "darkblue") +
  scale_y_continuous(labels = comma) +
  labs(title = "Total GHG Emissions in Alaska (1990–2022)",
       x = "Year",
       y = "Emissions (MtCO₂e)") +
  theme_minimal()
ggsave("output/plot_total_emissions.png", width = 8, height = 5)

# Stacked area chart: emissions by sector
sector_trend <- ghg_data %>%
  group_by(year, sector) %>%
  summarise(total = sum(emissions_MtCO2e, na.rm = TRUE), .groups = "drop")

ggplot(sector_trend, aes(x = year, y = total, fill = sector)) +
  geom_area(alpha = 0.85) +
  scale_y_continuous(labels = comma) +
  labs(title = "Emissions by Sector in Alaska (1990–2022)",
       x = "Year",
       y = "Emissions (MtCO₂e)",
       fill = "Sector") +
  theme_minimal()
ggsave("output/plot_sector_stacked.png", width = 9, height = 6)

# Line plot: gas-specific trends
gas_trend <- ghg_data %>%
  group_by(year, gas) %>%
  summarise(total = sum(emissions_MtCO2e, na.rm = TRUE), .groups = "drop")

ggplot(gas_trend, aes(x = year, y = total, color = gas)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_y_continuous(labels = comma) +
  labs(title = "Emissions by Gas in Alaska (1990–2022)",
       x = "Year",
       y = "Emissions (MtCO₂e)",
       color = "Gas") +
  theme_minimal()
ggsave("output/plot_gas_trends.png", width = 8, height = 5)

# -------------------------------
# 4. Query Examples
# -------------------------------

# Extract transportation emissions for 2000–2020
transport_subset <- ghg_data %>%
  filter(sector == "Transportation", year >= 2000, year <= 2020)

write_csv(transport_subset, "output/transport_2000_2020.csv")

# Extract emissions for selected years (1990, 2000, 2010, 2020)
selected_years <- ghg_data %>%
  filter(year %in% c(1990, 2000, 2010, 2020)) %>%
  arrange(sector, year)

write_csv(selected_years, "output/emissions_selected_years.csv")

# -------------------------------
# End of Script
# -------------------------------
