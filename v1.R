# Load required libraries
library(tidyverse)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(viridis)
library(scales)

# Read the data
df <- read.csv("Global Dataset of Inflation.csv")

# Convert wide format to long format
year_columns <- as.character(1970:2022)
df_long <- df %>%
  pivot_longer(
    cols = all_of(year_columns),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(Year = as.numeric(Year))

# Define selected countries
selected_countries <- c("United Kingdom", "France", "Germany", "Denmark")

# 1. GDP Growth Line Plot
gdp_plot <- df_long %>%
  filter(
    Country %in% selected_countries,
    `Series Name` == "GDP growth (annual %)"
  ) %>%
  ggplot(aes(x = Year, y = Value, color = Country, group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.6) +
  labs(
    title = "GDP Growth Over Time",
    x = "Year",
    y = "GDP Growth (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set2")

# 2. Inflation Rate Line Plot
inflation_plot <- df_long %>%
  filter(
    Country %in% selected_countries,
    `Series Name` == "Inflation, consumer prices (annual %)"
  ) %>%
  ggplot(aes(x = Year, y = Value, color = Country, group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.6) +
  labs(
    title = "Inflation Rate Over Time",
    x = "Year",
    y = "Inflation Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set2")

# 3. Unemployment Rate Bar Chart
unemployment_plot <- df_long %>%
  filter(
    Country %in% selected_countries,
    `Series Name` == "Unemployment, total (% of total labor force)",
    Year >= 2015  # Recent years for better visibility
  ) %>%
  ggplot(aes(x = Year, y = Value, fill = Country)) +
  geom_col(position = "dodge") +
  labs(
    title = "Unemployment Rate by Country (Recent Years)",
    x = "Year",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set2")

# 4. Economic Indicator Heatmap
heatmap_data <- df_long %>%
  filter(
    Country %in% selected_countries,
    Year >= 2010,  # Focus on recent years
    `Series Name` %in% c(
      "GDP growth (annual %)",
      "Inflation, consumer prices (annual %)",
      "Unemployment, total (% of total labor force)"
    )
  ) %>%
  mutate(
    `Series Name` = case_when(
      `Series Name` == "GDP growth (annual %)" ~ "GDP Growth",
      `Series Name` == "Inflation, consumer prices (annual %)" ~ "Inflation",
      TRUE ~ "Unemployment"
    )
  )

heatmap_plot <- ggplot(heatmap_data, 
                      aes(x = Year, y = Country, fill = Value)) +
  geom_tile() +
  facet_wrap(~`Series Name`, ncol = 1) +
  scale_fill_viridis() +
  labs(
    title = "Economic Indicators Heatmap (2010-2022)",
    x = "Year",
    y = "Country",
    fill = "Value (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold")
  )

# Arrange all plots in a grid
grid.arrange(
  gdp_plot, inflation_plot,
  unemployment_plot, heatmap_plot,
  ncol = 2,
  top = textGrob(
    "Economic Indicators Dashboard\nUnited Kingdom, France, Germany, and Denmark",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)

# Save the dashboard
ggsave("european_economic_dashboard.png", 
       arrangeGrob(
         gdp_plot, inflation_plot,
         unemployment_plot, heatmap_plot,
         ncol = 2,
         top = textGrob(
           "Economic Indicators Dashboard\nUnited Kingdom, France, Germany, and Denmark",
           gp = gpar(fontsize = 16, fontface = "bold")
         )
       ),
       width = 15, height = 12, dpi = 300)
