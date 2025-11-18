# R Script for SLR Data Analysis (Interoperability & Compatibility Metrics)
# Script Name: analyze_SLR_interop.R
# Date: 2025-08-10

# --- Preparation: Install and Load Libraries ---
# Ensure you have the necessary packages installed
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# --- Step 1: Load the Data ---
# Note: This script assumes you have saved the CSV data I provided
# into a file named 'SLR_data.csv' in the same directory.
file_path <- "SLR_venue_temporal_data.csv"
slr_data <- read_csv(file_path)

# Handle potential missing years by filtering them out for the analysis
slr_data <- slr_data %>% filter(!is.na(Year))

print("Data loaded successfully.")

# --- Step 2: Analysis #1 - LOESS Trendline ---

# Define the full range of years present in your data
min_year <- min(slr_data$Year, na.rm = TRUE)
max_year <- max(slr_data$Year, na.rm = TRUE)
all_years <- min_year:max_year

# Count number of publications per year and RQ
yearly_counts <- slr_data %>%
  count(Year, primary_rq)

# Create a complete grid to ensure all year/RQ combinations are present
complete_data <- expand_grid(
  Year = all_years,
  primary_rq = unique(yearly_counts$primary_rq)
) %>%
  left_join(yearly_counts, by = c("Year", "primary_rq")) %>%
  mutate(n = replace_na(n, 0)) %>%
  arrange(primary_rq, Year)

# Plot the LOESS smoothed trendline
trend_plot_loess <- ggplot(complete_data, aes(x = Year, y = n, color = primary_rq)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.75, linewidth = 1.2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Smoothed Trend of Publications by Research Question",
    subtitle = paste0("Smoothed trend using LOESS interpolation (", min_year, "–", max_year, ")"),
    x = "Publication Year",
    y = "Estimated Publications per Year (LOESS)",
    color = "Research Question"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_x_continuous(breaks = seq(min_year, max_year, by = 5))

# Save the plot
ggsave("figure_temporal_trend.pdf", plot = trend_plot_loess, width = 10, height = 6)
print("Temporal trend plot saved as 'figure_temporal_trend.pdf'")

# --- Step 3: Bar Charts for Venue Distribution ---

venue_colors <- c(
  "Journal" = "#D55E00",                 # Orange foncé
  "Conference & Workshop" = "#0072B2",  # Bleu
  "Thesis" = "#999999",                 # Gris
  "Preprint & Report" = "#009E73",      # Vert
  "Book" = "#F0E442"                   # Jaune/Or
)

# --- RQ1 ---
bar_chart_rq1 <- slr_data %>%
  filter(primary_rq == "RQ1") %>%
  count(venue_type, name = "count") %>%
  mutate(venue_type = reorder(venue_type, count)) %>%
  ggplot(aes(x = venue_type, y = count, fill = venue_type)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = count), hjust = -0.3) +
  labs(
    title = "Venue Type Distribution for RQ1",
    x = "Venue Type",
    y = "Number of Primary Studies"
  ) +
  scale_fill_manual(values = venue_colors) +
  theme_minimal(base_size = 14)

# Save the plot for RQ1
ggsave("figure_venues_rq1.pdf", plot = bar_chart_rq1, width = 8, height = 6)
print("RQ1 venue distribution bar chart saved as 'figure_venues_rq1.pdf'")


# --- RQ2 ---
bar_chart_rq2 <- slr_data %>%
  filter(primary_rq == "RQ2") %>%
  count(venue_type, name = "count") %>%
  mutate(venue_type = reorder(venue_type, count)) %>%
  ggplot(aes(x = venue_type, y = count, fill = venue_type)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = count), hjust = -0.3) +
  labs(
    title = "Venue Type Distribution for RQ2",
    x = "Venue Type",
    y = "Number of Primary Studies"
  ) +
  scale_fill_manual(values = venue_colors) +
  theme_minimal(base_size = 14)

# Save the plot for RQ2
ggsave("figure_venues_rq2.pdf", plot = bar_chart_rq2, width = 8, height = 6)
print("RQ2 venue distribution bar chart saved as 'figure_venues_rq2.pdf'")