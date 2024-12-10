# =============================================================
# SCRIPT NAME: FCC_Competition_Analysis.R
# AUTHOR: Mardoqueo Arteaga
# DATE: [redacted]
# =============================================================
# Additional Information:
# - Data Source: FCC Fixed Broadband Deployment Data, December 2019 (NY only)
# - Data File: Fixed_Broadband_Deployment_Data_December_2019_20240917.csv
# - Programming Language: R
# ============================================================

# ============================================================
# Pre-Work Set Up (Required Packages, Data, Path)
library(data.table)   # For efficient data handling
library(dplyr)        # For data manipulation
setwd("~/Downloads/FCC_Source")
data <- fread("Fixed_Broadband_Deployment_Data__December_2019_20240917.csv")
FCC <- data
# str(data)
# head(data)

# Renaming columns for simpler invcation 
# Rename all columns to simpler names
setnames(FCC, old = names(FCC), new = c(
  "logical_record_num", "provider_id", "frn", "provider_name", "dba_name", "holding_comp", "holding_comp_num",
  "holding_comp_final", "state", "block_code", "tech_code", "consumer", "max_ad_down", "max_ad_up", "business"
))
# ============================================================

# ============================================================
# Descriptive Statistics and Count Map
# Pre-Work for correct type and removing duplicates
FCC[, block_code := as.character(block_code)]   # Converting block_code to character type
FCC_unique <- unique(FCC, by = c("provider_id", "block_code"))    # Removing if multiple tech is in the same Census block (unique observations) 
providers_per_block <- FCC_unique[, .(provider_cnt = uniqueN(provider_id)), by = block_code]    # Calculate providers per Census block

# Descriptive statistics of providers per census block
mean_providers <- mean(providers_per_block$provider_cnt)
mean_providers_round <- round(mean_providers)   # Rounding since we can't have half a provider
median_providers <- median(providers_per_block$provider_cnt)

# Counting the number of Census blocks with either the mean or median number of providers
blocks_with_mean <- providers_per_block[provider_cnt == mean_providers_round, .N]
blocks_with_median <- providers_per_block[provider_cnt == median_providers, .N]

# Choropleth for NY State
# Additional Information:
# - Data Source: https://catalog.data.gov/dataset/tiger-line-shapefile-2022-state-new-york-ny-census-tract (NY is state FIPS 36)
# - Data File: tabblock2010_36_pophu.shp (Using the 2010 Census version since that’s the data specifications)

library(sf, ggplot)
ny_blocks <- st_read("tabblock2010_36_pophu.shp")
ny_blocks$block_code <- as.character(ny_blocks$BLOCKID10)
merged_data <- merge(ny_blocks, providers_per_block, by = "block_code")

# Plotting a choropleth map (there are better names for this but this makes it sound more fun)
#library(ggplot2)
ggplot(data = merged_data) +
  geom_sf(aes(fill = provider_cnt), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "sqrt",
    name = "Number of Providers"
  ) +
  labs(
    title = "Broadband Providers per Census Block in New York State",
    subtitle = "Data Source: FCC Fixed Broadband Deployment Data (December 2019)"
  ) +
  theme_void() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Adding in a histogram to see the distribution
ggplot(providers_per_block, aes(x = provider_cnt)) +
  geom_histogram(
    binwidth = 1,
    fill = "gray",
    color = "black",
    boundary = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(
      from = min(providers_per_block$provider_cnt),
      to = max(providers_per_block$provider_cnt),
      by = 5
    )
  ) +
  scale_y_continuous(
    labels = comma   
  ) +
  labs(
    title = "Distribution of Broadband Providers",
    x = "Number of Providers",
    y = "Number of Census Blocks"
  ) +
  theme_minimal() + 	# going to add a few changes to the plot to make it “publication worthy”
  theme(
    panel.grid.major = element_blank(),    
    panel.grid.minor = element_blank(),   
    axis.text = element_text(size = 12),  
    axis.title = element_text(size = 12),  
    plot.title = element_text(size = 12, face = "bold")
  )
# ============================================================

# ============================================================
# Speed Analysis
# Get maximum download/upload speed per block, merge with providers per block
speed_per_block <- FCC_unique[, .(
  max_downstream_speed = max(max_ad_down, na.rm = TRUE),
  max_upstream_speed = max(max_ad_up, na.rm = TRUE)
), by = block_code]
block_speed_providers <- merge(providers_per_block, speed_per_block, by = "block_code")

# Calculate correlation between number of providers and max downstream/upstream speeds (Did they calculate this using an Ookla test?)
correlation_downstream <- cor(block_speed_providers$provider_cnt, block_speed_providers$max_downstream_speed, use = "complete.obs")
correlation_upstream <- cor(block_speed_providers$provider_cnt, block_speed_providers$max_upstream_speed, use = "complete.obs")

# Scatter Plots for Downstream and Upstream Speeds vs Providers
ggplot(block_speed_providers, aes(x = provider_cnt, y = max_downstream_speed)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(
    title = "Relationship Between Providers and Max Download Speed",
    x = "Number of Providers per Census Block",
    y = "Maximum Advertised Download Speed (Mbps)"
  ) +
  theme_minimal()
ggplot(block_speed_providers, aes(x = provider_cnt, y = max_upstream_speed)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(
    title = "Relationship between Providers and Max Upload Speed",
    x = "Number of Providers per Census Block",
    y = "Maximum Advertised Upload Speed (Mbps)"
  ) +
  theme_minimal()

# Can add a regression model but it might be too naive. Could do a polynomial regression if we had more space.
# ============================================================

# ============================================================
# Upload/Download
nyblocks_speed <- merge(ny_blocks, speed_per_block, by = "block_code")
# Plotting a choropleth map for downstream
ggplot(data = nyblocks_speed) +
  geom_sf(aes(fill = max_downstream_speed), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "sqrt",
    name = "Downstream Speed"
  ) +
  labs(
    title = "Downstream Speeds per Census Block in New York State",
    subtitle = "Data Source: FCC Fixed Broadband Deployment Data (December 2019)"
  ) +
  theme_void() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
# Plotting a choropleth map for upstream
ggplot(data = nyblocks_speed) +
  geom_sf(aes(fill = max_upstream_speed), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "sqrt",
    name = "Upstream Speed"
  ) +
  labs(
    title = "Upstream Speeds per Census Block in New York State",
    subtitle = "Data Source: FCC Fixed Broadband Deployment Data (December 2019)"
  ) +
  theme_void() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
# ============================================================
