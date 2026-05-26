# Purpose: Play around with data from ACS
source("R/libraries.R")


# DP05_0001E = Total Population
# DP05_0038E = Black Population
# DP05_0037E = White Population


# question: what does the racial driversity look like among different tracks
# data <- read.csv("data/census_data/ACS_5-Year_Demographic_Characteristics_DC_Census_Tract.csv")
tract_geo <- st_read("data/census_data/ACS_5-Year_Demographic_Characteristics_of_DC_Census_Tracts.geojson")

tract_geo <- tract_geo %>% mutate(black_proportion = DP05_0038E / DP05_0001E, white_proportion = DP05_0037E / DP05_0001E)


dev.new()
ggplot(tract_geo) +
  geom_sf(aes(fill = white_proportion)) +
  scale_fill_gradient(low = "lightyellow", high = "darkred") +
  theme_minimal() +
  labs(title = "DC POP - White Proportion")



dev.new()
ggplot(tract_geo) +
  geom_sf(aes(fill = black_proportion)) +
  scale_fill_gradient(low = "lightyellow", high = "darkred") +
  theme_minimal() +
  labs(title = "DC POP - Black Proportion")
