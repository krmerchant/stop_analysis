library(tidyverse)
library(ggplot2)
library(dplyr) # loads %>% automatically
library(maps)
library(sf)
source("R/clean_stop_data.R")

# data cleaning
data <- clean_stop_data()


filtered <- data %>%
  filter(STOP_REASON_NONTICKET != "NULL") %>%
  filter(grepl("Sus", STOP_REASON_NONTICKET))
ggplot(filtered, aes(x = ETHNICITY, y = after_stat(prop), group = 1)) +
  geom_bar()

black_stops_by_district <- data %>%
  filter(!is.na(STOP_DISTRICT) & STOP_DISTRICT != "" & ETHNICITY != "Black") %>%
  group_by(STOP_DISTRICT) %>%
  summarize(black_count = n()) %>%
  mutate(black_prop = black_count / sum(black_count))

district_stop_counts <- data %>%
  filter(!is.na(STOP_DISTRICT) & STOP_DISTRICT != "") %>%
  group_by(STOP_DISTRICT) %>%
  summarize(cts = n()) %>%
  mutate(prop = cts / sum(cts))

dc_police <- st_read("/home/komelmerchant/git/stop_analysis/data/police_districts.geojson")
dc_police <- dc_police |> left_join(district_stop_counts, by = c("POLICEDISTRICT" = "STOP_DISTRICT"))
dc_police <- dc_police |> left_join(black_stops_by_district, by = c("POLICEDISTRICT" = "STOP_DISTRICT"))

dev.new()
ggplot(dc_police) +
  geom_sf(aes(fill = black_prop)) +
  geom_sf_text(aes(label = black_prop), size = 3, color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  theme_minimal() +
  labs(title = "Stop Counts by DC Police District: P( district  | stop, race=back)", fill = "Count")

dev.new()
ggplot(dc_police) +
  geom_sf(aes(fill = prop)) +
  geom_sf_text(aes(label = POLICEDISTRICT), size = 3, color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  theme_minimal() +
  labs(title = "Stop Counts by DC Police District: P( district | stopped)", fill = "Count")
ggsave("plots/stops_per_pdistrict.png")

dev.new()
dc_wards <- st_read("/home/komelmerchant/git/stop_analysis/data/Ward_-_2022.geojson")
ggplot(dc_wards) +
  geom_sf(aes(), color = "blue") +
  geom_sf_text(aes(label = WARD), size = 3, color = "black") +
  theme_minimal() +
  labs(title = "DC Wards")
ggsave("plots/wards.png")


data |>
  group_by(STOP_DISTRICT, ETHNICITY) |>
  summarise(count = n()) |>
  mutate(proportion = count / sum(count)) |>
  ggplot(aes(x = STOP_DISTRICT, y = ETHNICITY, fill = proportion)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")


data |>
  group_by(NOI_OFFICER_BUREAUS_TMA, ETHNICITY) |>
  summarise(count = n()) |>
  mutate(proportion = count / sum(count)) |>
  ggplot(aes(x = NOI_OFFICER_BUREAUS_TMA, y = ETHNICITY, fill = proportion)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")
