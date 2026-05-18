source("R/libraries.R")
source("R/load_stop_data.R")

data <- load_stop_data()

ped <- data |>
  filter(PEDESTRIAN == 0) |>
  mutate(DATE = as.POSIXlt.character(DATETIME), YEAR = format(DATE, "%Y"))

p_race_given_year <- ped |>
  group_by(YEAR, ETHNICITY) |>
  summarise(count = n()) |>
  mutate(prop = count / sum(count))

p_r_y_plot <- p_race_given_year |> ggplot(aes(x = YEAR, y = count, fill = ETHNICITY)) +
  geom_col(position = "dodge") +
  labs(title = "P(ETHNICITY | YEAR ={2023,2024,2025}, STOPPED, PEDESTRIAN=1)")
# ggsave( "plots/PEDESTRIAN_stop_per_year.png", plot = p_r_y_plot);
p_r_y_plot

p_race_given_year_with_arrests <- ped |>
  filter(TRAFFIC_ARREST == "1") |>
  group_by(YEAR, ETHNICITY) |>
  summarise(count = n()) |>
  mutate(prop = count / sum(count))

p_r_y_arrests_plot <- p_race_given_year_with_arrests |>
  ggplot(aes(x = YEAR, y = prop, fill = ETHNICITY)) +
  geom_col(position = "dodge") +
  labs(title = "P(ETHNICITY | YEAR ={2023,2024,2025}, ARREST_CHARGES!=NULL, PEDESTRIAN=1)")
# ggsave( "plots/PEDESTRIAN_stop_per_year.png", plot = p_r_y_arrests_plot);
p_r_y_arrests_plot



ped |>
  group_by(ETHNICITY) |>
  summarise(
    counts = n(),
    arrests = sum(TRAFFIC_ARREST == 1)
  ) |>
  mutate(prop = arrests / counts) |>
  ggplot(aes(x = ETHNICITY, y = prop, fill = ETHNICITY)) +
  theme_minimal() +
  geom_col() +
  labs(title = "P(ARREST | ETHNICITY)")
ggsave("plots/TrafficArrests_RACE.png", plot = p_r_y_arrests_plot)
