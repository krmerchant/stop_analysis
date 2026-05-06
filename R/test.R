library(tidyverse)
library(ggplot2)
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)   # loads %>% automatically

data <- read.csv("../data/Stop_Data.csv");

filtered <- data |> select(TRAFFIC_ARREST) |> filter(!is.na(TRAFFIC_ARREST)) |>summarise(

