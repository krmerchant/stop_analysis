library(tigris)
library(tidycensus)
library(tigris)
library(mapview)
library(tidyr)
library(dplyr)
v22 <- load_variables(2024, "acs5", cache = TRUE)
var_select = "B16001_005"

dc_spanish = get_acs(geography="tract",variables="B06007_005",state="DC",geometry = TRUE)
md_spanish = get_acs(geography="tract",variables="B06007_005",state="MD",geometry = TRUE,
                     #county=c("Montgomery","Prince George's"))
                     county=c("Prince George's"))
va_spanish = get_acs(geography="tract",variables="B06007_005",state="VA",geometry = TRUE,county=c("Arlington","Alexandria"))
dc_spanish_18_64_not_well = get_acs(geography="tract",variables="B16004_029",state="DC",geometry = TRUE)
dc_spanish_18_64_none = get_acs(geography="tract",variables="B16004_030",state="DC",geometry = TRUE)
plot(dc_spanish["estimate"])

all_spanish  = rbind(dc_spanish,md_spanish,va_spanish)
dc_md_spanish = rbind(dc_spanish,md_spanish)


mapview(
  dc_spanish,
  zcol = "estimate", 
  layer.name = "# Spanish speakers<br/>who don't speak English well"
)

var_select = "B25003_003" 
var_vec = c("B25003_003", "B25003_002")
pg_renter = get_acs(geography="tract",variables=var_select,state="MD",geometry = TRUE,
                     #county=c("Montgomery","Prince George's"))
                     county=c("Prince George's"))

pg_rent_own= get_acs(geography="tract",variables=var_vec,state="MD",geometry = TRUE,
                    #county=c("Montgomery","Prince George's"))
                    county=c("Prince George's"))%>%
  mutate(variable = case_when(variable == "B25003_003" ~ "Number of Renters",
                              variable == "B25003_002" ~ "Number of Owners")) %>%
  select(-moe) %>%
  pivot_wider(names_from = "variable",values_from = "estimate") %>%
  mutate(percent_renters = `Number of Renters`/(`Number of Renters` + `Number of Owners`))
  

mapview(
  pg_renter,
  zcol = "estimate", 
  layer.name = "# renter occupied households"
)

dc_renter = get_acs(geography="tract",variables=var_select,state="DC",geometry = TRUE)

mapview(
  dc_renter,
  zcol = "estimate", 
  layer.name = "# renter occupied households"
)
