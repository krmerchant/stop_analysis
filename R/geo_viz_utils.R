read_police_district_geo <- function() {
  dc_police <- st_read("/home/komelmerchant/git/stop_analysis/data/police_districts.geojson")

  return(dc_police)
}

read_ward_geo <- function() {
  dc_wards <- st_read("/home/komelmerchant/git/stop_analysis/data/Ward_-_2022.geojson")
  return(dc_wards)
}

read_census_tract_geo <- function() {
  dc_tracts <- st_read("/home/komelmerchant/git/stop_analysis/data/census_data/ACS_5-Year_Demographic_Characteristics_of_DC_Census_Tracts.geojson")
  return(dc_tracts)
}
