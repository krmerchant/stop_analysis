read_police_district_geo <- function() {
  dc_police <- st_read("/home/komelmerchant/git/stop_analysis/data/police_districts.geojson")

  return(dc_police)
}

read_ward_geo <- function() {
  dc_wards <- st_read("/home/komelmerchant/git/stop_analysis/data/Ward_-_2022.geojson")
  return(dc_wards)
}
