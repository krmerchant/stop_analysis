
load_stop_data <- function() {
  data <- read.csv("data/Stop_Data.csv");
  # clean up district name convention 
  # we don't really care about waterway stops ... only small percentage
  data <- data %>% filter(!is.na(STOP_DISTRICT) & STOP_DISTRICT  != "")   
  data <- data |> mutate(STOP_DISTRICT = ifelse(!grepl("D", STOP_DISTRICT), 
                                   paste0(STOP_DISTRICT, "D"), 
                                 STOP_DISTRICT))
  return(data)  
}
