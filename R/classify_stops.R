flag_suspicious_stops <- function(data) {
  data %>%
    filter(grepl("Suspicion of criminal activity", STOP_REASON_NONTICKET)) %>%
    mutate(
      DATE = as.POSIXlt.character(DATETIME),
      MONTH = format(DATE, "%Y-%m")
    )
}

classify_outcome <- function(data) {
  data %>%
    mutate(
      TICKET_COUNT_NUM = suppressWarnings(as.numeric(TICKET_COUNT)),
      OUTCOME = case_when(
        ARREST_CHARGES != "NULL" ~ "Arrest",
        !is.na(TICKET_COUNT_NUM) & TICKET_COUNT_NUM > 0 ~ "Ticket",
        TRUE ~ "No Charge"
      ),
      OUTCOME = factor(OUTCOME, levels = c("Arrest", "Ticket", "No Charge"))
    )
}
