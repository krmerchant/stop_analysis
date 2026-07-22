library(httr)
library(dplyr)
library(readr)

# Geocodes unique STOP_LOCATION_BLOCK strings to Census tract GEOIDs using the
# Census Bureau's free batch geocoder. Results are cached to `cache_path` and
# the function is resumable: blocks already present in the cache are skipped,
# and each batch is appended to disk as soon as it completes so a long run
# can be interrupted and picked back up. The public geocoder is unreliable
# under load (gateway timeouts, dropped connections), hence the small batch
# size and retry-with-backoff.
geocode_blocks_batch <- function(blocks, cache_path = "data/geocoded_blocks.csv",
                                  batch_size = 100, max_retries = 5, sleep_between = 3) {
  blocks <- unique(blocks)

  if (file.exists(cache_path)) {
    already <- read_csv(cache_path, show_col_types = FALSE, col_types = cols(.default = "c"))
    blocks <- setdiff(blocks, already$STOP_LOCATION_BLOCK)
  }

  if (length(blocks) == 0) {
    message("Nothing new to geocode; cache already covers all requested blocks.")
    return(invisible(read_csv(cache_path, show_col_types = FALSE)))
  }

  chunks <- split(blocks, ceiling(seq_along(blocks) / batch_size))
  message(sprintf("Geocoding %d blocks in %d batches of up to %d...", length(blocks), length(chunks), batch_size))

  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    tmp_in <- tempfile(fileext = ".csv")
    write.table(
      data.frame(id = seq_along(chunk), street = chunk, city = "", state = "", zip = ""),
      tmp_in,
      sep = ",", row.names = FALSE, col.names = FALSE, quote = TRUE, na = ""
    )

    parsed <- NULL
    for (attempt in seq_len(max_retries)) {
      resp <- tryCatch(
        httr::POST(
          "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch",
          body = list(
            addressFile = httr::upload_file(tmp_in, type = "text/csv"),
            benchmark = "Public_AR_Current",
            vintage = "Current_Current"
          ),
          httr::timeout(180)
        ),
        error = function(e) NULL
      )

      if (!is.null(resp) && httr::status_code(resp) == 200) {
        txt <- httr::content(resp, as = "text", encoding = "UTF-8")
        candidate <- tryCatch(
          read.csv(text = txt, header = FALSE, fill = TRUE, quote = "\"", colClasses = "character"),
          error = function(e) NULL
        )
        if (!is.null(candidate) && nrow(candidate) == length(chunk)) {
          parsed <- candidate
          break
        }
      }
      message(sprintf("  batch %d/%d attempt %d failed, retrying...", i, length(chunks), attempt))
      Sys.sleep(sleep_between * attempt)
    }

    if (is.null(parsed)) {
      warning(sprintf("Batch %d/%d failed after %d attempts; marking %d blocks as Error", i, length(chunks), max_retries, length(chunk)))
      parsed <- data.frame(
        V1 = seq_along(chunk), V2 = chunk, V3 = "Error", V4 = NA, V5 = NA, V6 = NA,
        V7 = NA, V8 = NA, V9 = NA, V10 = NA, V11 = NA, V12 = NA,
        stringsAsFactors = FALSE
      )
    }
    names(parsed)[1:min(12, ncol(parsed))] <- c(
      "id", "input_address", "match_status", "match_type", "matched_address",
      "coords", "tiger_line_id", "side", "state_fips", "county_fips", "tract", "block"
    )[1:min(12, ncol(parsed))]
    for (col in c("state_fips", "county_fips", "tract")) {
      if (!col %in% names(parsed)) parsed[[col]] <- NA_character_
    }

    out <- parsed %>%
      mutate(
        STOP_LOCATION_BLOCK = chunk,
        GEOID = ifelse(!is.na(state_fips) & state_fips != "" & !is.na(county_fips) & !is.na(tract) & tract != "",
          paste0(state_fips, county_fips, tract), NA_character_
        )
      ) %>%
      select(STOP_LOCATION_BLOCK, match_status, match_type, GEOID)

    write.table(out, cache_path,
      sep = ",", row.names = FALSE,
      col.names = !file.exists(cache_path), append = file.exists(cache_path)
    )

    message(sprintf(
      "[%s] batch %d/%d done (%d/%d matched)",
      format(Sys.time(), "%H:%M:%S"), i, length(chunks),
      sum(parsed$match_status == "Match", na.rm = TRUE), length(chunk)
    ))
    Sys.sleep(sleep_between)
  }

  invisible(read_csv(cache_path, show_col_types = FALSE))
}
