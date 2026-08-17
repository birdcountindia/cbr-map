library(googlesheets4)
library(tidyverse)
library(glue)
library(sf)
library(DBI)
library(geojsonsf)
library(rebird)
library(httr)
library(jsonlite)

update_map_data <- function() {
  
  gs4_auth(email = "alenalex@ncf-india.org")
  
  data <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1CKx9mL-AQxMGSr_nrLVmHdBVPNsVWCcbvWqVP_e51II/",
    sheet ="For website", range = "A:AJ", col_names = TRUE
  )
  
  # Total registrations (raw row count, before filtering)
  no_of_campus <- NROW(data)
  writeLines(as.character(no_of_campus), "no_of_campuses.txt")
  
  # --- Process fresh data ---
  data <- data |>
    select(c(2, 4, 5, 6, 8, 9, 11, 12, 15, 17, 18, 21, 32, 33)) |>
    setNames(c(
      "coords", "campus", "gmap", "web", "state", "area",
      "coordinator1", "type1", "email1",
      "coordinator2", "type2", "email2",
      "inat", "ebird"
    )) |>
    filter(!is.na(coords)) |>
    separate(
      coords,
      into = c("latitude", "longitude"),
      sep = ",\\s*",
      convert = TRUE
    ) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
    mutate(coordinator2 = map_chr(coordinator2, ~ as.character(c(.x, NA)[[1]])))
  
  # Distinct states / UTs among mapped campuses
  no_of_states <- n_distinct(data$state)
  writeLines(as.character(no_of_states), "no_of_states.txt")
  
  # --- Change detection (only decides the return value, NOT whether we write) ---
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  
  data_old <- NULL
  if (file.exists("data/data_old.RData")) {
    load("data/data_old.RData")  # loads object `data_old`
  }
  
  changed <- is.null(data_old) || !identical(data, data_old)
  
  if (changed) {
    data_old <- data
    save(data_old, file = "data/data_old.RData")
    message("--- Changes detected. Snapshot updated. ---")
  } else {
    message("--- No changes detected. Regenerating outputs anyway. ---")
  }
  
  # --- Always regenerate campuses.json from current data ---
  # Strip non-ASCII so the GeoJSON is clean for the web map.
  data_clean <- data %>%
    mutate(across(where(is.character), ~ str_squish(str_replace_all(., "[^[:ascii:]]", " "))))
  
  geojson <- sf_geojson(data_clean)
  
  # Guard: never write an NA/empty file silently.
  if (length(geojson) != 1 || is.na(geojson) || !nzchar(geojson)) {
    stop("sf_geojson() produced no output -- check that `data` has rows and is an sf object.")
  }
  
  writeLines(geojson, "campuses.json")
  message(glue("--- campuses.json written: {nrow(data_clean)} campuses. ---"))
  
  return(changed)
}
