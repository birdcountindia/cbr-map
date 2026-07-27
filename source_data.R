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
data<- read_sheet("https://docs.google.com/spreadsheets/d/1CKx9mL-AQxMGSr_nrLVmHdBVPNsVWCcbvWqVP_e51II/", 
                       sheet = 3,range = "A:AJ", col_names = TRUE)

no_of_campus <- NROW(data)
writeLines(as.character(no_of_campus), "no_of_campuses.txt")

# Process fresh data
data <- data |>
  select(c(2, 4, 5, 6, 8, 9, 11, 15, 16, 17, 21, 22, 32, 33)) |>
  setNames(c(
    "coords", "campus", "gmap", "web", "state", "area",
    "coordinator1", "email1", "phone1",
    "coordinator2", "email2", "phone2",
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
  mutate(
    phone1 = map_chr(phone1, ~ if (length(.x) == 0) NA_character_ else as.character(.x[[1]])),
    phone2 = map_chr(phone2, ~ if (length(.x) == 0) NA_character_ else as.character(.x[[1]]))
  )
  
no_of_states <- unique(data$state) 
no_of_states <-  n_distinct(no_of_states)
writeLines(as.character(no_of_states), "no_of_states.txt")

load("data/data_old.RData") 

if (identical(data, data_old)) {
  message("--- No changes detected. Skipping update. ---")
  return(FALSE)
} else {
  data_old <- data
  save(data_old, file = "data/data_old.RData")
  message("--- Changes detected! Proceeding with update... ---")

  data <- data %>%
    mutate(across(where(is.character), ~ {
      clean_text <- str_replace_all(., "[^[:ascii:]]", " ") 
      str_squish(clean_text)
    }))
  
  sf_geojson(data) %>% 
  write("campuses.json")
  
  message("--- Map data successfully updated. ---")
  return(TRUE)
}
}