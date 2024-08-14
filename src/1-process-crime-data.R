library("dplyr")
library("lubridate")
library("sf")

crimes = list.files("data/crimes/Derbyshire/crimes", pattern = "-street.csv", full.names = TRUE)

# currently want just last year for plotting
# Exclude before June 2023 because of changes in burlgary reporting
# Currently May 2023 - April 2024 inclusive
crimes = crimes[grepl('2023|2024', crimes)]
crimes = crimes[!grepl('2023-01|2023-02|2023-03|2023-04|2023-05', crimes)]
crimes = lapply(crimes, readr::read_csv, show_col_types = FALSE)
crimes = bind_rows(crimes)

crimes_notts = list.files("data/crimes/Nottinghamshire", pattern = "-street.csv", full.names = TRUE)
crimes_notts = crimes_notts[grepl("2023|2024", crimes_notts)]
crimes_notts = crimes_notts[!grepl('2023-01|2023-02|2023-03|2023-04|2023-05', crimes_notts)]
crimes_notts = lapply(crimes_notts, readr::read_csv, show_col_types = FALSE)
crimes_notts = bind_rows(crimes_notts)

crimes = bind_rows(crimes, crimes_notts)

crimes =
    crimes |>
    mutate(Month = ym(Month)) |>
    mutate(id = row_number()) |>
    select(id, Month, Longitude, Latitude, `Crime type`) |>
    rename(
        month      = Month,
        long       = Longitude,
        lat        = Latitude,
        crime_type = `Crime type`
    )

crimes =
    crimes |>
    filter(crime_type == "Burglary") |>
    filter(!is.na(lat))

crimes = 
    st_as_sf(crimes, coords = c("long", "lat"), crs = 4326) |>
    st_transform(crs = 27700)  # British National Grid

sf::write_sf(crimes, "data/crimes/burglary-2023-2024.gpkg")
