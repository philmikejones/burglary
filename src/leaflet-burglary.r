library("dplyr")
library("sf")
library("leaflet")


##
## Burglary data
##
burglary = list.files("data/crimes/Derbyshire/crimes", pattern = "-street.csv", full.names = TRUE)
notts = list.files("data/crimes/Nottinghamshire", pattern = "-street.csv", full.names = TRUE)
burglary = c(burglary, notts)

burglary = burglary[!grepl("2014", burglary)]
burglary = burglary[!grepl("2015", burglary)]
burglary = burglary[!grepl("2016", burglary)]
burglary = burglary[!grepl("2017", burglary)]
burglary = burglary[!grepl("2018", burglary)]
burglary = burglary[!grepl("2019", burglary)]
burglary = burglary[!grepl("2020", burglary)]
burglary = burglary[!grepl("2021", burglary)]
burglary = burglary[!grepl("2022", burglary)]
burglary = lapply(burglary, readr::read_csv, show_col_types = FALSE)
burglary = bind_rows(burglary)


burglary =
    burglary |>
    rename(
        month = Month,
        long = Longitude,
        lat = Latitude,
        type = `Crime type`
    ) |>
    select(month, type, long, lat) |>
    filter(type == "Burglary")|>
    mutate(month = paste0(month, "-01")) |>
    mutate(month = lubridate::as_date(month)) |>
    filter(month >= "2023-06-01") |>
    filter(!is.na(long), !is.na(lat)) |>
    st_as_sf(coords = c("long", "lat")) |>
    st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


##
## LSOAs
##
lsoas =
    read_sf("data/boundaries/Derbyshire/derbyshire_lsoas_simp.gpkg") |>
    sf::st_transform(crs = 4326)

nottinghamshire_lsoas =
    read_sf("data/boundaries/Nottinghamshire/notts-lsoas-simp.gpkg") |>
    sf::st_transform(crs = 4326)

lsoas = bind_rows(lsoas, nottinghamshire_lsoas)


##
## Constabularies
##
derbys =
    read_sf("data/boundaries/Derbyshire/derbyshire-dissolved-simp.gpkg") |>
    sf::st_transform(crs = 4326)

notts =
    read_sf("data/boundaries/Nottinghamshire/notts-dissolved-simp.gpkg") |>
    sf::st_transform(crs = 4326)


##
## Census data
## Households are unit at risk
##
houses = 
    readr::read_csv("data/census/households.csv", show_col_types = FALSE) |>
    rename(code = mnemonic, households = `2021`) |>
    select(code, households)

lsoas = 
    left_join(lsoas, houses, by = "code") |>
    mutate(households = as.integer(households))
assertthat::assert_that(!any(is.na(lsoas$households)))


# count burglaries per LSOA
lsoas$burglary_count <- lengths(st_intersects(lsoas, burglary))
lsoas =
    lsoas |>
    mutate(rate = (burglary_count / households) * 1000)

mean_rate = mean(lsoas$rate)
sd_rate = sd(lsoas$rate)

bins = c(-Inf, mean_rate, (mean_rate + (1.96 * sd_rate)), (mean_rate + (3 * sd_rate)), Inf)
pal = colorBin(c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33"), domain = lsoas$rate, bins = bins)
labels = sprintf(
  "<strong>%s</strong><br/>%g rate per 1,000 households",
  lsoas$code, lsoas$rate
) |> lapply(htmltools::HTML)


map =
    leaflet(lsoas) |>
    addTiles() |>
    addPolygons(data = derbys, fillColor = "transparent") |>
    addPolygons(data = notts, fillColor = "transparent") |>
    addPolygons(
        fillColor = ~pal(rate),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
        )
    ) |>
    addLegend(
        labels = c("Below average", "Above average", "Highest 95%", "Highest 99%"),
        colors = c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33"),
        values = ~rate, opacity = 0.7, title = NULL, position = "bottomright")

withr::with_dir(
    "./docs",
    htmlwidgets::saveWidget(map, file = "burglary-rate.html", title = "Burglary Rate")
)
