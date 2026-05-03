# Trans Balkan Race 2026 — one-time waypoint extractor
# Run locally to (re)generate waypoints.csv from the GPX file.
# weather_forecast.R reads the CSV; the GPX itself stays out of the public repo.
#
# Usage:  Rscript prepare_waypoints.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(xml2)
})

GPX_FILE <- "Trans Balkan Race 2026 __ Issue 1.GPX"
OUT_CSV  <- "waypoints.csv"

# 14 waypoints spanning the 5 climate zones from weather_outlook.md.
waypoint_targets <- tribble(
  ~name,                       ~km,
  "Trieste (start)",             0,
  "Slavnik karst",              28,
  "Platak mountain",           141,
  "Zavižan (Velebit summit)",  260,
  "Gospić",                    345,
  "CP1 Mazin",                 451,
  "Knin",                      537,
  "Livno",                     664,
  "Mostar",                    812,
  "Ulog (Bosnian highlands)",  897,
  "CP2 Kopilovi",             1060,
  "Žabljak (Durmitor)",       1160,
  "Nikšić",                   1325,
  "Risan (finish)",           1395
)

gpx <- read_xml(GPX_FILE)
ns  <- c(g = "http://www.topografix.com/GPX/1/1")
pts <- xml_find_all(gpx, ".//g:trkpt", ns)

track <- tibble(
  lat = as.numeric(xml_attr(pts, "lat")),
  lon = as.numeric(xml_attr(pts, "lon")),
  ele = as.numeric(xml_text(xml_find_first(pts, "g:ele", ns)))
)

haversine <- function(lat1, lon1, lat2, lon2) {
  r <- 6371
  phi1 <- lat1 * pi / 180; phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * r * asin(sqrt(a))
}

track <- track |>
  mutate(
    dist_km = haversine(lag(lat), lag(lon), lat, lon),
    cum_km  = cumsum(replace_na(dist_km, 0))
  )

waypoints <- waypoint_targets |>
  rowwise() |>
  mutate(
    .idx = which.min(abs(track$cum_km - km)),
    lat  = round(track$lat[.idx], 6),
    lon  = round(track$lon[.idx], 6),
    ele  = round(track$ele[.idx])
  ) |>
  ungroup() |>
  select(-.idx)

write_csv(waypoints, OUT_CSV)
cat(sprintf("Wrote %s (%d waypoints)\n", OUT_CSV, nrow(waypoints)))
print(waypoints, n = Inf)
