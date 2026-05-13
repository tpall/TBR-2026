# Shared TBR 2026 race-plan computations.
# Consumed by daily_distances.R (CLI) and report.qmd (HTML).
# Defines parameters, route data, and the GPX → segments → daily plan pipeline.

library(tidyverse)
library(xml2)

# ── Parameters ────────────────────────────────────────────────────────────────

GPX_FILE        <- "Trans Balkan Race 2026 __ Issue 1.GPX"
SLEEP_HOUR      <- 2      # target sleep time (02:00)
SLEEP_DURATION  <- 4.5    # minimum sleep hours; wake target = SLEEP_HOUR + SLEEP_DURATION = 06:30
MAX_SLEEP       <- 6      # maximum sleep hours; earliest valid stop = 06:30 − 6h = 00:30
MIN_RIDING_H    <- 10     # minimum riding hours per day (safety net against bridge days)
MAX_DAILY_CLIMB <- 4000   # m elevation gain/day (soft: only enforced after 00:30)
BASE_SPEED      <- 13.8   # km/h on flat terrain
MAX_DESCENT     <- 32     # km/h cap on descents
UPHILL_K        <- 0.08   # exponential penalty per % grade uphill
DOWNHILL_K      <- 0.04   # exponential gain per % grade downhill

RACE_START <- as.POSIXct("2026-05-29 09:00:00", tz = "Europe/Paris")

CHECKPOINTS <- tribble(
  ~name,             ~cum_dist_km,
  "CP1 Mazin",              451,
  "CP2 Kopilovi",          1060,
  "Finish Risan",          1395
)

RESUPPLY <- tribble(
  ~location,              ~km,   ~type,         ~on_route,
  "Kozina (SLO)",         16.6,  "Village",      TRUE,
  "Slavnik (SLO)",        28.2,  "Mountain hut", TRUE,
  "Dane (CRO)",           47.4,  "Fountain",     TRUE,
  "Sveti Pavel (SLO)",    74.2,  "Fountain",     TRUE,
  "Platak (CRO)",        141,    "Mountain hut", TRUE,
  "Fužine (CRO)",        164,    "Village",      TRUE,
  "Oltari (CRO)",        245,    "Fountain",     TRUE,
  "Dom Zavižan (CRO)",   260,    "Mountain hut", TRUE,
  "Gospić (CRO)",        345,    "City",         TRUE,
  "CP1 Mazin (CRO)",     451,    "Checkpoint",   TRUE,
  "Velika Popina (CRO)", 490,    "Fountain",     TRUE,
  "Knin (CRO)",          537,    "City",         TRUE,
  "Livno (BiH)",         664,    "Village",      TRUE,
  "Šuica (BiH)",         701,    "Village",      TRUE,
  "Mostar (BiH)",        812,    "City",         TRUE,
  "Nevesinje (BiH)",     858,    "Village",      FALSE,
  "Ulog (BiH)",          897,    "Village",      TRUE,
  "Fountain (BiH)",      938,    "Fountain",     TRUE,
  "Miljevina (BiH)",     990,    "Village",      FALSE,
  "Popov Most (BiH)",   1034,    "Village",      TRUE,
  "Brod (BiH)",         1055,    "Village",      TRUE,
  "CP2 Kopilovi (BiH)", 1060,    "Checkpoint",   TRUE,
  "Žabljak (MNE)",      1160,    "City",         TRUE,
  "Njegovuđa (MNE)",    1170,    "Village",      TRUE,
  "Kolašin (MNE)",      1228,    "City",         FALSE,
  "Nikšić (MNE)",       1325,    "City",         TRUE,
  "Risan / Finish",     1395,    "Finish",       TRUE
)

# ── Geometry & speed helpers ──────────────────────────────────────────────────

haversine <- function(lat1, lon1, lat2, lon2) {
  r    <- 6371
  phi1 <- lat1 * pi / 180;  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * r * asin(sqrt(a))
}

speed_for_grade <- function(grade) {
  ifelse(grade >= 0,
         BASE_SPEED * exp(-UPHILL_K * grade),
         pmin(BASE_SPEED * exp(-DOWNHILL_K * grade), MAX_DESCENT))
}

# ── Pipeline: GPX → segments → daily plan ─────────────────────────────────────

load_track <- function(path) {
  gpx <- read_xml(path)
  ns  <- c(g = "http://www.topografix.com/GPX/1/1")
  pts <- xml_find_all(gpx, ".//g:trkpt", ns)

  tibble(
    lat = as.numeric(xml_attr(pts, "lat")),
    lon = as.numeric(xml_attr(pts, "lon")),
    ele = as.numeric(xml_text(xml_find_first(pts, "g:ele", ns)))
  ) |>
    mutate(ele = zoo::rollmedian(ele, k = 31, fill = NA, align = "center")) |>
    filter(!is.na(ele))
}

compute_segments <- function(track) {
  track |>
    mutate(
      dist_km  = haversine(lag(lat), lag(lon), lat, lon),
      ele_diff = ele - lag(ele),
      grade    = pmax(pmin(ifelse(dist_km > 0, (ele_diff / (dist_km * 1000)) * 100, 0), 30), -30)
    ) |>
    slice(-1) |>
    filter(!is.na(dist_km), dist_km > 0) |>
    mutate(
      speed_kmh = speed_for_grade(grade),
      time_h    = dist_km / speed_kmh,
      gain_m    = pmax(ele_diff, 0),
      cum_dist  = cumsum(dist_km),
      cum_time  = cumsum(time_h),
      cum_gain  = cumsum(gain_m)
    )
}

# Each day ends at whichever limit is hit first:
#   (a) clock reaches SLEEP_HOUR (02:00)
#   (b) daily elevation gain reaches MAX_DAILY_CLIMB (soft: only after 00:30)
# After stopping: sleep SLEEP_DURATION..MAX_SLEEP hours so next day starts at 06:30.
simulate_days <- function(segs, start_wall) {
  WAKE_HOUR <- SLEEP_HOUR + SLEEP_DURATION   # 6.5 = 06:30

  days_list <- list()
  seg_idx   <- 1L
  wall_time <- start_wall

  while (seg_idx <= nrow(segs)) {
    day_h0          <- as.numeric(difftime(wall_time, floor_date(wall_time, "day"), units = "hours"))
    hrs_to_latest   <- (SLEEP_HOUR            - day_h0 + 24) %% 24  # hours until 02:00
    hrs_to_earliest <- (WAKE_HOUR - MAX_SLEEP - day_h0 + 24) %% 24  # hours until 00:30
    if (hrs_to_latest   == 0) hrs_to_latest   <- 24
    if (hrs_to_earliest == 0) hrs_to_earliest <- 24

    day_riding <- 0;  day_climb <- 0;  day_dist <- 0;  day_start <- wall_time

    while (seg_idx <= nrow(segs)) {
      s <- segs[seg_idx, ]
      if (day_riding + s$time_h > hrs_to_latest) break         # hard stop at 02:00
      if (day_climb  + s$gain_m > MAX_DAILY_CLIMB &&           # elevation cap —
          day_riding >= MIN_RIDING_H              &&            #   only after min ride
          day_riding >= hrs_to_earliest) break                  #   and after 00:30
      day_riding <- day_riding + s$time_h
      day_dist   <- day_dist   + s$dist_km
      day_climb  <- day_climb  + s$gain_m
      seg_idx    <- seg_idx + 1L
    }

    stop_wall <- day_start + as.difftime(day_riding, units = "hours")

    next_0630 <- floor_date(stop_wall, "day") + as.difftime(WAKE_HOUR, units = "hours")
    if (next_0630 <= stop_wall) next_0630 <- next_0630 + days(1)
    sleep_h   <- max(SLEEP_DURATION, min(MAX_SLEEP,
                   as.numeric(difftime(next_0630, stop_wall, units = "hours"))))

    days_list[[length(days_list) + 1]] <- list(
      day_start_wall = day_start,
      day_end_wall   = stop_wall,
      riding_h       = day_riding,
      dist_km        = day_dist,
      climb_m        = day_climb,
      sleep_h        = sleep_h
    )

    wall_time <- stop_wall + as.difftime(sleep_h, units = "hours")
    if (seg_idx > nrow(segs)) break
  }

  bind_rows(lapply(days_list, as_tibble)) |>
    mutate(day = row_number(), cum_dist = cumsum(dist_km), cum_riding = cumsum(riding_h))
}

# Convert ride-time hours into wall-clock POSIXct using the day plan.
wall_clock <- function(riding_time_h, day_info) {
  day_idx   <- pmin(findInterval(riding_time_h, day_info$cum_riding) + 1L, nrow(day_info))
  cum_start <- c(0, day_info$cum_riding)[day_idx]
  hours_in  <- riding_time_h - cum_start
  day_info$day_start_wall[day_idx] + as.difftime(hours_in, units = "hours")
}

build_resupply <- function(resupply, segments, day_info) {
  riding_time_at_dist <- function(dist_km) {
    approx(segments$cum_dist, segments$cum_time, xout = dist_km, rule = 2)$y
  }

  resupply |>
    mutate(
      riding_h = map_dbl(km, riding_time_at_dist),
      day_num  = findInterval(riding_h, day_info$cum_riding) + 1L,
      arrival  = do.call(c, map(riding_h, \(h) wall_clock(h, day_info))),
      gap_km   = km - lag(km, default = 0),
      gap_h    = riding_h - lag(riding_h, default = 0)
    )
}
