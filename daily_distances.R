library(tidyverse)
library(xml2)

# ── Parameters ────────────────────────────────────────────────────────────────

GPX_FILE        <- "Trans Balkan Race 2026 __ Issue 1.GPX"
SLEEP_HOUR      <- 2      # target sleep time (02:00)
SLEEP_DURATION  <- 4.5    # minimum sleep hours; wake target = SLEEP_HOUR + SLEEP_DURATION = 06:30
MAX_SLEEP       <- 6      # maximum sleep hours; earliest valid stop = 06:30 − 6h = 00:30
MIN_RIDING_H    <- 10     # minimum riding hours per day (safety net against bridge days)
MAX_DAILY_CLIMB <- 4000   # m of elevation gain per day (soft: only enforced after 00:30)
BASE_SPEED      <- 13.8   # km/h on flat terrain
MAX_DESCENT     <- 32     # km/h cap on descents (rough terrain)
UPHILL_K        <- 0.08   # exponential penalty per % grade uphill
DOWNHILL_K      <- 0.04   # exponential gain per % grade downhill

# ── Parse GPX ─────────────────────────────────────────────────────────────────

gpx  <- read_xml(GPX_FILE)
ns   <- c(g = "http://www.topografix.com/GPX/1/1")
pts  <- xml_find_all(gpx, ".//g:trkpt", ns)

track <- tibble(
  lat = as.numeric(xml_attr(pts, "lat")),
  lon = as.numeric(xml_attr(pts, "lon")),
  ele = as.numeric(xml_text(xml_find_first(pts, "g:ele", ns)))
) |>
  mutate(ele = zoo::rollmedian(ele, k = 31, fill = NA, align = "center")) |>
  filter(!is.na(ele))

cat(sprintf("Track points: %d\n", nrow(track)))

# ── Haversine distance between consecutive points (km) ────────────────────────

haversine <- function(lat1, lon1, lat2, lon2) {
  r    <- 6371
  phi1 <- lat1 * pi / 180;  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * r * asin(sqrt(a))
}

# ── Segment-level calculations ────────────────────────────────────────────────

segments <- track |>
  mutate(
    dist_km  = haversine(lag(lat), lag(lon), lat, lon),
    ele_diff = ele - lag(ele),
    grade    = pmax(pmin(ifelse(dist_km > 0, (ele_diff / (dist_km * 1000)) * 100, 0), 30), -30)
  ) |>
  slice(-1) |>
  filter(!is.na(dist_km), dist_km > 0) |>
  mutate(
    speed_kmh = case_when(
      grade >= 0 ~ BASE_SPEED * exp(-UPHILL_K * grade),
      grade <  0 ~ pmin(BASE_SPEED * exp(-DOWNHILL_K * grade), MAX_DESCENT)
    ),
    time_h    = dist_km / speed_kmh,
    gain_m    = pmax(ele_diff, 0),
    cum_dist  = cumsum(dist_km),
    cum_time  = cumsum(time_h),
    cum_gain  = cumsum(gain_m)
  )

total_dist  <- sum(segments$dist_km)
total_time  <- sum(segments$time_h)
total_climb <- sum(segments$gain_m)

cat(sprintf("\nTotal distance : %.0f km\n",  total_dist))
cat(sprintf("Total climb    : %.0f m\n",    total_climb))
cat(sprintf("Total descent  : %.0f m\n",    sum(pmax(-segments$ele_diff, 0))))
cat(sprintf("Effective time : %.1f h\n\n",  total_time))

# ── Day simulation ────────────────────────────────────────────────────────────
# Each day ends at whichever limit is hit first:
#   (a) clock reaches SLEEP_HOUR (02:00)
#   (b) daily elevation gain reaches MAX_DAILY_CLIMB
# After stopping: sleep SLEEP_DURATION hours, then resume.

race_start <- as.POSIXct("2026-05-29 09:00:00", tz = "Europe/Paris")

simulate_days <- function(segs, start_wall) {
  # Elevation cap is SOFT: only enforced after the "earliest valid stop" time.
  # Earliest stop  = wake_target - MAX_SLEEP = 06:30 - 6 h = 00:30.
  # Stopping no earlier than 00:30 guarantees the next day starts at 06:30
  # (sleep ≤ MAX_SLEEP) and eliminates short bridge days.
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

    # Sleep until next 06:30, clamped to [SLEEP_DURATION, MAX_SLEEP]
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

day_info <- simulate_days(segments, race_start)

total_days <- nrow(day_info) - 1 +
  (total_time - day_info$cum_riding[nrow(day_info) - 1]) /
  day_info$riding_h[nrow(day_info)]

finish_wall <- day_info$day_start_wall[nrow(day_info)] +
  as.difftime(
    total_time - c(0, day_info$cum_riding)[nrow(day_info)],
    units = "hours"
  )

cat(sprintf("Days (sleep 02:00 / max %.0fm climb/day) : %.1f\n", MAX_DAILY_CLIMB, total_days))
cat(sprintf("Estimated finish : %s\n\n", format(finish_wall, "%a %d %b %Y %H:%M %Z")))

# ── Wall-clock converter ──────────────────────────────────────────────────────

wall_clock <- function(riding_time_h) {
  day_idx   <- pmin(findInterval(riding_time_h, day_info$cum_riding) + 1L, nrow(day_info))
  cum_start <- c(0, day_info$cum_riding)[day_idx]
  hours_in  <- riding_time_h - cum_start
  day_info$day_start_wall[day_idx] + as.difftime(hours_in, units = "hours")
}

# ── Checkpoints ───────────────────────────────────────────────────────────────

checkpoints <- tribble(
  ~name,             ~cum_dist_km,
  "CP1 Mazin",              451,
  "CP2 Kopilovi",          1060,
  "Finish Risan",          1395
)

# ── Daily table ───────────────────────────────────────────────────────────────

cat("────────────────────────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-28s  %5s  %5s  %7s  %8s  %10s  %s\n",
            "Day", "ride h", "sleep", "km/day", "climb m", "cum km", "Checkpoints"))
cat("────────────────────────────────────────────────────────────────────────────────────\n")

for (i in seq_len(nrow(day_info))) {
  d          <- day_info[i, ]
  start_dist <- if (i == 1) 0 else day_info$cum_dist[i - 1]
  end_dist   <- min(d$cum_dist, total_dist)

  cps <- checkpoints |>
    filter(cum_dist_km > start_dist, cum_dist_km <= end_dist) |>
    pull(name) |> paste(collapse = ", ")

  date_range <- sprintf("%s %s–%s",
    format(d$day_start_wall, "%a %d %b"),
    format(d$day_start_wall, "%H:%M"),
    format(d$day_end_wall,   "%H:%M"))

  dist_show <- if (d$cum_dist >= total_dist) sprintf("%.0f ✓", total_dist) else sprintf("%.0f", d$cum_dist)

  cat(sprintf("%-28s  %5.1f  %5.1f  %7.0f  %8.0f  %10s  %s\n",
              date_range, d$riding_h, d$sleep_h, d$dist_km, d$climb_m, dist_show, cps))

  if (d$cum_dist >= total_dist) break
}

cat("────────────────────────────────────────────────────────────────────────────────────\n")

# ── Resupply schedule ─────────────────────────────────────────────────────────

riding_time_at_dist <- function(dist_km) {
  approx(segments$cum_dist, segments$cum_time, xout = dist_km, rule = 2)$y
}

resupply <- tribble(
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
) |>
  mutate(
    riding_h = map_dbl(km, riding_time_at_dist),
    day_num  = findInterval(riding_h, day_info$cum_riding) + 1L,
    arrival  = do.call(c, map(riding_h, wall_clock)),
    gap_km   = km - lag(km, default = 0)
  )

cat(sprintf("\nResupply schedule (sleep %02.0f:00 · %.1f h · wake %02.0f:%02.0f · max %.0f m/day)\n",
            SLEEP_HOUR, SLEEP_DURATION,
            floor(SLEEP_HOUR + SLEEP_DURATION),
            round(((SLEEP_HOUR + SLEEP_DURATION) %% 1) * 60),
            MAX_DAILY_CLIMB))
cat(sprintf("%-25s  %6s  %5s  %-14s  %-5s  %s\n",
            "Location", "km", "+km", "Type", "Day", "Est. arrival (CEST)"))
cat(strrep("─", 76), "\n")

for (i in seq_len(nrow(resupply))) {
  r   <- resupply[i, ]
  off <- if (!r$on_route) " *" else "  "
  cat(sprintf("%-25s  %6.0f  %5.0f  %-14s  Day%-2d  %s%s\n",
              r$location, r$km, r$gap_km, r$type, r$day_num,
              format(r$arrival, "%a %d %b %H:%M"), off))
}

cat(strrep("─", 76), "\n")
cat("* off-route stop\n\n")

# ── Plot ──────────────────────────────────────────────────────────────────────

elev_profile <- segments |>
  select(cum_dist, ele, cum_time) |>
  mutate(day = factor(pmin(
    findInterval(cum_time, day_info$cum_riding) + 1L, nrow(day_info)
  )))

day_lines <- day_info |>
  filter(cum_dist < total_dist) |>
  mutate(label = sprintf("Day %d\n%.0f km", day + 1, cum_dist))

p <- ggplot(elev_profile, aes(cum_dist, ele)) +
  geom_area(aes(fill = day), alpha = 0.55) +
  geom_vline(data = day_lines, aes(xintercept = cum_dist),
             linetype = "dashed", colour = "grey40", linewidth = 0.35) +
  geom_text(data = day_lines,
            aes(x = cum_dist, y = max(elev_profile$ele) * 0.95, label = label),
            size = 2.8, hjust = 1.08, colour = "grey30") +
  geom_vline(data = checkpoints, aes(xintercept = cum_dist_km),
             linetype = "dotted", colour = "firebrick", linewidth = 0.5) +
  geom_text(data = checkpoints, aes(x = cum_dist_km, y = 80, label = name),
            size = 2.6, hjust = -0.05, colour = "firebrick") +
  scale_fill_brewer(palette = "Set2", name = "Day") +
  scale_x_continuous(breaks = seq(0, 1400, 100)) +
  labs(
    title    = "Trans Balkan Race 2026 — Daily Distance Plan",
    subtitle = sprintf(
      "%.0f km · %.0f m climb · sleep 02:00 / wake 06:30 · max %.0f m/day · %.1f day finish",
      total_dist, total_climb, MAX_DAILY_CLIMB, total_days),
    x = "Distance from start (km)", y = "Elevation (m)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("daily_distances.png", p, width = 14, height = 5, dpi = 150)
cat("Plot saved to daily_distances.png\n")
