library(tidyverse)
library(xml2)

# ── Parameters ────────────────────────────────────────────────────────────────

GPX_FILE      <- "Trans Balkan Race 2026 __ Issue 1.GPX"
RIDING_HOURS  <- 16          # riding hours per day
BASE_SPEED    <- 15          # km/h on flat terrain
MAX_DESCENT   <- 32          # km/h cap on descents (rough terrain)
UPHILL_K      <- 0.08        # exponential penalty per % grade uphill
DOWNHILL_K    <- 0.04        # exponential gain per % grade downhill

# ── Parse GPX ─────────────────────────────────────────────────────────────────

gpx  <- read_xml(GPX_FILE)
ns   <- c(g = "http://www.topografix.com/GPX/1/1")
pts  <- xml_find_all(gpx, ".//g:trkpt", ns)

track <- tibble(
  lat = as.numeric(xml_attr(pts, "lat")),
  lon = as.numeric(xml_attr(pts, "lon")),
  ele = as.numeric(xml_text(xml_find_first(pts, "g:ele", ns)))
)

cat(sprintf("Track points: %d\n", nrow(track)))

# ── Smooth elevation to remove GPS noise (rolling median) ─────────────────────

smooth_window <- 15   # points each side (~300m window at typical point spacing)

track <- track |>
  mutate(ele = zoo::rollmedian(ele, k = smooth_window * 2 + 1,
                               fill = NA, align = "center")) |>
  filter(!is.na(ele))

# ── Haversine distance between consecutive points (km) ────────────────────────

haversine <- function(lat1, lon1, lat2, lon2) {
  r <- 6371
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * r * asin(sqrt(a))
}

# ── Segment-level calculations ────────────────────────────────────────────────

segments <- track |>
  mutate(
    dist_km  = haversine(lag(lat), lag(lon), lat, lon),
    ele_diff = ele - lag(ele),                # metres
    grade    = ifelse(dist_km > 0, (ele_diff / (dist_km * 1000)) * 100, 0),
    grade    = pmax(pmin(grade, 30), -30)    # cap at ±30%
  ) |>
  slice(-1) |>                               # drop first row (no lag)
  filter(!is.na(dist_km), dist_km > 0)

# Speed model: exponential penalty for uphills, capped gain on descents
segments <- segments |>
  mutate(
    speed_kmh = case_when(
      grade >= 0 ~ BASE_SPEED * exp(-UPHILL_K * grade),
      grade <  0 ~ pmin(BASE_SPEED * exp(-DOWNHILL_K * grade), MAX_DESCENT)
    ),
    time_h = dist_km / speed_kmh
  )

# ── Cumulative distance and time ──────────────────────────────────────────────

segments <- segments |>
  mutate(
    cum_dist = cumsum(dist_km),
    cum_time = cumsum(time_h)
  )

total_dist <- sum(segments$dist_km)
total_time <- sum(segments$time_h)
total_days <- total_time / RIDING_HOURS

cat(sprintf("\nTotal distance : %.0f km\n", total_dist))
cat(sprintf("Total climb    : %.0f m\n",  sum(pmax(segments$ele_diff, 0), na.rm = TRUE)))
cat(sprintf("Total descent  : %.0f m\n",  sum(pmax(-segments$ele_diff, 0), na.rm = TRUE)))
cat(sprintf("Effective time : %.1f h\n",  total_time))
cat(sprintf("Days (%.0fh/day) : %.1f\n\n", RIDING_HOURS, total_days))

# ── Daily breakdown ───────────────────────────────────────────────────────────

race_start <- as.POSIXct("2026-05-29 09:00:00", tz = "Europe/Paris")

checkpoints <- tribble(
  ~name,  ~cum_dist_km,
  "CP1 Mazin",        451,
  "CP2 Kopilovi",    1060,
  "Finish Risan",    1395
)

# Find the cumulative distance and time at day boundaries
day_boundaries <- tibble(day = 1:ceiling(total_days)) |>
  mutate(time_limit = day * RIDING_HOURS)

# For each day, find where on the route we'll be
find_position <- function(time_limit) {
  idx <- which(segments$cum_time <= time_limit)
  if (length(idx) == 0) return(list(cum_dist = 0, cum_time = 0))
  i <- max(idx)
  # Interpolate into the next segment
  remaining <- time_limit - segments$cum_time[i]
  extra_dist <- remaining * segments$speed_kmh[min(i + 1, nrow(segments))]
  list(
    cum_dist = segments$cum_dist[i] + extra_dist,
    cum_time = time_limit
  )
}

daily <- day_boundaries |>
  mutate(
    pos       = map(time_limit, find_position),
    cum_dist  = map_dbl(pos, "cum_dist"),
    cum_time  = map_dbl(pos, "cum_time"),
    date      = race_start + days(day - 1),
    date_str  = format(date, "%a %b %d")
  ) |>
  select(-pos) |>
  mutate(
    day_dist  = cum_dist - lag(cum_dist, default = 0),
    day_label = sprintf("Day %d (%s)", day, date_str)
  )

# ── Print daily table ─────────────────────────────────────────────────────────

cat("─────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-28s  %8s  %10s  %s\n", "Day", "km/day", "cum km", "Checkpoints passed"))
cat("─────────────────────────────────────────────────────────────────\n")

for (i in seq_len(nrow(daily))) {
  d     <- daily[i, ]
  start <- if (i == 1) 0 else daily$cum_dist[i - 1]
  end   <- d$cum_dist

  cps <- checkpoints |>
    filter(cum_dist_km > start, cum_dist_km <= end) |>
    pull(name) |>
    paste(collapse = ", ")

  if (d$cum_dist >= total_dist) {
    cat(sprintf("%-28s  %8.0f  %10.0f  %s\n",
                d$day_label, d$day_dist, total_dist, ifelse(nchar(cps) > 0, cps, "")))
    break
  } else {
    cat(sprintf("%-28s  %8.0f  %10.0f  %s\n",
                d$day_label, d$day_dist, d$cum_dist, cps))
  }
}

cat("─────────────────────────────────────────────────────────────────\n")

# ── Resupply schedule ─────────────────────────────────────────────────────────
# Convert riding time to wall-clock time:
#   day N starts at race_start + (N-1)*24h
#   within that day the rider has been riding for (riding_time - (N-1)*RIDING_HOURS)

riding_time_at_dist <- function(dist_km) {
  # Linear interpolation of cum_time at a given cum_dist
  approx(segments$cum_dist, segments$cum_time, xout = dist_km, rule = 2)$y
}

wall_clock <- function(riding_time_h) {
  day_num   <- ceiling(riding_time_h / RIDING_HOURS)           # which riding day
  day_num   <- pmax(day_num, 1)
  hours_in  <- riding_time_h - (day_num - 1) * RIDING_HOURS   # hours into that day
  race_start + days(day_num - 1) + hours(floor(hours_in)) +
    minutes(round((hours_in %% 1) * 60))
}

resupply <- tribble(
  ~location,              ~km,   ~type,        ~on_route,
  "Kozina (SLO)",         16.6,  "Village",     TRUE,
  "Slavnik (SLO)",        28.2,  "Mtn Hut",     TRUE,
  "Dane (CRO)",           47.4,  "Fountain",    TRUE,
  "Sveti Pavel (SLO)",    74.2,  "Fountain",    TRUE,
  "Platak (CRO)",        141,    "Mtn Hut",     TRUE,
  "Fužine (CRO)",        164,    "Village",     TRUE,
  "Oltari (CRO)",        245,    "Fountain",    TRUE,
  "Dom Zavižan (CRO)",   260,    "Mtn Hut",     TRUE,
  "Gospić (CRO)",        345,    "City",        TRUE,
  "CP1 Mazin (CRO)",     451,    "Checkpoint",  TRUE,
  "Velika Popina (CRO)", 490,    "Fountain",    TRUE,
  "Knin (CRO)",          537,    "City",        TRUE,
  "Livno (BiH)",         664,    "Village",     TRUE,
  "Šuica (BiH)",         701,    "Village",     TRUE,
  "Mostar (BiH)",        812,    "City",        TRUE,
  "Nevesinje (BiH)",     858,    "Village",     FALSE,
  "Ulog (BiH)",          897,    "Village",     TRUE,
  "Fountain (BiH)",      938,    "Fountain",    TRUE,
  "Miljevina (BiH)",     990,    "Village",     FALSE,
  "Popov Most (BiH)",   1034,    "Village",     TRUE,
  "Brod (BiH)",         1055,    "Village",     TRUE,
  "CP2 Kopilovi (BiH)", 1060,    "Checkpoint",  TRUE,
  "Žabljak (MNE)",      1160,    "City",        TRUE,
  "Njegovuđa (MNE)",    1170,    "Village",     TRUE,
  "Kolašin (MNE)",      1228,    "City",        FALSE,
  "Nikšić (MNE)",       1325,    "City",        TRUE,
  "Risan / Finish",     1395,    "Finish",      TRUE
) |>
  mutate(
    riding_h  = map_dbl(km, riding_time_at_dist),
    day_num   = pmax(ceiling(riding_h / RIDING_HOURS), 1),
    arrival   = map(riding_h, wall_clock),
    arrival   = do.call(c, arrival),
    date_str  = format(arrival, "%a %b %d"),
    time_str  = format(arrival, "%H:%M"),
    gap_km    = km - lag(km, default = 0)
  )

REST_HOURS <- 24 - RIDING_HOURS

cat("\nResupply schedule (16 h/day riding, 8 h rest starting at day end)\n")
cat(sprintf("%-25s  %6s  %5s  %-12s  %6s  %-10s  %s\n",
            "Location", "km", "+km", "Type", "Day", "Date", "Est. arrival"))
cat(strrep("─", 80), "\n")

for (i in seq_len(nrow(resupply))) {
  r <- resupply[i, ]
  off <- if (!r$on_route) " *" else "  "
  cat(sprintf("%-25s  %6.0f  %5.0f  %-12s  %6s  %-10s  %s%s\n",
              r$location, r$km, r$gap_km, r$type,
              paste0("Day ", r$day_num), r$date_str, r$time_str, off))
}

cat(strrep("─", 80), "\n")
cat("* off-route stop\n\n")

# ── Plot ──────────────────────────────────────────────────────────────────────

elev_profile <- segments |>
  select(cum_dist, ele, cum_time) |>
  mutate(day = pmin(ceiling(cum_time / RIDING_HOURS), ceiling(total_days)))

day_starts <- daily |>
  filter(cum_dist < total_dist) |>
  mutate(label = sprintf("Day %d\n%.0f km", day + 1, cum_dist))

p <- ggplot(elev_profile, aes(cum_dist, ele)) +
  geom_area(aes(fill = factor(day)), alpha = 0.6) +
  geom_vline(data = day_starts, aes(xintercept = cum_dist),
             linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_text(data = day_starts, aes(x = cum_dist, y = 2200, label = label),
            size = 2.8, hjust = 1.05, colour = "grey30") +
  geom_vline(data = checkpoints, aes(xintercept = cum_dist_km),
             linetype = "dotted", colour = "red", linewidth = 0.5) +
  geom_text(data = checkpoints, aes(x = cum_dist_km, y = 100, label = name),
            size = 2.5, hjust = -0.05, colour = "red") +
  scale_fill_brewer(palette = "Set2", name = "Day") +
  labs(
    title    = "Trans Balkan Race 2026 — Daily Distance Plan",
    subtitle = sprintf("%.0f km · %.0f m climb · %dh/day riding · %.1f day finish",
                       total_dist,
                       sum(pmax(segments$ele_diff, 0), na.rm = TRUE),
                       RIDING_HOURS, total_days),
    x = "Distance from start (km)",
    y = "Elevation (m)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("daily_distances.png", p, width = 14, height = 5, dpi = 150)
cat("\nPlot saved to daily_distances.png\n")
