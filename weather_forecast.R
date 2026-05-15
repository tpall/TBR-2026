# Trans Balkan Race 2026 — daily weather forecast along the route
# Uses Open-Meteo (free, no API key, 16-day daily + hourly forecast)
# Run via .github/workflows/weather-forecast.yml or manually with `Rscript weather_forecast.R`

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr2)
  library(jsonlite)
})

# Stable English date formatting regardless of host locale
invisible(Sys.setlocale("LC_TIME", "C"))

# ── Parameters ────────────────────────────────────────────────────────────────

WAYPOINTS_CSV       <- "waypoints.csv"   # generated once from GPX; see prepare_waypoints.R
RACE_START          <- as.POSIXct("2026-05-29 09:00:00", tz = "Europe/Belgrade")
RACE_END            <- as.POSIXct("2026-06-07 23:59:00", tz = "Europe/Belgrade")
NIGHT_START_H       <- 22       # nighttime window 22:00 → 06:00 next day
NIGHT_END_H         <- 6
HIGH_ALT_M          <- 1000     # waypoints at/above this elevation get ⛰️ marker
NIGHT_RAIN_ALERT_MM <- 3        # threshold for "Nights with measurable rain" section
OUT_MD              <- "weather_forecast.md"
OUT_JSON            <- "weather_forecast.json"

# ── Load waypoints (committed CSV, no GPX dependency) ────────────────────────

waypoints <- read_csv(WAYPOINTS_CSV, show_col_types = FALSE)
cat("Waypoints:\n")
print(waypoints, n = Inf)

# ── Fetch Open-Meteo daily + hourly ───────────────────────────────────────────

fetch_forecast <- function(lat, lon, ele) {
  resp <- request("https://api.open-meteo.com/v1/forecast") |>
    req_url_query(
      latitude        = lat,
      longitude       = lon,
      elevation       = ele,                 # tell API to use waypoint elevation, not station
      wind_speed_unit = "ms",                # report wind in m/s
      daily           = paste(c(
        "temperature_2m_max", "temperature_2m_min",
        "precipitation_sum", "precipitation_probability_max",
        "wind_speed_10m_max", "wind_gusts_10m_max",
        "weather_code"
      ), collapse = ","),
      hourly          = paste(c(
        "temperature_2m", "precipitation",
        "wind_speed_10m", "wind_gusts_10m"
      ), collapse = ","),
      forecast_days   = 16,
      timezone        = "Europe/Belgrade"
    ) |>
    req_retry(max_tries = 3, backoff = ~5) |>
    req_perform()

  body <- resp_body_json(resp)

  # Open-Meteo can return ragged series — e.g. `precipitation_probability_max`
  # only covers ~14 days vs 16 for other daily metrics, and `wind_gusts_10m`
  # may end a few hours earlier than other hourly metrics. Pad shorter
  # columns with NA so they align into a single tibble.
  pad_to <- function(x, n) {
    v <- unlist(lapply(x, function(z) if (is.null(z)) NA_real_ else z))
    if (length(v) < n) c(v, rep(NA_real_, n - length(v))) else v[seq_len(n)]
  }

  n_days <- length(body$daily$time)
  daily <- tibble(
    date       = as.Date(unlist(body$daily$time)),
    tmin       = pad_to(body$daily$temperature_2m_min, n_days),
    tmax       = pad_to(body$daily$temperature_2m_max, n_days),
    precip_mm  = pad_to(body$daily$precipitation_sum, n_days),
    precip_pct = pad_to(body$daily$precipitation_probability_max, n_days),
    wind_ms    = pad_to(body$daily$wind_speed_10m_max, n_days),
    gust_ms    = pad_to(body$daily$wind_gusts_10m_max, n_days),
    wcode      = pad_to(body$daily$weather_code, n_days)
  )

  n_hours <- length(body$hourly$time)
  hourly <- tibble(
    # Open-Meteo returns ISO 8601 timestamps ("2026-05-29T00:00"); the explicit
    # format is required — without it as.POSIXct silently drops the time and
    # sets every hour to 00:00, which collapses the night-window aggregation.
    ts       = as.POSIXct(unlist(body$hourly$time),
                          format = "%Y-%m-%dT%H:%M", tz = "Europe/Belgrade"),
    temp     = pad_to(body$hourly$temperature_2m, n_hours),
    precip   = pad_to(body$hourly$precipitation, n_hours),
    wind_ms  = pad_to(body$hourly$wind_speed_10m, n_hours),
    gust_ms  = pad_to(body$hourly$wind_gusts_10m, n_hours)
  )

  list(daily = daily, hourly = hourly)
}

cat("\nFetching forecasts from Open-Meteo...\n")
fc <- waypoints |>
  rowwise() |>
  mutate(forecast = list(fetch_forecast(lat, lon, ele))) |>
  ungroup()

daily <- fc |>
  mutate(d = map(forecast, "daily")) |>
  select(-forecast) |>
  unnest(d)

hourly <- fc |>
  mutate(h = map(forecast, "hourly")) |>
  select(-forecast) |>
  unnest(h)

# ── Aggregate hourly into night-precipitation per (waypoint, date) ────────────
# Night = NIGHT_START_H of date D → NIGHT_END_H of date D+1
# Attribute the night to date D (the date when sleep starts).

night_precip <- hourly |>
  mutate(
    hour     = as.integer(format(ts, "%H")),
    is_night = hour >= NIGHT_START_H | hour < NIGHT_END_H,
    # as.Date() on a POSIXct defaults to UTC; pass tz so the calendar date
    # is taken in local time (00:00-01:59 CEST would otherwise roll back a day).
    night_date = if_else(hour < NIGHT_END_H,
                         as.Date(ts, tz = "Europe/Belgrade") - 1,
                         as.Date(ts, tz = "Europe/Belgrade"))
  ) |>
  filter(is_night) |>
  group_by(name, km, ele, night_date) |>
  summarise(
    night_precip_mm = sum(precip, na.rm = TRUE),
    night_tmin      = min(temp, na.rm = TRUE),
    night_max_gust  = max(gust_ms, na.rm = TRUE),
    night_hours_wet = sum(precip > 0.1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  rename(date = night_date)

# Merge daily + night
combined <- daily |>
  left_join(night_precip, by = c("name", "km", "ele", "date"))

# Save raw JSON for downstream use (Quarto report etc.)
write_json(
  list(
    generated_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    waypoints     = waypoints,
    forecast      = combined
  ),
  OUT_JSON, auto_unbox = TRUE, pretty = TRUE
)
cat("Wrote", OUT_JSON, "(", nrow(combined), "rows )\n")

# ── WMO weather code → emoji ──────────────────────────────────────────────────

wcode_emoji <- function(code) {
  case_when(
    code %in% 0:1   ~ "☀️ Clear",
    code %in% 2:3   ~ "⛅ Cloudy",
    code %in% 45:48 ~ "🌫️ Fog",
    code %in% 51:57 ~ "🌦️ Drizzle",
    code %in% 61:67 ~ "🌧️ Rain",
    code %in% 71:77 ~ "🌨️ Snow",
    code %in% 80:82 ~ "🌧️ Showers",
    code %in% 85:86 ~ "🌨️ Snow showers",
    code %in% 95:99 ~ "⛈️ T-storm",
    TRUE            ~ as.character(code)
  )
}

alt_marker <- function(ele) ifelse(ele >= HIGH_ALT_M, "⛰️", "")

# ── Build markdown report ─────────────────────────────────────────────────────

race_dates     <- seq(as.Date(RACE_START), as.Date(RACE_END), by = "day")
race_rows      <- combined |> filter(date %in% race_dates) |> arrange(date, km)
days_to_start  <- as.integer(as.Date(RACE_START) - Sys.Date())

md <- c(
  "# Trans Balkan Race 2026 — Live Weather Forecast",
  "",
  sprintf("> Generated **%s** · Source [Open-Meteo](https://open-meteo.com) (16-day forecast, hourly resolution)",
          format(Sys.time(), "%Y-%m-%d %H:%M %Z", tz = "UTC")),
  "",
  sprintf("**Race window:** %s → %s · **Days to start:** %d",
          format(RACE_START, "%a %d %b %Y"),
          format(RACE_END,   "%a %d %b %Y"),
          days_to_start),
  "",
  sprintf("⛰️ = waypoint above %d m elevation (mountain conditions can differ sharply from valley forecasts).",
          HIGH_ALT_M),
  ""
)

if (nrow(race_rows) == 0) {
  first_visible <- as.Date(RACE_START) - 15
  md <- c(md,
    "## ⏳ Race not yet within Open-Meteo's 16-day forecast window",
    "",
    sprintf("First race day enters the forecast window on **%s**.",
            format(first_visible, "%a %d %b %Y")),
    "",
    "Until then, see climatology in [`weather_outlook.md`](weather_outlook.md).",
    "",
    "## Waypoints monitored",
    "",
    "| km | Waypoint | Elevation (m) |",
    "|---:|----------|--------------:|"
  )
  for (i in seq_len(nrow(waypoints))) {
    w <- waypoints[i, ]
    md <- c(md, sprintf("| %d | %s %s | %d |", w$km, w$name, alt_marker(w$ele), w$ele))
  }
} else {
  for (d in sort(unique(race_rows$date))) {
    day <- race_rows |> filter(date == d) |> arrange(km)
    md <- c(md,
      "",
      sprintf("## %s", format(as.Date(d, origin = "1970-01-01"), "%a %d %b %Y")),
      "",
      "| km | Waypoint | Elev m | Min °C | Max °C | Day rain mm (%) | **Night rain mm** | Night low °C | Wind / Gust m/s | Conditions |",
      "|---:|----------|-------:|-------:|-------:|----------------:|------------------:|-------------:|----------------:|------------|"
    )
    for (i in seq_len(nrow(day))) {
      w <- day[i, ]
      night_rain  <- ifelse(is.na(w$night_precip_mm), "—",
                            sprintf("**%.1f** (%dh wet)", w$night_precip_mm, w$night_hours_wet))
      night_low   <- ifelse(is.na(w$night_tmin), "—", sprintf("%.0f", w$night_tmin))
      md <- c(md, sprintf(
        "| %d | %s %s | %d | %.0f | %.0f | %.1f (%.0f) | %s | %s | %.0f / %.0f | %s |",
        w$km, w$name, alt_marker(w$ele), w$ele,
        w$tmin, w$tmax,
        w$precip_mm, w$precip_pct,
        night_rain, night_low,
        w$wind_ms, w$gust_ms,
        wcode_emoji(w$wcode)
      ))
    }
  }

  # ── High-altitude alert section ─────────────────────────────────────────────
  high <- race_rows |> filter(ele >= HIGH_ALT_M)
  if (nrow(high) > 0) {
    md <- c(md,
      "",
      sprintf("## ⛰️ High-altitude conditions (>%d m) — race window", HIGH_ALT_M),
      "",
      "Worst-case picks per waypoint across the race window — these are the points to watch for hypothermia, snow/sleet, or thunderstorms.",
      "",
      "| Waypoint | Elev m | Min temp °C | Max precip mm/day | Max gust m/s | Total night rain mm |",
      "|----------|-------:|------------:|------------------:|-------------:|--------------------:|"
    )
    summary_high <- high |>
      group_by(name, km, ele) |>
      summarise(
        min_temp        = min(tmin, na.rm = TRUE),
        max_day_precip  = max(precip_mm, na.rm = TRUE),
        max_gust        = max(gust_ms, na.rm = TRUE),
        total_night_rain = sum(night_precip_mm, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(km)
    for (i in seq_len(nrow(summary_high))) {
      s <- summary_high[i, ]
      md <- c(md, sprintf(
        "| %s | %d | %.0f | %.1f | %.0f | %.1f |",
        s$name, s$ele, s$min_temp, s$max_day_precip, s$max_gust, s$total_night_rain
      ))
    }
  }

  # ── Night-rain alert summary ────────────────────────────────────────────────
  night_alert <- race_rows |>
    filter(!is.na(night_precip_mm), night_precip_mm >= NIGHT_RAIN_ALERT_MM) |>
    arrange(date, km)
  if (nrow(night_alert) > 0) {
    md <- c(md,
      "",
      sprintf("## 🌧️ Nights with rain ≥ %.0f mm", NIGHT_RAIN_ALERT_MM),
      "",
      sprintf("Worth knowing for bivvy strategy and sleep-system choice. Threshold tunable via `NIGHT_RAIN_ALERT_MM` in `weather_forecast.R`."),
      "",
      "| Night of | km | Waypoint | Night rain mm | Wet hours | Night low °C |",
      "|----------|---:|----------|--------------:|----------:|-------------:|"
    )
    for (i in seq_len(nrow(night_alert))) {
      w <- night_alert[i, ]
      md <- c(md, sprintf(
        "| %s | %d | %s %s | %.1f | %d | %.0f |",
        format(as.Date(w$date, origin = "1970-01-01"), "%a %d %b"),
        w$km, w$name, alt_marker(w$ele),
        w$night_precip_mm, w$night_hours_wet, w$night_tmin
      ))
    }
  }
}

md <- c(md,
  "",
  "---",
  "",
  "Updated daily by [`weather-forecast.yml`](.github/workflows/weather-forecast.yml) · ",
  "Generator [`weather_forecast.R`](weather_forecast.R) · ",
  "Raw data [`weather_forecast.json`](weather_forecast.json) · ",
  "Climatology baseline [`weather_outlook.md`](weather_outlook.md)",
  ""
)

writeLines(md, OUT_MD)
cat("Wrote", OUT_MD, "\n")
