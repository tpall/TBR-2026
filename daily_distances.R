source("tbr_plan.R")

# ── Output ────────────────────────────────────────────────────────────────────

print_totals <- function(track, segments) {
  cat(sprintf("Track points: %d\n", nrow(track)))
  cat(sprintf("\nTotal distance : %.0f km\n",  sum(segments$dist_km)))
  cat(sprintf("Total climb    : %.0f m\n",    sum(segments$gain_m)))
  cat(sprintf("Total descent  : %.0f m\n",    sum(pmax(-segments$ele_diff, 0))))
  cat(sprintf("Effective time : %.1f h\n\n",  sum(segments$time_h)))
}

print_finish <- function(total_days, finish_wall) {
  cat(sprintf("Days (sleep 02:00 / max %.0fm climb/day) : %.1f\n", MAX_DAILY_CLIMB, total_days))
  cat(sprintf("Estimated finish : %s\n\n", format(finish_wall, "%a %d %b %Y %H:%M %Z")))
}

print_daily_table <- function(day_info, total_dist, checkpoints) {
  rule <- strrep("─", 84)
  cat(rule, "\n")
  cat(sprintf("%-28s  %5s  %5s  %7s  %8s  %10s  %s\n",
              "Day", "ride h", "sleep", "km/day", "climb m", "cum km", "Checkpoints"))
  cat(rule, "\n")

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
  cat(rule, "\n")
}

print_resupply <- function(resupply) {
  cat(sprintf("\nResupply schedule (sleep %02.0f:00 · %.1f h · wake %02.0f:%02.0f · max %.0f m/day)\n",
              SLEEP_HOUR, SLEEP_DURATION,
              floor(SLEEP_HOUR + SLEEP_DURATION),
              round(((SLEEP_HOUR + SLEEP_DURATION) %% 1) * 60),
              MAX_DAILY_CLIMB))
  cat(sprintf("%-25s  %6s  %5s  %5s  %-14s  %-5s  %s\n",
              "Location", "km", "+km", "+h", "Type", "Day", "Est. arrival (CEST)"))
  cat(strrep("─", 85), "\n")

  for (i in seq_len(nrow(resupply))) {
    r   <- resupply[i, ]
    off <- if (!r$on_route) " *" else "  "
    cat(sprintf("%-25s  %6.0f  %5.0f  %5.1f  %-14s  Day%-2d  %s%s\n",
                r$location, r$km, r$gap_km, r$gap_h, r$type, r$day_num,
                format(r$arrival, "%a %d %b %H:%M"), off))
  }
  cat(strrep("─", 76), "\n")
  cat("* off-route stop\n\n")
}

build_plot <- function(segments, day_info, checkpoints, total_dist, total_climb, total_days) {
  elev_profile <- segments |>
    select(cum_dist, ele, cum_time) |>
    mutate(day = factor(pmin(
      findInterval(cum_time, day_info$cum_riding) + 1L, nrow(day_info)
    )))

  day_lines <- day_info |>
    filter(cum_dist < total_dist) |>
    mutate(label = sprintf("Day %d\n%.0f km", day + 1, cum_dist))

  ggplot(elev_profile, aes(cum_dist, ele)) +
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
}

# ── Main ──────────────────────────────────────────────────────────────────────

track    <- load_track(GPX_FILE)
segments <- compute_segments(track)
print_totals(track, segments)

day_info    <- simulate_days(segments, RACE_START)
total_dist  <- sum(segments$dist_km)
total_climb <- sum(segments$gain_m)
total_time  <- sum(segments$time_h)

total_days <- nrow(day_info) - 1 +
  (total_time - day_info$cum_riding[nrow(day_info) - 1]) /
  day_info$riding_h[nrow(day_info)]

finish_wall <- day_info$day_start_wall[nrow(day_info)] +
  as.difftime(
    total_time - c(0, day_info$cum_riding)[nrow(day_info)],
    units = "hours"
  )

print_finish(total_days, finish_wall)
print_daily_table(day_info, total_dist, CHECKPOINTS)

resupply <- build_resupply(RESUPPLY, segments, day_info)
print_resupply(resupply)

p <- build_plot(segments, day_info, CHECKPOINTS, total_dist, total_climb, total_days)
ggsave("daily_distances.png", p, width = 14, height = 5, dpi = 150)
cat("Plot saved to daily_distances.png\n")
