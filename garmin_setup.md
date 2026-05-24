# Garmin Edge 1040 — TBR 2026 setup

Dedicated activity profile + power strategy for a multi-day, battery-saver, offline race with HR strap + power meter.

---

## Profile basics

- **Activity type:** Touring (longer-file defaults & navigation-first layout — better than Road for this use)
- **Profile name:** `TBR Race`
- **Icon/color:** distinct from your training profiles so you don't pick the wrong one at the start

---

## Sensors

| Sensor | Role | Notes |
|---|---|---|
| HRM (chest strap) | HR primary | Pair before the start; carry spare CR2032 |
| Power meter | Power + cadence primary | Set ANT+ & BLE; calibrate every morning |
| GPS | Speed + distance | No speed sensor needed |

Wake the Edge at the start line with both sensors connected — re-pairing mid-race wastes battery.

---

## Data pages (4 total — fewer = less screen draw)

> Garmin layout codes: `N` = number of fields, `A/B/C` = field-size arrangement. Pick the variant matching the layout described.

**Page 1 — Primary glance** · Layout **8A** (2 large top / 2 medium middle / 4 small bottom)
- Large: **Time of day** · **Distance**
- Medium: **HR** · **Power 3-sec avg**
- Small: **Cadence** · **Grade** · **Elevation** · **Battery %**

**Page 2 — Map** · Layout: full-screen map, track-up

**Page 3 — Day & forecast** · Layout **6A** (2 large + 4 small)
- Large: **Sunset time** · **Distance to Next** *(the act-on-now fields — "Distance to Next" = next course point; do not pick "Distance to Point", which is for saved-Point navigation only)*
- Small: **Total elapsed time** · **Total ascent** · **Avg speed** · **Total Work (kJ)** *(fueling proxy — 1 kJ ≈ 1 kcal for cycling; more reliable than the Calories estimator)*

**Page 4 — ClimbPro** · auto-triggered
Default top section (already shown): distance to go · ascent remaining · grade remaining · grade graph.
**Customize the 2 lower fields:** **HR** · **Power 3-sec avg** — mirror Page 1 so you don't lose your pacing references when ClimbPro takes over the screen.

**Delete from the profile:** Lap page, Training/performance pages, all Connect IQ data pages. Every wasted draw costs battery.

---

## Alerts

- **Power high:** ceiling at **90% FTP ≈ 215 W**. Power discipline matters more than HR in an ultra — HR drifts up across days at the same power, so a fixed bpm threshold gets less informative each day.
- **HR high (optional):** top of Z4 ≈ **149 bpm** (your zones: top-Z3 = 132, top-Z4 ≈ 149). Catches genuine red-line, not every climb.
- **Eat alert:** every 20 min
- **Drink alert:** every 20 min
- **Disable:** lap alerts, segment alerts, phone notifications

*(Off-course warnings are not in the Alerts menu — they live under Navigation → Course; see next section.)*

---

## Recording & auto-features

All per-activity settings under **Settings → Activity Profiles → TBR Race → [submenu]**:

| Setting | Value | Likely submenu | Why |
|---|---|---|---|
| Recording interval | **1-second** | Data Recording *(may be system-level: Settings → System → Data Recording)* | Cleaner power data; FIT files stay reasonable when split per day |
| Auto Pause | **OFF** | Timer (or Auto features) | Race clock is running — total elapsed = race elapsed, keeps sleep math correct |
| Auto Lap | OFF | Laps (or Auto features) | Use manual lap at checkpoints if you want them tagged |
| Auto Scroll | OFF | Data Screens | You get the field you expect when you wake the screen |
| Start Notice | OFF | Notifications (or Activity Settings) | No prompt at start line |

*(Auto-sleep is covered by Auto Off in the System section below — single setting on Edge 1040.)*

**Save & start a new activity every morning.** Do **not** run a single 10-day FIT file — large files can corrupt or fail to upload. Stop on wake, save, start fresh. Loading the same course continues navigation from your current position.

---

## Navigation — strict GPX following, no rerouting

- **Load** `Trans Balkan Race 2026 __ Issue 1.GPX` as a Course (USB-C → drop into `/Garmin/NewFiles/`)
- **Pre-load** TopoActive Europe map region — verify before departure

**Settings → Activity Profiles → TBR Race → Navigation:**

**→ Map** *(confirmed location)*
- **Orientation:** Track up
- **Auto-zoom:** ON

**→ Routing** *(confirmed location)*
- **Recalculation:** **OFF** ← the main no-reroute setting (don't auto-route back when off-course)
- **Calculation method:** *Minimize Distance* (irrelevant when recalc is off, set cleanly)
- **Avoidance setup:** leave default — won't fire without recalc

**→ Navigation prompts** *(confirmed: only "Text only" / "Map" options)*
- **Text only** — shows the turn cue as a text banner instead of redrawing the full map view. Less screen work, kinder to battery.

**Off-course alert** — automatic on Edge 1040 when a course is active. Fires (beep + banner) when you drift off the line. No user toggle needed.

**Turn guidance** — built into the loaded course; cues fire automatically at course-point turns.

**Climb Detection** (under Activity Profile):
- **Category threshold:** **Cat 2** — fires ClimbPro on HC + Cat 1 + Cat 2 climbs. Filters out Cat 3/4/Uncategorized so the screen stays calm on rolling sections.
- **Terrain type:** **Mixed** — TBR has both substantial tarmac and unpaved sections. Paved would undercategorize the off-road climbs; Unpaved would overcategorize the long tarmac stretches. Mixed splits the difference.
- Category cheat sheet: HC ≈ 1000 m+ gain · Cat 1 ≈ 600–1000 m · Cat 2 ≈ 300–600 m · Cat 3 ≈ 150–300 m · Cat 4 ≈ 80–150 m

Result: device follows the line, beeps if you drift, shuts up otherwise. You manage course corrections.

---

## System / power — the battery game

| Setting | Value | Menu |
|---|---|---|
| GPS mode | **GPS only** (skip Multi-band — saves ~30%) | Activity Profile → Positioning |
| Battery Saver | ON, auto-engage at 20% | Settings → System → Battery Saver *(also via status bar swipe-down)* |
| Backlight brightness | Lowest | Settings → System → Display → Backlight |
| Backlight timeout | 8–10 sec | Settings → System → Display → Backlight |
| Color mode | Dark | Settings → System → Display → Color Mode |
| Auto Off (idle power-off) | 15 min | *location unclear on this firmware — check System → Display sleep options, or may not be user-configurable* |
| Tones | **ON** *(only on/off toggle on this device — no volume slider)* | Settings → System → Sounds → Tones |
| Phone connection | **OFF** during ride; toggle on only at CPs to sync | Settings → Connected Features → Phone |
| Smart notifications | OFF | Settings → Connected Features → Phone → Smart Notifications |
| Wi-Fi | OFF | Settings → Connected Features → Wi-Fi |
| Bluetooth | tied to Phone connection — no separate toggle | (disabled via Phone toggle above) |
| LiveTrack Auto Start | **OFF** *(TBR uses a separate dot tracker; LiveTrack just drains your phone)* | Settings → Safety & Tracking → LiveTrack → Auto Start |

Toggle Multi-band GPS on **only** in deep canyon sections if you see drift. Off again as soon as you're out.

---

## Power strategy — 20k mAh bank, no dynamo

**Capacity math:** 20,000 mAh nominal ≈ ~13,000 mAh usable at 5V after conversion. Roughly:
- ~6 full Edge 1040 recharges (1,900 mAh each), or
- ~3 full smartphone recharges, or
- some mix + lights topping

Edge in battery-saver, GPS-only, screen-off = **~60–70 h** real-world per charge. You won't need a top-up until after sleep #2 in most plausible race schedules. The bank is a buffer, not the primary power source.

### Charging priorities (in order)

1. **Wall power first, always.** At every indoor stop (café, hotel, petrol station with a socket), plug the **bank** in first, then your devices. Bank takes 2–3 h to refill with PD; longer without — start it the second you sit down.
2. **Edge on the bike, off the bank, only when needed.** Don't burn bank cycles on a device sitting at 50%.
3. **Phone is the real consumer.** Weather, photos, dot-tracker app, backup nav — drains hard. Aggressive airplane mode + screen off between uses.

### On-bike Edge charging

- **Short USB-C cable** (10–15 cm or coiled) — long cables flap and unseat
- Edge 1040 supports **pass-through while recording** — just plug in, no menu action
- Top-tube bag with side cable port — bank inside, cable exits to mount
- **Never open the port cover in rain.** USB-C corrosion kills devices. Wait it out.

### Your hardware (confirmed)

- **Nitecore NB20000** — 20k mAh / 72 Wh, 45 W PD in/out, USB-C + USB-A out, 333 g, passthrough ✓
- **20 W dual-port wall charger** (USB-C + USB-A) — bank's 45 W input is bottlenecked here, so **refill takes ~3.5 h instead of ~2 h**. Fine for sleep stops; tight for short refuels.

### Also carry

- 2× USB-C cables (1 short for bike, 1 longer for hotel)
- 1× USB-A → USB-C cable — lets the wall's USB-A port charge a second device while the bank fills via USB-C
- Spare CR2032 for the HR strap (not rechargeable — dies silently)

### Workflow at stops

**Overnight (4+ h):**
1. Wall USB-C → Bank USB-C *(20 W; refills in ~3.5 h)*
2. Wall USB-A → Phone or Edge *(via USB-A→USB-C cable, ~12 W)*

**Short refuel (<1 h):** skip topping the bank — plug devices directly to wall. Bank gets the next long stop.

Bottom line: 20k is comfortable for this race profile. The discipline that actually matters is **plugging the bank into a wall every time you stop indoors**, not the bank size.

---

## Pre-departure checklist

- [ ] Profile built and field-tested on a 1-hour ride this week
- [ ] Course loaded and previewed end-to-end on the device
- [ ] TopoActive Europe map region installed (SE Europe quadrant)
- [ ] HR strap battery fresh; spare CR2032 in repair kit
- [ ] Power meter calibrated and battery topped
- [ ] Battery Saver toggle accessible (test the menu path under stress)
- [ ] All training/Connect IQ profiles set to *not* auto-launch
- [ ] Recalculation = OFF verified
- [ ] Firmware updated — but **at least 5 days before** departure (never on race week)
- [ ] Power bank fully charged; cables & wall charger packed
