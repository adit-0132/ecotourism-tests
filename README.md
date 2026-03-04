# Wildlife Spotter — Shiny App

An interactive Shiny app for exploring Australian wildlife sightings, built as part of the **GSoC 2026 selection test** for the `ecotourism` R package project.

---

## Overview

The app helps users discover the **best locations and times to spot wildlife across Australia** by combining three datasets from the `ecotourism` package: occurrence records, daily weather observations, and quarterly tourism data.

---

## Installation

```r
# Install dependencies
install.packages(c("shiny", "bslib", "leaflet", "dplyr", "ggplot2", "lubridate"))

# Install the ecotourism package
install.packages("ecotourism")
```

Then run the app:

```r
shiny::runApp("app.R")
```

---

## Features

### Sidebar controls
- **Organism selector** — switch between Manta Ray, Gouldian Finch, Glowworms, and Orchids
- **Month filter** — view sightings for a specific month or all year
- **Record type filter** — filter by observation method (human observation, preserved specimen, machine observation, material sample)

### Map panel
- Interactive Leaflet map centred on Australia, panning and zoom restricted to Australian bounds
- Green dot markers for each sighting, with popups showing date, record type, and weather station ID
- Download button exports the current filtered sightings as a CSV (date, lat, lon, record type, station)

### Overview panel
- Total sightings and years of data for the current filter selection, updated reactively

### Best Time to Visit
- Computes the top 3 peak sighting months from the full (unfiltered) occurrence dataset
- Displays typical temperature and rainfall during those peak months, derived by joining occurrence data with the `weather` table on `ws_id` and `date`

### Charts (all downloadable as PNG)
- **Sightings by Month** — bar chart with peak months highlighted in dark green, selected month highlighted in amber
- **Weather at Sighting Time** — boxplot of temperature distribution across months for records that have matched weather data
- **Sightings Over the Years** — area + line trend chart showing annual sighting counts from 2014 to 2024

---

## Code Architecture

The app is a single `app.R` file structured in four sections.

**Data loading** runs once at startup. All four organism datasets, weather, and weather station metadata are loaded explicitly with `data(..., package = "ecotourism")`.

**Reactive layer** uses three reactive expressions that form a pipeline:

```
base_data()        organism dataset (full, unfiltered)
    |
filtered()         filtered by month + record type
    |
with_weather()     filtered data joined to weather on ws_id + date
```

`peak_months()` is a separate reactive that always operates on `base_data()` — not `filtered()` — so the best time recommendation reflects the organism's true seasonal pattern regardless of what the user has filtered to.

**Plot builders** (`build_month_plot`, `build_weather_plot`, `build_year_plot`) are plain functions defined inside the server, not reactive expressions. This allows them to be called both by `renderPlot()` for display and by `downloadHandler()` for export, without duplicating code.

**Download handlers** use a `dl_plot()` factory function that wraps `downloadHandler` + `ggsave`, keeping the three chart download definitions to one line each. The map download exports a CSV rather than an image, since capturing a Leaflet map state as a PNG requires a headless browser.

---

## Design

The UI uses `bslib` with Bootstrap 5 and a custom nature/ecology theme built on:

- Background: warm cream (`#F5F0E8`) with subtle radial gradient texture
- Sidebar: deep forest green (`#1C2B1A`) with light green labels
- Accent: leaf green (`#7AB648`) for markers, badges, and highlights
- Typography: Playfair Display (headings) + Lora (body) from Google Fonts

Cards use a `card_with_dl()` helper function that injects a download button into the card header, keeping the UI layout DRY.

---

## Data notes

The `weather` table in the `ecotourism` package covers a curated subset of weather stations (via `top_stations`) and does not overlap completely with all occurrence station IDs. Weather-dependent panels will show partial data for some organism/month combinations — this reflects real-world data sparsity and is surfaced honestly in the app rather than silently dropped.

---

## Deployment

The app can be deployed to shinyapps.io:

```r
install.packages("rsconnect")
rsconnect::deployApp("path/to/app.R")
```

---

## Package

`ecotourism` v0.0.0.9000 — GitHub: [vahdatjavad/ecotourism](https://github.com/vahdatjavad/ecotourism)  
Data source: Atlas of Living Australia, 2014-2024
