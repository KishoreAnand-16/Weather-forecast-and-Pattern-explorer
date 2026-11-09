# Statistical Weather Analysis

An interactive **R Shiny** app that combines a **live weather dashboard** with **statistical analysis** of historical weather data.

- **Tab 1** shows current conditions and a 7-day forecast for any city in the world, using live data.
- **Tabs 2–4** analyse the Kaggle *Szeged Weather History* dataset (`weatherHistory.csv`) with descriptive statistics, probability, correlation/regression and hypothesis testing.

![Tab 1 – Weather Dashboard](tab1_preview.png)

---

## Contents

1. [Features](#features)
2. [Requirements](#requirements)
3. [Installation](#installation)
4. [Running the app](#running-the-app)
5. [Dataset](#dataset)
6. [How each tab works](#how-each-tab-works)
7. [Code structure](#code-structure)
8. [Troubleshooting](#troubleshooting)
9. [Credits](#credits)
10. [Team](#team)

---

## Features

| Tab | Name | What it does | Data source |
|---|---|---|---|
| 1 | **Weather Dashboard** | Live current conditions, city search with autocomplete, 24-hour outlook, 7-day forecast, automatic insights | Open-Meteo API (live) |
| 2 | **Descriptive Statistics** | Summary statistics, histogram with density and normal curve, monthly box plots, percentiles, threshold probability, IQR outliers | `weatherHistory.csv` |
| 3 | **Correlation and Regression** | Relationships between weather variables using correlation and regression models | `weatherHistory.csv` |
| 4 | **Statistical Testing** | Welch's two-sample t-test comparing mean daily temperature between two months | `weatherHistory.csv` |

---

## Requirements

- **R** 4.0 or newer (4.2+ recommended on Windows for full emoji/UTF-8 support)
- **RStudio** (optional, but the easiest way to run the app)
- An **internet connection** for Tab 1. Tabs 2–4 work offline.

### R packages

| Package | Used for |
|---|---|
| `shiny` | Web app framework |
| `ggplot2` | All charts |
| `jsonlite` | Reading live weather data in Tab 1 |

---

## Installation

Open R or RStudio and run once:

```r
install.packages(c("shiny", "ggplot2", "jsonlite"))
```

No API key or sign-up is needed. Open-Meteo is free for non-commercial use.

---

## Running the app

**Option A – RStudio:** open `app.R` and click **Run App**.

**Option B – R console:**

```r
shiny::runApp("path/to/folder/containing/app.R")
```

The app opens in your browser. Tab 1 loads live weather for **Szeged, Hungary** by default (the city in the dataset). For Tabs 2–4, upload `weatherHistory.csv` using the file picker.

---

## Dataset

**Szeged Weather History (2006–2016)** – Kaggle, hourly observations (~96,000 rows).

Columns used by the app:

| Column | Description |
|---|---|
| `Formatted Date` | Timestamp, e.g. `2006-04-01 00:00:00.000 +0200` |
| `Temperature (C)` | Air temperature |
| `Apparent Temperature (C)` | "Feels like" temperature |
| `Humidity` | Relative humidity (0–1) |
| `Wind Speed (km/h)` | Wind speed |
| `Visibility (km)` | Visibility |
| `Pressure (millibars)` | Air pressure |
| `Summary` | Text description (e.g. "Partly Cloudy") |

> The file is ~16 MB. The app allows uploads up to 100 MB (`options(shiny.maxRequestSize = 100 * 1024^2)`).
> A file uploaded in Tab 4 is reused by Tab 2 automatically.

---

## How each tab works

### Tab 1 – Weather Dashboard (live)

**Search**
- Type 2 or more letters and suggestions appear for **any city, town or village worldwide** (country flag, district, state).
- Use ↑ ↓ and Enter, or click a suggestion.
- Quick buttons cover common cities (Szeged, Erode, Coimbatore, Chennai, Bengaluru, Mumbai, Delhi, London, New York, Tokyo, Dubai).
- **📍 My location** uses the browser's location.
- If several places share a name, a **"Not the right place?"** dropdown lets you choose.

**Current conditions card**
- Temperature, condition, "feels like" with a reason (humidity or wind chill), today's high and low, and local time with time zone.
- The background changes with the weather: animated rain, snow, fog, storm flashes, and stars at night.

**Detail tiles**
- Humidity with dew point
- Wind with a compass arrow, gusts and direction
- Pressure and its 3-hour trend (rising, falling or steady)
- UV index with risk level
- Visibility and cloud cover
- Air quality (US AQI, PM2.5)
- Rain today with chance of rain
- Sunrise/sunset arc showing the sun's current position and day length

**Next 24 hours**
- An hour-by-hour strip with icons, temperature and rain chance.
- A chart of temperature (line) with rain-chance bars on a second axis.

**7-day forecast**
- Each day's low-to-high range is drawn as a coloured bar on a shared weekly scale, so days can be compared at a glance.
- A dot on today's row marks the current temperature.

**Week at a glance**
- A min/max temperature band chart.
- Automatic insights:
  - warmest day and coldest night
  - **7-day trend**: the slope of a linear regression (`lm`) fitted to the daily highs
  - total rain and the wettest day
  - average day–night temperature swing
  - "best day outdoors", scored on comfort temperature (~21 °C), rain chance and wind

**Other controls**
- °C/km/h ↔ °F/mph unit switch
- Refresh button; data also refreshes automatically every 15 minutes
- A friendly error card if the internet is unavailable

**APIs used** (all free, no key):
- Forecast: `api.open-meteo.com/v1/forecast`
- City search: `geocoding-api.open-meteo.com/v1/search`
- Air quality: `air-quality-api.open-meteo.com/v1/air-quality`

Weather conditions come as **WMO weather codes** (0 = clear, 61 = rain, 95 = thunderstorm, …). The app maps these codes to text, icons and colour themes.

### Tab 2 – Descriptive Statistics
Choose a variable to see:
- n, mean, median, SD, variance, quartiles, IQR and skewness
- a histogram with a density curve and a fitted normal curve
- box plots by month and a percentile table
- **threshold probability**, comparing the observed (empirical) value with the normal model (`pnorm`)
- outliers by the **1.5 × IQR rule**
- an automatic written interpretation

Placeholder zeros in Humidity and Pressure are removed before analysis.

### Tab 3 – Correlation and Regression
- Measures how strongly weather variables move together (for example temperature, humidity, pressure and wind speed) using **correlation**.
- Fits **regression** models to describe and predict one variable from others.
- Shows the results as charts and tables, with an interpretation of the relationships.

### Tab 4 – Statistical Testing
1. Hourly data is averaged into **daily mean temperatures**.
2. Pick a year and two months.
3. **Welch's two-sample t-test** (two-sided, α = 0.05) is run. The tab shows:
   - the hypotheses
   - sample statistics
   - box plot with jitter
   - t, df, p-value and 95% CI
   - an automatic reject / fail-to-reject decision

> Note: daily temperatures are autocorrelated, so the independence assumption of the t-test is only approximately met.

---

## Code structure

Everything is in a single file, `app.R`:

```
app.R
├── Libraries & options
├── TAB 1 helpers          (t1_*  : API calls, WMO code map, units, CSS, JS, t1_ui())
├── UI                     fluidPage → tabsetPanel with 4 tabs
├── SERVER
│   ├── t1_server(...)     Tab 1 – live dashboard
│   ├── t2_* reactives     Tab 2 – descriptive statistics
│   ├── Tab 3 reactives    Tab 3 – correlation and regression
│   └── Tab 4 reactives    raw_data → daily_data → analysis (t-test)
└── shinyApp(ui, server)
```

**Naming convention:** every input, output and helper starts with its tab prefix (`t1_`, `t2_`, …), so team members' code never clashes. Tab 1's CSS is scoped under `.t1-wrap`, so it doesn't change the look of other tabs.

> `jsonlite` is called as `jsonlite::fromJSON()` instead of `library(jsonlite)`, because jsonlite has its own `validate()` that would mask `shiny::validate()` used in Tab 4.

---

## Troubleshooting

| Problem | Fix |
|---|---|
| Tab 1 shows "Weather unavailable" | Check your internet connection or firewall/proxy, then press **Refresh**. |
| City suggestions don't appear | Type at least 2 letters. If you're offline, typing a name and pressing **Search** still shows an error message. |
| `there is no package called 'jsonlite'` | Run `install.packages("jsonlite")`. |
| Emojis show as boxes or `<U+…>` | Use R 4.2+ and a modern browser (Chrome, Edge, Firefox). |
| "My location" does nothing | Allow location access in the browser. It works on `localhost` or HTTPS only. |
| Upload fails / file too large | The limit is 100 MB. Make sure the file is the Kaggle `weatherHistory.csv`. |
| Tab 4 says required columns are missing | The CSV must contain `Formatted Date` and `Temperature (C)`. |

---

## Credits

- **Live weather data:** [Open-Meteo](https://open-meteo.com) (CC BY 4.0)
- **Historical dataset:** Szeged Weather History, Kaggle
## Team

| Tab | Section | Member |
|---|---|---|
| 1 | Weather Dashboard | _Shree Magathi_ |
| 2 | Descriptive Statistics | _Kishore Anand_ |
| 3 | Correlation & Regression | _Bhavana Shrii_ |
| 4 | Statistical Testing | _Aswini_ |
