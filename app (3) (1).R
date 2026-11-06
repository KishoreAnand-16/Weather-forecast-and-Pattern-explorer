# Initialize the application and load required packages
options(shiny.maxRequestSize = 100 * 1024^2)

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(broom)
library(ggcorrplot)
library(curl)
library(httr)

# We use Open-Meteo for live data since it's free and requires no API key.
# We call jsonlite::fromJSON directly here to avoid conflicts with shiny::validate later in the app.

t1_cities <- c("Szeged", "Erode", "Chennai", "Mumbai", "London",
               "New York", "Tokyo", "Dubai", "Sydney")

t1_get <- function(url) {
  req <- httr::GET(url)
  jsonlite::fromJSON(httr::content(req, as = "text", encoding = "UTF-8"))
}

t1_geocode <- function(q) {
  url <- paste0("https://geocoding-api.open-meteo.com/v1/search?count=6&language=en&format=json&name=",
                utils::URLencode(q, reserved = TRUE))
  r <- t1_get(url)$results
  if (is.null(r) || NROW(r) == 0) return(NULL)
  col <- function(x) if (is.null(x)) rep("", NROW(r)) else ifelse(is.na(x), "", as.character(x))
  data.frame(name = r$name, admin1 = col(r$admin1), country = col(r$country),
             lat = r$latitude, lon = r$longitude, stringsAsFactors = FALSE)
}

t1_forecast <- function(lat, lon) {
  cur <- paste("temperature_2m,relative_humidity_2m,apparent_temperature,is_day,precipitation",
               "weather_code,cloud_cover,pressure_msl,wind_speed_10m,wind_direction_10m",
               "wind_gusts_10m,dew_point_2m,visibility", sep = ",")
  hr  <- "temperature_2m,precipitation_probability,weather_code,is_day,pressure_msl"
  dl  <- paste("weather_code,temperature_2m_max,temperature_2m_min,sunrise,sunset,uv_index_max",
               "precipitation_sum,precipitation_probability_max,wind_speed_10m_max", sep = ",")
  t1_get(sprintf(paste0("https://api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f",
                        "&current=%s&hourly=%s&daily=%s&timezone=auto&forecast_days=7"),
                 lat, lon, cur, hr, dl))
}

t1_air <- function(lat, lon) {
  t1_get(sprintf(paste0("https://air-quality-api.open-meteo.com/v1/air-quality?",
                        "latitude=%.4f&longitude=%.4f&current=us_aqi,pm2_5&timezone=auto"), lat, lon))
}

# Map standard WMO weather codes to readable text, emojis, and CSS themes
t1_wmo_txt <- c(`0` = "Clear sky", `1` = "Mainly clear", `2` = "Partly cloudy", `3` = "Overcast",
                `45` = "Fog", `48` = "Rime fog", `51` = "Light drizzle", `53` = "Drizzle",
                `55` = "Dense drizzle", `56` = "Freezing drizzle", `57` = "Freezing drizzle",
                `61` = "Light rain", `63` = "Rain", `65` = "Heavy rain", `66` = "Freezing rain",
                `67` = "Freezing rain", `71` = "Light snow", `73` = "Snow", `75` = "Heavy snow",
                `77` = "Snow grains", `80` = "Light showers", `81` = "Showers",
                `82` = "Violent showers", `85` = "Snow showers", `86` = "Heavy snow showers",
                `95` = "Thunderstorm", `96` = "Thunderstorm with hail", `99` = "Severe thunderstorm")

t1_wmo <- function(code, is_day = 1) {
  code <- suppressWarnings(as.integer(code))
  day  <- isTRUE(suppressWarnings(as.integer(is_day)) == 1)
  if (length(code) == 0 || is.na(code)) return(list(txt = "Unknown", icon = "\u2753", css = "cloudy"))
  theme <- if (code <= 1) "clear" else if (code == 2) "partly" else if (code == 3) "cloudy"
  else if (code %in% 45:48) "fog" else if (code %in% c(71:77, 85:86)) "snow"
  else if (code >= 95) "storm" else "rain"
  icon <- switch(theme,
                 clear  = if (day) "\u2600\ufe0f" else "\U0001F319",
                 partly = if (day) "\u26c5" else "\u2601\ufe0f",
                 cloudy = "\u2601\ufe0f",
                 fog    = "\U0001F32B\ufe0f",
                 snow   = if (code %in% c(85, 86)) "\U0001F328\ufe0f" else "\u2744\ufe0f",
                 storm  = "\u26c8\ufe0f",
                 rain   = if (code %in% 51:57) "\U0001F326\ufe0f" else "\U0001F327\ufe0f")
  txt <- unname(t1_wmo_txt[as.character(code)])
  if (is.na(txt)) txt <- "Unknown"
  if (code == 0 && !day) txt <- "Clear night"
  css <- if (!day && theme %in% c("clear", "partly")) "night" else theme
  list(txt = txt, icon = icon, css = css)
}

t1_nz  <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x[1])
t1_f   <- function(x, unit = "\u00b0") if (is.na(x)) "\u2013" else paste0(round(x), unit)
t1_pt  <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M", tz = "UTC")
t1_dir <- function(deg) {
  if (is.na(deg)) return("")
  c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW",
    "W", "WNW", "NW", "NNW")[(round(deg / 22.5) %% 16) + 1]
}

# Generate a color gradient for temperatures (blue for cold, red for hot)
t1_tcol <- function(c) {
  pal <- grDevices::colorRampPalette(c("#60a5fa", "#22d3ee", "#34d399", "#facc15", "#fb923c", "#ef4444"))(51)
  pal[pmin(51, pmax(1, round((c + 10) / 50 * 50) + 1))]
}

t1_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(text = element_text(colour = "#cbd5e1"),
          axis.text = element_text(colour = "#94a3b8"),
          panel.grid.major = element_line(colour = "#FFFFFF14"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
}

# Main dashboard styling
t1_css <- r"---(
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;800&display=swap');
.t1-wrap{font-family:Inter,system-ui,-apple-system,Segoe UI,sans-serif;color:#e8ecf8;margin:16px 0 30px;padding:22px;border-radius:24px;
  background:radial-gradient(900px 500px at 0% -10%,rgba(59,130,246,.35),transparent 60%),radial-gradient(700px 500px at 100% 0%,rgba(168,85,247,.25),transparent 60%),linear-gradient(160deg,#0b1020,#111a33 60%,#0b1020);}
.t1-header{display:flex;justify-content:space-between;align-items:flex-end;margin-bottom:16px}
.t1-title{font-size:28px;font-weight:800;letter-spacing:-.5px;background:linear-gradient(90deg,#fff,#93c5fd);-webkit-background-clip:text;background-clip:text;color:transparent}
.t1-kicker{color:#94a3b8;font-size:13px}
.t1-bar{display:flex;flex-wrap:wrap;gap:10px;align-items:center}
.t1-search{flex:1 1 320px;display:flex;align-items:center;gap:6px;background:rgba(255,255,255,.07);border:1px solid rgba(255,255,255,.14);border-radius:14px;padding:4px 4px 4px 14px;transition:border-color .2s}
.t1-search:focus-within{border-color:#60a5fa;box-shadow:0 0 0 3px rgba(96,165,250,.2)}
.t1-search .form-group{margin:0;flex:1}
.t1-search input.form-control{background:transparent;border:none;color:#fff;box-shadow:none;height:38px;font-size:15px;padding-left:4px}
.t1-search input.form-control::placeholder{color:#94a3b8}
.t1-btn,.t1-btn:focus{border:none;border-radius:11px;background:linear-gradient(135deg,#3b82f6,#8b5cf6);color:#fff;font-weight:600;padding:8px 18px}
.t1-btn:hover{filter:brightness(1.15);color:#fff}
.t1-ghost,.t1-ghost:focus{background:rgba(255,255,255,.07);border:1px solid rgba(255,255,255,.14);color:#e8ecf8;border-radius:12px;padding:9px 14px;font-weight:500}
.t1-ghost:hover{background:rgba(255,255,255,.15);color:#fff}
.t1-units .form-group{margin:0}
.t1-units .radio-inline{background:rgba(255,255,255,.07);border:1px solid rgba(255,255,255,.14);border-radius:12px;padding:8px 12px 8px 30px;margin:0 6px 0 0;color:#e8ecf8}
.t1-chips{display:flex;flex-wrap:wrap;gap:8px;margin:12px 0 4px}
.t1-chip{padding:5px 13px;border-radius:999px;background:rgba(255,255,255,.06);border:1px solid rgba(255,255,255,.1);color:#cbd5e1!important;font-size:13px;text-decoration:none!important;transition:all .2s}
.t1-chip:hover{background:rgba(96,165,250,.25);border-color:#60a5fa;color:#fff!important;transform:translateY(-1px)}
.t1-matches{display:flex;align-items:center;gap:10px;margin-top:8px;color:#94a3b8;font-size:13px}
.t1-matches .form-group{margin:0}
.t1-matches select{background:#1e293b;color:#e8ecf8;border:1px solid rgba(255,255,255,.15);border-radius:10px}
.t1-alert{margin-top:10px;padding:10px 14px;border-radius:12px;background:rgba(239,68,68,.15);border:1px solid rgba(239,68,68,.4);color:#fecaca}
.t1-top{display:grid;grid-template-columns:minmax(300px,1fr) 1.35fr;gap:16px;margin-top:16px}
.t1-bottom{display:grid;grid-template-columns:1.1fr 1fr;gap:16px}
@media (max-width:992px){.t1-top,.t1-bottom{grid-template-columns:1fr}}
.t1-top>div{display:flex;flex-direction:column}
.t1-top>div>.t1-hero{flex:1}
.t1-hero{position:relative;overflow:hidden;border-radius:22px;padding:22px 24px;min-height:330px;display:flex;flex-direction:column;justify-content:space-between;box-shadow:0 20px 50px rgba(0,0,0,.35);color:#fff}
.t1-hero>*{position:relative;z-index:1}
.t1-hero::after{content:"";position:absolute;inset:0;pointer-events:none;z-index:0}
.t1-clear{background:radial-gradient(circle at 85% 18%,rgba(253,224,71,.85),transparent 35%),linear-gradient(135deg,#0ea5e9,#2563eb 55%,#4f46e5)}
.t1-partly{background:radial-gradient(circle at 85% 18%,rgba(253,224,71,.55),transparent 32%),linear-gradient(135deg,#38bdf8,#3b82f6 50%,#64748b)}
.t1-night{background:radial-gradient(circle at 85% 18%,rgba(226,232,240,.35),transparent 25%),linear-gradient(135deg,#0f172a,#1e1b4b 60%,#312e81)}
.t1-night::after{background-image:radial-gradient(1.5px 1.5px at 20% 30%,#fff,transparent),radial-gradient(1px 1px at 40% 70%,#fff,transparent),radial-gradient(1.5px 1.5px at 65% 25%,#fff,transparent),radial-gradient(1px 1px at 10% 80%,#fff,transparent),radial-gradient(1px 1px at 55% 50%,#fff,transparent),radial-gradient(1.5px 1.5px at 30% 12%,#fff,transparent);animation:t1twinkle 4s ease-in-out infinite}
.t1-cloudy{background:linear-gradient(135deg,#64748b,#334155 60%,#1e293b)}
.t1-fog{background:linear-gradient(135deg,#94a3b8,#64748b 60%,#475569)}
.t1-fog::after{background:linear-gradient(90deg,transparent,rgba(255,255,255,.18),transparent);background-size:200% 100%;animation:t1fog 8s linear infinite}
.t1-rain{background:linear-gradient(135deg,#1e3a8a,#334155 60%,#0f172a)}
.t1-rain::after{background-image:repeating-linear-gradient(105deg,rgba(255,255,255,.16) 0 1px,transparent 1px 14px);background-size:200% 200%;animation:t1rain .8s linear infinite}
.t1-storm{background:linear-gradient(135deg,#1e1b4b,#4c1d95 55%,#0f172a);animation:t1flash 6s infinite}
.t1-storm::after{background-image:repeating-linear-gradient(105deg,rgba(255,255,255,.14) 0 1px,transparent 1px 12px);background-size:200% 200%;animation:t1rain .6s linear infinite}
.t1-snow{background:linear-gradient(135deg,#60a5fa,#818cf8 60%,#6366f1)}
.t1-snow::after{background-image:radial-gradient(circle,rgba(255,255,255,.9) 1.5px,transparent 2.5px),radial-gradient(circle,rgba(255,255,255,.6) 1px,transparent 2px);background-size:32px 32px,51px 51px;animation:t1snow 6s linear infinite}
@keyframes t1rain{to{background-position:-40px 120px}}
@keyframes t1snow{to{background-position:20px 320px,-30px 510px}}
@keyframes t1fog{to{background-position:-200% 0}}
@keyframes t1twinkle{50%{opacity:.4}}
@keyframes t1flash{0%,90%,94%,100%{filter:brightness(1)}92%{filter:brightness(1.9)}}
@keyframes t1float{50%{transform:translateY(-10px)}}
@keyframes t1pulse{0%{box-shadow:0 0 0 0 rgba(74,222,128,.7)}100%{box-shadow:0 0 0 10px rgba(74,222,128,0)}}
.t1-hero-top{display:flex;justify-content:space-between;align-items:flex-start}
.t1-loc{font-size:24px;font-weight:700}
.t1-region{font-size:13px;opacity:.85}
.t1-live{font-size:11px;font-weight:700;letter-spacing:1.5px;background:rgba(0,0,0,.25);padding:4px 10px;border-radius:999px;display:flex;align-items:center;gap:6px}
.t1-dot{width:8px;height:8px;border-radius:50%;background:#4ade80;animation:t1pulse 1.6s infinite}
.t1-hero-mid{display:flex;justify-content:space-between;align-items:center}
.t1-temp{font-size:92px;font-weight:300;line-height:1;letter-spacing:-4px}
.t1-unit{font-size:32px;vertical-align:top;letter-spacing:0;margin-left:4px;opacity:.85}
.t1-cond{font-size:20px;font-weight:600;margin-top:4px}
.t1-feel{font-size:13px;opacity:.85;margin-top:2px}
.t1-big-icon{font-size:110px;line-height:1;filter:drop-shadow(0 10px 25px rgba(0,0,0,.35));animation:t1float 5s ease-in-out infinite}
.t1-hero-foot{display:flex;flex-wrap:wrap;gap:8px}
.t1-pill{background:rgba(255,255,255,.18);backdrop-filter:blur(6px);padding:5px 12px;border-radius:999px;font-size:13px}
.t1-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(165px,1fr));gap:12px;align-content:start}
.t1-tile{background:rgba(255,255,255,.055);border:1px solid rgba(255,255,255,.09);border-radius:16px;padding:13px 15px;transition:transform .2s,background .2s}
.t1-tile:hover{transform:translateY(-3px);background:rgba(255,255,255,.09)}
.t1-wide{grid-column:span 2}
.t1-tl{font-size:11px;text-transform:uppercase;letter-spacing:1.2px;color:#94a3b8;font-weight:600}
.t1-tv{font-size:24px;font-weight:600;margin:4px 0 2px}
.t1-tv small{font-size:13px;color:#94a3b8;font-weight:400;margin-left:3px}
.t1-ts{font-size:12px;color:#94a3b8}
.t1-meter{height:6px;border-radius:6px;background:rgba(255,255,255,.1);margin:6px 0 4px;position:relative}
.t1-meter>i{position:absolute;left:0;top:0;bottom:0;border-radius:6px;background:linear-gradient(90deg,#22d3ee,#3b82f6)}
.t1-uv{background:linear-gradient(90deg,#4ade80,#facc15,#fb923c,#ef4444,#a855f7)}
.t1-uv>b{position:absolute;top:-3px;width:12px;height:12px;border-radius:50%;background:#fff;border:2px solid #0f172a;transform:translateX(-50%)}
.t1-wind{display:flex;align-items:center;gap:12px}
.t1-compass{width:44px;height:44px;border-radius:50%;border:2px solid rgba(255,255,255,.2);position:relative;flex:none}
.t1-compass:before{content:"N";position:absolute;top:-1px;left:50%;transform:translateX(-50%);font-size:9px;color:#94a3b8}
.t1-needle{position:absolute;inset:0;display:flex;align-items:center;justify-content:center;font-size:20px;color:#38bdf8;transition:transform .6s}
.t1-sunrow{display:flex;align-items:center;gap:12px}
.t1-sun{width:130px;flex:none}
.t1-card{background:rgba(255,255,255,.045);border:1px solid rgba(255,255,255,.08);border-radius:20px;padding:16px 18px;margin-top:16px}
.t1-h{font-size:12px;text-transform:uppercase;letter-spacing:1.5px;color:#94a3b8;font-weight:700;margin-bottom:10px}
.t1-strip{display:flex;gap:8px;overflow-x:auto;padding-bottom:8px;scrollbar-width:thin}
.t1-hr{flex:none;width:66px;text-align:center;padding:10px 4px;border-radius:14px;background:rgba(255,255,255,.04);border:1px solid transparent}
.t1-hr.t1-now{background:linear-gradient(180deg,rgba(59,130,246,.45),rgba(139,92,246,.35));border-color:rgba(255,255,255,.2)}
.t1-hr-t{font-size:12px;color:#cbd5e1}
.t1-hr-i{font-size:24px;margin:4px 0}
.t1-hr-v{font-weight:600}
.t1-hr-p{font-size:11px;color:#7dd3fc;min-height:15px}
.t1-day{display:grid;grid-template-columns:78px 40px 1fr 52px 44px minmax(90px,2fr) 44px;align-items:center;gap:8px;padding:9px 4px;border-bottom:1px solid rgba(255,255,255,.06)}
.t1-day:last-child{border-bottom:none}
.t1-dname{font-weight:600}
.t1-ddate{font-size:11px;color:#94a3b8;font-weight:400}
.t1-dicon{font-size:26px}
.t1-dtxt{font-size:12px;color:#cbd5e1}
.t1-dp{font-size:12px;color:#7dd3fc}
.t1-dmin{color:#94a3b8;text-align:right}
.t1-dmax{font-weight:600}
.t1-track{position:relative;height:7px;border-radius:7px;background:rgba(255,255,255,.08)}
.t1-range{position:absolute;top:0;bottom:0;border-radius:7px}
.t1-dotnow{position:absolute;top:50%;width:11px;height:11px;border-radius:50%;background:#fff;border:2px solid #0f172a;transform:translate(-50%,-50%)}
.t1-ins{display:grid;grid-template-columns:repeat(auto-fill,minmax(190px,1fr));gap:10px;margin-top:8px}
.t1-in{display:flex;gap:10px;align-items:center;background:rgba(255,255,255,.05);border-radius:14px;padding:10px 12px}
.t1-in-i{font-size:22px}
.t1-in-l{font-size:11px;color:#94a3b8;text-transform:uppercase;letter-spacing:1px}
.t1-in-v{font-size:14px;font-weight:600}
.t1-foot{margin-top:14px;font-size:12px;color:#64748b;text-align:center}
.t1-wrap .shiny-output-error-validation{color:#fca5a5}
.t1-top>div,.t1-bottom>div,.t1-card{min-width:0}
@media (max-width:600px){.t1-wrap{padding:14px}.t1-day{grid-template-columns:52px 32px 46px 34px 1fr 34px;gap:6px}.t1-dtxt{display:none}
  .t1-temp{font-size:72px}.t1-big-icon{font-size:78px}.t1-grid{grid-template-columns:1fr 1fr}}
)---"

# Apply consistent styling across all tabs so they match the dashboard's design
t1_app_css <- r"---(
.t1-wrap .t1-header{margin-bottom:20px}
.t1-wrap .t1-page-title{font-size:28px;font-weight:800;letter-spacing:-.5px;background:linear-gradient(90deg,#fff,#93c5fd);-webkit-background-clip:text;background-clip:text;color:transparent;margin:0}
.t1-wrap .t1-section-intro{color:#94a3b8;font-size:14px;line-height:1.65;margin:6px 0 18px}
.t1-wrap h2,.t1-wrap h3,.t1-wrap h4{color:#f8fafc;font-weight:650}
.t1-wrap h2{margin-top:4px}
.t1-wrap h3{font-size:17px;letter-spacing:.1px;margin-top:4px}
.t1-wrap .t1-card{color:#e8ecf8}
.t1-wrap .well{background:rgba(255,255,255,.045);border:1px solid rgba(255,255,255,.10);border-radius:20px;padding:18px;color:#e8ecf8;box-shadow:0 12px 28px rgba(0,0,0,.12)}
.t1-wrap .control-label,.t1-wrap label{color:#cbd5e1;font-weight:600}
.t1-wrap .help-block,.t1-wrap .shiny-input-container .help-block{color:#94a3b8}
.t1-wrap .form-control,.t1-wrap select.form-control,.t1-wrap input.form-control{background:rgba(15,23,42,.82);color:#f8fafc;border:1px solid rgba(255,255,255,.16);border-radius:10px;box-shadow:none}
.t1-wrap .form-control:focus{border-color:#60a5fa;box-shadow:0 0 0 3px rgba(96,165,250,.17)}
.t1-wrap select option{background:#111827;color:#f8fafc}
.t1-wrap input[type=file]{color:#cbd5e1}
.t1-wrap .btn-primary,.t1-wrap .btn-primary:focus{border:0;border-radius:11px;background:linear-gradient(135deg,#3b82f6,#8b5cf6);color:#fff;font-weight:650;padding:9px 16px;box-shadow:0 8px 18px rgba(59,130,246,.18)}
.t1-wrap .btn-primary:hover{filter:brightness(1.12);color:#fff}
.t1-wrap .table{color:#e8ecf8;background:transparent;margin-bottom:0}
.t1-wrap .table>thead>tr>th{color:#93c5fd;border-bottom:1px solid rgba(255,255,255,.16)}
.t1-wrap .table>tbody>tr>td,.t1-wrap .table>tbody>tr>th{border-top:1px solid rgba(255,255,255,.08)}
.t1-wrap .table-striped>tbody>tr:nth-of-type(odd){background:rgba(255,255,255,.035)}
.t1-wrap .table-hover>tbody>tr:hover{background:rgba(96,165,250,.08)}
.t1-wrap pre{background:rgba(15,23,42,.72);color:#dbeafe;border:1px solid rgba(255,255,255,.10);border-radius:14px;padding:15px;white-space:pre-wrap;word-break:normal}
.t1-wrap hr{border-top:1px solid rgba(255,255,255,.10)}
.t1-wrap .t1-metric-row{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:12px;margin:14px 0}
.t1-wrap .t1-metric{background:rgba(255,255,255,.055);border:1px solid rgba(255,255,255,.09);border-radius:16px;padding:14px 16px}
.t1-wrap .t1-metric-label{font-size:11px;text-transform:uppercase;letter-spacing:1.1px;color:#94a3b8;font-weight:700}
.t1-wrap .t1-metric-value{font-size:25px;font-weight:700;color:#f8fafc;margin-top:5px}
.t1-wrap .t1-note{padding:12px 14px;background:rgba(96,165,250,.08);border:1px solid rgba(96,165,250,.20);border-radius:14px;color:#cbd5e1;margin:10px 0}
.t1-wrap .t1-placeholder-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:14px}
.t1-wrap .t1-placeholder{background:rgba(255,255,255,.045);border:1px solid rgba(255,255,255,.09);border-radius:18px;padding:18px;min-height:145px}
.t1-wrap .t1-placeholder-icon{font-size:26px;margin-bottom:10px}
.t1-wrap .t1-placeholder-title{font-size:16px;font-weight:700;color:#f8fafc;margin-bottom:6px}
.t1-wrap .t1-placeholder-copy{font-size:13px;color:#94a3b8;line-height:1.6}
body{background:#0b1020}
.nav-tabs{border-bottom:1px solid rgba(255,255,255,.12);margin-top:8px}
.nav-tabs>li>a{color:#94a3b8;border:1px solid transparent;border-radius:12px 12px 0 0;font-weight:600}
.nav-tabs>li>a:hover{background:rgba(96,165,250,.10);border-color:rgba(255,255,255,.08);color:#e8ecf8}
.nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus{background:linear-gradient(135deg,rgba(59,130,246,.22),rgba(139,92,246,.18));color:#fff;border:1px solid rgba(255,255,255,.14);border-bottom-color:#0b1020}
.t1-wrap .shiny-output-error,.t1-wrap .shiny-output-error-validation{color:#fca5a5}
@media (max-width:650px){.t1-wrap{padding:14px}.t1-wrap .t1-page-title{font-size:24px}.t1-wrap .well{padding:13px}}
)---"

t1_js <- r"---(
function t1Locate(){
  if(!navigator.geolocation){Shiny.setInputValue('t1_geo_err','geolocation not supported by this browser',{priority:'event'});return;}
  navigator.geolocation.getCurrentPosition(
    function(p){Shiny.setInputValue('t1_geo',{lat:p.coords.latitude,lon:p.coords.longitude},{priority:'event'});},
    function(e){Shiny.setInputValue('t1_geo_err',e.message,{priority:'event'});},
    {timeout:10000});
}
$(document).on('keyup','#t1_city',function(e){ if(e.key==='Enter'){ $('#t1_go').click(); } });
)---"

t1_ui <- function() {
  div(class = "t1-wrap",
      tags$head(tags$style(HTML(t1_css)), tags$script(HTML(t1_js))),
      div(class = "t1-header",
          div(div(class = "t1-title", "Weather Dashboard"),
              div(class = "t1-kicker", "Live conditions \u00b7 24-hour outlook \u00b7 7-day forecast"))),
      div(class = "t1-bar",
          div(class = "t1-search",
              span(HTML("&#128269;")),
              textInput("t1_city", NULL, placeholder = "Search any city in the world\u2026", width = "100%"),
              actionButton("t1_go", "Search", class = "t1-btn")),
          tags$button(class = "btn t1-ghost", onclick = "t1Locate()", HTML("&#128205; My location")),
          actionButton("t1_refresh", HTML("&#8635; Refresh"), class = "t1-ghost"),
          div(class = "t1-units",
              radioButtons("t1_units", NULL, inline = TRUE,
                           choices = c("\u00b0C \u00b7 km/h" = "metric", "\u00b0F \u00b7 mph" = "imperial")))),
      div(class = "t1-chips",
          lapply(seq_along(t1_cities), function(i)
            actionLink(paste0("t1_q", i), t1_cities[i], class = "t1-chip"))),
      uiOutput("t1_matches_ui"),
      uiOutput("t1_msg_ui"),
      div(class = "t1-top", uiOutput("t1_hero"), uiOutput("t1_tiles")),
      div(class = "t1-card",
          div(class = "t1-h", "Next 24 hours"),
          uiOutput("t1_hourly"),
          plotOutput("t1_hplot", height = "230px")),
      div(class = "t1-bottom",
          div(class = "t1-card", div(class = "t1-h", "7-day forecast"), uiOutput("t1_week")),
          div(class = "t1-card", div(class = "t1-h", "Week at a glance"),
              plotOutput("t1_dplot", height = "240px"), uiOutput("t1_insights"))),
      uiOutput("t1_footer"))
}


t1_server <- function(input, output, session) {
  
  # Default location is set to Szeged, which matches our Kaggle dataset
  t1_loc     <- reactiveVal(list(name = "Szeged", region = "Csongr\u00e1d, Hungary",
                                 lat = 46.253, lon = 20.148))
  t1_matches <- reactiveVal(NULL)
  t1_msg     <- reactiveVal(NULL)
  
  t1_row_loc <- function(r) {
    parts <- c(r$admin1, r$country)
    list(name = r$name, region = paste(parts[nzchar(parts)], collapse = ", "),
         lat = r$lat, lon = r$lon)
  }
  
  t1_search <- function(q) {
    q <- trimws(q)
    if (nchar(q) < 2) { t1_msg("Type at least 2 letters of a city name."); return(invisible()) }
    g <- tryCatch(t1_geocode(q), error = function(e) "error")
    if (identical(g, "error")) {
      t1_msg("Could not reach the city search service. Check your internet connection.")
    } else if (is.null(g)) {
      t1_msg(paste0("No place found for \"", q, "\". Try another spelling."))
    } else {
      t1_msg(NULL)
      t1_matches(if (nrow(g) > 1) g else NULL)
      t1_loc(t1_row_loc(g[1, ]))
    }
  }
  
  observeEvent(input$t1_go, t1_search(input$t1_city))
  
  lapply(seq_along(t1_cities), function(i) {
    observeEvent(input[[paste0("t1_q", i)]], {
      updateTextInput(session, "t1_city", value = t1_cities[i])
      t1_search(t1_cities[i])
    }, ignoreInit = TRUE)
  })
  
  output$t1_matches_ui <- renderUI({
    g <- t1_matches(); req(g)
    labs <- paste0(g$name, ifelse(nzchar(g$admin1), paste0(", ", g$admin1), ""),
                   ifelse(nzchar(g$country), paste0(", ", g$country), ""))
    div(class = "t1-matches", span("Not the right place?"),
        selectInput("t1_pick", NULL, setNames(seq_len(nrow(g)), labs),
                    selectize = FALSE, width = "340px"))
  })
  
  observeEvent(input$t1_pick, {
    g <- t1_matches(); req(g)
    i <- as.integer(input$t1_pick)
    if (!is.na(i) && i <= nrow(g)) t1_loc(t1_row_loc(g[i, ]))
  }, ignoreInit = TRUE)
  
  observeEvent(input$t1_geo, {
    g <- input$t1_geo
    t1_matches(NULL); t1_msg(NULL)
    t1_loc(list(name = "My location", region = sprintf("%.3f, %.3f", g$lat, g$lon),
                lat = g$lat, lon = g$lon))
  })
  observeEvent(input$t1_geo_err, t1_msg(paste("Location unavailable:", input$t1_geo_err)))
  
  # Grab the latest live weather data and refresh it every 15 minutes
  t1_data <- reactive({
    input$t1_refresh
    invalidateLater(15 * 60 * 1000)
    loc <- t1_loc()
    wx <- tryCatch(t1_forecast(loc$lat, loc$lon), error = function(e) NULL)
    if (is.null(wx) || is.null(wx$current))
      return(list(ok = FALSE, loc = loc,
                  err = "The live weather service is unreachable. Check your internet connection and press Refresh."))
    aq <- tryCatch(t1_air(loc$lat, loc$lon)$current, error = function(e) NULL)
    h  <- wx$hourly
    i0 <- match(substr(wx$current$time, 1, 13), substr(h$time, 1, 13))
    if (is.na(i0)) i0 <- 1
    list(ok = TRUE, loc = loc, cur = wx$current, h = h, d = wx$daily, aq = aq,
         idx = i0:min(i0 + 23, length(h$time)), i0 = i0,
         tz = wx$timezone, off = t1_nz(wx$utc_offset_seconds), fetched = Sys.time())
  })
  
  # Helper functions to handle unit conversions based on user preferences
  t1_u <- reactive({
    imp <- identical(input$t1_units, "imperial")
    list(T  = function(c) if (imp) c * 9 / 5 + 32 else c,
         D  = function(c) if (imp) c * 9 / 5 else c,
         W  = function(k) if (imp) k * 0.621371 else k,
         V  = function(m) if (imp) m / 1609.34 else m / 1000,
         P  = function(mm) if (imp) mm / 25.4 else mm,
         tu = if (imp) "\u00b0F" else "\u00b0C", wu = if (imp) "mph" else "km/h",
         vu = if (imp) "mi" else "km", pu = if (imp) "in" else "mm")
  })
  
  # Keep track of the local time at the chosen city, updating every minute
  t1_now <- reactive({
    invalidateLater(60 * 1000)
    d <- t1_data(); req(isTRUE(d$ok))
    off <- if (is.na(d$off)) 0 else d$off
    as.POSIXct(format(Sys.time(), tz = "UTC"), tz = "UTC") + off
  })
  
  # Render the main hero card showing current conditions
  output$t1_hero <- renderUI({
    d <- t1_data()
    if (!isTRUE(d$ok))
      return(div(class = "t1-hero t1-cloudy",
                 div(class = "t1-loc", d$loc$name),
                 div(div(class = "t1-big-icon", "\U0001F4E1"),
                     div(class = "t1-cond", "Weather unavailable"),
                     div(class = "t1-feel", d$err))))
    u <- t1_u(); cc <- d$cur; dd <- d$d
    w <- t1_wmo(cc$weather_code, cc$is_day)
    temp <- t1_nz(cc$temperature_2m); feel <- t1_nz(cc$apparent_temperature)
    note <- if (is.na(feel - temp)) ""
    else if (feel - temp > 2) " \u00b7 humidity makes it feel warmer"
    else if (feel - temp < -2) " \u00b7 wind makes it feel colder" else ""
    div(class = paste0("t1-hero t1-", w$css),
        div(class = "t1-hero-top",
            div(div(class = "t1-loc", d$loc$name), div(class = "t1-region", d$loc$region)),
            div(class = "t1-live", span(class = "t1-dot"), "LIVE")),
        div(class = "t1-hero-mid",
            div(div(class = "t1-temp", round(u$T(temp)), span(class = "t1-unit", u$tu)),
                div(class = "t1-cond", w$txt),
                div(class = "t1-feel", paste0("Feels like ", t1_f(u$T(feel)), note))),
            div(class = "t1-big-icon", w$icon)),
        div(class = "t1-hero-foot",
            span(class = "t1-pill", paste0("\u2191 ", t1_f(u$T(dd$temperature_2m_max[1])),
                                           "   \u2193 ", t1_f(u$T(dd$temperature_2m_min[1])))),
            span(class = "t1-pill", HTML("&#128339; "), textOutput("t1_clock", inline = TRUE)),
            if (!is.null(d$tz)) span(class = "t1-pill", d$tz)))
  })
  
  output$t1_clock <- renderText(format(t1_now(), "%a %d %b, %H:%M"))
  
  # Build the grid of current weather condition tiles
  output$t1_tiles <- renderUI({
    d <- t1_data(); req(isTRUE(d$ok))
    u <- t1_u(); cc <- d$cur; dd <- d$d; h <- d$h; now <- t1_now()
    tile <- function(label, value, sub = NULL, extra = NULL, cls = "")
      div(class = paste("t1-tile", cls), div(class = "t1-tl", label),
          div(class = "t1-tv", value), extra, if (!is.null(sub)) div(class = "t1-ts", sub))
    
    hum <- t1_nz(cc$relative_humidity_2m)
    ws <- t1_nz(cc$wind_speed_10m); wg <- t1_nz(cc$wind_gusts_10m); wd <- t1_nz(cc$wind_direction_10m)
    pr <- t1_nz(cc$pressure_msl)
    p3 <- if (d$i0 + 3 <= length(h$pressure_msl)) h$pressure_msl[d$i0 + 3] else NA
    ptrend <- if (is.na(p3 - pr)) "" else if (p3 - pr > 1) "\u2197 Rising" else
      if (p3 - pr < -1) "\u2198 Falling" else "\u2192 Steady"
    vis <- t1_nz(cc$visibility)
    uv <- t1_nz(dd$uv_index_max[1])
    uvl <- if (is.na(uv)) "" else if (uv < 3) "Low" else if (uv < 6) "Moderate" else
      if (uv < 8) "High" else if (uv < 11) "Very high" else "Extreme"
    cloud <- t1_nz(cc$cloud_cover)
    rain_now <- t1_nz(cc$precipitation)
    aqi <- t1_nz(d$aq$us_aqi); pm <- t1_nz(d$aq$pm2_5)
    aq_lab <- if (is.na(aqi)) c("n/a", "#94a3b8") else if (aqi <= 50) c("Good", "#4ade80") else
      if (aqi <= 100) c("Moderate", "#facc15") else if (aqi <= 150) c("Unhealthy for sensitive", "#fb923c") else
        if (aqi <= 200) c("Unhealthy", "#ef4444") else c("Very unhealthy", "#a855f7")
    
    # Calculate the sun's position for the sunrise/sunset visual
    sr <- t1_pt(dd$sunrise[1]); ss <- t1_pt(dd$sunset[1])
    daylen <- as.numeric(difftime(ss, sr, units = "mins"))
    frac <- min(1, max(0, as.numeric(difftime(now, sr, units = "mins")) / daylen))
    ang <- pi * (1 - frac); sx <- 100 + 80 * cos(ang); sy <- 90 - 80 * sin(ang)
    up <- frac > 0 && frac < 1
    sun_svg <- sprintf(paste0(
      '<svg viewBox="0 0 200 100" class="t1-sun">',
      '<path d="M20,90 A80,80 0 0 1 180,90" fill="none" stroke="rgba(255,255,255,.25)" stroke-width="2" stroke-dasharray="4 5"/>',
      '<path d="M20,90 A80,80 0 0 1 %.1f,%.1f" fill="none" stroke="#fbbf24" stroke-width="3" stroke-linecap="round"/>',
      '<line x1="5" y1="90" x2="195" y2="90" stroke="rgba(255,255,255,.2)"/>',
      '<circle cx="%.1f" cy="%.1f" r="8" fill="%s"/></svg>'),
      sx, sy, sx, sy, if (up) "#fde047" else "#64748b")
    
    div(class = "t1-grid",
        tile("Humidity", paste0(round(hum), "%"),
             paste0("Dew point ", t1_f(u$T(t1_nz(cc$dew_point_2m)))),
             div(class = "t1-meter", tags$i(style = sprintf("width:%.0f%%", ifelse(is.na(hum), 0, hum))))),
        tile("Wind", HTML(sprintf("%s<small>%s</small>", t1_f(u$W(ws), ""), u$wu)),
             sprintf("Gusts %s %s \u00b7 from %s", t1_f(u$W(wg), ""), u$wu, t1_dir(wd)),
             div(class = "t1-compass",
                 span(class = "t1-needle", style = sprintf("transform:rotate(%.0fdeg)", ifelse(is.na(wd), 0, wd + 180)),
                      "\u2191")), cls = "t1-windtile"),
        tile("Pressure", HTML(sprintf("%s<small>hPa</small>", t1_f(pr, ""))), ptrend),
        tile("UV index", HTML(sprintf("%s<small>%s</small>", ifelse(is.na(uv), "\u2013", round(uv, 1)), uvl)),
             "Today's peak",
             div(class = "t1-meter t1-uv", tags$b(style = sprintf("left:%.0f%%", min(100, ifelse(is.na(uv), 0, uv) / 11 * 100))))),
        tile("Visibility", HTML(sprintf("%s<small>%s</small>", ifelse(is.na(vis), "\u2013", round(u$V(vis), 1)), u$vu)),
             if (is.na(vis)) "" else if (vis >= 10000) "Clear view" else if (vis >= 4000) "Moderate" else "Poor visibility"),
        tile("Cloud cover", paste0(round(cloud), "%"),
             paste0("Rain now: ", round(u$P(ifelse(is.na(rain_now), 0, rain_now)), 2), " ", u$pu),
             div(class = "t1-meter", tags$i(style = sprintf("width:%.0f%%;background:linear-gradient(90deg,#94a3b8,#e2e8f0)",
                                                            ifelse(is.na(cloud), 0, cloud))))),
        tile("Air quality (US AQI)",
             HTML(sprintf("%s<small style='color:%s'>%s</small>", ifelse(is.na(aqi), "\u2013", round(aqi)), aq_lab[2], aq_lab[1])),
             if (is.na(pm)) NULL else sprintf("PM2.5: %.1f \u00b5g/m\u00b3", pm)),
        tile("Rain today", HTML(sprintf("%s<small>%s</small>", round(u$P(t1_nz(dd$precipitation_sum[1])), 1), u$pu)),
             paste0("Chance: ", t1_f(t1_nz(dd$precipitation_probability_max[1]), "%"))),
        div(class = "t1-tile t1-wide",
            div(class = "t1-tl", "Sunrise & sunset"),
            div(class = "t1-sunrow", HTML(sun_svg),
                div(div(class = "t1-tv", style = "font-size:16px",
                        paste0("\U0001F305 Sunrise ", format(sr, "%H:%M"))),
                    div(class = "t1-tv", style = "font-size:16px",
                        paste0("\U0001F307 Sunset ", format(ss, "%H:%M"))),
                    div(class = "t1-ts", sprintf("Daylight %dh %02dm \u00b7 %s", floor(daylen / 60), round(daylen %% 60),
                                                 if (up) sprintf("%d%% of daylight gone", round(frac * 100))
                                                 else if (frac >= 1) "Sun has set" else "Before sunrise"))))))
  })
  
  # Create the horizontal scrolling strip for the hourly forecast
  output$t1_hourly <- renderUI({
    d <- t1_data(); req(isTRUE(d$ok)); u <- t1_u(); h <- d$h
    div(class = "t1-strip", lapply(seq_along(d$idx), function(k) {
      j <- d$idx[k]; w <- t1_wmo(h$weather_code[j], h$is_day[j]); p <- h$precipitation_probability[j]
      div(class = paste("t1-hr", if (k == 1) "t1-now"), title = w$txt,
          div(class = "t1-hr-t", if (k == 1) "Now" else substr(h$time[j], 12, 16)),
          div(class = "t1-hr-i", w$icon),
          div(class = "t1-hr-v", t1_f(u$T(h$temperature_2m[j]))),
          div(class = "t1-hr-p", if (!is.na(p) && p >= 10) paste0("\U0001F4A7", p, "%") else HTML("&nbsp;")))
    }))
  })
  
  # Plot the 24-hour temperature trend and rain probability
  output$t1_hplot <- renderPlot({
    d <- t1_data(); req(isTRUE(d$ok)); u <- t1_u(); h <- d$h; i <- d$idx
    df <- data.frame(t = t1_pt(h$time[i]), temp = u$T(h$temperature_2m[i]),
                     p = h$precipitation_probability[i])
    df$p[is.na(df$p)] <- 0
    lo <- floor(min(df$temp, na.rm = TRUE)) - 2; hi <- ceiling(max(df$temp, na.rm = TRUE)) + 2
    rng <- hi - lo
    lab <- df[seq(1, nrow(df), by = 3), ]
    ggplot(df, aes(t, temp)) +
      geom_rect(aes(xmin = t - 1500, xmax = t + 1500, ymin = lo, ymax = lo + p / 100 * rng),
                fill = "#38bdf8", alpha = .35, inherit.aes = FALSE) +
      geom_ribbon(aes(ymin = lo, ymax = temp), fill = "#f59e0b", alpha = .12) +
      geom_line(colour = "#fbbf24", linewidth = 1.3) +
      geom_point(data = lab, colour = "#fbbf24", size = 2.2) +
      geom_text(data = lab, aes(label = paste0(round(temp), "\u00b0")), vjust = -1.1,
                colour = "white", size = 3.6) +
      scale_y_continuous(limits = c(lo, hi + rng * 0.12), expand = c(0, 0),
                         sec.axis = sec_axis(~ (. - lo) / rng * 100, name = "Rain chance (%)",
                                             breaks = c(0, 50, 100))) +
      scale_x_datetime(date_labels = "%H:%M", date_breaks = "3 hours") +
      labs(x = NULL, y = paste0("Temp (", u$tu, ")")) +
      t1_theme()
  }, bg = "transparent")
  
  # Build the 7-day forecast list
  output$t1_week <- renderUI({
    d <- t1_data(); req(isTRUE(d$ok)); u <- t1_u(); dd <- d$d
    mins <- dd$temperature_2m_min; maxs <- dd$temperature_2m_max
    lo <- min(mins, na.rm = TRUE); hi <- max(maxs, na.rm = TRUE); sp <- max(hi - lo, 1)
    dates <- as.Date(dd$time)
    names_ <- format(dates, "%a"); names_[1] <- "Today"
    div(lapply(seq_along(dates), function(k) {
      mn <- mins[k]; mx <- maxs[k]; w <- t1_wmo(dd$weather_code[k], 1)
      p <- dd$precipitation_probability_max[k]
      left <- (mn - lo) / sp * 100; width <- max((mx - mn) / sp * 100, 3)
      dot <- if (k == 1) {
        ct <- min(max(t1_nz(d$cur$temperature_2m), lo), hi)
        span(class = "t1-dotnow", style = sprintf("left:%.1f%%", (ct - lo) / sp * 100))
      }
      div(class = "t1-day",
          div(class = "t1-dname", names_[k], div(class = "t1-ddate", format(dates[k], "%d %b"))),
          div(class = "t1-dicon", title = w$txt, w$icon),
          div(class = "t1-dtxt", w$txt),
          div(class = "t1-dp", if (!is.na(p) && p >= 20) paste0("\U0001F4A7", p, "%") else ""),
          div(class = "t1-dmin", t1_f(u$T(mn))),
          div(class = "t1-track",
              div(class = "t1-range", style = sprintf("left:%.1f%%;width:%.1f%%;background:linear-gradient(90deg,%s,%s)",
                                                      left, width, t1_tcol(mn), t1_tcol(mx))),
              dot),
          div(class = "t1-dmax", t1_f(u$T(mx))))
    }))
  })
  
  # Plot the weekly temperature ranges
  output$t1_dplot <- renderPlot({
    d <- t1_data(); req(isTRUE(d$ok)); u <- t1_u(); dd <- d$d
    df <- data.frame(day = as.Date(dd$time), mn = u$T(dd$temperature_2m_min),
                     mx = u$T(dd$temperature_2m_max))
    ggplot(df, aes(day)) +
      geom_ribbon(aes(ymin = mn, ymax = mx), fill = "#a78bfa", alpha = .22) +
      geom_line(aes(y = mx), colour = "#fb923c", linewidth = 1.2) +
      geom_line(aes(y = mn), colour = "#60a5fa", linewidth = 1.2) +
      geom_point(aes(y = mx), colour = "#fb923c", size = 2.6) +
      geom_point(aes(y = mn), colour = "#60a5fa", size = 2.6) +
      geom_text(aes(y = mx, label = paste0(round(mx), "\u00b0")), vjust = -1, colour = "white", size = 3.5) +
      geom_text(aes(y = mn, label = paste0(round(mn), "\u00b0")), vjust = 2, colour = "#cbd5e1", size = 3.5) +
      scale_x_date(date_labels = "%a", date_breaks = "1 day") +
      scale_y_continuous(expand = expansion(mult = .3)) +
      labs(x = NULL, y = paste0("Temp (", u$tu, ")")) +
      t1_theme()
  }, bg = "transparent")
  
  # Generate automatic text insights for the upcoming week
  output$t1_insights <- renderUI({
    d <- t1_data(); req(isTRUE(d$ok)); u <- t1_u(); dd <- d$d
    mx <- dd$temperature_2m_max; mn <- dd$temperature_2m_min
    ps <- dd$precipitation_sum; ps[is.na(ps)] <- 0
    pp <- dd$precipitation_probability_max; pp[is.na(pp)] <- 0
    wmax <- dd$wind_speed_10m_max; wmax[is.na(wmax)] <- 0
    dn <- format(as.Date(dd$time), "%A"); dn[1] <- "Today"
    iw <- which.max(mx); ic <- which.min(mn); iwet <- which.max(ps)
    slope <- unname(coef(lm(mx ~ seq_along(mx)))[2])          
    trend <- if (is.na(slope) || abs(slope) < 0.3) "Fairly steady"
    else sprintf("%s ~%.1f%s/day", if (slope > 0) "Warming" else "Cooling", abs(u$D(slope)), u$tu)
    comfort <- abs((mx + mn) / 2 - 21) + pp / 10 + wmax / 10   
    ib <- which.min(comfort)
    items <- list(
      c("\U0001F525", "Warmest day", paste0(dn[iw], " \u00b7 ", t1_f(u$T(mx[iw]), u$tu))),
      c("\U0001F976", "Coldest night", paste0(dn[ic], " \u00b7 ", t1_f(u$T(mn[ic]), u$tu))),
      c("\U0001F4C8", "7-day trend", trend),
      c("\U0001F327\ufe0f", "Rain this week",
        if (sum(ps) >= 1) sprintf("%.1f %s \u00b7 wettest %s", u$P(sum(ps)), u$pu, dn[iwet]) else "Mostly dry"),
      c("\u2195\ufe0f", "Avg day\u2013night swing", t1_f(u$D(mean(mx - mn, na.rm = TRUE)), u$tu)),
      c("\U0001F33F", "Best day outdoors", dn[ib]))
    div(class = "t1-ins", lapply(items, function(x)
      div(class = "t1-in", span(class = "t1-in-i", x[1]),
          div(div(class = "t1-in-l", x[2]), div(class = "t1-in-v", x[3])))))
  })
  
  output$t1_footer <- renderUI({
    d <- t1_data(); req(isTRUE(d$ok))
    div(class = "t1-foot",
        sprintf("Live data: Open-Meteo.com \u00b7 %.3f, %.3f \u00b7 Updated %s \u00b7 Auto-refreshes every 15 min",
                d$loc$lat, d$loc$lon, format(d$fetched, "%H:%M:%S")))
  })
}

#APP UI CODE

ui <- fluidPage(
  
  # Apply dashboard styling
  tags$head(
    tags$style(HTML(t1_css)),
    tags$style(HTML(t1_app_css))
  ),
  
  titlePanel("Statistical Weather Analysis"),
  
  tabsetPanel(
    
    tabPanel(
      "1. Weather Dashboard",
      t1_ui()
    ),
    
    tabPanel(
      "2. Descriptive Statistics",
      div(class = "t1-wrap t1-page",
          div(class = "t1-header",
              div(
                div(class = "t1-page-title", "Exploratory Analysis & Probability"),
                div(class = "t1-kicker", "Descriptive statistics · distributions · percentiles · outliers")
              )
          ),
          div(class = "t1-section-intro",
              "Explore the weather dataset, understand its distribution, and estimate the probability of crossing a selected threshold."
          ),
          sidebarLayout(
            sidebarPanel(
              h4("ANALYSIS CONTROLS"),
              fileInput("t2_file", "Choose weatherHistory.csv", accept = ".csv"),
              helpText("If you already uploaded the dataset in Tab 4, that file can be used automatically."),
              selectInput("t2_var", "Weather variable",
                          c("Temperature (C)", "Apparent Temperature (C)", "Humidity",
                            "Wind Speed (km/h)", "Visibility (km)", "Pressure (millibars)")),
              hr(),
              uiOutput("t2_threshold_ui"),
              radioButtons("t2_tail", "Estimate probability of a value",
                           c("Above threshold" = "above", "Below threshold" = "below"))
            ),
            mainPanel(
              div(class = "t1-card",
                  div(class = "t1-h", "SUMMARY STATISTICS"),
                  tableOutput("t2_stats")
              ),
              div(class = "t1-card",
                  div(class = "t1-h", "DISTRIBUTION ANALYSIS"),
                  plotOutput("t2_hist", height = "300px"),
                  plotOutput("t2_box", height = "300px")
              ),
              div(class = "t1-card",
                  div(class = "t1-h", "PERCENTILES"),
                  tableOutput("t2_pct")
              ),
              div(class = "t1-card",
                  div(class = "t1-h", "THRESHOLD PROBABILITY"),
                  verbatimTextOutput("t2_prob")
              ),
              div(class = "t1-card",
                  div(class = "t1-h", "OUTLIERS · IQR RULE"),
                  verbatimTextOutput("t2_out")
              ),
              div(class = "t1-card",
                  div(class = "t1-h", "AUTOMATIC INTERPRETATION"),
                  verbatimTextOutput("t2_text")
              )
            )
          )
      )
    ),
    
    tabPanel(
      "3. Correlation and Regression",
      div(class = "t1-wrap t1-page",
          div(class = "t1-header",
              div(
                div(class = "t1-page-title", "Correlation & Regression Lab"),
                div(class = "t1-kicker", "Explore relationships \u00b7 Heatmaps \u00b7 Linear Regression")
              )
          ),
          div(class = "t1-section-intro",
              "Investigate how weather variables relate to one another using interactive scatter plots and automated linear regression models."
          ),
          sidebarLayout(
            sidebarPanel(
              h4("DATA CONTROLS"),
              
              radioButtons("t3_source", "Select Data Source:",
                           choices = c("Upload CSV File" = "csv", "Fetch Live Data" = "api"),
                           selected = "csv"),
              
              conditionalPanel(
                condition = "input.t3_source == 'csv'",
                fileInput("t3_file", "Choose weather_data.csv", accept = ".csv"),
                helpText("Automatically uses Tab 4's dataset if uploaded.")
              ),
              
              conditionalPanel(
                condition = "input.t3_source == 'api'",
                textInput("t3_api_city", "City Name:", value = "London"),
                actionButton("t3_fetch_api", "Fetch 30-Day Data", class = "btn btn-primary"),
                helpText("Pulls the last 30 days of hourly data for regression.")
              ),
              
              conditionalPanel(
                condition = "input.t3_tabs == 'scatter_tab'",
                hr(),
                h4("SCATTER PLOT CONTROLS"),
                selectInput("t3_x", "X-Axis Variable:",
                            choices = c("Temperature (C)", "Apparent Temperature (C)", "Humidity", "Wind Speed (km/h)", "Visibility (km)", "Pressure (millibars)"),
                            selected = "Humidity",
                            selectize = FALSE),
                selectInput("t3_y", "Y-Axis Variable:",
                            choices = c("Temperature (C)", "Apparent Temperature (C)", "Humidity", "Wind Speed (km/h)", "Visibility (km)", "Pressure (millibars)"),
                            selected = "Temperature (C)",
                            selectize = FALSE),
                checkboxInput("t3_trend", "Show Line of Best Fit", value = TRUE)
              ),
              
              conditionalPanel(
                condition = "input.t3_tabs == 'reg_tab'",
                hr(),
                h4("REGRESSION CONTROLS"),
                selectInput("t3_target", "Target Variable (Y):",
                            choices = c("Temperature (C)", "Apparent Temperature (C)", "Humidity", "Wind Speed (km/h)", "Visibility (km)", "Pressure (millibars)"),
                            selected = "Temperature (C)",
                            selectize = FALSE),
                checkboxGroupInput("t3_predictors", "Predictor Variables (X):",
                                   choices = c("Temperature (C)", "Apparent Temperature (C)", "Humidity", "Wind Speed (km/h)", "Visibility (km)", "Pressure (millibars)"),
                                   selected = c("Humidity"))
              )
            ),
            mainPanel(
              tabsetPanel(id = "t3_tabs",
                          tabPanel("Correlation Matrix", value = "corr_tab",
                                   br(),
                                   div(class = "t1-card",
                                       div(class = "t1-h", "CORRELATION HEATMAP"),
                                       plotOutput("t3_heatmap", height = "450px"),
                                       br(),
                                       div(class = "t1-note", uiOutput("t3_heatmap_text"))
                                   )
                          ),
                          tabPanel("Scatter Explorer", value = "scatter_tab",
                                   br(),
                                   div(class = "t1-card",
                                       div(class = "t1-h", "INTERACTIVE SCATTER PLOT"),
                                       plotlyOutput("t3_scatter", height = "450px")
                                   )
                          ),
                          tabPanel("Regression Model", value = "reg_tab",
                                   br(),
                                   div(class = "t1-card",
                                       div(class = "t1-h", "MODEL COEFFICIENTS"),
                                       tableOutput("t3_reg_summary"),
                                       br(),
                                       div(class = "t1-note", uiOutput("t3_reg_interpretation"))
                                   ),
                                   div(class = "t1-card",
                                       div(class = "t1-h", "MODEL DIAGNOSTICS"),
                                       fluidRow(
                                         column(6, plotOutput("t3_diag1", height = "280px")),
                                         column(6, plotOutput("t3_diag2", height = "280px"))
                                       )
                                   )
                          )
              )
            )
          )
      )
    ),
    
    tabPanel(
      "4. Statistical Testing",
      div(class = "t1-wrap t1-page",
          div(class = "t1-header",
              div(
                div(class = "t1-page-title", "Hypothesis Testing Lab"),
                div(class = "t1-kicker", "Compare daily temperatures · Welch's t-test · confidence intervals")
              )
          ),
          div(class = "t1-section-intro",
              "Upload the Kaggle weather dataset and compare daily average temperatures between two months in a selected year."
          ),
          sidebarLayout(
            sidebarPanel(
              h4("ANALYSIS CONTROLS"),
              fileInput(
                "dataset_file",
                "Choose weather_data.csv",
                accept = ".csv"
              ),
              helpText("Select the CSV file downloaded from Kaggle."),
              hr(),
              uiOutput("year_selector"),
              selectInput(
                "month1",
                "First month",
                choices = month.name,
                selected = "January"
              ),
              selectInput(
                "month2",
                "Second month",
                choices = month.name,
                selected = "June"
              ),
              actionButton(
                "analyze",
                "Perform Statistical Test",
                class = "t1-btn"
              )
            ),
            mainPanel(
              div(class = "t1-card",
                  div(class = "t1-h", "UPLOADED DATASET PREVIEW"),
                  tableOutput("data_preview"),
                  textOutput("dataset_status")
              ),
              div(class = "t1-card",
                  div(class = "t1-h", "HYPOTHESES"),
                  verbatimTextOutput("hypotheses")
              ),
              div(class = "t1-card",
                  div(class = "t1-h", "SAMPLE STATISTICS"),
                  tableOutput("summary_table")
              ),
              div(class = "t1-card",
                  div(class = "t1-h", "TEMPERATURE DISTRIBUTION"),
                  plotOutput("distribution_plot", height = "350px")
              ),
              div(class = "t1-card",
                  div(class = "t1-h", "WELCH'S TWO-SAMPLE T-TEST"),
                  tableOutput("test_table")
              ),
              div(class = "t1-card",
                  div(class = "t1-h", "STATISTICAL INTERPRETATION"),
                  verbatimTextOutput("conclusion")
              )
            )
          )
      )
    )
  )
)

# APP SERVER CODE

server <- function(input, output, session) {
  
  t1_server(input, output, session)
  
  t2_raw <- reactive({
    f <- if (!is.null(input$t2_file)) input$t2_file else input$dataset_file
    req(f)
    d <- read.csv(f$datapath, check.names = FALSE)
    d[!duplicated(d[["Formatted Date"]]), ]
  })
  
  # Filter the selected variable by month and remove invalid zero values
  t2_data <- reactive({
    d <- t2_raw()
    df <- data.frame(
      Month = factor(month.name[as.integer(substr(d[["Formatted Date"]], 6, 7))],
                     levels = month.name),
      Value = as.numeric(d[[input$t2_var]])
    )
    df <- df[is.finite(df$Value), ]
    if (input$t2_var %in% c("Humidity", "Pressure (millibars)")) df <- df[df$Value > 0, ]
    df
  })
  
  output$t2_threshold_ui <- renderUI({
    numericInput("t2_thr", "Threshold value",
                 round(quantile(t2_data()$Value, 0.75), 1))
  })
  
  # Summary statistics
  t2_s <- reactive({
    x <- t2_data()$Value
    list(x = x, m = mean(x), sd = sd(x), q = quantile(x, c(.25, .5, .75)),
         skew = mean(((x - mean(x)) / sd(x))^3))
  })
  
  output$t2_stats <- renderTable({
    s <- t2_s(); x <- s$x
    data.frame(
      Statistic = c("n", "Mean", "Median", "Std deviation", "Variance",
                    "Minimum", "Q1", "Q3", "Maximum", "IQR", "Skewness"),
      Value = round(c(length(x), s$m, s$q[2], s$sd, var(x), min(x),
                      s$q[1], s$q[3], max(x), s$q[3] - s$q[1], s$skew), 3)
    )
  }, striped = TRUE, bordered = TRUE)
  
  # Histogram and normal curve plot
  output$t2_hist <- renderPlot({
    s <- t2_s()
    ggplot(t2_data(), aes(Value)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40,
                     fill = "#9ecae1", colour = "white") +
      geom_density(colour = "blue", linewidth = 1) +
      stat_function(fun = dnorm, args = list(mean = s$m, sd = s$sd),
                    colour = "red", linetype = "dashed") +
      labs(title = paste("Distribution of", input$t2_var),
           subtitle = "Blue = density, red dashed = normal curve", x = input$t2_var) +
      t1_theme()
  }, bg = "transparent")
  
  # Box plot sorted by month
  output$t2_box <- renderPlot({
    ggplot(t2_data(), aes(Month, Value, fill = Month)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = paste(input$t2_var, "by month"), y = input$t2_var) +
      t1_theme() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  }, bg = "transparent")
  
  # Percentile calculations
  output$t2_pct <- renderTable({
    p <- c(.01, .05, .10, .25, .50, .75, .90, .95, .99)
    data.frame(Percentile = paste0(p * 100, "th"),
               Value = round(quantile(t2_data()$Value, p, names = FALSE), 3))
  }, striped = TRUE, bordered = TRUE)
  
  # Calculate threshold probability using both empirical data and normal models
  t2_p <- reactive({
    req(input$t2_thr); s <- t2_s(); x <- s$x; t <- input$t2_thr
    up <- input$t2_tail == "above"
    list(emp = if (up) mean(x > t) else mean(x < t),
         norm = pnorm(t, s$m, s$sd, lower.tail = !up))
  })
  
  output$t2_prob <- renderText({
    p <- t2_p()
    paste0("P(value ", input$t2_tail, " ", input$t2_thr, ")\n",
           "Empirical (observed): ", round(p$emp * 100, 2), "%\n",
           "Normal model (pnorm): ", round(p$norm * 100, 2), "%")
  })
  
  # Detect outliers using the IQR rule
  t2_o <- reactive({
    s <- t2_s(); iqr <- s$q[3] - s$q[1]
    lo <- s$q[1] - 1.5 * iqr; hi <- s$q[3] + 1.5 * iqr
    list(lo = lo, hi = hi, n = sum(s$x < lo | s$x > hi))
  })
  
  output$t2_out <- renderText({
    o <- t2_o()
    paste0("Lower bound: ", round(o$lo, 2), "   Upper bound: ", round(o$hi, 2), "\n",
           "Outliers found: ", o$n, " (", round(100 * o$n / length(t2_s()$x), 2), "%)")
  })
  
  # Generate automatic statistical interpretation for Tab 2
  output$t2_text <- renderText({
    s <- t2_s(); p <- t2_p(); o <- t2_o()
    shape <- if (abs(s$skew) < 0.5) "approximately symmetric"
    else if (s$skew > 0) "right-skewed" else "left-skewed"
    paste0(
      "The mean ", input$t2_var, " was ", round(s$m, 2), " (median ",
      round(s$q[2], 2), ", SD ", round(s$sd, 2), "). The distribution is ", shape,
      " (skewness ", round(s$skew, 2), ").\n",
      "The probability of a value ", input$t2_tail, " ", input$t2_thr, " is ",
      round(p$emp * 100, 1), "% from the data and ", round(p$norm * 100, 1),
      "% from the normal model",
      if (abs(p$emp - p$norm) > 0.05) ", so the normal model is a poor fit here.\n"
      else ", so the normal model fits reasonably.\n",
      "The IQR rule flagged ", o$n, " unusual observations."
    )
  })
  
  
  # When the user requests API data, geocode the city, pull the last 30 days of hourly data, and format it to match our CSV structure
  t3_api_data <- eventReactive(input$t3_fetch_api, {
    req(input$t3_api_city)
    
    geo <- t1_geocode(input$t3_api_city)
    if(is.null(geo)) {
      showNotification("City not found. Please try another.", type = "error")
      return(NULL)
    }
    
    url <- sprintf("https://api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f&past_days=30&hourly=temperature_2m,apparent_temperature,relative_humidity_2m,wind_speed_10m,visibility,pressure_msl&timezone=auto", 
                   geo$lat[1], geo$lon[1])
    
    res <- tryCatch({
      req <- httr::GET(url)
      jsonlite::fromJSON(httr::content(req, as = "text", encoding = "UTF-8"))
    }, error = function(e) NULL)
    
    if(is.null(res) || is.null(res$hourly)) {
      showNotification("Failed to retrieve data from Open-Meteo.", type = "error")
      return(NULL)
    }
    
    h <- res$hourly
    df <- data.frame(
      `Temperature (C)` = as.numeric(h$temperature_2m),
      `Apparent Temperature (C)` = as.numeric(h$apparent_temperature),
      `Humidity` = as.numeric(h$relative_humidity_2m),
      `Wind Speed (km/h)` = as.numeric(h$wind_speed_10m),
      `Visibility (km)` = as.numeric(h$visibility) / 1000, 
      `Pressure (millibars)` = as.numeric(h$pressure_msl),
      check.names = FALSE
    )
    
    showNotification(paste("Successfully loaded 30 days of data for", geo$name[1]), type = "message")
    df
  })
  
  # Allow the user to toggle between using uploaded CSV data or fetched API data
  t3_raw <- reactive({
    if (input$t3_source == "csv") {
      f <- if (!is.null(input$t3_file)) input$t3_file else input$dataset_file
      req(f)
      read.csv(f$datapath, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA", "N/A", "null", "NULL"))
    } else {
      req(t3_api_data())
      t3_api_data()
    }
  })
  
  # Ensure all necessary variables are numeric and clean up invalid rows
  t3_data <- reactive({
    d <- t3_raw() 
    vars <- c("Temperature (C)", "Apparent Temperature (C)", "Humidity", "Wind Speed (km/h)", "Visibility (km)", "Pressure (millibars)")
    df <- d[, intersect(names(d), vars), drop = FALSE]
    
    for(col in names(df)) df[[col]] <- as.numeric(df[[col]])
    
    df <- df[complete.cases(df), , drop = FALSE]
    if("Humidity" %in% names(df)) df <- df[df$Humidity > 0, ]
    if("Pressure (millibars)" %in% names(df)) df <- df[df$`Pressure (millibars)` > 0, ]
    df
  })
  
  # Dynamically remove the selected X-axis variable from the Y-axis dropdown options
  observeEvent(input$t3_x, {
    all_vars <- c("Temperature (C)", "Apparent Temperature (C)", "Humidity", 
                  "Wind Speed (km/h)", "Visibility (km)", "Pressure (millibars)")
    valid_y_choices <- setdiff(all_vars, input$t3_x)
    current_y <- isolate(input$t3_y)
    new_y <- ifelse(current_y %in% valid_y_choices, current_y, valid_y_choices[1])
    updateSelectInput(session, "t3_y", choices = valid_y_choices, selected = new_y)
  })
  
  # Dynamically remove the chosen Target (Y) from the Predictor (X) checkboxes
  observeEvent(input$t3_target, {
    all_vars <- c("Temperature (C)", "Apparent Temperature (C)", "Humidity", 
                  "Wind Speed (km/h)", "Visibility (km)", "Pressure (millibars)")
    valid_predictor_choices <- setdiff(all_vars, input$t3_target)
    current_predictors <- isolate(input$t3_predictors)
    new_predictors <- intersect(current_predictors, valid_predictor_choices)
    if(length(new_predictors) == 0) new_predictors <- valid_predictor_choices[1]
    
    updateCheckboxGroupInput(session, "t3_predictors", 
                             choices = valid_predictor_choices, 
                             selected = new_predictors)
  })
  
  # Render the correlation heatmap
  output$t3_heatmap <- renderPlot({
    df <- t3_data()
    req(nrow(df) > 0)
    
    clean_names <- gsub(" \\(.*\\)", "", colnames(df))
    clean_names <- gsub("Apparent Temperature", "App Temp", clean_names)
    colnames(df) <- clean_names
    
    corr_matrix <- cor(df, use = "complete.obs")
    
    ggcorrplot(corr_matrix, hc.order = FALSE, type = "lower",
               lab = TRUE, lab_size = 4, tl.cex = 11, tl.srt = 45,
               colors = c("#ef4444", "white", "#3b82f6")) +
      ggtitle("Pearson Correlation Matrix") +
      theme(
        text = element_text(colour = "#cbd5e1"),
        axis.text.x = element_text(colour = "#94a3b8", angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(colour = "#94a3b8"),
        plot.title = element_text(size = 14),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(colour = "#cbd5e1"),
        plot.margin = margin(t = 10, r = 10, b = 40, l = 10)
      )
  }, bg = "transparent")
  
  output$t3_heatmap_text <- renderUI({
    df <- t3_data()
    req(nrow(df) > 0)
    corr_matrix <- cor(df, use = "complete.obs")
    corr_matrix[lower.tri(corr_matrix, diag = TRUE)] <- NA
    
    max_cor <- max(corr_matrix, na.rm = TRUE)
    min_cor <- min(corr_matrix, na.rm = TRUE)
    max_ind <- which(corr_matrix == max_cor, arr.ind = TRUE)
    min_ind <- which(corr_matrix == min_cor, arr.ind = TRUE)
    
    HTML(paste0(
      "<b>Key Insight:</b> The strongest positive correlation is between <b>", 
      rownames(corr_matrix)[max_ind[1, 1]], " and ", colnames(corr_matrix)[max_ind[1, 2]], 
      " (r = ", round(max_cor, 2), ")</b>. ",
      "The strongest negative correlation is between <b>", 
      rownames(corr_matrix)[min_ind[1, 1]], " and ", colnames(corr_matrix)[min_ind[1, 2]], 
      " (r = ", round(min_cor, 2), ")</b>."
    ))
  })
  
  # Render the interactive scatter plot
  output$t3_scatter <- renderPlotly({
    df <- t3_data()
    req(nrow(df) > 0, input$t3_x, input$t3_y)
    
    if(nrow(df) > 3000) df <- df[sample(nrow(df), 3000), ]
    
    p <- ggplot(df, aes_string(x = paste0("`", input$t3_x, "`"), y = paste0("`", input$t3_y, "`"))) +
      geom_point(alpha = 0.5, color = "#60a5fa") +
      theme_minimal(base_size = 13) +
      theme(
        text = element_text(colour = "#cbd5e1"),
        axis.text = element_text(colour = "#94a3b8"),
        panel.grid.major = element_line(colour = "#FFFFFF14"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)
      ) + labs(x = input$t3_x, y = input$t3_y)
    
    if (input$t3_trend) p <- p + geom_smooth(method = "lm", color = "#fb923c", se = FALSE)
    
    ggplotly(p) %>% layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  # Compute regression model parameters
  t3_model <- reactive({
    df <- t3_data()
    req(nrow(df) > 0, length(input$t3_predictors) > 0)
    
    predictors <- input$t3_predictors[input$t3_predictors != input$t3_target]
    req(length(predictors) > 0)
    
    pred_str <- paste(paste0("`", predictors, "`"), collapse = " + ")
    formula_str <- paste0("`", input$t3_target, "` ~ ", pred_str)
    
    lm(as.formula(formula_str), data = df)
  })
  
  output$t3_reg_summary <- renderTable({
    req(t3_model())
    tidy(t3_model()) %>%
      mutate(
        p.value = ifelse(p.value < 0.001, "<0.001", sprintf("%.4f", p.value)),
        estimate = sprintf("%.4f", estimate),
        std.error = sprintf("%.4f", std.error),
        statistic = sprintf("%.2f", statistic)
      ) %>%
      rename(Term = term, Estimate = estimate, `Std. Error` = std.error, 
             `t-value` = statistic, `p-value` = p.value)
  }, striped = TRUE, bordered = TRUE)
  
  output$t3_reg_interpretation <- renderUI({
    req(t3_model())
    mod <- t3_model()
    glance_mod <- glance(mod)
    r_squared <- round(glance_mod$r.squared * 100, 1)
    
    tidy_mod <- tidy(mod)
    if(nrow(tidy_mod) > 1) {
      first_pred <- gsub("`", "", tidy_mod$term[2])
      first_coef <- round(tidy_mod$estimate[2], 3)
      first_p <- tidy_mod$p.value[2]
      sig_text <- if(first_p < 0.05) "statistically significant" else "NOT statistically significant"
      
      coef_interp <- paste0(
        "For every 1-unit increase in <b>", first_pred, "</b>, ",
        input$t3_target, " changes by an average of <b>", first_coef, "</b> units.<br>",
        "This relationship is <b>", sig_text, "</b> (p-value = ", ifelse(first_p < 0.001, "<0.001", round(first_p, 4)), ")."
      )
    } else {
      coef_interp <- "No valid predictors selected."
    }
    
    HTML(paste0("<b>Model Fit (R^2):</b> This model explains <b>", r_squared, "%</b> of the variance in ", input$t3_target, ".<br><br>", coef_interp))
  })
  
  output$t3_diag1 <- renderPlot({
    req(t3_model())
    par(
      bg = "transparent", col.axis = "#94a3b8", col.lab = "#cbd5e1", col.main = "#f8fafc", fg = "#cbd5e1",
      mar = c(4.5, 4.5, 2.5, 1.5)
    )
    plot(t3_model(), which = 1, sub.caption = "", col = "#60a5fa", pch = 16, cex = 0.6)
  }, bg = "transparent")
  
  output$t3_diag2 <- renderPlot({
    req(t3_model())
    par(
      bg = "transparent", col.axis = "#94a3b8", col.lab = "#cbd5e1", col.main = "#f8fafc", fg = "#cbd5e1",
      mar = c(4.5, 4.5, 2.5, 1.5)
    )
    plot(t3_model(), which = 2, sub.caption = "", col = "#60a5fa", pch = 16, cex = 0.6)
  }, bg = "transparent")
  
  # Read and validate the uploaded CSV file for Tab 4
  raw_data <- reactive({
    req(input$dataset_file)
    data <- tryCatch(
      read.csv(
        input$dataset_file$datapath,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        na.strings = c("", "NA", "N/A", "null", "NULL")
      ),
      error = function(e) {
        stop(
          paste("Could not read the CSV file:", conditionMessage(e))
        )
      }
    )
    
    shiny::validate(
      shiny::need(nrow(data) > 0, "The uploaded CSV contains no records."),
      shiny::need(
        all(c("Formatted Date", "Temperature (C)") %in% names(data)),
        "Required columns are missing. Expected: Formatted Date and Temperature (C)."
      )
    )
    data
  })
  
  # Show a quick preview of the dataset
  output$data_preview <- renderTable({
    data <- raw_data()
    preview_columns <- intersect(
      c("Formatted Date", "Temperature (C)", "Humidity", "Summary"),
      names(data)
    )
    head(data[, preview_columns, drop = FALSE], 6)
  }, rownames = FALSE)
  
  output$dataset_status <- renderText({
    data <- raw_data()
    paste("Successfully loaded", format(nrow(data), big.mark = ","), "hourly records.")
  })
  
  # Clean data: extract dates and convert hourly temperatures into daily averages
  daily_data <- reactive({
    data <- raw_data()
    date_text <- substr(trimws(as.character(data[["Formatted Date"]])), 1, 10)
    dates <- as.Date(date_text, format = "%Y-%m-%d")
    temperatures <- suppressWarnings(as.numeric(as.character(data[["Temperature (C)"]])))
    
    cleaned <- data.frame(Date = dates, Temperature = temperatures)
    cleaned <- cleaned[!is.na(cleaned$Date) & is.finite(cleaned$Temperature), , drop = FALSE]
    
    shiny::validate(
      shiny::need(nrow(cleaned) > 0, "No valid records found. Check the date and temperature columns.")
    )
    
    daily <- aggregate(Temperature ~ Date, data = cleaned, FUN = mean)
    daily$Year <- format(daily$Date, "%Y")
    month_number <- as.integer(format(daily$Date, "%m"))
    daily$Month <- month.name[month_number]
    daily
  })
  
  # Dynamically list available years for filtering
  output$year_selector <- renderUI({
    data <- daily_data()
    years <- sort(unique(data$Year))
    selected_year <- tail(years, 1)
    selectInput("year", "Select Year", choices = years, selected = selected_year)
  })
  
  # Run Welch's two-sample t-test on the selected months
  analysis <- eventReactive(input$analyze, {
    data <- daily_data()
    req(input$year, input$month1, input$month2)
    
    shiny::validate(
      shiny::need(input$month1 != input$month2, "Please select two different months.")
    )
    
    selected_data <- data[data$Year == as.character(input$year), , drop = FALSE]
    first <- selected_data$Temperature[selected_data$Month == input$month1]
    second <- selected_data$Temperature[selected_data$Month == input$month2]
    
    shiny::validate(
      shiny::need(length(first) >= 2, paste("Not enough observations for", input$month1, "in the selected year.")),
      shiny::need(length(second) >= 2, paste("Not enough observations for", input$month2, "in the selected year."))
    )
    
    test <- tryCatch(
      t.test(first, second, alternative = "two.sided", conf.level = 0.95),
      error = function(e) NULL
    )
    
    shiny::validate(
      shiny::need(!is.null(test), "The t-test could not be calculated. Please check the selected data.")
    )
    
    list(data = selected_data, first = first, second = second, 
         test = test, difference = mean(first) - mean(second))
  }, ignoreInit = TRUE)
  
  # Text readouts for hypothesis definitions
  output$hypotheses <- renderText({
    paste0(
      "Null hypothesis (H0):\n",
      "The population mean daily temperatures in ", input$month1, " and ", input$month2, " are equal.\n\n",
      "Alternative hypothesis (H1):\n",
      "The population mean daily temperatures in ", input$month1, " and ", input$month2, " are different.\n\n",
      "Significance level: 0.05"
    )
  })
  
  # Populate summary statistics table
  output$summary_table <- renderTable({
    result <- analysis()
    req(result)
    data.frame(
      Month = c(input$month1, input$month2),
      Number_of_Days = c(length(result$first), length(result$second)),
      Mean_Temperature_C = round(c(mean(result$first), mean(result$second)), 2),
      Standard_Deviation = round(c(sd(result$first), sd(result$second)), 2),
      Minimum = round(c(min(result$first), min(result$second)), 2),
      Maximum = round(c(max(result$first), max(result$second)), 2),
      check.names = FALSE
    )
  }, rownames = FALSE, striped = TRUE, bordered = TRUE)
  
  # Plot the distribution of average temperatures between the two months
  output$distribution_plot <- renderPlot({
    result <- analysis()
    req(result)
    
    plot_data <- data.frame(
      Temperature = c(result$first, result$second),
      Month = factor(c(rep(input$month1, length(result$first)), rep(input$month2, length(result$second))),
                     levels = c(input$month1, input$month2))
    )
    
    ggplot(plot_data, aes(x = Month, y = Temperature, fill = Month)) +
      geom_boxplot(alpha = 0.65, outlier.shape = NA) +
      geom_jitter(width = 0.12, alpha = 0.45) +
      labs(title = "Daily Average Temperature by Month", x = "Month", y = "Temperature (°C)") +
      t1_theme() +
      theme(legend.position = "none")
  }, bg = "transparent")
  
  # Render final t-test metrics (degrees of freedom shown as a whole number)
  output$test_table <- renderTable({
    result <- analysis()
    req(result)
    test <- result$test
    
    data.frame(
      Statistic = c(
        "t statistic",
        "Degrees of freedom",
        "p-value",
        "95% CI lower bound (°C)",
        "95% CI upper bound (°C)",
        "Mean difference (first - second) (°C)"
      ),
      Value = c(
        sprintf("%.4f", unname(test$statistic)),
        sprintf("%d", as.integer(round(unname(test$parameter)))),
        format(signif(test$p.value, 5), scientific = FALSE),
        sprintf("%.3f", test$conf.int[1]),
        sprintf("%.3f", test$conf.int[2]),
        sprintf("%.3f", result$difference)
      ),
      check.names = FALSE
    )
  }, rownames = FALSE, striped = TRUE, bordered = TRUE)
  
  # Provide automatic statistical interpretations depending on the p-value
  output$conclusion <- renderText({
    result <- analysis()
    req(result)
    test <- result$test
    
    if (test$p.value < 0.05) {
      decision <- paste(
        "Reject H0 at the 5% significance level.",
        "There is statistical evidence of a difference in mean daily temperature between the selected months."
      )
    } else {
      decision <- paste(
        "Fail to reject H0 at the 5% significance level.",
        "There is insufficient statistical evidence of a difference in mean daily temperature."
      )
    }
    
    paste0(
      decision, "\n\n",
      "Mean temperature in ", input$month1, ": ", round(mean(result$first), 2), " °C\n",
      "Mean temperature in ", input$month2, ": ", round(mean(result$second), 2), " °C\n",
      "Estimated mean difference (first minus second): ", round(result$difference, 2), " °C\n",
      "p-value: ", signif(test$p.value, 5), "\n\n",
      "95% confidence interval for the mean difference: [",
      round(test$conf.int[1], 2), ", ", round(test$conf.int[2], 2), "] °C\n\n",
      "Interpretation: Compare the p-value with 0.05. ",
      "The conclusion depends on the assumptions of the test. Daily temperatures may be correlated across consecutive ",
      "days, so the independence assumption should be considered."
    )
  })
}

shinyApp(ui = ui, server = server)