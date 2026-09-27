# ============================================================
# Initial exploration: load data, sanity checks, and quick EDA
# Goal: understand the structure, ranges, and basic relationships
# before fitting any models.
# ============================================================


library(tidyverse)
library(mgcv)
library(lmtest)
library(sandwich)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

# keep all initial EDA figures in one place so I can reuse them later
dir.create("figures_new/initial_analysis", showWarnings = FALSE, recursive = TRUE)

# Helper to save ggplot objects cleanly
save_eda_plot <- function(p, filename, w = 9, h = 6, dpi = 300) {
  ggsave(
    filename = file.path("figures_new/initial_analysis", filename),
    plot = p,
    width = w, height = h, dpi = dpi
  )
}

# load the historical dataset (used to fit models) and the future dataset
# (used later for climate scenario projections).
hist <- read_csv("Historical.csv")
fut  <- read_csv("Future.csv")

# quickly inspect the columns + types to confirm everything loaded correctly
glimpse(hist)
glimpse(fut)

# print summary stats to check ranges (e.g., temp/prec magnitude) and spot outliers
summary(hist)
summary(fut)

# verify there are no missing values (NA) that could silently break models later
colSums(is.na(hist))
colSums(is.na(fut))

# confirming the coverage of the panel in the historical data and what scenarios exist in the future data
hist %>%
  summarise(
    n_countries = n_distinct(country),
    min_year = min(year),
    max_year = max(year)
  )

fut %>%
  summarise(
    n_countries = n_distinct(country),
    min_year = min(year),
    max_year = max(year),
    scenarios = paste(unique(scenario), collapse = ", ")
  )

# plotting GDP and log(GDP) histograms to understand scale/skewness.
# Using log(GDP) often makes relationships closer to linear and reduces the impact of extreme values.
p_gdp_hist <- ggplot(hist, aes(x = gdp)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribution of GDP per capita",
    x = "GDP per capita",
    y = "Count"
  ) +
  theme_minimal() + theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10, face = "bold")
  )

p_loggdp_hist <- ggplot(hist, aes(x = log(gdp))) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribution of log(GDP per capita)",
    x = "log(GDP per capita)",
    y = "Count"
  ) +
  theme_minimal() + theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold")
  )

save_eda_plot(p_gdp_hist, "hist_gdp.png")
save_eda_plot(p_loggdp_hist, "hist_log_gdp.png")

# plotting raw GDP against climate variables just to see the untransformed relationship.
# I use transparency because there are many observations and points overlap heavily.
p_gdp_temp <- ggplot(hist, aes(x = temp, y = gdp)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "GDP vs Temperature",
    x = "Temperature (°C)",
    y = "GDP per capita"
  ) +
  theme_minimal() + theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10, face = "bold")
  )

p_gdp_prec <- ggplot(hist, aes(x = prec, y = gdp)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "GDP vs Precipitation",
    x = "Precipitation (mm)",
    y = "GDP per capita"
  ) +
  theme_minimal() + theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10, face = "bold")
  )

save_eda_plot(p_gdp_temp, "scatter_gdp_vs_temp.png")
save_eda_plot(p_gdp_prec, "scatter_gdp_vs_prec.png")

# plotting log(GDP) against climate variables because this is the scale I’ll model later.
p_loggdp_temp <- ggplot(hist, aes(x = temp, y = log(gdp))) +
  geom_point(alpha = 0.3) +
  labs(
    title = "log(GDP) vs Temperature",
    x = "Temperature (°C)",
    y = "log(GDP per capita)"
  ) +
  theme_minimal() + theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10, face = "bold")
  )

p_loggdp_prec <- ggplot(hist, aes(x = prec, y = log(gdp))) +
  geom_point(alpha = 0.3) +
  labs(
    title = "log(GDP) vs Precipitation",
    x = "Precipitation (mm)",
    y = "log(GDP per capita)"
  ) +
  theme_minimal() + theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10, face = "bold")
  )

save_eda_plot(p_loggdp_temp, "scatter_loggdp_vs_temp.png")
save_eda_plot(p_loggdp_prec, "scatter_loggdp_vs_prec.png")

# viewing the plots in my session too
p_gdp_hist
p_loggdp_hist
p_gdp_temp
p_gdp_prec
p_loggdp_temp
p_loggdp_prec

# ============================================================
# Historical climate and  GDP comparisons (visual)
# Here I plot how log(GDP per capita) relates to temperature and precipitation
# in the historical panel. I use both a scatter+LOESS view and a hexbin+LOESS
# view to handle overplotting.
# ============================================================


dir.create("figures_new/historical_comparisons", showWarnings = FALSE, recursive = TRUE)

save_histcomp_plot <- function(p, filename, w = 10, h = 6, dpi = 300) {
  ggsave(
    filename = file.path("figures_new/historical_comparisons", filename),
    plot = p,
    width = w, height = h, dpi = dpi
  )
}

# I add log(GDP) once so all plots reuse the same variable name consistently
hist_plot <- hist %>% mutate(log_gdp = log(gdp))

# 1) Scatter + LOESS (temp / prec)
make_scatter_loess <- function(df, xvar, xlab, subtitle_suffix = "Each point is a country-year observation; line is a LOESS smooth") {
  stopifnot(xvar %in% names(df))
  
  ggplot(df, aes(x = .data[[xvar]], y = log_gdp)) +
    geom_point(alpha = 0.15, size = 0.7) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(
      title = paste0("Historical relationship between ", xlab, " and GDP per capita"),
      subtitle = subtitle_suffix,
      x = xlab,
      y = "log(GDP per capita)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 10),
      plot.margin = margin(8, 8, 8, 8)
    )
}

p_scatter_temp <- make_scatter_loess(
  df = hist_plot,
  xvar = "temp",
  xlab = "Annual mean temperature (°C)"
)

p_scatter_prec <- make_scatter_loess(
  df = hist_plot,
  xvar = "prec",
  xlab = "Annual mean precipitation (mm)"
)

save_histcomp_plot(p_scatter_temp, "scatter_loess_loggdp_vs_temp.png", w = 10, h = 6)
save_histcomp_plot(p_scatter_prec, "scatter_loess_loggdp_vs_prec.png", w = 10, h = 6)

# 2) Hexbin + LOESS (temp / prec) 
# I use hexbin to make dense regions readable.

make_hex_loess <- function(df, xvar, xlab, bins = 40, subtitle_suffix = "Hexbin density with LOESS smooth") {
  stopifnot(xvar %in% names(df))
  
  ggplot(df, aes(x = .data[[xvar]], y = log_gdp)) +
    geom_hex(bins = bins) +
    geom_smooth(method = "loess", se = TRUE, color = "white") +
    labs(
      title = paste0("Historical relationship between ", xlab, " and GDP per capita"),
      subtitle = subtitle_suffix,
      x = xlab,
      y = "log(GDP per capita)",
      fill = "Obs\nper bin"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

p_hex_temp <- make_hex_loess(
  df = hist_plot,
  xvar = "temp",
  xlab = "Annual mean temperature (°C)",
  bins = 40
)

p_hex_prec <- make_hex_loess(
  df = hist_plot,
  xvar = "prec",
  xlab = "Annual mean precipitation (mm)",
  bins = 40
)

save_histcomp_plot(p_hex_temp, "hex_loess_loggdp_vs_temp.png", w = 10, h = 6)
save_histcomp_plot(p_hex_prec, "hex_loess_loggdp_vs_prec.png", w = 10, h = 6)

# View in-session
p_scatter_temp
p_scatter_prec
p_hex_temp
p_hex_prec

# ============================================================
# Geographic visualizations + coverage check (choropleths)
# In this section I first check which 10-year early window gives the
# largest overlap of countries with the fixed late window (2011–2020).
# Then I map country-level changes (late minus early) for:
#   (1) Delta log(GDP per capita), (2) Delta temperature, (3) Delta precipitation
# for multiple early windows:
#   1961–1970, 1971–1980, 1981–1990, 1991–2000
# I keep the map styling consistent and save everything to figures_new/geo_visuals/.
# ============================================================

dir.create("figures_new/geo_visuals", showWarnings = FALSE, recursive = TRUE)

save_geo_plot <- function(p, filename, w = 11, h = 6.5, dpi = 300) {
  ggsave(
    filename = file.path("figures_new/geo_visuals", filename),
    plot = p,
    width = w, height = h, dpi = dpi
  )
}

# 1) Coverage table (rolling early windows vs fixed late window)

count_countries_both_windows <- function(df, early_start, early_end, late_start, late_end) {
  early_c <- df %>%
    filter(year >= early_start, year <= early_end) %>%
    distinct(country) %>%
    pull(country)
  
  late_c <- df %>%
    filter(year >= late_start, year <= late_end) %>%
    distinct(country) %>%
    pull(country)
  
  length(intersect(early_c, late_c))
}

late_start <- 2011
late_end   <- 2020
window_len <- 10

early_min <- floor(min(hist$year, na.rm = TRUE))
early_max <- floor(max(hist$year, na.rm = TRUE))

early_starts <- early_min:(early_max - window_len + 1)

coverage_tbl <- tibble(
  early_start = early_starts,
  early_end   = early_starts + window_len - 1
) %>%
  mutate(
    late_start = late_start,
    late_end   = late_end,
    n_countries_both = pmap_int(
      list(early_start, early_end),
      ~ count_countries_both_windows(hist, ..1, ..2, late_start, late_end)
    )
  ) %>%
  arrange(desc(n_countries_both), early_start)

# I save this so I can reference it in the writeup
write_csv(coverage_tbl, file.path("figures_new/geo_visuals", "coverage_table_early_vs_2011_2020.csv"))


# 2) Prep: world map + country name harmonization

world <- ne_countries(scale = "medium", returnclass = "sf")

# I keep the country-name cleanup in one place so every window reuses it
fix_country_names_for_map <- function(df) {
  df %>%
    mutate(
      country_map = recode(
        country,
        "Bahamas, The"                    = "Bahamas",
        "Central African Republic"        = "Central African Rep.",
        "Congo, Dem. Rep."                = "Dem. Rep. Congo",
        "Congo, Rep."                     = "Congo",
        "Cote d'Ivoire"                   = "Côte d'Ivoire",
        "Dominican Republic"              = "Dominican Rep.",
        "Egypt, Arab Rep."                = "Egypt",
        "Gambia, The"                     = "Gambia",
        "Hong Kong SAR, China"            = "Hong Kong",
        "Iran, Islamic Rep."              = "Iran",
        "Korea, Rep."                     = "South Korea",
        "Micronesia, Fed. Sts."           = "Micronesia",
        "Puerto Rico (US)"                = "Puerto Rico",
        "Sao Tome and Principe"           = "São Tomé and Príncipe",
        "Somalia, Fed. Rep."              = "Somalia",
        "St. Vincent and the Grenadines"  = "Saint Vincent and the Grenadines",
        "Syrian Arab Republic"            = "Syria",
        "Turkiye"                         = "Turkey",
        "United States"                   = "United States of America",
        "Venezuela, RB"                   = "Venezuela",
        "Eswatini"                        = "eSwatini",
        "French Polynesia"                = "Fr. Polynesia",
        "Marshall Islands"                = "Marshall Is.",
        "St. Lucia"                       = "Saint Lucia",
        "Antigua and Barbuda"             = "Antigua and Barb.",
        "Brunei Darussalam"               = "Brunei",
        "Equatorial Guinea"               = "Eq. Guinea",
        "Solomon Islands"                 = "Solomon Is.",
        "Bosnia and Herzegovina"          = "Bosnia and Herz.",
        "Kyrgyz Republic"                 = "Kyrgyzstan",
        "Lao PDR"                         = "Laos",
        "Macao SAR, China"                = "Macao",
        "Russian Federation"              = "Russia",
        "Slovak Republic"                 = "Slovakia",
        "Viet Nam"                        = "Vietnam",
        "Yemen, Rep."                     = "Yemen",
        "West Bank and Gaza"              = "Palestine",
        .default = country
      )
    ) %>%
    mutate(
      country_map = case_when(
        grepl("Sao Tome", country, ignore.case = TRUE) ~ "São Tomé and Principe",
        grepl("Vincent",  country, ignore.case = TRUE) ~ "St. Vin. and Gren.",
        grepl("Curacao",  country, ignore.case = TRUE) ~ "Curaçao",
        TRUE ~ country_map
      )
    )
}

# A consistent theme so all maps look coherent across periods
map_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.title.position = "plot",
    plot.caption = element_text(size = 12, face = "bold", hjust = 0.7),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width  = unit(0.35, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(8, 8, 8, 8)
  )

# 3) Function: compute deltas + build 3 maps for any early window
build_country_changes <- function(df, early_start, early_end, late_start, late_end) {
  df2 <- df %>%
    mutate(
      log_gdp = log(gdp),
      period = case_when(
        year >= early_start & year <= early_end ~ "early",
        year >= late_start  & year <= late_end  ~ "late",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(period))
  
  country_summary <- df2 %>%
    group_by(country, period) %>%
    summarise(
      mean_log_gdp = mean(log_gdp, na.rm = TRUE),
      mean_temp    = mean(temp,    na.rm = TRUE),
      mean_prec    = mean(prec,    na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from  = period,
      values_from = c(mean_log_gdp, mean_temp, mean_prec)
    ) %>%
    mutate(
      d_log_gdp = mean_log_gdp_late - mean_log_gdp_early,
      d_temp    = mean_temp_late    - mean_temp_early,
      d_prec    = mean_prec_late    - mean_prec_early
    )
  
  country_changes <- country_summary %>%
    filter(!is.na(d_log_gdp), !is.na(d_temp), !is.na(d_prec))
  
  country_changes
}

make_maps_for_window <- function(early_start, early_end, late_start = 2011, late_end = 2020) {
  # I compute the early-vs-late deltas at the country level
  country_changes <- build_country_changes(hist, early_start, early_end, late_start, late_end)
  
  # I fix names once and join to the world polygons
  country_changes_fixed <- fix_country_names_for_map(country_changes)
  
  world_changes <- world %>%
    left_join(country_changes_fixed, by = c("name" = "country_map")) %>%
    filter(name != "Antarctica")
  
  # Titles/captions with explicit periods
  ttl_suffix <- paste0(early_start, "–", early_end, " vs ", late_start, "–", late_end)
  cap_text   <- paste0("Country-level averages computed for ", early_start, "–", early_end,
                       " (early) and ", late_start, "–", late_end, " (late).")
  
  # delta log(GDP)
  p_gdp <- ggplot(world_changes) +
    geom_sf(aes(fill = d_log_gdp), color = "gray80", size = 0.1) +
    scale_fill_gradient2(name = "Delta log(GDP per capita)", midpoint = 0) +
    labs(
      title   = paste0("Change in GDP per capita (log scale), ", ttl_suffix),
      caption = cap_text
    ) +
    map_theme
  
  # Delta temperature
  p_temp <- ggplot(world_changes) +
    geom_sf(aes(fill = d_temp), color = "gray80", size = 0.1) +
    scale_fill_gradient2(name = "Delta Temperature (°C)", midpoint = 0) +
    labs(
      title   = paste0("Change in annual mean temperature, ", ttl_suffix),
      caption = cap_text
    ) +
    map_theme
  
  # Delta precipitation
  p_prec <- ggplot(world_changes) +
    geom_sf(aes(fill = d_prec), color = "gray80", size = 0.1) +
    scale_fill_gradient2(
      name = "Delta Precipitation (mm)",
      midpoint = 0,
      limits = c(-1500, 1500),
      oob = squish
    ) +
    labs(
      title   = paste0("Change in annual mean precipitation, ", ttl_suffix),
      caption = cap_text
    ) +
    map_theme
  
  # I save all three maps
  tag <- paste0("early_", early_start, "_", early_end, "_vs_late_", late_start, "_", late_end)
  save_geo_plot(p_gdp,  paste0("map_delta_log_gdp_", tag, ".png"))
  save_geo_plot(p_temp, paste0("map_delta_temp_",    tag, ".png"))
  save_geo_plot(p_prec, paste0("map_delta_prec_",    tag, ".png"))
  
  list(p_gdp = p_gdp, p_temp = p_temp, p_prec = p_prec)
}


# 4) Make maps for all windows
maps_1961 <- make_maps_for_window(1961, 1970, late_start, late_end)
maps_1971 <- make_maps_for_window(1971, 1980, late_start, late_end)
maps_1981 <- make_maps_for_window(1981, 1990, late_start, late_end)
maps_1991 <- make_maps_for_window(1991, 2000, late_start, late_end)

# view them in-session
maps_1961$p_gdp; maps_1961$p_temp; maps_1961$p_prec
maps_1971$p_gdp; maps_1971$p_temp; maps_1971$p_prec
maps_1981$p_gdp; maps_1981$p_temp; maps_1981$p_prec
maps_1991$p_gdp; maps_1991$p_temp; maps_1991$p_prec

# ============================================================
# Modelling (baseline -> quadratic -> fixed effects -> GAM)
# In this section I fit a sequence of models to explain log(GDP per capita)
# using temperature and precipitation. I start simple (linear), add curvature
# (quadratic), then add fixed effects (country + year), and finally use a GAM
# to flexibly capture nonlinear climate relationships while still controlling
# for country and year.
# ============================================================


dir.create("figures_new/model_plots", showWarnings = FALSE, recursive = TRUE)

# I save every plot I care about into the same folder for clean report reuse.
save_model_plot <- function(p, filename, w = 11, h = 6.5, dpi = 300) {
  ggsave(
    filename = file.path("figures_new/model_plots", filename),
    plot = p,
    width = w, height = h, dpi = dpi
  )
}


# 1) Fit the model ladder

# Baseline linear model (sanity check: linear climate effects only)
lm_baseline <- lm(log(gdp) ~ temp + prec, data = hist)
summary(lm_baseline)

# Quadratic model (adds curvature without fixed effects)
lm_quad <- lm(log(gdp) ~ temp + I(temp^2) + prec + I(prec^2), data = hist)
summary(lm_quad)

# Quadratic + country fixed effects (controls for time-invariant country differences)
lm_fe_quad <- lm(
  log(gdp) ~ temp + I(temp^2) + prec + I(prec^2) + factor(country),
  data = hist
)
summary(lm_fe_quad)

# Quadratic + country + year fixed effects (main parametric FE benchmark)
lm_fe_quad_year <- lm(
  log(gdp) ~ temp + I(temp^2) + prec + I(prec^2) + factor(country) + factor(year),
  data = hist
)
summary(lm_fe_quad_year)

# Cluster-robust inference (clusters at country level)
coeftest(lm_fe_quad_year, vcov = vcovCL, cluster = ~ country)

# GAM with country + year FE (main flexible model candidate)
gam_fe_year_k10 <- gam(
  log(gdp) ~ s(temp, k = 10) + s(prec, k = 10) + factor(country) + factor(year),
  data = hist,
  method = "REML"
)
summary(gam_fe_year_k10)

# I check if k=10 is too restrictive for the smooths
mgcv::gam.check(gam_fe_year_k10)

# Refit with higher basis dimension to avoid underfitting the smooths
gam_fe_year_k20 <- gam(
  log(gdp) ~ s(temp, k = 20) + s(prec, k = 20) + factor(country) + factor(year),
  data = hist,
  method = "REML"
)
summary(gam_fe_year_k20)
mgcv::gam.check(gam_fe_year_k20)

#gam_fe_year_k15 <- gam(
#  log(gdp) ~ s(temp, k = 15) + s(prec, k = 15) + factor(country) + factor(year),
#  data = hist,
#  method = "REML"
#)
##summary(gam_fe_year_k15)
#mgcv::gam.check(gam_fe_year_k15)

# ----------------------------
# 2) Model comparison table (the three models I care about most)
# I compare:
#   (a) FE quadratic (country + year)  - interpretable benchmark
#   (b) GAM FE k=10                   - flexible but maybe k too small
#   (c) GAM FE k=20                   - flexible and passes k-check
# ----------------------------

# numbers I’ll likely cite in the report to justify why k=20 and not k=10
AIC(lm_fe_quad_year, gam_fe_year_k10, gam_fe_year_k20)
BIC(lm_fe_quad_year, gam_fe_year_k10, gam_fe_year_k20)


# 3) GAM smooth plots (k=10 vs k=20) with consistent y-limits
# visual to justify why I moved from k=10 to k=20.

# I pull the smooth contributions and set a common y-range for comparability
smooth_temp_k10 <- plot(gam_fe_year_k10, select = 1, pages = 0)
smooth_temp_k20 <- plot(gam_fe_year_k20, select = 1, pages = 0)
smooth_prec_k10 <- plot(gam_fe_year_k10, select = 2, pages = 0)
smooth_prec_k20 <- plot(gam_fe_year_k20, select = 2, pages = 0)

# I just use a manual common range that worked well earlier in my analysis.
common_ylim <- c(-2, 1)

# Save the 2x2 smooth comparison as one image
png("figures_new/model_plots/gam_smooths_k10_vs_k20.png", width = 1800, height = 1400, res = 200)
#par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
par(mfrow = c(2, 2), mar = c(4.5, 5, 3, 1), cex.lab = 1.3, cex.main = 1.3, font.lab = 2, font.main = 2, cex.axis = 1.1)
plot(gam_fe_year_k10, select = 1, ylim = common_ylim, se = TRUE, rug = TRUE, main = "s(temp), k=10")
plot(gam_fe_year_k20, select = 1, ylim = common_ylim, se = TRUE, rug = TRUE, main = "s(temp), k=20")
plot(gam_fe_year_k10, select = 2, ylim = common_ylim, se = TRUE, rug = TRUE, main = "s(prec), k=10")
plot(gam_fe_year_k20, select = 2, ylim = common_ylim, se = TRUE, rug = TRUE, main = "s(prec), k=20")

dev.off()


# 4) Residual diagnostics for the three main models
# I use residual vs fitted + QQ, so I can quickly check fit quality and see if i want to use them in report.

make_resid_df <- function(model_obj, model_name) {
  tibble(
    fitted = fitted(model_obj),
    resid  = residuals(model_obj),
    model  = model_name
  )
}

resid_df <- bind_rows(
  make_resid_df(lm_fe_quad_year, "FE quadratic (country+year)"),
  make_resid_df(gam_fe_year_k10, "GAM FE (k=10)"),
  make_resid_df(gam_fe_year_k20, "GAM FE (k=20)")
)

p_resid <- ggplot(resid_df, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ model, scales = "free_x") +
  labs(
    title = "Residuals vs fitted values (diagnostic check)",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

p_resid
save_model_plot(p_resid, "diagnostic_residuals_vs_fitted.png", w = 12, h = 7)

## QQ plots to check if they violate normal distribution or not
png("figures_new/model_plots/diagnostic_qqplots.png", width = 1800, height = 1400, res = 200)

layout(matrix(c(1, 2,3, 3), nrow = 2, byrow = TRUE))

par(mar = c(4, 4, 3, 1))

qqnorm(residuals(lm_fe_quad_year), main = "QQ: FE quadratic (country+year)")
qqline(residuals(lm_fe_quad_year))

qqnorm(residuals(gam_fe_year_k10), main = "QQ: GAM FE (k=10)")
qqline(residuals(gam_fe_year_k10))

qqnorm(residuals(gam_fe_year_k20), main = "QQ: GAM FE (k=20)")
qqline(residuals(gam_fe_year_k20))

dev.off()



# ============================================================
# Future projections (climate-only GDP impacts)
# In this section I use the fitted GAM (with country+year fixed effects)
# to project climate-driven changes in log(GDP) under future scenarios.
# I only keep the smooth components s(temp) and s(prec) and exclude the
# country/year fixed effects so that the projection reflects climate alone.
# ============================================================

dir.create("figures_new/future_analysis", showWarnings = FALSE, recursive = TRUE)

# I save figures into figures_new/future_analysis so they are easy to reuse later.
save_future_plot <- function(p, filename, w = 11, h = 7, dpi = 300) {
  ggsave(
    filename = file.path("figures_new/future_analysis", filename),
    plot = p,
    width = w, height = h, dpi = dpi
  )
}

# 1) Build prediction datasets for the climate smooths only

# Build a baseline climate (e.g. 2011–2020)
baseline <- hist %>%
  filter(year >= 2011, year <= 2020) %>%
  group_by(country) %>%
  summarise(
    temp_base = mean(temp, na.rm = TRUE),
    prec_base = mean(prec, na.rm = TRUE),
    .groups = "drop"
  )

baseline

# Merge baseline into future data
fut2 <- fut %>%
  left_join(baseline, by = "country")

fut2

stopifnot(!any(is.na(fut2$temp_base)))
stopifnot(!any(is.na(fut2$prec_base)))

# I grab the factor levels that were used when fitting the GAM so I can
# construct valid newdata (even though I will exclude the FE in prediction).
mf <- model.frame(gam_fe_year_k20)

country_levels <- levels(mf$`factor(country)`)
year_levels    <- levels(mf$`factor(year)`)

# I use any training-valid levels as dummies to satisfy mgcv's predict().
dummy_country <- country_levels[1]
dummy_year    <- year_levels[1]

# I create a "baseline climate" dataset and a "future climate" dataset.
# The only variables that matter for the smooth terms are temp and prec.
new_base <- fut2 %>%
  transmute(
    temp    = temp_base,
    prec    = prec_base,
    country = factor(dummy_country, levels = country_levels),
    year    = factor(dummy_year,    levels = year_levels)
  )

new_future <- fut2 %>%
  transmute(
    temp    = temp,
    prec    = prec,
    country = factor(dummy_country, levels = country_levels),
    year    = factor(dummy_year,    levels = year_levels)
  )

new_base
new_future

# 2) Predict smooth terms and compute climate-only GDP impacts

# I predict term-by-term contributions and explicitly exclude FE terms.
pred_base_terms <- predict(
  gam_fe_year_k20,
  newdata  = new_base,
  type     = "terms",
  exclude  = c("factor(country)", "factor(year)")
)

pred_future_terms <- predict(
  gam_fe_year_k20,
  newdata  = new_future,
  type     = "terms",
  exclude  = c("factor(country)", "factor(year)")
)

pred_base_terms
pred_future_terms
# I store the climate-only delta in log GDP and convert it into a % change.
# This is: delta log(GDP) = [s(temp)_future - s(temp)_base] + [s(prec)_future - s(prec)_base]
fut2 <- fut2 %>%
  mutate(
    d_log_gdp_climate =
      (pred_future_terms[, "s(temp)"] - pred_base_terms[, "s(temp)"]) +
      (pred_future_terms[, "s(prec)"] - pred_base_terms[, "s(prec)"]),
    pct_change_gdp = 100 * (exp(d_log_gdp_climate) - 1)
  )

# Quick checks so I can check magnitudes before plotting
colnames(pred_base_terms)
summary(fut2$d_log_gdp_climate)
summary(fut2$pct_change_gdp)


# 3) Aggregate paths (country panels + global average + key years)
# I aggregate to a clean country-scenario-year panel
path_country <- fut2 %>%
  group_by(country, scenario, year) %>%
  summarise(
    dlog = mean(d_log_gdp_climate, na.rm = TRUE),
    pct  = mean(pct_change_gdp,   na.rm = TRUE),
    .groups = "drop"
  )

# I extract a few report years for bar charts (2050/2070/2100).
key_years <- c(2050, 2070, 2100)

impact_key <- path_country %>%
  filter(year %in% key_years) %>%
  arrange(scenario, year, country)

# Global mean path (averaged across countries in my future dataset).
path_global <- path_country %>%
  group_by(scenario, year) %>%
  summarise(
    avg_pct  = mean(pct,  na.rm = TRUE),
    avg_dlog = mean(dlog, na.rm = TRUE),
    .groups  = "drop"
  )


# 4) Plot 1: Global average path

p_global <- ggplot(path_global, aes(x = year, y = avg_pct, color = scenario)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Projected climate-only impact on GDP (global average)",
    y = "% change in GDP (relative to baseline climate)",
    x = "Year",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.8),
    axis.title = element_text(size = 12, face = "bold")
  )

p_global
save_future_plot(p_global, "global_avg_climate_impact.png", w = 10, h = 6)


# 5) Plot 2: Country panels (sample-style layout, clean x spacing)

# I keep a consistent scenario ordering so colors and legends stay stable.
path_country_plot <- path_country %>%
  mutate(
    scenario = factor(scenario, levels = c("SSP1-2.6", "SSP5-8.5")),
    year     = as.numeric(year)
  )

p_country_panels <- ggplot(
  path_country_plot,
  aes(x = year, y = pct, color = scenario)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6, color = "grey35") +
  geom_line(linewidth = 1) +
  facet_wrap(~ country, scales = "free_y") +
  scale_color_manual(values = c("SSP1-2.6" = "#F8766D", "SSP5-8.5" = "#00BFC4")) +
  scale_x_continuous(
    breaks = seq(2020, 2100, by = 20),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title = "Projected climate-driven GDP impact (relative to baseline climate)",
    subtitle = "Climate-only effect from GAM smooth terms; country/year fixed effects excluded",
    x = "Year",
    y = "Estimated % change in GDP per capita (climate-driven)",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.8),
    axis.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "bold", hjust = 0.6),
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p_country_panels
save_future_plot(p_country_panels, "future_pct_impact_country_panels.png", w = 11, h = 7)

# 6) Plot 3: 2100 country impacts (bar chart)

impact_2100 <- impact_key %>% filter(year == 2100)

p_2100 <- ggplot(impact_2100, aes(x = reorder(country, pct), y = pct, fill = scenario)) +
  geom_col(position = "dodge") +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Projected climate-only % GDP impact in 2100",
    y = "% change in GDP",
    x = "Country",
    fill = "Scenario"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.8),
    axis.title = element_text(size = 12, face = "bold")
  )

p_2100
save_future_plot(p_2100, "impact_2100_by_country.png", w = 9, h = 6)


