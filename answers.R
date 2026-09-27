rm(list = ls())

# 0) Setup ----
packages <- c(
  "tidyverse", "lubridate",
  "fixest",    # FE regressions
  "mgcv",      # GAMs
  "broom", "modelsummary",
  "maps"       # world polygons
)

installed <- rownames(installed.packages())
for (p in packages) if (!p %in% installed) install.packages(p)
invisible(lapply(packages, library, character.only = TRUE))

theme_set(theme_minimal(base_size = 12))

dir.create("figures", showWarnings = FALSE)
dir.create("tables",  showWarnings = FALSE)

# OPTIONAL: If your CSVs are not in your working directory, set the path here:
# setwd("PATH/TO/FOLDER/WITH/CSVS")

# 1) Load data ----
hist <- read_csv("Historical.csv",
                 col_types = cols(
                   country = col_character(),
                   year    = col_integer(),
                   gdp     = col_double(),
                   temp    = col_double(),
                   prec    = col_double()
                 ))

fut <- read_csv("Future.csv",
                col_types = cols(
                  country  = col_character(),
                  year     = col_integer(),
                  scenario = col_character(),
                  temp     = col_double(),
                  prec     = col_double()
                ))

# 2) Basic transforms ----
# Centering temp/prec before squaring (fixes conditioning + reduces VCOV PSD issues)
temp_mean <- mean(hist$temp, na.rm = TRUE)
prec_mean <- mean(hist$prec, na.rm = TRUE)

hist <- hist %>%
  mutate(
    log_gdp = log(gdp),
    temp_c  = temp - temp_mean,
    prec_c  = prec - prec_mean,
    temp_c2 = temp_c^2,
    prec_c2 = prec_c^2,
    decade  = floor(year / 10) * 10,
    country_f = factor(country)
  )

# Quick sanity checks (optional)
print(colSums(is.na(hist[, c("log_gdp","temp","prec","country","year")])))

# 3) Exploratory visuals (prompt item 1) ----
p1 <- ggplot(hist, aes(temp, log_gdp)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Historical relationship: Temperature vs log(GDP per capita)",
    x = "Annual mean temperature (°C)",
    y = "log(GDP per capita, 2023 USD)"
  )
ggsave("figures/scatter_temp_loggdp.png", p1, width = 7, height = 5, dpi = 300)

p2 <- ggplot(hist, aes(prec, log_gdp)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Historical relationship: Precipitation vs log(GDP per capita)",
    x = "Annual mean precipitation (mm)",
    y = "log(GDP per capita, 2023 USD)"
  )
ggsave("figures/scatter_prec_loggdp.png", p2, width = 7, height = 5, dpi = 300)

# Optional: global averages over time (simple, unweighted)
global_ts <- hist %>%
  group_by(year) %>%
  summarize(
    mean_log_gdp = mean(log_gdp, na.rm = TRUE),
    mean_temp    = mean(temp, na.rm = TRUE),
    mean_prec    = mean(prec, na.rm = TRUE),
    .groups = "drop"
  )

p_ts1 <- ggplot(global_ts, aes(year, mean_temp)) +
  geom_line(linewidth = 1) +
  labs(title = "Global mean temperature over time", x = "Year", y = "Mean temp (°C)")
ggsave("figures/global_temp_timeseries.png", p_ts1, width = 7, height = 4.5, dpi = 300)

p_ts2 <- ggplot(global_ts, aes(year, mean_log_gdp)) +
  geom_line(linewidth = 1) +
  labs(title = "Global mean log(GDP per capita) over time", x = "Year", y = "Mean log GDPpc")
ggsave("figures/global_loggdp_timeseries.png", p_ts2, width = 7, height = 4.5, dpi = 300)

# 4) Geographic visualizations (prompt item 2) ----
# Compare early period vs late period: 1961-1970 vs 2011-2020
early <- hist %>% filter(year >= 1961, year <= 1970) %>%
  group_by(country) %>% summarize(
    gdp_e  = mean(gdp,  na.rm = TRUE),
    temp_e = mean(temp, na.rm = TRUE),
    prec_e = mean(prec, na.rm = TRUE),
    .groups = "drop"
  )

late <- hist %>% filter(year >= 2011, year <= 2020) %>%
  group_by(country) %>% summarize(
    gdp_l  = mean(gdp,  na.rm = TRUE),
    temp_l = mean(temp, na.rm = TRUE),
    prec_l = mean(prec, na.rm = TRUE),
    .groups = "drop"
  )

chg <- early %>%
  inner_join(late, by = "country") %>%
  mutate(
    dgdp_pct = 100 * (gdp_l / gdp_e - 1),
    dtemp = temp_l - temp_e,
    dprec = prec_l - prec_e
  )

world <- map_data("world")

# NOTE: Name matching between hist$country and map_data("world") region can be imperfect.
# We'll plot what matches; optionally you can create a small recode table if needed.
map_df <- world %>%
  left_join(chg, by = c("region" = "country"))

m1 <- ggplot(map_df, aes(long, lat, group = group, fill = dgdp_pct)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  coord_quickmap() +
  labs(title = "Change in GDP per capita (1960s → 2010s)", fill = "% change")
ggsave("figures/map_dgdp.png", m1, width = 10, height = 5, dpi = 300)

m2 <- ggplot(map_df, aes(long, lat, group = group, fill = dtemp)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  coord_quickmap() +
  labs(title = "Change in temperature (1960s → 2010s)", fill = "°C")
ggsave("figures/map_dtemp.png", m2, width = 10, height = 5, dpi = 300)

m3 <- ggplot(map_df, aes(long, lat, group = group, fill = dprec)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  coord_quickmap() +
  labs(title = "Change in precipitation (1960s → 2010s)", fill = "mm")
ggsave("figures/map_dprec.png", m3, width = 10, height = 5, dpi = 300)

# 5) Historical effects (prompt item 3) ----
# FE quadratic model: country + year fixed effects, clustered SEs by country
m_fe <- feols(
  log_gdp ~ temp_c + temp_c2 + prec_c + prec_c2 | country + year,
  data = hist,
  cluster = "country"
)

# GAM robustness model:
# - s(temp), s(prec): nonlinear climate response
# - s(year): smooth global time trend (instead of bs="re" for year)
# - s(country_f, bs="re"): country random intercept (absorbs time-invariant country differences)
m_gam <- gam(
  log_gdp ~ s(temp, bs = "cs") + s(prec, bs = "cs") +
    s(year, bs = "cs", k = 20) +
    s(country_f, bs = "re"),
  data = hist,
  method = "REML"
)

# Save model summaries/tables
# (HTML is easy to view; you can change output to "tables/model_fe.tex" if you prefer LaTeX.)
modelsummary(
  list("FE (centered quad + country/year FE)" = m_fe),
  output = "tables/model_fe.html"
)

# Save R objects for reproducibility
saveRDS(m_fe,  "tables/m_fe.rds")
saveRDS(m_gam, "tables/m_gam.rds")

# 6) Future projections (prompt item 4) ----
# We use the FE model's climate-response function (coefficients on centered climate terms)
# and compute impacts relative to baseline climate year 2021 for each country+scenario.

b <- coef(m_fe)

# helper: climate-only component (no fixed effects)
pred_climate_component <- function(temp, prec, b, temp_mean, prec_mean) {
  temp_c <- temp - temp_mean
  prec_c <- prec - prec_mean
  b["temp_c"]  * temp_c +
    b["temp_c2"] * (temp_c^2) +
    b["prec_c"]  * prec_c +
    b["prec_c2"] * (prec_c^2)
}

fut2 <- fut %>%
  mutate(
    climate_hat = pred_climate_component(temp, prec, b, temp_mean, prec_mean)
  ) %>%
  group_by(country, scenario) %>%
  mutate(
    # baseline: 2021 climate within each country+scenario
    climate_hat_base = climate_hat[year == 2021][1],
    dlog = climate_hat - climate_hat_base,
    pct_impact = 100 * (exp(dlog) - 1)
  ) %>%
  ungroup()

p_future <- ggplot(fut2, aes(year, pct_impact, linetype = scenario)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ country, scales = "free_y") +
  labs(
    title = "Projected climate-driven GDP impact (relative to 2021 climate)",
    x = "Year",
    y = "Estimated % change in GDP per capita (climate-driven)",
    linetype = "Scenario"
  )
ggsave("figures/future_pct_impact.png", p_future, width = 11, height = 7, dpi = 300)

# 7) Diagnostics (helpful for report credibility) ----
diag_df <- tibble(
  fitted = fitted(m_fe),
  resid  = resid(m_fe)
)

p_diag <- ggplot(diag_df, aes(fitted, resid)) +
  geom_point(alpha = 0.2, size = 0.6) +
  geom_hline(yintercept = 0) +
  labs(title = "FE model residuals vs fitted", x = "Fitted", y = "Residual")
ggsave("figures/diag_resid_fitted.png", p_diag, width = 7, height = 5, dpi = 300)

# Optional: GAM smooth plots
png("figures/gam_smooths.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1, 2))
plot(m_gam, select = 1, shade = TRUE, main = "s(temp)")
plot(m_gam, select = 2, shade = TRUE, main = "s(prec)")
dev.off()

# 8) Print quick summaries to console ----
cat("\n===== FE model summary =====\n")
print(summary(m_fe))

cat("\n===== GAM model summary =====\n")
print(summary(m_gam))

cat("\nDone. Figures saved to ./figures and tables saved to ./tables\n")

head(fut2)
tail(fut2)
summary(fut2$pct_impact)
