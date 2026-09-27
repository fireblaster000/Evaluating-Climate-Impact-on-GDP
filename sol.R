# Load libraries
library(tidyverse)

# Read data
hist <- read_csv("Historical.csv")
fut  <- read_csv("Future.csv")

# Basic structure
glimpse(hist)
glimpse(fut)

# Summary statistics
summary(hist)
summary(fut)

# Check missing values
colSums(is.na(hist))
colSums(is.na(fut))

# How many countries and years?
hist %>% summarise(
  n_countries = n_distinct(country),
  min_year = min(year),
  max_year = max(year)
)

fut %>% summarise(
  n_countries = n_distinct(country),
  min_year = min(year),
  max_year = max(year),
  scenarios = paste(unique(scenario), collapse = ", ")
)


# Histograms to check scales
ggplot(hist, aes(x = gdp)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of GDP per capita", x = "GDP per capita", y = "Count")

ggplot(hist, aes(x = log(gdp))) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of log(GDP per capita)", x = "log(GDP per capita)", y = "Count")

# Scatter: raw GDP
ggplot(hist, aes(x = temp, y = gdp)) +
  geom_point(alpha = 0.3) +
  labs(title = "GDP vs Temperature", x = "Temperature (°C)", y = "GDP per capita")

ggplot(hist, aes(x = prec, y = gdp)) +
  geom_point(alpha = 0.3) +
  labs(title = "GDP vs Precipitation", x = "Precipitation (mm)", y = "GDP per capita")

# Scatter: log GDP
ggplot(hist, aes(x = temp, y = log(gdp))) +
  geom_point(alpha = 0.3) +
  labs(title = "log(GDP) vs Temperature", x = "Temperature (°C)", y = "log(GDP per capita)")

ggplot(hist, aes(x = prec, y = log(gdp))) +
  geom_point(alpha = 0.3) +
  labs(title = "log(GDP) vs Precipitation", x = "Precipitation (mm)", y = "log(GDP per capita)")

## geographic visualizations data analysis

hist2 <- hist %>%
  mutate(log_gdp = log(gdp),
         period = case_when(
           year <= 1970 ~ "early",
           year >= 2011 ~ "late",
           TRUE ~ NA_character_
         )) %>%
  filter(!is.na(period))

country_summary <- hist2 %>%
  group_by(country, period) %>%
  summarise(
    mean_log_gdp = mean(log_gdp),
    mean_temp = mean(temp),
    mean_prec = mean(prec),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = c(mean_log_gdp, mean_temp, mean_prec)
  ) %>%
  mutate(
    d_log_gdp = mean_log_gdp_late - mean_log_gdp_early,
    d_temp = mean_temp_late - mean_temp_early,
    d_prec = mean_prec_late - mean_prec_early
  )

# Inspect the changes
summary(country_summary$d_log_gdp)
summary(country_summary$d_temp)
summary(country_summary$d_prec)

head(country_summary)

# want to check how many countries remain for the early to late period if I filter out the countries with NA

country_changes <- country_summary %>%
  filter(
    !is.na(d_log_gdp),
    !is.na(d_temp),
    !is.na(d_prec)
  )

# How many countries left?
nrow(country_changes)

# Quick summaries after filtering
summary(country_changes$d_log_gdp)
summary(country_changes$d_temp)
summary(country_changes$d_prec)

# Peek at a few rows
head(country_changes)

# before plotting choropleth check if countries name differ
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Inspect name column to see how countries are labeled
names(world)

# Join with our changes data
world_changes <- world %>%
  left_join(country_changes, by = c("name" = "country"))

# Check how many matched
sum(!is.na(world_changes$d_log_gdp))
nrow(world_changes)

# Countries in the data that did NOT match the map's "name" so we know what to recode
unmatched <- anti_join(
  country_changes %>% select(country),
  world %>% st_drop_geometry() %>% select(name),
  by = c("country" = "name")
) %>% distinct()

unmatched
nrow(unmatched)

# recoding countries name to resolve the mismatch
country_changes_fixed <- country_changes %>%
  mutate(country_map = recode(country,
                              "Bahamas, The" = "Bahamas",
                              "Central African Republic" = "Central African Rep.",
                              "Congo, Dem. Rep." = "Dem. Rep. Congo",
                              "Congo, Rep." = "Congo",
                              "Cote d'Ivoire" = "Côte d'Ivoire",
                              "Dominican Republic" = "Dominican Rep.",
                              "Egypt, Arab Rep." = "Egypt",
                              "Gambia, The" = "Gambia",
                              "Hong Kong SAR, China" = "Hong Kong",
                              "Iran, Islamic Rep." = "Iran",
                              "Korea, Rep." = "South Korea",
                              "Micronesia, Fed. Sts." = "Micronesia",
                              "Puerto Rico (US)" = "Puerto Rico",
                              "Sao Tome and Principe" = "São Tomé and Príncipe",
                              "Somalia, Fed. Rep." = "Somalia",
                              "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines",
                              "Syrian Arab Republic" = "Syria",
                              "Turkiye" = "Turkey",
                              "United States" = "United States of America",
                              "Venezuela, RB" = "Venezuela",
                              "Eswatini" = "eSwatini",
                              "French Polynesia" = "Fr. Polynesia",
                              "Marshall Islands" = "Marshall Is.",
                              "São Tomé and Príncipe" = "São Tomé and Principe",
                              "Saint Vincent and the Grenadines" = "St. Vin. and Gren.",
                              .default = country
  ))

# Re-join to world map
world_changes <- world %>%
  left_join(country_changes_fixed, by = c("name" = "country_map"))

# Check matches again
sum(!is.na(world_changes$d_log_gdp))
nrow(world_changes)

# still 5 are unmatched
remaining_unmatched <- anti_join(
  country_changes_fixed %>% select(country, country_map),
  world %>% st_drop_geometry() %>% select(name),
  by = c("country_map" = "name")
) %>% distinct()

remaining_unmatched
nrow(remaining_unmatched)

# resolving the two countries which had special character issues
country_changes_fixed <- country_changes_fixed %>%
  mutate(country_map = case_when(
    grepl("Sao Tome", country, ignore.case = TRUE) ~ "São Tomé and Principe",
    grepl("Vincent", country, ignore.case = TRUE) ~ "St. Vin. and Gren.",
    TRUE ~ country_map
  ))
world_changes <- world %>%
  left_join(country_changes_fixed, by = c("name" = "country_map"))
sum(!is.na(world_changes$d_log_gdp))

remaining_unmatched <- anti_join(
  country_changes_fixed %>% select(country, country_map),
  world %>% st_drop_geometry() %>% select(name),
  by = c("country_map" = "name")
) %>% distinct()

remaining_unmatched
nrow(remaining_unmatched)


## plotting the choropleths now

### Log gdp per capita choropleth
library(ggplot2)

ggplot(world_changes %>% dplyr::filter(name != "Antarctica")) +
  geom_sf(aes(fill = d_log_gdp), color = "gray80", size = 0.1) +
  scale_fill_gradient2(
    name = "Δ log(GDP per capita)",
    midpoint = 0
  ) +
  labs(
    title = "Change in GDP per capita (log scale), 1961–1970 vs 2011–2020",
    caption = "Country-level averages computed for 1961–1970 (early) and 2011–2020 (late)."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.title.position = "plot",
    plot.caption = element_text(size = 9, hjust = -4),
    
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width  = unit(0.3, "cm"),
    
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
  )


### temperature change choropleth

ggplot(world_changes %>% dplyr::filter(name != "Antarctica")) +
  geom_sf(aes(fill = d_temp), color = "gray80", size = 0.1) +
  scale_fill_gradient2(
    name = "Δ Temperature (°C)\n(2011–20 minus 1961–70)",
    midpoint = 0
  ) +
  labs(
    title = "Change in annual mean temperature, 1961–1970 vs 2011–2020",
    caption = "Country-level averages computed for 1961–1970 (early) and 2011–2020 (late)."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width  = unit(0.4, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )


library(ggplot2)
library(dplyr)

# 1) Common map base (same projection/layout)
world_changes_no_ant <- world_changes %>%
  filter(name != "Antarctica")

# 2) Define ONE shared theme (applies to every map)
map_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.title.position = "plot",
    plot.caption = element_text(size = 9, hjust = -12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.height = unit(0.7, "cm"),
    legend.key.width  = unit(0.3, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
  )

# --- MAP 1: Δ log(GDP) ---
p_gdp <- ggplot(world_changes_no_ant) +
  geom_sf(aes(fill = d_log_gdp), color = "gray80", size = 0.1) +
  scale_fill_gradient2(name = "Δ log(GDP per capita)", midpoint = 0) +
  labs(
    title = "Change in GDP per capita (log scale), 1961–1970 vs 2011–2020",
    caption = "Country-level averages computed for 1961–1970 (early) and 2011–2020 (late)."
  ) +
  map_theme

# --- MAP 2: Δ Temperature ---
p_temp <- ggplot(world_changes_no_ant) +
  geom_sf(aes(fill = d_temp), color = "gray80", size = 0.1) +
  scale_fill_gradient2(name = "Δ Temperature (°C)", midpoint = 0) +
  labs(
    title = "Change in annual mean temperature, 1961–1970 vs 2011–2020",
    caption = "Country-level averages computed for 1961–1970 (early) and 2011–2020 (late)."
  ) +
  map_theme

# --- MAP 3: Δ Precipitation ---
p_prec <- ggplot(world_changes_no_ant) +
  geom_sf(aes(fill = d_prec), color = "gray80", size = 0.1) +
  scale_fill_gradient2(
    name = "Δ Precipitation (mm)",
    midpoint = 0,
    limits = c(-1500, 1500),
    oob = scales::squish
  ) +
  labs(
    title = "Change in annual mean precipitation, 1961–1970 vs 2011–2020",
    caption = "Country-level averages computed for 1961–1970 (early) and 2011–2020 (late)."
  ) +
  map_theme

# Print maps 
p_gdp
p_temp
p_prec


### Base scatter plot for gdp and temp

hist_plot <- hist %>%
  mutate(log_gdp = log(gdp))

p1 <- ggplot(hist_plot, aes(x = temp, y = log_gdp)) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Historical relationship between temperature and GDP per capita",
    subtitle = "Each point is a country-year observation (1961–2020); line is a LOESS smooth",
    x = "Annual mean temperature (°C)",
    y = "log(GDP per capita, 2023 USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 10),
    plot.margin = margin(5, 5, 5, 5)
  )

p1

p_hex <- ggplot(hist_plot, aes(x = temp, y = log_gdp)) +
  geom_hex(bins = 40) +
  geom_smooth(method = "loess", se = TRUE, color = "white") +
  labs(
    title = "Historical relationship between temperature and GDP per capita",
    subtitle = "Hexbin density with LOESS smooth (1961–2020)",
    x = "Annual mean temperature (°C)",
    y = "log(GDP per capita, 2023 USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 10),
    plot.margin = margin(5, 5, 5, 5)
  )

p_hex

library(tidyverse)

# Make sure log_gdp exists
hist_plot <- hist %>%
  mutate(log_gdp = log(gdp))

plot_loggdp_vs_climate <- function(df, xvar, xlab, bins = 40) {
  stopifnot(xvar %in% names(df))
  
  ggplot(df, aes(x = .data[[xvar]], y = log_gdp)) +
    geom_hex(bins = bins) +
    geom_smooth(method = "loess", se = TRUE, color = "white") +
    labs(
      title = paste0("Historical relationship between ", xlab, " and GDP per capita"),
      subtitle = "Hexbin density with LOESS smooth (1961–2020)",
      x = xlab,
      y = "log(GDP per capita, 2023 USD)"
    ) +
    scale_fill_continuous(name = "Observations\nper bin") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.2),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      axis.title = element_text(size = 10),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# ---- Use it for both plots ----
p_temp <- plot_loggdp_vs_climate(
  df = hist_plot,
  xvar = "temp",
  xlab = "Annual mean temperature (°C)"
)

p_prec <- plot_loggdp_vs_climate(
  df = hist_plot,
  xvar = "prec",
  xlab = "Annual mean precipitation (mm)"
)

p_temp
p_prec


## Start of modelling
# Baseline linear model: linear effects only
lm_baseline <- lm(log(gdp) ~ temp + prec, data = hist)

# Summary of the model
summary(lm_baseline)


# Quadratic model: allow nonlinear effects
lm_quad <- lm(log(gdp) ~ temp + I(temp^2) + prec + I(prec^2), data = hist)

# Summary of the model
summary(lm_quad)

# Country Fixed Effects Quadratic Mode;
lm_fe_quad <- lm(log(gdp) ~ temp + I(temp^2) + prec + I(prec^2) + factor(country), data = hist)
summary(lm_fe_quad)

# GAM model
library(mgcv)

gam_fe <- gam(
  log(gdp) ~ s(temp, k = 10) + s(prec, k = 10) + factor(country),
  data = hist,
  method = "REML"
)

summary(gam_fe)

plot(gam_fe, pages = 1, shade = TRUE)

# GAM fixed effect + year
gam_fe_year <- gam(
  log(gdp) ~ s(temp, k=10) + s(prec, k=10) + factor(country) + factor(year),
  data = hist,
  method = "REML"
)
summary(gam_fe_year)

par(mfrow = c(1,2))
plot(gam_fe_year, shade = TRUE, pages = 1, seWithMean = TRUE)

# Fixed effect model with country and year
library(lmtest)
library(sandwich)

lm_fe_quad_year <- lm(
  log(gdp) ~ temp + I(temp^2) + prec + I(prec^2) + factor(country) + factor(year),
  data = hist
)
summary(lm_fe_quad_year)
coeftest(lm_fe_quad_year, vcov = vcovCL, cluster = ~ country)

# check if k = 10 is too small for GAM
mgcv::gam.check(gam_fe_year)

# re fit with higher k value and re plot
gam_fe_year_k20 <- gam(
  log(gdp) ~ s(temp, k = 20) + s(prec, k = 20) + factor(country) + factor(year),
  data = hist,
  method = "REML"
)

summary(gam_fe_year_k20)
mgcv::gam.check(gam_fe_year_k20)

par(mfrow = c(1, 2))
plot(gam_fe_year_k20, select = 1, ylim = c(-2, 1), se = TRUE, rug = TRUE,
     main = "s(temp), k=20")
plot(gam_fe_year_k20, select = 2, ylim = c(-2, 1), se = TRUE, rug = TRUE,
     main = "s(prec), k=20")


AIC(lm_fe_quad_year, gam_fe_year, gam_fe_year_k20)
BIC(lm_fe_quad_year, gam_fe_year, gam_fe_year_k20)

# plotting both smooth plots side by side
par(mfrow = c(2, 2))
plot(gam_fe_year,     select = 1, ylim = c(-2, 1), se = TRUE, rug = TRUE, main="temp k=10")
plot(gam_fe_year_k20, select = 1, ylim = c(-2, 1), se = TRUE, rug = TRUE, main="temp k=20")
plot(gam_fe_year,     select = 2, ylim = c(-2, 1), se = TRUE, rug = TRUE, main="prec k=10")
plot(gam_fe_year_k20, select = 2, ylim = c(-2, 1), se = TRUE, rug = TRUE, main="prec k=20")




## Future Projections

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

stopifnot(!any(is.na(fut2$temp_base)))
stopifnot(!any(is.na(fut2$prec_base)))


# Get the factor levels used when fitting the GAM
mf <- model.frame(gam_fe_year_k20)

country_levels <- levels(mf$`factor(country)`)
year_levels    <- levels(mf$`factor(year)`)

# pick any valid training levels (first ones are fine)
dummy_country <- country_levels[1]
dummy_year    <- year_levels[1]

# 2) Build prediction datasets with required columns
new_base <- fut2 %>%
  transmute(
    temp = temp_base,
    prec = prec_base,
    country = factor(dummy_country, levels = country_levels),
    year    = factor(dummy_year,    levels = year_levels)
  )

new_future <- fut2 %>%
  transmute(
    temp = temp,
    prec = prec,
    country = factor(dummy_country, levels = country_levels),
    year    = factor(dummy_year,    levels = year_levels)
  )

# 3) Predict smooth-only terms (exclude FE)
pred_base_terms <- predict(
  gam_fe_year_k20,
  newdata = new_base,
  type = "terms",
  exclude = c("factor(country)", "factor(year)")
)

pred_future_terms <- predict(
  gam_fe_year_k20,
  newdata = new_future,
  type = "terms",
  exclude = c("factor(country)", "factor(year)")
)

# 4) Compute climate-only delta
fut2 <- fut2 %>%
  mutate(
    d_log_gdp_climate =
      (pred_future_terms[, "s(temp)"] - pred_base_terms[, "s(temp)"]) +
      (pred_future_terms[, "s(prec)"] - pred_base_terms[, "s(prec)"]),
    pct_change_gdp = 100 * (exp(d_log_gdp_climate) - 1)
  )
colnames(pred_base_terms)

## analysis of future predictions
summary(fut2$d_log_gdp_climate)
summary(fut2$pct_change_gdp)

path_country <- fut2 %>%
  group_by(country, scenario, year) %>%
  summarise(
    dlog = mean(d_log_gdp_climate),
    pct  = mean(pct_change_gdp),
    .groups = "drop"
  )

key_years <- c(2050, 2070, 2100)

impact_key <- path_country %>%
  filter(year %in% key_years) %>%
  arrange(scenario, year, country)

path_global <- path_country %>%
  group_by(scenario, year) %>%
  summarise(
    avg_pct = mean(pct),
    avg_dlog = mean(dlog),
    .groups = "drop"
  )


p_global <- ggplot(path_global, aes(x = year, y = avg_pct, color = scenario)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Projected climate-only impact on GDP (global average)",
    y = "% change in GDP (relative to baseline climate)",
    x = "Year",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 13) + theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.8)
  )

ggsave(
  "figures_new/global_avg_climate_impact.png",
  p_global,
  width = 10, height = 6, dpi = 300
)



path_country <- path_country %>%
  mutate(
    scenario = factor(scenario, levels = c("SSP1-2.6", "SSP5-8.5")),
    year = as.numeric(year)
  )

p_future_like_sample <- ggplot(
  path_country,
  aes(x = year, y = pct, color = scenario)   # keep colors
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6, color = "grey35") +
  geom_line(linewidth = 1) +
  facet_wrap(~ country, scales = "free_y") +
  scale_color_manual(values = c("SSP1-2.6" = "#F8766D", "SSP5-8.5" = "#00BFC4")) +
  scale_x_continuous(
    breaks = seq(2020, 2100, by = 20),     # gives spacing (not every year)
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title = "Projected climate-driven GDP impact (relative to 2021 climate)",
    subtitle = "Climate-only effect from GAM smooth terms; country/year fixed effects excluded",
    x = "Year",
    y = "Estimated % change in GDP per capita (climate-driven)",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.8),
    plot.subtitle = element_text(size = 15, hjust = 0.6),
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figures_new/future_pct_impact_like_sample.png",
  p_future_like_sample,
  width = 11, height = 7, dpi = 300
)



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
  theme_minimal(base_size = 13) + theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.8)
  )

ggsave(
  "figures_new/impact_2100_by_country.png",
  p_2100,
  width = 9, height = 6, dpi = 300
)



