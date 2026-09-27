library(tidyverse)

# --- Load data ---
hist <- read_csv("Historical.csv")

# --- Helper: count countries with coverage in BOTH early and late windows ---
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

# --- Main: build a table over many early windows (rolling windows) ---
# You can change these:
late_start <- 2011
late_end   <- 2020
window_len <- 10          # early window length in years
early_min  <- floor(min(hist$year, na.rm = TRUE))
early_max  <- floor(max(hist$year, na.rm = TRUE))

# rolling early windows: [y, y+window_len-1]
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

# --- View top candidates ---
coverage_tbl %>% slice(1:15)

# --- Best window (max coverage) ---
best_window <- coverage_tbl %>% slice(1)
best_window

# --- Optional: quick plot (coverage vs early_start) ---
ggplot(coverage_tbl, aes(x = early_start, y = n_countries_both)) +
  geom_line() +
  geom_point(size = 1) +
  geom_vline(xintercept = best_window$early_start, linetype = "dashed") +
  labs(
    title = "Country coverage for rolling early windows (must also exist in late window)",
    subtitle = paste0("Late window fixed at ", late_start, "–", late_end,
                      "; Early window length = ", window_len, " years"),
    x = "Early window start year",
    y = "# countries with data in BOTH windows"
  ) +
  theme_minimal()

# --- Optional: save the table + plot ---
dir.create("figures_new/coverage_checks", recursive = TRUE, showWarnings = FALSE)

write_csv(coverage_tbl, "figures_new/coverage_checks/coverage_table.csv")

ggsave(
  "figures_new/coverage_checks/coverage_by_early_start.png",
  width = 10, height = 5, dpi = 300
)

# World polygons (names live in world$name)
world_names <- world %>% st_drop_geometry() %>% distinct(name)

# Main diagnostic for ONE window 
find_unmatched_countries_for_window <- function(df, early_start, early_end, late_start, late_end) {
  country_changes <- build_country_changes(df, early_start, early_end, late_start, late_end)
  
  fixed <- fix_country_names_for_map(country_changes) %>%
    distinct(country, country_map) %>%
    arrange(country)
  
  unmatched <- fixed %>%
    anti_join(world_names, by = c("country_map" = "name")) %>%
    arrange(country)
  
  summary_row <- tibble(
    early_start = early_start,
    early_end   = early_end,
    late_start  = late_start,
    late_end    = late_end,
    n_countries_in_changes = n_distinct(country_changes$country),
    n_unmatched_after_fix  = nrow(unmatched),
    n_matched_after_fix    = n_distinct(country_changes$country) - nrow(unmatched)
  )
  
  list(summary = summary_row, unmatched = unmatched, fixed = fixed)
}

# Batch diagnostic over MANY windows I am using
diagnose_windows <- function(df, windows, late_start = 2011, late_end = 2020) {
  results <- purrr::pmap(
    windows,
    function(early_start, early_end) {
      out <- find_unmatched_countries_for_window(df, early_start, early_end, late_start, late_end)
      list(
        summary = out$summary,
        unmatched = out$unmatched %>% mutate(early_start = early_start, early_end = early_end)
      )
    }
  )
  
  summary_tbl <- bind_rows(purrr::map(results, "summary")) %>%
    arrange(desc(n_unmatched_after_fix), early_start)
  
  unmatched_tbl <- bind_rows(purrr::map(results, "unmatched")) %>%
    arrange(early_start, country)
  
  list(summary_tbl = summary_tbl, unmatched_tbl = unmatched_tbl)
}


# Run it on my 4 early windows
windows_used <- tibble(
  early_start = c(1961, 1971, 1981, 1991),
  early_end   = c(1970, 1980, 1990, 2000)
)

diag_out <- diagnose_windows(hist, windows_used, late_start = 2011, late_end = 2020)

# Table: how many unmatched per window
diag_out$summary_tbl

# Table: exactly which ones are missing (country + the country_map name it tried)
diag_out$unmatched_tbl

write_csv(diag_out$summary_tbl,  "figures_new/geo_visuals/unmatched_summary_by_window.csv")
write_csv(diag_out$unmatched_tbl, "figures_new/geo_visuals/unmatched_countries_by_window.csv")

