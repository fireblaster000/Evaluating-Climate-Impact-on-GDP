# Climate and GDP: Historical Effects and Future Projections

This project examines how temperature and precipitation relate to GDP per capita. It uses a historical country-year dataset to estimate nonlinear climate relationships, then applies those estimates to future climate scenarios for six countries.

The accompanying report is [Climate_and_GDP\_\_Historical_Effects_and_Future_Projections.pdf](Climate_and_GDP__Historical_Effects_and_Future_Projections.pdf). It finds a pronounced nonlinear relationship between temperature and log GDP per capita, with a flatter relationship for precipitation. Future impacts differ considerably across the six countries and are more adverse under SSP5-8.5 than SSP1-2.6 by the end of the century.

These projections isolate the climate component estimated by the model. They are not forecasts of total future GDP: they do not model future institutions, technology, adaptation, or broader economic growth.

## Data

Both CSV files are read from the project root when the scripts run.

| File             | Contents                                                                                                                                                                                                                    |
| ---------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Historical.csv` | 10,277 country-year observations for 209 countries, covering 1961-2020. Columns: `country`, `year`, `gdp`, `temp`, and `prec`.                                                                                              |
| `Future.csv`     | Annual temperature and precipitation projections for China, Greece, India, Iran, Kuwait, and the United States, from 2021 to 2100, under SSP1-2.6 and SSP5-8.5. Columns: `country`, `year`, `scenario`, `temp`, and `prec`. |

The historical data are not a balanced panel: coverage varies by country and year. The report compares country averages across periods and uses a 1981-1990 to 2011-2020 comparison for its main maps, retaining 190 countries in both periods.

## Analysis

The primary workflow is in `climate_gdp_impact.R`. It explores the historical data, creates geographic comparisons, fits fixed-effects models, checks GAM basis dimensions, and estimates future climate-only impacts. The report's preferred historical model is a generalized additive model (GAM) with country and year fixed effects and temperature and precipitation smooths using `k = 20`. The report favors this model based on its flexibility and model diagnostics; BIC alone slightly favors the `k = 10` version.

For projections, the script uses only the fitted temperature and precipitation smooths. It compares each future year with the country's average climate in 2011-2020 and excludes country and year fixed effects from the projection. The resulting percentage changes describe estimated climate-driven differences relative to that baseline, not changes in total GDP.

## Running the analysis

Use R from the project root, where both CSV files are located. The main script uses these packages:

```r
install.packages(c(
  "tidyverse", "mgcv", "lmtest", "sandwich", "sf",
  "rnaturalearth", "rnaturalearthdata", "scales"
))
```

Then run:

```sh
Rscript climate_gdp_impact.R
```

The script creates its figure directories as needed. Running the submitted copy with `Rscript MIF_midterm/final_sol.R` also requires launching it from the project root so it can find `Historical.csv` and `Future.csv`.

## Files and folders

| Path                                                             | Purpose                                                                                                                                                                                                       |
| ---------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Climate_and_GDP__Historical_Effects_and_Future_Projections.pdf` | Main project report.                                                                                                                                                                                          |
| `Historical.csv`, `Future.csv`                                   | Historical observations and future scenario inputs.                                                                                                                                                           |
| `climate_gdp_impact.R`                                           | Main analysis and figure-generation script.                                                                                                                                                                   |
| `coverage.R`                                                     | Rolling-window country coverage analysis and map-name diagnostics. Its later diagnostics rely on `world` and helper functions defined in `climate_gdp_impact.R`; run that script first in the same R session. |
| `sol.R`                                                          | Earlier, broad exploration and modeling work; overlaps with the main script.                                                                                                                                  |
| `answers.R`                                                      | Alternative analysis workflow that saves figures to `figures/` and model objects and a summary table to `tables/`. Its model specifications differ from those used in the report.                             |
| `figures/`                                                       | Figures from the `answers.R` workflow.                                                                                                                                                                        |
| `figures_new/initial_analysis/`                                  | Data summaries and exploratory plots.                                                                                                                                                                         |
| `figures_new/historical_comparisons/`                            | Scatter and hexbin plots with LOESS smooths.                                                                                                                                                                  |
| `figures_new/geo_visuals/`                                       | Country coverage table and maps comparing several early periods with 2011-2020.                                                                                                                               |
| `figures_new/model_plots/`                                       | Model comparison, smooth, and residual diagnostic figures.                                                                                                                                                    |
| `figures_new/future_analysis/`                                   | Country and sample-average projection figures and saved projection tables.                                                                                                                                    |
| `new_figures/model_plots/`                                       | Additional model comparison outputs.                                                                                                                                                                          |
| `MIF_midterm/`                                                   | Packaged submission copy: final report, analysis script, and copies of selected figures and tables.                                                                                                           |

The scripts write paths relative to the current working directory. Run them from the project root unless a script's input paths have been changed.

## Interpretation

The estimates describe associations in historical data after controlling for country and year effects. The future exercise applies those estimated relationships to projected climate values; it does not establish that the projections will occur. The report also notes that the analysis does not account for sector-specific effects, adaptation and institutional differences, climate extremes, or uncertainty across climate-model ensembles.
