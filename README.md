# nbssma

An R package providing functions for analyzing and visualizing SMA (Systemic Risk Assessment) survey data at NBS. Covers both static `ggplot2` charts and interactive `plotly`/`ggiraph` visuals for HTML reporting with Quarto.

## Installation

```r
# Install from GitHub
remotes::install_github("olepaech/nbssma")
```

## Functions

**Static charts**
| Function | Description |
|----------|-------------|
| `aggregate_risk_development()` | Aggregate risk trend |
| `aggregated_risk_factors()` | Risk factor decomposition |
| `bar_line()` | Bar chart with trend line |
| `group_bar_category()` | Grouped bars by category |
| `group_bar_month()` | Grouped bars by month |
| `grouped_histogram()` | Grouped histogram |
| `heatmap_all_categories()` | Category heatmap |
| `inflation_risk_history()` | Inflation risk history |
| `Risk_Development_Area()` | Area chart for risk development |
| `Risk_Development_Bars()` | Bar chart for risk development |
| `Risk_Factors()` | Risk factor chart |
| `Boxplot_all_categories()` | Box plots by category |
| `Dotplot_all_categories()` | Dot plots by category |
| `risk_bubbles()` | Bubble chart risk overview |
| `spiderplot_category()` | Spider / radar plot |

**Interactive charts**
| Function | Description |
|----------|-------------|
| `hover_bars()` | Interactive bar chart |
| `hover_boxplot()` | Interactive box plot |
| `hover_violin()` | Interactive violin plot |
| `plot_3d()` | 3D scatter / surface |
| `plot_expectation()` | Expectation chart |
| `plot_reasons()` | Reasons breakdown chart |

**Data utilities**
| Function | Description |
|----------|-------------|
| `load_participant_files()` | Load participant survey files |
| `load_risk_files()` | Load risk assessment files |
| `prepare_file_list()` | Prepare file list for batch processing |
| `get_current_dfr()` | Retrieve current DFR value |
| `extract_labels()` | Extract chart labels |
| `refresh_quarto_NBS()` | Refresh the Quarto NBS report |

## Dependencies

`dplyr`, `tidyr`, `readxl`, `stringr`, `tibble`, `purrr`, `magrittr`, `fmsb`, `ggplot2`, `plotly`, `gridExtra`, `lubridate`, `ggiraph`, `gt`, `modeest`, `rvest`, `scales`, `tidyselect`

## Author

Ole Paech
