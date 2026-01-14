# AI Coding Agent Instructions: Left-Right Associations Shiny App

## Project Overview

This is a **R Shiny application** visualizing political ideology research on word associations with "left" and "right" concepts. It recreates Figure 5 from Warode (2025), showing words in a 2D semantic-positional space using **modular Shiny architecture**.

**Paper Citation:** Warode, L. (2025). Mapping left-right associations: a framework using open-ended survey responses and political positions. *Humanities and Social Sciences Communications*, 12:1318. https://doi.org/10.1057/s41599-025-05679-x

**Target Users:** Researchers, policymakers, and industry professionals interested in political ideology, text analysis, and data visualization.

## Architecture

### Module System

All interactive components are **Shiny modules** with `UI` and `Server` functions:
- [`mod_scatter_plot.R`](R/mod_scatter_plot.R) - Main plotly scatter plot with click events
- [`mod_word_details.R`](R/mod_word_details.R) - Distribution plots and party bar charts
- [`mod_context_viewer.R`](R/mod_context_viewer.R) - Political text examples viewer

**Pattern:** Each module exports `<name>UI()` and `<name>Server()` functions. Server functions take reactive `data` and `selected_word` parameters.

### Data Flow

```
app.R (startup) → load_app_data() → USE_REAL_DATA flag check
  ├─ TRUE: Load CSVs from data/ directory
  └─ FALSE: generate_all_placeholder_data() from generate_placeholder_data.R

data structure = list(
  word_associations,        # Main scatter plot data
  position_distributions,   # Detail panel distributions
  party_usage,             # Party bar chart data
  context_database,        # Text examples
  party_definitions        # Party metadata
)
```

**Critical:** App can run in **placeholder mode** (`USE_REAL_DATA = FALSE`) for development without real data files. Toggle this in [`app.R`](app.R) line 13.

### Key Data Structures

**word_associations.csv** (primary dataset):
- `lr_position_mean` (X-axis): 1-11 scale, left to right political position
- `semantic_score` (Y-axis): -1 to +1, semantic left-right association
- `quadrant`: One of `in_left`, `out_left`, `in_right`, `out_right`, `center`
- `word` (German) + `word_en` (English translation)

**Quadrant Logic:** 
- "In-ideology" = people describe their own side (e.g., left-leaning people use "justice" for left)
- "Out-ideology" = people describe opposite side (e.g., left-leaning people use "nationalism" for right)

## Critical Patterns

### Color System (Google Colors)

**NEVER change these colors** - they match the paper and Google's palette:
```r
QUADRANT_COLORS <- c(
  "in_left" = "#34A853",    # Green
  "out_left" = "#FBBC05",   # Yellow/Orange
  "in_right" = "#4285F4",   # Blue
  "out_right" = "#EA4335",  # Red
  "center" = "#9E9E9E"      # Gray
)

PARTY_COLORS <- c(
  "LINKE" = "#BE3075", "GRUENE" = "#1AA037", "SPD" = "#E3000F",
  "FDP" = "#FFED00", "CDU" = "#000000", "CSU" = "#008AC5", "AfD" = "#009EE0"
)
```
Defined in [`utils_plots.R`](R/utils_plots.R) lines 15-33.

### Optional Dependencies Pattern

The app gracefully handles missing `shinycssloaders`:
```r
HAS_SPINNERS <- requireNamespace("shinycssloaders", quietly = TRUE)
if (HAS_SPINNERS) {
  library(shinycssloaders)
} else {
  withSpinner <- function(ui, ...) ui  # Fallback no-op
}
```
Apply this pattern for any optional package.

### Party Ordering Convention

German parties MUST be ordered **left to right politically**:
```r
party_levels <- c("LINKE", "GRUENE", "SPD", "FDP", "CDU", "CSU", "AfD")
```
See [`utils_data.R`](R/utils_data.R) line 153.

## Development Workflows

### Running the App

```bash
# Install dependencies first (only needed once)
Rscript install_packages.R

# Run app (from project root)
R -e "shiny::runApp()"

# Alternative: Run with port specification
R -e "shiny::runApp(port = 8080)"
```

**Debugging:** 
- Check `USE_REAL_DATA` flag in [`app.R`](app.R) line 13 if data loading fails
- Use placeholder mode (`USE_REAL_DATA = FALSE`) during development
- Check console messages for data loading confirmation (e.g., "Loaded X word associations")
- Placeholder data includes realistic German words based on paper findings

### Package Management

Install script ([install_packages.R](install_packages.R)) handles:
- Core packages: `shiny`, `bslib`, `plotly`, `ggplot2`, tidyverse suite
- Optional: `shinycssloaders` (gracefully degrades if missing)
- Font rendering: `showtext`, `sysfonts` for EB Garamond typography

**Note:** No renv/package lock currently - app uses latest CRAN versions.

### Adding New Visualizations

1. Create module file in `R/mod_<name>.R`
2. Export `<name>UI(id)` and `<name>Server(id, data, selected_word)`
3. Source in [`app.R`](app.R) (line ~45)
4. Add UI call to layout (line ~136)
5. Add server call in server section (line ~200+)

### Data Validation

Use `validate_data_structure()` from [`utils_data.R`](R/utils_data.R) (line ~86+) to check required columns. Called automatically on data load.

### Typography & Fonts

App uses **EB Garamond** for plots (matching paper typography):
- Loaded via Google Fonts CDN in app header
- Applied through `showtext` package with `font_add_google("EB Garamond", "ebgaramond")`
- All plots use `theme_lr_app()` which specifies `family = "EB Garamond"`
- UI uses **Roboto** via `bslib` theme configuration

### Testing & Validation

**No formal test suite currently.** Manual testing checklist in README includes:
- Data loading (real vs placeholder modes)
- Plot rendering and interactivity
- Module communication (click events → detail panel updates)
- Responsive layout across breakpoints

**For CI/CD:** Consider adding data validation tests using `testthat` framework.

## Gotchas

1. **Bootstrap 5 Theme:** Using `bslib` with `version = 5` - ensure any custom HTML uses BS5 classes (not BS3/4)
2. **Plotly Click Events:** Selected word comes from `event_data("plotly_click")$customdata` - the `customdata` aesthetic MUST contain the word column
3. **German Text:** Primary language is German (`word` column), English is secondary (`word_en` for display)
4. **Party CSU vs CDU:** Both are center-right but separate parties (Bavarian vs national) - do NOT merge them
5. **Reactive Data Access:** Always use `req(data())` and `req(selected_word())` at start of reactive contexts to handle NULL states
6. **Empty data/ Directory:** Real data files not included in repo - app defaults to placeholder mode. Data directory exists but is empty.

## Deployment

**Target Platform:** shinyapps.io (see README deployment section)

Deploy command pattern:
```r
rsconnect::deployApp(
  appDir = ".",
  appName = "left-right-associations",
  appTitle = "Left-Right Ideological Associations"
)
```

**Pre-deployment checklist:**
- Verify `USE_REAL_DATA` flag is set appropriately
- Test with placeholder data if real data unavailable
- Check package availability on shinyapps.io (all packages are CRAN stable)

## External Dependencies

- **shinyapps.io** for deployment (see README for instructions)
- **Google Fonts:** Roboto loaded via CDN in UI theme
- **Bootstrap Icons:** CDN link in [`app.R`](app.R) line ~85

## Common Tasks

**Add new plot type:** Follow module pattern in `R/utils_plots.R` - all plots use `theme_lr_app()` base theme (line 42).

**Modify scatter plot:** Main plot logic in [`mod_scatter_plot.R`](R/mod_scatter_plot.R) server section - uses `create_scatter_plot()` from utils, converted to plotly via `ggplotly()`.

**Update party colors:** Edit `PARTY_COLORS` in [`utils_plots.R`](R/utils_plots.R) line 25 - used throughout app automatically.
