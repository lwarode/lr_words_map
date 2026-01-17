# Mapping Left-Right Associations

Interactive Shiny app visualizing how political elites associate words with "left" and "right" ideology, based on [Warode (2025)](https://doi.org/10.1057/s41599-025-05679-x).

**Live App:** https://lukas-warode.shinyapps.io/lr-words-map/

## Overview

The app maps word associations along two dimensions:

- **X-axis (Position):** Mean left-right self-placement of people using the word (1-11 scale)
- **Y-axis (Semantics):** How "left" or "right" a word is semantically (-1 to +1)

This creates four quadrants:

| Quadrant | Description | Examples |
|----------|-------------|----------|
| **In-Ideology Left** | Left-leaning people describing "left" | justice, peace, solidarity |
| **Out-Ideology Left** | Right-leaning people describing "left" | socialism, paternalism |
| **In-Ideology Right** | Right-leaning people describing "right" | patriotism, family, freedom |
| **Out-Ideology Right** | Left-leaning people describing "right" | racism, nationalism |

## Features

- **Interactive scatter plot** with click-to-select words
- **Distribution plot** showing left-right self-placement of respondents
- **Framework diagram** highlighting the word's quadrant position
- **Downloadable figures** (PNG format)
- **Mobile-responsive** design

## Data

The app uses curated data from German GLES candidate studies (2013, 2017, 2021):

| File | Description |
|------|-------------|
| `lr_pos_sem_curated.csv` | Main word data (229 curated words) |
| `df_exp_left_curated.csv` | Individual-level data for left associations |
| `df_exp_right_curated.csv` | Individual-level data for right associations |

### Understanding Mean Values

The app displays different means in different contexts:

| Location | Value | Description |
|----------|-------|-------------|
| Scatter plot / UI | `lr_self_mean` | Weighted mean of ALL respondents |
| Distribution plot | `lr_self_mean_left` or `_right` | Mean of the specific distribution shown |

Example: "social (sozial)" shows Position **3.37** in UI (all 741 respondents) but **x̄ = 3.29** in the distribution plot (659 left-association respondents only).

## Project Structure

```
├── app.R                 # Main application
├── R/
│   ├── mod_scatter_plot.R    # Main plot module
│   ├── mod_word_details.R    # Side panel module
│   ├── utils_plots.R         # Plot functions
│   └── utils_data.R          # Data loading
├── data/                 # CSV data files
└── www/custom.css        # Styling
```

## Tech Stack

- **R Shiny** with bslib (Bootstrap 5)
- **plotly** for interactive scatter plot
- **ggplot2** for static plots
- Deployed on **shinyapps.io**

## Citation

```
Warode, L. (2025). Mapping left-right associations: a framework using
open-ended survey responses and political positions. Humanities and
Social Sciences Communications, 12:1318.
https://doi.org/10.1057/s41599-025-05679-x
```

## Contact

- Author: Lukas Warode
- Email: lukas.warode@uni-mannheim.de
