# Left-Right Ideological Associations: Interactive Visualization

An interactive Shiny application visualizing the results from **"Mapping left-right associations: a framework using open-ended survey responses and political positions"** by Lukas Warode (2025).

**Target Audience:** Researchers, policymakers, and industry professionals interested in political ideology, text analysis, and data visualization.

---

## Table of Contents

1. [Project Overview](#project-overview)
2. [Key Concepts from the Paper](#key-concepts-from-the-paper)
3. [App Features](#app-features)
4. [Technical Architecture](#technical-architecture)
5. [Data Structure](#data-structure)
6. [File Structure](#file-structure)
7. [UI/UX Specifications](#uiux-specifications)
8. [Implementation Guide](#implementation-guide)
9. [Deployment](#deployment)
10. [Optional Future Features](#optional-future-features)

---

## Project Overview

This Shiny app provides an interactive exploration of how political elites associate words with "left" and "right" ideology. The visualization recreates and extends **Figure 5** from the paper, allowing users to:

- Explore word associations in a two-dimensional semantic-positional space
- Click on words to reveal detailed information
- View distributions of who uses specific words
- See real political text examples highlighting selected words

### Paper Citation

```
Warode, L. (2025). Mapping left-right associations: a framework using
open-ended survey responses and political positions. Humanities and
Social Sciences Communications, 12:1318.
https://doi.org/10.1057/s41599-025-05679-x
```

---

## Key Concepts from the Paper

### Two-Dimensional Model

The framework maps word associations along two dimensions:

| Dimension | Axis | Description | Range |
|-----------|------|-------------|-------|
| **Position** | X-axis | Mean left-right self-placement of individuals using the word | 1 (left) to 11 (right) |
| **Semantics** | Y-axis | How "left" or "right" a word is semantically | -1 (left) to +1 (right) |

### Four Quadrants

| Quadrant | Position | Semantics | Interpretation |
|----------|----------|-----------|----------------|
| **Upper-Left** | Left-leaning | Right semantics | Out-ideology (left individuals describing right) |
| **Upper-Right** | Right-leaning | Right semantics | In-ideology (right individuals describing right) |
| **Lower-Left** | Left-leaning | Left semantics | In-ideology (left individuals describing left) |
| **Lower-Right** | Right-leaning | Left semantics | Out-ideology (right individuals describing left) |

### Example Associations

| Quadrant | Example Words |
|----------|---------------|
| In-ideology Left | justice, peace, solidarity, tolerance |
| Out-ideology Left | socialism, paternalism, egalitarianism |
| In-ideology Right | patriotism, family, rule of law, personal responsibility |
| Out-ideology Right | racism, fascism, nationalism |

---

## App Features

### 1. Interactive Scatter Plot (Figure 5)

**Primary visualization recreating the paper's main figure:**

- Interactive scatter plot with all word associations
- Points colored by quadrant membership
- Hover tooltips showing word and basic statistics
- Click-to-select functionality for detailed exploration
- Reference lines showing mean values (quadrant boundaries)
- Zoom and pan capabilities

### 2. Word Detail Panel (On Click)

When a user clicks on a word, display:

#### 2.1 Left-Right Distribution Plot
- Density plot showing the distribution of left-right self-placement
- For individuals who used this word in their associations
- Separate distributions for "associated with left" vs "associated with right"
- Mean markers with connecting line showing distance

#### 2.2 Party Usage Bar Plot
- Bar chart showing count of individuals using the word
- Grouped by German political party:
  - DIE LINKE (far-left)
  - Greens (center-left)
  - SPD (center-left)
  - FDP (liberal/center-right)
  - CDU/CSU (center-right/conservative)
  - AfD (far-right)
- Party colors matching standard German political color coding

#### 2.3 Context Text Highlights
- Display 3-5 example sentences from political texts
- Selected word highlighted within the text
- Metadata shown: speaker party, date, source type
- Scrollable panel for multiple examples

---

## Technical Architecture

### Technology Stack

| Component | Technology | Purpose |
|-----------|------------|---------|
| Framework | R Shiny | Web application |
| UI Framework | bslib (Bootstrap 5) | Modern responsive design |
| Interactive Plots | plotly | Scatter plot with click events |
| Static Plots | ggplot2 | Distribution and bar plots |
| Data Manipulation | dplyr, tidyr | Data processing |
| Tables | DT | Interactive data tables (if needed) |
| Deployment | shinyapps.io | Hosting |

### Required R Packages

```r
# Core
library(shiny)
library(bslib)

# Visualization
library(plotly)
library(ggplot2)

# Data manipulation
library(dplyr)
library(tidyr)
library(readr)

# Optional
library(DT)
library(htmltools)
library(stringr)
```

### Shiny Modules

The app uses modular architecture for maintainability:

| Module | File | Responsibility |
|--------|------|----------------|
| Scatter Plot | `mod_scatter_plot.R` | Main interactive visualization |
| Word Details | `mod_word_details.R` | Distribution + bar plot panel |
| Context Viewer | `mod_context_viewer.R` | Political text highlights |

---

## Data Structure

### Data Mode Configuration

The app supports two data modes:

```r
# In global.R or app.R
USE_REAL_DATA <- FALSE
# Set to TRUE when real data is available

if (USE_REAL_DATA) {

data <- load_real_data()
} else {
data <- generate_placeholder_data()
}
```
### Required Data Files

#### 1. `data/word_associations.csv`

Main dataset for the scatter plot.

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| `word` | character | German word | "gerechtigkeit" |
| `word_en` | character | English translation | "justice" |
| `lr_position_mean` | numeric | Mean left-right self-placement (1-11) | 3.2 |
| `semantic_score` | numeric | Semantic association score (-1 to +1) | -0.85 |
| `frequency` | integer | Total frequency of word usage | 245 |
| `quadrant` | character | Quadrant classification | "in_left" |
| `freq_left_assoc` | integer | Frequency when associating with left | 230 |
| `freq_right_assoc` | integer | Frequency when associating with right | 15 |

**Quadrant values:** `"in_left"`, `"out_left"`, `"in_right"`, `"out_right"`, `"center"`

### Understanding Mean Values

The app displays different mean values in different contexts. This is intentional and reflects the underlying data structure:

| Display Location | Value Used | Description |
|-----------------|------------|-------------|
| **Main Scatter Plot (X-axis)** | `lr_self_mean` | Weighted mean across ALL respondents who used the word |
| **UI "Position (Mean)"** | `lr_self_mean` | Same as scatter plot - overall weighted position |
| **Distribution Plot Annotation** | `lr_self_mean_left` or `lr_self_mean_right` | Mean of the SPECIFIC distribution being shown |

**Example:** For "social (sozial)":
- UI shows **Position: 3.37** (weighted mean of all 741 respondents)
- Distribution plot shows **x̄ = 3.29** (mean of the 659 LEFT-association respondents only)

This distinction exists because:
1. The **scatter plot position** represents where the word sits in the ideological space based on ALL people who use it
2. The **distribution plot** shows only the relevant distribution (left OR right) based on the word's semantic classification, so its annotation reflects that specific subset

The values come from `lr_pos_sem_curated.csv`:
- `lr_self_mean_left`: Mean position of people who associated this word with "left"
- `lr_self_mean_right`: Mean position of people who associated this word with "right"
- `lr_self_mean`: Weighted combination used for the main plot's x-axis position

#### 2. `data/position_distributions.csv`

Distribution data for the detail panel.

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| `word` | character | German word | "gerechtigkeit" |
| `association_with` | character | "left" or "right" | "left" |
| `lr_position` | integer | Left-right position (1-11) | 3 |
| `count` | integer | Number of individuals | 45 |

#### 3. `data/party_usage.csv`

Party breakdown for bar plots.

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| `word` | character | German word | "gerechtigkeit" |
| `party` | character | Party abbreviation | "SPD" |
| `party_full` | character | Full party name | "Sozialdemokratische Partei Deutschlands" |
| `count` | integer | Usage count | 52 |
| `party_color` | character | Hex color code | "#E3000F" |

**Party data structure:**

| Party | Full Name | Color | Position |
|-------|-----------|-------|----------|
| LINKE | DIE LINKE | #BE3075 | Far-left |
| GRUENE | Buendnis 90/Die Gruenen | #1AA037 | Center-left |
| SPD | Sozialdemokratische Partei Deutschlands | #E3000F | Center-left |
| FDP | Freie Demokratische Partei | #FFEF00 | Center/Liberal |
| CDU | Christlich Demokratische Union | #000000 | Center-right |
| CSU | Christlich-Soziale Union | #008AC5 | Center-right |
| AfD | Alternative fuer Deutschland | #009EE0 | Far-right |

#### 4. `data/context_database.csv`

Political text examples (100 entries).

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| `id` | integer | Unique identifier | 1 |
| `text` | character | Political statement (1-3 sentences) | "Soziale Gerechtigkeit..." |
| `text_en` | character | English translation | "Social justice..." |
| `words_contained` | character | Pipe-separated list of target words | "gerechtigkeit\|sozial" |
| `speaker_party` | character | Party of speaker | "SPD" |
| `speaker_name` | character | Simulated speaker name | "Maria Schmidt" |
| `date` | date | Date of statement | "2021-03-15" |
| `source_type` | character | Type of source | "Parliamentary Speech" |

---

## File Structure

```
left-right-associations-app/
│
├── app.R # Main application entry point
│
├── R/
│ ├── mod_scatter_plot.R # Scatter plot module
│ ├── mod_word_details.R # Word details module (distributions + bar)
│ ├── mod_context_viewer.R # Context text viewer module
│ ├── utils_data.R # Data loading and transformation
│ ├── utils_plots.R # Plotting helper functions
│ └── generate_placeholder_data.R # Placeholder data generation
│
├── data/
│ ├── word_associations.csv # Main word data (or placeholder)
│ ├── position_distributions.csv # Distribution data
│ ├── party_usage.csv # Party breakdown data
│ └── context_database.csv # Political text examples
│
├── www/
│ ├── custom.css # Custom styles
│ ├── logo.png # Optional logo
│ └── favicon.ico # Browser favicon
│
├── tests/
│ └── test_data_integrity.R # Data validation tests
│
├── .Rprofile # Package loading configuration
├── renv.lock # Package version lock (if using renv)
├── README.md # This file
└── deploy.R # Deployment script for shinyapps.io
```

---

## UI/UX Specifications

### Design System

#### Color Palette (Google Colors)

| Purpose | Color | Hex Code |
|---------|-------|----------|
| Primary (Interactive elements) | Blue | #4285F4 |
| In-ideology Left | Green | #34A853 |
| Out-ideology Left | Yellow/Orange | #FBBC05 |
| In-ideology Right | Blue | #4285F4 |
| Out-ideology Right | Red | #EA4335 |
| Background | Light Gray | #F8F9FA |
| Text Primary | Dark Gray | #202124 |
| Text Secondary | Medium Gray | #5F6368 |

#### Typography

| Element | Font | Size | Weight |
|---------|------|------|--------|
| Headings | Google Sans / Roboto | 24-32px | 500-700 |
| Body | Roboto | 14-16px | 400 |
| Labels | Roboto | 12-14px | 400 |
| Code/Data | Roboto Mono | 13px | 400 |

### Layout Structure

```
+------------------------------------------------------------------+
| HEADER: Title + Brief Description |
+------------------------------------------------------------------+
| |
| +------------------------+ +--------------------------------+ |
| | | | WORD DETAILS PANEL | |
| | MAIN SCATTER PLOT | | (appears on word click) | |
| | (Figure 5) | | | |
| | | | [Distribution Plot] | |
| | - Interactive | | | |
| | - Click to select | | [Party Bar Plot] | |
| | - Hover for tooltip | | | |
| | | | [Context Text Examples] | |
| | | | | |
| +------------------------+ +--------------------------------+ |
| |
+------------------------------------------------------------------+
| FOOTER: Citation + Links |
+------------------------------------------------------------------+
```

### Responsive Breakpoints

| Breakpoint | Width | Layout |
|------------|-------|--------|
| Desktop | > 1200px | Side-by-side (70/30 split) |
| Tablet | 768-1200px | Side-by-side (60/40 split) |
| Mobile | < 768px | Stacked (plot above, details below) |

### Interaction States

| State | Visual Feedback |
|-------|-----------------|
| Hover (point) | Enlarged point + tooltip |
| Selected (point) | Highlighted border + connected panel |
| Loading | Spinner overlay |
| No selection | Placeholder text in detail panel |
| Error | Red alert banner with message |

---

## Implementation Guide

### Step 1: Project Setup

```r
# Create project structure
dir.create("left-right-associations-app")
setwd("left-right-associations-app")

# Create subdirectories
dir.create("R")
dir.create("data")
dir.create("www")
dir.create("tests")

# Initialize renv for package management (recommended)
renv::init()
```

### Step 2: Implement Data Layer

**Priority: `R/generate_placeholder_data.R`**

Create functions to generate realistic placeholder data matching the paper's findings:

```r
# Key functions to implement:
generate_word_associations() # ~60-100 words from Figure 5/6
generate_position_distributions() # Per-word distributions
generate_party_usage() # Party breakdown per word
generate_context_database() # 100 political text snippets
```

**Important placeholder data guidelines:**
- Include all example words from Table 1 and Figures 5-6
- Ensure quadrant distributions match paper findings
- German parties should have realistic ideological spread
- Context texts should be politically neutral but realistic

### Step 3: Implement Modules

**Order of implementation:**

1. `mod_scatter_plot.R` - Core visualization
2. `mod_word_details.R` - Detail panel
3. `mod_context_viewer.R` - Text highlighting

### Step 4: Assemble Main App

**`app.R` structure:**

```r
# Load packages
# Source modules
# Load/generate data
# Define UI with bslib
# Define server logic
# Run app
```

### Step 5: Styling and Polish

- Implement `www/custom.css`
- Add loading states
- Error handling
- Performance optimization

### Step 6: Testing and Deployment

- Test with placeholder data
- Swap to real data
- Deploy to shinyapps.io

---

## Deployment

### shinyapps.io Configuration

```r
# deploy.R
library(rsconnect)

# Configure account (one-time setup)
rsconnect::setAccountInfo(
name = "your-account",
token = "your-token",
secret = "your-secret"
)
# Deploy application
rsconnect::deployApp(
appDir = ".",
appName = "left-right-associations",
appTitle = "Left-Right Ideological Associations"
)
```

### Performance Considerations

| Issue | Solution |
|-------|----------|
| Large data load time | Pre-aggregate data, use feather/parquet format |
| Slow plot rendering | Limit initial points, use WebGL renderer |
| Memory usage | Use data.table for large datasets |
| Concurrent users | Implement caching with memoise |

---

## Optional Future Features

### High Priority

- [ ] **Word search/filter**: Text input to search for specific words
- [ ] **Year filter**: Toggle between 2013, 2017, 2021 survey waves
- [ ] **Quadrant filter**: Show/hide specific quadrants
- [ ] **Download functionality**: Export plot as PNG/SVG, data as CSV
- [ ] **Multilingual support**: German/English toggle for interface

### Medium Priority

- [ ] **Zoom to quadrant**: Quick navigation buttons
- [ ] **Comparison mode**: Select two words and compare distributions
- [ ] **Animation**: Animate changes across survey years
- [ ] **Mobile optimization**: Touch-friendly interactions
- [ ] **Accessibility**: Screen reader support, keyboard navigation
- [ ] **Dark mode**: Alternative color scheme

### Low Priority / Experimental

- [ ] **3D visualization**: Add third dimension (frequency/time)
- [ ] **Network view**: Show word co-occurrence relationships
- [ ] **User annotations**: Allow users to add notes (with backend)
- [ ] **API endpoint**: Expose data via plumber API
- [ ] **Embedding explorer**: Interactive word embedding visualization
- [ ] **Real-time updates**: Connect to live data source
- [ ] **PDF report generation**: Export analysis as formatted report
- [ ] **Tutorial mode**: Guided walkthrough for new users

### Language/Localization

- [ ] **German interface**: Full translation of UI elements
- [ ] **Other languages**: Framework for additional translations
- [ ] **RTL support**: Right-to-left language compatibility

---

## Development Notes

### Code Style

- Follow tidyverse style guide
- Use roxygen2 documentation for functions
- Implement input validation for all user inputs
- Log errors for debugging

### Testing Checklist

- [ ] All data files load correctly
- [ ] Scatter plot renders with correct quadrant colors
- [ ] Click events trigger detail panel updates
- [ ] Distribution plot shows correct data for selected word
- [ ] Party bar plot displays all parties
- [ ] Context viewer highlights correct words
- [ ] Responsive layout works on all breakpoints
- [ ] No console errors in browser
- [ ] Performance acceptable (< 3s initial load)

### Git Workflow

```bash
# Branch naming
feature/scatter-plot-module
feature/word-details-panel
fix/distribution-calculation
docs/readme-update
```

---

## Visual Inspiration

Reference dashboard for design patterns:
- https://lukas-warode.shinyapps.io/ParlGov_Dashboard/

Key elements to adapt:
- Clean card-based layout
- Consistent color usage
- Clear data presentation
- Professional typography

---

## Contact & Support

For questions about the underlying research:
- Author: Lukas Warode
- Email: lukas.warode@uni-mannheim.de
- Paper DOI: https://doi.org/10.1057/s41599-025-05679-x

---

*This README serves as a comprehensive specification for building the Shiny application. All components are designed to be modular and extensible for future development.*
