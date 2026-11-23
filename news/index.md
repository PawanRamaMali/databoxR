# Changelog

## databoxR 0.2.0

### Cross-domain profiles and grounded AI 🔎

#### New Features

- **Subject Profile tab**: Follow one subject (USUBJID) across every
  loaded domain, with KPI cards, a demographics panel, an interactive
  cross-domain timeline (with CDISC study day), colour-coded adverse
  events, and a CSV and R-script export.
- **Ask tab**: Ask a question in plain English. Claude proposes a
  structured query plan, databoxR validates every field against the
  loaded data and runs the query itself, and the tab shows the plan it
  ran and the reproducible R code. Real rows only, never generated data.
- **Per-subject AI summary**: In the Subject Profile tab, databoxR
  computes a subject’s facts and hands only those facts to Claude for a
  short plain-language summary. The facts are shown next to the summary
  so every statement is checkable. It describes the data and is not
  medical advice.
- **Grounding model**: Both AI features follow one rule. R does the data
  work and the model only phrases it, so the worst case is wording,
  never fabricated records.

#### Optional AI setup

- The Ask and AI summary features use the `ellmer` package and an
  `ANTHROPIC_API_KEY`. Without them the panels show a short notice and
  the rest of databoxR is unaffected. See the README for setup.

#### Infrastructure

- **Continuous integration**: R-CMD-check across macOS, Windows, and
  Ubuntu (release, devel, oldrel-1), plus a status badge.
- **pkgdown site**: Published reference documentation at
  <https://pawanramamali.github.io/databoxR/>.
- **Packaging**: Documented every exported function, added an
  `Authors@R` field and an MIT `LICENSE`, and cleaned up imports so the
  package passes `R CMD check`.

## databoxR 0.1.1

### Enhanced UI and Visualization Update 🎨

#### New Features

- **Enhanced Tab Design**: Professional pill-style tabs with proper
  spacing, gradients, and visual separation
- **Plotly Integration**: Complete migration from base R graphics to
  interactive plotly visualizations
- **Improved Demo Data**: Synthetic CDISC-compliant data generation
  without file I/O dependencies
- **Advanced Error Handling**: Comprehensive validation and graceful
  error recovery throughout the application

#### UI Improvements

- **Professional Tab Styling**: Enhanced tab appearance with gradient
  backgrounds, proper padding, and visual separation
- **Interactive Visualizations**: All charts now use plotly for better
  interactivity and responsiveness
- **No Margin Issues**: Eliminated “figure margins too large” errors by
  migrating to plotly
- **Consistent Spacing**: Proper 10px gaps between icons and text, 12px
  margins between tabs
- **Modern Card Design**: Enhanced card layouts with Bootstrap styling
  and shadows

#### Bug Fixes

- **Package Loading**: Fixed critical NAMESPACE import issues including
  missing h1, h2 HTML functions
- **Escape Sequences**: Resolved `'\|' is an unrecognized escape` error
  in dataset_loader.R
- **Notification Types**: Fixed invalid showNotification parameters
  (changed “success”/“info” to “message”)
- **Demo Data Issues**: Eliminated CSV file dependencies with direct
  synthetic data generation
- **JavaScript Errors**: Removed problematic JS() function calls causing
  browser errors

#### Technical Improvements

- **Plotly Visualizations**: Complete migration to plotly for all charts
  (histograms, boxplots, density, violin plots)
- **Robust Data Loading**: Enhanced read_sdtm_adam() function with
  better error handling
- **Performance Optimization**: Data sampling for large datasets (\>10K
  rows) to improve rendering speed
- **Memory Management**: Better handling of large datasets with progress
  notifications

## databoxR 0.1.0

### First Stable Release 🚀

This is the first stable release of **databoxR**, a comprehensive
Shiny-based R package for exploring SDTM and ADaM clinical trial
datasets with a Windows Explorer-style interface.

#### Major Features

##### Dashboard Interface

- **Modern bs4Dash Interface**: Clean, responsive dashboard with proper
  tab navigation
- **Three-Tab Structure**: Upload/Management, Dataset Explorer, and
  Analysis
- **Demo Mode**: Built-in sample datasets (DM, AE, VS domains) with
  `run_databox(demo_data = TRUE)`
- **Auto-Navigation**: Automatically switches to Explorer tab when data
  is loaded

##### Data Management

- **Multi-Format Support**: Load XPT, CSV, and TXT files seamlessly
- **Interactive File Selection**: Upload multiple files with selection
  interface
- **Robust Loading**: Comprehensive error handling with progress
  indicators
- **Data Validation**: Built-in dataset validation with quality checks

##### Dataset Explorer

- **Interactive Preview**: Filterable, searchable data tables with
  reactable
- **Variable Information**: Comprehensive metadata including types,
  labels, missing data
- **Export Functionality**: Download filtered datasets as CSV files
- **Loading States**: Proper loading indicators and empty state handling

##### Analysis Suite

- **Metadata Explorer**: Variable labels, data dictionary, CDISC
  compliance checks
- **Exploratory Data Analysis**: Distribution plots, missing data
  patterns, data quality metrics
- **Interactive Plotly Visualizations**: Modern interactive charts with
  zoom, pan, and hover capabilities
- **Quality Assessment**: Duplicate detection, constant variables,
  missing data analysis

#### Technical Improvements

##### User Experience

- **Loading Indicators**: shinycssloaders integration for all async
  operations
- **Error Handling**: Comprehensive error messages and graceful
  fallbacks
- **Notifications**: Success/warning/error feedback throughout the
  interface
- **Responsive Design**: Works across different screen sizes

##### Code Quality

- **Modular Architecture**: Clean separation with dedicated Shiny
  modules
- **Input Validation**: Edge case handling for empty datasets, constant
  variables
- **Utility Functions**: Reusable validation and data handling functions
- **Comprehensive Testing**: testthat framework with sample tests

#### Dependencies

- **Core**: shiny, bs4Dash, reactable, DT
- **Data Processing**: haven, readr, dplyr, data.table
- **UI Enhancement**: shinycssloaders, shinyWidgets
- **Visualization**: plotly (NEW in 0.1.1)
- **Testing**: testthat framework

#### Sample Data

- **Demographics (DM)**: Subject demographics with standard CDISC
  variables
- **Adverse Events (AE)**: Adverse event data with MedDRA terms
- **Vital Signs (VS)**: Vital signs measurements and metadata

#### Installation & Usage

``` r

# Install from GitHub
remotes::install_github("PawanRamaMali/databoxR")

# Launch with demo data for immediate exploration
library(databoxR)
run_databox(demo_data = TRUE)

# Launch for file upload workflow
run_databox()
```

#### Breaking Changes

None - this is the first stable release.

#### Bug Fixes

- Fixed tab navigation issues with proper bs4Dash implementation
- Resolved reactable rendering errors with proper NAMESPACE imports
- Improved error handling for edge cases in data loading
- Fixed responsive layout issues in Analysis tab

#### Known Limitations

- Basic CDISC validation (extensible framework in place)
- No advanced statistical modeling (focused on exploration)
- Limited cross-domain linking capabilities

#### Future Roadmap

- Enhanced CDISC validation rules and Define.xml integration
- Advanced statistical summaries and modeling capabilities
- Cross-domain linking and relationship analysis
- Custom report generation and export options
- Advanced plotly dashboard features

------------------------------------------------------------------------

**Target Users**: Clinical data scientists, biostatisticians, regulatory
reviewers, and clinical research professionals working with SDTM/ADaM
datasets.

**Feedback**: This is a stable release ready for production use. Please
report issues and feature requests via GitHub Issues.
