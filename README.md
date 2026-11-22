# 📦 databoxR

<!-- badges: start -->
[![R-CMD-check](https://github.com/PawanRamaMali/databoxR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PawanRamaMali/databoxR/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**databoxR** is a Shiny-based R package that provides a Windows Explorer-style interface for browsing SDTM and ADaM datasets. It's designed for clinical data scientists, biostatisticians, and reviewers who want to explore datasets, metadata, and content interactively.

---

## ✨ Features

* 📁 **File Management**: Upload and select multiple SDTM/ADaM files (XPT, CSV, TXT formats)
* 👁️ **Dataset Preview**: Interactive tables with filtering, searching, and variable information
* 🧾 **Metadata Explorer**: Variable labels, data dictionary, and CDISC compliance checks
* 📊 **Exploratory Data Analysis**: Distributions, correlations, missing data patterns
* 🧑‍⚕️ **Subject Profile**: Follow one subject (USUBJID) across every loaded domain with a cross-domain timeline, KPI cards, and highlighted adverse events
* 📈 **Interactive Visualizations**: Histograms, boxplots, density plots, correlation matrices
* 🔍 **Data Quality Assessment**: Missing data analysis, duplicate detection, constant variables
* 📤 **Export Functionality**: Download filtered datasets and analysis results
* 🧪 **Demo Mode**: Built-in sample datasets for immediate exploration

---

## 📦 Installation

```r
# Development version from GitHub
remotes::install_github("PawanRamaMali/databoxR")
```

---

## 🚀 Quick Start

### Launch the Application

```r
library(databoxR)

# Launch with file upload interface
run_databox()

# Launch with sample data pre-loaded
run_databox(demo_data = TRUE)
```

### Using Sample Data

The package includes sample CDISC datasets (DM, AE, VS domains) for immediate exploration:

```r
# Run with demo data to see the interface in action
run_databox(demo_data = TRUE)
```

### Loading Your Own Data

1. Launch the app: `run_databox()`
2. Use the file upload interface to select your SDTM/ADaM files
3. Supported formats: `.xpt`, `.csv`, `.txt`
4. Select files from the uploaded list to explore

---

## 📋 Main Interface Components

### Dataset Preview
- Interactive table with pagination and searching
- Variable summary statistics
- Missing data percentages
- Export functionality

### Metadata Explorer
- **Variable Labels**: View variable labels and types
- **Data Dictionary**: Detailed variable analysis and summaries
- **CDISC Compliance**: Basic validation checks for standard variables

### Exploratory Data Analysis
- **Overview**: Dataset summary with quality metrics
- **Distributions**: Histograms, boxplots, and density plots
- **Correlations**: Correlation matrix and top correlations table  
- **Missing Data**: Missing data patterns and summaries

### Subject Profile
- **Cross-domain drill-down**: Pick one subject and see their records across every loaded domain
- **KPI cards**: Age, sex, arm, site, plus adverse event counts, serious AEs, worst severity, and visit count
- **Timeline**: Interactive plot of dated events per domain, with CDISC study day
- **Adverse events**: Severity colour coding and serious-flag emphasis
- **Reproducible export**: Download the timeline as CSV and a ready-to-run R script

---

## 🔧 Development

### Package Structure
```
databoxR/
├── R/                     # R source code
│   ├── run_app.R         # Main application function
│   ├── dataset_loader.R  # Data loading utilities
│   ├── mod_*.R          # Shiny modules
├── inst/
│   ├── extdata/         # Sample datasets
│   └── shiny-examples/  # Example applications
├── man/                 # Documentation
├── tests/               # Unit tests
└── DESCRIPTION          # Package metadata
```

### Running Tests
```r
devtools::test()
```

### Building Documentation

The `NAMESPACE` is maintained by hand (the source has no `@importFrom` tags), so
regenerate only the help pages and leave `NAMESPACE` untouched:

```r
# Generates man/*.Rd only; does not rewrite NAMESPACE
roxygen2::roxygenise(roclets = "rd")
```

---

## 📄 Supported File Formats

- **SAS Transport Files** (`.xpt`): Read with `haven::read_xpt()`
- **CSV Files** (`.csv`): Read with `readr::read_csv()`
- **Text Files** (`.txt`): Read with `readr::read_csv()`

---

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Submit a pull request

---

## 📜 License

MIT License - see LICENSE file for details
