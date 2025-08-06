# databoxR 0.0.0.9000

## Initial Release 🚀

This is the first release of **databoxR**, a comprehensive Shiny-based R package for exploring SDTM and ADaM clinical trial datasets with a Windows Explorer-style interface.

### New Features

#### Core Functionality
* **Interactive Dashboard**: Modern bs4Dash interface with sidebar navigation
* **Multi-format Support**: Load XPT, CSV, and TXT files seamlessly
* **File Management**: Upload multiple files with interactive selection
* **Demo Mode**: Built-in sample datasets (DM, AE, VS domains) for immediate exploration

#### Dataset Preview
* **Interactive Tables**: Filterable and searchable data preview with reactable
* **Variable Information**: Comprehensive variable metadata including types, labels, and missing data statistics
* **Export Functionality**: Download filtered datasets as CSV files
* **Customizable Display**: Adjustable row limits and column visibility

#### Metadata Explorer
* **Variable Labels**: View and explore variable labels and attributes
* **Data Dictionary**: Interactive variable analysis with summary statistics and frequency tables
* **CDISC Compliance**: Basic validation checks for standard CDISC variables
* **Visual Analysis**: Automatic plotting for numeric and categorical variables

#### Exploratory Data Analysis (EDA)
* **Dashboard Overview**: Key metrics with value boxes (total rows/columns, missing data %, numeric variables)
* **Data Quality Assessment**: Comprehensive quality checks including duplicate detection and constant variables
* **Distribution Analysis**: Multiple visualization options (histograms, boxplots, density plots) with log scaling
* **Correlation Analysis**: Interactive correlation matrix and ranked correlation table
* **Missing Data Patterns**: Visual and tabular missing data analysis

### Technical Features

#### Package Structure
* **Modular Architecture**: Clean separation with dedicated Shiny modules for each component
* **Robust Error Handling**: Comprehensive error checking and user notifications
* **Extensible Design**: Easy to add new modules and functionality
* **Standard R Package**: Full CRAN-ready package structure with documentation and tests

#### Functions
* `run_databox()` - Main application launcher with optional demo data
* `read_sdtm_adam()` - Universal dataset loader for multiple formats
* `get_dataset_info()` - Dataset summary and metadata extraction

### Sample Data Included
* **Demographics (DM)**: Sample subject demographics with standard CDISC variables
* **Adverse Events (AE)**: Sample adverse event data with MedDRA coding
* **Vital Signs (VS)**: Sample vital signs measurements

### Installation & Usage

```r
# Install from GitHub
remotes::install_github("PawanRamaMali/databoxR")

# Launch with demo data
library(databoxR)
run_databox(demo_data = TRUE)

# Launch for file upload
run_databox()
```

### Dependencies
* **Core**: shiny, bs4Dash, reactable, DT
* **Data Handling**: haven, readr, dplyr, data.table
* **Visualization**: Built-in R graphics with custom themes
* **Testing**: testthat framework with initial test suite

### Known Limitations
* Currently supports basic CDISC validation (extensible for future enhancements)
* Visualization limited to base R graphics (plotly integration planned)
* No advanced statistical modeling features (focused on exploration)

### Future Roadmap
* Enhanced CDISC validation rules
* Define.xml parsing and integration  
* Advanced statistical summaries
* Cross-domain linking capabilities
* Plotly integration for interactive charts
* Custom report generation

---

**Target Users**: Clinical data scientists, biostatisticians, regulatory reviewers, and anyone working with SDTM/ADaM datasets who needs rapid data exploration capabilities.

**Feedback Welcome**: This is an initial release - please report issues and feature requests via GitHub Issues.