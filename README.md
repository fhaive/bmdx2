# 📦 BMDx2 Shiny Application

**BMDx2** is a comprehensive **R-Shiny application** for **dose-dependent analysis of toxicogenomic data**.  

It integrates three core R packages into a single graphical interface, enabling streamlined **dose–response modeling**, **pathway enrichment**, and **adverse outcome pathway (AOP) mapping**:

- [BMDx package](https://github.com/fhaive/bmdx/) 
- [AOPfingerprintR package](https://github.com/fhaive/AOPfingerprintR)  
- [FunMappOne package](https://github.com/fhaive/FunMappOnePackage)  

BMDx APIs are also hosted [here](https://test.enaloscloud.novamechanics.com/insight/bmdx/__docs__/ )  
AOPfingerprint APIs are also hosted [here](https://test.enaloscloud.novamechanics.com/insight/aop_fingerprint/__docs__/ )  

---

## Docker

- BMDx2 is also available as a docker at [docker hub](https://hub.docker.com/r/fhaive/bmdx2)

How to use: 

```
docker pull fhaive/bmdx2:latest
mkdir -p GLP_logs
docker run --rm -it -p 3838:3838 -v ./GLP_logs/:/srv/shiny-server/bmdx2/GLP_logs fhaive/bmdx2:latest
```

- AOPFingerprint APIs are also available as a docker at [docker hub](https://hub.docker.com/r/fhaive/aopfingerprint_api)
- BMDx APIs are also available as a docker at [docker hub](https://hub.docker.com/r/fhaive/bmdx_api)

---

## 🚀 Quick Start (Demo)

You can try out **BMDx2** immediately with the demo dataset included in the repository under the bleomycin_data folder.

This is a  transcriptomics dataset comprises RNA-Seq data from bleomycin exposure at six doses (0, 20, 40, 60, 80, 100 µg/mL) and three time points (24, 48, and 72 hours).
[Click here for the original article describing the dataset](https://doi.org/10.1016/j.csbj.2024.10.010)  

## Launch the app locally

```r
source("run_app_local.R")
```

---

## 📁 Folder Structure

```plaintext
.
├── data/                         # Data used by the app
├── manual/                       # Documentation or user guides
├── renv/                         # R environment management (package cache)
├── renv.lock                     # Locked dependencies for reproducibility
├── report.Rmd                    # R Markdown report for results summarization
├── run_app_local.R               # Script to launch the app (entry point)
├── server_modules/               # Modular server-side logic (Shiny modules)
├── server.R                      # Main Shiny server function
├── ui_functions/                 # Custom UI components used in the app
├── ui.R                          # Main Shiny UI definition
└── www/                          # Static resources (images, CSS, JavaScript)
```

---


## 📑 Description

- This app provides a user-friendly interface for navigating BMD analysis outputs.
- Modular architecture using `server_modules/` and `ui_functions/` supports reusability.
- Includes markdown-based reporting (`report.Rmd`) and embedded static assets (`www/`).

---

## 📦 Requirements

- R ≥ 4.0
- R packages are managed via `renv` (see `renv.lock`)

---

## 📬 Contact

For questions, suggestions, or contributions, please contact [Angela Serra] at [angela.serra@tuni.fi].
