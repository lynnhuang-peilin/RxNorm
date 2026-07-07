# RxNorm Lookup Tool

A Shiny app for looking up drug information via the [RxNorm REST API](https://rxnav.nlm.nih.gov/RxNormAPIs.html).

Live app: https://lynnhuangpeilin.shinyapps.io/RxNorm/

## Features

- **NDC → Drug Info**: paste NDC codes (or upload a CSV/Excel file) and get back RxCUI, RxNorm name, term type (TTY), and available strength for each.
- **ATC → RxCUI / NDC**: look up an ATC code, get related RxNorm concepts (ingredient, clinical drug, branded drug, etc.), and optionally expand to the full list of historical NDCs.
- **Drug Name → RxCUI**: fuzzy-search a drug name to find matching RxCUIs.

All three tabs support downloading results as CSV.

## Files

- `RxNorm Shiny.R` — the app (UI + server + RxNorm API helper functions). This is the file deployed to shinyapps.io.
- `RxNorm.Rmd` — original notebook where the underlying API functions were prototyped against local files.

## Running locally

```r
install.packages(c("shiny", "httr", "jsonlite", "stringr", "DT", "readxl", "readr"))
shiny::runApp("RxNorm Shiny.R")
```

## Deploying to shinyapps.io

This repo is already linked to the `RxNorm` app under the `lynnhuangpeilin` shinyapps.io account (see `rsconnect/documents/`). To push an update, open the project in RStudio and run:

```r
rsconnect::deployApp(appFiles = "RxNorm Shiny.R", appName = "RxNorm")
```

or click **Publish** on the app in RStudio's Viewer pane. Make sure the packages listed above are installed locally first — `rsconnect` bundles them automatically based on what the script `library()`s.
