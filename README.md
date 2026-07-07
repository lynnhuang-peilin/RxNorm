# RxNorm Lookup Tool

A Shiny app for looking up drug information via the [RxNorm REST API](https://rxnav.nlm.nih.gov/RxNormAPIs.html).

Live app: https://lynnhuangpeilin.shinyapps.io/RxNorm/

## Features

- **NDC → Drug Info**: paste NDC codes (or upload a CSV/Excel file) and get back RxCUI, RxNorm name, term type (TTY), and available strength for each.
- **ATC → RxCUI / NDC**: look up an ATC code, get related RxNorm concepts (ingredient, clinical drug, branded drug, etc.), and optionally expand to the full list of historical NDCs.
- **Drug Name → RxCUI**: fuzzy-search a drug name to find matching RxCUIs.

All three tabs support downloading results as CSV.

## Running locally

```r
install.packages(c("shiny", "httr", "jsonlite", "stringr", "DT", "readxl", "readr"))
shiny::runApp("RxNorm Shiny.R")
```

## Data source

All lookups are powered by the National Library of Medicine's [RxNorm REST API](https://rxnav.nlm.nih.gov/RxNormAPIs.html), a standardized nomenclature for clinical drugs.
