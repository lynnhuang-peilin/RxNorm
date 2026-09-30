# RxNorm / ATC to NDC explorer

Shiny app that turns generic names or ATC codes (any level, e.g. `N02A`, `N06AA`, `A10BK01`) into a product table: brand name (BN), generic name (GNN), ingredient, dosage form, strength, and NDCs (11-digit and 5-4-2). Data come live from the NLM RxNorm / RxClass REST APIs.

Tabs: **Name / ATC to NDC**, **NDC to drug info**, **Drug name search**. Inputs can be typed or uploaded (CSV/Excel). Results download as CSV or Excel.

## Run locally
```r
install.packages(c("shiny","bslib","httr2","DT","dplyr","purrr","stringr","tibble","readr","readxl","openxlsx"))
shiny::runApp()
```

## Deploy
Posit Connect Cloud: connect this repo, pick `app.R`.
shinyapps.io from GitHub: add repo secrets `SHINYAPPS_NAME`, `SHINYAPPS_TOKEN`, `SHINYAPPS_SECRET` (shinyapps.io > Account > Tokens); pushes to `main` redeploy via `.github/workflows/deploy.yml`.
Manual: `rsconnect::deployApp(appFiles = "app.R", appName = "RxNorm")`.

## Notes
- Calls are made in parallel batches, cached for 24 h, and throttled below NLM's 20 requests/sec limit. Large ATC classes can take a few minutes.
- `ingredient_strength` = strength of the searched ingredient (from SCDC components); `strength` = full product strength.
- NDC scope: "currently associated" uses `/ndcs`; "all ever associated" uses `/allhistoricalndcs?history=1` and adds start/end dates.
- Open CSVs with the NDC column imported as text, or use the Excel download, to keep leading zeros.
- Research use only; not affiliated with NLM.
