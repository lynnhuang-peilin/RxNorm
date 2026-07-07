# =============================================================================
# RxNorm Lookup Tool - Shiny App
# author: Lynn Huang
# Deployed at: https://lynnhuangpeilin.shinyapps.io/RxNorm/
# Source API: https://rxnav.nlm.nih.gov/RxNormAPIs.html
#
# This app wraps the RxNorm REST API helper functions originally prototyped in
# RxNorm.Rmd (NDC -> Drug Name / RxCUI / Strength / Properties, ATC -> RxCUI /
# NDC, and Name -> RxCUI) into an interactive tool with single or batch lookup,
# a results table, and CSV download.
# =============================================================================

library(shiny)
library(httr)
library(jsonlite)
library(stringr)
library(DT)
library(readxl)
library(readr)
library(tools)

# ---- small helper -----------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- RxNorm API helper functions --------------------------------------------

# Low-level GET + JSON parse with basic error handling so one bad lookup
# doesn't crash the whole app.
rxnorm_get <- function(path) {
  res <- tryCatch(
    httr::GET(path, httr::timeout(15)),
    error = function(e) NULL
  )
  if (is.null(res) || httr::status_code(res) != 200) return(NULL)
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

# NDC -> RxCUI
ndc_to_rxcui <- function(ndc) {
  ndc <- stringr::str_pad(trimws(ndc), 11, side = "left", pad = "0")
  path <- paste0("https://rxnav.nlm.nih.gov/REST/rxcui.json?idtype=NDC&id=", ndc)
  res <- rxnorm_get(path)
  rxcui <- res$idGroup$rxnormId[[1]]
  if (is.null(rxcui)) NA_character_ else as.character(rxcui)
}

# RxCUI -> named vector of all NAME + ATTRIBUTE properties
rxcui_to_properties <- function(rxcui) {
  if (is.na(rxcui)) return(NULL)
  path <- paste0("https://rxnav.nlm.nih.gov/REST/rxcui/", rxcui,
                  "/allProperties.json?prop=NAMES+ATTRIBUTES")
  res <- rxnorm_get(path)
  props <- res$propConceptGroup$propConcept
  if (is.null(props) || length(props) == 0) return(NULL)
  vals <- vapply(props, function(x) as.character(x$propValue %||% NA), character(1))
  names(vals) <- vapply(props, function(x) as.character(x$propName %||% NA), character(1))
  vals
}

# NDC -> one-row data.frame with RxCUI, RxNorm Name, TTY, Available Strength
ndc_lookup <- function(ndc) {
  rxcui <- ndc_to_rxcui(ndc)
  if (is.na(rxcui)) {
    return(data.frame(
      NDC = ndc, RxCUI = NA_character_, RxNorm_Name = NA_character_,
      TTY = NA_character_, Available_Strength = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  props <- rxcui_to_properties(rxcui)
  get_prop <- function(nm) if (!is.null(props) && nm %in% names(props)) props[[nm]] else NA_character_
  data.frame(
    NDC = ndc,
    RxCUI = rxcui,
    RxNorm_Name = get_prop("RxNorm Name"),
    TTY = get_prop("TTY"),
    Available_Strength = get_prop("AVAILABLE_STRENGTH"),
    stringsAsFactors = FALSE
  )
}

# ATC -> RxCUI
atc_to_rxcui <- function(atc) {
  path <- paste0("https://rxnav.nlm.nih.gov/REST/rxcui.json?idtype=ATC&id=", trimws(atc))
  res <- rxnorm_get(path)
  rxcui <- res$idGroup$rxnormId[[1]]
  if (is.null(rxcui)) NA_character_ else as.character(rxcui)
}

# ATC -> related concepts (default term types: IN, SCD, SCDC, SBD, SBDC)
atc_to_related <- function(atc, ttys = c("IN", "SCD", "SCDC", "SBD", "SBDC")) {
  rxcui <- atc_to_rxcui(atc)
  if (is.na(rxcui)) return(data.frame())
  tty_str <- paste(ttys, collapse = "+")
  path <- paste0("https://rxnav.nlm.nih.gov/REST/rxcui/", rxcui, "/related.json?tty=", tty_str)
  res <- rxnorm_get(path)
  groups <- res$relatedGroup$conceptGroup
  if (is.null(groups) || length(groups) == 0) return(data.frame())

  rows <- list()
  for (g in groups) {
    cps <- g$conceptProperties
    if (is.null(cps)) next
    for (cp in cps) {
      rows[[length(rows) + 1]] <- data.frame(
        RxCUI = as.character(cp$rxcui %||% NA),
        Name = as.character(cp$name %||% NA),
        TTY = as.character(cp$tty %||% NA),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  out$ATC <- atc
  out
}

# RxCUI -> all historical NDCs on file
rxcui_to_ndcs <- function(rxcui) {
  path <- paste0("https://rxnav.nlm.nih.gov/REST/rxcui/", rxcui, "/allhistoricalndcs.json")
  res <- rxnorm_get(path)
  times <- res$historicalNdcConcept$historicalNdcTime
  if (is.null(times) || length(times) == 0) return(character(0))
  ndcs <- character(0)
  for (t in times) {
    for (nt in (t$ndcTime %||% list())) {
      nd <- nt$ndc
      if (!is.null(nd)) {
        ndcs <- c(ndcs, vapply(nd, as.character, character(1)))
      }
    }
  }
  unique(ndcs)
}

# ATC -> expanded table of NDC codes (one row per NDC)
atc_to_ndc <- function(atc, ttys = c("IN", "SCD", "SCDC", "SBD", "SBDC")) {
  related <- atc_to_related(atc, ttys)
  if (nrow(related) == 0) return(data.frame())
  rows <- lapply(seq_len(nrow(related)), function(i) {
    ndcs <- rxcui_to_ndcs(related$RxCUI[i])
    if (length(ndcs) == 0) return(NULL)
    data.frame(
      NDC = ndcs, RxCUI = related$RxCUI[i], Name = related$Name[i],
      TTY = related$TTY[i], ATC = atc, stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

# Drug name -> candidate RxCUIs (approximate match, search=2)
name_to_rxcui <- function(name) {
  path <- paste0("https://rxnav.nlm.nih.gov/REST/rxcui.json?name=",
                  utils::URLencode(name, reserved = TRUE), "&allsrc=0&search=2")
  res <- rxnorm_get(path)
  rxcui <- res$idGroup$rxnormId
  if (is.null(rxcui) || length(rxcui) == 0) return(character(0))
  unique(vapply(rxcui, as.character, character(1)))
}

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  titlePanel("RxNorm Lookup Tool"),
  p(
    "Look up drug information via the National Library of Medicine's RxNorm API. ",
    a("API documentation", href = "https://rxnav.nlm.nih.gov/RxNormAPIs.html", target = "_blank")
  ),
  tabsetPanel(

    # ---- NDC lookup ---------------------------------------------------------
    tabPanel(
      "NDC → Drug Info",
      sidebarLayout(
        sidebarPanel(
          radioButtons("ndc_input_mode", "Input method",
                        c("Type/paste NDC codes" = "text", "Upload a file" = "file")),
          conditionalPanel(
            "input.ndc_input_mode == 'text'",
            textAreaInput("ndc_text", "NDC code(s), one per line",
                          rows = 6, placeholder = "00310620530\n00004000512\n76519117002")
          ),
          conditionalPanel(
            "input.ndc_input_mode == 'file'",
            fileInput("ndc_file", "Upload CSV or Excel file", accept = c(".csv", ".xlsx", ".xls")),
            uiOutput("ndc_col_ui")
          ),
          actionButton("ndc_go", "Look up", class = "btn-primary"),
          br(), br(),
          downloadButton("ndc_download", "Download results (.csv)")
        ),
        mainPanel(
          DTOutput("ndc_table")
        )
      )
    ),

    # ---- ATC lookup ----------------------------------------------------------
    tabPanel(
      "ATC → RxCUI / NDC",
      sidebarLayout(
        sidebarPanel(
          textInput("atc_code", "ATC code", placeholder = "A10BK01"),
          checkboxGroupInput("atc_ttys", "Term types to include",
                              choices = c("IN", "MIN", "PIN", "SCD", "SCDC", "SBD", "SBDC"),
                              selected = c("IN", "SCD", "SCDC", "SBD", "SBDC")),
          checkboxInput("atc_expand_ndc", "Also fetch NDC codes for each concept (slower)", FALSE),
          actionButton("atc_go", "Look up", class = "btn-primary"),
          br(), br(),
          downloadButton("atc_download", "Download results (.csv)")
        ),
        mainPanel(
          DTOutput("atc_table")
        )
      )
    ),

    # ---- Name search -----------------------------------------------------
    tabPanel(
      "Drug Name → RxCUI",
      sidebarLayout(
        sidebarPanel(
          textInput("name_query", "Drug name", placeholder = "hydrocodone"),
          actionButton("name_go", "Search", class = "btn-primary"),
          br(), br(),
          downloadButton("name_download", "Download results (.csv)")
        ),
        mainPanel(
          DTOutput("name_table")
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # ---- NDC tab -----------------------------------------------------------
  ndc_uploaded_data <- reactive({
    req(input$ndc_file)
    ext <- tolower(tools::file_ext(input$ndc_file$name))
    if (ext == "csv") {
      readr::read_csv(input$ndc_file$datapath, show_col_types = FALSE)
    } else {
      readxl::read_excel(input$ndc_file$datapath)
    }
  })

  output$ndc_col_ui <- renderUI({
    req(ndc_uploaded_data())
    selectInput("ndc_col", "Column containing NDC codes", choices = names(ndc_uploaded_data()))
  })

  ndc_results <- eventReactive(input$ndc_go, {
    ndcs <- if (input$ndc_input_mode == "text") {
      validate(need(nchar(trimws(input$ndc_text %||% "")) > 0, "Please enter at least one NDC code."))
      strsplit(input$ndc_text, "[,\n\r]+")[[1]]
    } else {
      validate(need(!is.null(input$ndc_file), "Please upload a file."))
      req(input$ndc_col)
      as.character(ndc_uploaded_data()[[input$ndc_col]])
    }
    ndcs <- trimws(ndcs)
    ndcs <- unique(ndcs[ndcs != ""])
    validate(need(length(ndcs) > 0, "No valid NDC codes found."))

    withProgress(message = "Looking up NDCs...", value = 0, {
      n <- length(ndcs)
      out <- vector("list", n)
      for (i in seq_len(n)) {
        out[[i]] <- ndc_lookup(ndcs[i])
        incProgress(1 / n)
      }
      do.call(rbind, out)
    })
  })

  output$ndc_table <- renderDT({
    ndc_results()
  }, options = list(pageLength = 15), rownames = FALSE)

  output$ndc_download <- downloadHandler(
    filename = function() paste0("NDC_lookup_", Sys.Date(), ".csv"),
    content = function(file) write.csv(ndc_results(), file, row.names = FALSE)
  )

  # ---- ATC tab -------------------------------------------------------------
  atc_results <- eventReactive(input$atc_go, {
    validate(need(nchar(trimws(input$atc_code %||% "")) > 0, "Please enter an ATC code."))
    validate(need(length(input$atc_ttys) > 0, "Select at least one term type."))

    res <- withProgress(message = "Querying RxNorm...", value = 0.3, {
      if (isTRUE(input$atc_expand_ndc)) {
        atc_to_ndc(input$atc_code, input$atc_ttys)
      } else {
        atc_to_related(input$atc_code, input$atc_ttys)
      }
    })
    validate(need(nrow(res) > 0, "No results found for this ATC code."))
    res
  })

  output$atc_table <- renderDT({
    atc_results()
  }, options = list(pageLength = 15), rownames = FALSE)

  output$atc_download <- downloadHandler(
    filename = function() paste0("ATC_lookup_", Sys.Date(), ".csv"),
    content = function(file) write.csv(atc_results(), file, row.names = FALSE)
  )

  # ---- Name search tab -----------------------------------------------------
  name_results <- eventReactive(input$name_go, {
    validate(need(nchar(trimws(input$name_query %||% "")) > 0, "Please enter a drug name."))
    rxcuis <- name_to_rxcui(input$name_query)
    validate(need(length(rxcuis) > 0, "No matching RxCUI found."))

    withProgress(message = "Fetching details...", value = 0, {
      n <- length(rxcuis)
      out <- vector("list", n)
      for (i in seq_len(n)) {
        props <- rxcui_to_properties(rxcuis[i])
        get_prop <- function(nm) if (!is.null(props) && nm %in% names(props)) props[[nm]] else NA_character_
        out[[i]] <- data.frame(
          RxCUI = rxcuis[i],
          Name = get_prop("RxNorm Name"),
          TTY = get_prop("TTY"),
          stringsAsFactors = FALSE
        )
        incProgress(1 / n)
      }
      do.call(rbind, out)
    })
  })

  output$name_table <- renderDT({
    name_results()
  }, options = list(pageLength = 15), rownames = FALSE)

  output$name_download <- downloadHandler(
    filename = function() paste0("Name_lookup_", Sys.Date(), ".csv"),
    content = function(file) write.csv(name_results(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
