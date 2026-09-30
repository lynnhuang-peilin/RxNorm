# RxNorm / ATC -> NDC explorer ------------------------------------------------
# Shiny app built on the NLM RxNav REST API (https://lhncbc.nlm.nih.gov/RxNav/APIs/)
#
# Tabs
#   1. Generic name / ATC -> products (brand name, generic name, strength, NDC ...)
#   2. NDC -> drug information
#   3. Fuzzy drug-name search
#
# Requires R >= 4.1, httr2 >= 1.0, dplyr >= 1.1

library(shiny)
library(bslib)
library(httr2)
library(DT)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)
library(readxl)
library(openxlsx)

# ---- constants ---------------------------------------------------------------
BASE      <- "https://rxnav.nlm.nih.gov/REST"
UA        <- "RxNormShiny/2.0 (research tool; https://github.com/lynnhuang-peilin/RxNorm)"
CHUNK     <- 10      # requests per parallel batch
PAUSE     <- 0.5     # seconds between batches (NLM limit: 20 requests/sec/IP)
ING_TTY   <- c("IN", "MIN", "PIN")
ATC_REGEX <- "^[A-Za-z]([0-9]{2}([A-Za-z]([A-Za-z]([0-9]{2})?)?)?)?$"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- HTTP layer (parallel, cached, polite) -------------------------------------
.cache <- new.env(parent = emptyenv())
.state <- new.env(parent = emptyenv())
.state$t0 <- Sys.time()

reset_cache_if_old <- function() {
  age <- as.numeric(difftime(Sys.time(), .state$t0, units = "hours"))
  if (age > 24 || length(ls(.cache)) > 50000) {
    rm(list = ls(.cache), envir = .cache)
    .state$t0 <- Sys.time()
  }
}

set_detail <- function(txt) {
  tryCatch(shiny::setProgress(detail = txt), error = function(e) NULL)
}

#' Fetch many URLs. Returns a list aligned with `urls` (NULL where a call failed).
rx_fetch <- function(urls, label = "Fetching") {
  reset_cache_if_old()
  todo <- unique(urls[!vapply(urls, exists, logical(1), envir = .cache, inherits = FALSE)])
  n <- length(todo)
  i <- 1
  while (i <= n) {
    idx  <- i:min(i + CHUNK - 1, n)
    reqs <- lapply(todo[idx], function(u) {
      request(u) |>
        req_user_agent(UA) |>
        req_timeout(30) |>
        req_retry(max_tries = 3,
                  is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503, 504))
    })
    resps <- req_perform_parallel(reqs, max_active = 5, on_error = "continue", progress = FALSE)
    for (k in seq_along(idx)) {
      r <- resps[[k]]
      ok <- inherits(r, "httr2_response") && resp_status(r) == 200
      val <- if (ok) tryCatch(resp_body_json(r, simplifyVector = FALSE), error = function(e) NULL) else NULL
      if (!is.null(val)) assign(todo[idx[k]], val, envir = .cache)  # never cache failures
    }
    i <- max(idx) + 1
    set_detail(sprintf("%s: %d / %d", label, min(i - 1, n), n))
    if (i <= n) Sys.sleep(PAUSE)
  }
  lapply(urls, get0, envir = .cache, inherits = FALSE)
}

enc <- function(x) utils::URLencode(x, reserved = TRUE)

# ---- NDC helpers ---------------------------------------------------------------
#' Normalise an NDC to 11 digits. Accepts 11 digits, or hyphenated 4-4-2 / 5-3-2 / 5-4-1.
#' Unhyphenated 10-digit codes are ambiguous and return NA.
normalize_ndc <- function(x) {
  x <- str_trim(as.character(x))
  map_chr(x, function(v) {
    if (is.na(v) || v == "") return(NA_character_)
    if (str_detect(v, "-")) {
      p <- str_split(v, "-")[[1]]
      if (length(p) != 3 || any(!str_detect(p, "^[0-9]+$"))) return(NA_character_)
      p <- c(str_pad(p[1], 5, "left", "0"), str_pad(p[2], 4, "left", "0"), str_pad(p[3], 2, "left", "0"))
      out <- paste0(p, collapse = "")
      return(if (nchar(out) == 11) out else NA_character_)
    }
    d <- str_remove_all(v, "[^0-9]")
    if (nchar(d) == 11) d else if (nchar(d) < 10) str_pad(d, 11, "left", "0") else NA_character_
  })
}

format_ndc <- function(x) {
  ifelse(is.na(x) | nchar(x) != 11, x,
         paste(substr(x, 1, 5), substr(x, 6, 9), substr(x, 10, 11), sep = "-"))
}

# ---- parsing helpers ---------------------------------------------------------------
empty_rel <- function() tibble(src_rxcui = character(), rxcui = character(), name = character(), tty = character())

props_tbl <- function(ids, label = "Concept names") {
  ids <- unique(ids[!is.na(ids) & ids != ""])
  if (!length(ids)) return(tibble(rxcui = character(), name = character(), tty = character()))
  res <- rx_fetch(sprintf("%s/rxcui/%s/properties.json", BASE, ids), label)
  map2_dfr(ids, res, function(id, r) {
    tibble(rxcui = id,
           name  = pluck(r, "properties", "name") %||% NA_character_,
           tty   = pluck(r, "properties", "tty") %||% NA_character_)
  })
}

rel_concepts <- function(ids, ttys, label = "Related concepts") {
  ids <- unique(ids[!is.na(ids) & ids != ""])
  if (!length(ids)) return(empty_rel())
  urls <- sprintf("%s/rxcui/%s/related.json?tty=%s", BASE, ids, paste(ttys, collapse = "+"))
  res  <- rx_fetch(urls, label)
  out <- map2_dfr(ids, res, function(id, r) {
    groups <- pluck(r, "relatedGroup", "conceptGroup") %||% list()
    rows <- map_dfr(groups, function(g) {
      map_dfr(g$conceptProperties %||% list(), function(cp) {
        tibble(rxcui = cp$rxcui %||% NA_character_,
               name  = cp$name  %||% NA_character_,
               tty   = cp$tty   %||% NA_character_)
      })
    })
    if (nrow(rows)) mutate(rows, src_rxcui = id) else empty_rel()
  })
  bind_rows(empty_rel(), out)
}

# ATC class -> ingredient members
atc_members <- function(codes) {
  codes <- toupper(unique(codes))
  urls  <- sprintf("%s/rxclass/classMembers.json?classId=%s&relaSource=ATC", BASE, codes)
  res   <- rx_fetch(urls, "ATC classes")
  out <- map2_dfr(codes, res, function(code, r) {
    mem <- pluck(r, "drugMemberGroup", "drugMember") %||% list()
    map_dfr(mem, function(m) {
      tibble(query = code, atc_code = code,
             ingredient_rxcui = pluck(m, "minConcept", "rxcui") %||% NA_character_,
             ingredient       = pluck(m, "minConcept", "name")  %||% NA_character_)
    })
  })
  if (!nrow(out)) return(tibble(query = character(), atc_code = character(),
                                ingredient_rxcui = character(), ingredient = character()))
  distinct(out)
}

# generic names -> ingredient concepts
name_ingredients <- function(terms) {
  urls <- sprintf("%s/rxcui.json?name=%s&allsrc=0&search=2", BASE, map_chr(terms, enc))
  res  <- rx_fetch(urls, "Matching names")
  hits <- map2_dfr(terms, res, function(t, r) {
    ids <- unlist(pluck(r, "idGroup", "rxnormId")) %||% character()
    tibble(query = t, rxcui = as.character(ids))
  })
  empty <- tibble(query = character(), atc_code = character(),
                  ingredient_rxcui = character(), ingredient = character())
  if (!nrow(hits)) return(empty)
  hp <- left_join(hits, props_tbl(hits$rxcui, "Resolving matches"), by = "rxcui")
  ing <- hp |> filter(tty %in% ING_TTY) |>
    transmute(query, atc_code = NA_character_, ingredient_rxcui = rxcui, ingredient = name)
  other <- hp |> filter(!(tty %in% ING_TTY) | is.na(tty))
  if (nrow(other)) {
    rel <- rel_concepts(other$rxcui, "IN", "Resolving ingredients")
    ing2 <- other |> select(query, src_rxcui = rxcui) |>
      inner_join(rel, by = "src_rxcui", relationship = "many-to-many") |>
      transmute(query, atc_code = NA_character_, ingredient_rxcui = rxcui, ingredient = name)
    ing <- bind_rows(ing, ing2)
  }
  distinct(bind_rows(empty, ing))
}

# one row per concept: strength, human-drug flag, dosage form, components, ingredients
enrich_concepts <- function(ids) {
  ids <- unique(ids[!is.na(ids) & ids != ""])
  cols <- tibble(rxcui = character(), strength = character(), human_drug = logical(),
                 dosage_form = character(), components = character(), ingredients = character())
  if (!length(ids)) return(cols)

  ap <- rx_fetch(sprintf("%s/rxcui/%s/allProperties.json?prop=ATTRIBUTES", BASE, ids), "Strength & flags")
  ap_tbl <- map2_dfr(ids, ap, function(id, r) {
    pc  <- pluck(r, "propConceptGroup", "propConcept") %||% list()
    nm  <- map_chr(pc, ~ .x$propName  %||% NA_character_)
    val <- map_chr(pc, ~ .x$propValue %||% NA_character_)
    st  <- unique(val[grepl("AVAILABLE_STRENGTH", nm)])
    tibble(rxcui = id,
           strength   = if (length(st)) paste(st, collapse = " | ") else NA_character_,
           human_drug = any(grepl("HUMAN_DRUG", nm)))
  })

  rl <- rx_fetch(sprintf("%s/rxcui/%s/related.json?tty=DF+SCDC+IN", BASE, ids), "Dosage forms")
  rl_tbl <- map2_dfr(ids, rl, function(id, r) {
    groups <- pluck(r, "relatedGroup", "conceptGroup") %||% list()
    get <- function(tt) {
      g <- keep(groups, ~ identical(.x$tty, tt))
      unique(unlist(map(g, function(x) map_chr(x$conceptProperties %||% list(), ~ .x$name %||% NA_character_))))
    }
    j <- function(v) if (length(v)) paste(v, collapse = "; ") else NA_character_
    tibble(rxcui = id, dosage_form = j(get("DF")), components = j(get("SCDC")), ingredients = j(get("IN")))
  })

  ap_tbl |> left_join(rl_tbl, by = "rxcui")
}

pick_component <- function(components, ingredient) {
  if (is.na(components) || is.na(ingredient) || !nzchar(components)) return(NA_character_)
  comps <- strsplit(components, "; ", fixed = TRUE)[[1]]
  lc <- tolower(comps); li <- tolower(ingredient)
  hit <- comps[startsWith(lc, li)]
  if (!length(hit)) {
    stems <- str_remove(lc, "\\s[0-9.]+.*$")
    hit <- comps[startsWith(li, stems)]
  }
  if (!length(hit)) return(NA_character_)
  str_extract(hit[1], "(?<=\\s)[0-9]*\\.?[0-9]+\\s.*$")
}

first_component <- function(strength) {
  s <- str_remove_all(strength, "\\(.*?\\)")
  str_trim(str_split_fixed(s, " / | \\| ", 2)[, 1])
}

# add BN / GNN / strength columns to a table that already has rxcui + name + tty
finish_products <- function(df) {
  df <- df |>
    mutate(
      bn  = str_match(name, "\\[([^\\]]+)\\]\\s*$")[, 2],
      gnn = str_squish(str_remove(name, "\\s*\\[[^\\]]+\\]\\s*$"))
    )
  if (!"ingredient" %in% names(df)) df$ingredient <- NA_character_
  df |>
    mutate(
      ingredient_strength = map2_chr(components, ingredient, pick_component),
      .s  = coalesce(ingredient_strength, first_component(strength)),
      strength_num  = suppressWarnings(as.numeric(str_extract(.s, "^[0-9]*\\.?[0-9]+"))),
      strength_unit = na_if(str_trim(str_remove(.s, "^[0-9]*\\.?[0-9]+")), "")
    ) |>
    select(-.s)
}

add_ndcs <- function(df, mode = c("current", "history")) {
  mode <- match.arg(mode)
  ids  <- unique(df$rxcui[!is.na(df$rxcui)])
  empty <- tibble(rxcui = character(), ndc = character(), ndc_start = character(), ndc_end = character())
  if (!length(ids)) return(mutate(df, ndc_11 = NA_character_, ndc_formatted = NA_character_,
                                  ndc_start = NA_character_, ndc_end = NA_character_))
  if (mode == "current") {
    res <- rx_fetch(sprintf("%s/rxcui/%s/ndcs.json", BASE, ids), "Current NDCs")
    nd <- map2_dfr(ids, res, function(id, r) {
      v <- as.character(unlist(pluck(r, "ndcGroup", "ndcList", "ndc")))
      tibble(rxcui = id, ndc = v, ndc_start = NA_character_, ndc_end = NA_character_)
    })
  } else {
    res <- rx_fetch(sprintf("%s/rxcui/%s/allhistoricalndcs.json?history=1", BASE, ids), "Historical NDCs")
    nd <- map2_dfr(ids, res, function(id, r) {
      ht <- pluck(r, "historicalNdcConcept", "historicalNdcTime") %||% list()
      map_dfr(ht, function(h) {
        map_dfr(h$ndcTime %||% list(), function(n) {
          tibble(rxcui = id, ndc = as.character(unlist(n$ndc)),
                 ndc_start = n$startDate %||% NA_character_,
                 ndc_end   = n$endDate   %||% NA_character_)
        })
      })
    })
  }
  nd <- bind_rows(empty, nd) |> distinct() |>
    mutate(ndc_11 = normalize_ndc(ndc), ndc_formatted = format_ndc(ndc_11)) |>
    select(-ndc)
  left_join(df, nd, by = "rxcui", relationship = "many-to-many")
}

# ---- input helpers ---------------------------------------------------------------
split_terms <- function(txt) {
  v <- str_trim(unlist(str_split(txt %||% "", "[\n;,\t]")))
  unique(v[v != ""])
}

# tiny module: textarea + optional upload (CSV / XLSX) with column picker
input_ui <- function(id, label, placeholder) {
  ns <- NS(id)
  tagList(
    textAreaInput(ns("txt"), label, rows = 6, placeholder = placeholder),
    fileInput(ns("file"), "...or upload a CSV / Excel file", accept = c(".csv", ".xlsx", ".xls")),
    uiOutput(ns("col_ui"))
  )
}

input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    up <- reactive({
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      tryCatch(
        if (ext == "csv") read_csv(input$file$datapath, col_types = cols(.default = "c"), show_col_types = FALSE)
        else read_excel(input$file$datapath, col_types = "text"),
        error = function(e) { showNotification(paste("Could not read file:", conditionMessage(e)), type = "error"); NULL }
      )
    })
    output$col_ui <- renderUI({
      d <- up(); req(d)
      selectInput(session$ns("col"), "Column to use", choices = names(d))
    })
    reactive({
      txt <- split_terms(input$txt)
      d <- up()
      fv <- if (!is.null(d) && !is.null(input$col) && input$col %in% names(d)) {
        v <- str_trim(d[[input$col]]); v[!is.na(v) & v != ""]
      } else character()
      unique(c(txt, fv))
    })
  })
}

download_buttons <- function(id) {
  ns <- NS(id)
  div(class = "d-flex gap-2 mb-2",
      downloadButton(ns("csv"), "CSV", class = "btn-sm btn-outline-primary"),
      downloadButton(ns("xlsx"), "Excel", class = "btn-sm btn-outline-primary"))
}

download_server <- function(id, data, stem) {
  moduleServer(id, function(input, output, session) {
    output$csv <- downloadHandler(
      filename = function() sprintf("%s_%s.csv", stem, format(Sys.Date(), "%Y%m%d")),
      content  = function(file) write_csv(data(), file, na = "")
    )
    output$xlsx <- downloadHandler(
      filename = function() sprintf("%s_%s.xlsx", stem, format(Sys.Date(), "%Y%m%d")),
      content  = function(file) openxlsx::write.xlsx(data(), file)
    )
  })
}

show_table <- function(df) {
  datatable(df, rownames = FALSE, filter = "top", class = "compact stripe",
            options = list(pageLength = 25, scrollX = TRUE, deferRender = TRUE))
}

run_safely <- function(expr) {
  tryCatch(expr, error = function(e) {
    showNotification(paste("Something went wrong:", conditionMessage(e)), type = "error", duration = 10)
    NULL
  })
}

PRODUCT_COLS <- c("query", "atc_code", "ingredient_rxcui", "ingredient", "tty", "rxcui", "bn", "gnn",
                  "name", "dosage_form", "strength", "ingredient_strength", "strength_num", "strength_unit",
                  "ndc_11", "ndc_formatted", "ndc_start", "ndc_end", "human_drug")

# ---- UI ---------------------------------------------------------------------------
ui <- page_navbar(
  title = "RxNorm / ATC to NDC",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  nav_panel(
    "Name / ATC to NDC",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        input_ui("p1", "Generic names and/or ATC codes",
                 "One per line, e.g.\noxycodone\nN02AA\nA10BK"),
        checkboxGroupInput("tty", "Product types (TTY)",
                           choices = c("SCD - generic drug" = "SCD", "SBD - branded drug" = "SBD",
                                       "GPCK - generic pack" = "GPCK", "BPCK - branded pack" = "BPCK"),
                           selected = c("SCD", "SBD")),
        checkboxInput("human_only", "Human drugs only", TRUE),
        checkboxInput("want_ndc", "Include NDCs (one extra call per product)", TRUE),
        radioButtons("ndc_mode", "NDC scope",
                     choices = c("Currently associated" = "current", "All ever associated (with dates)" = "history")),
        actionButton("run1", "Search", class = "btn-primary w-100")
      ),
      uiOutput("sum1"),
      download_buttons("dl1"),
      DTOutput("tbl1")
    )
  ),
  nav_panel(
    "NDC to drug info",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        input_ui("p2", "NDC codes", "11 digits, or hyphenated (5-4-2, 5-3-2, 4-4-2)"),
        actionButton("run2", "Look up", class = "btn-primary w-100")
      ),
      download_buttons("dl2"),
      DTOutput("tbl2")
    )
  ),
  nav_panel(
    "Drug name search",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        input_ui("p3", "Drug names (approximate match)", "e.g.\ntylenol 500\nlipitr"),
        sliderInput("maxn", "Max candidates per term", 1, 20, 5),
        checkboxInput("want_ndc3", "Include current NDCs", FALSE),
        actionButton("run3", "Search", class = "btn-primary w-100")
      ),
      download_buttons("dl3"),
      DTOutput("tbl3")
    )
  ),
  nav_panel(
    "About",
    div(class = "container my-4", style = "max-width: 800px;",
        h4("About"),
        p("Data come live from the U.S. National Library of Medicine's RxNorm and RxClass APIs. ",
          "This app is not affiliated with NLM and is for research use, not clinical decision making."),
        tags$ul(
          tags$li("BN (brand name) is parsed from the bracketed suffix of SBD/BPCK names; GNN (generic name) is the RxNorm name without the brand."),
          tags$li("ingredient_strength is the strength of the searched ingredient (from RxNorm SCDC components); strength is the full product strength."),
          tags$li("In CSV files NDCs may lose leading zeros if opened directly in Excel; use the Excel download or import the column as text."),
          tags$li("Requests are cached for 24 hours and throttled to stay within the NLM rate limit.")
        ))
  )
)

# ---- server -----------------------------------------------------------------------
server <- function(input, output, session) {
  t1 <- input_server("p1")
  t2 <- input_server("p2")
  t3 <- input_server("p3")

  # Tab 1 ------------------------------------------------------------------
  res1 <- eventReactive(input$run1, {
    terms <- t1()
    validate(need(length(terms) > 0, "Enter at least one generic name or ATC code."))
    validate(need(length(input$tty) > 0, "Select at least one product type."))
    run_safely(withProgress(message = "Searching RxNorm", value = 0.5, (function() {
      is_atc <- str_detect(terms, ATC_REGEX)
      ing <- bind_rows(
        if (any(is_atc))  atc_members(terms[is_atc]),
        if (any(!is_atc)) name_ingredients(terms[!is_atc])
      )
      if (!nrow(ing)) {
        showNotification("No ingredients found for those inputs.", type = "warning"); return(NULL)
      }
      rel <- rel_concepts(ing$ingredient_rxcui, input$tty, "Products")
      prod <- ing |>
        inner_join(rel, by = c("ingredient_rxcui" = "src_rxcui"), relationship = "many-to-many") |>
        distinct()
      if (!nrow(prod)) {
        showNotification("Ingredients found, but no products of the selected types.", type = "warning"); return(NULL)
      }
      prod <- prod |> left_join(enrich_concepts(prod$rxcui), by = "rxcui") |> finish_products()
      if (input$human_only) prod <- filter(prod, human_drug %in% TRUE)
      if (input$want_ndc) {
        prod <- add_ndcs(prod, input$ndc_mode)
      } else {
        prod <- mutate(prod, ndc_11 = NA_character_, ndc_formatted = NA_character_,
                       ndc_start = NA_character_, ndc_end = NA_character_)
      }
      prod |>
        select(any_of(PRODUCT_COLS), everything(), -any_of(c("components", "ingredients"))) |>
        arrange(query, ingredient, tty, name, ndc_11)
    })()))
  })

  output$sum1 <- renderUI({
    d <- res1(); req(d)
    div(class = "mb-2 text-muted",
        sprintf("%s ingredients | %s products | %s NDCs",
                format(n_distinct(d$ingredient_rxcui), big.mark = ","),
                format(n_distinct(d$rxcui), big.mark = ","),
                format(n_distinct(d$ndc_11, na.rm = TRUE), big.mark = ",")))
  })
  output$tbl1 <- renderDT({ d <- res1(); req(d); show_table(d) })
  download_server("dl1", function() req(res1()), "rxnorm_products")

  # Tab 2 ------------------------------------------------------------------
  res2 <- eventReactive(input$run2, {
    raw <- t2()
    validate(need(length(raw) > 0, "Enter at least one NDC."))
    run_safely(withProgress(message = "Looking up NDCs", value = 0.5, {
      keyed <- tibble(ndc_input = raw, ndc_11 = normalize_ndc(raw))
      ok <- filter(keyed, !is.na(ndc_11))
      st <- tibble(ndc_11 = character(), ndc_status = character(), rxcui = character())
      if (nrow(ok)) {
        u <- unique(ok$ndc_11)
        res <- rx_fetch(sprintf("%s/ndcstatus.json?ndc=%s", BASE, u), "NDC status")
        st <- map2_dfr(u, res, function(n, r) {
          s <- pluck(r, "ndcStatus")
          tibble(ndc_11 = n,
                 ndc_status = s$status %||% NA_character_,
                 rxcui = na_if(as.character(s$rxcui %||% ""), ""))
        })
      }
      out <- keyed |> left_join(st, by = "ndc_11") |>
        mutate(ndc_formatted = format_ndc(ndc_11))
      ids <- out$rxcui[!is.na(out$rxcui)]
      info <- props_tbl(ids, "Concept names") |>
        left_join(enrich_concepts(ids), by = "rxcui")
      out |> left_join(info, by = "rxcui") |>
        mutate(ingredient = ingredients) |>
        finish_products() |>
        mutate(note = ifelse(is.na(ndc_11), "Unparseable or ambiguous NDC (use 11 digits or hyphens)",
                             ifelse(is.na(rxcui), "No RxNorm concept for this NDC", NA_character_))) |>
        select(ndc_input, ndc_11, ndc_formatted, ndc_status, rxcui, tty, bn, gnn, name,
               dosage_form, strength, strength_num, strength_unit, ingredient = ingredients, human_drug, note)
    }))
  })
  output$tbl2 <- renderDT({ d <- res2(); req(d); show_table(d) })
  download_server("dl2", function() req(res2()), "ndc_lookup")

  # Tab 3 ------------------------------------------------------------------
  res3 <- eventReactive(input$run3, {
    terms <- t3()
    validate(need(length(terms) > 0, "Enter at least one drug name."))
    run_safely(withProgress(message = "Searching names", value = 0.5, (function() {
      res <- rx_fetch(sprintf("%s/approximateTerm.json?term=%s&maxEntries=%d&option=1",
                              BASE, map_chr(terms, enc), as.integer(input$maxn)), "Approximate match")
      cand <- map2_dfr(terms, res, function(t, r) {
        cs <- pluck(r, "approximateGroup", "candidate") %||% list()
        map_dfr(cs, function(c) tibble(term = t, rxcui = c$rxcui %||% NA_character_,
                                       score = suppressWarnings(as.numeric(c$score %||% NA)),
                                       rank  = suppressWarnings(as.integer(c$rank %||% NA))))
      })
      if (!nrow(cand)) { showNotification("No matches.", type = "warning"); return(NULL) }
      cand <- cand |> group_by(term, rxcui) |> slice_max(score, n = 1, with_ties = FALSE) |> ungroup()
      info <- props_tbl(cand$rxcui, "Concept names") |> left_join(enrich_concepts(cand$rxcui), by = "rxcui")
      out <- cand |> left_join(info, by = "rxcui") |>
        mutate(ingredient = ingredients) |> finish_products()
      if (input$want_ndc3) out <- add_ndcs(out, "current")
      out |>
        select(any_of(c("term", "score", "rank", "rxcui", "tty", "bn", "gnn", "name", "dosage_form", "strength",
                        "strength_num", "strength_unit", "ndc_11", "ndc_formatted")),
               ingredient = ingredients, human_drug) |>
        arrange(term, desc(score))
    })()))
  })
  output$tbl3 <- renderDT({ d <- res3(); req(d); show_table(d) })
  download_server("dl3", function() req(res3()), "drug_name_search")
}

shinyApp(ui, server)
