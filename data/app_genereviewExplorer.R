###############################################################################
# GeneReviews Explorer v5 – Modern UI redesign
#
# Design : clean, minimal, soft shadows, warm earthy palette, card-based
# No shinydashboard / no bslib — pure Shiny + custom CSS
#
# Structure fichier : shortname | NBKid | genesymbol | dzname
# Packages : shiny, DT, httr, xml2
###############################################################################

library(shiny)
library(DT)
library(httr)
library(xml2)

# --------------------------------------------------------------------------- #
BOOKS_URL <- "https://www.ncbi.nlm.nih.gov/books/"
LOCAL_FILE <- "GRshortname_NBKid_genesymbol_dzname.txt"

SECTIONS <- list(
  "Clinical Characteristics" = list(
    pat = "clinical\\s*(characteristics|description|features)|suggestive\\s*findings",
    icon = "stethoscope", col = "#9EB7C5", bg = "#F3F5F7"
  ),
  "Diagnosis" = list(
    pat = "^diagnosis|establish.*diagnosis|testing\\s*strategy|diagnostic\\s*criteria",
    icon = "flask", col = "#9FC8C5", bg = "#F2F7F6"
  ),
  "Management" = list(
    pat = "^management|^treatment|^surveillance|^prevention\\s*of|^agents",
    icon = "medkit", col = "#6BBDD1", bg = "#F0F7F9"
  ),
  "Genetic Counseling" = list(
    pat = "^genetic\\s*counseli|^inheritance|^risk\\s*to\\s*family",
    icon = "users", col = "#E3A4A4", bg = "#F9F2F2"
  ),
  "Molecular Genetics" = list(
    pat = "^molecular\\s*(genetics|pathogenesis)|^pathogenic\\s*variants|genotype.phenotype|^gene\\s*(and|structure|function)",
    icon = "dna", col = "#B1B9C5", bg = "#F3F4F6"
  )
)
SEC_NAMES <- names(SECTIONS)

# --------------------------------------------------------------------------- #
# Lecture fichier local
# --------------------------------------------------------------------------- #
load_local_file <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  raw <- readLines(path, encoding = "UTF-8", warn = FALSE)
  raw <- raw[!grepl("^#|^\\s*$", raw)]
  if (length(raw) == 0) {
    return(NULL)
  }
  sep <- if (grepl("|", raw[1], fixed = TRUE)) "|" else "\t"
  rows <- lapply(raw, function(l) {
    p <- trimws(strsplit(l, sep, fixed = TRUE)[[1]])
    if (length(p) < 4) {
      return(NULL)
    }
    data.frame(
      Shortname = p[1], NBK = p[2], Gene = p[3], Disease = p[4],
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  rows <- Filter(function(r) grepl("^NBK\\d+$", r$NBK), rows)
  if (length(rows) == 0) {
    return(NULL)
  }
  do.call(rbind, rows)
}

# --------------------------------------------------------------------------- #
# Nettoyage HTML
# --------------------------------------------------------------------------- #
clean_node_html <- function(node, base_url) {
  for (sel in c(
    ".//script", ".//style", ".//sup", ".//figure",
    ".//nav", ".//button", ".//noscript",
    ".//*[@class and contains(@class,'ref')]",
    ".//*[@class and contains(@class,'figure')]",
    ".//*[@class and contains(@class,'icon')]"
  )) {
    tryCatch(
      {
        bad <- xml_find_all(node, sel)
        for (b in bad) xml_remove(b)
      },
      error = function(e) NULL
    )
  }
  links <- xml_find_all(node, ".//a")
  for (a in links) {
    href <- xml_attr(a, "href")
    if (!is.na(href) && nchar(href) > 0 && !grepl("^http", href)) {
      xml_attr(a, "href") <- paste0(base_url, href)
    }
    xml_attr(a, "target") <- "_blank"
  }
  imgs <- xml_find_all(node, ".//img")
  for (img in imgs) xml_remove(img)
  as.character(node)
}

# --------------------------------------------------------------------------- #
# Extraction section -> HTML propre
# --------------------------------------------------------------------------- #
extract_section_html <- function(doc, header_node, base_url) {
  container <- NULL
  parent <- xml_parent(header_node)
  htext <- xml_text(header_node)
  for (i in 1:12) {
    if (is.null(parent)) break
    tag <- xml_name(parent)
    if (tag %in% c("body", "html")) break
    if (tag %in% c("div", "section", "article") &&
      nchar(xml_text(parent)) > nchar(htext) + 150) {
      container <- parent
      break
    }
    parent <- xml_parent(parent)
  }
  if (is.null(container)) {
    return(NULL)
  }
  children <- xml_children(container)
  content_nodes <- list()
  capture <- FALSE
  for (ch in children) {
    tag <- xml_name(ch)
    text <- trimws(xml_text(ch))
    if (!capture) {
      if (tag %in% c("h2", "h3", "h4") &&
        trimws(xml_text(ch)) == trimws(htext)) {
        capture <- TRUE
      }
      next
    }
    if (tag %in% c("h2", "h3") && capture) break
    if (tag %in% c("p", "ul", "ol", "table", "dl", "div", "blockquote", "h4", "h5") &&
      nchar(text) > 5) {
      content_nodes <- c(content_nodes, list(ch))
    }
  }
  if (length(content_nodes) == 0) {
    content_nodes <- list(container)
  }
  html_parts <- vapply(content_nodes, function(n) {
    tryCatch(clean_node_html(n, base_url), error = function(e) "")
  }, character(1))
  paste(html_parts[nchar(html_parts) > 10], collapse = "\n")
}

# --------------------------------------------------------------------------- #
# Fetch + parse chapitre
# --------------------------------------------------------------------------- #
fetch_chapter <- function(nbk_id, log_fn) {
  url <- paste0(BOOKS_URL, nbk_id, "/")
  log_fn(paste0("Fetch : ", url))
  resp <- tryCatch(
    GET(
      url, timeout(35),
      add_headers(
        Accept = "text/html",
        `User-Agent` = "Mozilla/5.0 (GeneReviewsExplorer/5.0)"
      )
    ),
    error = function(e) {
      log_fn(paste0("Erreur r\u00e9seau : ", e$message))
      NULL
    }
  )
  if (is.null(resp) || status_code(resp) != 200) {
    log_fn(paste0("HTTP ", if (!is.null(resp)) status_code(resp) else "NULL"))
    return(NULL)
  }
  html_text <- content(resp, "text", encoding = "UTF-8")
  doc <- tryCatch(read_html(html_text), error = function(e) NULL)
  if (is.null(doc)) {
    log_fn("Erreur parse HTML")
    return(NULL)
  }
  title <- ""
  h1s <- xml_find_all(doc, "//h1")
  if (length(h1s) > 0) title <- trimws(xml_text(h1s[[1]]))
  if (nchar(title) < 3) {
    tt <- xml_find_all(doc, "//title")
    if (length(tt) > 0) {
      title <- sub(
        "\\s*[-\u2013]\\s*(GeneReviews|NCBI).*", "",
        trimws(xml_text(tt[[1]]))
      )
    }
  }
  log_fn(paste0("Titre : ", title))
  parsed <- list()
  hdrs_found <- character(0)
  for (hx in c("//h2", "//h3", "//h4")) {
    nodes <- xml_find_all(doc, hx)
    for (nd in nodes) {
      htxt <- trimws(xml_text(nd))
      if (nchar(htxt) < 2 || nchar(htxt) > 200) next
      hdrs_found <- c(hdrs_found, htxt)
      matched <- NULL
      for (sn in SEC_NAMES) {
        if (!is.null(parsed[[sn]])) next
        if (grepl(SECTIONS[[sn]]$pat, htxt, ignore.case = TRUE, perl = TRUE)) {
          matched <- sn
          break
        }
      }
      if (is.null(matched)) next
      html_content <- extract_section_html(doc, nd, BOOKS_URL)
      if (!is.null(html_content) && nchar(html_content) > 50) {
        parsed[[matched]] <- html_content
      }
      log_fn(paste0(
        "Section '", matched, "' -> ",
        nchar(html_content %||% ""), " car. HTML"
      ))
    }
    if (length(parsed) >= length(SEC_NAMES)) break
  }
  log_fn(paste0("Sections pars\u00e9es : ", paste(names(parsed), collapse = ", ")))
  list(title = title, sections = parsed, headers = hdrs_found)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# --------------------------------------------------------------------------- #
# CSS
# --------------------------------------------------------------------------- #
styles_css <- "
/* ================================================================
   RESET & BASE
================================================================ */
*, *::before, *::after { box-sizing: border-box; }
body {
  margin: 0; padding: 0;
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  background: #f0f0f3;
  color: #1e293b;
  -webkit-font-smoothing: antialiased;
  overflow-x: hidden;
}
.container-fluid { padding: 0 !important; }

/* ================================================================
   LAYOUT : sidebar + main
================================================================ */
.app-shell {
  display: flex;
  min-height: 100vh;
}

/* ---------- SIDEBAR ------------------------------------------- */
.app-sidebar {
  width: 280px;
  min-width: 280px;
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  border-right: 1px solid rgba(255,255,255,0.38);
  padding: 28px 22px;
  display: flex;
  flex-direction: column;
  gap: 8px;
  position: fixed;
  top: 0; left: 0; bottom: 0;
  overflow-y: auto;
  z-index: 100;
}
.app-main {
  margin-left: 280px;
  flex: 1;
  padding: 36px 40px;
  min-height: 100vh;
}

/* Logo / brand */
.sidebar-brand {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 28px;
  padding-bottom: 20px;
  border-bottom: 1px solid rgba(255,255,255,0.3);
}
.sidebar-brand .brand-icon {
  width: 36px; height: 36px;
  background: #1e293b;
  border-radius: 10px;
  display: flex; align-items: center; justify-content: center;
  color: #fff; font-size: 16px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.1);
}
.sidebar-brand .brand-text {
  font-size: 17px; font-weight: 700;
  color: #0f172a; letter-spacing: -.3px;
}
.sidebar-brand .brand-sub {
  font-size: 11px; font-weight: 400;
  color: #94a3b8; margin-top: 1px;
}

/* Sidebar section labels */
.sidebar-label {
  font-size: 10px; font-weight: 600;
  text-transform: uppercase;
  letter-spacing: .8px;
  color: #94a3b8;
  margin: 18px 0 8px 2px;
}

/* Search input */
.search-wrap {
  position: relative;
  margin-bottom: 6px;
}
.search-wrap .form-group { margin-bottom: 0; }
.search-wrap input[type='text'] {
  width: 100%;
  padding: 10px 14px 10px 38px;
  border: 1px solid rgba(255,255,255,0.5);
  border-radius: 10px;
  font-size: 13px;
  font-family: 'Inter', sans-serif;
  background: #E6E6EA;
  color: #1e293b;
  transition: border-color .2s, box-shadow .2s, background .2s;
  outline: none;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,.50);
}
.search-wrap input[type='text']::placeholder { color: #94a3b8; }
.search-wrap input[type='text']:focus {
  border-color: rgba(95,157,231,0.4);
  box-shadow: -2px -1px 8px 0px #ffffff,
              2px 1px 8px 0px rgb(95 157 231 / 30%),
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.35) inset;
  background: #E6E6EA;
}
.search-icon {
  position: absolute;
  left: 13px; top: 50%;
  transform: translateY(-50%);
  color: #94a3b8; font-size: 13px;
  pointer-events: none;
  z-index: 2;
}

/* Sidebar button */
.sidebar-btn {
  width: 100%;
  padding: 9px 16px;
  border: none;
  border-radius: 10px;
  font-size: 13px;
  font-weight: 600;
  font-family: 'Inter', sans-serif;
  cursor: pointer;
  transition: all .15s;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 8px;
}
.sidebar-btn-primary {
  background: #1e293b;
  color: #fff;
  box-shadow: 0 1px 3px rgba(0,0,0,0.1);
}
.sidebar-btn-primary:hover {
  background: #334155;
  box-shadow: 0 2px 6px rgba(0,0,0,0.15);
}
#btn_search { margin-top: 8px; }

/* File status */
.file-status {
  padding: 10px 14px;
  background: #E6E6EA;
  border-radius: 10px;
  font-size: 12px;
  color: #475569;
  display: flex;
  align-items: center;
  gap: 8px;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,.50);
}
.file-status .status-dot {
  width: 7px; height: 7px;
  border-radius: 50%;
  flex-shrink: 0;
}
.file-status .status-dot.ok { background: #22c55e; }
.file-status .status-dot.err { background: #ef4444; }

/* Example pills */
.example-pills {
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
  margin-top: 4px;
}
.example-pill {
  padding: 4px 12px;
  background: #E6E6EA;
  border: 1px solid rgba(255,255,255,0.5);
  border-radius: 8px;
  font-size: 12px;
  font-weight: 500;
  color: #475569;
  font-family: 'SF Mono', 'Fira Code', monospace;
  cursor: default;
  transition: all .15s;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,.50);
}
.example-pill:hover {
  box-shadow: -2px -1px 8px 0px #ffffff,
              2px 1px 8px 0px rgb(95 157 231 / 30%),
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.35) inset;
}

/* Sidebar checkboxes */
.sidebar-checks .checkbox {
  margin: 3px 0;
  padding: 0;
}
.sidebar-checks .checkbox label {
  font-size: 13px;
  color: #475569;
  padding-left: 6px;
  cursor: pointer;
}
.sidebar-checks .checkbox input[type='checkbox'] {
  accent-color: #1e293b;
}
.sidebar-checks-actions {
  display: flex;
  gap: 6px;
  margin-top: 6px;
}
.sidebar-checks-actions .btn {
  flex: 1;
  padding: 5px 10px;
  border-radius: 8px;
  font-size: 11px;
  font-weight: 600;
  font-family: 'Inter', sans-serif;
  border: 1px solid rgba(255,255,255,0.8);
  background: rgba(255,255,255,0.3);
  color: #475569;
  cursor: pointer;
  transition: all .15s;
}
.sidebar-checks-actions .btn:hover {
  background: rgba(255,255,255,0.5);
  color: #0f172a;
}

/* Debug checkbox */
.debug-check {
  margin-top: 10px;
  padding-top: 10px;
  border-top: 1px solid rgba(255,255,255,0.3);
}
.debug-check .checkbox label {
  font-size: 11px;
  color: #94a3b8;
}

/* ================================================================
   WELCOME SCREEN
================================================================ */
.welcome-card {
  border-radius: 10px;
  border: 1px solid rgba(255,255,255,0.38);
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  padding: 80px 50px 60px;
  text-align: center;
  box-shadow: 0 8px 32px rgba(31,38,135,0.15);
  max-width: 640px;
  margin: 60px auto;
}
.welcome-icon {
  width: 72px; height: 72px;
  background: #E6E6EA;
  border-radius: 20px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  margin-bottom: 24px;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,.50);
}
.welcome-icon i { font-size: 30px; color: #475569; }
.welcome-card h2 {
  font-size: 28px; font-weight: 700;
  color: #0f172a; margin: 0 0 10px;
  letter-spacing: -.5px;
}
.welcome-card .welcome-sub {
  color: #94a3b8; font-size: 15px;
  line-height: 1.6; margin-bottom: 28px;
}
.welcome-divider {
  height: 1px;
  background: rgba(255,255,255,0.3);
  margin: 24px 0;
}
.welcome-examples {
  display: flex;
  justify-content: center;
  flex-wrap: wrap;
  gap: 8px;
  margin-bottom: 24px;
}
.welcome-examples .tag {
  padding: 6px 16px;
  background: #E6E6EA;
  border: 1px solid rgba(255,255,255,0.5);
  border-radius: 10px;
  font-size: 13px;
  font-weight: 600;
  color: #475569;
  font-family: 'SF Mono', 'Fira Code', monospace;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,.50);
}
.welcome-status {
  font-size: 12px; color: #94a3b8;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 6px;
}
.welcome-status .dot {
  width: 6px; height: 6px;
  border-radius: 50%;
  display: inline-block;
}
.welcome-status .dot.ok { background: #22c55e; }
.welcome-status .dot.err { background: #ef4444; }

/* ================================================================
   SPINNER
================================================================ */
.loading-card {
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  border: 1px solid rgba(255,255,255,0.38);
  border-radius: 10px;
  padding: 80px 40px;
  text-align: center;
  box-shadow: 0 8px 32px rgba(31,38,135,0.15);
  max-width: 500px;
  margin: 80px auto;
}
.loading-card .spinner-ring {
  width: 48px; height: 48px;
  border: 3px solid #E6E6EA;
  border-top-color: #1e293b;
  border-radius: 50%;
  animation: spin .8s linear infinite;
  margin: 0 auto 20px;
}
@keyframes spin { to { transform: rotate(360deg); } }
.loading-card p {
  color: #94a3b8; font-size: 14px; margin: 0;
}

/* ================================================================
   TABLE RESULTS CARD
================================================================ */
.results-card {
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  border: 1px solid rgba(255,255,255,0.38);
  border-radius: 10px;
  padding: 28px 32px;
  box-shadow: 0 8px 32px rgba(31,38,135,0.1);
  margin-bottom: 24px;
}
.results-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 4px;
}
.results-header h3 {
  font-size: 18px; font-weight: 700;
  color: #0f172a; margin: 0;
  letter-spacing: -.3px;
}
.results-count {
  font-size: 12px; font-weight: 600;
  color: #475569;
  background: #E6E6EA;
  border: 1px solid rgba(255,255,255,0.5);
  padding: 4px 12px;
  border-radius: 20px;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,.50);
}
.results-sub {
  color: #94a3b8; font-size: 13px;
  margin: 4px 0 18px;
}
.results-divider {
  height: 1px; background: rgba(255,255,255,0.3);
  margin: 0 0 16px;
}

/* DT table overrides */
.results-card table.dataTable { border-collapse: separate; border-spacing: 0; }
.results-card table.dataTable thead th {
  background: rgba(0,0,0,0.02) !important;
  color: #64748b !important;
  font-size: 11px !important;
  font-weight: 600 !important;
  text-transform: uppercase;
  letter-spacing: .6px;
  padding: 10px 16px !important;
  border-bottom: 1px solid rgba(0,0,0,0.06) !important;
  border-top: none !important;
}
.results-card table.dataTable tbody td {
  padding: 12px 16px !important;
  font-size: 13px;
  border-bottom: 1px solid rgba(0,0,0,0.04) !important;
  vertical-align: middle;
}
.results-card table.dataTable tbody tr { cursor: pointer; transition: background .15s; }
.results-card table.dataTable tbody tr:hover { background: rgba(230,230,234,0.5) !important; }
.results-card table.dataTable tbody tr.selected { background: #E6E6EA !important; }
.results-card .dataTables_wrapper { font-size: 13px; }
.results-card .dataTables_filter input {
  border: 1px solid rgba(255,255,255,0.5);
  border-radius: 10px;
  padding: 6px 12px;
  font-family: 'Inter', sans-serif;
  font-size: 13px;
  outline: none;
  background: #E6E6EA;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,.50);
  transition: all .2s;
}
.results-card .dataTables_filter input:focus {
  border-color: rgba(95,157,231,0.4);
  box-shadow: -2px -1px 8px 0px #ffffff,
              2px 1px 8px 0px rgb(95 157 231 / 30%),
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.35) inset;
}
.results-card .dataTables_info {
  font-size: 12px;
  color: #94a3b8;
}
.results-card .dataTables_paginate .paginate_button {
  border-radius: 8px !important;
  font-size: 12px !important;
  padding: 4px 10px !important;
}
.results-card .dataTables_paginate .paginate_button.current {
  background: #1e293b !important;
  color: #fff !important;
  border-color: #1e293b !important;
}

/* ================================================================
   CHAPTER HEADER BANNER
================================================================ */
.chapter-banner {
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  border: 1px solid rgba(255,255,255,0.38);
  border-radius: 10px;
  padding: 28px 32px;
  margin-bottom: 20px;
  box-shadow: 0 8px 32px rgba(31,38,135,0.1);
  border-left: 4px solid #1e293b;
}
.chapter-banner h2 {
  font-size: 22px; font-weight: 700;
  color: #0f172a; margin: 0 0 14px;
  letter-spacing: -.3px; line-height: 1.3;
}
.chapter-meta {
  display: flex; flex-wrap: wrap; gap: 8px;
}
.meta-tag {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  padding: 5px 14px;
  background: #E6E6EA;
  border: 1px solid rgba(255,255,255,0.5);
  border-radius: 20px;
  font-size: 12px;
  font-weight: 500;
  color: #475569;
  transition: all .15s;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,.50);
}
.meta-tag i { font-size: 11px; color: #94a3b8; }
.meta-tag.link-tag {
  color: #475569;
  border-color: rgba(255,255,255,0.5);
  background: #E6E6EA;
  text-decoration: none;
}
.meta-tag.link-tag:hover {
  box-shadow: -2px -1px 8px 0px #ffffff,
              2px 1px 8px 0px rgb(95 157 231 / 30%),
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.35) inset;
}

/* ================================================================
   CONTROL BAR
================================================================ */
.ctrl-bar {
  display: flex;
  align-items: center;
  gap: 12px;
  flex-wrap: wrap;
  margin-bottom: 20px;
  padding: 12px 20px;
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  border: 1px solid rgba(255,255,255,0.38);
  border-radius: 10px;
  box-shadow: 0 8px 32px rgba(31,38,135,0.08);
}
.ctrl-bar .btn-back {
  padding: 7px 16px;
  border: 1px solid rgba(255,255,255,0.8);
  border-radius: 10px;
  background: rgba(255,255,255,0.3);
  backdrop-filter: blur(8px);
  color: #475569;
  font-size: 13px;
  font-weight: 500;
  font-family: 'Inter', sans-serif;
  cursor: pointer;
  transition: all .15s;
  display: flex;
  align-items: center;
  gap: 6px;
}
.ctrl-bar .btn-back:hover {
  background: rgba(255,255,255,0.5);
  color: #0f172a;
}
.ctrl-bar .section-count {
  font-size: 13px;
  color: #94a3b8;
  display: flex;
  align-items: center;
  gap: 6px;
}
.ctrl-bar .btn-export {
  margin-left: auto;
  padding: 7px 18px;
  border: none;
  border-radius: 10px;
  background: #1e293b;
  color: #fff;
  font-size: 12px;
  font-weight: 600;
  font-family: 'Inter', sans-serif;
  cursor: pointer;
  transition: all .15s;
  display: flex;
  align-items: center;
  gap: 6px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.1);
}
.ctrl-bar .btn-export:hover {
  background: #334155;
  box-shadow: 0 2px 6px rgba(0,0,0,0.15);
}

/* ================================================================
   SECTION CARDS
================================================================ */
.sec-card {
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  border: 1px solid rgba(255,255,255,0.38);
  border-radius: 10px;
  margin-bottom: 16px;
  box-shadow: 0 8px 32px rgba(31,38,135,0.08);
  overflow: hidden;
  transition: box-shadow .2s;
}
.sec-card:hover {
  box-shadow: 0 8px 32px rgba(31,38,135,0.15);
}
.sec-header {
  padding: 16px 24px;
  display: flex;
  align-items: center;
  gap: 14px;
  border-bottom: 1px solid rgba(255,255,255,0.3);
}
.sec-icon {
  width: 38px; height: 38px;
  border-radius: 8px;
  border: 1px solid rgba(255,255,255,0.5);
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 15px;
  flex-shrink: 0;
  background: #E6E6EA;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,.50);
}
.sec-title-text {
  font-size: 15px;
  font-weight: 700;
  color: #0f172a;
  letter-spacing: -.1px;
}
.sec-body {
  padding: 22px 28px;
  font-size: 14px;
  line-height: 1.75;
  color: #475569;
  max-height: 700px;
  overflow-y: auto;
}

/* Section body content */
.sec-body p { margin: 0 0 12px; color: #475569; }
.sec-body h3, .sec-body h4, .sec-body h5 {
  margin: 20px 0 8px;
  font-weight: 600;
  color: #0f172a;
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: .5px;
  padding-bottom: 6px;
  border-bottom: 1px solid rgba(255,255,255,0.3);
}
.sec-body ul, .sec-body ol { margin: 0 0 14px; padding-left: 22px; }
.sec-body li { margin-bottom: 5px; color: #475569; }
.sec-body li::marker { color: #94a3b8; }
.sec-body dl { margin: 0 0 14px; }
.sec-body dt {
  font-weight: 700; color: #0f172a;
  margin-top: 10px; font-size: 13px;
}
.sec-body dd {
  margin-left: 18px; color: #475569;
  margin-bottom: 4px;
}
.sec-body strong, .sec-body b { color: #0f172a; font-weight: 700; }
.sec-body em, .sec-body i { color: #64748b; font-style: italic; }
.sec-body a {
  color: #475569;
  text-decoration: none;
  border-bottom: 1px dotted #94a3b8;
  transition: all .15s;
}
.sec-body a:hover {
  color: #0f172a;
  border-bottom-style: solid;
}

/* Tables inside sections */
.sec-body table {
  width: 100%; border-collapse: collapse;
  margin: 14px 0 18px; font-size: 13px;
  border-radius: 10px; overflow: hidden;
  box-shadow: 0 1px 4px rgba(0,0,0,.05);
}
.sec-body thead tr { background: rgba(0,0,0,0.02); }
.sec-body thead th {
  padding: 10px 14px; text-align: left;
  font-weight: 600; font-size: 11px;
  letter-spacing: .5px; text-transform: uppercase;
  color: #64748b; border-bottom: 1px solid rgba(0,0,0,0.06);
}
.sec-body tbody tr:nth-child(even) { background: rgba(230,230,234,0.3); }
.sec-body tbody tr:nth-child(odd) { background: transparent; }
.sec-body tbody tr:hover { background: rgba(230,230,234,0.5); }
.sec-body td {
  padding: 9px 14px; border-bottom: 1px solid rgba(0,0,0,0.04);
  vertical-align: top; color: #475569;
}
.sec-body td:first-child { font-weight: 600; color: #0f172a; }

/* Scrollbar inside sec-body */
.sec-body::-webkit-scrollbar { width: 5px; }
.sec-body::-webkit-scrollbar-track { background: transparent; }
.sec-body::-webkit-scrollbar-thumb {
  background: #d1d5db;
  border-radius: 10px;
}
.sec-body::-webkit-scrollbar-thumb:hover { background: #9ca3af; }

/* ================================================================
   SEGMENTED CONTROL
================================================================ */
.seg-control {
  display: flex;
  background: #e5e7eb;
  border-radius: 10px;
  padding: 3px;
  gap: 2px;
  margin-bottom: 16px;
  box-shadow: 0 2px 6px rgba(15,23,42,0.15);
}
.seg-btn {
  flex: 1;
  padding: 9px 8px;
  border: none;
  border-radius: 8px;
  background: transparent;
  color: #9ca3af;
  font-size: 12px;
  font-weight: 600;
  font-family: 'Inter', sans-serif;
  cursor: pointer;
  transition: all .2s ease;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 7px;
  white-space: nowrap;
}
.seg-btn:hover {
  color: #4b5563;
}
.seg-btn.active {
  background: #fff;
  color: #1e293b;
  box-shadow: 0 1px 3px rgba(15,23,42,0.25);
}
.seg-btn .seg-icon {
  width: 22px; height: 22px;
  border-radius: 4px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  font-size: 10px;
  color: #fff;
  flex-shrink: 0;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,.50);
}
.seg-panel {
  display: none;
}
.seg-panel.active {
  display: block;
}

/* ================================================================
   ALERTS
================================================================ */
.alert-warn {
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  border: 1px solid rgba(245,158,11,0.3);
  border-radius: 10px;
  padding: 14px 20px;
  margin-bottom: 16px;
  color: #a16207;
  font-size: 13px;
  display: flex;
  align-items: center;
  gap: 10px;
}
.alert-err {
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  border: 1px solid rgba(239,68,68,0.3);
  border-radius: 10px;
  padding: 14px 20px;
  margin-bottom: 16px;
  color: #dc2626;
  font-size: 13px;
  display: flex;
  align-items: center;
  gap: 10px;
}
.not-found-bar {
  text-align: center;
  padding: 14px 20px;
  color: #94a3b8;
  font-size: 12px;
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  border: 1px solid rgba(255,255,255,0.38);
  border-radius: 10px;
  margin-top: 8px;
}

/* ================================================================
   DEBUG PANEL
================================================================ */
.dbg-panel {
  background: #1e293b;
  color: #94a3b8;
  border-radius: 10px;
  padding: 16px 20px;
  font-family: 'SF Mono', 'Fira Code', 'Courier New', monospace;
  font-size: 11.5px;
  max-height: 320px;
  overflow-y: auto;
  margin-top: 20px;
  white-space: pre-wrap;
  box-shadow: 0 4px 16px rgba(0,0,0,.12);
}

/* ================================================================
   RESPONSIVE
================================================================ */
@media (max-width: 900px) {
  .app-sidebar {
    width: 240px;
    min-width: 240px;
    padding: 20px 16px;
  }
  .app-main {
    margin-left: 240px;
    padding: 24px 20px;
  }
}
@media (max-width: 680px) {
  .app-sidebar {
    position: relative;
    width: 100%;
    min-width: 100%;
    border-right: none;
    border-bottom: 1px solid rgba(255,255,255,0.38);
  }
  .app-main {
    margin-left: 0;
    padding: 20px 16px;
  }
  .app-shell { flex-direction: column; }
}

/* Hide default Shiny elements */
.shiny-input-container { width: 100% !important; }
.form-group { margin-bottom: 0; }

/* Modal override - relative sidebar when in a modal */
.modal .app-sidebar {
  position: relative;
  width: 100%;
  min-width: 100%;
  border-right: none;
  border-bottom: 1px solid rgba(255,255,255,0.38);
}
.modal .app-main {
  margin-left: 0;
}
.modal .app-shell {
  flex-direction: column;
}
"

# --------------------------------------------------------------------------- #
# JavaScript will be generated dynamically in the module UI
# --------------------------------------------------------------------------- #

# ============================================================================ #
# UI (module)
# ============================================================================ #
reviewExplorerUI <- function(id) {
  ns <- NS(id)

  module_js <- sprintf("
$(document).on('keypress','#%s',function(e){
  if(e.which===13) $('#%s').click();
});
$(document).on('click','.seg-btn',function(){
  var sec=$(this).data('section');
  $('.seg-btn').removeClass('active');
  $(this).addClass('active');
  $('.seg-panel').removeClass('active');
  $('.seg-panel[data-section=\"'+sec+'\"]').addClass('active');
});
", ns("query"), ns("btn_search"))

  tagList(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap",
        rel = "stylesheet"
      ),
      tags$link(
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css",
        rel = "stylesheet"
      ),
      tags$style(HTML(styles_css)),
      tags$script(HTML(module_js))
    ),

    # App shell
    div(class = "app-shell",

      # ---- SIDEBAR -------------------------------------------------------- #
      div(class = "app-sidebar",

        # Brand
        div(class = "sidebar-brand",
          div(class = "brand-icon", tags$i(class = "fa-solid fa-dna")),
          div(
            div(class = "brand-text", "GeneReviews"),
            div(class = "brand-sub", "Explorer")
          )
        ),

        # Search
        div(class = "sidebar-label", "Search"),
        div(class = "search-wrap",
          tags$i(class = "fa-solid fa-magnifying-glass search-icon"),
          textInput(ns("query"), NULL, placeholder = "Gene, disease, shortname...")
        ),
        tags$button(
          id = ns("btn_search"), type = "button",
          class = "sidebar-btn sidebar-btn-primary action-button",
          tags$i(class = "fa-solid fa-magnifying-glass"),
          "Search"
        ),

        # File status
        div(class = "sidebar-label", "Index"),
        uiOutput(ns("file_status")),

        # Examples
        div(class = "sidebar-label", "Examples"),
        div(class = "example-pills",
          span(class = "example-pill", "BRCA1"),
          span(class = "example-pill", "Marfan"),
          span(class = "example-pill", "Lynch"),
          span(class = "example-pill", "22q11"),
          span(class = "example-pill", "CFTR")
        ),

        # Filters (conditionally shown)
        conditionalPanel(
          paste0("output['", ns("show_filters"), "']"),
          div(class = "sidebar-label", style = "margin-top: 22px;", "Sections"),
          div(class = "sidebar-checks",
            checkboxGroupInput(ns("secs"), NULL, SEC_NAMES, SEC_NAMES)
          ),
          div(class = "sidebar-checks-actions",
            actionButton(ns("all_secs"), "All", class = "btn"),
            actionButton(ns("none_secs"), "None", class = "btn")
          ),
          div(class = "debug-check",
            checkboxInput(ns("show_debug"), "Log debug", FALSE)
          )
        )
      ),

      # ---- MAIN CONTENT --------------------------------------------------- #
      div(class = "app-main",
        uiOutput(ns("main_ui"))
      )
    )
  )
}

# ============================================================================ #
# SERVER (module)
# ============================================================================ #
reviewExplorerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(
    gr_table  = NULL, file_ok  = FALSE,
    filtered  = NULL, loading  = FALSE,
    err       = NULL, chapter  = NULL,
    nbk_sel   = NULL, row_sel  = NULL,
    dbg       = character(0)
  )

  ts <- function() format(Sys.time(), "%H:%M:%S")
  lg <- function(...) rv$dbg <- c(rv$dbg, paste0("[", ts(), "] ", ...))

  output$show_filters <- reactive({
    !is.null(rv$chapter)
  })
  outputOptions(output, "show_filters", suspendWhenHidden = FALSE)

  observeEvent(
    input$all_secs,
    updateCheckboxGroupInput(session, "secs", selected = SEC_NAMES)
  )
  observeEvent(
    input$none_secs,
    updateCheckboxGroupInput(session, "secs", selected = character(0))
  )

  # Chargement fichier local
  observe({
    if (!is.null(rv$gr_table)) {
      return()
    }
    df <- load_local_file(LOCAL_FILE)
    if (!is.null(df)) {
      rv$gr_table <- df
      rv$file_ok <- TRUE
    } else {
      rv$file_ok <- FALSE
    }
  })

  output$file_status <- renderUI({
    if (rv$file_ok && !is.null(rv$gr_table)) {
      div(class = "file-status",
        span(class = "status-dot ok"),
        span(nrow(rv$gr_table), " GeneReviews loaded")
      )
    } else {
      div(class = "file-status",
        span(class = "status-dot err"),
        span("File not found")
      )
    }
  })

  # ---------------------------------------------------------------------- #
  # Recherche
  # ---------------------------------------------------------------------- #
  observeEvent(input$btn_search, {
    q <- trimws(input$query)
    if (nchar(q) == 0) {
      return()
    }
    rv$err <- NULL
    rv$chapter <- NULL
    rv$nbk_sel <- NULL
    rv$row_sel <- NULL
    rv$filtered <- NULL
    rv$dbg <- character(0)
    lg(paste0("=== SEARCH: '", q, "' ==="))

    if (!rv$file_ok || is.null(rv$gr_table)) {
      rv$err <- paste0("File '", LOCAL_FILE, "' not found.")
      return()
    }

    tbl <- rv$gr_table
    q_low <- tolower(q)
    hits <- tbl[
      grepl(q_low, tolower(tbl$Shortname), fixed = TRUE) |
        grepl(q_low, tolower(tbl$NBK), fixed = TRUE) |
        grepl(q_low, tolower(tbl$Gene), fixed = TRUE) |
        grepl(q_low, tolower(tbl$Disease), fixed = TRUE),
    ]

    hits <- hits[!duplicated(hits$NBK), ]
    lg(paste0("Results: ", nrow(hits), " GeneReviews (after dedup)"))

    if (nrow(hits) == 0) {
      rv$err <- paste0("No results for \"", q, "\".")
      return()
    }
    rv$filtered <- hits
    if (nrow(hits) == 1) do_load_chapter(hits$NBK[1], hits[1, ])
  })

  # ---------------------------------------------------------------------- #
  # Charger un chapitre
  # ---------------------------------------------------------------------- #
  do_load_chapter <- function(nbk, row = NULL) {
    rv$nbk_sel <- nbk
    rv$row_sel <- row
    rv$chapter <- NULL
    rv$err <- NULL
    rv$loading <- TRUE
    lg(paste0("=== FETCH : ", nbk, " ==="))
    ch <- fetch_chapter(nbk, lg)
    rv$loading <- FALSE
    if (is.null(ch)) {
      rv$err <- paste0("Unable to load ", nbk, ".")
      return()
    }
    rv$chapter <- ch
    if (length(ch$sections) == 0) {
      rv$err <- paste0(
        "Page loaded \u2014 0 standard sections. Headers: ",
        paste(head(ch$headers, 8), collapse = " | ")
      )
    }
  }

  observeEvent(input$gr_table_rows_selected, {
    sel <- input$gr_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      return()
    }
    df <- rv$filtered
    if (is.null(df) || sel > nrow(df)) {
      return()
    }
    do_load_chapter(df$NBK[sel], df[sel, ])
  })

  observeEvent(input$btn_back, {
    rv$chapter <- NULL
    rv$nbk_sel <- NULL
    rv$err <- NULL
    rv$dbg <- character(0)
  })

  # ---------------------------------------------------------------------- #
  # Rendu principal
  # ---------------------------------------------------------------------- #
  output$main_ui <- renderUI({
    dbg_ui <- if (isTRUE(input$show_debug) && length(rv$dbg) > 0) {
      div(class = "dbg-panel", paste(rv$dbg, collapse = "\n"))
    }

    # Loading
    if (rv$loading) {
      return(div(class = "loading-card",
        div(class = "spinner-ring"),
        p("Loading from NCBI...")
      ))
    }

    # Error (no chapter loaded)
    if (!is.null(rv$err) && is.null(rv$chapter)) {
      parts <- list(
        div(class = "alert-err",
          tags$i(class = "fa-solid fa-circle-exclamation"),
          span(rv$err)
        )
      )
      if (!is.null(rv$nbk_sel)) {
        parts <- c(parts, list(div(
          style = "text-align:center; margin:16px 0; display:flex; justify-content:center; gap:10px;",
          tags$a(
            href = paste0(BOOKS_URL, rv$nbk_sel, "/"), target = "_blank",
            class = "sidebar-btn",
            style = "width:auto; padding:8px 20px; background:#10598A; color:#fff; text-decoration:none; border-radius:10px; font-size:13px; font-weight:600;",
            tags$i(class = "fa-solid fa-arrow-up-right-from-square"),
            " View on NCBI"
          ),
          if (!is.null(rv$filtered) && nrow(rv$filtered) > 1) {
            actionButton(ns("btn_back"), HTML("&larr; Back"),
              class = "btn-back",
              style = "border:1.5px solid #D9E1E6; border-radius:10px; background:#fff; color:#6B645C; font-size:13px; padding:8px 20px;"
            )
          }
        )))
      }
      return(tagList(parts, dbg_ui))
    }

    # ---- Vue chapitre --------------------------------------------------- #
    if (!is.null(rv$chapter)) {
      ch <- rv$chapter
      ss <- ch$sections
      sel <- input$secs
      nbk <- rv$nbk_sel
      u <- paste0(BOOKS_URL, nbk, "/")
      row <- rv$row_sel

      # Banner
      tags_list <- list()
      if (!is.null(row)) {
        if (nchar(row$Shortname) > 0) {
          tags_list <- c(tags_list, list(span(class = "meta-tag",
            tags$i(class = "fa-solid fa-tag"), row$Shortname
          )))
        }
        tags_list <- c(tags_list, list(span(class = "meta-tag",
          tags$i(class = "fa-solid fa-book"), nbk
        )))
        if (nchar(row$Gene) > 0 && row$Gene != "Not applicable") {
          tags_list <- c(tags_list, list(span(class = "meta-tag",
            tags$i(class = "fa-solid fa-dna"), row$Gene
          )))
        }
      }
      tags_list <- c(tags_list, list(
        tags$a(
          href = u, target = "_blank",
          class = "meta-tag link-tag",
          tags$i(class = "fa-solid fa-arrow-up-right-from-square"), "NCBI"
        )
      ))

      header <- div(class = "chapter-banner",
        h2(if (nchar(ch$title) > 3) ch$title else row$Disease),
        div(class = "chapter-meta", tags_list)
      )

      n_shown <- sum(sel %in% names(ss))
      n_avail <- sum(SEC_NAMES %in% names(ss))

      ctrl_bar <- div(class = "ctrl-bar",
        if (!is.null(rv$filtered) && nrow(rv$filtered) > 1) {
          actionButton(ns("btn_back"), HTML("&larr; Back"), class = "btn-back")
        },
        span(class = "section-count",
          tags$i(class = "fa-solid fa-layer-group"),
          paste0(n_shown, " / ", n_avail, " sections")
        ),
        downloadButton(ns("btn_dl"), "Export TXT", class = "btn-export")
      )

      warn_ui <- if (!is.null(rv$err)) {
        div(class = "alert-warn",
          tags$i(class = "fa-solid fa-triangle-exclamation"),
          span(rv$err)
        )
      }

      # Segmented control navigation
      avail_secs <- SEC_NAMES[SEC_NAMES %in% sel & SEC_NAMES %in% names(ss)]
      if (length(avail_secs) > 0) {
        active_sec <- avail_secs[1]

        seg_btns <- lapply(avail_secs, function(sn) {
          d <- SECTIONS[[sn]]
          act <- if (sn == active_sec) " active" else ""
          tags$button(
            class = paste0("seg-btn", act),
            `data-section` = sn,
            div(class = "seg-icon",
              style = paste0("background:", d$col, ";"),
              tags$i(class = paste0("fa-solid fa-", d$icon))
            ),
            span(sn)
          )
        })

        seg_panels <- lapply(avail_secs, function(sn) {
          d <- SECTIONS[[sn]]
          act <- if (sn == active_sec) " active" else ""
          div(class = paste0("seg-panel", act),
            `data-section` = sn,
            div(class = "sec-card",
              div(class = "sec-header",
                style = paste0("background:", d$bg, ";"),
                div(class = "sec-icon",
                  style = paste0("background:", d$col, "; color:#fff;"),
                  tags$i(class = paste0("fa-solid fa-", d$icon))
                ),
                span(class = "sec-title-text",
                  style = paste0("color:", d$col, ";"), sn
                )
              ),
              div(class = "sec-body", HTML(ss[[sn]]))
            )
          )
        })

        cards <- tagList(
          div(class = "seg-control", seg_btns),
          seg_panels
        )
      } else {
        cards <- NULL
      }

      miss <- setdiff(SEC_NAMES, names(ss))
      miss_ui <- if (length(miss) > 0) {
        div(class = "not-found-bar",
          tags$i(class = "fa-solid fa-circle-info"),
          " Sections not found: ", paste(miss, collapse = ", ")
        )
      }

      return(tagList(header, ctrl_bar, warn_ui, cards, miss_ui, dbg_ui))
    }

    # ---- Tableau resultats ---------------------------------------------- #
    if (!is.null(rv$filtered) && nrow(rv$filtered) > 0) {
      return(tagList(
        div(class = "results-card",
          div(class = "results-header",
            h3("Results"),
            span(class = "results-count", nrow(rv$filtered), " found")
          ),
          p(class = "results-sub", "Click a row to view full content."),
          div(class = "results-divider"),
          DTOutput(ns("gr_table"))
        ),
        dbg_ui
      ))
    }

    # ---- Accueil -------------------------------------------------------- #
    tagList(
      div(class = "welcome-card",
        div(class = "welcome-icon", tags$i(class = "fa-solid fa-dna")),
        h2("GeneReviews Explorer"),
        p(class = "welcome-sub",
          "Search the local index, view details via NCBI."
        ),
        div(class = "welcome-divider"),
        div(class = "welcome-examples",
          lapply(c("BRCA1", "Marfan", "Lynch", "22q11", "CFTR", "PKD1"), function(ex) {
            span(class = "tag", ex)
          })
        ),
        div(class = "welcome-divider"),
        div(class = "welcome-status",
          span(class = paste0("dot ", if (rv$file_ok) "ok" else "err")),
          if (rv$file_ok) "Index loaded" else "Index missing"
        )
      ),
      dbg_ui
    )
  })

  # ---------------------------------------------------------------------- #
  # DataTable
  # ---------------------------------------------------------------------- #
  output$gr_table <- renderDT(
    {
      req(rv$filtered)
      df <- rv$filtered[, c("Shortname", "NBK", "Gene", "Disease")]
      colnames(df) <- c("GR Shortname", "NBK ID", "Gene", "Disease")
      datatable(df,
        selection = "single", rownames = FALSE,
        options = list(
          pageLength = 15, scrollX = TRUE, dom = "ftip",
          language = list(
            search = "Filter:",
            paginate = list(previous = "Previous", `next` = "Next"),
            info = "_START_\u2013_END_ of _TOTAL_",
            zeroRecords = "No results"
          )
        ),
        class = "display compact hover"
      ) |>
        formatStyle("GR Shortname",
          color = "#9AA6B3", fontSize = "12px",
          fontFamily = "monospace"
        ) |>
        formatStyle("NBK ID",
          fontWeight = "600", color = "#10598A"
        ) |>
        formatStyle("Gene",
          fontWeight = "600", color = "#8BA398"
        ) |>
        formatStyle("Disease", color = "#534D45")
    },
    server = FALSE
  )

  # ---------------------------------------------------------------------- #
  # Export TXT
  # ---------------------------------------------------------------------- #
  output$btn_dl <- downloadHandler(
    filename = function() paste0("GeneReview_", rv$nbk_sel, ".txt"),
    content = function(file) {
      ch <- rv$chapter
      ss <- ch$sections
      sel <- input$secs
      row <- rv$row_sel

      html_to_text <- function(html_str) {
        tryCatch(
          {
            d <- read_html(html_str)
            txt <- xml_text(d)
            trimws(gsub("\\s{3,}", "\n\n", txt))
          },
          error = function(e) html_str
        )
      }

      lines <- c(
        if (nchar(ch$title) > 3) ch$title else row$Disease,
        paste0("NBK       : ", rv$nbk_sel),
        if (!is.null(row)) paste0("Shortname : ", row$Shortname),
        if (!is.null(row)) paste0("Gene      : ", row$Gene),
        if (!is.null(row)) paste0("Disease   : ", row$Disease),
        paste0("URL       : ", BOOKS_URL, rv$nbk_sel, "/"),
        paste(rep("=", 70), collapse = ""), ""
      )

      for (sn in SEC_NAMES) {
        if (sn %in% sel && !is.null(ss[[sn]])) {
          lines <- c(
            lines, paste0("## ", sn),
            paste(rep("-", 50), collapse = ""),
            html_to_text(ss[[sn]]), ""
          )
        }
      }

      lines <- c(lines, "---", paste0("Exported: ", Sys.Date()))
      writeLines(lines, file)
    }
  )
  })
}

# ============================================================================ #
# Standalone guard
# ============================================================================ #
if (sys.nframe() == 0L) {
  shinyApp(
    ui = fluidPage(lang = "en", reviewExplorerUI("app")),
    server = function(input, output, session) { reviewExplorerServer("app") }
  )
}
