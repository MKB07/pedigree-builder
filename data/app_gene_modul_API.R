# =============================================================================
# UniProt Gene Explorer — Glassmorphism Light Blue-Grey
# =============================================================================
# install.packages(c("shiny", "httr2", "jsonlite", "DT", "visNetwork"))
# =============================================================================

library(shiny)
library(httr2)
library(jsonlite)
library(DT)
library(visNetwork)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

API_TIMEOUT_SEC <- 12
API_USER_AGENT <- "GeneExplorerShiny/3.1 (research and education; Shiny app)"

api_perform <- function(req) {
  req |>
    req_timeout(API_TIMEOUT_SEC) |>
    req_headers("User-Agent" = API_USER_AGENT) |>
    req_perform()
}

# =============================================================================
#  CHROMOSOME DATA (GRCh38)
# =============================================================================
CHR_LEN <- c(
  "1" = 248956422, "2" = 242193529, "3" = 198295559, "4" = 190214555, "5" = 181538259,
  "6" = 170805979, "7" = 159345973, "8" = 145138636, "9" = 138394717, "10" = 133797422,
  "11" = 135086622, "12" = 133275309, "13" = 114364328, "14" = 107043718, "15" = 101991189,
  "16" = 90338345, "17" = 83257441, "18" = 80373285, "19" = 58617616, "20" = 64444167,
  "21" = 46709983, "22" = 50818468, "X" = 156040895, "Y" = 57227415
)
CHR_CEN <- c(
  "1" = 123400000, "2" = 93900000, "3" = 90900000, "4" = 50000000, "5" = 48800000,
  "6" = 59800000, "7" = 60100000, "8" = 45200000, "9" = 43000000, "10" = 39800000,
  "11" = 53400000, "12" = 35500000, "13" = 17700000, "14" = 17200000, "15" = 19000000,
  "16" = 36800000, "17" = 25100000, "18" = 18500000, "19" = 26200000, "20" = 28100000,
  "21" = 12000000, "22" = 15000000, "X" = 61000000, "Y" = 10400000
)

# =============================================================================
#  HELPER FUNCTIONS
# =============================================================================
safe <- function(expr, default = "-") tryCatch(expr, error = function(e) default)

scalar <- function(x, default = "-") {
  tryCatch({
    if (is.null(x) || length(x) == 0) return(default)
    v <- if (is.list(x)) x[[1]] else x
    if (is.null(v) || length(v) == 0) return(default)
    s <- as.character(v)
    if (length(s) == 0 || is.na(s[1]) || s[1] %in% c("NA", "NULL", "null", "")) default else s[1]
  }, error = function(e) default)
}

fmt <- function(n) tryCatch(format(as.integer(n), big.mark = ","), error = function(e) as.character(n))

safe_df <- function(items, row_fn) {
  rows <- Filter(Negate(is.null), lapply(items, function(i) {
    tryCatch(row_fn(i), error = function(e) NULL)
  }))
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

get_name <- function(e) {
  safe(e$proteinDescription$recommendedName$fullName$value,
       safe(e$proteinDescription$submissionNames[[1]]$fullName$value, "Unknown"))
}
get_gene <- function(e) safe(e$genes[[1]]$geneName$value, "Unknown")

get_cc_type <- function(e, type) {
  if (is.null(e$comments)) return(list())
  Filter(function(c) identical(c$commentType, type), e$comments)
}

taxid_sp <- function(tid) {
  c("9606" = "homo_sapiens", "10090" = "mus_musculus", "10116" = "rattus_norvegicus",
    "7955" = "danio_rerio", "7227" = "drosophila_melanogaster",
    "6239" = "caenorhabditis_elegans")[as.character(tid)] %||% "homo_sapiens"
}

# =============================================================================
#  API FUNCTIONS — UniProt / Ensembl / MyGene / PubMed / STRING
# =============================================================================
api_uniprot_search <- function(query, organism, limit = 25) {
  tryCatch({
    resp <- request("https://rest.uniprot.org/uniprotkb/search") |>
      req_url_query(
        query = paste0("(gene:", query, ") AND (organism_id:", organism, ")"),
        format = "json", size = limit
      ) |>
      req_headers("Accept" = "application/json") |>
      api_perform()
    fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  }, error = function(e) NULL)
}

api_uniprot_entry <- function(acc) {
  tryCatch({
    resp <- request(paste0("https://rest.uniprot.org/uniprotkb/", acc, ".json")) |>
      req_headers("Accept" = "application/json") |>
      api_perform()
    fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  }, error = function(e) NULL)
}

api_ensembl_lookup <- function(gene, species = "homo_sapiens") {
  tryCatch({
    resp <- request(paste0("https://rest.ensembl.org/lookup/symbol/", species, "/", gene)) |>
      req_url_query(`content-type` = "application/json", expand = 1) |>
      req_headers("Content-Type" = "application/json") |>
      api_perform()
    fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  }, error = function(e) NULL)
}

api_ensembl_phenotypes <- function(ensembl_id) {
  if (is.null(ensembl_id)) return(NULL)
  tryCatch({
    resp <- request(paste0("https://rest.ensembl.org/phenotype/gene/homo_sapiens/", ensembl_id)) |>
      req_url_query(`content-type` = "application/json") |>
      req_headers("Accept" = "application/json") |>
      api_perform()
    fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  }, error = function(e) NULL)
}

api_mygene <- function(symbol, species = "human") {
  tryCatch({
    fields <- paste(c(
      "symbol", "name", "summary", "entrezgene", "HGNC", "MIM",
      "ensembl.gene", "uniprot.Swiss-Prot", "type_of_gene", "biotype",
      "genomic_pos", "chromosome", "alias"
    ), collapse = ",")
    resp <- request("https://mygene.info/v3/query") |>
      req_url_query(q = symbol, species = species, size = 1, fields = fields) |>
      req_headers("Accept" = "application/json") |>
      api_perform()
    res <- fromJSON(resp_body_string(resp), simplifyVector = FALSE)
    hits <- res[["hits"]]
    if (!is.null(hits) && length(hits) > 0) hits[[1]] else NULL
  }, error = function(e) NULL)
}

api_pubmed <- function(symbol, n = 20) {
  tryCatch({
    r1 <- request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi") |>
      req_url_query(
        db = "pubmed",
        term = paste0('"', symbol, '"[Gene Name] OR "', symbol, '"[Title]'),
        retmax = n, retmode = "json", sort = "relevance"
      ) |>
      api_perform()
    ids <- unlist(fromJSON(resp_body_string(r1), simplifyVector = FALSE)[["esearchresult"]][["idlist"]])
    if (is.null(ids) || length(ids) == 0) return(NULL)
    r2 <- request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi") |>
      req_url_query(db = "pubmed", id = paste(ids, collapse = ","), retmode = "json") |>
      api_perform()
    arts <- fromJSON(resp_body_string(r2), simplifyVector = FALSE)[["result"]]
    if (is.null(arts)) return(NULL)
    arts[["uids"]] <- NULL
    rows <- lapply(arts, function(a) {
      auth <- tryCatch({
        au <- a[["authors"]]
        if (is.null(au) || length(au) == 0) return("")
        paste(sapply(au[seq_len(min(3, length(au)))],
                     function(x) tryCatch(as.character(x[["name"]]), error = function(e) "")),
              collapse = ", ")
      }, error = function(e) "")
      data.frame(
        PMID = scalar(a[["uid"]]), Title = scalar(a[["title"]]),
        Authors = auth, Journal = scalar(a[["fulljournalname"]]),
        Year = substr(scalar(a[["pubdate"]], ""), 1, 4), stringsAsFactors = FALSE
      )
    })
    do.call(rbind, Filter(Negate(is.null), rows))
  }, error = function(e) NULL)
}

api_string <- function(symbol, species = 9606, limit = 20) {
  tryCatch({
    resp <- request("https://string-db.org/api/json/interaction_partners") |>
      req_url_query(
        identifiers = symbol, species = as.character(species),
        limit = as.character(limit), caller_identity = "GeneExplorer"
      ) |>
      req_headers("Accept" = "application/json") |>
      req_timeout(25) |>
      api_perform()
    if (resp_status(resp) != 200) return(NULL)
    fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  }, error = function(e) NULL)
}

api_reactome_pathways <- function(symbol, limit = 12) {
  tryCatch({
    resp <- request("https://reactome.org/ContentService/search/query") |>
      req_url_query(
        query = symbol,
        species = "Homo sapiens",
        types = "Pathway",
        cluster = "true"
      ) |>
      req_headers("Accept" = "application/json") |>
      api_perform()
    res <- fromJSON(resp_body_string(resp), simplifyVector = FALSE)
    groups <- res[["results"]]
    if (is.null(groups) || length(groups) == 0) return(NULL)
    entries <- groups[[1]][["entries"]]
    if (is.null(entries) || length(entries) == 0) return(NULL)
    head(entries, limit)
  }, error = function(e) NULL)
}

api_interpro_domains <- function(uniprot_id, limit = 15) {
  if (is.null(uniprot_id) || !nzchar(uniprot_id)) return(NULL)
  tryCatch({
    resp <- request(paste0(
      "https://www.ebi.ac.uk/interpro/api/entry/all/protein/UniProt/",
      uniprot_id
    )) |>
      req_url_query(page_size = limit) |>
      req_headers("Accept" = "application/json") |>
      api_perform()
    fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  }, error = function(e) NULL)
}

api_myvariant_clinvar <- function(symbol, facet_size = 10, size = 200) {
  tryCatch({
    resp <- request("https://myvariant.info/v1/query") |>
      req_url_query(
        q = paste0("clinvar.gene.symbol:", symbol),
        fields = "clinvar",
        size = size,
        facets = "clinvar.clinical_significance",
        facet_size = facet_size
      ) |>
      req_headers("Accept" = "application/json") |>
      api_perform()
    fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  }, error = function(e) NULL)
}

api_panelapp <- function(symbol, limit = 20) {
  tryCatch({
    resp <- request("https://panelapp.genomicsengland.co.uk/api/v1/genes/") |>
      req_url_query(entity_name = symbol, format = "json") |>
      req_headers("Accept" = "application/json") |>
      api_perform()
    res <- fromJSON(resp_body_string(resp), simplifyVector = FALSE)
    if (!is.null(res[["results"]]) && length(res[["results"]]) > limit) {
      res[["results"]] <- res[["results"]][seq_len(limit)]
    }
    res
  }, error = function(e) NULL)
}

# =============================================================================
#  MONARCH INITIATIVE — v3 API
# =============================================================================
MONARCH_BASE <- "https://api-v3.monarchinitiative.org/v3/api"

m_str <- function(x) {
  tryCatch({
    if (is.null(x) || length(x) == 0) return("")
    if (is.logical(x)) return("")
    if (is.list(x)) {
      if (length(x) == 0) return("")
      x <- x[[1]]
      if (is.null(x)) return("")
    }
    s <- as.character(x)[[1]]
    if (is.na(s) || s %in% c("NA", "NULL", "null", "", "none", "None", "false", "FALSE")) return("")
    trimws(s)
  }, error = function(e) "")
}

api_monarch_search <- function(symbol) {
  tryCatch({
    resp <- request(paste0(MONARCH_BASE, "/search")) |>
      req_url_query(q = symbol, category = "biolink:Gene", limit = 5) |>
      req_headers("Accept" = "application/json", "User-Agent" = "GeneExplorerShiny/3.0") |>
      req_timeout(20) |>
      api_perform()
    if (resp_status(resp) != 200) return(NULL)
    fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  }, error = function(e) NULL)
}

api_monarch_pheno <- function(mid, limit = 200) {
  if (is.null(mid) || nchar(mid) == 0) return(NULL)
  tryCatch({
    url <- paste0(MONARCH_BASE, "/entity/",
                  URLencode(mid, reserved = TRUE),
                  "/biolink:GeneToPhenotypicFeatureAssociation")
    resp <- request(url) |>
      req_url_query(limit = limit) |>
      req_headers("Accept" = "application/json", "User-Agent" = "GeneExplorerShiny/3.0") |>
      req_timeout(30) |>
      api_perform()
    if (resp_status(resp) != 200) return(NULL)
    fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  }, error = function(e) NULL)
}

freq_colour <- function(lbl) {
  switch(tolower(trimws(lbl)),
    "obligate" = , "always present" = "#16a34a",
    "very frequent" = "#2563eb",
    "frequent"      = "#0891b2",
    "occasional"    = "#ca8a04",
    "very rare"     = "#ea580c",
    "excluded"      = "#dc2626",
    "#64748b"
  )
}

freq_badge <- function(lbl) {
  if (!nzchar(lbl)) return("")
  col <- freq_colour(lbl)
  paste0('<span style="background:', col,
         ';color:white;padding:2px 8px;border-radius:10px;',
         'font-size:11px;white-space:nowrap;font-weight:600">',
         htmltools::htmlEscape(lbl), '</span>')
}

build_monarch_df <- function(raw) {
  items <- tryCatch(raw[["items"]] %||% raw[["associations"]] %||% list(),
                    error = function(e) list())
  if (length(items) == 0) return(NULL)
  rows <- lapply(items, function(a) {
    tryCatch({
      gene_lbl   <- m_str(a[["subject_label"]])
      if (!nzchar(gene_lbl)) gene_lbl <- m_str(a[["subject"]])
      pred       <- gsub("^biolink:", "", m_str(a[["predicate"]]))
      pheno_lbl  <- m_str(a[["object_label"]])
      if (!nzchar(pheno_lbl)) pheno_lbl <- m_str(a[["object"]])
      pheno_id   <- m_str(a[["object"]])
      freq_lbl   <- m_str(a[["frequency_qualifier_label"]])
      freq_id    <- m_str(a[["frequency_qualifier"]])
      freq_display <- if (nzchar(freq_lbl)) freq_lbl else freq_id
      onset_lbl  <- m_str(a[["onset_qualifier_label"]])
      onset_id   <- m_str(a[["onset_qualifier"]])
      onset_display <- if (nzchar(onset_lbl)) onset_lbl else onset_id
      src <- m_str(a[["primary_knowledge_source"]])
      if (!nzchar(src)) {
        pb <- a[["provided_by"]]
        if (is.list(pb) && length(pb) > 0) src <- m_str(pb[[1]])
      }
      src <- gsub("^infores:", "", src)
      data.frame(Gene = gene_lbl, Association = pred, Phenotype = pheno_lbl,
                 HP_ID = pheno_id, Frequency = freq_display,
                 Onset = onset_display, Source = src, stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# =============================================================================
#  CHROMOSOME SVG (light-theme palette)
# =============================================================================
make_chr_svg <- function(chr_name, gstart, gend, gene, strand = 1) {
  ck   <- gsub("^chr", "", as.character(chr_name))
  clen <- CHR_LEN[ck]; cpos <- CHR_CEN[ck]
  if (is.na(clen)) return(NULL)
  W <- 760; H <- 170; ML <- 50; MR <- 30; CW <- W - ML - MR
  CY <- 80; CH <- 26; CT <- CY - CH / 2
  sc <- CW / clen
  gx1 <- ML + gstart * sc; gx2 <- ML + gend * sc
  gmid <- (gx1 + gx2) / 2; gw <- max(gx2 - gx1, 4)
  cx <- ML + cpos * sc
  st <- if (strand >= 0) "+" else "-"
  ti <- if (clen > 150e6) 25e6 else if (clen > 80e6) 20e6 else 10e6
  tks <- paste0(vapply(seq(0, clen, by = ti), function(p) {
    tx <- round(ML + p * sc, 1)
    paste0('<line x1="', tx, '" y1="', CT + CH + 3, '" x2="', tx, '" y2="', CT + CH + 9,
           '" stroke="#c0cdd8" stroke-width="0.7"/>',
           '<text x="', tx, '" y="', CT + CH + 19,
           '" text-anchor="middle" font-size="8" fill="#8a9bb5">', round(p / 1e6), 'Mb</text>')
  }, character(1)), collapse = "")
  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ', W, ' ', H,
    '" style="width:100%;max-width:', W, 'px;height:auto;font-family:-apple-system,sans-serif;">',
    '<text x="', ML, '" y="18" font-size="14" font-weight="bold" fill="#2d6da3">Chromosome ', ck, '</text>',
    '<text x="', ML, '" y="34" font-size="10" fill="#6b7f96">', gene, ' (', fmt(gstart),
    ' - ', fmt(gend), ', ', fmt(gend - gstart), ' bp, strand ', st, ')</text>',
    '<rect x="', round(ML, 1), '" y="', CT, '" width="', round(cx - ML, 1), '" height="', CH,
    '" rx="13" fill="#dbe6f0" stroke="#9db8d4" stroke-width="1.2"/>',
    '<rect x="', round(cx, 1), '" y="', CT, '" width="', round(ML + CW - cx, 1), '" height="', CH,
    '" rx="13" fill="#dbe6f0" stroke="#9db8d4" stroke-width="1.2"/>',
    '<ellipse cx="', round(cx, 1), '" cy="', CY, '" rx="4" ry="', CH / 2,
    '" fill="#7fa8c9" stroke="#5a8db5" stroke-width="1"/>',
    tks,
    '<rect x="', round(gx1, 1), '" y="', CT - 3, '" width="', round(gw, 1), '" height="', CH + 6,
    '" fill="#dc2626" opacity="0.85" rx="2"/>',
    '<line x1="', round(gmid, 1), '" y1="', CT - 3, '" x2="', round(gmid, 1), '" y2="47"',
    ' stroke="#dc2626" stroke-width="1" stroke-dasharray="3,3"/>',
    '<rect x="', round(gmid - 28, 1), '" y="36" width="56" height="14" rx="7" fill="#dc2626"/>',
    '<text x="', round(gmid, 1), '" y="47" font-size="9" font-weight="bold" fill="white"',
    ' text-anchor="middle">', gene, '</text></svg>')
}

# =============================================================================
#  UI COMPONENTS
# =============================================================================
glass_card <- function(..., header = NULL, class = "") {
  tags$div(
    class = paste("glass-card", class),
    if (!is.null(header)) tags$div(class = "glass-card-header", header),
    tags$div(class = "glass-card-body", ...)
  )
}

kv_card <- function(label, value) {
  tags$div(class = "kv-card",
    tags$div(class = "kv-label", label),
    tags$div(class = "kv-value", value)
  )
}

ext_link <- function(label, url) {
  tags$a(label, href = url, target = "_blank", class = "ext-link")
}

empty_state <- function(msg, ico = "dna") {
  tags$div(class = "empty-state",
    icon(ico, class = "empty-state-icon"),
    tags$p(msg, class = "empty-state-text")
  )
}

glass_alert <- function(msg, type = "info", ico = NULL) {
  tags$div(class = paste0("glass-alert glass-alert-", type),
    if (!is.null(ico)) icon(ico),
    msg
  )
}

section_label <- function(text) {
  tags$div(class = "section-label", text)
}

DT_OPTS <- list(
  pageLength = 10, scrollX = TRUE,
  language = list(
    search = "Filter:", lengthMenu = "Show _MENU_ rows",
    info = "_START_-_END_ of _TOTAL_",
    paginate = list(previous = "\u25C0", `next` = "\u25B6")
  )
)

# =============================================================================
#  CSS — LIGHT GLASSMORPHISM BLUE-GREY THEME
# =============================================================================
APP_CSS <- "
/* ── Reset & Base ─────────────────────────────────────────────────────────── */
*, *::before, *::after { box-sizing: border-box; }

body {
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  background: #f0f0f3;
  color: #1e293b;
  min-height: 100vh;
  margin: 0;
  -webkit-font-smoothing: antialiased;
}

a { color: #475569; }
a:hover { color: #0f172a; }
hr { border-color: rgba(0,0,0,0.06); }

.container-fluid { max-width: 1260px; padding: 0 24px; }

/* ── App Header ───────────────────────────────────────────────────────────── */
.app-header {
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  border-bottom: 1px solid rgba(255,255,255,0.38);
  padding: 18px 32px;
  margin-bottom: 0;
  box-shadow: 0 8px 32px rgba(31,38,135,0.08);
}
.app-header-inner {
  display: flex;
  align-items: center;
  gap: 14px;
  max-width: 1260px;
  margin: 0 auto;
}
.header-icon { font-size: 22px; color: #475569; }
.header-title { font-size: 20px; font-weight: 700; color: #0f172a; letter-spacing: -0.3px; }
.header-sub { font-size: 12px; color: #94a3b8; margin-left: 4px; }

/* ── App Body ─────────────────────────────────────────────────────────────── */
.app-body { max-width: 1260px; margin: 0 auto; padding: 24px 24px 60px; }

/* ── Segmented Control (main nav) ─────────────────────────────────────────── */
.seg-main { margin-bottom: 24px; }
.seg-main > .tabbable > .nav-pills {
  background: #e5e7eb;
  border-radius: 10px;
  padding: 3px;
  display: inline-flex;
  flex-wrap: wrap;
  gap: 2px;
  margin-bottom: 24px;
  overflow-x: auto;
  box-shadow: 0 2px 6px rgba(15,23,42,0.15);
}
.seg-main > .tabbable > .nav-pills > li { margin: 0; }
.seg-main > .tabbable > .nav-pills > li > a {
  border-radius: 8px;
  color: #9ca3af;
  padding: 9px 18px;
  font-size: 13px;
  font-weight: 500;
  transition: all 0.2s ease;
  border: none;
  background: transparent;
  white-space: nowrap;
  display: flex;
  align-items: center;
  gap: 6px;
}
.seg-main > .tabbable > .nav-pills > li > a:hover {
  color: #4b5563;
}
.seg-main > .tabbable > .nav-pills > li.active > a,
.seg-main > .tabbable > .nav-pills > li.active > a:hover,
.seg-main > .tabbable > .nav-pills > li.active > a:focus {
  background: #ffffff !important;
  color: #1e293b !important;
  border: none;
  box-shadow: 0 1px 3px rgba(15,23,42,0.25);
}
.seg-main > .tabbable > .nav-pills > li > a .fa { font-size: 12px; }

/* ── Segmented Control (sub nav — clinical) ───────────────────────────────── */
.seg-sub { margin-bottom: 20px; }
.seg-sub > .tabbable > .nav-pills {
  background: #e5e7eb;
  border-radius: 10px;
  padding: 3px;
  display: inline-flex;
  gap: 2px;
  margin-bottom: 20px;
  box-shadow: 0 2px 6px rgba(15,23,42,0.1);
}
.seg-sub > .tabbable > .nav-pills > li { margin: 0; }
.seg-sub > .tabbable > .nav-pills > li > a {
  border-radius: 8px;
  color: #9ca3af;
  padding: 7px 14px;
  font-size: 12px;
  font-weight: 500;
  transition: all 0.2s ease;
  border: none;
  background: transparent;
}
.seg-sub > .tabbable > .nav-pills > li > a:hover { color: #4b5563; }
.seg-sub > .tabbable > .nav-pills > li.active > a,
.seg-sub > .tabbable > .nav-pills > li.active > a:hover {
  background: #ffffff !important;
  color: #1e293b !important;
  border: none;
  box-shadow: 0 1px 3px rgba(15,23,42,0.2);
}

/* ── Tab content reset ────────────────────────────────────────────────────── */
.tab-content > .tab-pane { padding: 0; }

/* ── Glass Card ───────────────────────────────────────────────────────────── */
.glass-card {
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  border: 1px solid rgba(255,255,255,0.38);
  border-radius: 10px;
  margin-bottom: 20px;
  overflow: hidden;
  transition: border-color 0.3s ease, box-shadow 0.3s ease;
  box-shadow: 0 8px 32px rgba(31,38,135,0.1);
}
.glass-card:hover { border-color: rgba(255,255,255,0.55); box-shadow: 0 8px 32px rgba(31,38,135,0.15); }
.glass-card-header {
  padding: 14px 20px;
  border-bottom: 1px solid rgba(255,255,255,0.3);
  font-size: 14px;
  font-weight: 600;
  color: #0f172a;
  display: flex;
  align-items: center;
  gap: 8px;
}
.glass-card-body { padding: 20px; }

/* ── Key-Value Cards ──────────────────────────────────────────────────────── */
.kv-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(155px, 1fr));
  gap: 10px;
  margin-bottom: 20px;
}
.kv-card {
  background: #E6E6EA;
  border: 1px solid rgba(255,255,255,0.5);
  border-radius: 10px;
  padding: 12px 14px;
  transition: all 0.25s ease;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50);
}
.kv-card:hover {
  box-shadow: -2px -1px 8px 0px #ffffff,
              2px 1px 8px 0px rgb(95 157 231 / 30%),
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.35) inset;
}
.kv-label {
  font-size: 9px;
  text-transform: uppercase;
  letter-spacing: 1px;
  color: #94a3b8;
  margin-bottom: 4px;
}
.kv-value {
  font-size: 14px;
  font-weight: 600;
  color: #0f172a;
  word-break: break-all;
  line-height: 1.3;
}

/* ── Section Labels ───────────────────────────────────────────────────────── */
.section-label {
  font-size: 10px;
  text-transform: uppercase;
  letter-spacing: 1px;
  color: #64748b;
  font-weight: 700;
  margin-bottom: 10px;
  margin-top: 16px;
}

/* ── External Links ───────────────────────────────────────────────────────── */
.ext-link {
  display: inline-block;
  padding: 6px 14px;
  margin: 3px;
  border-radius: 10px;
  border: 1px solid rgba(255,255,255,0.5);
  background: #E6E6EA;
  color: #475569;
  font-size: 12px;
  text-decoration: none;
  transition: all 0.2s ease;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50);
}
.ext-link:hover {
  box-shadow: -2px -1px 8px 0px #ffffff,
              2px 1px 8px 0px rgb(95 157 231 / 30%),
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.35) inset;
  color: #0f172a;
  text-decoration: none;
}

/* ── Alerts ───────────────────────────────────────────────────────────────── */
.glass-alert {
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  border: 1px solid rgba(255,255,255,0.38);
  border-radius: 10px;
  padding: 14px 18px;
  font-size: 13px;
  color: #475569;
  display: flex;
  align-items: center;
  gap: 10px;
}
.glass-alert-info { border-left: 3px solid #94a3b8; }
.glass-alert-success { border-left: 3px solid rgba(22,163,74,0.5); color: #15803d; }
.glass-alert-warning { border-left: 3px solid rgba(202,138,4,0.5); color: #a16207; }

/* ── Empty State ──────────────────────────────────────────────────────────── */
.empty-state { text-align: center; padding: 60px 20px; }
.empty-state-icon { font-size: 3rem; opacity: 0.2; color: #94a3b8; margin-bottom: 16px; display: block; }
.empty-state-text { font-size: 15px; color: #64748b; margin: 0; }

/* ── Search Hero ──────────────────────────────────────────────────────────── */
.search-hero { padding: 40px 0 30px; text-align: center; }
.search-card {
  display: inline-block;
  text-align: left;
  background: rgba(255,255,255,0.5);
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  border: 1px solid rgba(255,255,255,0.38);
  border-radius: 10px;
  padding: 32px 36px 24px;
  min-width: 520px;
  max-width: 640px;
  box-shadow: 0 8px 32px rgba(31,38,135,0.15);
}
.search-title {
  font-size: 20px;
  font-weight: 600;
  color: #0f172a;
  margin-bottom: 20px;
  text-align: center;
}
.search-row {
  display: flex;
  gap: 10px;
  align-items: flex-end;
  margin-bottom: 12px;
}
.search-field { flex: 1; }
.search-field-sm { width: 160px; }
.search-row .form-group { margin-bottom: 0; }
.search-row .shiny-input-container { width: 100% !important; }

/* ── Form Controls ────────────────────────────────────────────────────────── */
.form-control {
  background: #E6E6EA !important;
  border: 1px solid rgba(255,255,255,0.5) !important;
  color: #1e293b !important;
  border-radius: 10px !important;
  padding: 8px 12px;
  font-size: 14px;
  font-family: 'Inter', sans-serif;
  transition: all 0.25s ease;
  height: auto;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50);
}
.form-control:focus {
  background: #E6E6EA !important;
  border-color: rgba(95,157,231,0.4) !important;
  box-shadow: -2px -1px 8px 0px #ffffff,
              2px 1px 8px 0px rgb(95 157 231 / 30%),
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.35) inset !important;
  color: #1e293b !important;
  outline: none;
}
.form-control::placeholder { color: #94a3b8 !important; }

label:not(.checkbox label) {
  color: #64748b;
  font-weight: 600;
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin-bottom: 4px;
}
.shiny-input-container > label:empty { display: none; margin: 0; padding: 0; }

/* Selectize */
.selectize-input {
  background: #E6E6EA !important;
  border: 1px solid rgba(255,255,255,0.5) !important;
  color: #1e293b !important;
  border-radius: 10px !important;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50) !important;
  padding: 7px 12px !important;
  min-height: 38px;
}
.selectize-input.focus {
  border-color: rgba(95,157,231,0.4) !important;
  background: #E6E6EA !important;
  box-shadow: -2px -1px 8px 0px #ffffff,
              2px 1px 8px 0px rgb(95 157 231 / 30%),
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.35) inset !important;
}
.selectize-input .item { color: #1e293b !important; }
.selectize-dropdown {
  background: rgba(255,255,255,0.96) !important;
  backdrop-filter: blur(8px) !important;
  border: 1px solid rgba(255,255,255,0.38) !important;
  border-radius: 10px !important;
  color: #1e293b !important;
  margin-top: 4px;
  box-shadow: 0 8px 32px rgba(31,38,135,0.15);
}
.selectize-dropdown .option { padding: 8px 12px; }
.selectize-dropdown .active { background: #E6E6EA !important; color: #0f172a !important; }
.selectize-dropdown-content { max-height: 240px; }

/* Checkbox */
.checkbox label { color: #475569; font-size: 13px; }
.checkbox input[type='checkbox'] { margin-right: 6px; accent-color: #1e293b; }

/* Action button */
.btn-accent {
  background: #1e293b;
  color: white;
  border: none;
  border-radius: 10px;
  padding: 9px 24px;
  font-weight: 600;
  font-size: 14px;
  font-family: 'Inter', sans-serif;
  cursor: pointer;
  transition: all 0.15s ease;
  white-space: nowrap;
  box-shadow: 0 1px 3px rgba(0,0,0,0.1);
}
.btn-accent:hover, .btn-accent:focus {
  background: #334155;
  box-shadow: 0 2px 6px rgba(0,0,0,0.15);
  color: white;
}
.btn-glass-sm {
  display: inline-block;
  background: rgba(255,255,255,0.3);
  border: 1px solid rgba(255,255,255,0.8);
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  color: #475569;
  border-radius: 10px;
  padding: 6px 14px;
  font-size: 12px;
  font-family: 'Inter', sans-serif;
  cursor: pointer;
  transition: all 0.15s ease;
  text-decoration: none;
}
.btn-glass-sm:hover { background: rgba(255,255,255,0.5); color: #0f172a; text-decoration: none; }

/* ── Gene Header ──────────────────────────────────────────────────────────── */
.gene-header {
  padding-bottom: 16px;
  margin-bottom: 20px;
  border-bottom: 1px solid rgba(255,255,255,0.3);
}
.gene-header h2 { color: #0f172a; font-size: 24px; font-weight: 700; margin: 0 0 4px; }
.gene-header h4 { color: #64748b; font-size: 15px; font-weight: 400; margin: 0; }

/* ── Badges ───────────────────────────────────────────────────────────────── */
.badge-glass {
  display: inline-block;
  padding: 3px 10px;
  border-radius: 8px;
  font-size: 11px;
  font-weight: 600;
}
.badge-blue { background: rgba(30,41,59,0.08); color: #1e293b; border: 1px solid rgba(30,41,59,0.15); }
.badge-green { background: rgba(22,163,74,0.08); color: #15803d; border: 1px solid rgba(22,163,74,0.15); }
.badge-teal { background: rgba(8,145,178,0.08); color: #0e7490; border: 1px solid rgba(8,145,178,0.15); }
.badge-purple { background: rgba(124,58,237,0.08); color: #7c3aed; border: 1px solid rgba(124,58,237,0.12); }
.badge-orange { background: rgba(234,88,12,0.08); color: #c2410c; border: 1px solid rgba(234,88,12,0.12); }
.badge-grey { background: rgba(0,0,0,0.04); color: #64748b; border: 1px solid rgba(0,0,0,0.08); }

/* Alias chips */
.alias-chip {
  display: inline-block;
  padding: 3px 10px;
  margin: 2px;
  border-radius: 8px;
  font-size: 11px;
  color: #475569;
  background: #E6E6EA;
  border: 1px solid rgba(255,255,255,0.5);
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
	            1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50);
}

/* ── PanelApp / Domains visual rows ───────────────────────────────────────── */
.gene-visual-list {
  display: grid;
  gap: 8px;
}
.panelapp-row,
.domain-row {
  display: grid;
  grid-template-columns: minmax(180px, 1fr) minmax(220px, 1.6fr);
  align-items: center;
  gap: 16px;
  padding: 6px 0;
  border-bottom: 1px solid rgba(0,0,0,0.04);
}
.panelapp-row__name,
.domain-row__name {
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  color: #1e293b;
  font-size: 13px;
}
.panelapp-row__moi,
.domain-row__coords {
  color: #8aa0bb;
  font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
  font-size: 11px;
  text-align: right;
}
.panel-dot {
  width: 7px;
  height: 7px;
  display: inline-block;
  margin-right: 10px;
  border-radius: 50%;
}
.domain-track {
  position: relative;
  height: 8px;
  border-radius: 999px;
  background: rgba(148,163,184,0.10);
  overflow: hidden;
}
.domain-fill {
  position: absolute;
  top: 0;
  height: 100%;
  min-width: 8px;
  border-radius: 999px;
}
.domain-row__right {
  display: grid;
  grid-template-columns: minmax(120px, 1fr) 72px;
  align-items: center;
  gap: 10px;
}
.gene-visual-source {
  margin-top: 14px;
  padding-top: 10px;
  border-top: 1px solid rgba(0,0,0,0.05);
  color: #8aa0bb;
  font-size: 11px;
}

@media (max-width: 760px) {
  .panelapp-row,
  .domain-row {
    grid-template-columns: 1fr;
    gap: 6px;
  }
  .panelapp-row__moi,
  .domain-row__coords {
    text-align: left;
  }
}

/* ── Data Tables Light Theme ──────────────────────────────────────────────── */
.dataTables_wrapper { color: #475569; font-family: 'Inter', sans-serif; }
.dataTables_wrapper .dataTables_filter label,
.dataTables_wrapper .dataTables_length label { color: #64748b; font-size: 12px; }
.dataTables_wrapper .dataTables_info { color: #94a3b8; font-size: 12px; }

table.dataTable { background: transparent !important; color: #1e293b; border-collapse: collapse !important; width: 100% !important; }
table.dataTable thead th {
  background: rgba(0,0,0,0.02) !important;
  color: #64748b !important;
  border-bottom: 1px solid rgba(0,0,0,0.08) !important;
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  font-weight: 600;
  padding: 10px 12px !important;
}
table.dataTable thead th:first-child { border-radius: 8px 0 0 0; }
table.dataTable thead th:last-child { border-radius: 0 8px 0 0; }
table.dataTable tbody td {
  border: none !important;
  border-bottom: 1px solid rgba(0,0,0,0.04) !important;
  padding: 10px 12px !important;
  font-size: 13px;
}
table.dataTable tbody tr { background: transparent !important; transition: background 0.15s; }
table.dataTable tbody tr:hover { background: rgba(230,230,234,0.5) !important; }
table.dataTable tbody tr.selected { background: #E6E6EA !important; }
table.dataTable tbody tr.odd { background: rgba(0,0,0,0.01) !important; }

.dataTables_filter input, .dataTables_length select {
  background: #E6E6EA !important;
  border: 1px solid rgba(255,255,255,0.5) !important;
  color: #1e293b !important;
  border-radius: 10px !important;
  padding: 5px 10px;
  font-size: 13px;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50);
}
.dataTables_filter input:focus {
  border-color: rgba(95,157,231,0.4) !important;
  box-shadow: -2px -1px 8px 0px #ffffff,
              2px 1px 8px 0px rgb(95 157 231 / 30%),
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.35) inset !important;
  outline: none;
}
.dataTables_paginate .paginate_button {
  color: #64748b !important;
  background: rgba(255,255,255,0.3) !important;
  border: 1px solid rgba(255,255,255,0.8) !important;
  border-radius: 8px !important;
  padding: 4px 10px !important;
  margin: 0 2px;
  font-size: 12px;
}
.dataTables_paginate .paginate_button:hover {
  color: #0f172a !important;
  background: rgba(255,255,255,0.5) !important;
  border-color: rgba(255,255,255,0.8) !important;
}
.dataTables_paginate .paginate_button.current {
  background: #1e293b !important;
  color: #fff !important;
  border-color: #1e293b !important;
  font-weight: 600;
}
.dataTables_paginate .paginate_button.disabled { opacity: 0.3 !important; }
table.dataTable thead .sorting_asc::after { content: ' \\2191'; color: #475569; }
table.dataTable thead .sorting_desc::after { content: ' \\2193'; color: #475569; }
table.dataTable thead .sorting::after { content: ' \\2195'; color: #d1d5db; }
.dataTables_scrollBody { border: none !important; }

/* ── Tables (non-DT) ─────────────────────────────────────────────────────── */
.tbl-glass { width: 100%; border-collapse: collapse; }
.tbl-glass th {
  text-align: left;
  padding: 8px 12px;
  font-size: 12px;
  color: #64748b;
  font-weight: 600;
  border-bottom: 1px solid rgba(0,0,0,0.06);
  width: 160px;
  vertical-align: top;
}
.tbl-glass td {
  padding: 8px 12px;
  font-size: 13px;
  color: #1e293b;
  border-bottom: 1px solid rgba(0,0,0,0.04);
}

/* ── visNetwork ───────────────────────────────────────────────────────────── */
.vis-network { background: transparent !important; }
.vis-navigation .vis-button {
  background-color: rgba(255,255,255,0.5) !important;
  border: 1px solid rgba(255,255,255,0.38) !important;
  border-radius: 10px;
  backdrop-filter: blur(8px);
}

/* ── Shiny Notifications / Progress ───────────────────────────────────────── */
.shiny-notification {
  background: rgba(255,255,255,0.95) !important;
  backdrop-filter: blur(8px);
  border: 1px solid rgba(255,255,255,0.38);
  color: #1e293b;
  border-radius: 10px;
  box-shadow: 0 8px 32px rgba(31,38,135,0.15);
}
.progress { background: #E6E6EA; border-radius: 8px; height: 6px; }
.progress-bar { background: #1e293b; border-radius: 8px; }

/* ── Summary Block ────────────────────────────────────────────────────────── */
.summary-block {
  background: #E6E6EA;
  border-radius: 10px;
  padding: 14px 18px;
  font-size: 13px;
  line-height: 1.75;
  color: #475569;
  box-shadow: 1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50) inset,
              -1.26px -1.26px 7.549px 0 #FBFCFE,
              1.26px 1.26px 2.519px 0 rgba(88,102,132,0.50);
}

/* ── Monarch header ───────────────────────────────────────────────────────── */
.monarch-header {
  display: flex;
  align-items: center;
  gap: 14px;
  flex-wrap: wrap;
}
.monarch-badges { margin-left: auto; display: flex; gap: 6px; flex-wrap: wrap; }

/* ── Utility ──────────────────────────────────────────────────────────────── */
.text-accent { color: #475569; }
.text-muted-glass { color: #94a3b8; }
"

# =============================================================================
#  UI
# =============================================================================
geneExplorerUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(APP_CSS)),

    # App header
    tags$div(class = "app-header",
      tags$div(class = "app-header-inner",
        icon("dna", class = "header-icon"),
        tags$span("Gene Explorer", class = "header-title"),
        tags$span("UniProt \u00b7 Ensembl \u00b7 Monarch \u00b7 STRING", class = "header-sub")
      )
    ),

    # Main body
    tags$div(class = "app-body",
      tags$div(class = "seg-main",
        tabsetPanel(
          id = ns("main_nav"), type = "pills",

          # ── SEARCH ───────────────────────────────────────────────────────────
          tabPanel(
            title = tagList(icon("search"), " Search"),
            value = "Search",
            tags$div(class = "search-hero",
              tags$div(class = "search-card",
                tags$h3("Search for a gene", class = "search-title"),
                tags$div(class = "search-row",
                  tags$div(class = "search-field",
                    textInput(ns("q"), NULL, placeholder = "e.g. BRCA1, TP53, CFTR...")
                  ),
                  tags$div(class = "search-field-sm",
                    selectInput(ns("org"), NULL, c(
                      "Human" = "9606", "Mouse" = "10090", "Rat" = "10116",
                      "Zebrafish" = "7955", "Drosophila" = "7227", "C. elegans" = "6239"
                    ))
                  ),
                  actionButton(ns("go"), tagList(icon("search"), " Search"), class = "btn-accent")
                ),
                checkboxInput(ns("rev"), "Reviewed (Swiss-Prot) only", TRUE)
              )
            ),
            uiOutput(ns("search_ui"))
          ),

          # ── OVERVIEW ─────────────────────────────────────────────────────────
          tabPanel(
            title = tagList(icon("circle-info"), " Overview"),
            value = "Overview",
            uiOutput(ns("ui_overview"))
          ),

          # ── DETAILS ──────────────────────────────────────────────────────────
          tabPanel(
            title = tagList(icon("dna"), " Details"),
            value = "Details",
            uiOutput(ns("ui_details"))
          ),

          # ── CLINICAL ─────────────────────────────────────────────────────────
          tabPanel(
            title = tagList(icon("stethoscope"), " Clinical"),
            value = "Clinical",
            uiOutput(ns("ui_clinical"))
          ),

          # ── PATHWAYS & DOMAINS ────────────────────────────────────────────────
          tabPanel(
            title = tagList(icon("route"), " Pathways"),
            value = "Pathways",
            uiOutput(ns("ui_pathways"))
          ),

          # ── NETWORK ──────────────────────────────────────────────────────────
          tabPanel(
            title = tagList(icon("circle-nodes"), " Network"),
            value = "Network",
            uiOutput(ns("ui_network"))
          ),

          # ── LITERATURE ───────────────────────────────────────────────────────
          tabPanel(
            title = tagList(icon("book-open"), " Literature"),
            value = "Literature",
            uiOutput(ns("ui_literature"))
          )
        )
      )
    )
  )
}

# =============================================================================
#  SERVER
# =============================================================================
geneExplorerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  rv <- reactiveValues(
    results = NULL, entry = NULL, ens = NULL, acc = NULL,
    mg = NULL, pub = NULL, si = NULL, phen = NULL,
    monarch_id = NULL, monarch_raw = NULL,
    reactome = NULL, interpro = NULL, clinvar_facets = NULL,
    panelapp = NULL
  )

  # ── SEARCH ─────────────────────────────────────────────────────────────────
  observeEvent(input$go, {
    req(nchar(trimws(input$q)) > 0)
    for (nm in c("results", "entry", "ens", "acc", "mg", "pub", "si", "phen",
                 "monarch_id", "monarch_raw", "reactome", "interpro", "clinvar_facets",
                 "panelapp")) rv[[nm]] <- NULL

    g   <- trimws(input$q)
    org <- input$org

    withProgress(message = paste("Loading", g, "..."), value = 0, {

      incProgress(.10, detail = "UniProt search")
      raw <- tryCatch(api_uniprot_search(g, org)[["results"]], error = function(e) NULL)
      if (is.null(raw)) {
        rv$results <- list()
      } else {
        if (input$rev) raw <- Filter(function(r) identical(r$entryType, "UniProtKB reviewed (Swiss-Prot)"), raw)
        rv$results <- raw %||% list()
      }

      if (!is.null(rv$results) && length(rv$results) > 0) {
        acc <- rv$results[[1]]$primaryAccession
        incProgress(.08, detail = "UniProt entry")
        rv$entry <- api_uniprot_entry(acc)
        rv$acc   <- acc
      }

      incProgress(.08, detail = "Ensembl")
      rv$ens <- api_ensembl_lookup(g, taxid_sp(org))

      ens_id <- tryCatch(rv$ens[["id"]], error = function(e) NULL)
      if (!is.null(ens_id) && org == "9606") {
        incProgress(.07, detail = "Ensembl phenotypes")
        rv$phen <- api_ensembl_phenotypes(ens_id)
      }

      mg_sp <- c("9606" = "human", "10090" = "mouse", "10116" = "rat",
                  "7955" = "zebrafish", "7227" = "fruitfly", "6239" = "nematode")[org] %||% "human"
      incProgress(.10, detail = "MyGene.info")
      rv$mg <- api_mygene(g, mg_sp)

      incProgress(.10, detail = "PubMed")
      rv$pub <- api_pubmed(g, 20)

      incProgress(.10, detail = "STRING-db")
      rv$si <- api_string(g, as.integer(org))

      if (org == "9606") {
        incProgress(.06, detail = "Reactome pathways")
        rv$reactome <- api_reactome_pathways(g)

        incProgress(.05, detail = "ClinVar facets")
        rv$clinvar_facets <- api_myvariant_clinvar(g)

        incProgress(.05, detail = "PanelApp")
        rv$panelapp <- api_panelapp(g)
      }

      if (!is.null(rv$acc) && nzchar(rv$acc)) {
        incProgress(.05, detail = "InterPro domains")
        rv$interpro <- api_interpro_domains(rv$acc)
      }

      # Monarch
      incProgress(.08, detail = "Monarch search")
      m_res   <- api_monarch_search(g)
      m_items <- tryCatch(m_res[["items"]] %||% list(), error = function(e) list())
      if (length(m_items) > 0) {
        hit <- tryCatch({
          hn <- Filter(function(i) grepl("NCBIGene|HGNC", m_str(i[["id"]])), m_items)
          if (length(hn) > 0) hn[[1]] else m_items[[1]]
        }, error = function(e) m_items[[1]])
        rv$monarch_id <- tryCatch(m_str(hit[["id"]]), error = function(e) NULL)
      }
      if (!is.null(rv$monarch_id) && nzchar(rv$monarch_id)) {
        incProgress(.09, detail = "Monarch phenotypes")
        rv$monarch_raw <- api_monarch_pheno(rv$monarch_id, limit = 200)
      }

      incProgress(.10)
    })

    if (!is.null(rv$entry)) {
      showNotification(paste("Data loaded for", g), type = "message", duration = 3)
      updateTabsetPanel(session, "main_nav", selected = "Overview")
    } else {
      showNotification(
        paste("No UniProt entry loaded for", g, "- check the symbol, organism, or Reviewed-only filter."),
        type = "warning",
        duration = 6
      )
      updateTabsetPanel(session, "main_nav", selected = "Search")
    }
  })

  # ── SEARCH RESULTS UI ─────────────────────────────────────────────────────
  output$search_ui <- renderUI({
    if (is.null(rv$results))
      return(glass_card(empty_state("Enter a gene name and click Search.")))
    if (length(rv$results) == 0)
      return(glass_card(glass_alert("No results found.", "warning", "triangle-exclamation")))
    tagList(
      glass_alert(paste(length(rv$results), "result(s). Click a row to load all data."), "success", "circle-check"),
      tags$div(style = "margin-top:14px", DTOutput(ns("tbl_search")))
    )
  })

  output$tbl_search <- renderDT({
    req(rv$results, length(rv$results) > 0)
    df <- safe_df(rv$results, function(r) {
      data.frame(
        Accession = r$primaryAccession %||% "-",
        Gene = safe(r$genes[[1]]$geneName$value),
        Protein = safe(r$proteinDescription$recommendedName$fullName$value,
                       safe(r$proteinDescription$submissionNames[[1]]$fullName$value)),
        Length = safe(as.character(r$sequence$length)),
        Status = if (identical(r$entryType, "UniProtKB reviewed (Swiss-Prot)")) "Reviewed" else "Unreviewed",
        stringsAsFactors = FALSE
      )
    })
    datatable(df, selection = "single", rownames = FALSE,
              options = list(pageLength = 10, dom = "ftp", scrollX = TRUE,
                             language = list(search = "Filter:")))
  })

  observeEvent(input$tbl_search_rows_selected, {
    idx <- input$tbl_search_rows_selected; req(idx)
    acc <- rv$results[[idx]]$primaryAccession
    withProgress(message = paste("Loading", acc, "..."), value = 0.1, {
      e <- api_uniprot_entry(acc)
      if (!is.null(e)) {
        rv$entry <- e; rv$acc <- acc
        incProgress(.4, detail = "Ensembl")
        rv$ens <- api_ensembl_lookup(get_gene(e), taxid_sp(safe(as.character(e$organism$taxonId), "9606")))
        incProgress(.5)
        updateTabsetPanel(session, "main_nav", selected = "Overview")
      }
    })
  })

  # ══════════════════════════════════════════════════════════════════════════
  #  OVERVIEW
  # ══════════════════════════════════════════════════════════════════════════
  output$ui_overview <- renderUI({
    if (is.null(rv$entry)) return(glass_card(empty_state("Search for a gene first.")))
    e    <- rv$entry
    mg   <- rv$mg
    gene <- get_gene(e)

    entrez  <- scalar(tryCatch(mg[["entrezgene"]], error = function(e) NULL), "\u2014")
    ens_g   <- scalar(tryCatch(mg[["ensembl"]][["gene"]], error = function(e) NULL),
                      safe(rv$ens[["id"]], "\u2014"))
    chr_lbl <- scalar(tryCatch({
      gp <- mg[["genomic_pos"]]
      if (!is.null(names(gp)) && "chr" %in% names(gp)) gp[["chr"]] else gp[[1]][["chr"]]
    }, error = function(e) NULL), "\u2014")
    biotype <- scalar(tryCatch(mg[["type_of_gene"]] %||% mg[["biotype"]], error = function(e) NULL), "\u2014")
    hgnc    <- scalar(tryCatch(mg[["HGNC"]], error = function(e) NULL), "\u2014")
    mim     <- scalar(tryCatch(mg[["MIM"]], error = function(e) NULL), "\u2014")
    up_id   <- e$primaryAccession %||% "\u2014"

    summ <- tryCatch(as.character(mg[["summary"]]), error = function(e) NULL) %||%
      tryCatch(paste(get_cc_type(e, "FUNCTION")[[1]]$texts[[1]]$value), error = function(e) NULL) %||%
      "No functional summary available."

    aliases <- tryCatch({
      a <- mg[["alias"]]
      if (is.null(a) || length(a) == 0) character(0) else as.character(unlist(a))
    }, error = function(e) character(0))

    tagList(
      # Gene header
      tags$div(class = "gene-header",
        tags$h2(paste0(gene, " \u2014 ", safe(e$proteinDescription$recommendedName$fullName$value))),
        tags$h4(up_id),
        tags$div(style = "margin-top:8px; display:flex; gap:6px; flex-wrap:wrap;",
          tags$span(class = "badge-glass badge-blue", up_id),
          tags$span(class = "badge-glass badge-teal", paste(e$sequence$length, "aa")),
          if (identical(e$entryType, "UniProtKB reviewed (Swiss-Prot)"))
            tags$span(class = "badge-glass badge-green", "Reviewed")
        )
      ),

      # ID cards grid
      tags$div(class = "kv-grid",
        kv_card("Symbol", gene), kv_card("Entrez ID", entrez),
        kv_card("Ensembl", ens_g), kv_card("UniProt", up_id),
        kv_card("Chromosome", chr_lbl), kv_card("Biotype", biotype),
        kv_card("HGNC", hgnc), kv_card("OMIM", mim)
      ),

      # Functional summary
      glass_card(
        header = tags$strong("Functional Summary"),
        tags$div(class = "summary-block", summ),

        if (length(aliases) > 0) tagList(
          section_label("Aliases & Synonyms"),
          tags$div(lapply(head(aliases, 25), function(a) tags$span(class = "alias-chip", a)))
        ),

        section_label("External Links"),
        tags$div(
          if (entrez != "\u2014") ext_link("NCBI Gene", paste0("https://www.ncbi.nlm.nih.gov/gene/", entrez)),
          if (ens_g != "\u2014")  ext_link("Ensembl", paste0("https://www.ensembl.org/id/", ens_g)),
          ext_link("UniProt", paste0("https://www.uniprot.org/uniprot/", up_id)),
          if (mim != "\u2014")    ext_link("OMIM", paste0("https://www.omim.org/entry/", mim)),
          ext_link("GeneCards", paste0("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", gene)),
          if (ens_g != "\u2014")  ext_link("Open Targets", paste0("https://platform.opentargets.org/target/", ens_g)),
          ext_link("GTEx", paste0("https://gtexportal.org/home/gene/", gene)),
          ext_link("COSMIC", paste0("https://cancer.sanger.ac.uk/cosmic/gene/analysis?ln=", gene))
        )
      )
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  #  DETAILS — Names, Location, Keywords, Expression
  # ══════════════════════════════════════════════════════════════════════════
  output$ui_details <- renderUI({
    if (is.null(rv$entry)) return(glass_card(empty_state("Search for a gene first.")))
    e <- rv$entry

    # Names & Taxonomy
    organism  <- safe(e$organism$scientificName)
    taxid     <- safe(as.character(e$organism$taxonId))
    lineage   <- safe(paste(e$organism$lineage, collapse = " > "))
    alt_names <- tryCatch(sapply(e$proteinDescription$alternativeNames, function(n) n$fullName$value),
                          error = function(err) NULL)
    synonyms  <- tryCatch(sapply(e$genes[[1]]$synonyms, function(s) s$value), error = function(err) NULL)

    names_rows <- tagList(tags$tr(tags$th("Protein name"), tags$td(get_name(e))))
    if (!is.null(alt_names) && length(alt_names) > 0)
      names_rows <- tagList(names_rows, tags$tr(tags$th("Alternative names"), tags$td(paste(alt_names, collapse = "; "))))
    names_rows <- tagList(names_rows, tags$tr(tags$th("Gene"), tags$td(tags$strong(get_gene(e)))))
    if (!is.null(synonyms) && length(synonyms) > 0)
      names_rows <- tagList(names_rows, tags$tr(tags$th("Synonyms"), tags$td(paste(synonyms, collapse = ", "))))
    names_rows <- tagList(names_rows,
      tags$tr(tags$th("Organism"), tags$td(tags$em(organism))),
      tags$tr(tags$th("Taxonomic ID"), tags$td(tags$a(taxid,
        href = paste0("https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=", taxid), target = "_blank"))),
      tags$tr(tags$th("Lineage"), tags$td(tags$small(style = "color:#8a9bb5", lineage)))
    )

    # Genomic Location
    ens  <- rv$ens
    gene <- get_gene(e)
    loc_content <- if (!is.null(ens) && !is.null(ens$seq_region_name)) {
      chr    <- ens$seq_region_name
      gs     <- ens$start; ge <- ens$end
      strand <- ens$strand %||% 1
      bt     <- ens$biotype %||% ""
      svg_code <- make_chr_svg(chr, gs, ge, gene, strand)
      tagList(
        tags$table(class = "tbl-glass", tags$tbody(
          tags$tr(tags$th("Chromosome"), tags$td(tags$strong(chr))),
          tags$tr(tags$th("Start"), tags$td(fmt(gs))),
          tags$tr(tags$th("End"), tags$td(fmt(ge))),
          tags$tr(tags$th("Strand"), tags$td(if (strand >= 0) "Forward (+)" else "Reverse (-)")),
          tags$tr(tags$th("Size"), tags$td(paste(fmt(ge - gs), "bp"))),
          if (nchar(bt) > 0) tags$tr(tags$th("Biotype"), tags$td(bt))
        )),
        if (!is.null(svg_code)) tags$div(style = "margin:16px 0; padding:12px;
          background:rgba(255,255,255,0.4); border:1px solid rgba(0,0,0,0.05);
          border-radius:12px;", HTML(svg_code)),
        tags$div(style = "display:flex; gap:8px; margin-top:12px;",
          tags$a("View on Ensembl",
            href = paste0("https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", ens$id %||% gene),
            target = "_blank", class = "btn-glass-sm"),
          tags$a("View on UCSC",
            href = paste0("https://genome.ucsc.edu/cgi-bin/hgTracks?db=hg38&position=chr", chr, ":", gs, "-", ge),
            target = "_blank", class = "btn-glass-sm")
        )
      )
    } else {
      glass_alert("Could not retrieve genomic coordinates from Ensembl.", "warning")
    }

    # Keywords
    kw <- e$keywords
    kw_content <- if (!is.null(kw) && length(kw) > 0) {
      cats <- unique(sapply(kw, function(k) k$category %||% "Other"))
      tagList(lapply(cats, function(cat) {
        items <- Filter(function(k) identical(k$category %||% "Other", cat), kw)
        nms <- sapply(items, function(k) k$name %||% "-")
        tagList(
          tags$strong(cat, style = "display:block; margin:10px 0 6px; color:#2d6da3; font-size:12px;"),
          tags$div(lapply(nms, function(n) tags$span(class = "alias-chip", n)))
        )
      }))
    } else {
      tags$p("No keywords.", class = "text-muted-glass")
    }

    # Expression
    cc_expr <- get_cc_type(e, "TISSUE SPECIFICITY")
    expr_content <- if (length(cc_expr) > 0) {
      tagList(lapply(cc_expr, function(c) {
        txts <- tryCatch(sapply(c$texts, function(t) t$value), error = function(e) NULL)
        if (!is.null(txts) && length(txts) > 0) tags$p(style = "color:#3d4f63; line-height:1.7;",
                                                         paste(txts, collapse = " "))
      }))
    } else {
      tags$p("No expression data.", class = "text-muted-glass")
    }

    tagList(
      glass_card(header = tags$strong("Names & Taxonomy"),
        tags$table(class = "tbl-glass", tags$tbody(names_rows))
      ),
      glass_card(header = tags$strong("Genomic Location"), loc_content),
      glass_card(header = tags$strong("Keywords"), kw_content),
      glass_card(header = tags$strong("Expression / Tissue Specificity"), expr_content)
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  #  CLINICAL — Disease, Ensembl Phenotypes, Monarch
  # ══════════════════════════════════════════════════════════════════════════
  output$ui_clinical <- renderUI({
    if (is.null(rv$entry)) return(glass_card(empty_state("Search for a gene first.", "stethoscope")))
    e    <- rv$entry
    gene <- get_gene(e)

    # Disease & Variants
    dc   <- get_cc_type(e, "DISEASE")
    vars <- if (!is.null(e$features)) Filter(function(f) identical(f$type, "Natural variant"), e$features) else list()

    disease_ui <- tagList(
      if (length(dc) > 0) {
        tagList(lapply(dc, function(d) {
          nm   <- safe(d$disease$diseaseId, "Unknown")
          acr  <- safe(d$disease$acronym, "")
          desc <- safe(d$disease$description, "")
          note <- tryCatch(paste(sapply(d$texts, function(t) t$value), collapse = " "), error = function(err) "")
          tagList(
            tags$div(style = "margin-bottom:16px;",
              tags$h5(style = "color:#1e2a3a; margin:0 0 4px;", nm,
                if (nchar(acr) > 0) tags$small(paste0(" (", acr, ")"), style = "color:#8a9bb5")),
              if (nchar(note) > 0) tags$p(style = "font-size:13px; color:#4a5e75; margin:4px 0;", note),
              if (nchar(desc) > 0) tags$p(style = "font-size:12px; color:#6b7f96; margin:4px 0;", desc)
            ),
            tags$hr(style = "border-color:rgba(0,0,0,0.05); margin:12px 0;")
          )
        }))
      } else {
        tags$p("No disease associations.", class = "text-muted-glass")
      },
      if (length(vars) > 0) tagList(
        tags$h5(style = "color:#1e2a3a; margin:20px 0 10px;",
                paste0("Natural Variants (", length(vars), ")")),
        DTOutput(ns("tbl_vars"))
      )
    )

    # Ensembl Phenotypes
    phen <- rv$phen
    ens_phen_ui <- if (is.null(phen) || length(phen) == 0) {
      glass_alert("No Ensembl phenotype data (human only).", "warning")
    } else {
      df <- tryCatch(unique(do.call(rbind, lapply(phen, function(p) {
        data.frame(Phenotype = scalar(p[["description"]], "-"),
                   Source = scalar(p[["source"]], "-"),
                   Chr = scalar(p[["seq_region_name"]], "-"), stringsAsFactors = FALSE)
      }))), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) {
        glass_alert("No phenotype data found.", "warning")
      } else {
        tagList(
          glass_alert(paste(nrow(df), "Ensembl phenotype entries for", gene), "info", "circle-info"),
          tags$div(style = "margin-top:12px;", DTOutput(ns("dt_phen")))
        )
      }
    }

    # Monarch Phenotypes
    mid <- rv$monarch_id
    raw <- rv$monarch_raw
    monarch_ui <- if (is.null(mid) || !nzchar(mid)) {
      glass_alert("Monarch Initiative \u2014 gene ID not found. Requires a human HGNC or NCBIGene identifier.", "warning", "triangle-exclamation")
    } else {
      n_total      <- tryCatch(length(raw[["items"]] %||% raw[["associations"]] %||% list()), error = function(e) 0)
      df           <- build_monarch_df(raw)
      n_rows       <- if (!is.null(df)) nrow(df) else 0
      n_with_freq  <- if (!is.null(df)) sum(nzchar(df$Frequency)) else 0
      n_with_onset <- if (!is.null(df)) sum(nzchar(df$Onset)) else 0

      tagList(
        glass_card(
          tags$div(class = "monarch-header",
            icon("crown", style = "font-size:22px; color:#7c3aed;"),
            tags$div(
              tags$div(style = "font-size:15px; font-weight:700; color:#1e2a3a;",
                paste(gene, "\u2014 Monarch Initiative")),
              tags$div(style = "font-size:12px; color:#6b7f96; margin-top:2px;",
                "Monarch ID: ", tags$code(style = "color:#2d6da3; font-size:11px;", mid), " \u00b7 ",
                tags$a("View on Monarch", href = paste0("https://monarchinitiative.org/", mid),
                       target = "_blank", style = "color:#7c3aed;"))
            ),
            tags$div(class = "monarch-badges",
              tags$span(class = "badge-glass badge-grey", paste(n_rows, "phenotypes")),
              tags$span(class = "badge-glass badge-green", paste(n_with_freq, "with frequency")),
              tags$span(class = "badge-glass badge-teal", paste(n_with_onset, "with onset"))
            )
          )
        ),
        if (n_rows > 0) {
          tagList(
            tags$p(style = "font-size:11px; color:#6b7f96; margin-bottom:10px;",
              icon("circle-info", style = "margin-right:4px;"),
              sprintf("%d/%d with frequency \u00b7 %d/%d with onset \u00b7 Source: OMIM / Orphanet HPO",
                      n_with_freq, n_rows, n_with_onset, n_rows)),
            DTOutput(ns("dt_monarch"))
          )
        } else {
          glass_alert("No phenotype associations returned from Monarch for this gene.", "warning", "triangle-exclamation")
        }
      )
    }

    panel_df <- panelapp_df(rv$panelapp)
    panelapp_ui <- if (is.null(panel_df) || nrow(panel_df) == 0) {
      glass_alert("PanelApp data unavailable for this gene.", "warning", "triangle-exclamation")
    } else {
      tagList(
        panelapp_visual(panel_df),
        tags$div(style = "margin-top:16px;", DTOutput(ns("dt_panelapp")))
      )
    }

    clinvar_df <- clinvar_sig_df(rv$clinvar_facets)
    clinvar_ui <- if (is.null(clinvar_df) || nrow(clinvar_df) == 0) {
      glass_alert("ClinVar classification summary unavailable from MyVariant.info.", "warning", "triangle-exclamation")
    } else {
      total_variants <- tryCatch(as.integer(rv$clinvar_facets[["total"]] %||% sum(clinvar_df$Count)), error = function(e) sum(clinvar_df$Count))
      tagList(
        glass_alert(paste(format(total_variants, big.mark = ","), "ClinVar-indexed variants found for", gene), "info", "circle-info"),
        tags$p(style = "font-size:12px; color:#6b7f96; margin:12px 0 10px;",
          "Counts summarize the most frequent clinical significance labels returned by MyVariant.info / ClinVar."
        ),
        DTOutput(ns("dt_clinvar_summary")),
        tags$div(style = "margin-top:12px;",
          ext_link("Open ClinVar search", paste0("https://www.ncbi.nlm.nih.gov/clinvar/?term=", gene, "[gene]"))
        )
      )
    }

    # Assemble with sub-segmented control
    tags$div(class = "seg-sub",
      tabsetPanel(
        id = ns("clinical_nav"), type = "pills",
        tabPanel(
          title = tagList(icon("virus"), " Disease & Variants"),
          value = "disease",
          tags$div(style = "padding-top:4px;", disease_ui)
        ),
        tabPanel(
          title = tagList(icon("microscope"), " Ensembl Phenotypes"),
          value = "ensembl_phen",
          tags$div(style = "padding-top:4px;", ens_phen_ui)
        ),
        tabPanel(
          title = tagList(icon("crown"), " Monarch Phenotypes"),
          value = "monarch_phen",
          tags$div(style = "padding-top:4px;", monarch_ui)
        ),
        tabPanel(
          title = tagList(icon("list-check"), " Gene Panels"),
          value = "gene_panels",
          tags$div(style = "padding-top:4px;", panelapp_ui)
        ),
        tabPanel(
          title = tagList(icon("triangle-exclamation"), " ClinVar Summary"),
          value = "clinvar_summary",
          tags$div(style = "padding-top:4px;", clinvar_ui)
        )
      )
    )
  })

  # Variants table
  output$tbl_vars <- renderDT({
    req(rv$entry)
    vars <- Filter(function(f) identical(f$type, "Natural variant"), rv$entry$features %||% list())
    req(length(vars) > 0)
    df <- safe_df(vars, function(v) {
      data.frame(
        Position = safe({
          s <- v$location$start$value; e <- v$location$end$value
          if (!is.null(e) && !identical(e, s)) paste0(s, "-", e) else as.character(s)
        }),
        Change = safe({
          o <- v$alternativeSequence$originalSequence %||% ""
          a <- paste(v$alternativeSequence$alternativeSequences, collapse = "/")
          if (nchar(o) > 0 && nchar(a) > 0) paste0(o, " > ", a) else "-"
        }),
        Description = v$description %||% "-",
        ID = v$featureId %||% "-", stringsAsFactors = FALSE
      )
    })
    datatable(df, rownames = FALSE, options = DT_OPTS)
  })

  # Ensembl phenotypes table
  output$dt_phen <- renderDT({
    req(rv$phen, length(rv$phen) > 0)
    df <- tryCatch(unique(do.call(rbind, lapply(rv$phen, function(p) {
      data.frame(Phenotype = scalar(p[["description"]], "-"),
                 Source = scalar(p[["source"]], "-"),
                 Chr = scalar(p[["seq_region_name"]], "-"), stringsAsFactors = FALSE)
    }))), error = function(e) NULL)
    req(!is.null(df) && nrow(df) > 0)
    datatable(df, rownames = FALSE, options = DT_OPTS)
  })

  # Monarch phenotype table
  output$dt_monarch <- renderDT({
    raw <- rv$monarch_raw; req(!is.null(raw))
    df <- build_monarch_df(raw)
    req(!is.null(df) && nrow(df) > 0)
    df$Frequency <- sapply(df$Frequency, freq_badge)
    df$HP_ID <- sapply(df$HP_ID, function(id) {
      if (!nzchar(id)) return("")
      url <- paste0("https://hpo.jax.org/app/browse/term/", id)
      paste0('<a href="', url, '" target="_blank" style="font-size:11px;color:#2d6da3">', htmltools::htmlEscape(id), '</a>')
    })
    datatable(df, escape = FALSE, rownames = FALSE,
      colnames = c("Gene", "Association", "Phenotype", "HP ID", "Frequency", "Onset", "Source"),
      options = c(DT_OPTS, list(
        columnDefs = list(
          list(width = "20%", targets = 2),
          list(width = "8%", targets = 3),
          list(width = "11%", targets = 4),
          list(width = "9%", targets = 5),
          list(className = "dt-center", targets = c(3, 4, 5))
        )
      ))
    )
  })

  output$dt_clinvar_summary <- renderDT({
    df <- clinvar_sig_df(rv$clinvar_facets)
    req(!is.null(df) && nrow(df) > 0)
    df$Percent <- paste0(round(df$Count / sum(df$Count) * 100, 1), "%")
    datatable(df, rownames = FALSE, options = DT_OPTS)
  })

  output$dt_panelapp <- renderDT({
    df <- panelapp_df(rv$panelapp)
    req(!is.null(df) && nrow(df) > 0)
    datatable(df, rownames = FALSE, options = DT_OPTS)
  })

  strip_html <- function(x) {
    x <- scalar(x, "")
    x <- gsub("<[^>]+>", "", x)
    trimws(gsub("\\s+", " ", x))
  }

  reactome_df <- function(entries) {
    if (is.null(entries) || length(entries) == 0) return(NULL)
    safe_df(entries, function(p) {
      data.frame(
        Pathway = strip_html(p[["name"]]),
        Reactome_ID = scalar(p[["stId"]] %||% p[["id"]], "-"),
        Type = scalar(p[["type"]], "-"),
        Disease = if (isTRUE(p[["isDisease"]] %||% p[["disease"]])) "Yes" else "No",
        Compartment = paste(unlist(p[["compartmentNames"]] %||% ""), collapse = ", "),
        stringsAsFactors = FALSE
      )
    })
  }

  interpro_df <- function(raw) {
    items <- raw[["results"]]
    if (is.null(items) || length(items) == 0) return(NULL)
    safe_df(items, function(item) {
      m <- item[["metadata"]]
      integrated <- m[["integrated"]]
      data.frame(
        Accession = scalar(m[["accession"]], "-"),
        Name = scalar(m[["name"]], "-"),
        Source = scalar(m[["source_database"]], "-"),
        Type = scalar(m[["type"]], "-"),
        InterPro = scalar(integrated[["accession"]], ""),
        stringsAsFactors = FALSE
      )
    })
  }

  interpro_domain_rows <- function(raw) {
    items <- raw[["results"]]
    if (is.null(items) || length(items) == 0) return(NULL)
    safe_df(items, function(item) {
      m <- item[["metadata"]]
      proteins <- item[["proteins"]]
      if (is.null(proteins) || length(proteins) == 0) return(NULL)
      prot <- proteins[[1]]
      protein_length <- as.numeric(prot[["protein_length"]] %||% NA)
      locs <- prot[["entry_protein_locations"]]
      if (is.null(locs) || length(locs) == 0) return(NULL)
      frags <- locs[[1]][["fragments"]]
      if (is.null(frags) || length(frags) == 0) return(NULL)
      frag <- frags[[1]]
      start <- as.numeric(frag[["start"]] %||% NA)
      end <- as.numeric(frag[["end"]] %||% NA)
      if (!is.finite(start) || !is.finite(end) || !is.finite(protein_length)) return(NULL)
      data.frame(
        Name = scalar(m[["name"]], "-"),
        Accession = scalar(m[["accession"]], "-"),
        Source = scalar(m[["source_database"]], "-"),
        Start = start,
        End = end,
        ProteinLength = protein_length,
        stringsAsFactors = FALSE
      )
    })
  }

  panelapp_df <- function(raw) {
    rows <- raw[["results"]]
    if (is.null(rows) || length(rows) == 0) return(NULL)
    safe_df(rows, function(r) {
      panel <- r[["panel"]]
      data.frame(
        Panel = scalar(panel[["name"]], "-"),
        Confidence = scalar(r[["confidence_level"]], "-"),
        Mode = scalar(r[["mode_of_inheritance"]], ""),
        Penetrance = scalar(r[["penetrance"]], ""),
        DiseaseGroup = scalar(panel[["disease_group"]], ""),
        PanelID = scalar(panel[["id"]], ""),
        stringsAsFactors = FALSE
      )
    })
  }

  panelapp_visual <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    tagList(
      tags$p(style = "font-size:11px; color:#8aa0bb; margin-bottom:10px;",
        icon("circle-info", style = "margin-right:4px;"),
        "Green = high, Amber = moderate, Red = low."
      ),
      tags$div(
        class = "gene-visual-list",
        lapply(seq_len(min(nrow(df), 12)), function(i) {
          conf <- df$Confidence[[i]]
          col <- if (identical(conf, "3")) "#22a06b" else if (identical(conf, "2")) "#c98a08" else "#cc3d4a"
          tags$div(
            class = "panelapp-row",
            tags$div(
              class = "panelapp-row__name",
              tags$span(class = "panel-dot", style = paste0("background:", col, ";")),
              df$Panel[[i]]
            ),
            tags$div(class = "panelapp-row__moi", df$Mode[[i]])
          )
        })
      ),
      tags$div(class = "gene-visual-source", "From ", tags$code("PanelApp"))
    )
  }

  domain_visual <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    palette <- c("#2f76dd", "#1495b7", "#22a06b", "#c98a08", "#cc3d4a", "#57799d")
    tagList(
      tags$p(style = "font-size:11px; color:#8aa0bb; margin-bottom:10px;",
        icon("circle-info", style = "margin-right:4px;"),
        paste0("Domains (", max(df$ProteinLength, na.rm = TRUE), " aa).")
      ),
      tags$div(
        class = "gene-visual-list",
        lapply(seq_len(min(nrow(df), 12)), function(i) {
          len <- df$ProteinLength[[i]]
          left <- max(0, min(100, df$Start[[i]] / len * 100))
          width <- max(1, min(100 - left, (df$End[[i]] - df$Start[[i]] + 1) / len * 100))
          col <- palette[((i - 1) %% length(palette)) + 1]
          tags$div(
            class = "domain-row",
            tags$div(class = "domain-row__name", df$Name[[i]]),
            tags$div(
              class = "domain-row__right",
              tags$div(
                class = "domain-track",
                tags$span(
                  class = "domain-fill",
                  style = sprintf("left:%.3f%%; width:%.3f%%; background:%s;", left, width, col)
                )
              ),
              tags$div(class = "domain-row__coords", paste0(df$Start[[i]], "-", df$End[[i]]))
            )
          )
        })
      ),
      tags$div(class = "gene-visual-source", "From ", tags$code("InterPro"))
    )
  }

  clinvar_sig_df <- function(raw) {
    if (is.null(raw)) return(NULL)

    terms <- tryCatch(raw[["facets"]][["clinvar.clinical_significance"]][["terms"]], error = function(e) NULL)
    if (!is.null(terms) && length(terms) > 0) {
      df <- safe_df(terms, function(t) {
        data.frame(
          Classification = scalar(t[["term"]], "-"),
          Count = as.integer(t[["count"]] %||% 0),
          stringsAsFactors = FALSE
        )
      })
      if (!is.null(df) && nrow(df) > 0 && sum(df$Count, na.rm = TRUE) > 0) return(df)
    }

    hits <- raw[["hits"]]
    if (is.null(hits) || length(hits) == 0) return(NULL)
    sigs <- unlist(lapply(hits, function(h) {
      rcv <- tryCatch(h[["clinvar"]][["rcv"]], error = function(e) NULL)
      if (is.null(rcv)) return(NULL)
      if (is.list(rcv) && !is.null(rcv[["clinical_significance"]])) {
        return(rcv[["clinical_significance"]])
      }
      unlist(lapply(rcv, function(r) r[["clinical_significance"]] %||% NULL))
    }), use.names = FALSE)
    sigs <- sigs[nzchar(sigs)]
    if (length(sigs) == 0) return(NULL)
    tab <- sort(table(sigs), decreasing = TRUE)
    data.frame(
      Classification = names(tab),
      Count = as.integer(tab),
      stringsAsFactors = FALSE
    )
  }

  # ══════════════════════════════════════════════════════════════════════════
  #  PATHWAYS — Reactome, InterPro, AlphaFold
  # ══════════════════════════════════════════════════════════════════════════
  output$ui_pathways <- renderUI({
    if (is.null(rv$entry)) return(glass_card(empty_state("Search for a gene first.", "route")))
    gene <- get_gene(rv$entry)
    up_id <- rv$acc %||% rv$entry$primaryAccession
    pathways <- reactome_df(rv$reactome)
    domains <- interpro_df(rv$interpro)
    domain_rows <- interpro_domain_rows(rv$interpro)

    pathway_content <- if (is.null(pathways) || nrow(pathways) == 0) {
      glass_alert("Reactome pathways unavailable for this gene.", "warning", "triangle-exclamation")
    } else {
      first_id <- pathways$Reactome_ID[[1]]
      first_name <- pathways$Pathway[[1]]
      tagList(
        tags$p(style = "font-size:12px; color:#6b7f96; margin-bottom:10px;",
          "Pathway list and the first available pathway diagram from Reactome."
        ),
        if (nzchar(first_id) && first_id != "-") {
          tags$div(style = "margin-bottom:14px;",
            tags$div(style = "font-size:12px; color:#6b7f96; margin-bottom:6px;", first_name),
            tags$img(
              src = paste0("https://reactome.org/ContentService/exporter/diagram/", first_id, ".png?quality=7&diagramProfile=Modern"),
              alt = paste("Reactome pathway", first_id),
              style = "width:100%; max-height:360px; object-fit:contain; border:1px solid rgba(0,0,0,0.08); border-radius:12px; background:#fff;"
            )
          )
        },
        DTOutput(ns("dt_reactome"))
      )
    }

    domain_content <- if (is.null(domains) || nrow(domains) == 0) {
      glass_alert("InterPro domains unavailable for this protein.", "warning", "triangle-exclamation")
    } else {
      tagList(
        tags$p(style = "font-size:12px; color:#6b7f96; margin-bottom:10px;",
          paste("Protein domain and family annotations for", up_id, "from InterPro.")
        ),
        domain_visual(domain_rows),
        tags$div(style = "margin-top:16px;"),
        DTOutput(ns("dt_interpro"))
      )
    }

    structure_content <- if (is.null(up_id) || !nzchar(up_id)) {
      glass_alert("AlphaFold structure unavailable: no UniProt accession.", "warning", "triangle-exclamation")
    } else {
      tagList(
        tags$p(style = "font-size:12px; color:#6b7f96; margin-bottom:10px;",
          "Predicted aligned error image. Darker blue regions indicate higher confidence."
        ),
        tags$img(
          src = paste0("https://alphafold.ebi.ac.uk/files/AF-", up_id, "-F1-predicted_aligned_error_v4.png"),
          alt = paste("AlphaFold PAE", up_id),
          style = "width:100%; max-height:420px; object-fit:contain; border:1px solid rgba(0,0,0,0.08); border-radius:12px; background:#fff;"
        ),
        tags$div(style = "margin-top:10px;",
          ext_link("Open AlphaFold", paste0("https://alphafold.ebi.ac.uk/entry/", up_id))
        )
      )
    }

    tagList(
      tags$div(class = "gene-header",
        tags$h2(paste0(gene, " — Pathways & Domains")),
        tags$h4("Reactome · InterPro · AlphaFold")
      ),
      glass_card(header = tags$strong("Reactome Pathways"), pathway_content),
      glass_card(header = tags$strong("InterPro Domains"), domain_content),
      glass_card(header = tags$strong("Predicted Structure Confidence"), structure_content)
    )
  })

  output$dt_reactome <- renderDT({
    df <- reactome_df(rv$reactome)
    req(!is.null(df) && nrow(df) > 0)
    df$Reactome_ID <- sprintf(
      '<a href="https://reactome.org/content/detail/%s" target="_blank" style="color:#2d6da3">%s</a>',
      htmltools::htmlEscape(df$Reactome_ID), htmltools::htmlEscape(df$Reactome_ID)
    )
    datatable(df, escape = FALSE, rownames = FALSE, options = DT_OPTS)
  })

  output$dt_interpro <- renderDT({
    df <- interpro_df(rv$interpro)
    req(!is.null(df) && nrow(df) > 0)
    source_path <- tolower(df$Source)
    df$Accession <- sprintf(
      '<a href="https://www.ebi.ac.uk/interpro/entry/%s/%s/" target="_blank" style="color:#2d6da3">%s</a>',
      htmltools::htmlEscape(source_path),
      htmltools::htmlEscape(df$Accession),
      htmltools::htmlEscape(df$Accession)
    )
    datatable(df, escape = FALSE, rownames = FALSE, options = DT_OPTS)
  })

  # ══════════════════════════════════════════════════════════════════════════
  #  NETWORK — STRING Interactions
  # ══════════════════════════════════════════════════════════════════════════
  parse_si <- function(si) {
    if (is.null(si) || length(si) == 0) return(NULL)
    tryCatch(do.call(rbind, lapply(si, function(r) {
      data.frame(
        Prot_A = scalar(r[["preferredName_A"]], "-"),
        Prot_B = scalar(r[["preferredName_B"]], "-"),
        Score  = tryCatch(round(as.numeric(r[["score"]]), 3), error = function(e) 0),
        stringsAsFactors = FALSE
      )
    })), error = function(e) NULL)
  }

  output$ui_network <- renderUI({
    if (is.null(rv$entry)) return(glass_card(empty_state("Search for a gene first.", "circle-nodes")))
    df   <- parse_si(rv$si)
    gene <- get_gene(rv$entry)
    if (is.null(df) || nrow(df) == 0)
      return(glass_card(glass_alert("STRING-db \u2014 no interactions found.", "warning")))
    tagList(
      glass_card(
        header = tagList(tags$strong(paste0("Interactions \u2014 ", gene, " (STRING-db)")),
                         tags$span(class = "badge-glass badge-grey", style = "margin-left:8px;", paste(nrow(df), "partners"))),
        visNetworkOutput(ns("vis_ppi"), height = "500px")
      ),
      glass_card(header = tags$strong("Interaction Table"), DTOutput(ns("dt_ppi")))
    )
  })

  output$vis_ppi <- renderVisNetwork({
    df <- parse_si(rv$si)
    req(!is.null(df) && nrow(df) > 0)
    gene  <- get_gene(rv$entry)
    all_n <- unique(c(df$Prot_A, df$Prot_B))
    nodes <- data.frame(
      id = all_n, label = all_n,
      color = ifelse(all_n == gene, "#dc2626", "#3d7ec2"),
      font.color = "#1e2a3a",
      size = ifelse(all_n == gene, 30, 16),
      shadow = TRUE, stringsAsFactors = FALSE
    )
    edges <- data.frame(
      from = df$Prot_A, to = df$Prot_B, value = df$Score,
      title = paste0("Score: ", df$Score), stringsAsFactors = FALSE
    )
    visNetwork(nodes, edges) |>
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled = TRUE,
        style = "background:rgba(255,255,255,0.7);color:#1e2a3a;border:1px solid rgba(0,0,0,0.1);border-radius:8px;padding:6px;")) |>
      visEdges(smooth = list(enabled = TRUE, type = "continuous"),
               color = list(color = "#c0cdd8", highlight = "#3d7ec2")) |>
      visNodes(font = list(color = "#1e2a3a", size = 13), shadow = TRUE,
               borderWidth = 2, borderWidthSelected = 3) |>
      visPhysics(barnesHut = list(gravitationalConstant = -4000, centralGravity = 0.3, springLength = 130)) |>
      visInteraction(navigationButtons = TRUE, zoomView = TRUE)
  })

  output$dt_ppi <- renderDT({
    df <- parse_si(rv$si)
    req(!is.null(df) && nrow(df) > 0)
    datatable(df, rownames = FALSE, options = DT_OPTS)
  })

  # ══════════════════════════════════════════════════════════════════════════
  #  LITERATURE — PubMed
  # ══════════════════════════════════════════════════════════════════════════
  output$ui_literature <- renderUI({
    if (is.null(rv$entry)) return(glass_card(empty_state("Search for a gene first.", "book-open")))
    gene <- get_gene(rv$entry)
    pub  <- rv$pub
    if (is.null(pub) || nrow(pub) == 0)
      return(glass_card(glass_alert("PubMed \u2014 no publications found.", "warning")))
    glass_card(
      header = tagList(
        tags$strong(paste0("Publications \u2014 ", gene)),
        tags$span(class = "badge-glass badge-grey", style = "margin-left:8px;", paste(nrow(pub), "articles"))
      ),
      DTOutput(ns("dt_pub"))
    )
  })

  output$dt_pub <- renderDT({
    pub <- rv$pub
    req(!is.null(pub) && nrow(pub) > 0)
    pub$Link <- sprintf('<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank"
      style="color:#2d6da3">%s</a>', pub$PMID, pub$PMID)
    pub$PMID <- NULL
    datatable(pub, escape = FALSE, rownames = FALSE,
      options = c(DT_OPTS, list(columnDefs = list(list(width = "40%", targets = 0)))))
  })
  })
}

# =============================================================================
#  RUN
# =============================================================================
if (sys.nframe() == 0L) {
  shinyApp(
    ui = fluidPage(title = "Gene Explorer", geneExplorerUI("app")),
    server = function(input, output, session) { geneExplorerServer("app") }
  )
}
