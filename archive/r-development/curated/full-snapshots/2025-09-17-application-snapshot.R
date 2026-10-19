# Archived R development file
# Original path: archive version/APPLICATION.R
# Original created: 2025-09-17 10:29:14
# Original modified: 2025-09-25 22:53:06
# Archive rationale: Large application snapshot preserving an intermediate full-app state.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# ========================= LIBRARIES =========================
library(shiny)
library(shinyjs)
library(htmltools)
library(bslib)
library(lubridate)
library(shinyWidgets)
library(DT)
library(pedtools)
library(sortable)
library(jsonlite)
# ---------------- Anti-distortion (layout) -------------------
ANTI_DEF_WIDTH <- 0 # 0 = auto (recommended)
ANTI_DEF_SLOT_PX <- 150 # pixels per column for auto layout
get_parent_info <- function(parent_id, ped_df) {
  if (is.na(parent_id) || parent_id == "" || !(parent_id %in% ped_df$id)) {
    return("")
  }
  parent_row <- ped_df[ped_df$id == parent_id, ]
  parent_info <- paste0(
    parent_id, " (",
    parent_row$prénom, " ", parent_row$nom, ")"
  )
  return(parent_info)
}

getSexSymbolSVG <- function(sex, size = 100) {
  filter_tag <- '<filter id="shadow" x="-50%" y="-50%" width="200%" height="200%">
                   <feDropShadow dx="0" dy="1" stdDeviation="2" flood-color="rgba(0,0,0,0.24)" />
                 </filter>'

  shape <- switch(as.character(sex),

    # Homme → carré
    "1" = '<rect x="10" y="10" width="80" height="80" fill="rgba(202,194,189,30%)" stroke="#333" stroke-width="1" filter="url(#shadow)"/>',

    # Femme → cercle
    "2" = '<circle cx="50" cy="50" r="40" fill="rgba(202,194,189,30%)"  stroke="#333" stroke-width="1" filter="url(#shadow)"/>',

    # Inconnu → losange
    '<polygon points="50,5 95,50 50,95 5,50" fill="rgba(202,194,189,30%)"  stroke="#333" stroke-width="1" filter="url(#shadow)"/>'
  )

  sprintf(
    '<svg width="%s" height="%s" viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">
       %s
       %s
     </svg>',
    size, size, filter_tag, shape
  )
}
# ========================= UTILITIES =========================
`%||%` <- function(x, y) if (is.null(x)) y else x

breakLabs <- function(x, breakAt = "  ") {
  labs <- labels(x)
  names(labs) <- gsub(breakAt, "\n", labs)
  labs
}

stop2 <- function(...) {
  args <- lapply(list(...), toString)
  args <- append(args, list(call. = FALSE))
  do.call(stop, args)
}

# ---- Partenaires ----
getPartners <- function(ped, id) {
  kids <- pedtools::children(ped, id, internal = FALSE)
  if (length(kids) == 0) {
    return(NULL)
  }
  partnerList <- lapply(kids, function(kid) setdiff(pedtools::parents(ped, kid), id))
  unique(unlist(partnerList))
}
getPartner <- function(ped, id) {
  ps <- getPartners(ped, id)
  if (is.null(ps) || length(ps) == 0) {
    return(NULL)
  }
  if (length(ps) == 1) {
    return(ps[1])
  }
  NULL
}

# ---- Ajout enfant avec/sans partenaire ----
addChildWithPartner <- function(ped, id, partner = NULL, childSex = 0, ...) {
  id_sex <- pedtools::getSex(ped, id)
  if (id_sex == 1) {
    pedtools::addChildren(ped, father = id, mother = partner, sex = childSex, ...)
  } else if (id_sex == 2) {
    pedtools::addChildren(ped, mother = id, father = partner, sex = childSex, ...)
  } else {
    stop2("Impossible d’ajouter un enfant : sexe inconnu pour l’individu ", id)
  }
}

# ---- Ajout de parents ----
addPar <- function(x, ids) {
  n <- length(ids)
  pars <- ids[-1]
  if (n == 1) {
    return(pedtools::addParents(x, ids[1], verbose = FALSE))
  }
  parsex <- pedtools::getSex(x, pars)
  fa <- mo <- NULL
  if (n == 3) {
    if (parsex[1] == 1 && parsex[2] == 2) {
      fa <- pars[1]
      mo <- pars[2]
    } else if (parsex[1] == 2 && parsex[2] == 1) {
      fa <- pars[2]
      mo <- pars[1]
    } else {
      stop2("Sexes parentaux incompatibles pour: ", ids[2:3])
    }
  } else if (n == 2) {
    if (parsex[1] == 1) {
      fa <- ids[2]
    } else if (parsex[1] == 2) {
      mo <- ids[2]
    } else {
      stop2("Sexe inconnu pour le parent: ", ids[2])
    }
  } else {
    stop2("Trop d’individus sélectionnés")
  }
  pedtools::addParents(x, ids[1], father = fa, mother = mo, verbose = FALSE)
}

# ---- Ajout sibling ----
addSib <- function(x, id, sex = 1, side = c("right", "left")) {
  if (length(id) != 1) stop(sprintf("Sélectionnez un seul individu (%s).", paste(id, collapse = ",")))
  if (!pedtools::is.ped(x)) stop("Pedigree invalide.")
  if (id %in% pedtools::founders(x)) x <- pedtools::addParents(x, id, verbose = FALSE)
  pars <- pedtools::parents(x, id)
  if (length(pars) != 2 || any(is.na(pars))) stop("Parents inconnus/incomplets.")
  newped <- pedtools::addChildren(x, father = pars[1], mother = pars[2], sex = sex, verbose = FALSE)
  idInt <- pedtools::internalID(x, id)
  n <- length(x$ID)
  ord <- switch(match.arg(side),
    left  = c(seq_len(idInt - 1), n + 1, idInt:n),
    right = c(seq_len(idInt), n + 1, if (idInt < n) seq.int(idInt + 1, n))
  )
  pedtools::reorderPed(newped, ord)
}

# ---- Triplets (2 siblings) ----
addTriplets <- function(ped, id, sexes = c(1, 2)) {
  stopifnot(length(id) == 1)
  if (id %in% pedtools::founders(ped)) ped <- pedtools::addParents(ped, id, verbose = FALSE)
  parents <- pedtools::parents(ped, id)
  ped2 <- pedtools::addChild(ped, parents, sex = sexes[1], verbose = FALSE)
  ped3 <- pedtools::addChild(ped2, parents, sex = sexes[2], verbose = FALSE)
  new_ids <- setdiff(labels(ped3), labels(ped))
  if (length(new_ids) != 2) stop("Erreur lors de l'ajout des triplés")
  triplet_ids <- sort(c(id, new_ids))
  list(ped = ped3, triplet_ids = triplet_ids)
}

# ---- Demi-frère/soeur (crée explicitement le parent manquant) ----
add_half_sibling <- function(ped, anchor_id, side = c("paternal", "maternal"), childSex = 0) {
  side <- match.arg(side)
  f_id <- pedtools::father(ped, anchor_id, internal = FALSE)
  m_id <- pedtools::mother(ped, anchor_id, internal = FALSE)
  next_id <- as.character(max(suppressWarnings(as.integer(labels(ped))), na.rm = TRUE) + 1)

  if (side == "paternal") {
    if (is.na(f_id) || !nzchar(f_id)) stop("Father unknown: cannot add paternal half-sibling.")
    new_mother <- next_id
    ped2 <- pedtools::addChildren(ped, father = f_id, mother = new_mother, sex = childSex, verbose = FALSE)
    ped2 <- pedtools::setSex(ped2, ids = new_mother, sex = 2)
  } else {
    if (is.na(m_id) || !nzchar(m_id)) stop("Mother unknown: cannot add maternal half-sibling.")
    new_father <- next_id
    ped2 <- pedtools::addChildren(ped, father = new_father, mother = m_id, sex = childSex, verbose = FALSE)
    ped2 <- pedtools::setSex(ped2, ids = new_father, sex = 1)
  }
  ped2
}

# ---- Utilitaires divers ----
changeSex <- function(ped, id, sex) pedtools::setSex(ped, ids = id, sex = sex)
formatAnnot <- function(textAnnot, cex, font = 2, col = "blue") {
  if (is.null(textAnnot)) {
    return(NULL)
  }
  lapply(textAnnot, function(b) list(b, cex = cex, font = font, col = col))
}
toggleId <- function(vec, id) {
  v <- vec %||% character(0)
  if (id %in% v) setdiff(v, id) else union(v, id)
}

# ── Reusable Modal Builders ───────────────────────────────────────────────────
createPedigreeModal <- function(title_value = "", selected_model = "") {
  modalDialog(
    title = h4("Pedigree Model Selection", style = "font-weight:600;margin-bottom:3px;"),
    div(
      style = "display:flex;flex-direction:column;gap:16px;",
      textInput(
        "modalPedTitle",
        "Pedigree title:",
        value = title_value %||% "",
        placeholder = "Enter a pedigree title"
      ),
      selectInput(
        "modalPedChoice",
        label = "Choose a pedigree to display:",
        choices = c("--- Select ---" = "", names(pedigree_list)),
        selected = selected_model
      ),
      div(
        style = "background:rgba(255,255,255,0.46);border-radius:14px;box-shadow:0 2px 9px rgba(120,140,200,0.09);padding:15px;",
        h5("Preview of selected pedigree:"),
        plotOutput("modalPreviewPed", height = "180px")
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("modalConfirmPed", "Confirm", class = "btn btn-primary")
    ),
    easyClose = TRUE
  )
}

createRandomPedigreeModal <- function() {
  modalDialog(
    title = "Random Pedigree Preview",
    textInput(
      inputId = "randomPedTitle",
      label = "Pedigree title:",
      value = "",
      width = "100%",
      placeholder = "Enter a title for this pedigree"
    ),
    plotOutput("previewPlot"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirmPed", "Confirm", class = "btn btn-primary")
    ),
    easyClose = TRUE
  )
}
# ======================= PEDIGREE CATALOG ====================
pedigree_list <- list(
  "Trio" = function() nuclearPed(1),
  "Siblings" = function() nuclearPed(2),
  "Sibship of 3" = function() nuclearPed(3, sex = c(1, 2, 1)),
  "Half-sibs, maternal" = function() halfSibPed(1, 1, type = "maternal"),
  "Half-sibs, paternal" = function() halfSibPed(1, 1),
  "Avuncular" = function() avuncularPed(),
  "Grandparent" = function() ancestralPed(2),
  "Great-grandparent" = function() ancestralPed(3),
  "1st cousins" = function() cousinPed(1, symmetric = TRUE),
  "1st cousins + child" = function() cousinPed(1, symmetric = TRUE, child = TRUE),
  "2nd cousins" = function() cousinPed(2, symmetric = TRUE),
  "2nd cousins + child" = function() cousinPed(2, symmetric = TRUE, child = TRUE),
  "Half 1st cousins" = function() halfCousinPed(1, symmetric = TRUE),
  "Half 1st cousins + child" = function() halfCousinPed(1, symmetric = TRUE, child = TRUE),
  "Half 2nd cousins" = function() halfCousinPed(2, symmetric = TRUE),
  "Half 2nd cousins + child" = function() halfCousinPed(2, symmetric = TRUE, child = TRUE),
  "3/4-siblings" = function() {
    nuclearPed(2) |>
      addSon(c(3, 5), verbose = FALSE) |>
      addSon(4:5)
  },
  "3/4-siblings + child" = function() {
    nuclearPed(2) |>
      addSon(c(3, 5), verbose = FALSE) |>
      addDaughter(4:5) |>
      addSon(6:7)
  },
  "Double 1st cousins" = function() doubleFirstCousins(),
  "Double 1st cousins + child" = function() doubleCousins(1, 1, child = TRUE),
  "Quad half 1st cousins" = function() quadHalfFirstCousins()
)

# Calculate age (full text, e.g. "3 years 2 months 5 days")
calculateAgeText <- function(birth, death = NA) {
  if (is.na(birth) || is.null(birth) || birth == "") {
    return("")
  }
  birth <- as.Date(birth, format = "%d-%m-%Y")
  end_date <- if (!is.na(death) && death != "") as.Date(death, format = "%d-%m-%Y") else Sys.Date()
  days <- as.integer(difftime(end_date, birth, units = "days"))
  if (is.na(days) || days < 0) {
    return("")
  }
  if (days >= 365) {
    years <- floor(days / 365.25)
    months <- floor((days %% 365.25) / 30.44)
    rest_days <- round(days - (years * 365.25) - (months * 30.44))
    age_parts <- c(
      if (years > 0) paste0(years, " year", ifelse(years > 1, "s", "")),
      if (months > 0) paste0(months, " month", ifelse(months > 1, "s", "")),
      if (rest_days > 0) paste0(rest_days, " day", ifelse(rest_days > 1, "s", ""))
    )
    paste(age_parts, collapse = " ")
  } else if (days >= 30) {
    months <- floor(days / 30.44)
    rest_days <- round(days - months * 30.44)
    age_parts <- c(
      paste0(months, " month", ifelse(months > 1, "s", "")),
      if (rest_days > 0) paste0(rest_days, " day", ifelse(rest_days > 1, "s", ""))
    )
    paste(age_parts, collapse = " ")
  } else if (days >= 7) {
    weeks <- floor(days / 7)
    rest_days <- days - weeks * 7
    age_parts <- c(
      paste0(weeks, " week", ifelse(weeks > 1, "s", "")),
      if (rest_days > 0) paste0(rest_days, " day", ifelse(rest_days > 1, "s", ""))
    )
    paste(age_parts, collapse = " ")
  } else {
    paste0(days, " day", ifelse(days > 1, "s", ""))
  }
}

# Helper: Parse age string and extract a given unit (advanced utility)
extract_units <- function(txt, unit) {
  pattern <- paste0("([0-9]+)\\s*", unit)
  match <- regmatches(txt, regexpr(pattern, txt, perl = TRUE))
  if (length(match) > 0 && nchar(match[1]) > 0) {
    as.integer(gsub("\\D", "", match[1]))
  } else {
    0
  }
}

# Compute a relative date from a reference (e.g., subtract N years/months/days)
compute_relative_date <- function(reference, years, months, days, direction = "backward") {
  if (direction == "backward") {
    reference %m-% years(years) %m-% months(months) - days
  } else {
    reference %m+% years(years) %m+% months(months) + days
  }
}

# ℹ️ CSS ================================
styles_css <- "
/* --- IMPORTS EN TÊTE --- */
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=DM+Mono:wght@300;400;500&display=swap');
@import url('https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css');
@import url('https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@24,350,0,0');

/* --- VARIABLES --- */
:root{
  --black: #000000;
  --black-3: #242424;
  --white: #FFFFFF;
  --chalk: #E2E2E2;
  --grey-7: #747373;
  --grey-9: #A0A0A0;
  --h-transition: 220ms ease;
}

html, body { height:100%; background:rgba(246, 246, 246, 0.99); }
*{ box-sizing:border-box; }
.container-fluid { padding-left:0; padding-right:0; }

/* ---- HERO (collapsible) ---- */
.sirius-hero{
  position: sticky; top: 0; z-index: 1000;
  display:flex; flex-direction:column; justify-content:center; align-items:center; gap:40px;
  width:100%; min-height:420px; padding:64px 112px; background:var(--black);
  transition: padding var(--h-transition), min-height var(--h-transition), gap var(--h-transition),
              box-shadow var(--h-transition), backdrop-filter var(--h-transition);
  border-bottom: 1px solid rgba(255,255,255,0.06);
}
.sirius-hero.scrolled{ box-shadow: 0 8px 24px rgba(0,0,0,.55); backdrop-filter: saturate(120%) blur(6px); }

.hero-container{
  display:flex; flex-direction:column; align-items:center; gap:24px;
  width:100%; max-width:830px; margin:0 auto; transition: gap var(--h-transition);
}
.hero-stack{ display:flex; flex-direction:column; align-items:center; gap:12px; width:100%; }

.hero-eyebrow{
  font-family:'DM Mono', monospace; font-weight:400; font-size:14px; line-height:20px;
  letter-spacing:1.3px; text-transform:uppercase; color:var(--grey-7);
  display:flex; align-items:center; justify-content:center;
  transition: opacity var(--h-transition), max-height var(--h-transition), margin var(--h-transition);
}
.hero-title{
  width:100%; min-height:48px;
  font-family:'Inter', system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif;
  font-weight:400; font-size:40px; line-height:120%; letter-spacing:-1.5px; color:rgba(244, 244, 244, 0.8);
  display:flex; align-items:center; justify-content:center; text-align:center;
  transition: font-size var(--h-transition), letter-spacing var(--h-transition);
}
.hero-subtitle{
  width:100%; min-height:24px;
  font-family:'Inter', system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif;
  font-weight:400; font-size:16px; line-height:150%; letter-spacing:-0.3px; color:var(--grey-9);
  display:flex; align-items:center; justify-content:center; text-align:center;
  transition: opacity var(--h-transition), max-height var(--h-transition), margin var(--h-transition);
}

/* --- CTAs container --- */
.hero-ctas{
  border: 1px solid rgba(255,255,255,.06);
  display:grid;
  grid-template-columns: repeat(3, minmax(220px, 1fr));
  gap:16px;
  width:100%;
  max-width: 980px;
  margin: 0 auto;
  align-items: stretch;
}

/* --- Boutons carte --- */
.btn.btn-pill, .action-button.btn-pill{
  display:flex; flex-direction:column; align-items:center; justify-content:center; text-align:center;
  gap:10px;
  padding:16px 18px;

  /* sizing cohérent (pas de doublons) */
  min-width:220px;
  max-width:100%;
  min-height:140px;

  border:none; border-radius:10px;
  background:var(--black-3); color:var(--chalk);

  font-family:'Inter', system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif;
  font-weight:500; font-size:16px; line-height:24px; letter-spacing:-0.3px;

  cursor:pointer; text-decoration:none;
  transition: transform .06s ease, opacity .2s ease, background-color .2s ease,
              color .2s ease, padding var(--h-transition), height var(--h-transition),
              box-shadow .2s ease;
}
.btn.btn-pill:hover{ opacity:.95; box-shadow:0 6px 16px rgba(0,0,0,.35); }
.btn.btn-pill:active{ transform: translateY(1px); }
.btn.btn-pill:focus{ outline:2px solid rgba(255,255,255,.35); outline-offset:2px; }

/* Variante claire si besoin */
.btn-light{ background:var(--chalk)!important; color:var(--black)!important; }

/* Variante sombre référencée par le code */
.btn-dark{ background:var(--black-3)!important; color:var(--chalk)!important; }

.card-ico{ line-height:0; }
.card-ico img{ width:36px; height:36px; opacity:.96; filter: invert(1) brightness(1.2) contrast(1.05); }

.card-title{ font-weight:600; font-size:15px; line-height:1.2; color:#fff; margin-top:2px; }
.smallcaps{ letter-spacing:.6px; text-transform:uppercase; font-size:12px; opacity:.9; }

.card-desc{
  display:block; width:100%;
  font-size:13px; font-weight:400; line-height:1.45; color: rgb(170,170,180);
  max-width: 90%;
  white-space:normal !important; word-break:break-word; overflow-wrap:anywhere;
  margin-left:auto; margin-right:auto;
}

/* ---- BOUTON COLLAPSE (chevron) ---- */
.collapse-toggle{
  position:absolute; right:16px; top:16px;
  width:36px; height:36px; border-radius:999px;
  display:flex; align-items:center; justify-content:center;
  background:rgba(255,255,255,.06); color:#fff; border:1px solid rgba(255,255,255,.12);
  cursor:pointer; transition: background .2s ease, transform var(--h-transition);
}
.collapse-toggle:hover{ background:rgba(255,255,255,.12); }
.collapse-toggle svg{ transition: transform var(--h-transition); }

/* ---- ÉTAT COLLAPSÉ ---- */
.sirius-hero.collapsed{ min-height:76px; padding:10px 16px; gap:8px; }
.sirius-hero.collapsed .hero-container{ gap:4px; }
.sirius-hero.collapsed .hero-eyebrow,
.sirius-hero.collapsed .hero-subtitle{ opacity:0; max-height:0; margin:0; overflow:hidden; }
.sirius-hero.collapsed .hero-title{ font-size:18px; letter-spacing:-0.4px; min-height:auto; }

.sirius-hero.collapsed .hero-ctas{
  grid-template-columns: repeat(3, 1fr);
}
.sirius-hero.collapsed .hero-ctas .btn.btn-pill{
  min-height:72px;
  flex-direction:row;
}
.sirius-hero.collapsed .card-desc{ display:none; }
.sirius-hero.collapsed .card-title{ font-size:14px; }
.sirius-hero.collapsed .collapse-toggle svg{ transform: rotate(180deg); }

/* ---- Responsive ---- */
@media (max-width: 900px){
  .sirius-hero{ padding:48px 24px; }
  .hero-title{ font-size:34px; letter-spacing:-1.2px; }
  .hero-subtitle{ font-size:15px; }
}
@media (max-width: 520px){
  .hero-title{ font-size:28px; letter-spacing:-1px; }
  .hero-subtitle{ font-size:14px; }
  .hero-ctas{ grid-template-columns: 1fr; }
  .btn.btn-pill{ min-height:120px; }
}

/* ---- Démo scroll (optionnel) ---- */
.demo-content{ padding:12px; color:rgba(196, 196, 196, 0.8); max-width:900px; margin:0 auto; }
.demo-block{ background:linear-gradient(180deg, rgba(0,0,0,0.03), rgba(0,0,0,0.01)); border:1px dashed rgba(0,0,0,0.08); border-radius:16px; }

/* Zone centrale */
#center_col{
  display:flex; flex-direction:column; align-items:center; text-align:center;
  font-family:'Helvetica Neue', Inter, Arial, sans-serif; justify-content:center; padding-top:44px;
}
#center_col img{ width: 80px; height: 80px; margin-bottom: 8px; object-fit: contain; filter: drop-shadow(0 4px 12px rgba(45,91,145,.18)); }
#center_col h1{ color: rgba(0,0,0,.6); text-shadow: 3px 2px 3px rgba(255,255,255,.2);
  font-size: clamp(30px, 4.2vw, 40px); letter-spacing:.3px; font-weight: 400; margin: 0 0 10px 0; }
#center_col h3{ max-width: 64ch; margin: 0 auto; color: rgba(0,0,0,.76); font-weight: 200; font-size: clamp(15px, 2.2vw, 18px); line-height: 1.5; letter-spacing: .01em; }

hr.left_hr{ line-height:1em; position:relative; outline:0; border:0; text-align:center; height:1.5em; width:100%; opacity:.55; margin: 8px 0 16px 0; }
hr.left_hr::before{ content:''; background: linear-gradient(to right, transparent, #818078, transparent);
  position:absolute; left:0; top:50%; width:100%; height:1px; }
hr.left_hr::after{ content: attr(data-content); position:relative; display:inline-block; color:#818078; background-color: transparent; padding:0 .5em; line-height:1.5em; }

/* Boutons ronds en bas de page */
.btn_group_menu {
  display: flex;
  justify-content: center;
  align-items: center;
  background: transparent;
  padding: 20px;
  margin-top: 10px;
  width: 100%;
  margin-left: auto;
  margin-right: auto;
  gap: 24px;
}
.btn_group_menu .btn {
  width: 65px;
  height: 65px;
  border-radius: 50%;
  background-color: rgba(106, 114, 130, 0.75);
  backdrop-filter: blur(16px) saturate(180%);
  -webkit-backdrop-filter: blur(16px) saturate(180%);
  border: none;
  box-shadow: rgba(50, 50, 93, 0.25) 0px 2px 5px -1px,
              rgba(0, 0, 0, 0.3) 0px 1px 3px -1px;
  display: flex;
  justify-content: center;
  align-items: center;
  padding: 0;
  transition: transform 0.2s ease-in-out;
}
.btn_group_menu .btn:hover { transform: scale(1.05); }
.btn_group_menu .btn-wrapper { display: flex; flex-direction: column; align-items: center; gap: 6px; }
.btn_group_menu .icon { color: #ffffff; font-size: 24px; }
.btn_group_menu .tab-label {
  color: #3E3E3D; font-size: 13px; font-weight: 300; font-family: 'Helvetica Neue', Inter, Arial, sans-serif; text-align: center;
}
/* Toolbar bas */
.toolbar-outer{ width:100%; }
.toolbar-glass{
  background: linear-gradient(180deg, rgba(255,255,255,0.9), rgba(255,255,255,0.86));
  backdrop-filter: blur(16px) saturate(1.05); -webkit-backdrop-filter: blur(16px) saturate(1.05);
  border-radius: 14px;
  box-shadow: 0 8px 24px rgba(16,38,60,0.10), inset 0 1px 0 rgba(255,255,255,0.65);
  display: flex; align-items: center; justify-content: flex-start; gap: 12px; padding: 10px 12px;
}
.toolbar-glass::after{ content:''; position:absolute; inset:-1px; border-radius:inherit; pointer-events:none; background: radial-gradient(120% 100% at 50% -20%, rgba(120,170,255,0.18), transparent 60%); mix-blend-mode: screen; }
.toolbar-inner{ width: auto !important; max-width: none !important; margin: 0 !important; flex: 0 0 auto !important; display: flex; align-items: center; gap: 12px; }
.toolbar-glass .shiny-input-container{ margin: 0 !important; flex: 1 1 auto !important; min-width: 240px; }
.toolbar-glass .icon-btn.btn{ flex: 0 0 auto !important; height: 40px; }
.v-sep{ width: 1px; height: 26px; align-self: center; flex: 0 0 1px; margin: 0 12px; border-radius: 1px;
  background: linear-gradient(to bottom, transparent, rgba(20,20,20,.22), transparent);
}
@media (prefers-color-scheme: dark){
  .v-sep{ background: linear-gradient(to bottom, transparent, rgba(255,255,255,.28), transparent); }
}
#plotTitle{
  width:100%; max-width:none; height:40px;
  background: rgba(255,255,255,0.16);
  border: 1px solid rgba(180,200,220,0.85);
  border-radius: 999px; padding: 10px 16px;
  color:  rgba(0,0,0,0.9); font-weight: 300; font-size: 14px; line-height:1.2;
  box-shadow: inset 0 1px 3px rgba(0,0,0,0.12);
  transition: background .18s ease, border-color .18s ease, box-shadow .18s ease, transform .12s ease;
}
#plotTitle::placeholder{ color: rgba(28,28,30,0.68); font-style: italic; }
#plotTitle:focus{ background: rgba(255,255,255,.26); border-color: rgba(120,170,255,.85); outline:none; transform: translateY(-1px); }
.logo_DNA{ width:28px; height:28px; filter: drop-shadow(0 4px 12px rgba(45,91,145,.18)); opacity:0.9; transform: rotate(45deg); }
.main-split{display:grid;grid-template-columns:320px 1fr 480px;gap:12px;margin:8px}
@media (max-width:1220px){.main-split{grid-template-columns:1fr 1fr}}
@media (max-width:860px){.main-split{grid-template-columns:1fr}}
.card{border-radius:16px;background:#fff;border:1px solid #e5e7eb;box-shadow:0 10px 30px rgba(2,6,23,.08),0 2px 8px rgba(2,6,23,.06);overflow:hidden}
.card .card-header{padding:12px 16px;font-weight:700;color:#0f172a;border-bottom:1px solid #eef2f7;background:#fafbfc}
.card .card-body{padding:12px 16px;color:#334155}
/* --- DataTable: remove blue selectedrow highlight --- */
table.dataTable tbody tr.selected,
table.dataTable tbody tr.selected > td,
table.dataTable tbody tr.selected > th {
  background-color: transparent !important;
  color: inherit !important;
}

/* Subtle hover only (very light gray) */
table.dataTable tbody tr:hover > td {
  background-color: #f9fafb !important;
}

/* Remove focus ring inside cells (prevents blue outlines) */
.dataTables_wrapper table.dataTable td:focus,
.dataTables_wrapper table.dataTable th:focus {
  outline: none !important;
}

/* Make the +/- cell look clickable but not loud */
td.details-control {
  cursor: pointer;
  color: #64748b;            /* muted */
}
tr.shown td.details-control {
  color: #334155;            /* slightly darker when open */
}

/* --- Compact, quiet row-details container --- */
table.dataTable tbody tr.child > td {
  padding: 0 !important;     /* tighter child row */
  border-top: none !important;
}
.row-details {
  padding: 10px 12px 12px 14px;
  margin: 0;
  background: #fcfcfd;
  border-left: 3px solid #e5e7eb;
  border-top: 1px solid #f1f5f9;
  font-size: 13px;
  line-height: 1.35;
  color: #475569;
}
.row-details hr {
  border: none;
  border-top: 1px solid #eef2f7;
  margin: 8px 0;
}
.row-details b {
  font-weight: 600;
  color: #334155;
}

/* --- Sortable pill look: compact, unobtrusive --- */
.rank-list {
  gap: 8px !important;
}
.rank-list .rank-list-item {
  display: inline-block;
  padding: 6px 10px;
  background: #f6f7f9;
  border: 1px solid #e5e7eb;
  border-radius: 10px;
  box-shadow: 0 1px 2px rgba(2,6,23,.06);
  font-size: 12.5px;
  color: #334155;
  margin: 4px 6px 0 0;
}
.rank-list .rank-list-item:hover {
  background: #eef2f7;
  border-color: #dfe3ea;
}

/* ===== Séparateur avec chip ===== */
.hr-slot{
  display:flex; align-items:center; gap:12px; width:100%; margin:8px 0 18px; font-family:'Helvetica Neue'; justify-content:center;
}
.hr-slot::before, .hr-slot::after{ content:''; height:1px; flex:1 1 auto; background-image:linear-gradient(to right, transparent, rgba(61,61,61,.9), transparent); }
.hr-chip{
  display:inline-flex; align-items:center; justify-content:center; padding:.35em .9em; border-radius:.6em;
  background:rgba(245,247,250,.6); color:#3D3D3D; font-weight:300; letter-spacing:.18em; font-size:14px; text-decoration:none;
  transition:filter .2s ease, transform .08s ease;
}
.hr-chip:hover{ filter:brightness(1.05); }
.hr-chip:active{ transform:translateY(1px); }
.hr-chip:focus-visible{ outline:2px solid #0d6efd; outline-offset:2px; border-radius:.6em; }

/* Keep the three sections visually separated but light */
#parents_ .shiny-html-output,
#sib_ .shiny-html-output,
#children_ .shiny-html-output { /* fallback if ids are prefixed */
  margin-top: 6px;
}
/* ---------------- CARD ---------------- */

 /* taille/réglages des Material Symbols */
      .material-symbols-outlined{
        font-variation-settings: 'FILL' 0, 'wght' 400, 'GRAD' 0, 'opsz' 20;
        font-size:18px; line-height:1; display:inline-flex; align-items:center;
      }

      /* ---- Card ---- */
      .legend-wrap{
        width:280px; margin:24px auto; background:#cdd3db;
        border-radius:10px; box-shadow:0 6px 18px rgba(0,0,0,.06);
        overflow:hidden;
      }
      .legend-header{
        background:#f6f6f7; padding:14px 16px; text-align:center;
        letter-spacing:.5px; font-size:22px; color:#535b61; font-weight:600;
        border-bottom:1px solid #eceff3;
      }

      /* ---- Toolbar ---- */
      .legend-toolbar{
        display:flex; justify-content:space-around; gap:6px;
        background:#f6f6f7; padding:10px 14px; border-bottom:1px solid #eceff3;
      }
      .tool-btn.btn{
        all:unset; display:flex; flex-direction:column; align-items:center; gap:6px;
        cursor:pointer; user-select:none; position:relative; overflow:hidden;
        transition:transform .06s ease, box-shadow .2s ease, background .2s ease;
        -webkit-tap-highlight-color:transparent; outline:none !important; box-shadow:none !important;
      }
      .tool-btn .ico{
        width:34px; height:34px; border-radius:999px;
        border:1px solid #d8dde5; background:#ffffff;
        display:flex; align-items:center; justify-content:center;
        box-shadow:0 2px 6px rgba(0,0,0,.05);
      }
      .tool-btn span.txt{ font-size:11px; color:#8a939e; }
      .tool-btn:hover{ transform:translateY(-1px); }
      .tool-btn:active{ transform:translateY(0); }

      /* pas d'anneau de focus */
      .tool-btn.btn:focus,
      .tool-btn.btn:focus-visible,
      .tool-btn.btn:-moz-focusring{ outline:0 !important; box-shadow:none !important; }
      .tool-btn.btn::-webkit-focus-inner{ border:0; }

      /* ripple */
      .tool-btn::after{
        content:''; position:absolute; left:50%; top:50%;
        width:0; height:0; border-radius:999px; background:rgba(0,0,0,.10);
        transform:translate(-50%,-50%); opacity:0;
      }
      .tool-btn:active::after{ animation:ripple .50s ease-out; }
      @keyframes ripple{
        from{ width:0; height:0; opacity:.35; }
        to  { width:260px; height:260px; opacity:0; }
      }

      /* ---- Body ---- */
      .legend-body{ padding:12px; }
      .section-title{
        font-size:13px; color:#6f7883; font-weight:600; margin:10px 0 6px;
      }
      .list{
        background:#ffffff; border-radius:8px;
        box-shadow:0 0 0 1px rgba(0,0,0,.04); overflow:hidden;
      }

      /* ---- Items (actionButton) ---- */
      .legend-item-btn.btn{
        all:unset; display:flex; align-items:center; gap:10px; width:100%;
        background:#fff; padding:10px 12px; font-size:14px; color:#2f3640;
        border-bottom:1px solid #eceff3; cursor:pointer; user-select:none;
        position:relative; overflow:hidden;
        transition:transform .06s ease, background .2s ease, box-shadow .2s ease;
        -webkit-tap-highlight-color:transparent; outline:none !important; box-shadow:none !important;
      }
      .legend-item-btn.btn:last-child{ border-bottom:none; }
      .legend-item-btn.btn:hover{ background:#f7f9fb; transform:translateY(-1px); }
      .legend-item-btn.btn:active{ transform:translateY(0); }

      /* pas d'anneau de focus */
      .legend-item-btn.btn:focus,
      .legend-item-btn.btn:focus-visible,
      .legend-item-btn.btn:-moz-focusring{ outline:0 !important; box-shadow:none !important; }
      .legend-item-btn.btn::-webkit-focus-inner{ border:0; }

      /* Ripple items */
      .legend-item-btn.btn::after{
        content:''; position:absolute; left:50%; top:50%;
        width:0; height:0; border-radius:999px; background:rgba(0,0,0,.10);
        transform:translate(-50%,-50%); opacity:0;
      }
      .legend-item-btn.btn:active::after{ animation:ripple .50s ease-out; }

      /* Texte item (évite le conflit Bootstrap .label) */
      .legend-item-text{ flex:1; color:#2f3640; font-weight:500; letter-spacing:.2px; }

      /* Icône à gauche (contenant) */
      .mark{
        width:20px; height:20px; border:1px solid #bfc7d2; border-radius:5px;
        display:flex; align-items:center; justify-content:center;
        background:#fff; color:#6b7684; font-size:11px; flex:0 0 20px;
      }

      /* ---- Footer ---- */
      .legend-footer{
        display:flex; align-items:center; justify-content:space-between;
        padding:12px; gap:12px;
      }
      .footer-btn.btn{
        all:unset; display:flex; align-items:center; justify-content:space-between;
        gap:12px; width:100%; cursor:pointer; user-select:none; color:#5b6672; font-size:14px;
        position:relative; overflow:hidden;
        transition:transform .06s ease, background .2s ease, box-shadow .2s ease;
        -webkit-tap-highlight-color:transparent; outline:none !important; box-shadow:none !important;
      }
      .footer-btn:hover{ transform:translateY(-1px); }
      .footer-btn:active{ transform:translateY(0); }
      .footer-btn:focus,
      .footer-btn:focus-visible,
      .footer-btn:-moz-focusring{ outline:0 !important; box-shadow:none !important; }
      .footer-btn::-webkit-focus-inner{ border:0; }

      .footer-btn::after{
        content:''; position:absolute; left:50%; top:50%;
        width:0; height:0; border-radius:999px; background:rgba(0,0,0,.10);
        transform:translate(-50%,-50%); opacity:0;
      }
      .footer-btn:active::after{ animation:ripple .50s ease-out; }

      .plus-btn{
        width:32px; height:32px; border-radius:8px; border:1px solid #d0d6de; background:#ffffff;
        display:flex; align-items:center; justify-content:center;
        box-shadow:0 2px 6px rgba(0,0,0,.05);
      }

" # 🔚

# ℹ️ SCRIPT =============================
script_js <- "
document.addEventListener('DOMContentLoaded', function(){
  const hero = document.getElementById('sirius-hero');
  const btn  = document.getElementById('collapseBtn');
  if(!hero || !btn) return;

  let manual = false; // si l'utilisateur force l'état on n'écrase pas avec le scroll

  function setCollapsed(state){
    hero.classList.toggle('collapsed', state);
    btn.setAttribute('aria-pressed', state ? 'true' : 'false');
  }
  function onScroll(){
    if (manual) return;
    const y = window.scrollY || document.documentElement.scrollTop;
    hero.classList.toggle('scrolled', y > 2);
    setCollapsed(y > 80); // se replie dès qu'on a un peu scrollé
  }

  btn.addEventListener('click', function(){
    manual = true;
    setCollapsed(!hero.classList.contains('collapsed'));
    hero.classList.add('scrolled');
  });

  window.addEventListener('scroll', onScroll, {passive:true});
  // init
  onScroll();
});
" # 🔚

# ️⭕️ USER INTERFACE =====================
# ------------------------------
# UI — Clean, translated, and reorganized
# Notes
# - English comments/labels; French comments removed or translated
# - Safer quoting (HTML snippets use single quotes inside HTML())
# - Accessibility: aria-labels, roles, titles added where relevant
# - IDs aligned with server rewrite (e.g., randomPed, select_list)
# - Keeps your existing CSS/JS via styles_css and script_js objects
# ------------------------------

ui <- fluidPage(
  useShinyjs(),

  # Head: global CSS and JS
  tags$head(
    tags$style(HTML(styles_css)),
    tags$script(HTML(script_js))
  ),

  # ===================== HERO (collapsible) =====================
  tags$section(
    id = "sirius-hero",
    class = "sirius-hero",

    # Collapse/expand toggle button
    tags$button(
      id = "collapseBtn", class = "collapse-toggle", `aria-label` = "Toggle header",
      HTML('<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="6 9 12 15 18 9"></polyline></svg>')
    ),

    # Hero content
    div(
      class = "hero-container",
      div(
        class = "hero-stack",
        div(class = "hero-eyebrow", "Get started"),
        div(class = "hero-title", "Genetic Pedigree"),
        div(
          class = "hero-subtitle",
          "Visualize, create and analyze family relationships with powerful pedigree tools."
        )
      ),

      # Three hero call-to-actions
      div(
        class = "hero-ctas",
        actionButton(
          inputId = "create",
          class = "btn-pill btn-dark",
          label = tagList(
            div(class = "card-ico", img(src = "attribution-pen.svg", alt = "Create new pedigree")),
            div(class = "card-title smallcaps", "Create"),
            div(class = "card-desc", "Start a new pedigree from scratch or using a template.")
          )
        ),
        actionButton(
          inputId = "select_list",
          class = "btn-pill btn-dark",
          label = tagList(
            div(class = "card-ico", img(src = "users-medical.svg", alt = "Select an existing pedigree")),
            div(class = "card-title smallcaps", "Select"),
            div(class = "card-desc", "Open an existing pedigree from your workspace.")
          )
        ),
        actionButton(
          inputId = "randomPed",
          class = "btn-pill btn-dark",
          label = tagList(
            div(class = "card-ico", img(src = "dice-five-light.svg", alt = "Random pedigree")),
            div(class = "card-title smallcaps", "Random"),
            div(class = "card-desc", "Generate a sample pedigree for quick demos and tests.")
          )
        )
      )
    )
  ),

  # ===================== DEMO CONTENT (for scroll behavior) =====================
  div(
    class = "demo-content",
    id = "center_col",
    img(src = "dna_svglogo.svg", alt = "DNA logo"),
    h1(style = "font-size:35px; letter-spacing:.3px; margin: 0 0 4px 0;", "GENETIC PEDIGREE"),
    h3(uiOutput("hero_title")),
    p("Draw Pedigree."),
    div(class = "demo-block"),

    # Round buttons menu (duplicated quick actions)
    div(
      class = "btn_group_menu",
      div(
        class = "btn-wrapper",
        actionButton(
          inputId = "create222",
          label   = HTML('<i class="bi bi-pencil-square icon" aria-hidden="true"></i>'),
          class   = "btn",
          title   = "New pedigree"
        ),
        div(class = "tab-label", "New")
      ),
      div(
        class = "btn-wrapper",
        actionButton(
          inputId = "select_list222",
          label   = HTML('<i class="bi bi-diagram-3-fill icon" aria-hidden="true"></i>'),
          class   = "btn",
          title   = "Select pedigree"
        ),
        div(class = "tab-label", "Select")
      ),
      div(
        class = "btn-wrapper",
        actionButton(
          inputId = "randomPed222",
          label   = HTML('<i class="bi bi-dice-5 icon" aria-hidden="true"></i>'),
          class   = "btn",
          title   = "Random pedigree"
        ),
        div(class = "tab-label", "Random")
      )
    )
  ),
  hr(),

  # ===================== BOTTOM TOOLBAR =====================
  div(
    class = "toolbar-outer",
    div(
      class = "toolbar-glass smallcaps",

      # Left brand and label
      div(
        class = "toolbar-inner",
        div(img(src = "dna-light.svg", class = "logo_DNA", alt = "DNA icon")),
        span(class = "toolbar-label", "PEDIGREE TITLE")
      ),
      div(class = "v-sep"),

      # Editable title field
      textInput(
        inputId = "plotTitle",
        label = NULL,
        value = "",
        width = "100%",
        placeholder = "Enter a pedigree title here"
      ),

      # Clear button (no server handler wired in this snippet)
      actionButton(
        inputId = "clear",
        label = HTML('<i class="bi bi-trash3" aria-hidden="true"></i>'),
        class = "icon-btn btn danger",
        title = "Delete",
        `data-toggle` = "tooltip",
        `data-placement` = "bottom"
      )
    )
  ),

  # ===================== MAIN CONTENT — 3 CARDS LAYOUT =====================
  div(
    class = "main-split",

    # ---------- (A) Legend & Actions ----------
    wellPanel(
      class = "card",
      div(class = "card-header", "LEGEND & ACTIONS"),
      div(
        class = "card-body",
        style = "display:flex;flex-direction:column;gap:8px;",
        h5("Gender"),
        actionButton("Male", "Male"),
        actionButton("Female", "Female"),
        actionButton("Unknown", "Unknown"),
        h5("Traits & Status"),
        actionButton("Adopted", "Adopted"),
        actionButton("Carrier", "Carrier"),
        actionButton("Proband", "Proband"),
        actionButton("Deceased", "Deceased"),
        actionButton("Miscarriage", "Miscarriage"),
        actionButton("Starred", "Starred"),
        hr(),
        h5("Custom phenotypes"),
        actionButton("newPheno", "Create phenotype"),
        div(uiOutput("phenoButtonsUI"))
      )
    ),

    # ---------- (B) Canvas + Table ----------
    div(
      class = "card",
      div(class = "card-header", "Canvas"),
      div(
        class = "card-body",
        plotOutput("plot", height = "650px", click = "ped_click", dblclick = "ped_dblclick"),
        div(
          class = "hr-slot",
          a(
            id = "toggleAdvanced",
            class = "hr-chip",
            "Show/hide Table info",
            href = "javascript:void(0)",
            role = "button",
            `aria-controls` = "advanced",
            `aria-expanded` = "false"
          )
        ),
        div(
          id = "advanced",
          class = "container-page",
          # DataTable now includes expandable row-details (parents_/sib_/children_)
          DTOutput("pedTable", width = "100%")
        )
      )
    ),

    # ---------- (C) Data & Stats ----------
    div(
      class = "card",
      div(
        class = "card-header gm-card__header",
        div(
          class = "gm-avatar",
          HTML('<span class="material-symbols-outlined">person</span>')
        ),
        div(
          class = "gm-title-wrap",
          div(class = "gm-card__title", textOutput("selectedIndividual", inline = TRUE)),
          div(class = "gm-card__subtitle", "Manage family relations for the selected person")
        )
      ),
      div(
        class = "card-body",
        tabsetPanel(
          id = "tabset", type = "tabs",
          tabPanel(
            title = "FAMILY",
            br(),
            h5("Parents"),
            actionButton("addparents", "Add parents"),
            h5("Siblings"),
            actionButton("sister", "Add sister"),
            actionButton("brother", "Add brother"),
            actionButton("sib_unknown", "Undefined sib"),
            tags$span(" "),
            actionButton("Twins", "Add twins"),
            actionButton("Triplets", "Add triplets"),
            actionButton("half", "Add half sib"),
            h5("Children"),
            div(
              class = "mb-2",
              actionButton("child_daughter", "Add daughter"),
              actionButton("child_son", "Add son"),
              actionButton("child_unknown", "Add undefined child")
            ),
            div(
              class = "mb-2",
              actionButton("add_Miscarriage", "Add miscarriage"),
              actionButton("choose_partner", "Add partner")
            )
          ),
          tabPanel(
            title = "MATRIX",
            br(),
            uiOutput("matrix_header"),
            div(class = "hr-slot"),
            fluidRow(
              column(4, uiOutput("matrix_parents")),
              column(4, uiOutput("matrix_sibs")),
              column(4, uiOutput("matrix_children"))
            )
          )
        )
      )
    )
  )
)


# ⭕️ SERVER =============================
# ------------------------------
# SERVER — Clean, translated, and reorganized
# Notes
# - English labels, comments, and messages
# - Fixed a few issues:
#   * Use input$plotTitle (was input$title in one place)
#   * Use input$randomPed consistently (removed input$randomped typo)
#   * Removed duplicated reactiveValues(selectedIndiv)
#   * Safer ordering logic + clearer helpers
# - Relies on your existing utilities defined above (e.g., addChildWithPartner, add_half_sibling, getPartner, etc.)
# ------------------------------

server <- function(input, output, session) {
  # ===== MATRIX (read-only) helpers =====

  niceLabel <- function(id) {
    if (is.null(pedigree$ped) || !nzchar(id)) {
      return(id)
    }
    fn <- ln <- ""
    if (!is.null(values$pedData)) {
      i <- match(id, values$pedData$id)
      if (!is.na(i)) {
        fn <- values$pedData$first_name[i] %||% ""
        ln <- values$pedData$last_name[i] %||% ""
      }
    }
    sx <- pedtools::getSex(pedigree$ped, id)
    sym <- if (is.na(sx) || sx == 0) "?" else if (sx == 1) "♂" else "♀"
    full <- trimws(paste(fn, ln))
    paste0(id, if (nzchar(full)) paste0(" — ", full) else "", " ", sym)
  }

  getParentsVec <- function(id) {
    fa <- pedtools::father(pedigree$ped, id, internal = FALSE)
    mo <- pedtools::mother(pedigree$ped, id, internal = FALSE)
    v <- c(fa, mo)
    v[!is.na(v) & nzchar(v)]
  }

  getFullSibsVec <- function(id) {
    fa <- pedtools::father(pedigree$ped, id, internal = FALSE)
    mo <- pedtools::mother(pedigree$ped, id, internal = FALSE)
    if (is.na(fa) || !nzchar(fa) || is.na(mo) || !nzchar(mo)) {
      return(character(0))
    }
    s1 <- pedtools::children(pedigree$ped, fa, internal = FALSE)
    s2 <- pedtools::children(pedigree$ped, mo, internal = FALSE)
    setdiff(intersect(s1, s2), id)
  }

  getChildrenVec <- function(id) {
    kids <- pedtools::children(pedigree$ped, id, internal = FALSE)
    unique(kids[!is.na(kids) & nzchar(kids)])
  }

  # ------------------ Reactive stores (global state) ------------------
  pedigree <- reactiveValues(ped = NULL, twins = NULL, miscarriage = NULL, title = "")
  styles <- reactiveValues(
    hatched = NULL, carrier = NULL, Deceased = NULL,
    proband = NULL, adopted = NULL, aff = NULL,
    starred = NULL, title = NULL, fill = NULL,
    dashed = NULL
  )
  sel <- reactiveVal(character(0)) # currently selected label
  textAnnot <- reactiveVal(NULL) # optional text annotations (not used yet)
  # Phenotypes (specs + assignations)
  phenotypes <- reactiveValues(list = list(), assign = list())
  pheno_editing <- reactiveVal(NULL)
  # Table-oriented state
  values <- reactiveValues(pedData = NULL)

  # Selected individual (row + index in values$pedData)
  selectedIndiv <- reactiveValues(row = NULL, index = NULL)

  # Persisted per-individual orders for sortable widgets
  siblingsOrder <- reactiveValues()
  parentsOrder <- reactiveValues()
  childrenOrder <- reactiveValues()

  # Cache for the three sortable panels in row-details
  moveCache <- reactiveValues()

  phenotypes <- reactiveValues(list = list())
  modalData <- reactiveValues(row = NULL, index = NULL)
  modalInput <- reactiveValues(dob = NULL, dod = NULL, deceased = FALSE)
  # Bracket drawing params (for Adopted stylings)
  bracketParams <- list(
    scale_cex = 1, col = "red", lwd = 2, offsetX = -0.02, offsetY = 0,
    gap_factor = 0.28, radius_factor = 0.18, vertical_factor = 1
  )

  # ------------------ Small helpers ------------------
  `%||%` <- function(x, y) if (is.null(x)) y else x
  # Toggle Advanced
  shinyjs::onclick("toggleAdvanced", {
    shinyjs::toggle(id = "advanced", anim = TRUE)
    runjs("
      const t = document.getElementById('toggleAdvanced');
      const expanded = t.getAttribute('aria-expanded') === 'true';
      t.setAttribute('aria-expanded', (!expanded).toString());
    ")
  })
  sanitizeTitle <- function(txt) {
    if (is.null(txt) || !nzchar(txt)) {
      return("")
    }
    txt <- substr(txt, 1, 60)
    clean <- gsub("[^a-zA-Z0-9 .,'\\-_:()/]", "", txt)
    clean <- gsub("\\s+", " ", clean)
    clean <- trimws(clean)
    if (nchar(txt) >= 60) showNotification("Title is too long (max 60 characters). Truncated.", type = "warning")
    clean
  }

  # Compute automatic layout width from available pixels
  auto_width_cols <- function(x, plot_px, slot_px) {
    gens <- pedtools::generations(x)
    max(max(table(gens)), max(1, floor(plot_px / max(1, slot_px))))
  }

  plotWidthPx <- reactive({
    session$clientData$output_plot_width %||% 800
  })

  safeDom <- function(x) paste0("row_", gsub("[^A-Za-z0-9_-]", "_", x))
  makePedData <- function(ped) {
    if (is.null(ped)) {
      return(NULL)
    }
    df <- as.data.frame(ped, stringsAsFactors = FALSE)
    extra_cols <- c("first_name", "last_name", "date_of_birth", "Deceased", "date_of_death", "age", "comments")
    for (col in extra_cols) if (!(col %in% names(df))) df[[col]] <- if (col == "Deceased") FALSE else ""
    df
  }



  setPedigree <- function(newped, newtitle = NULL) {
    safe_ped <- tryCatch(pedtools::relabel(newped, new = "generations"), error = function(e) NULL)
    if (is.null(safe_ped)) {
      showNotification("Pedigree relabeling failed.", type = "error")
      return(invisible(FALSE))
    }
    pedigree$ped <- safe_ped
    pedigree$title <- sanitizeTitle(newtitle %||% pedigree$title %||% "")
    updateTextInput(session, "plotTitle", value = pedigree$title)
    updatePedData()
    invisible(TRUE)
  }

  # ------------------ Hero subtitle text ------------------
  output$hero_title <- renderText({
    "Build and explore complex family trees interactively."
  })

  # ------------------ Plot building blocks ------------------
  plotAlignment <- reactive({
    req(pedigree$ped)
    w <- if (ANTI_DEF_WIDTH > 0) ANTI_DEF_WIDTH else auto_width_cols(pedigree$ped, plotWidthPx(), ANTI_DEF_SLOT_PX)
    pedtools:::.pedAlignment(
      pedigree$ped,
      proband = styles$proband,
      twins = pedigree$twins,
      miscarriage = pedigree$miscarriage,
      arrows = FALSE,
      align = c(1.5, 2),
      packed = TRUE,
      width = w,
      straight = FALSE
    )
  })

  plotAnnotation <- reactive({
    req(pedigree$ped)
    labs <- labels(pedigree$ped)
    names(labs) <- labs
    annot <- .pedAnnotation(
      pedigree$ped,
      labs = labs,
      hatched = styles$hatched, hatchDensity = 20,
      carrier = styles$carrier, proband = styles$proband,
      Deceased = styles$Deceased, starred = styles$starred,
      title = if (nzchar(input$plotTitle)) input$plotTitle else NULL,
      textAnnot = if (!is.null(textAnnot())) lapply(textAnnot(), function(b) list(b, cex = 1.2, font = 2, col = "blue")) else NULL,
      col = list("#3c8dbc" = sel()),
      fill = if (length(styles$fill) > 0) unlist(styles$fill) else NA,
      lty = list(dashed = styles$dashed),
      lwd = list(`3` = sel(), `1.5` = setdiff(styles$dashed, sel()))
    )

    if (!is.null(values$pedData)) {
      pedData <- values$pedData
      ids <- labels(pedigree$ped)
      for (i in seq_along(ids)) {
        id <- ids[i]
        text_parts <- c(as.character(id))
        first_name <- if (!is.null(pedData$first_name) && length(pedData$first_name) >= i && !is.na(pedData$first_name[i])) pedData$first_name[i] else ""
        last_name <- if (!is.null(pedData$last_name) && length(pedData$last_name) >= i && !is.na(pedData$last_name[i])) pedData$last_name[i] else ""
        fullname <- trimws(paste(first_name, last_name))
        if (nzchar(fullname)) text_parts <- c(text_parts, fullname)
        age <- if (!is.null(pedData$age) && length(pedData$age) >= i && !is.na(pedData$age[i])) pedData$age[i] else ""
        if (nzchar(age)) text_parts <- c(text_parts, paste0("(", age, ")"))
        Deceased <- if (!is.null(pedData$Deceased) && length(pedData$Deceased) >= i) pedData$Deceased[i] else FALSE
        if (isTRUE(Deceased) || identical(Deceased, "✝️")) text_parts <- c(text_parts, "†")
        comments <- if (!is.null(pedData$comments) && length(pedData$comments) >= i && !is.na(pedData$comments[i])) pedData$comments[i] else ""
        if (nzchar(comments)) text_parts <- c(text_parts, comments)
        if (length(text_parts) > 0) annot$textUnder[[as.character(id)]] <- paste(text_parts, collapse = "\n")
      }
    }
    annot
  })

  plotScaling <- reactive({
    req(pedigree$ped)
    pedtools:::.pedScaling(plotAlignment(), plotAnnotation(),
      cex = 1.2, symbolsize = 1, margins = 1,
      autoScale = TRUE, minsize = 0.15
    )
  })

  positionDf <- reactive({
    req(pedigree$ped)
    al <- plotAlignment()
    sc <- plotScaling()
    data.frame(
      id_plot = al$plotord,
      xc = al$xall + sc$boxw / 2,
      yc = al$yall + sc$boxh / 2,
      boxw = sc$boxw, boxh = sc$boxh
    )
  })

  output$plot <- renderPlot({
    req(pedigree$ped)
    align <- plotAlignment()
    annot <- plotAnnotation()
    sc <- plotScaling()
    pedtools:::.drawPed(align, annotation = annot, scaling = sc)
    pedtools:::.annotatePed(align, annotation = annot, scaling = sc)

    # Draw adopted rounded brackets around individuals flagged as adopted
    ids_to_bracket <- intersect(styles$adopted %||% character(0), labels(pedigree$ped))
    if (length(ids_to_bracket)) {
      al <- align
      df <- positionDf()
      plot_row <- match(match(ids_to_bracket, labels(pedigree$ped)), al$plotord)

      draw_arc <- function(cx, cy, r, from, to, n = 20, col = "red", lwd = 2) {
        ang <- seq(from, to, length.out = max(2L, n))
        lines(cx + r * cos(ang), cy + r * sin(ang), col = col, lwd = lwd, xpd = NA)
      }
      draw_rounded_brackets <- function(xc, yc, bw, bh, p = bracketParams) {
        gap <- p$gap_factor * bw * p$scale_cex
        r <- min(p$radius_factor * min(bw, bh) * p$scale_cex, (bh * 0.65))
        xL <- (xc + p$offsetX) - (bw / 2 + gap)
        xR <- (xc + p$offsetX) + (bw / 2 + gap)
        yB <- (yc + p$offsetY) - (bh / 2 + gap)
        yT <- (yc + p$offsetY) + (bh / 2 + gap)
        midY <- (yB + yT) / 2
        halfH <- (yT - yB) / 2 * p$vertical_factor
        y1 <- midY - halfH + r
        y2 <- midY + halfH - r
        segments(xL, y1, xL, y2, col = p$col, lwd = p$lwd, xpd = NA)
        segments(xR, y1, xR, y2, col = p$col, lwd = p$lwd, xpd = NA)
        draw_arc(xL + r, y2, r, pi, pi / 2, col = p$col, lwd = p$lwd)
        draw_arc(xL + r, y1, r, -pi / 2, -pi, col = p$col, lwd = p$lwd)
        draw_arc(xR - r, y2, r, 0, pi / 2, col = p$col, lwd = p$lwd)
        draw_arc(xR - r, y1, r, 0, -pi / 2, col = p$col, lwd = p$lwd)
      }
      for (k in which(!is.na(plot_row))) {
        r <- df[plot_row[k], ]
        draw_rounded_brackets(r$xc, r$yc, r$boxw, r$boxh)
      }
    }
  })

  # ------------------ Click selection sync (plot -> table) ------------------
  observeEvent(input$ped_click, {
    req(pedigree$ped)
    al <- plotAlignment()
    sc <- plotScaling()
    centers <- data.frame(x = al$xall + sc$boxw / 2, y = al$yall + sc$boxh / 2, id_plot = al$plotord)
    hit <- nearPoints(centers, input$ped_click, xvar = "x", yvar = "y", threshold = 20, maxpoints = 1)$id_plot
    req(length(hit) == 1)
    lab <- labels(pedigree$ped)[al$plotord[al$plotord == hit]]
    if (identical(sel(), lab)) {
      sel("")
      selectedIndiv$row <- NULL
      selectedIndiv$index <- NULL
    } else {
      sel(lab)
      i <- match(lab, values$pedData$id)
      if (!is.na(i)) {
        selectedIndiv$row <- values$pedData[i, ]
        selectedIndiv$index <- i
      }
    }
  })

  # ------------------ Random / predefined pedigree loaders ------------------
  # Random (modal preview + confirm)
  randomVars <- reactiveValues(ped = NULL)

  observeEvent(input$randomPed, { # <- unified id
    ped <- NULL
    for (tries in 1:5) {
      ped <- tryCatch(pedtools::randomPed(n = sample(5:10, 1), founders = sample(1:3, 1)), error = function(e) NULL)
      if (!is.null(ped)) break
    }
    if (is.null(ped)) {
      showNotification("Random pedigree generation failed.", type = "error")
      return()
    }
    randomVars$ped <- ped
    showModal(createRandomPedigreeModal())
  })

  output$previewPlot <- renderPlot({
    req(randomVars$ped)
    preview_ped <- tryCatch(pedtools::relabel(randomVars$ped, new = "generations"), error = function(e) NULL)
    req(preview_ped)
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mar = c(1, 1, 2, 1))
    plot(preview_ped, cex = 1.2)
    safe_title <- sanitizeTitle(input$randomPedTitle)
    if (nzchar(safe_title)) title(main = safe_title, cex.main = 1.1, col.main = "#3c8dbc")
  })

  observeEvent(input$confirmPed, {
    req(randomVars$ped)
    safe_title <- sanitizeTitle(input$randomPedTitle)
    if (isTRUE(setPedigree(randomVars$ped, safe_title))) {
      removeModal()
      sel(character(0))
      selectedIndiv$row <- NULL
      selectedIndiv$index <- NULL
    }
  })

  # Predefined models (modal with live preview)
  modalVars <- reactiveValues(previewPed = NULL)

  observeEvent(input$modalPedChoice, {
    req(input$modalPedChoice != "")
    modalVars$pedChoice <- input$modalPedChoice
    pedigree_fun <- pedigree_list[[input$modalPedChoice]]
    if (!is.null(pedigree_fun) && is.function(pedigree_fun)) {
      preview <- tryCatch(pedigree_fun(), error = function(e) NULL)
      if (is.null(preview)) showNotification("Error generating preview.", type = "error")
      modalVars$previewPed <- preview
    } else {
      showNotification("Unknown pedigree model selected.", type = "error")
      modalVars$previewPed <- NULL
    }
  })

  observeEvent(input$select_list, {
    resetModalVars()
    showModal(createPedigreeModal())
  })

  output$modalPreviewPed <- renderPlot({
    req(modalVars$previewPed)
    preview_ped <- tryCatch(relabel(modalVars$previewPed, new = "generations"), error = function(e) NULL)
    if (is.null(preview_ped)) {
      return()
    }
    par(mar = c(1, 1, 2, 1))
    plot(preview_ped, cex = 1.2)
    safe_title <- sanitizeTitle(input$modalPedTitle)
    if (nzchar(safe_title)) title(main = safe_title, cex.main = 1.1, col.main = "#3c8dbc")
  })


  observeEvent(input$modalConfirmPed, {
    req(modalVars$previewPed)
    safe_title <- sanitizeTitle(input$modalPedTitle)
    setPedigree(modalVars$previewPed, safe_title)
    removeModal()
    sel(character(0))
    selectedIndiv$row <- NULL
    selectedIndiv$index <- NULL
  })

  resetModalVars <- function() {
    modalVars$previewPed <- NULL
    modalVars$pedChoice <- ""
    modalVars$pedTitle <- ""
  }
  resetRandomVars <- function() {
    randomVars$ped <- NULL
    randomVars$title <- NULL
  }

  # ------------------ Trait / status toggles ------------------
  observeEvent(input$Proband, {
    req(selectedIndiv$row)
    id <- as.character(selectedIndiv$row$id)
    styles$proband <- if (identical(styles$proband, id)) NULL else id
  })
  observeEvent(input$Carrier, {
    req(selectedIndiv$row)
    id <- as.character(selectedIndiv$row$id)
    styles$carrier <- if (id %in% (styles$carrier %||% "")) setdiff(styles$carrier, id) else union(styles$carrier, id)
  })
  observeEvent(input$Starred, {
    req(selectedIndiv$row)
    id <- as.character(selectedIndiv$row$id)
    styles$starred <- if (id %in% (styles$starred %||% "")) setdiff(styles$starred, id) else union(styles$starred, id)
  })
  observeEvent(input$Adopted, {
    req(selectedIndiv$row)
    id <- as.character(selectedIndiv$row$id)
    styles$adopted <- if (id %in% (styles$adopted %||% "")) setdiff(styles$adopted, id) else union(styles$adopted, id)
  })
  observeEvent(input$Miscarriage, {
    req(selectedIndiv$row)
    id <- as.character(selectedIndiv$row$id)
    pedigree$miscarriage <- if (id %in% (pedigree$miscarriage %||% "")) setdiff(pedigree$miscarriage, id) else union(pedigree$miscarriage, id)
  })

  observeEvent(input$Deceased, {
    req(selectedIndiv$row, values$pedData)
    id <- as.character(selectedIndiv$row$id)
    i <- match(id, values$pedData$id)
    if (!is.na(i)) {
      values$pedData$Deceased[i] <- !isTRUE(values$pedData$Deceased[i])
      styles$Deceased <- if (values$pedData$Deceased[i]) union(styles$Deceased, id) else setdiff(styles$Deceased, id)
    }
  })

  # ------------------ Sex change ------------------
  changeSexObs <- function(sexVal) {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    p <- tryCatch(pedtools::setSex(pedigree$ped, ids = id, sex = sexVal), error = function(e) {
      showNotification(e$message, type = "error")
      NULL
    })
    if (!is.null(p)) {
      pedigree$ped <- p
      values$pedData$sex[values$pedData$id == id] <- sexVal
    }
  }
  observeEvent(input$Male, {
    changeSexObs(1)
  })
  observeEvent(input$Female, {
    changeSexObs(2)
  })
  observeEvent(input$Unknown, {
    changeSexObs(0)
  })

  # ------------------ Half-sibling / miscarriage / quick children ------------------
  observeEvent(input$half, {
    req(pedigree$ped, selectedIndiv$row)
    id <- as.character(selectedIndiv$row$id)
    f_id <- pedtools::father(pedigree$ped, id, internal = FALSE)
    m_id <- pedtools::mother(pedigree$ped, id, internal = FALSE)
    choices <- c()
    if (!is.na(f_id) && nzchar(f_id)) choices <- c(choices, "Paternal (share father)" = "paternal")
    if (!is.na(m_id) && nzchar(m_id)) choices <- c(choices, "Maternal (share mother)" = "maternal")
    if (!length(choices)) {
      showNotification("Cannot add a half-sibling: no known parent.", type = "error")
      return()
    }
    showModal(modalDialog(
      title = sprintf("Add a half-sibling to %s", id),
      selectInput("half_side", "Side:", choices = choices),
      selectInput("half_sex", "Child sex:", choices = c("♂ Male" = 1, "♀ Female" = 2, "Unknown" = 0), selected = 0),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_add_half", "Add", class = "btn btn-primary")),
      easyClose = TRUE
    ))
  })
  output$matrix_header <- renderUI({
    if (is.null(pedigree$ped)) {
      return(div(class = "text-muted", "Charge ou crée un pedigree pour utiliser la matrice."))
    }
    id <- sel()
    if (!nzchar(id)) {
      return(div(class = "text-muted", "Sélectionne une personne (clic sur le graphe ou “Select” dans la table)."))
    }
    div(HTML(sprintf("<b>Individu sélectionné :</b> %s", htmltools::htmlEscape(niceLabel(id)))))
  })

  output$matrix_parents <- renderUI({
    req(pedigree$ped)
    id <- sel()
    req(nzchar(id))
    v <- getParentsVec(id)
    wellPanel(
      h5("Parents"),
      if (!length(v)) {
        div(class = "text-muted", "Aucun parent connu.")
      } else {
        tags$ul(lapply(v, function(x) tags$li(niceLabel(x))))
      }
    )
  })

  output$matrix_sibs <- renderUI({
    req(pedigree$ped)
    id <- sel()
    req(nzchar(id))
    v <- getFullSibsVec(id)
    wellPanel(
      h5("Fratrie"),
      if (!length(v)) {
        div(class = "text-muted", "Pas de frère/soeur (mêmes deux parents).")
      } else {
        tags$ul(lapply(v, function(x) tags$li(niceLabel(x))))
      }
    )
  })

  output$matrix_children <- renderUI({
    req(pedigree$ped)
    id <- sel()
    req(nzchar(id))
    v <- getChildrenVec(id)
    wellPanel(
      h5("Enfants"),
      if (!length(v)) {
        div(class = "text-muted", "Aucun enfant.")
      } else {
        tags$ul(lapply(v, function(x) tags$li(niceLabel(x))))
      }
    )
  })

  observeEvent(input$confirm_add_half, {
    req(pedigree$ped, selectedIndiv$row, input$half_side, input$half_sex)
    removeModal()
    id <- as.character(selectedIndiv$row$id)
    newped <- tryCatch(
      add_half_sibling(pedigree$ped, anchor_id = id, side = as.character(input$half_side), childSex = as.integer(input$half_sex)),
      error = function(e) {
        showNotification(e$message, type = "error")
        NULL
      }
    )
    if (!is.null(newped)) {
      pedigree$ped <- pedtools::relabel(newped, new = "generations")
      updatePedData()
    }
  })

  observeEvent(input$add_Miscarriage, {
    req(pedigree$ped, selectedIndiv$row)
    parent_id <- as.character(selectedIndiv$row$id)
    psex <- pedtools::getSex(pedigree$ped, parent_id)
    if (is.na(psex) || psex == 0) {
      showNotification("Unknown parent sex.", type = "error")
      return()
    }
    kids_before <- pedtools::children(pedigree$ped, parent_id, internal = FALSE)
    tryCatch(
      {
        partner <- getPartner(pedigree$ped, parent_id)
        newped <- addChildWithPartner(pedigree$ped, parent_id, partner = partner, childSex = 0)
        pedigree$ped <- pedtools::relabel(newped, new = "generations")
        updatePedData()
        kids_after <- pedtools::children(pedigree$ped, parent_id, internal = FALSE)
        new_child <- setdiff(kids_after, kids_before)
        if (length(new_child) == 1) pedigree$miscarriage <- union(pedigree$miscarriage %||% character(0), new_child)
      },
      error = function(e) showModal(modalDialog(title = "Error", e$message, easyClose = TRUE))
    )
  })

  addChildQuick <- function(sexVal) {
    req(pedigree$ped, selectedIndiv$row)
    id <- as.character(selectedIndiv$row$id)
    partner <- getPartner(pedigree$ped, id)
    updatePed <- tryCatch(addChildWithPartner(pedigree$ped, id, partner, childSex = sexVal), error = function(e) {
      showNotification(e$message, type = "error")
      NULL
    })
    if (!is.null(updatePed)) {
      pedigree$ped <- pedtools::relabel(updatePed, new = "generations")
      updatePedData()
    }
  }
  observeEvent(input$child_son, {
    addChildQuick(1)
  })
  observeEvent(input$child_daughter, {
    addChildQuick(2)
  })
  observeEvent(input$child_unknown, {
    addChildQuick(0)
  })
  updatePedData <- function() {
    if (is.null(pedigree$ped)) {
      values$pedData <- NULL
    } else {
      # ⛔️ IMPORTANT : supprime le petit bloc “perdu” qui écrasait pedData
      #   values$pedData <- data.frame(id = labels(ped), ...)
      # Il ne doit plus exister.
      values$pedData <- buildPedTableData(pedigree$ped, old = isolate(values$pedData))
    }
  }

  # ------------------ Data for the (live) table ------------------
  # --- Helpers -----------------------------------------------------------------

  chr1 <- function(x) {
    if (length(x) == 0) {
      return("")
    }
    x <- x[1]
    ifelse(is.na(x), "", as.character(x))
  }

  sex_to_chr <- function(ped, ids) {
    vapply(ids, function(id) {
      s <- pedtools::getSex(ped, id)
      if (is.na(s) || s == 0) "Unknown" else if (s == 1) "Male" else "Female"
    }, character(1))
  }

  # Fabrique les boutons d'action HTML par ligne (id lisible via data-id)
  make_action_buttons <- function(ids) {
    select_btn <- sprintf(
      '<button class="btn btn-success btn-xs ped-select" data-id="%s">Select</button>', ids
    )
    delete_btn <- sprintf(
      '<button class="btn btn-danger  btn-xs ped-delete" data-id="%s">Delete</button>', ids
    )
    list(select = select_btn, delete = delete_btn)
  }

  # --- Données pour DT ---------------------------------------------------------

  ped_df <- reactive({
    req(pedigree$ped)
    base <- values$pedData
    if (is.null(base)) base <- buildPedTableData(pedigree$ped)

    # Ajoute les boutons
    btns <- make_action_buttons(base$id)
    base$Select <- btns$select
    base$Delete <- btns$delete

    # Réordonne exactement comme tu veux
    base[, c(
      "id", "last_name", "first_name", "Sex", "Father", "Mother",
      "Birth", "Deceased", "Death", "age", "Select", "Delete"
    )]
  })

  # --- DataTable avec boutons ---------------------------------------------------

  output$pedTable <- DT::renderDT({
    df <- ped_df()

    # colonnes actions non triables
    select_col <- which(names(df) == "Select") - 1L
    delete_col <- which(names(df) == "Delete") - 1L
    deceased_col <- which(names(df) == "Deceased") - 1L

    DT::datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("csv", "colvis"),
        pageLength = 15,
        scrollX = TRUE,
        # fixe la 1re colonne (id) si tu veux
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(
          list(targets = c(select_col, delete_col), orderable = FALSE, searchable = FALSE),
          list(targets = deceased_col, className = "dt-center")
        )
      ),
      callback = DT::JS(
        "table.on('click', 'button.ped-select', function() {",
        "  var id = $(this).data('id');",
        "  Shiny.setInputValue('ped_select', id, {priority: 'event'});",
        "});",
        "table.on('click', 'button.ped-delete', function() {",
        "  var id = $(this).data('id');",
        "  if(confirm('Delete '+id+' ?')){",
        "    Shiny.setInputValue('ped_delete', id, {priority: 'event'});",
        "  }",
        "});"
      )
    )
  })

  # --- Observers : synchronisation & actions -----------------------------------

  # Remplace l'ancien observeEvent(input$pedTable_rows_selected, ...)
  observeEvent(input$ped_select,
    {
      req(nzchar(input$ped_select))
      id <- input$ped_select
      sel(id) # votre sélecteur global pour le plot

      # Met à jour l'objet 'selectedIndiv' comme avant
      i <- match(id, values$pedData$id)
      if (!is.na(i)) {
        selectedIndiv$row <- values$pedData[i, ]
        selectedIndiv$index <- i
      }
    },
    ignoreInit = TRUE
  )

  # (optionnel) supprimer un individu quand on clique sur Delete
  observeEvent(input$ped_delete,
    {
      id <- input$ped_delete
      req(nzchar(id))
      # TODO: implémentez ici la logique métier pour retirer l'individu du pedigree
      # par ex. pedigree$ped <- pedtools::removeIndividuals(pedigree$ped, id)
      # puis rafraîchir tout ce qui dépend : values$pedData, plot, etc.
      showNotification(sprintf("Requested delete: %s", id), type = "message", duration = 3)
    },
    ignoreInit = TRUE
  )

  # Si vous devez encore refléter une sélection externe -> bouton "Select" non requis
  # mais vous pouvez garder ceci pour mettre à jour visuellement autre chose au besoin :
  # observeEvent(sel(), { ... })



  ## LEGEND =======================
  # observeEvent(input$Deceased, ignoreInit = TRUE, {
  #   req(selectedIndiv$row, pedigree$ped, values$pedData)
  #   id <- as.character(selectedIndiv$row$id)
  #   i <- which(values$pedData$id == id)
  #   if (length(i) == 1) {
  #     values$pedData$Deceased[i] <- TRUE
  #     styles$Deceased <- union(styles$Deceased, id)
  #     showNotification(paste("Individual", id, "marked as Deceased."), type = "message")
  #   } else {
  #     showNotification("Individual not found in pedData.", type = "error")
  #   }
  # })
  #
  # observeEvent(input$Proband, {
  #   id <- req(sel())
  #   styles$proband <- union(styles$proband, id)
  # })
  #
  # observeEvent(input$Carrier, ignoreInit = TRUE, {
  #   req(selectedIndiv$row, pedigree$ped)
  #   id <- as.character(selectedIndiv$row$id)
  #   if (!(id %in% labels(pedigree$ped))) {
  #     showNotification("Individual not found in the pedigree.", type = "error")
  #     return()
  #   }
  #   styles$carrier <- union(styles$carrier, id)
  #   showNotification(paste("Phenotype 'Carrier' applied to", id), type = "message")
  # })
  #
  # observeEvent(input$Miscarriage, ignoreInit = TRUE, {
  #   req(selectedIndiv$row, pedigree$ped)
  #   id <- as.character(selectedIndiv$row$id)
  #   if (!(id %in% labels(pedigree$ped))) {
  #     showNotification("Individual not found in the pedigree.", type = "error")
  #     return()
  #   }
  #   pedigree$miscarriage <- union(pedigree$miscarriage, id)
  #   showNotification(paste("Individual", id, "marked as miscarriage."), type = "message")
  # })
  #
  # # Changement de sexe
  # observeEvent(input$Male, ignoreInit = TRUE, {
  #   req(selectedIndiv$row, pedigree$ped)
  #   id_val <- as.character(selectedIndiv$row$id)
  #   new_ped <- tryCatch(changeSex(pedigree$ped, id_val, sex = 1), error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type = "error")
  #     NULL
  #   })
  #   if (!is.null(new_ped)) {
  #     pedigree$ped <- new_ped
  #     if ("sex" %in% names(values$pedData)) values$pedData[values$pedData$id == id_val, "sex"] <- 1
  #     showNotification("Gender changed to 'Male'.", type = "message")
  #   }
  # })
  # observeEvent(input$Female, ignoreInit = TRUE, {
  #   req(selectedIndiv$row, pedigree$ped)
  #   id_val <- as.character(selectedIndiv$row$id)
  #   new_ped <- tryCatch(changeSex(pedigree$ped, id_val, sex = 2), error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type = "error")
  #     NULL
  #   })
  #   if (!is.null(new_ped)) {
  #     pedigree$ped <- new_ped
  #     if ("sex" %in% names(values$pedData)) values$pedData[values$pedData$id == id_val, "sex"] <- 2
  #     showNotification("Gender changed to 'Female'.", type = "message")
  #   }
  # })
  # observeEvent(input$Unknown, ignoreInit = TRUE, {
  #   req(selectedIndiv$row, pedigree$ped)
  #   id_val <- as.character(selectedIndiv$row$id)
  #   new_ped <- tryCatch(changeSex(pedigree$ped, id_val, sex = 0), error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type = "error")
  #     NULL
  #   })
  #   if (!is.null(new_ped)) {
  #     pedigree$ped <- new_ped
  #     if ("sex" %in% names(values$pedData)) values$pedData[values$pedData$id == id_val, "sex"] <- 0
  #     showNotification("Gender has been changed to 'Unknown'.", type = "message")
  #   }
  # })
  # # --------- Boutons : ajouter / retirer les crochets ---------
  # observeEvent(input$Adopted, {
  #   ids <- sel()
  #   if (length(ids) != 1) {
  #     showNotification("Sélectionnez un seul individu à encadrer.", type = "warning")
  #     return(invisible())
  #   }
  #   current <- bracketIds()
  #   if (!(ids %in% current)) {
  #     bracketIds(union(current, ids))
  #     showNotification(sprintf("Crochets ajoutés à %s.", ids), type = "message")
  #   } else {
  #     showNotification("Cet individu a déjà des crochets.", type = "default")
  #   }
  # })
  #
  # # === utilitaire: quart d’arc (polyligne) ===
  # draw_arc <- function(cx, cy, r, from, to, n = 20, col = "red", lwd = 2) {
  #   n <- as.integer(n)
  #   if (is.na(n) || n < 2) n <- 20
  #   r <- as.numeric(r)
  #   if (!is.finite(r) || r <= 0) {
  #     return(invisible(NULL))
  #   }
  #   ang <- seq(from, to, length.out = n)
  #   lines(cx + r * cos(ang), cy + r * sin(ang), col = col, lwd = lwd, xpd = NA)
  # }
  #
  # # === crochets arrondis autour d'une boîte ===
  # draw_rounded_brackets <- function(xc, yc, bw, bh,
  #                                   scale_cex = 1, col = "red", lwd = 2,
  #                                   offsetX = -0.025, offsetY = 0,
  #                                   gap_factor = 0.30, radius_factor = 0.18, vertical_factor = 1) {
  #   gap <- gap_factor * bw * scale_cex
  #   r <- min(radius_factor * min(bw, bh) * scale_cex, (bh * 0.65))
  #   xL <- (xc + offsetX) - (bw / 2 + gap)
  #   xR <- (xc + offsetX) + (bw / 2 + gap)
  #   yB <- (yc + offsetY) - (bh / 2 + gap)
  #   yT <- (yc + offsetY) + (bh / 2 + gap)
  #   midY <- (yB + yT) / 2
  #   halfH <- (yT - yB) / 2 * vertical_factor
  #   y1 <- midY - halfH + r
  #   y2 <- midY + halfH - r
  #   segments(xL, y1, xL, y2, col = col, lwd = lwd, xpd = NA)
  #   segments(xR, y1, xR, y2, col = col, lwd = lwd, xpd = NA)
  #   draw_arc(cx = xL + r, cy = y2, r = r, from = pi, to = pi / 2, col = col, lwd = lwd)
  #   draw_arc(cx = xL + r, cy = y1, r = r, from = -pi / 2, to = -pi, col = col, lwd = lwd)
  #   draw_arc(cx = xR - r, cy = y2, r = r, from = 0, to = pi / 2, col = col, lwd = lwd)
  #   draw_arc(cx = xR - r, cy = y1, r = r, from = 0, to = -pi / 2, col = col, lwd = lwd)
  # }
  # ===================== PHENOTYPES: CREATE / EDIT / APPLY / DELETE =====================
  # Remise en forme, factorisation légère et commentaires.
  # Hypothèses :
  #  - objets réactifs existants : `styles`, `phenotypes`, `pheno_editing`, `pedigree`, `selectedIndiv`
  #  - opérateur `%||%` défini (valeur par défaut type purrr)
  #  - packages utilisés : shiny, shinyWidgets, htmltools, pedtools, grDevices, stats

  # -------------------------------------------------------------------------------------
  # UTILITIES
  # -------------------------------------------------------------------------------------
  css_to_hex <- function(rgb_str) {
    # Convertit "rgb(R G B)" en hexadécimal #RRGGBB. Retourne NA_character_ si échec.
    nums <- as.numeric(unlist(regmatches(rgb_str, gregexpr("[0-9]+", rgb_str))))
    if (length(nums) != 3 || any(is.na(nums))) {
      return(NA_character_)
    }
    grDevices::rgb(nums[1], nums[2], nums[3], maxColorValue = 255)
  }

  color_item_html <- function(col_value, col_label = NULL) {
    # Élément HTML pour un item de couleur (pastille + nom)
    lab <- if (is.null(col_label) || !nzchar(col_label)) col_value else col_label
    htmltools::HTML(sprintf(
      '<span class="swatch" style="display:inline-block;width:14px;height:14px;border:1px solid rgba(0,0,0,.2);border-radius:3px;margin-right:8px;vertical-align:middle;background:%s"></span><span class="cname" style="vertical-align:middle;">%s</span>',
      col_value, htmltools::htmlEscape(lab)
    ))
  }

  build_picker_choices <- function(values, labels = NULL) {
    # Construit la liste choices + contenus HTML pour shinyWidgets::pickerInput
    if (is.null(labels)) labels <- values
    names(values) <- labels
    choices <- stats::setNames(unname(values), names(values))
    contents <- lapply(seq_along(values), function(i) color_item_html(unname(values)[i], names(values)[i]))
    list(choices = choices, contents = contents)
  }

  # -------------------------------------------------------------------------------------
  # STYLES REBUILD
  # -------------------------------------------------------------------------------------
  rebuildStyles <- function() {
    styles$fill <- list()
    styles$hatched <- character(0)
    styles$dashed <- character(0)

    if (length(phenotypes$assign) == 0) {
      return(invisible(NULL))
    }

    for (nm in names(phenotypes$assign)) {
      ids <- phenotypes$assign[[nm]] %||% character(0)
      if (!nm %in% names(phenotypes$list) || length(ids) == 0) next

      ph <- phenotypes$list[[nm]]
      for (id in ids) styles$fill[[id]] <- ph$fill
      if (isTRUE(ph$hatched)) styles$hatched <- unique(c(styles$hatched, ids))
      if (identical(ph$lty, "dashed")) styles$dashed <- unique(c(styles$dashed, ids))
    }
    invisible(NULL)
  }

  # -------------------------------------------------------------------------------------
  # MODALE CREATE/EDIT PHENOTYPE
  # -------------------------------------------------------------------------------------
  openPhenoModal <- function(prefill = NULL) {
    # Palette custom (en RGB CSS, convertie en HEX pour picker)
    custom_palette_rgb <- c(
      "rgb(254 242 242)", "rgb(255 226 226)", "rgb(255 201 201)", "rgb(255 162 162)",
      "rgb(255 100 103)", "rgb(251 44 54)", "rgb(231 24 11)", "rgb(193 16 7)",
      "rgb(159 7 18)", "rgb(130 24 26)", "rgb(70 8 9)", "rgb(232 245 233)",
      "rgb(200 230 201)", "rgb(165 214 167)", "rgb(129 199 132)", "rgb(102 187 106)",
      "rgb(76 175 80)", "rgb(67 160 71)", "rgb(56 142 60)", "rgb(46 125 50)",
      "rgb(27 94 32)", "rgb(224 247 250)", "rgb(178 235 242)", "rgb(128 222 234)",
      "rgb(77 208 225)", "rgb(38 198 218)", "rgb(0 188 212)", "rgb(0 172 193)",
      "rgb(0 151 167)", "rgb(0 131 143)", "rgb(0 96 100)", "rgb(249 250 251)",
      "rgb(243 244 246)", "rgb(229 231 235)", "rgb(209 213 220)", "rgb(153 161 175)",
      "rgb(106 114 130)", "rgb(74 85 101)", "rgb(54 65 83)", "rgb(30 41 57)",
      "rgb(16 24 40)", "rgb(3 7 18)"
    )
    custom_palette_hex <- vapply(custom_palette_rgb, css_to_hex, character(1))
    cp <- build_picker_choices(values = custom_palette_hex, labels = custom_palette_rgb)

    base_vals <- stats::setNames(colors(), colors())
    bp <- build_picker_choices(values = unname(base_vals), labels = names(base_vals))

    grouped_choices <- list("Custom" = cp$choices, "Base R" = bp$choices)
    grouped_contents <- c(cp$contents, bp$contents)

    # Valeurs par défaut si prefill présent
    default_name <- if (!is.null(prefill)) prefill$name else ""
    default_fill <- if (!is.null(prefill)) prefill$fill else unname(cp$choices[[1]])
    default_col <- if (!is.null(prefill)) prefill$col else "black"
    default_lty <- if (!is.null(prefill)) prefill$lty else "solid"
    default_hat <- if (!is.null(prefill)) if (prefill$hatched) "TRUE" else "FALSE" else "FALSE"

    showModal(modalDialog(
      title = if (is.null(prefill)) "Create a new phenotype" else paste("Edit phenotype:", prefill$name),
      fluidRow(
        column(
          6,
          h5("Preview"),
          plotOutput("previewPheno", height = "300px")
        ),
        column(
          6,
          tags$style(HTML(".swatch{display:inline-block;width:14px;height:14px;border:1px solid rgba(0,0,0,.2);border-radius:3px;margin-right:8px;vertical-align:middle}.cname{vertical-align:middle}")),
          shinyWidgets::pickerInput(
            inputId = "pheno_fill", label = "Fill color",
            choices = grouped_choices,
            choicesOpt = list(content = grouped_contents),
            options = list("live-search" = TRUE, size = 10, dropupAuto = FALSE),
            selected = default_fill
          ),
          shinyWidgets::pickerInput(
            inputId = "pheno_col", label = "Border color",
            choices = grouped_choices,
            choicesOpt = list(content = grouped_contents),
            options = list("live-search" = TRUE, size = 10, dropupAuto = FALSE),
            selected = default_col
          ),
          selectInput("pheno_lty", "Line type",
            choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
            selected = default_lty
          ),
          shinyWidgets::radioGroupButtons(
            inputId = "pheno_hatched", label = "Pattern fill",
            choices = c("Border" = "FALSE", "Background" = "TRUE"),
            selected = default_hat, justified = TRUE, size = "sm", status = "primary",
            checkIcon = list(yes = icon("check"))
          ),
          textInput("pheno_name", "Phenotype name", value = default_name)
        )
      ),
      footer = tagList(modalButton("Cancel"), actionButton("savePheno", if (is.null(prefill)) "Save" else "Save changes")),
      size = "l", easyClose = TRUE
    ))
  }

  # -------------------------------------------------------------------------------------
  # UI TRIGGER – NOUVEAU PHÉNOTYPE
  # -------------------------------------------------------------------------------------
  observeEvent(input$newPheno, {
    pheno_editing(NULL)
    openPhenoModal(NULL)
  })

  # -------------------------------------------------------------------------------------
  # PREVIEW DANS LA MODALE
  # -------------------------------------------------------------------------------------
  output$previewPheno <- renderPlot({
    req(input$pheno_fill, input$pheno_col, input$pheno_lty, input$pheno_hatched)
    y <- pedtools::singletons("test", sex = 0)
    hat <- if (isTRUE(as.logical(input$pheno_hatched))) "test" else NULL

    plot(
      y,
      fill = input$pheno_fill %||% "",
      col = input$pheno_col %||% "black",
      lty = input$pheno_lty %||% "solid",
      hatched = hat,
      symbolsize = 6.0,
      cex = 1.4,
      main = "", axes = FALSE, labs = NA
    )
  })

  # -------------------------------------------------------------------------------------
  # SAUVEGARDE (CREATE/EDIT)
  # -------------------------------------------------------------------------------------
  observeEvent(input$savePheno, {
    req(input$pheno_name, nzchar(input$pheno_name))
    nm <- input$pheno_name
    spec <- list(
      fill = input$pheno_fill,
      col = input$pheno_col,
      lty = input$pheno_lty,
      hatched = isTRUE(as.logical(input$pheno_hatched)),
      name = nm
    )

    editing <- pheno_editing()
    if (is.null(editing)) {
      # Création
      if (nm %in% names(phenotypes$list)) {
        showNotification("Phenotype name already in use.", type = "error")
        return()
      }
      phenotypes$list[[nm]] <- spec
      phenotypes$assign[[nm]] <- character(0)
    } else {
      # Édition
      if (nm != editing) {
        if (nm %in% names(phenotypes$list)) {
          showNotification("Target name already exists.", type = "error")
          return()
        }
        phenotypes$list[[nm]] <- spec
        phenotypes$assign[[nm]] <- phenotypes$assign[[editing]] %||% character(0)
        phenotypes$list[[editing]] <- NULL
        phenotypes$assign[[editing]] <- NULL
        pheno_editing(NULL)
      } else {
        phenotypes$list[[nm]] <- spec
      }
    }

    removeModal()
    rebuildStyles()
  })

  # -------------------------------------------------------------------------------------
  # RENDER LISTE DES PHÉNOTYPES + BOUTONS
  # -------------------------------------------------------------------------------------
  output$phenoButtonsUI <- renderUI({
    if (length(phenotypes$list) == 0) {
      return("No defined phenotype.")
    }

    tagList(lapply(names(phenotypes$list), function(nm) {
      fluidRow(
        column(2, plotOutput(paste0("legendplot_", nm), height = "36px", width = "36px")),
        column(5, actionLink(paste0("applypheno_", nm), nm, style = "font-weight:600;")),
        column(5, div(
          style = "display:flex; gap:6px; justify-content:flex-end;",
          actionButton(paste0("editpheno_", nm), "Edit", class = "btn btn-sm"),
          actionButton(paste0("deletepheno_", nm), "Delete", class = "btn btn-sm btn-danger")
        ))
      )
    }))
  })

  # -------------------------------------------------------------------------------------
  # MINI-LEGENDES (aperçu de chaque phénotype)
  # -------------------------------------------------------------------------------------
  observe({
    lapply(names(phenotypes$list), function(nm) {
      output[[paste0("legendplot_", nm)]] <- renderPlot({
        ph <- phenotypes$list[[nm]]
        y <- pedtools::singletons("leg", sex = 0)
        hat <- if (isTRUE(ph$hatched)) "test" else NULL

        par(mar = rep(0, 4), xpd = NA)
        plot(
          y,
          fill = ph$fill,
          col = ph$col,
          lty = ph$lty,
          hatched = hat,
          margins = rep(0.01, 4),
          symbolsize = 1.7, cex = 2,
          main = "", axes = FALSE, labs = NA
        )
      })
    })
  })

  # -------------------------------------------------------------------------------------
  # APPLY / UNAPPLY PHENOTYPE SUR L'INDIVIDU SÉLECTIONNÉ
  # -------------------------------------------------------------------------------------
  observe({
    lapply(names(phenotypes$list), function(nm) {
      observeEvent(input[[paste0("applypheno_", nm)]],
        {
          req(pedigree$ped, selectedIndiv$row)
          id <- as.character(selectedIndiv$row$id)
          cur <- phenotypes$assign[[nm]] %||% character(0)

          if (id %in% cur) {
            phenotypes$assign[[nm]] <- setdiff(cur, id)
            showNotification(paste("Removed phenotype", nm, "from", id), type = "default")
          } else {
            phenotypes$assign[[nm]] <- union(cur, id)
            showNotification(paste("Applied phenotype", nm, "to", id), type = "message")
          }
          rebuildStyles()
        },
        ignoreInit = TRUE
      )
    })
  })

  # -------------------------------------------------------------------------------------
  # EDIT PHENOTYPE (OUVERTURE MODALE)
  # -------------------------------------------------------------------------------------
  observe({
    lapply(names(phenotypes$list), function(nm) {
      observeEvent(input[[paste0("editpheno_", nm)]],
        {
          pheno_editing(nm)
          openPhenoModal(phenotypes$list[[nm]])
        },
        ignoreInit = TRUE
      )
    })
  })

  # -------------------------------------------------------------------------------------
  # DELETE PHENOTYPE
  # -------------------------------------------------------------------------------------
  observe({
    lapply(names(phenotypes$list), function(nm) {
      observeEvent(input[[paste0("deletepheno_", nm)]],
        {
          phenotypes$list[[nm]] <- NULL
          phenotypes$assign[[nm]] <- NULL
          rebuildStyles()
          showNotification(paste("Deleted phenotype", nm), type = "warning")
        },
        ignoreInit = TRUE
      )
    })
  })
  # Sex -> libellé
  sex_to_chr <- function(ped, ids) {
    vapply(ids, function(id) {
      sx <- pedtools::getSex(ped, id)
      if (length(sx) == 0 || is.na(sx) || sx == 0) {
        "Unknown"
      } else if (sx == 1) "Male" else "Female"
    }, character(1))
  }

  # Age calculé si Birth/Death connus (ISO "YYYY-MM-DD" si possible)
  compute_age <- function(birth_chr, death_chr) {
    b <- suppressWarnings(lubridate::ymd(birth_chr))
    d <- suppressWarnings(lubridate::ymd(death_chr))
    vapply(seq_along(b), function(i) {
      if (is.na(b[i])) {
        return(NA_integer_)
      }
      ref <- if (!is.na(d[i])) d[i] else Sys.Date()
      floor(lubridate::time_length(lubridate::interval(b[i], ref), "years"))
    }, integer(1))
  }

  # Construit la table de base (et réutilise les données existantes si présentes)
  buildPedTableData <- function(ped, old = NULL) {
    ids <- labels(ped)
    father_chr <- vapply(ids, function(id) chr1(pedtools::father(ped, id, internal = FALSE)), character(1))
    mother_chr <- vapply(ids, function(id) chr1(pedtools::mother(ped, id, internal = FALSE)), character(1))


    # récupère une colonne ancienne par id si elle existe
    old_by_id <- function(col, default = "") {
      if (is.null(old) || !(col %in% names(old))) {
        return(setNames(rep(default, length(ids)), ids))
      }
      x <- setNames(old[[col]], old$id)
      out <- x[ids]
      out[is.na(out)] <- default
      out
    }

    last_name <- unname(old_by_id("last_name", ""))
    first_name <- unname(old_by_id("first_name", ""))
    Birth <- unname(old_by_id("Birth", ""))
    Deceased <- unname(as.logical(old_by_id("Deceased", FALSE)))
    Death <- unname(old_by_id("Death", ""))
    age_old <- old_by_id("age", NA_integer_)

    # calcule un age si possible (et si vide)
    age_calc <- compute_age(Birth, ifelse(Deceased & nzchar(Death), Death, NA_character_))
    age <- ifelse(is.na(as.integer(age_old)), age_calc, as.integer(age_old))

    data.frame(
      id = ids,
      last_name = last_name,
      first_name = first_name,
      Sex = sex_to_chr(ped, ids),
      Father = father_chr,
      Mother = mother_chr,
      Birth = Birth,
      Deceased = Deceased,
      Death = Death,
      age = age,
      stringsAsFactors = FALSE
    )
  }

  # Boutons d'action par ligne
  make_action_buttons <- function(ids) {
    select_btn <- sprintf(
      '<button class="btn btn-success btn-xs ped-select" data-id="%s">Select</button>', ids
    )
    delete_btn <- sprintf(
      '<button class="btn btn-danger  btn-xs ped-delete" data-id="%s">Delete</button>', ids
    )
    list(select = select_btn, delete = delete_btn)
  }
  # Qui est en cours d’édition dans la modale (index de values$pedData)
  editing_idx <- reactiveVal(NULL)
  editing_id <- reactiveVal(NULL) # utile pour setSex pedtools

  ## FENETRE MODALE -----------------
  txtInp <- function(pos, id, annots) {
    value <- annots[[pos]][[id]] %||% ""
    textInput(
      inputId = paste0("annot_", pos, "_", id),
      label = NULL,
      value = value,
      placeholder = pos,
      width = "80px"
    )
  }

  # ================== DOUBLE-CLICK: ouvrir une fiche & éditer ==================
  # ================== DOUBLE-CLICK → ouvrir la modale d'édition ==================
  observeEvent(input$ped_dblclick, {
    req(pedigree$ped, values$pedData)

    # Localise l’individu cliqué (même logique que le simple clic)
    al <- plotAlignment()
    sc <- plotScaling()
    centers <- data.frame(
      x = al$xall + sc$boxw / 2,
      y = al$yall + sc$boxh / 2,
      id_plot = al$plotord
    )

    click <- input$ped_dblclick
    if (is.null(click$x) || is.null(click$y)) {
      return()
    }

    centers$dist <- sqrt((centers$x - click$x)^2 + (centers$y - click$y)^2)
    idx <- which.min(centers$dist)
    if (!length(idx) || centers$dist[idx] > 20) {
      return()
    }

    lab <- labels(pedigree$ped)[centers$id_plot[idx]]
    if (!nzchar(lab)) {
      return()
    }

    i <- match(lab, values$pedData$id)
    if (is.na(i)) {
      return()
    }

    row <- values$pedData[i, , drop = FALSE]

    # mémorise qui on édite
    editing_idx(i)
    editing_id(lab)

    # déduis le sexe courant (int 0/1/2)
    sex_current <- switch(tolower(row$Sex %||% ""),
      "male" = 1,
      "female" = 2,
      0
    )

    # Prépare valeurs de dates (shinyWidgets accepte Date)
    birth_val <- if (nzchar(row$Birth)) suppressWarnings(lubridate::ymd(row$Birth)) else NULL
    death_val <- if (nzchar(row$Death)) suppressWarnings(lubridate::ymd(row$Death)) else NULL

    showModal(modalDialog(
      title = sprintf("Éditer l’individu %s", lab),
      easyClose = TRUE, size = "m",
      footer = tagList(
        modalButton("Fermer"),
        actionButton("ped_save", "Enregistrer", class = "btn btn-primary")
      ),
      fluidPage(
        fluidRow(
          column(
            6,
            textInput("m_first_name", "Prénom", value = row$first_name %||% ""),
            shinyWidgets::airDatepickerInput(
              "m_birth", "Date de naissance",
              value = birth_val, dateFormat = "yyyy-MM-dd",
              autoClose = TRUE, clearButton = TRUE
            ),
            checkboxInput("m_deceased", "Décédé(e)", value = isTRUE(row$Deceased))
          ),
          column(
            6,
            textInput("m_last_name", "Nom", value = row$last_name %||% ""),
            selectInput("m_sex", "Sexe",
              choices = c("Inconnu" = 0, "Homme" = 1, "Femme" = 2),
              selected = sex_current
            ),
            uiOutput("m_death_ui")
          )
        ),
        textAreaInput("m_comments", "Commentaires", value = row$comments %||% "", width = "100%"),
        hr(),
        # uiOutput("m_age_preview")
      )
    ))

    # Champ date de décès conditionnel
    output$m_death_ui <- renderUI({
      if (isTRUE(input$m_deceased)) {
        shinyWidgets::airDatepickerInput(
          "m_death", "Date de décès",
          value = death_val, dateFormat = "yyyy-MM-dd",
          autoClose = TRUE, clearButton = TRUE
        )
      }
    })

    # Aperçu d’âge simple (années)
    output$m_age_preview <- renderUI({
      b <- suppressWarnings(lubridate::ymd(input$m_birth))
      d <- if (isTRUE(input$m_deceased)) suppressWarnings(lubridate::ymd(input$m_death)) else NA
      if (is.na(b)) {
        return(div(class = "text-muted", "Âge : —"))
      }
      ref <- if (is.na(d)) Sys.Date() else d
      yrs <- floor(lubridate::time_length(lubridate::interval(b, ref), "years"))
      div(HTML(sprintf("<b>Âge&nbsp;:</b> %s ans", if (is.finite(yrs)) yrs else "—")))
    })
  })

  observeEvent(input$table, {
    mode("table")
  })
  observeEvent(input$infosmed, {
    mode("tools")
  })

  output$ui_multiview <- renderUI({
    if (is.null(mode())) {
      return(NULL)
    }
    if (mode() == "table") {
      bsCollapse(bsCollapsePanel(title = "📋 Pedigree Table:", DTOutput("pedTableDT")))
    } else if (mode() == "tools") {
      bsCollapse(bsCollapsePanel(title = "View Options", "TEST2", hr(), "📌"))
    }
  })

  # Live update of modal inputs for age synchronization
  observeEvent(input$modal_dob,
    {
      modalInput$dob <- input$modal_dob
    },
    ignoreInit = TRUE
  )
  observeEvent(input$modal_deceased,
    {
      modalInput$deceased <- input$modal_deceased
      if (!isTRUE(input$modal_deceased)) modalInput$dod <- NULL
    },
    ignoreInit = TRUE
  )
  observeEvent(input$modal_dod,
    {
      modalInput$dod <- input$modal_dod
    },
    ignoreInit = TRUE
  )

  output$modal_dod_ui <- renderUI({
    req(modalData$row)
    if (isTRUE(input$modal_deceased)) {
      airDatepickerInput(
        "modal_dod", "Date of death",
        value = modalInput$dod, placeholder = "🗓️ Select date",
        dateFormat = "dd/MM/yyyy", language = "en", autoClose = TRUE, clearButton = TRUE
      )
    }
  })

  output$modal_age_ui <- renderUI({
    dob <- modalInput$dob
    dod <- if (isTRUE(modalInput$deceased)) modalInput$dod else NA
    age_txt <- ""
    if (!is.null(dob) && !is.na(dob)) {
      age_txt <- calculateAgeText(
        format(as.Date(dob), "%d-%m-%Y"),
        if (!is.null(dod) && !is.na(dod)) format(as.Date(dod), "%d-%m-%Y") else NA
      )
    }
    textInput("modal_age", "Age", value = age_txt)
  })

  # ================== SAVE (global) ==================
  observeEvent(input$ped_save,
    {
      i <- editing_idx()
      lab <- editing_id()
      req(!is.null(i), nzchar(lab), values$pedData, pedigree$ped)

      # Récupère inputs
      new_first <- input$m_first_name %||% ""
      new_last <- input$m_last_name %||% ""
      new_sex <- as.integer(input$m_sex %||% 0)
      new_birth <- if (!is.null(input$m_birth) && !is.na(input$m_birth)) as.character(input$m_birth) else ""
      new_deceased <- isTRUE(input$m_deceased)
      new_death <- if (new_deceased && !is.null(input$m_death) && !is.na(input$m_death)) as.character(input$m_death) else ""
      new_comments <- input$m_comments %||% ""

      # Met à jour pedtools si le sexe a changé
      old_sex <- switch(tolower(values$pedData$Sex[i] %||% ""),
        "male" = 1,
        "female" = 2,
        0
      )
      if (!is.na(new_sex) && new_sex %in% c(0, 1, 2) && new_sex != old_sex) {
        p2 <- tryCatch(pedtools::setSex(pedigree$ped, ids = lab, sex = new_sex),
          error = function(e) {
            showNotification(e$message, type = "error")
            NULL
          }
        )
        if (!is.null(p2)) pedigree$ped <- p2
      }

      # Recalcule l’âge (années)
      calc_age_years <- function(bchr, dchr, decd) {
        b <- suppressWarnings(lubridate::ymd(bchr))
        d <- if (isTRUE(decd) && nzchar(dchr)) suppressWarnings(lubridate::ymd(dchr)) else NA
        if (is.na(b)) {
          return(NA_integer_)
        }
        ref <- if (is.na(d)) Sys.Date() else d
        floor(lubridate::time_length(lubridate::interval(b, ref), "years"))
      }

      # Écrit dans la table réactive
      df <- values$pedData
      df$first_name[i] <- new_first
      df$last_name[i] <- new_last
      df$Sex[i] <- if (new_sex == 1) "Male" else if (new_sex == 2) "Female" else "Unknown"
      df$Birth[i] <- new_birth
      df$Deceased[i] <- new_deceased
      df$Death[i] <- if (new_deceased) new_death else ""
      df$comments[i] <- new_comments
      df$age[i] <- calc_age_years(df$Birth[i], df$Death[i], df$Deceased[i])
      values$pedData <- df

      # Resserre la sélection et ferme la modale
      sel(lab)
      selectedIndiv$row <- df[i, , drop = FALSE]
      selectedIndiv$index <- i
      removeModal()

      # Reset état d’édition
      editing_idx(NULL)
      editing_id(NULL)

      showNotification(sprintf("Individu %s enregistré.", lab), type = "message")
    },
    ignoreInit = TRUE
  )


  observeEvent(input$modal_age, {
    idx <- modalData$index
    if (is.null(idx) || length(idx) != 1 || is.null(values$pedData)) {
      return()
    }
    df <- values$pedData
    df[idx, "age"] <- input$modal_age
    df2 <- updatePedTableDates(df)
    dob_new <- df2[idx, "date_of_birth"]
    dod_new <- df2[idx, "date_of_death"]
    updateAirDateInput(session, "modal_dob",
      value = if (!is.na(dob_new) && dob_new != "") as.Date(dob_new, "%d-%m-%Y") else NULL
    )
    updateAirDateInput(session, "modal_dod",
      value = if (!is.na(dod_new) && dod_new != "") as.Date(dod_new, "%d-%m-%Y") else NULL
    )
    modalInput$dob <- if (!is.na(dob_new) && dob_new != "") as.Date(dob_new, "%d-%m-%Y") else NULL
    modalInput$dod <- if (!is.na(dod_new) && dod_new != "") as.Date(dod_new, "%d-%m-%Y") else NULL
    values$pedData <- df2
  })
  updatePedTableDates <- function(df) {
    for (i in seq_len(nrow(df))) {
      dob <- df$date_of_birth[i]
      dod <- df$date_of_death[i]
      deceased <- isTRUE(df$deceased[i])
      age_txt <- tolower(trimws(df$age[i]))

      if (!deceased) df$date_of_death[i] <- ""

      n_years <- extract_units(age_txt, "year|years")
      n_months <- extract_units(age_txt, "month|months")
      n_days <- extract_units(age_txt, "day|days")
      age_entered <- (n_years + n_months + n_days) > 0

      if (age_entered && (is.na(dob) || dob == "" || dob == "NA") && !deceased) {
        birth_estimate <- tryCatch(compute_relative_date(Sys.Date(), n_years, n_months, n_days, "backward"), error = function(e) NA)
        df$date_of_birth[i] <- format(birth_estimate, "%d-%m-%Y")
      }
      if (age_entered && deceased && !is.na(dob) && dob != "" && (is.na(dod) || dod == "" || dod == "NA")) {
        death_estimate <- tryCatch(compute_relative_date(as.Date(dob, "%d-%m-%Y"), n_years, n_months, n_days, "forward"), error = function(e) NA)
        df$date_of_death[i] <- format(death_estimate, "%d-%m-%Y")
      }
      if (age_entered && deceased && !is.na(dod) && dod != "" && (is.na(dob) || dob == "" || dob == "NA")) {
        birth_estimate <- tryCatch(compute_relative_date(as.Date(dod, "%d-%m-%Y"), n_years, n_months, n_days, "backward"), error = function(e) NA)
        df$date_of_birth[i] <- format(birth_estimate, "%d-%m-%Y")
      }
      if ((!age_entered) && (is.na(dob) || dob == "") && (is.na(dod) || dod == "")) {
        df$age[i] <- ""
      } else {
        dob2 <- df$date_of_birth[i]
        dod2 <- ifelse(deceased, df$date_of_death[i], NA)
        df$age[i] <- calculateAgeText(dob2, dod2)
      }
    }
    return(df)
  }
  observeEvent(input$save_modal, {
    removeModal()
  })
}
# 🔚

# ✅ RUN APP ============================
shinyApp(ui = ui, server = server)
