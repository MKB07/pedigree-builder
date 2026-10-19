# Archived R development file
# Original path: 🧩 Versions_Support/complete/full_app_module.R
# Original created: 2025-06-17 19:46:46
# Original modified: 2025-09-25 13:00:03
# Archive rationale: Full modular application snapshot from the support versions.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# -------------------- LIBRAIRIES -----------------------
library(shiny)
library(shinyBS)
library(shinyjs)
library(pedtools)
library(ribd)
library(ggplot2)
library(ggrepel)
library(glue)
library(lubridate)
library(rhandsontable)
library(bslib)
library(shinyWidgets)
library(gridExtra)
library(shiny)
library(lubridate)
library(timevis)
library(dplyr)
library(httr)
library(jsonlite)
library(DT)

### ORPHANET ---------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
### PREGNANCY --------------------
calcul_suivi <- function(ddr, cycle_length = 28) {
  dpa <- ddr + days(280 + (cycle_length - 28))
  suivi <- tibble(
    Étape = c(
      "1ère consultation prénatale",
      "Déclaration de grossesse (avant 14 SA)",
      "Échographie 1er trimestre",
      "Échographie 2ème trimestre",
      "Échographie 3ème trimestre",
      "Consultation anesthésique",
      "Début congé maternité",
      "Terme estimé (DPA)"
    ),
    Semaine = c(6, 13, 12, 22, 32, 33, 35, 40),
    Date = ddr + weeks(c(6, 13, 12, 22, 32, 33, 35, 40) + (cycle_length - 28) / 7)
  ) %>% mutate(Date = as.Date(Date))
  list(suivi = suivi, dpa = dpa)
}

get_amenorrhee <- function(ddr, today, cycle_length = 28) {
  semaines <- as.integer(interval(ddr, today) / weeks(1))
  mois <- floor(semaines / 4) + 1
  jours_restants <- as.integer((ddr + days(280 + (cycle_length - 28)) - today))
  pct_avancee <- percent((interval(ddr, today) / days(280 + (cycle_length - 28))))
  list(sems = semaines, mois = mois, jours_restants = jours_restants, pct = pct_avancee)
}

#### --------------
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

# -------------Ajout famille ---------------
getPartners <- function(ped, id) {
  kids <- pedtools::children(ped, id)
  if (length(kids) == 0) {
    return(NULL)
  }

  partnerList <- lapply(kids, function(kid) setdiff(pedtools::parents(ped, kid), id))
  unique(unlist(partnerList))
} # Enfants ------------

addSon <- function(ped, id, ...) {
  if (length(id) != 1) {
    stop2("Veuillez sélectionner un seul individu pour ajouter un fils.")
  }

  sex <- pedtools::getSex(ped, id)
  partner <- getPartner(ped, id)

  if (sex == 1) {
    # Père connu
    pedtools::addChildren(
      ped,
      father = id,
      mother = partner, # NULL si inconnu
      sex = 1,
      ...
    )
  } else if (sex == 2) {
    # Mère connue
    pedtools::addChildren(
      ped,
      mother = id,
      father = partner, # NULL si inconnu
      sex = 1,
      ...
    )
  } else {
    stop2("Impossible d’ajouter un fils : sexe inconnu pour l’individu ", id)
  }
}

addDaughter <- function(ped, id, ...) {
  if (length(id) != 1) {
    stop2("Veuillez sélectionner un seul individu pour ajouter une fille.")
  }

  sex <- pedtools::getSex(ped, id)
  partner <- getPartner(ped, id)

  if (sex == 1) {
    pedtools::addChildren(
      ped,
      father = id,
      mother = partner,
      sex = 2,
      ...
    )
  } else if (sex == 2) {
    pedtools::addChildren(
      ped,
      mother = id,
      father = partner,
      sex = 2,
      ...
    )
  } else {
    stop2("Impossible d’ajouter une fille : sexe inconnu pour l’individu ", id)
  }
}

addChildWithPartner <- function(ped, id, partner = NULL, childSex = 1, ...) {
  sex <- pedtools::getSex(ped, id)

  if (sex == 1) {
    pedtools::addChildren(ped, father = id, mother = partner, sex = childSex, ...)
  } else if (sex == 2) {
    pedtools::addChildren(ped, mother = id, father = partner, sex = childSex, ...)
  } else {
    stop2("Sexe inconnu pour l’individu ", id)
  }
}
generateNewId <- function(ped) {
  # Trouver le prochain ID numérique disponible
  existing_ids <- as.character(labels(ped))
  # Si tes IDs sont numériques, prends max+1
  numeric_ids <- suppressWarnings(as.integer(existing_ids))
  new_id <- as.character(max(numeric_ids, na.rm = TRUE) + 1)
  new_id
}

showPartnerModal <- function(id, partners) {
  showModal(modalDialog(
    title = "Choisissez le partenaire",
    selectInput("partner_choice", "Partenaire :", choices = partners),
    footer = tagList(
      modalButton("Annuler"),
      actionButton("validate_partner", "Valider", class = "btn btn-primary")
    ),
    easyClose = TRUE
  ))
}

addPar <- function(x, ids) {
  n <- length(ids)
  pars <- ids[-1] # parents, if more than 1 is selected
  parsex <- getSex(x, pars)
  fa <- mo <- NULL
  if (n == 3) {
    if (parsex[1] == 1 && parsex[2] == 2) {
      fa <- pars[1]
      mo <- pars[2]
    } else if (parsex[1] == 2 && parsex[2] == 1) {
      fa <- pars[2]
      mo <- pars[1]
    } else {
      stop2("Incompatible sex of selected parents: ", ids[2:3])
    }
  } else if (n == 2) {
    if (parsex[1] == 1) {
      fa <- ids[2]
    } else if (parsex[1] == 2) {
      mo <- ids[2]
    } else {
      stop2("Cannot use individuals of uknown sex as parent: ", ids[2])
    }
  } else if (n != 1) {
    stop2("Too many individuals selected")
  }

  addParents(x, ids[1], father = fa, mother = mo, verbose = F)
}
# Fratrie --------
addSib <- function(x, id, sex = 1, side = c("right", "left")) {
  # Vérification du nombre d'identifiants sélectionnés
  if (length(id) != 1) {
    stop(sprintf("Pour ajouter un frère/soeur, sélectionnez un seul individu. Sélection actuelle : %s", paste(id, collapse = ",")))
  }
  # Vérification que l'objet est bien un pedigree
  if (!pedtools::is.ped(x)) {
    stop("Impossible d’ajouter un sibling à un pedigree déconnecté ou non valide.")
  }
  # Si l’individu est fondateur (pas de parents connus), on ajoute les parents fictifs
  if (id %in% pedtools::founders(x)) {
    x <- pedtools::addParents(x, id, verbose = FALSE)
  }
  # On récupère les parents de l’individu
  pars <- pedtools::parents(x, id)
  # Ajoute l’enfant (le sibling) aux mêmes parents
  newped <- pedtools::addChild(x, pars, sex = sex, verbose = FALSE)
  # Calcul de la position de l'individu AVANT ajout
  idInt <- pedtools::internalID(x, id)
  n <- length(x$ID)
  # Calcul de l'ordre des individus dans le pedigree pour placer le sibling à gauche ou droite
  ord <- switch(match.arg(side),
    left = c(seq_len(idInt - 1), n + 1, idInt:n),
    right = c(seq_len(idInt), n + 1, if (idInt < n) seq.int(idInt + 1, n))
  )
  # Réordonne le pedigree pour un affichage logique
  pedtools::reorderPed(newped, ord)
}
addTriplets <- function(ped, id, sexes = c(1, 2)) {
  # id : id de l'individu sélectionné
  # sexes : un vecteur de longueur 2 (sexe des 2 nouveaux siblings, 1=H, 2=F, 0=inconnu)
  stopifnot(length(id) == 1)
  if (id %in% pedtools::founders(ped)) {
    ped <- pedtools::addParents(ped, id, verbose = FALSE)
  }
  parents <- pedtools::parents(ped, id)
  # Ajoute deux nouveaux siblings
  ped2 <- pedtools::addChild(ped, parents, sex = sexes[1], verbose = FALSE)
  ped3 <- pedtools::addChild(ped2, parents, sex = sexes[2], verbose = FALSE)
  # Récupère les ID des nouveaux enfants (ce sont ceux qui n’étaient pas là avant)
  new_ids <- setdiff(labels(ped3), labels(ped))
  # On s'assure de bien identifier les nouveaux enfants
  if (length(new_ids) != 2) stop("Erreur lors de l'ajout des triplés")
  triplet_ids <- sort(c(id, new_ids))
  list(ped = ped3, triplet_ids = triplet_ids)
}

# ====================== FONCTIONS ======================
# Fonctions utilitaires
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




# Calcule l'âge (texte complet)
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
      if (years > 0) paste0(years, " an", ifelse(years > 1, "s", "")),
      if (months > 0) paste0(months, " mois"),
      if (rest_days > 0) paste0(rest_days, " jour", ifelse(rest_days > 1, "s", ""))
    )
    paste(age_parts, collapse = " ")
  } else if (days >= 30) {
    months <- floor(days / 30.44)
    rest_days <- round(days - months * 30.44)
    age_parts <- c(
      paste0(months, " mois"),
      if (rest_days > 0) paste0(rest_days, " jour", ifelse(rest_days > 1, "s", ""))
    )
    paste(age_parts, collapse = " ")
  } else if (days >= 7) {
    weeks <- floor(days / 7)
    rest_days <- days - weeks * 7
    age_parts <- c(
      paste0(weeks, " semaine", ifelse(weeks > 1, "s", "")),
      if (rest_days > 0) paste0(rest_days, " jour", ifelse(rest_days > 1, "s", ""))
    )
    paste(age_parts, collapse = " ")
  } else {
    paste0(days, " jour", ifelse(days > 1, "s", ""))
  }
}

# Pour parser un âge libre et convertir en jours/ans/mois (bonus avancé)
extract_units <- function(txt, unit) {
  pattern <- paste0("([0-9]+)\\s*", unit)
  match <- regmatches(txt, regexpr(pattern, txt, perl = TRUE))
  if (length(match) > 0 && nchar(match[1]) > 0) {
    as.integer(gsub("\\D", "", match[1]))
  } else {
    0
  }
}
compute_relative_date <- function(reference, years, months, days, direction = "backward") {
  if (direction == "backward") {
    reference %m-% years(years) %m-% months(months) - days
  } else {
    reference %m+% years(years) %m+% months(months) + days
  }
}

# Liste des colonnes extra pour la table pedigree
extra_cols <- c("prénom", "nom", "date_of_birth", "deceased", "date_of_death", "age", "commentaire")

formatAnnot <- function(textAnnot, cex, font = 2, col = "blue") {
  if (is.null(textAnnot)) {
    return(NULL)
  }
  lapply(textAnnot, function(b) list(b, cex = cex, font = font, col = col))
}

## Fonction change sex ---------
changeSex <- function(ped, id, sex) {
  pedtools::setSex(ped, ids = id, sex = sex)
}

# ----------------- INTERFACE UTILISATEUR -----------------
ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
    tags$script(HTML("
    function captureAndDownload() {
      var target = document.getElementById('capture-zone');
      if (!target) return;
      html2canvas(target).then(function(canvas) {
        var link = document.createElement('a');
        link.download = 'pedigree_capture.png';
        link.href = canvas.toDataURL();
        link.click();
      });
    }
  "))
  ),
  tags$style(HTML("
  /* Apparence globale de la barre */
  .navbar-default {
    background-color: rgba(47, 47, 47, 0.9);
    font-size: 14px;
    font-family: 'Helvetica Neue', sans-serif;
    font-weight: 200;
    border: none;
     margin-bottom: 0px !important;
  }

  /* Hauteur globale réduite */
  .navbar {
    min-height: 30px !important;  /* min-height est plus efficace que height */
  }

  /* Réduction des paddings des onglets */
  .navbar-nav > li > a {
    padding-top: 6px !important;
    padding-bottom: 6px !important;
    line-height: 18px;
  }

  /* Réduction du padding du titre */
  .navbar-brand {
      background-color: rgba(47, 47, 47, 0.6);
    padding-top: 5px !important;
    padding-bottom: 5px !important;
    font-size: 18px;
    height: auto !important;
 font-weight: 300;
  color: #EFEFEF !important;

  }

  /* Soulignement de l'onglet actif */
  .navbar-default .navbar-nav > .active > a,
  .navbar-default .navbar-nav > .active > a:focus,
  .navbar-default .navbar-nav > .active > a:hover {
    text-decoration: underline;
      background-color: rgba(47, 47, 47, 0.6);
 font-weight: 400;
  }
.navbar-default .navbar-nav > li > a {
  color: #BBBBBB !important;  /* 🔁 couleur des onglets non sélectionnés */
}

  /* Soulignement au survol */
  .navbar-default .navbar-nav > li > a:hover {
    text-decoration: underline;
  }
")),
  titlePanel(div(HTML("<h2><span style='font-family: Helvetica Neue;font-weight: 100; color:#4E4A4A'>PEDIGREE CREATOR</span></h2>"))),
  navbarPage(
    title = "TOOLS",
    tabPanel(
      "Pedigree",
      fluidRow(
        style = "
    margin-top: 0px;
    padding-top: 0px;
    background-image: url('white_A.jpeg');
    background-size:100%;
    background-position: center;
    background-repeat: no-repeat;
    padding-bottom: 30px;
  ",
        column(
          width = 4, offset = 4,
          style = "
    margin-top: 40px;
    padding: 20px 20px;
    background: rgba(255, 255, 255, 0.22);                /* translucide */
    box-shadow: 0 8px 32px 0 rgba(31, 38, 135, 0.15);      /* ombre subtile et profonde */
    backdrop-filter: blur(8px);                            /* effet glassy */
    -webkit-backdrop-filter: blur(8px);
    border-radius: 10px;                                   /* coins très arrondis */
    border: 1px solid rgba(255, 255, 255, 0.38);           /* bordure blanche subtile */
    min-width: 340px;
  ",
          div(
            class = "text-center",
            tags$h4(
              "Pedigree Selection",
              style = "
        margin-bottom: 20px;
z-index: 9999 !important;
        font-family: 'Helvetica Neue', sans-serif;
    font-weight: 400;
        letter-spacing: 0.04em;
        color: #222;
        text-shadow: 0 1px 2px rgba(255,255,255,0.15);
      "
            ),
            hr(style = "
      margin-top: 0;
      margin-bottom: 28px;
      border: none;
      border-top: 1.5px solid rgba(200,200,200,0.5);
      width: 100%;
      margin-left: auto;
      margin-right: auto;
    "),

            # --- Ligne select + bouton random ---
            div(
              style = "
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 16px;
               font-family: 'Helvetica Neue', sans-serif;
        margin-bottom: 22px;
           font-weight: 400;
      ",
              div(
                style = "flex: 2;margin-top:12px;z-index: 9999 !important;",
                selectInput(
                  inputId = "pedChoice",
                  label = NULL,
                  choices = c(
                    "Choose a pedigree" = "", "Single Unknown", "Single Female", "Single Male", "Trio",
                    "Full siblings", "Grandparent", "Great-grandparent",
                    "Half siblings (mat)", "Half siblings (pat)", "Avuncular"
                  ),
                  selectize = TRUE,
                  width = "100%",
                )
              ),
              div(
                style = "flex: 1;",
                actionButton(
                  inputId = "randomPed",
                  label = tagList(tags$i(class = "fas fa-random", style = "margin-right:5px;"), "Random"),
                  style = "
            width: 100%;
            background: rgba(40, 180, 170, 0.15);
            border: none;

            color: #217074;

            border-radius: 10px;
            box-shadow: 0 1.5px 4px 0 rgba(80,100,120,0.08);
            transition: background 0.2s;
          "
                )
              )
            ),

            # --- Ligne boutons reset + capture ---
            div(
              style = "display: flex; gap: 16px;",
              actionButton(
                inputId = "reset",
                label = tagList(tags$i(class = "fas fa-sync-alt", style = "margin-right:5px;"), "Reset"),
                style = "
          flex: 1;
          background: rgba(255, 80, 80, 0.14);
          border: none;
          color: #D44;
                   font-family: 'Helvetica Neue', sans-serif;
          font-weight: 400;
          border-radius: 10px;
          box-shadow: 0 1px 4px 0 rgba(255, 80, 80, 0.09);
          transition: background 0.2s;
        "
              ),
              actionButton(
                inputId = "capture_btn",
                label = icon("save"),
                onclick = "captureAndDownload()",
                style = "
          flex: 1;
          background: rgba(80, 160, 255, 0.12);
          border: none;
          color: #2466B3;
                   font-family: 'Helvetica Neue', sans-serif;
          font-weight: 400;
          border-radius: 10px;
          box-shadow: 0 1px 4px 0 rgba(80, 160, 255, 0.07);
          transition: background 0.2s;
        "
              )
            )
          )
        )
      ),
      br(),
      div(
        id = "capture-zone",

        # Titre du plot et individu sélectionné
        fluidRow(
          column(
            12,


            # Champ texte sans label natif
            textInput(
              inputId = "plotTitle",
              label = NULL, # <--- très important !
              value = "",
              width = "100%",
              placeholder = "Enter a pedigree title here"
            ),
            hr()
          )
        ),
        # ---- Responsive Plot + Legend Side-by-Side ----
        div(
          # CSS global pour layout responsive
          tags$style(HTML("
    .pedigree-container {
      display: flex;
      gap: 30px;
      align-items: flex-start;
      flex-wrap: wrap;
  width: 100%;
  max-width: 1600px;
  margin: 0;
  padding-left: 12px;
  padding-right: 12px;
    }
    .pedigree-plot-zone {
      flex: 1 1 350px;
      min-width: 0;


      display: flex;
      flex-direction: column;
      align-items: center;
    }
    .pedigree-aspect-ratio-box {
      width: 100%;
      aspect-ratio: 4/3;
      background: #fff;
      box-shadow: 0 2px 10px 0 rgba(0,0,0,0.05);
      border-radius: 14px;
      display: flex;
      align-items: center;
      justify-content: center;
      margin-bottom: 10px;
      overflow: hidden;
      min-width: 0;
    }
    .pedigree-legend-zone {
      width: 280px;
      min-width: 210px;
      max-width: 95vw;
      background-color: #EEEEEE;
      font-family: 'Helvetica Neue', sans-serif;
      box-shadow: rgba(9, 30, 66, 0.25) 0px 1px 1px, rgba(9, 30, 66, 0.13) 0px 0px 1px 1px;
      border-radius: 14px;
      padding: 18px 18px 14px 18px;
      margin-bottom: 18px;
    }
    @media (max-width: 400px) {
      .pedigree-container {
        flex-direction: column;
        gap: 24px;
        align-items: stretch;
      }
      .pedigree-plot-zone {
max-width: 100px;

      }
      .pedigree-legend-zone {
        width: 100% !important;
        min-width: 0 !important;
        margin: 0 auto;
      }
    }
    .pedigree-details-zone {
  width: 600px;
  min-width: 180px;
  max-width: 99vw;

  font-family: 'Helvetica Neue', sans-serif;

  border-radius: 16px;
  padding: 16px 18px 14px 18px;
  margin-bottom: 18px;
  margin-left: 0;
  transition: box-shadow 0.18s;
  /* Ajuste la hauteur, l’overflow etc. au besoin */
}

/* Responsive : passe dessous sur petit écran */
@media (max-width: 900px) {
  .pedigree-details-zone {
    width: 100% !important;
    min-width: 0 !important;
    margin: 0 auto;
    margin-top: 0;
  }
}

/* Responsive mobile : supprime arrondi/ombre, padding mini */
@media (max-width: 600px) {
  .pedigree-details-zone {
    max-width: 100vw !important;
    width: 100% !important;
    border-radius: 0;
    box-shadow: none;
    padding-left: 0;
    padding-right: 0;
  }
}

    @media (max-width: 600px) {
      .pedigree-plot-zone, .pedigree-legend-zone {
        max-width: 100vw !important;
        width: 100% !important;
        border-radius: 0;
        padding-left: 0;
        padding-right: 0;
      }
      .pedigree-legend-zone {
        box-shadow: none;
        border-radius: 0;
      }
    }
  ")),
          class = "pedigree-container",

          # ------ PLOT ZONE ------
          div(
            class = "pedigree-plot-zone",
            div(
              class = "pedigree-aspect-ratio-box",
              plotOutput(
                "plot",
                width = "100%",
                height = "100%",
                click = "ped_click",
                dblclick = "ped_dblclick"
              )
            ),
            p("Double-click an individual to add text", class = "text-muted", style = "margin: 0;")
          ),

          # ------ LEGEND ZONE ------
          div(
            class = "pedigree-legend-zone",
            tags$style(HTML("
      .sex-button-container {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 12px;
        font-family: 'Helvetica Neue', sans-serif;
      }
      .sex-button {
        width: 36px;
        height: 36px;
        border-radius: 6px;
        background-color: white;
        box-shadow: rgba(0, 0, 0, 0.12) 0px 1px 3px, rgba(0, 0, 0, 0.24) 0px 1px 2px;
        display: flex;
        align-items: center;
        justify-content: center;
        cursor: pointer;
        transition: all 0.2s ease;
      }
      .sex-button:hover {
        background-color: #e6f2fb;
      }
      .sex-symbol {
        width: 18px;
        height: 18px;
        display: inline-block;
      }
      .male-symbol {
        border: 2px solid #333;
      }
      .female-symbol {
        border: 2px solid #333;
        border-radius: 50%;
      }
      .unknown-symbol {
        width: 18px;
        height: 18px;
        background-color: white;
        border: 2px solid #333;
        transform: rotate(45deg);
      }
    ")),
            h4("LEGEND", style = "font-family: 'Helvetica Neue', sans-serif; font-weight: 400; margin-bottom: 7px;"),
            hr(style = "margin-top: 0; margin-bottom: 15px;"),
            # --- Composants UI stylisés ---
            tags$div(
              class = "sex-button-container",
              tags$div(
                class = "sex-button", onclick = "Shiny.setInputValue('Male', Math.random())",
                tags$div(class = "sex-symbol male-symbol")
              ),
              tags$span("Male")
            ),
            tags$div(
              class = "sex-button-container",
              tags$div(
                class = "sex-button", onclick = "Shiny.setInputValue('Female', Math.random())",
                tags$div(class = "sex-symbol female-symbol")
              ),
              tags$span("Female")
            ),
            tags$div(
              class = "sex-button-container",
              tags$div(
                class = "sex-button", onclick = "Shiny.setInputValue('Unknown', Math.random())",
                tags$div(class = "sex-symbol unknown-symbol")
              ),
              tags$span("Unknown")
            ),
            tags$div(
              class = "sex-button-container",
              tags$div(
                class = "sex-button", onclick = "Shiny.setInputValue('Deceded', Math.random())",
                HTML('
<svg width="20" height="20" viewBox="0 0 100 100">
  <polygon points="50,5 95,50 50,95 5,50" fill="white" stroke="#333" stroke-width="8"/>
  <line x1="20" y1="80" x2="80" y2="20" stroke="#333" stroke-width="8"/>
</svg>
')
              ),
              tags$span("Deceded")
            ),
            tags$div(
              class = "sex-button-container",
              tags$div(
                class = "sex-button", onclick = "Shiny.setInputValue('Carrier', Math.random())",
                HTML('
<svg width="20" height="20" viewBox="0 0 100 100">
  <polygon points="50,5 95,50 50,95 5,50" fill="white" stroke="#333" stroke-width="8"/>
  <circle cx="50" cy="50" r="15" fill="black"/>
</svg>
')
              ),
              tags$span("Carrier")
            ),
            tags$div(
              class = "sex-button-container",
              tags$div(
                class = "sex-button", onclick = "Shiny.setInputValue('Miscarriage', Math.random())",
                HTML('
<svg width="20" height="20" viewBox="0 0 100 100">
  <polygon points="50,5 95,90 5,90" fill="white" stroke="#333" stroke-width="8"/>
</svg>
')
              ),
              tags$span("Miscarriage")
            ),
            hr(),
            tags$h5("PHENOTYPES", style = "color:#2376ab;font-family: 'Helvetica Neue', sans-serif; font-weight: 400;"),
            actionButton("newPheno", "New Phenotype", icon = icon("plus")),
            uiOutput("phenoButtonsUI"),
            br()
          ),
          # --- Optionnel : panneau latéral individuel
          div(
            class = "pedigree-details-zone",
            uiOutput("sidePanelIndiv")
          )
        ),


        # Table des données
        fluidRow(
          column(
            12,
            hr(),
            h4("📋 Pedigree Table :"),
            rHandsontableOutput("pedTable"),
            br()
          )
        )
      )
    ),
    tabPanel(
      "Pregnancy Tool",
      div(HTML("<h2><span style='font-family: Helvetica Neue;font-weight: 100; color:#4E4A4A'>SUIVI GROSSESSE</span></h2>")),
      hr(),
      fluidRow(
        column(
          4,
          offset = 4, style = " background-image: url('white_A.jpeg');
      background-size:center;
    background-position: center;
    background-repeat: no-repeat;
      box-shadow: rgba(60, 64, 67, 0.3) 0px 1px 2px 0px, rgba(60, 64, 67, 0.15) 0px 1px 3px 1px;",
          div(
            style = "padding: 30px;",
            dateInput(
              "ddr",
              label = "📅 Sélectionnez la date des dernières règles (DDR)",
              value = Sys.Date() - 280,
              format = "dd/mm/yyyy",
              width = "100%"
            ),
            numericInput(
              "cycle",
              label = "🔄 Durée du cycle menstruel (jours)",
              min = 20, max = 45, value = 28, step = 1,
              width = "100%"
            ),
            actionButton(
              inputId = "calculer",
              label = "Calculer",
              style = "material-flat",
            ),
            helpText(
              "La durée moyenne d’un cycle est 28 jours. Ajustez si différent."
            )
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          style = "margin: 10px ;box-shadow: rgba(0, 0, 0, 0.25) 0px 0.0625em 0.0625em, rgba(0, 0, 0, 0.25) 0px 0.125em 0.5em, rgba(255, 255, 255, 0.1) 0px 0px 0px 1px inset;",
          5,
          div(
            h3("Résumé de la grossesse", style = "font-weight:600; color:#105080; font-size:1.4em;"),
            uiOutput("resultats"),
            br(),
            # progressBar(
            #   id = "progress_grossesse", value = 0, total = 100,
            #   display_pct = TRUE, striped = TRUE, status = "info"
            # )
          )
        ),
        column(
          style = "margin: 10px ;box-shadow: rgba(0, 0, 0, 0.25) 0px 0.0625em 0.0625em, rgba(0, 0, 0, 0.25) 0px 0.125em 0.5em, rgba(255, 255, 255, 0.1) 0px 0px 0px 1px inset;",
          5,
          div(
            h4("Étapes médicales importantes", style = "color:#b92577; font-weight:600;"),
            DTOutput("calendrier"),
            br(),
            uiOutput("prochaine_etape")
          )
        )
      ),
      br(),
      fluidRow(
        style = "box-shadow: rgba(3, 102, 214, 0.3) 0px 0px 0px 3px;",
        column(
          12,
          div(
            class = "glass-panel",
            h4("Frise chronologique de la grossesse", style = "color:#266fc5; font-weight:600;"),
            timevisOutput("timeline", height = "340px")
          )
        )
      )
    ),
    tabPanel(
      "Orphanet Tool",
      div(HTML("<h2><span style='font-family: Helvetica Neue;font-weight: 100; color:#4E4A4A'>ORPHANET TOOL</span></h2>")),
      sidebarLayout(
        sidebarPanel(
          radioButtons("search_mode", "Mode de recherche :",
            choices = c("Par nom de pathologie" = "name", "Par code ORPHA" = "code"),
            selected = "name"
          ),
          uiOutput("dynamic_input"),
          selectInput("lang", "Langue :",
            choices = list(
              "Anglais" = "en", "Français" = "fr", "Espagnol" = "es",
              "Allemand" = "de", "Italien" = "it", "Néerlandais" = "nl"
            ),
            selected = "en"
          ),
          actionButton("go", "Rechercher", icon = icon("search")),
          br(),
          textOutput("error_text")
        ),
        mainPanel(
          htmlOutput("disease_name"),
          htmlOutput("definition"),
          htmlOutput("info_block"),
          htmlOutput("natural_history"),
          br(),
          tabsetPanel(
            tabPanel("\U0001F52C Phénotypes HPO", DTOutput("hpo_table")),
            tabPanel("\U0001F4CA Épidémiologie", DTOutput("epi_table")),
            tabPanel("\U0001F9EC Gènes associés", DTOutput("gene_table"))
          )
        )
      )
    )
  )
)



# ---------------------- SERVEUR --------------------------
server <- function(input, output, session) {
  selectedIndiv <- reactiveValues(row = NULL, index = NULL)
  phenotypes <- reactiveValues(list = list())
  modalData <- reactiveValues(row = NULL, index = NULL)
  modalInput <- reactiveValues(dob = NULL, dod = NULL, deceased = FALSE)
  annots <- reactiveValues(data = list())

  moveCache <- reactiveValues()

  pedigree <- reactiveValues(
    ped = NULL, # Pas de pedigree chargé au départ
    twins = NULL,
    miscarriage = NULL
  )
  styles <- reactiveValues(
    hatched = NULL,
    carrier = NULL,
    deceased = NULL,
    proband = NULL,
    aff = NULL,
    starred = NULL,
    title = NULL,
    fill = NULL
  )
  textAnnot <- reactiveVal(NULL)
  sel <- reactiveVal(character(0)) # IDs sélectionnés
  values <- reactiveValues(
    previewPed = NULL,
    pedData = NULL
  )

  makePedData <- function(ped) {
    if (is.null(ped)) {
      return(NULL)
    }
    df <- as.data.frame(ped, stringsAsFactors = FALSE)
    for (col in extra_cols) {
      if (!(col %in% names(df))) {
        if (col == "deceased") df[[col]] <- FALSE else df[[col]] <- ""
      }
    }
    df$age <- mapply(calculateAgeText, df$date_of_birth, ifelse(df$deceased, df$date_of_death, NA))
    col_order <- c("id", "fid", "mid", "sex", extra_cols)
    col_order <- col_order[col_order %in% names(df)]
    df <- df[, c(col_order, setdiff(names(df), col_order)), drop = FALSE]
    df
  }

  updatePedData <- function() {
    if (is.null(pedigree$ped)) {
      values$pedData <- NULL
    } else {
      values$pedData <- makePedData(pedigree$ped)
    }
  }

  observeEvent(pedigree$ped, {
    updatePedData()
  })
  observeEvent(input$Deceded, {
    req(selectedIndiv$row, pedigree$ped, values$pedData)
    id <- as.character(selectedIndiv$row$id)

    # Trouver l’index dans le data.frame
    i <- which(values$pedData$id == id)
    if (length(i) == 1) {
      # Marquer comme décédé
      values$pedData$deceased[i] <- TRUE

      # Mise à jour du style visuel
      styles$deceased <- union(styles$deceased, id)

      showNotification(paste("Individu", id, "marqué comme décédé."), type = "message")
    }
  })
  observeEvent(input$Carrier, {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)

    if (!(id %in% labels(pedigree$ped))) {
      showNotification("Individu non trouvé dans le pedigree.", type = "error")
      return()
    }

    # Ajout du style carrier
    styles$carrier <- union(styles$carrier, id)

    showNotification(paste("Phénotype 'Porteur' appliqué à", id), type = "message")
  })
  observeEvent(input$Miscarriage, {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)

    if (!(id %in% labels(pedigree$ped))) {
      showNotification("Individu non trouvé dans le pedigree.", type = "error")
      return()
    }

    # Ajout à la liste des miscarriages
    pedigree$miscarriage <- union(pedigree$miscarriage, id)

    showNotification(paste("Individu", id, "marqué comme fausse couche."), type = "message")
  })

  observeEvent(input$pedChoice, {
    req(input$pedChoice != "")
    ped <- switch(input$pedChoice,
      "Single Unknown" = singletons(id = 1, sex = 0),
      "Single Female" = singletons(id = 1, sex = 2),
      "Single Male" = singletons(id = 1, sex = 1),
      "Trio" = nuclearPed(1),
      "Full siblings" = nuclearPed(2, sex = c(1, 2)),
      "Grandparent" = ancestralPed(2),
      "Great-grandparent" = ancestralPed(3),
      "Half siblings (mat)" = halfSibPed(1, 1, type = "maternal"),
      "Half siblings (pat)" = halfSibPed(1, 1, type = "paternal"),
      "Avuncular" = avuncularPed()
    )
    values$previewPed <- ped
    showModal(modalDialog(
      title = "Aperçu du nouveau pedigree",
      plotOutput("previewPlot"),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirmPed", "Valider", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$randomPed, {
    ped <- NULL
    while (is.null(ped)) {
      ped <- tryCatch(
        {
          randomPed(n = sample(5:10, 1), founders = sample(1:3, 1)) |> relabel()
        },
        error = function(e) NULL
      )
    }
    values$previewPed <- ped
    showModal(modalDialog(
      title = "Aperçu du pedigree aléatoire",
      plotOutput("previewPlot"),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirmPed", "Valider", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  output$previewPlot <- renderPlot({
    req(values$previewPed)
    par(mar = c(1, 1, 1, 1))
    plot(values$previewPed, cex = 1.2)
  })

  observeEvent(input$confirmPed, {
    req(values$previewPed)
    pedigree$ped <- values$previewPed
    values$previewPed <- NULL
    removeModal()
    updatePedData()
  })

  observeEvent(input$reset, {
    pedigree$ped <- NULL
    sel(character(0))
    textAnnot(NULL)
    values$pedData <- NULL
  })

  # Affiche le tableau éditable SEULEMENT si un pedigree existe
  output$pedTable <- renderRHandsontable({
    req(!is.null(values$pedData))
    df <- values$pedData
    df$age <- mapply(calculateAgeText, df$date_of_birth, ifelse(df$deceased, df$date_of_death, NA))
    rhandsontable(df, useTypes = TRUE) %>%
      hot_col(c("id", "fid", "mid", "sex"), readOnly = TRUE) %>%
      hot_col("date_of_birth", type = "date", dateFormat = "DD-MM-YYYY") %>%
      hot_col("date_of_death", type = "date", dateFormat = "DD-MM-YYYY") %>%
      hot_col("deceased", type = "checkbox") %>%
      hot_col("age", type = "text")
  })

  observeEvent(input$pedTable, {
    df <- hot_to_r(input$pedTable)
    if (!is.null(df)) {
      for (i in seq_len(nrow(df))) {
        dob <- df$date_of_birth[i]
        dod <- df$date_of_death[i]
        deceased <- isTRUE(df$deceased[i])
        age_txt <- tolower(trimws(df$age[i]))

        # Correction automatique de la date de décès si "décédé" non coché
        if (!deceased) {
          df$date_of_death[i] <- ""
        }
        n_years <- extract_units(age_txt, "an|ans")
        n_months <- extract_units(age_txt, "mois")
        n_days <- extract_units(age_txt, "jour|jours")
        age_saisi <- (n_years + n_months + n_days) > 0

        if (age_saisi && (is.na(dob) || dob == "" || dob == "NA") && !deceased) {
          birth_estimate <- tryCatch(
            compute_relative_date(Sys.Date(), n_years, n_months, n_days, "backward"),
            error = function(e) NA
          )
          df$date_of_birth[i] <- format(birth_estimate, "%d-%m-%Y")
        }
        if (age_saisi && deceased && !is.na(dob) && dob != "" && (is.na(dod) || dod == "" || dod == "NA")) {
          death_estimate <- tryCatch(
            compute_relative_date(as.Date(dob, "%d-%m-%Y"), n_years, n_months, n_days, "forward"),
            error = function(e) NA
          )
          df$date_of_death[i] <- format(death_estimate, "%d-%m-%Y")
        }
        if (age_saisi && deceased && !is.na(dod) && dod != "" && (is.na(dob) || dob == "" || dob == "NA")) {
          birth_estimate <- tryCatch(
            compute_relative_date(as.Date(dod, "%d-%m-%Y"), n_years, n_months, n_days, "backward"),
            error = function(e) NA
          )
          df$date_of_birth[i] <- format(birth_estimate, "%d-%m-%Y")
        }
        if ((!age_saisi) && (is.na(dob) || dob == "") && (is.na(dod) || dod == "")) {
          df$age[i] <- ""
        } else {
          dob2 <- df$date_of_birth[i]
          dod2 <- ifelse(deceased, df$date_of_death[i], NA)
          df$age[i] <- calculateAgeText(dob2, dod2)
        }
      }
      values$pedData <- df

      # --- MISE À JOUR DYNAMIQUE DE L'AFFICHAGE DU STATUT DÉCÉDÉ ---
      if (!is.null(df$id)) {
        ped_ids <- as.character(labels(pedigree$ped))
        dec_ids <- as.character(df$id)[which(as.logical(df$deceased))]
        styles$deceased <- intersect(dec_ids, ped_ids)
      }
      # -------------------------------------------------------------
    }
  })

  positionDf <- reactive({
    req(pedigree$ped)
    al <- plotAlignment()
    sc <- plotScaling()
    data.frame(x = al$xall, y = al$yall + sc$boxh / 2, idInt = al$plotord)
  })
  plotLabs <- reactive({
    req(pedigree$ped)
    breakLabs(pedigree$ped)
  })
  plotAlignment <- reactive({
    req(pedigree$ped)
    .pedAlignment(
      pedigree$ped,
      twins = pedigree$twins,
      miscarriage = pedigree$miscarriage,
      arrows = FALSE,
      align = c(1.5, 2)
    )
  })
  plotAnnotation <- reactive({
    req(pedigree$ped)

    annot <- .pedAnnotation(
      pedigree$ped,
      labs = plotLabs(),
      hatched = styles$hatched,
      hatchDensity = 20,
      carrier = styles$carrier,
      deceased = styles$deceased,
      textAnnot = formatAnnot(textAnnot(), 1.2),
      col = list("#3c8dbc" = sel()),
      fill = if (length(styles$fill) > 0) unlist(styles$fill) else NA,
      lty = list(dashed = styles$dashed),
      lwd = list(
        `3` = sel(),
        `1.5` = setdiff(styles$dashed, sel())
      )
    )

    if (!is.null(values$pedData)) {
      pedData <- values$pedData
      ids <- labels(pedigree$ped)

      for (i in seq_along(ids)) {
        id <- ids[i]
        text_parts <- c()

        # ID
        if (!is.na(id) && nzchar(id)) {
          text_parts <- c(text_parts, as.character(id))
        }

        # Nom & prénom
        full_name <- paste(na.omit(c(pedData$nom[i], pedData$prénom[i])), collapse = " ")
        if (nzchar(full_name)) {
          text_parts <- c(text_parts, full_name)
        }

        # Âge
        age <- pedData$age[i]
        if (nzchar(age)) {
          age_text <- paste0("(", age, ")")
          text_parts <- c(text_parts, age_text)
        }

        # Symbole décès
        if (isTRUE(pedData$deceased[i])) {
          text_parts <- c(text_parts, "†")
        }

        # Commentaire
        commentaire <- pedData$commentaire[i]
        if (nzchar(commentaire)) {
          text_parts <- c(text_parts, commentaire)
        }

        if (length(text_parts) > 0) {
          annot$textUnder[[as.character(id)]] <- paste(text_parts, collapse = "\n")
        }
      }
    }

    annot
  })

  plotScaling <- reactive({
    req(pedigree$ped)
    .pedScaling(
      plotAlignment(),
      plotAnnotation(),
      cex = 1.4,
      symbolsize = 1,
      margins = rep(3, 4)
    )
  })
  ## PLOT ------------------
  output$plot <- renderPlot({
    req(pedigree$ped)
    align <- withCallingHandlers(
      plotAlignment(),
      warning = function(w) if (startsWith(w$message, "Unexpected")) invokeRestart("muffleWarning")
    )
    annot <- plotAnnotation()
    sc <- plotScaling()
    drawPed(align, annotation = annot, scaling = sc)
    if (!is.null(input$plotTitle) && nzchar(input$plotTitle)) {
      title(main = input$plotTitle, cex.main = 1.7, col.main = "#3c8dbc", line.main = 0.3, font.main = 1)
    }
  })

  ## PED CLICK ---------------------
  observeEvent(input$ped_click, {
    req(pedigree$ped)
    hit <- nearPoints(positionDf(), input$ped_click, xvar = "x", yvar = "y", threshold = 20, maxpoints = 1)$idInt
    req(hit)
    that <- labels(pedigree$ped)[hit]

    curr <- sel()

    if (length(curr) == 1 && curr == that) {
      # Si l’individu cliqué est déjà sélectionné, on le désélectionne
      sel("")
      selectedIndiv$row <- NULL
      selectedIndiv$index <- NULL
    } else {
      # Sinon, on sélectionne l’individu cliqué
      sel(that)
      ind_row <- which(values$pedData$id == that)
      if (length(ind_row) > 0) {
        selectedIndiv$row <- values$pedData[ind_row, ]
        selectedIndiv$index <- ind_row
      }
    }
  })



  output$selectedIndividual <- renderText({
    req(pedigree$ped)
    ids <- sel()
    if (length(ids) == 0) {
      "Aucun individu sélectionné."
    } else {
      paste("Individu(s) sélectionné(s) :", paste(ids, collapse = ", "))
    }
  })

  ## LEGENDE ----------------------------
  observeEvent(input$Male, {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    new_ped <- tryCatch(
      {
        changeSex(pedigree$ped, id, sex = 1)
      },
      error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error")
        return(NULL)
      }
    )
    if (!is.null(new_ped)) {
      pedigree$ped <- new_ped
      values$pedData[values$pedData$id == id, "sex"] <- 1
      showNotification("Gender changed to 'Male''.", type = "message")
    }
  })

  observeEvent(input$Female, {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    new_ped <- tryCatch(
      {
        changeSex(pedigree$ped, id, sex = 2)
      },
      error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error")
        return(NULL)
      }
    )
    if (!is.null(new_ped)) {
      pedigree$ped <- new_ped
      values$pedData[values$pedData$id == id, "sex"] <- 2
      showNotification("Gender changed to 'Female'.", type = "message")
    }
  })

  observeEvent(input$Unknown, {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    new_ped <- tryCatch(
      {
        changeSex(pedigree$ped, id, sex = 0)
      },
      error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error")
        return(NULL)
      }
    )
    if (!is.null(new_ped)) {
      pedigree$ped <- new_ped
      values$pedData[values$pedData$id == id, "sex"] <- 0
      showNotification("Gender has been changed to 'Unknown'.", type = "message")
    }
  })


  ### PHENOTYPES ---------

  # Création du modal pour nouveau phénotype
  observeEvent(input$newPheno, {
    showModal(
      modalDialog(
        title = "Create a new phenotype",
        fluidRow(
          column(
            6,
            h5("Preview :"),
            plotOutput("previewPheno", height = "300px")
          ),
          column(
            6,
            selectInput("pheno_fill", "Couleur du fond", choices = colors(), selected = ""),
            selectInput("pheno_col", "Couleur du contour", choices = colors(), selected = "black"),
            selectInput("pheno_lty", "Motifs",
              choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid"
            ),
            checkboxInput("pheno_hatched", "Motif hachuré", value = FALSE),
            textInput("pheno_name", "Phenotype name")
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("savePheno", "Save")
        ),
        size = "l", easyClose = TRUE
      )
    )
  })

  # Rendu du plot de prévisualisation dans le modal
  output$previewPheno <- renderPlot({
    y <- singletons("test", sex = 0)
    hach <- if (isTRUE(input$pheno_hatched)) "test" else NULL
    plot(y,
      fill = input$pheno_fill %||% "",
      col = input$pheno_col %||% "black",
      lty = input$pheno_lty %||% "solid",
      hatched = hach,
      symbolsize = 6.0, cex = 1.4, main = "", axes = FALSE, labs = NA
    )
  })

  # Sauvegarde du phénotype créé
  observeEvent(input$savePheno, {
    req(input$pheno_name, nzchar(input$pheno_name))
    nom <- input$pheno_name
    if (nom %in% names(phenotypes$list)) {
      showNotification("Phenotype name already in use.", type = "error")
      return()
    }
    phenotypes$list[[nom]] <- list(
      fill = input$pheno_fill,
      col = input$pheno_col,
      lty = input$pheno_lty,
      hatched = input$pheno_hatched
    )
    removeModal()
  })

  # UI des boutons de phénotypes : rendu cohérent avec Homme/Femme/Inconnu
  output$phenoButtonsUI <- renderUI({
    if (length(phenotypes$list) == 0) {
      return("No defined phenotype.")
    }
    lapply(names(phenotypes$list), function(nom) {
      tags$div(
        style = "display:flex; align-items:center; gap:10px; margin-bottom:10px;",
        actionLink(
          inputId = paste0("applypheno_", nom),
          label = tags$div(
            style = "display:flex; align-items:center; gap:10px;",
            div( # ← Ce div encapsule le plot avec style
              style = "margin:0;",
              plotOutput(paste0("legendplot_", nom), height = "36px", width = "36px")
            ),
            span(nom, style = "font-weight:600; font-size:1em;")
          ),
          style = "border:none; background:none; padding:0; margin:0;"
        )
      )
    })
  })


  # Génération des mini-plots de légende
  observe({
    lapply(names(phenotypes$list), function(nom) {
      output[[paste0("legendplot_", nom)]] <- renderPlot({
        ph <- phenotypes$list[[nom]]
        y <- singletons("leg", sex = 0)
        hach <- if (isTRUE(ph$hatched)) "leg" else NULL
        par(mar = rep(0, 4), xpd = NA)
        plot(y,
          fill = ph$fill,
          col = ph$col,
          lty = ph$lty,
          hatched = hach,
          margins = rep(0.01, 4),
          symbolsize = 1.7,
          cex = 2,
          main = "", axes = FALSE, labs = NA
        )
      })
    })
  })

  # Application du style d’un phénotype à un individu sélectionné
  observe({
    lapply(names(phenotypes$list), function(nom) {
      observeEvent(input[[paste0("applypheno_", nom)]],
        {
          req(pedigree$ped, selectedIndiv$row)
          id <- as.character(selectedIndiv$row$id)
          ph <- phenotypes$list[[nom]]

          # Application des styles
          styles$fill[[id]] <- ph$fill
          if (ph$hatched) {
            styles$hatched <- unique(c(styles$hatched, id))
          } else {
            styles$hatched <- setdiff(styles$hatched, id)
          }

          if (ph$lty == "dashed") {
            styles$dashed <- unique(c(styles$dashed, id))
          } else {
            styles$dashed <- setdiff(styles$dashed, id)
          }

          showNotification(paste("Phenotype", nom, "set to", id), type = "message")
        },
        ignoreInit = TRUE
      )
    })
  })
  output$sidePanelIndiv <- renderUI({
    req(pedigree$ped, selectedIndiv$row)
    id <- selectedIndiv$row$id
    sex <- selectedIndiv$row$sex

    div(
      style = "box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px; border-radius: 10px; padding: 15px; background-color: #fff; width: 100%;",

      # Header
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4(HTML(paste0("🧾 Individual <span style='color:#3c8dbc'>", id, "</span>")), style = "margin: 0;"),
        actionButton("closeSidePanel", "✖", class = "btn btn-sm btn-outline-secondary", style = "font-weight:bold; padding: 2px 8px;")
      ),

      # Add Parents Button
      div(
        style = "text-align: center; margin-bottom: 15px;",
        actionButton("addparent", "➕ Add Parents", class = "btn btn-outline-primary btn-sm")
      ),
      div(
        style = "display: flex; gap: 15px; align-items: center;",

        # Left Column: Sibling Buttons
        div(
          style = "display: flex; align-items: center; justify-content: center; gap: 25px;",
          div(
            style = "display: flex; flex-direction: column; gap: 8px;",
            tags$div(
              class = "sex-button-container",
              actionButton("brother",
                label = tagList(
                  tags$div(class = "sex-button", tags$div(class = "sex-symbol male-symbol")),
                  tags$div("Brother", style = "font-size: 13px;")
                ),
                style = "display: flex; align-items: center; gap: 8px; border: none; background: none;"
              )
            ),
            tags$div(
              class = "sex-button-container",
              actionButton("sister",
                label = tagList(
                  tags$div(class = "sex-button", tags$div(class = "sex-symbol female-symbol")),
                  tags$div("Sister", style = "font-size: 13px;")
                ),
                style = "display: flex; align-items: center; gap: 8px; border: none; background: none;"
              )
            ),
            tags$div(
              class = "sex-button-container",
              actionButton("sib_unknown",
                label = tagList(
                  tags$div(class = "sex-button", tags$div(class = "sex-symbol unknown-symbol")),
                  tags$div("Unknown", style = "font-size: 13px;")
                ),
                style = "display: flex; align-items: center; gap: 8px; border: none; background: none;"
              )
            )
          ),

          # Central Sex Symbol
          div(
            HTML(getSexSymbolSVG(sex, size = 180)),
            style = "text-align: center;"
          ),

          # Right Column: Twins / Triplets / Reorder
          div(
            style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("Twins", label = tagList("👥", tags$div("Twin", style = "font-size: 13px;")), class = "btn btn-outline-secondary btn-sm", style = "display:flex; flex-direction: column; align-items: center;"),
            actionButton("Triplets", label = tagList("3️⃣", tags$div("Triplet", style = "font-size: 13px;")), class = "btn btn-outline-secondary btn-sm", style = "display:flex; flex-direction: column; align-items: center;"),
            actionButton("Move", label = tagList("🔄", tags$div("Reorder", style = "font-size: 13px;")), class = "btn btn-outline-secondary btn-sm", style = "display:flex; flex-direction: column; align-items: center;")
          )
        )
      ),

      # Row for Children (under the symbol)
      div(
        style = "display: flex; justify-content: center; gap: 10px; margin-top: 10px;",
        actionButton("child_son", label = HTML(
          '<div style="display: flex; flex-direction: column; align-items: center;">
          <svg width="34" height="34">
            <rect x="5" y="5" width="24" height="24" fill="none" stroke="#222" stroke-width="2"/>
          </svg>
          <span style="font-size: 12px;">Son</span>
        </div>'
        ), class = "sib-btn", style = "border: none; background: none;"),
        actionButton("child_unknown", label = HTML(
          '<div style="display: flex; flex-direction: column; align-items: center;">
          <svg width="34" height="34">
            <polygon points="17,4 30,17 17,30 4,17" fill="none" stroke="#222" stroke-width="2"/>
          </svg>
          <span style="font-size: 12px;">Unknown</span>
        </div>'
        ), class = "sib-btn", style = "border: none; background: none;"),
        actionButton("child_daughter", label = HTML(
          '<div style="display: flex; flex-direction: column; align-items: center;">
          <svg width="34" height="34">
            <circle cx="17" cy="17" r="12" fill="none" stroke="#222" stroke-width="2"/>
          </svg>
          <span style="font-size: 12px;">Daughter</span>
        </div>'
        ), class = "sib-btn", style = "border: none; background: none;"),
        actionButton("choose_partner", label = HTML(
          '<div style="display: flex; flex-direction: column; align-items: center;">
          <span style="font-size: 24px;">👥</span>
          <span style="font-size: 12px;">Partner</span>
        </div>'
        ), class = "sib-btn", style = "border: none; background: none;")
      ),
      tags$hr()
    )
  })


  observeEvent(input$closeSidePanel, {
    sel("") # Désélectionne l'individu
    selectedIndiv$row <- NULL
    selectedIndiv$index <- NULL
  })


  # ajout apparenté -----------
  observeEvent(input$addParentButton, {
    req(pedigree$ped)
    infos <- selectedIndiv$row
    req(!is.null(infos))

    child_id <- as.character(infos$id)
    idx <- which(labels(pedigree$ped) == child_id)
    father_exists <- !is.na(pedigree$ped$FIDX[idx]) && pedigree$ped$FIDX[idx] != 0
    mother_exists <- !is.na(pedigree$ped$MIDX[idx]) && pedigree$ped$MIDX[idx] != 0

    if (father_exists && mother_exists) {
      showModal(modalDialog(
        title = "Ajout impossible",
        "Cet individu a déjà des parents.",
        easyClose = TRUE
      ))
      return(NULL)
    }

    # Utilisation de la fonction addPar()
    new_ped <- tryCatch(
      {
        addPar(pedigree$ped, child_id)
      },
      error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error")
        return(NULL)
      }
    )

    if (!is.null(new_ped)) {
      pedigree$ped <- new_ped
      updatePedData()
      showNotification("parents added to the selected individual.", type = "message")
    }
  })

  # ----------- AJOUT FRATRIE -----------

  # Ajouter un frère (male, sex = 1) à droite
  observeEvent(input$brother, {
    req(pedigree$ped)
    infos <- selectedIndiv$row
    req(!is.null(infos))
    ind <- as.character(infos$id)
    tryCatch(
      {
        newped <- addSib(pedigree$ped, ind, sex = 1, side = "right")
        pedigree$ped <- newped
        updatePedData()
        showNotification("Borther add", type = "message")
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Erreur lors de l’ajout d’un frère à droite",
          paste("Impossible d’ajouter le sibling :", e$message),
          easyClose = TRUE
        ))
      }
    )
  })

  # Ajouter une sœur (female, sex = 2) à droite
  observeEvent(input$sister, {
    req(pedigree$ped)
    infos <- selectedIndiv$row
    req(!is.null(infos))
    ind <- as.character(infos$id)
    tryCatch(
      {
        newped <- addSib(pedigree$ped, ind, sex = 2, side = "right")
        pedigree$ped <- newped
        updatePedData()
        showNotification("Sister add", type = "message")
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Erreur lors de l’ajout d’une sœur à droite",
          paste("Impossible d’ajouter le sibling :", e$message),
          easyClose = TRUE
        ))
      }
    )
  })

  # Ajouter un sibling au sexe inconnu (sex = 0) à droite
  observeEvent(input$sib_unknown, {
    req(pedigree$ped)
    infos <- selectedIndiv$row
    req(!is.null(infos))
    ind <- as.character(infos$id)
    tryCatch(
      {
        newped <- addSib(pedigree$ped, ind, sex = 0, side = "right")
        pedigree$ped <- newped
        updatePedData()
        showNotification("Sibling Unknown add.", type = "message")
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Erreur lors de l’ajout d’un sibling inconnu à droite",
          paste("Impossible d’ajouter le sibling :", e$message),
          easyClose = TRUE
        ))
      }
    )
  })

  observeEvent(input$Twins, {
    req(pedigree$ped)
    infos <- selectedIndiv$row
    req(!is.null(infos))
    ind <- as.character(infos$id)
    sexe_ind <- infos$sex

    # Valeur texte du sexe sélectionné
    labelSexe <- switch(as.character(sexe_ind),
      "1" = "Male",
      "2" = "Female",
      "0" = "Unknown",
      "Inconnu"
    )

    showModal(modalDialog(
      title = sprintf("Ajouter un jumeau à %s", ind),

      # UI dynamique selon sexe connu/inconnu
      uiOutput("twinSexUI"),
      selectInput("twin_type", "Twin type :",
        choices = c("Monozygote (MZ)" = 1, "Dizygote (DZ)" = 2), selected = 2
      ),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_add_twin", "add twin", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))

    # Stocker temporairement le sexe de l'individu sélectionné
    updateSelectInput(session, "twin_sex", selected = sexe_ind)
    output$twinSexUI <- renderUI({
      req(input$twin_type) # pour que la condition s'applique

      if (as.numeric(input$twin_type) == 2) {
        selectInput(
          "twin_sex",
          "Sexe du jumeau :",
          choices = c("Male" = 1, "Female" = 2, "Inconnu" = 0),
          selected = sexe_ind
        )
      } else {
        # Pour monozygote, on n'affiche rien ou un texte explicatif
        tags$p(
          HTML(glue::glue("Sexe imposé : <strong>{labelSexe}</strong> (monozygote)")),
          style = "margin-bottom: 0.5em;"
        )
      }
    })
  })


  observeEvent(input$confirm_add_twin, {
    removeModal()
    req(pedigree$ped)
    infos <- selectedIndiv$row
    req(!is.null(infos))
    ind <- as.character(infos$id)
    sex_sibling <- as.numeric(input$twin_sex)
    twin_code <- as.numeric(input$twin_type)
    ids_avant <- labels(pedigree$ped)
    # Ajout du sibling
    new_ped <- tryCatch(
      {
        addSib(pedigree$ped, id = ind, sex = sex_sibling, side = "right")
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Erreur lors de l'ajout du sibling",
          e$message,
          easyClose = TRUE
        ))
        return(NULL)
      }
    )
    if (is.null(new_ped)) {
      return(NULL)
    }
    ids_apres <- labels(new_ped)
    id_sibling <- setdiff(ids_apres, ids_avant)
    if (length(id_sibling) != 1) {
      showModal(modalDialog(
        title = "Erreur",
        "Impossible de déterminer le nouvel ID du sibling.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    ids_jumeaux <- sort.default(c(ind, id_sibling))
    # Ajout du lien de gémellité dans pedigree$twins
    new_twins <- rbind(
      pedigree$twins,
      data.frame(id1 = ids_jumeaux[1], id2 = ids_jumeaux[2], code = twin_code)
    )
    pedigree$ped <- new_ped
    pedigree$twins <- new_twins
    updatePedData()
    showNotification(sprintf("Jumeau ajouté à %s (%s)", ind, ifelse(twin_code == 1, "MZ", "DZ")), type = "message")
  })

  # Ajouter dans ton server:
  observeEvent(input$Triplets, {
    req(pedigree$ped)
    infos <- selectedIndiv$row
    req(!is.null(infos))
    selected_id <- as.character(infos$id)
    tryCatch(
      {
        # Par défaut : garçon + fille (tu peux personnaliser les sexes avec une modale si tu veux)
        res <- addTriplets(pedigree$ped, selected_id, sexes = c(1, 2))
        pedigree$ped <- res$ped
        triplet_ids <- sort(res$triplet_ids)
        if (is.null(pedigree$twins) || nrow(pedigree$twins) == 0) {
          pedigree$twins <- data.frame(id1 = character(), id2 = character(), code = integer())
        }
        new_twins <- rbind(
          pedigree$twins,
          data.frame(id1 = triplet_ids[1], id2 = triplet_ids[2], code = 2),
          data.frame(id1 = triplet_ids[2], id2 = triplet_ids[3], code = 2)
        )
        pedigree$twins <- new_twins
        updatePedData()
        showNotification("Triplets ajoutés à l’individu sélectionné.", type = "message")
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Erreur lors de l’ajout de triplés",
          paste("Impossible d’ajouter les triplets :", e$message),
          easyClose = TRUE
        ))
      }
    )
  })
  # ----- Ouvre la modale pour réorganiser la fratrie de l'individu sélectionné
  observeEvent(input$Move, {
    req(pedigree$ped)
    infos <- selectedIndiv$row
    req(!is.null(infos))
    sel_id <- as.character(infos$id)

    # 1. Trouver les parents
    parents_sel <- pedtools::parents(pedigree$ped, sel_id)
    if (any(is.na(parents_sel))) {
      showModal(modalDialog(
        title = "Réorganisation impossible",
        "L’individu sélectionné n’a pas de parents connus, donc pas de fratrie à réorganiser.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    # 2. Récupérer la fratrie (tous les enfants des mêmes parents, y compris l'individu lui-même)
    siblings <- pedtools::children(pedigree$ped, parents_sel)
    current_order <- siblings

    # 3. Récupérer les noms et prénoms pour chaque sibling
    ped_data <- values$pedData
    getLabel <- function(id) {
      ligne <- ped_data[ped_data$id == id, ]
      label <- paste(
        id,
        if (!is.null(ligne$prénom) && nzchar(ligne$prénom)) ligne$prénom else "",
        if (!is.null(ligne$nom) && nzchar(ligne$nom)) ligne$nom else ""
      )
      label <- trimws(label)
      return(label)
    }
    labels_vec <- sapply(current_order, getLabel, USE.NAMES = FALSE)
    names(current_order) <- labels_vec # names = ce qui s'affiche, values = id

    # 4. Affiche la modale
    showModal(modalDialog(
      title = sprintf("Réorganiser la fratrie de %s", getLabel(sel_id)),
      helpText("Faites glisser pour réorganiser la fratrie, puis validez."),
      selectizeInput(
        "new_sib_order", "Nouvel ordre de la fratrie :",
        choices = current_order,
        selected = current_order,
        multiple = TRUE,
        options = list(plugins = list("drag_drop"))
      ),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_sib_order", "Valider l’ordre", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
    # Tu peux utiliser une reactiveValues ici pour mémoriser la fratrie
    moveCache$siblings <- siblings
  })


  # Quand l’utilisateur valide le nouvel ordre
  observeEvent(input$confirm_sib_order, {
    removeModal()
    req(pedigree$ped)
    infos <- selectedIndiv$row
    req(!is.null(infos))
    sel_id <- as.character(infos$id)
    siblings <- moveCache$siblings
    new_sib_order <- input$new_sib_order
    if (!setequal(new_sib_order, siblings)) {
      showNotification("Tous les membres de la fratrie doivent être présents dans l’ordre !", type = "error")
      return(NULL)
    }
    all_ids <- labels(pedigree$ped)
    sib_indices <- which(all_ids %in% siblings)
    if (length(sib_indices) != length(siblings)) {
      showNotification("Erreur interne : la fratrie n'a pas pu être identifiée correctement.", type = "error")
      return(NULL)
    }
    new_order <- all_ids
    new_order[sib_indices] <- new_sib_order
    new_ped <- tryCatch(
      {
        pedtools::reorderPed(pedigree$ped, new_order)
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Erreur",
          paste("Impossible de réorganiser la fratrie :", e$message),
          easyClose = TRUE
        ))
        return(NULL)
      }
    )
    if (!is.null(new_ped)) {
      pedigree$ped <- new_ped
      updatePedData()
      showNotification("Nouvel ordre de la fratrie appliqué.", type = "message")
    }
  })

  # --------------- AJOUT ENFANTS ----------------------

  # Fils
  observeEvent(input$child_son, {
    req(!is.null(selectedIndiv$row), pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    tryCatch(
      {
        partners <- getPartners(pedigree$ped, id)
        if (is.null(partners)) {
          pedigree$ped <- addChildWithPartner(pedigree$ped, id, partner = NULL, childSex = 1)
          updatePedData()
          showNotification("Fils ajouté (partenaire inconnu/créé).", type = "message")
        } else if (length(partners) == 1) {
          pedigree$ped <- addChildWithPartner(pedigree$ped, id, partner = partners[1], childSex = 1)
          updatePedData()
          showNotification("Fils ajouté avec le partenaire existant.", type = "message")
        } else {
          # Plusieurs partenaires possibles → modale pour choix
          values$pendingAction <- list(type = "addson", id = id)
          showPartnerModal(id, partners)
        }
      },
      error = function(e) showModal(modalDialog(title = "Erreur", e$message))
    )
  })

  # Fille
  observeEvent(input$child_daughter, {
    req(!is.null(selectedIndiv$row), pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    tryCatch(
      {
        partners <- getPartners(pedigree$ped, id)
        if (is.null(partners)) {
          pedigree$ped <- addChildWithPartner(pedigree$ped, id, partner = NULL, childSex = 2)
          updatePedData()
          showNotification("Fille ajoutée (partenaire inconnu/créé).", type = "message")
        } else if (length(partners) == 1) {
          pedigree$ped <- addChildWithPartner(pedigree$ped, id, partner = partners[1], childSex = 2)
          updatePedData()
          showNotification("Fille ajoutée avec le partenaire existant.", type = "message")
        } else {
          values$pendingAction <- list(type = "adddaughter", id = id)
          showPartnerModal(id, partners)
        }
      },
      error = function(e) showModal(modalDialog(title = "Erreur", e$message))
    )
  })

  # Sexe inconnu
  observeEvent(input$child_unknown, {
    req(!is.null(selectedIndiv$row), pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    tryCatch(
      {
        partners <- getPartners(pedigree$ped, id)
        if (is.null(partners)) {
          pedigree$ped <- addChildWithPartner(pedigree$ped, id, partner = NULL, childSex = 0)
          updatePedData()
          showNotification("Enfant (sexe inconnu) ajouté.", type = "message")
        } else if (length(partners) == 1) {
          pedigree$ped <- addChildWithPartner(pedigree$ped, id, partner = partners[1], childSex = 0)
          updatePedData()
          showNotification("Enfant (sexe inconnu) ajouté avec le partenaire existant.", type = "message")
        } else {
          values$pendingAction <- list(type = "addunknown", id = id)
          showPartnerModal(id, partners)
        }
      },
      error = function(e) showModal(modalDialog(title = "Erreur", e$message))
    )
  })
  observeEvent(input$choose_partner, {
    req(!is.null(selectedIndiv$row), pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    partners <- getPartners(pedigree$ped, id)
    partner_choices <- c()
    if (!is.null(partners) && length(partners) > 0) {
      # Peut-être afficher le nom/prénom dans le selectInput si dispo
      partner_choices <- setNames(partners, partners)
    }
    partner_choices <- c(partner_choices, "Nouveau partenaire" = "new_partner")

    showModal(modalDialog(
      title = paste("Ajouter un enfant avec un partenaire pour", id),
      selectInput("partner_modal_choice", "Choisissez le partenaire :", choices = partner_choices),
      selectInput("child_sex_modal", "Sexe de l'enfant à ajouter :",
        choices = c("Fils" = 1, "Fille" = 2, "Sexe inconnu" = 0)
      ),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("validate_partner_modal", "Valider", class = "btn btn-success")
      ),
      easyClose = TRUE
    ))
  })
  observeEvent(input$validate_partner_modal, {
    req(!is.null(selectedIndiv$row), pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    partner_choice <- input$partner_modal_choice
    child_sex <- as.integer(input$child_sex_modal)
    if (partner_choice == "new_partner") {
      pedigree$ped <- addChildWithPartner(pedigree$ped, id, partner = NULL, childSex = child_sex)
      updatePedData()
      removeModal()
      showNotification("Enfant et nouveau partenaire ajoutés.", type = "message")
    } else {
      pedigree$ped <- addChildWithPartner(pedigree$ped, id, partner = partner_choice, childSex = child_sex)
      updatePedData()
      removeModal()
      showNotification("Enfant ajouté avec le partenaire sélectionné.", type = "message")
    }
  })
  showPartnerModal <- function(id, partners) {
    showModal(modalDialog(
      title = "Choisissez le partenaire",
      selectInput("partner_choice", "Partenaire :", choices = partners),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("validate_partner", "Valider", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  }

  # Si tu veux utiliser la modale générique ci-dessus lors de plusieurs partenaires, ajoute aussi l'observer :
  observeEvent(input$validate_partner, {
    req(!is.null(values$pendingAction), !is.null(selectedIndiv$row), pedigree$ped)
    action_type <- values$pendingAction$type
    id <- values$pendingAction$id
    chosen_partner <- input$partner_choice
    childSex <- switch(action_type,
      addson = 1,
      adddaughter = 2,
      addunknown = 0
    )
    pedigree$ped <- addChildWithPartner(pedigree$ped, id, partner = chosen_partner, childSex = childSex)
    updatePedData()
    removeModal()
    showNotification("Enfant ajouté avec le partenaire choisi.", type = "message")
    values$pendingAction <- NULL
  })
  ## FENETRE MODALE -----------------
  txtInp <- function(pos, id, annots) {
    value <- annots[[pos]][[id]] %||% ""
    textInput(
      inputId = paste0("annot_", pos, "_", id),
      label = NULL,
      value = value,
      placeholder = pos,
      width = "80px" # ✅ Ajout de la largeur pleine cellule
    )
  }

  observeEvent(input$ped_dblclick, {
    req(pedigree$ped, values$pedData)
    pos_df <- positionDf()
    click <- input$ped_dblclick
    if (is.null(click)) {
      return()
    }
    pos_df$dist <- sqrt((pos_df$x - click$x)^2 + (pos_df$y - click$y)^2)
    idx <- which.min(pos_df$dist)
    if (pos_df$dist[idx] > 20) {
      return()
    }
    id_clicked <- as.character(labels(pedigree$ped)[pos_df$idInt[idx]])
    ind_row <- which(values$pedData$id == id_clicked)
    if (length(ind_row) == 0) {
      return()
    }
    infos <- values$pedData[ind_row, ]

    # Préparation des variables d'affichage (c'est la clef pour glue !)
    id_affiche <- infos$id
    nom_affiche <- infos$nom
    prenom_affiche <- infos$prénom
    age_affiche <- infos$age
    if (isTRUE(infos$sex == 2)) {
      sexe_affiche <- "♀︎"
    } else if (isTRUE(infos$sex == 1)) {
      sexe_affiche <- "♂︎"
    } else {
      sexe_affiche <- "⚧"
    }
    deces_affiche <- if (isTRUE(infos$deceased)) "✝︎" else ""

    modalData$row <- infos
    modalData$index <- ind_row
    modalInput$dob <- if (nzchar(infos$date_of_birth)) as.Date(infos$date_of_birth, "%d-%m-%Y") else NULL
    modalInput$dod <- if (nzchar(infos$date_of_death)) as.Date(infos$date_of_death, "%d-%m-%Y") else NULL
    modalInput$deceased <- isTRUE(infos$deceased)

    annot_positions <- c("topleft", "top", "topright", "left", "inside", "right", "bottomleft", "bottom", "bottomright")
    ind_id <- as.character(infos$id)
    currAnn <- annots$data

    # --- gestion du symbole sexe dans le modal, synchrone à la valeur du champ (utile si l'utilisateur le change dans le selectInput)
    local({
      this_ind_id <- ind_id
      output$modal_sex_symbol <- renderUI({
        sex_sel <- input$modal_sex
        if (is.null(sex_sel)) sex_sel <- as.numeric(infos$sex)
        HTML(getSexSymbolSVG(as.numeric(sex_sel), size = 150))
      })
    })

    showModal(modalDialog(
      title = HTML(glue::glue(
        "{id_clicked} <b>●</b> {infos$nom} <b>,</b> {infos$prénom}
        <b>{if (infos$sex == 2) '♀︎' else if (infos$sex == 1) '♂︎' else '⚧'}</b>
        {infos$age} <b>{if (isTRUE(infos$deceased)) '✝︎' else ''}</b>"
      )),
      fluidPage(
        fluidRow(
          bsCollapse(
            id = "collapseSection", open = NULL,
            bsCollapsePanel(
              "Annotations",
              # CSS
              tags$style(
                HTML("
      #grid-container {
        display: grid;
        gap: 10px;
        grid-template-columns: auto auto auto;
        grid-template-rows: auto auto auto;
        justify-content: center;
        align-items: center;
        margin-top: 20px;
      }

      #symbol {
        grid-column: 2;
        grid-row: 2;
        position: relative;
        width: 150px;
        height: 150px;
        display: flex;
        justify-content: center;
        align-items: center;
      }

      .sex-svg {
        position: absolute;
        top: -7px;
        left: 0;
        z-index: 1;
      }

      .inside-input {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 2;
        width: 90px;


        text-align: center;
        font-size: 13px;
        padding: 2px 4px;
        border-radius: 4px;
      }
    ")
              ),

              # GRID
              div(
                id = "grid-container",

                # Ligne 1
                div(class = "annot-input topleft", style = "grid-area: 1 / 1 / 2 / 2;", txtInp("topleft", ind_id, currAnn)),
                div(class = "annot-input top", style = "grid-area: 1 / 2 / 2 / 3; padding-left :40px;", txtInp("top", ind_id, currAnn)),
                div(class = "annot-input topright", style = "grid-area: 1 / 3 / 2 / 4;", txtInp("topright", ind_id, currAnn)),

                # Ligne 2
                div(class = "annot-input left", style = "grid-area: 2 / 1;", txtInp("left", ind_id, currAnn)),

                # Symbole + champ "inside"
                div(
                  id = "symbol",
                  uiOutput("modal_sex_symbol"),
                  div(class = "inside-input", style = "padding-left:10px;", txtInp("inside", ind_id, currAnn))
                ),
                div(class = "annot-input right", style = "grid-area: 2 / 3;", txtInp("right", ind_id, currAnn)),

                # Ligne 3
                div(class = "annot-input bottomleft", style = "grid-area: 3 / 1;", txtInp("bottomleft", ind_id, currAnn)),
                div(class = "annot-input bottomright", style = "grid-area: 3 / 3;", txtInp("bottomright", ind_id, currAnn)),
                div(class = "annot-input bottom", style = "grid-area: 3 / 2;padding-left :40px;", txtInp("bottom", ind_id, currAnn))
              )
            )
          )
        ),
        div(
          style = "display: flex; gap: 12px; align-items: flex-end; margin-bottom:16px;",
          textInput("modal_prenom", "Prénom", value = infos$prénom),
          textInput("modal_nom", "Nom", value = infos$nom),
          selectInput("modal_sex", "Sexe",
            choices = c("Femme" = 2, "Homme" = 1, "Inconnu" = 0),
            selected = infos$sex
          )
        ),
        hr(),
        div(
          airDatepickerInput(
            "modal_dob",
            "Date of birth",
            value = modalInput$dob,
            placeholder = "🗓️ Sélectionner la date",
            dateFormat = "dd/MM/yyyy",
            language = "fr",
            autoClose = TRUE,
            clearButton = TRUE
          ), ,
          checkboxInput("modal_deceased", "Décédé", value = isTRUE(infos$deceased)),
          uiOutput("modal_dod_ui"),
          uiOutput("modal_age_ui")
        ),
        hr(),
        textInput("modal_commentaire", "Commentaire", value = infos$commentaire)
      ),
      footer = tagList(
        modalButton("Fermer"),
        actionButton("save_modal", "Enregistrer", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))

    # Observe tous les inputs d'annotation
    lapply(annot_positions, function(pos) {
      observeEvent(input[[paste0("annot_", pos, "_", ind_id)]],
        {
          if (is.null(annots$data[[pos]])) annots$data[[pos]] <<- list()
          annots$data[[pos]][[ind_id]] <<- input[[paste0("annot_", pos, "_", ind_id)]]
          textAnnot(annots$data) # Réactivité immédiate sur le pedigree
        },
        ignoreInit = TRUE
      )
    })
  })

  # Synchronisation live des champs modaux classiques
  champs <- c("modal_prenom", "modal_nom", "modal_sex", "modal_dob", "modal_deceased", "modal_dod", "modal_age", "modal_commentaire")

  lapply(champs, function(champ) {
    observeEvent(input[[champ]],
      {
        # Sécurise l'accès à l'index
        req(!is.null(modalData$index), !is.null(values$pedData))
        df <- values$pedData
        idx <- modalData$index
        # Sécurise l'index
        if (is.null(idx) || length(idx) != 1 || idx > nrow(df)) {
          showNotification("Erreur interne : index de l’individu invalide.", type = "error")
          return(NULL)
        }
        id <- df[idx, "id"]
        tryCatch(
          {
            # Traitement champ par champ
            if (champ == "modal_prenom") df[idx, "prénom"] <- input$modal_prenom
            if (champ == "modal_nom") df[idx, "nom"] <- input$modal_nom

            if (champ == "modal_sex") {
              new_sex <- as.numeric(input$modal_sex)
              df[idx, "sex"] <- new_sex
              # Synchronise aussi l'objet pedigree (pour update le plot et le leader !)
              new_ped <- tryCatch(
                {
                  changeSex(pedigree$ped, id, sex = new_sex)
                },
                error = function(e) {
                  showNotification(paste("Erreur lors du changement de sexe :", e$message), type = "error")
                  NULL
                }
              )
              if (!is.null(new_ped)) {
                pedigree$ped <- new_ped
              }
            }

            if (champ == "modal_dob") {
              dob_value <- input$modal_dob
              if (!is.null(dob_value) && !is.na(dob_value)) {
                df[idx, "date_of_birth"] <- format(as.Date(dob_value), "%d-%m-%Y")
              } else {
                df[idx, "date_of_birth"] <- ""
              }
            }
            if (champ == "modal_deceased") df[idx, "deceased"] <- isTRUE(input$modal_deceased)
            if (champ == "modal_dod") {
              dod_value <- input$modal_dod
              if (!is.null(dod_value) && !is.na(dod_value)) {
                df[idx, "date_of_death"] <- format(as.Date(dod_value), "%d-%m-%Y")
              } else {
                df[idx, "date_of_death"] <- ""
              }
            }
            if (champ == "modal_age") df[idx, "age"] <- input$modal_age
            if (champ == "modal_commentaire") df[idx, "commentaire"] <- input$modal_commentaire

            # Correction des dates et de l'âge pour garder tout cohérent
            df <- tryCatch(
              {
                updatePedTableDates(df)
              },
              error = function(e) {
                showNotification(paste("Erreur lors de la mise à jour des dates :", e$message), type = "error")
                df # On renvoie le df original
              }
            )
            values$pedData <- df

            # (Optionnel : synchro du panneau latéral)
            # selectedIndiv$row <- df[idx, ]
          },
          error = function(e) {
            showNotification(paste("Erreur inattendue :", e$message), type = "error")
            # Ici, tu peux logger l’erreur ou faire d’autres actions si besoin
            return(NULL)
          }
        )
      },
      ignoreInit = TRUE
    )
  })




  # Mise à jour "live" des inputs du modal pour la synchro de l'âge
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
        "modal_dod",
        "Date of birth",
        value = modalInput$dod,
        placeholder = "🗓️ Sélectionner la date",
        dateFormat = "dd/MM/yyyy",
        language = "fr",
        autoClose = TRUE,
        clearButton = TRUE
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
    textInput("modal_age", "Âge", value = age_txt)
  })

  observeEvent(input$save_modal, {
    removeModal()
  })
  # Synchronisation live du champ "âge" vers les champs "date" dans la fenêtre modale
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

      n_years <- extract_units(age_txt, "an|ans")
      n_months <- extract_units(age_txt, "mois")
      n_days <- extract_units(age_txt, "jour|jours")
      age_saisi <- (n_years + n_months + n_days) > 0

      if (age_saisi && (is.na(dob) || dob == "" || dob == "NA") && !deceased) {
        birth_estimate <- tryCatch(
          compute_relative_date(Sys.Date(), n_years, n_months, n_days, "backward"),
          error = function(e) NA
        )
        df$date_of_birth[i] <- format(birth_estimate, "%d-%m-%Y")
      }
      if (age_saisi && deceased && !is.na(dob) && dob != "" && (is.na(dod) || dod == "" || dod == "NA")) {
        death_estimate <- tryCatch(
          compute_relative_date(as.Date(dob, "%d-%m-%Y"), n_years, n_months, n_days, "forward"),
          error = function(e) NA
        )
        df$date_of_death[i] <- format(death_estimate, "%d-%m-%Y")
      }
      if (age_saisi && deceased && !is.na(dod) && dod != "" && (is.na(dob) || dob == "" || dob == "NA")) {
        birth_estimate <- tryCatch(
          compute_relative_date(as.Date(dod, "%d-%m-%Y"), n_years, n_months, n_days, "backward"),
          error = function(e) NA
        )
        df$date_of_birth[i] <- format(birth_estimate, "%d-%m-%Y")
      }
      if ((!age_saisi) && (is.na(dob) || dob == "") && (is.na(dod) || dod == "")) {
        df$age[i] <- ""
      } else {
        dob2 <- df$date_of_birth[i]
        dod2 <- ifelse(deceased, df$date_of_death[i], NA)
        df$age[i] <- calculateAgeText(dob2, dod2)
      }
    }
    return(df)
  }
  output$download_pedigree <- downloadHandler(
    filename = function() {
      paste0("pedigree_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Taille de l'image PNG (en pouces)
      width <- 10
      height <- 12
      dpi <- 300

      png(file, width = width, height = height, units = "in", res = dpi)

      layout(matrix(c(1, 2, 3), ncol = 1), heights = c(0.4, 0.2, 0.4))
      par(mar = c(1, 1, 3, 1))

      ## 1. --- PLOT PEDIGREE ---
      if (!is.null(pedigree$ped)) {
        align <- plotAlignment()
        annot <- plotAnnotation()
        sc <- plotScaling()
        drawPed(align, annotation = annot, scaling = sc)
        title(main = input$plotTitle %||% "Pedigree", cex.main = 1.5)
      } else {
        plot.new()
        text(0.5, 0.5, "Aucun pedigree sélectionné", cex = 1.4)
      }

      ## 2. --- LEGENDE SIMPLIFIÉE ---
      plot.new()
      legend("center",
        legend = c("Homme", "Femme", "Inconnu", "Décédé", "Porteur", "Fausse couche"),
        pch = c(15, 19, 18, 4, 16, 2),
        pt.cex = 2,
        col = "black", bty = "n", cex = 1.2
      )

      ## 3. --- TABLEAU DES DONNÉES ---
      plot.new()
      if (!is.null(values$pedData)) {
        gridExtra::grid.table(values$pedData, rows = NULL)
      } else {
        text(0.5, 0.5, "Aucune donnée disponible", cex = 1.2)
      }

      dev.off()
    }
  )
  output$modal_sex_symbol <- renderUI({
    sex_sel <- input$modal_sex
    if (is.null(sex_sel)) sex_sel <- as.numeric(infos$sex)
    HTML(getSexSymbolSVG(as.numeric(sex_sel), size = 150))
  })

  ### PREGNANCY SERVEUR  --------------
  observeEvent(input$calculer, {
    req(input$ddr)
    today <- Sys.Date()

    # Validation
    if (input$ddr > today) {
      shinyFeedback::showFeedbackDanger("ddr", "La date de DDR ne peut pas être dans le futur.")
      output$resultats <- renderUI({
        NULL
      })
      output$calendrier <- DT::renderDataTable({
        NULL
      })
      output$timeline <- renderTimevis({
        NULL
      })
      return()
    } else {
      shinyFeedback::hideFeedback("ddr")
    }

    # Calculs
    suivi_info <- calcul_suivi(input$ddr, input$cycle)
    suivi <- suivi_info$suivi
    dpa <- suivi_info$dpa
    info_grossesse <- get_amenorrhee(input$ddr, today, input$cycle)

    # Résumé grossesse
    output$resultats <- renderUI({
      HTML(paste0(
        "<b>📌 DDR :</b> ", format(input$ddr, "%d/%m/%Y"), "<br>",
        "<b>🔄 Durée du cycle :</b> ", input$cycle, " jours<br>",
        "<b>🎯 DPA estimée :</b> ", format(dpa, "%d/%m/%Y"), "<br>",
        "<b>🗓️ Semaine d'aménorrhée actuelle :</b> ", info_grossesse$sems, " SA<br>",
        "<b>🤰 Mois de grossesse :</b> ", info_grossesse$mois, "e mois<br>",
        "<b>⏳ Jours restants avant DPA :</b> ", info_grossesse$jours_restants, " jours"
      ))
    })

    # Progression
    updateProgressBar(
      session = session,
      id = "progress_grossesse",
      value = round(100 * as.numeric(info_grossesse$sems) / 40),
      total = 100
    )

    # Table
    suivi_affiche <- suivi %>%
      mutate(
        Date = format(Date, "%d/%m/%Y"),
        Prochaine = ifelse(as.Date(Date, "%d/%m/%Y") >= today,
          "<span class='prochaine-td'>À venir</span>",
          "<span style='color:#bbc0ca;'>Passée</span>"
        )
      )
    output$calendrier <- DT::renderDT({
      datatable(
        suivi_affiche[, c("Étape", "Date", "Prochaine")],
        rownames = FALSE,
        options = list(dom = "tp", pageLength = 8, searching = FALSE, ordering = FALSE),
        escape = FALSE,
        class = "compact stripe cell-border"
      )
    })

    # Prochaine étape
    etapes_a_venir <- suivi %>% filter(Date >= today)
    if (nrow(etapes_a_venir) > 0) {
      prochaine <- etapes_a_venir %>% slice(1)
      output$prochaine_etape <- renderUI({
        tags$div(
          class = "glass-alert",
          paste0("🔔 Prochaine étape : <b>", prochaine$Étape, "</b> prévue le <b>", format(prochaine$Date, "%d/%m/%Y"), "</b>")
        )
      })
    } else {
      output$prochaine_etape <- renderUI({
        NULL
      })
    }

    # Frise chronologique
    frise_semaines <- tibble(
      id = 1:40,
      content = paste0("S ", 1:40, "<br><span style='font-size:0.9em;'>", format(input$ddr + weeks(0:39), "%d/%m"), "</span>"),
      start = input$ddr + weeks(0:39),
      end = input$ddr + weeks(0:39) + days(6),
      type = "range",
      group = "Semaines"
    )
    frise_evenements <- tibble(
      id = 100 + 1:8,
      content = paste0(
        "<b>", suivi$Étape, "</b><br><span style='color:#b92577; font-size:0.95em;'>", format(suivi$Date, "%d/%m"), "</span>"
      ),
      start = suivi$Date,
      end = NA,
      type = NA,
      group = "Événements"
    )
    frise_complete <- bind_rows(frise_semaines, frise_evenements)
    groupes <- tibble(
      id = c("Événements", "Semaines"),
      content = c("📍 Étapes", "📆 Semaines")
    )

    output$timeline <- renderTimevis({
      timevis(
        data = frise_complete,
        groups = groupes,
        options = list(
          stack = TRUE,
          showCurrentTime = TRUE,
          zoomable = TRUE,
          showMajorLabels = TRUE,
          showMinorLabels = TRUE,
          timeAxis = list(scale = "week", step = 1),
          orientation = "top"
        )
      )
    })
  })
  ### ORPHAET SERVER -----------------
  output$dynamic_input <- renderUI({
    req(input$search_mode)
    if (input$search_mode == "name") {
      textInput("disease_name", "Nom de la pathologie :", value = "CYSTIC FIBROSIS")
    } else {
      textInput("orphacode", "Code ORPHA :", value = "586")
    }
  })

  observeEvent(input$go, {
    output$error_text <- renderText("")
    output$disease_name <- renderUI(NULL)
    output$definition <- renderUI(NULL)
    output$info_block <- renderUI(NULL)
    output$natural_history <- renderUI(NULL)
    output$hpo_table <- renderDT(NULL)
    output$epi_table <- renderDT(NULL)
    output$gene_table <- renderDT(NULL)

    lang <- input$lang
    url <- NULL

    if (input$search_mode == "name") {
      name <- trimws(input$disease_name)
      if (name == "") {
        output$error_text <- renderText("❗ Veuillez entrer un nom.")
        return()
      }
      url <- paste0("https://api.orphadata.com/rd-cross-referencing/orphacodes/names/", URLencode(name), "?lang=", lang)
    } else {
      code <- trimws(input$orphacode)
      if (code == "") {
        output$error_text <- renderText("❗ Veuillez entrer un code.")
        return()
      }
      url <- paste0("https://api.orphadata.com/rd-cross-referencing/orphacodes/", code, "?lang=", lang)
    }

    resp <- tryCatch(GET(url, add_headers("accept" = "application/json")), error = function(e) NULL)
    if (is.null(resp) || http_error(resp)) {
      output$error_text <- renderText("❌ Erreur d'accès à l'API principale.")
      return()
    }
    json <- tryCatch(content(resp, as = "parsed", simplifyDataFrame = FALSE), error = function(e) NULL)
    data <- tryCatch(
      {
        json$data$results %||% json$results
      },
      error = function(e) NULL
    )

    if (is.null(data)) {
      output$error_text <- renderText("⚠️ Données introuvables.")
      return()
    }

    output$disease_name <- renderUI({
      h3(data$`Preferred term` %||% "Nom non disponible")
    })
    def <- tryCatch(
      {
        if (!is.null(data$SummaryInformation)) data$SummaryInformation[[1]]$Definition %||% "Non disponible" else "Non disponible"
      },
      error = function(e) "Non disponible"
    )

    output$definition <- renderUI({
      HTML(sprintf("<div style='border:1px solid #d33;padding:10px;'><b style='color:#d33;'>Définition :</b><br>%s</div>", def))
    })

    extract_refs <- function(source) {
      tryCatch(
        {
          refs <- Filter(function(x) x$Source == source, data$ExternalReference)
          paste0(sapply(refs, function(x) x$Reference), collapse = ", ")
        },
        error = function(e) ""
      )
    }

    icd10 <- extract_refs("ICD-10")
    icd11 <- extract_refs("ICD-11")
    omim <- extract_refs("OMIM")
    umls <- extract_refs("UMLS")
    mesh <- extract_refs("MeSH")
    gard <- extract_refs("GARD")
    meddra <- extract_refs("MedDRA")

    synonyms <- tryCatch(paste(unlist(data$Synonym), collapse = "<br>"), error = function(e) "Aucun")
    orpha_code_final <- data$ORPHAcode
    orphalink <- data$OrphanetURL

    output$info_block <- renderUI({
      HTML(sprintf(
        "<div style='background:#f5f5f5;padding:15px;border-radius:8px;'>
        <b>ORPHA:%s</b><br><a href='%s' target='_blank'>Fiche Orphanet</a><br><br>
        <b>Synonymes :</b><br>%s<br><br>
        <b>Références :</b><br>
        CIM-10 : %s<br>CIM-11 : %s<br>OMIM : %s<br>UMLS : %s<br>MeSH : %s<br>GARD : %s<br>MedDRA : %s</div>",
        orpha_code_final, orphalink, synonyms, icd10, icd11, omim, umls, mesh, gard, meddra
      ))
    })

    fetch_table <- function(url, extract_func) {
      tryCatch(
        {
          response <- GET(url, add_headers("accept" = "application/json"))
          if (http_error(response)) {
            return(NULL)
          }
          extract_func(content(response, as = "parsed", simplifyDataFrame = FALSE))
        },
        error = function(e) NULL
      )
    }

    # Histoire naturelle
    nat_data <- fetch_table(
      paste0("https://api.orphadata.com/rd-natural_history/orphacodes/", orpha_code_final, "?lang=", lang),
      function(json) json$data$results
    )

    if (!is.null(nat_data)) {
      onset <- nat_data$AverageAgeOfOnset %||% "Inconnu"
      death <- nat_data$AverageAgeOfDeath %||% "Inconnu"
      inherit <- tryCatch(paste(unlist(nat_data$TypeOfInheritance), collapse = ", "), error = function(e) "Inconnu")
      output$natural_history <- renderUI({
        HTML(sprintf("<div style='background:#eef1f4;padding:15px;border-radius:8px;'>
          <h4>\U0001F9ED Histoire naturelle</h4>
          <b>Âge d’apparition :</b> %s<br><b>Âge de décès :</b> %s<br><b>Hérédité :</b> %s</div>", onset, death, inherit))
      })
    }

    # HPO
    hpo_data <- fetch_table(
      paste0("https://api.orphadata.com/rd-phenotypes/orphacodes/", orpha_code_final, "?lang=", lang),
      function(json) json$data$results$Disorder$HPODisorderAssociation
    )

    if (!is.null(hpo_data)) {
      hpo_df <- tryCatch(
        {
          data.frame(
            HPO_ID = sapply(hpo_data, function(x) x$HPO$HPOId %||% NA),
            Terme = sapply(hpo_data, function(x) x$HPO$HPOTerm %||% NA),
            Fréquence = sapply(hpo_data, function(x) x$HPOFrequency %||% NA),
            stringsAsFactors = FALSE
          )
        },
        error = function(e) NULL
      )
      if (!is.null(hpo_df)) output$hpo_table <- renderDT(datatable(hpo_df))
    }

    # Épidémiologie
    epi_data <- fetch_table(
      paste0("https://api.orphadata.com/rd-epidemiology/orphacodes/", orpha_code_final, "?lang=", lang),
      function(json) json$data$results$Prevalence
    )

    if (!is.null(epi_data)) {
      epi_df <- tryCatch(
        {
          data.frame(
            Type = sapply(epi_data, `[[`, "PrevalenceType"),
            Classe = sapply(epi_data, `[[`, "PrevalenceClass"),
            Moyenne = sapply(epi_data, `[[`, "ValMoy"),
            Région = sapply(epi_data, `[[`, "PrevalenceGeographic"),
            Source = sapply(epi_data, `[[`, "Source"),
            stringsAsFactors = FALSE
          )
        },
        error = function(e) NULL
      )
      if (!is.null(epi_df)) output$epi_table <- renderDT(datatable(epi_df))
    }

    # Gènes associés
    gene_data <- fetch_table(
      paste0("https://api.orphadata.com/rd-associated-genes/orphacodes/", orpha_code_final),
      function(json) json$data$results$DisorderGeneAssociation
    )

    if (!is.null(gene_data)) {
      gene_df <- tryCatch(
        {
          data.frame(
            Symbole = sapply(gene_data, function(g) g$Gene$Symbol %||% NA),
            Nom = sapply(gene_data, function(g) g$Gene$name %||% NA),
            Locus = sapply(gene_data, function(g) g$Gene$Locus[[1]]$GeneLocus %||% NA),
            Type = sapply(gene_data, function(g) g$DisorderGeneAssociationType %||% NA),
            Source = sapply(gene_data, function(g) g$SourceOfValidation %||% NA),
            stringsAsFactors = FALSE
          )
        },
        error = function(e) NULL
      )
      if (!is.null(gene_df)) output$gene_table <- renderDT(datatable(gene_df))
    }
  })
}

# ------------------ LANCEMENT APPLICATION ------------------
shinyApp(ui = ui, server = server)
