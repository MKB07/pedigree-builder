# Archived R development file
# Original path: 🧩 Versions_Support/Base/work_base.R
# Original created: 2025-09-06 23:30:36
# Original modified: 2025-10-26 06:05:23
# Archive rationale: Large intermediate base snapshot showing consolidation of core pedigree workflows.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# -------------------- LIBRAIRIES -----------------------
library(shiny)
library(pedtools)
library(bslib)
library(officer)

library(htmltools)
library(sortable)
if (!exists("isCount", mode = "function")) {
  isCount <- function(x) {
    is.numeric(x) && length(x) == 1 && is.finite(x) &&
      x >= 0 && (x == floor(x))
  }
}
# ------------------ LISTE DES PEDIGREES -----------------
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
# ====================== FONCTIONS UTIL ======================
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
.makeRichLabel <- function(id, ped_data) {
  ligne <- ped_data[ped_data$id == id, , drop = FALSE]
  if (nrow(ligne) == 0) {
    # fallback si absence (ne devrait pas arriver)
    return(as.character(id))
  }
  # Sexe → symbole
  sexe_chr <- as.character(if ("sex" %in% names(ligne)) ligne$sex[1] else NA)
  sexe_icon <- if (!is.na(sexe_chr) && nzchar(sexe_chr)) {
    if (sexe_chr %in% c("1", "M", "Male", "male")) {
      "♂︎"
    } else if (sexe_chr %in% c("2", "F", "Female", "female")) {
      "♀︎"
    } else {
      "⚬"
    }
  } else {
    "⚬"
  }

  # Décès → croix
  decede_icon <- if ("deceased" %in% names(ligne) && isTRUE(ligne$deceased[1])) " ✝︎" else ""

  # Age (texte), optionnel
  age_str <- if ("age" %in% names(ligne) && !is.na(ligne$age[1]) && nzchar(ligne$age[1])) paste0(", ", ligne$age[1]) else ""

  ln <- if ("last_name" %in% names(ligne) && nzchar(ligne$last_name[1])) as.character(ligne$last_name[1]) else ""
  fn <- if ("first_name" %in% names(ligne) && nzchar(ligne$first_name[1])) as.character(ligne$first_name[1]) else ""

  lab <- paste0(id, ", ", ln, ", ", fn, " ", sexe_icon, decede_icon, age_str)
  gsub("\\s+", " ", trimws(lab))
}

# --------- Sexe ---------
changeSex <- function(ped, id, sex) pedtools::setSex(ped, ids = id, sex = sex)

# --------- Partenaires ---------
getPartners <- function(ped, id) {
  kids <- pedtools::children(ped, id)
  if (length(kids) == 0) {
    return(NULL)
  }
  partnerList <- lapply(kids, function(kid) setdiff(pedtools::parents(ped, kid), id))
  unique(unlist(partnerList))
}

# Un partenaire si unique, sinon NULL (cas géré via modale ailleurs)
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
sanitizeTitle <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("\\s{2,}", " ", x)
  trimws(x)
}

createRandomPedigreeModal <- function() {
  modalDialog(
    title = "Random Pedigree Preview",
    actionButton("reroll_btn", "Reroll"),
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
# --------- Ajout d'enfant avec (ou sans) partenaire ---------
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

# --------- Ajout de parents ---------
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

# --------- Ajout d’un sibling ---------
addSib <- function(x, id, sex = 1, side = c("right", "left")) {
  if (length(id) != 1) stop(sprintf("Pour ajouter un sibling, sélectionnez un seul individu. Sélection actuelle : %s", paste(id, collapse = ",")))
  if (!pedtools::is.ped(x)) stop("Impossible d’ajouter un sibling à un pedigree invalide.")
  if (id %in% pedtools::founders(x)) x <- pedtools::addParents(x, id, verbose = FALSE)
  pars <- pedtools::parents(x, id)
  if (length(pars) != 2 || any(is.na(pars))) stop("Parents inconnus/incomplets ; impossible d’ajouter un sibling.")
  newped <- pedtools::addChildren(x, father = pars[1], mother = pars[2], sex = sex, verbose = FALSE)
  idInt <- pedtools::internalID(x, id)
  n <- length(x$ID)
  ord <- switch(match.arg(side),
    left  = c(seq_len(idInt - 1), n + 1, idInt:n),
    right = c(seq_len(idInt), n + 1, if (idInt < n) seq.int(idInt + 1, n))
  )
  pedtools::reorderPed(newped, ord)
}

# --------- Triplets (2 nouveaux siblings) ---------
addTriplets <- function(ped, id, sexes = c(1, 2)) {
  stopifnot(length(id) == 1)
  if (id %in% pedtools::founders(ped)) ped <- pedtools::addParents(ped, id, verbose = FALSE)
  parents <- pedtools::parents(ped, id)
  ped2 <- pedtools::addChild(ped, parents, sex = sexes[1], verbose = FALSE)
  ped3 <- pedtools::addChild(ped2, parents, sex = sexes[2], verbose = FALSE)
  new_ids <- setdiff(labels(ped3), labels(ped))
  if (length(new_ids) != 2) stop("Error when adding triplets")
  triplet_ids <- sort(c(id, new_ids))
  list(ped = ped3, triplet_ids = triplet_ids)
}

generateNewId <- function(ped) {
  # Find the next available numeric ID
  existing_ids <- as.character(labels(ped))
  # If your IDs are numeric, take max+1
  numeric_ids <- suppressWarnings(as.integer(existing_ids))
  new_id <- as.character(max(numeric_ids, na.rm = TRUE) + 1)
  new_id
}

formatAnnot <- function(textAnnot, cex, font = 2, col = "blue") {
  if (is.null(textAnnot)) {
    return(NULL)
  }
  lapply(textAnnot, function(b) list(b, cex = cex, font = font, col = col))
}

# ----------------- STYLES -----------------
styles_css <- "
@import url('https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined');

body { font-family: 'Helvetica Neue', Arial, sans-serif; color:#0f172a; }
h4, .h4, h5 { margin-top:8px; margin-bottom:8px; font-weight:600; color:#111827; }
.text-muted { color:#6b7280; }
.mt-1{margin-top:6px;} .mt-2{margin-top:12px;} .mt-3{margin-top:18px;}
.mb-1{margin-bottom:6px;} .mb-2{margin-bottom:12px;} .mb-3{margin-bottom:18px;}
.box { background:#fff; border:1px solid #e5e7eb; border-radius:10px; padding:12px 14px; margin-bottom:14px; box-shadow:0 1px 2px rgba(0,0,0,.03); }
.box-header { font-size:14px; font-weight:700; text-transform:uppercase; letter-spacing:.04em; color:#6b7280; margin-bottom:8px; }
.btn-sm { padding:5px 9px; font-size:12px; }
.legend-item-text { margin-left:4px; }
.mark { display:inline-flex; align-items:center; margin-right:6px; }
"

# ----------------- INTERFACE UTILISATEUR -----------------
ui <- fluidPage(
  tags$head(tags$style(HTML(styles_css))),

  # Titre
  div(class = "text-center", style = "font-size:28px; font-weight:700; padding:16px 0 4px;", "PEDIGREE APPLICATION"),
  div(class = "text-center text-muted", "Interactively build & annotate pedigrees"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      div(
        class = "box",
        div(class = "box-header", "Pedigree setup"),
        actionButton(
          "select_list", "select_list"
        ),
        actionButton(
          "randomPed", "randomPed"
        )
      ),
      div(
        class = "box",
        tabsetPanel(
          id = "tabset", type = "tabs",

          # ------------ ONGLET FAMILY ------------
          tabPanel(
            title = "FAMILY",
            br(),
            h5("Parents"),
            div(
              class = "mb-2",
              actionButton("addparents", "Add parents", class = "btn btn-primary btn-sm")
            ),
            h5("Siblings"),
            div(
              class = "mb-2",
              actionButton("sister", "Add sister", class = "btn btn-default btn-sm"),
              actionButton("brother", "Add brother", class = "btn btn-default btn-sm"),
              actionButton("sib_unknown", "Undefined sib", class = "btn btn-default btn-sm"),
              tags$span(" "),
              actionButton("Twins", "Add twins", class = "btn btn-info btn-sm"),
              actionButton("Triplets", "Add triplets", class = "btn btn-info btn-sm"),
              actionButton("half", "Add half sib", class = "btn btn-default btn-sm")
            ),
            h5("Children"),
            div(
              class = "mb-2",
              actionButton("child_daughter", "Add daughter", class = "btn btn-success btn-sm"),
              actionButton("child_son", "Add son", class = "btn btn-success btn-sm"),
              actionButton("child_unknown", "Add undefined child", class = "btn btn-default btn-sm")
            ),
            div(
              class = "mb-2",
              actionButton("add_Miscarriage", "Add miscarriage", class = "btn btn-danger btn-sm"),
              actionButton("choose_partner", "Add partner", class = "btn btn-default btn-sm")
            )
          ),

          # ------------ ONGLET LEGEND ------------
          tabPanel(
            title = "LEGEND",
            br(),
            h5("Sex"),
            div(
              class = "mb-2",
              actionButton("Male",
                label = tagList(
                  div(class = "mark", tags$span(class = "material-symbols-outlined", "check_box_outline_blank")),
                  span(class = "legend-item-text", "Male")
                ),
                class = "btn btn-default btn-sm"
              ),
              actionButton("Female",
                label = tagList(
                  div(class = "mark", tags$span(class = "material-symbols-outlined", "circle")),
                  span(class = "legend-item-text", "Female")
                ),
                class = "btn btn-default btn-sm"
              ),
              actionButton("Unknown",
                label = tagList(
                  div(class = "mark", tags$span(class = "material-symbols-outlined", "thermostat_carbon")),
                  span(class = "legend-item-text", "Unknown")
                ),
                class = "btn btn-default btn-sm"
              )
            ),
            h5("Traits & status"),
            div(
              class = "mb-2",
              # ⚠ Adopted = toggle + dessine des crochets
              actionButton("Adopted",
                label = tagList(
                  div(class = "mark", tags$span(class = "material-symbols-outlined", "data_array")),
                  span(class = "legend-item-text", "Adopted (brackets)")
                ),
                class = "btn btn-warning btn-sm"
              ),
              actionButton("Carrier",
                label = tagList(
                  div(class = "mark", tags$span(class = "material-symbols-outlined", "control_camera")),
                  span(class = "legend-item-text", "Carrier")
                ),
                class = "btn btn-default btn-sm"
              ),
              actionButton("Proband",
                label = tagList(
                  div(class = "mark", tags$span(class = "material-symbols-outlined", "right_click")),
                  span(class = "legend-item-text", "Proband")
                ),
                class = "btn btn-primary btn-sm"
              ),
              actionButton("Deceased",
                label = tagList(
                  div(class = "mark", tags$span(class = "material-symbols-outlined", "frame_person_off")),
                  span(class = "legend-item-text", "Deceased")
                ),
                class = "btn btn-danger btn-sm"
              ),
              actionButton("Miscarriage",
                label = tagList(
                  div(class = "mark", tags$span(class = "material-symbols-outlined", "change_history")),
                  span(class = "legend-item-text", "Miscarriage")
                ),
                class = "btn btn-danger btn-sm"
              ),
              actionButton("Starred",
                label = tagList(
                  div(class = "mark", tags$span(class = "material-symbols-outlined", "grade")),
                  span(class = "legend-item-text", "Starred")
                ),
                class = "btn btn-default btn-sm"
              )
            ),
            div(
              style = "display: flex; flex-direction: column;",
              actionButton(
                "newPheno",
                label = tagList(
                  span("Add Phenotype"),
                  div(class = "plus-btn", tags$span(class = "material-symbols-outlined", "add"))
                ),
                class = "footer-btn", title = "Ajouter un phénotype",
              ),
              div(uiOutput("phenoButtonsUI"))
            )
          ),

          # ------------ ONGLET PERSONAL ------------
          tabPanel(
            title = "PERSONAL",
            br()
          ),
          tabPanel(
            title = "REORDER",
            br()
          )
        )
      ),
      div(
        class = "box",
        div(class = "box-header", "Current selection"),
        textOutput("selectedIndividual")
      ),
      downloadButton("savePlotPng", "PNG", icon = icon("file-image"), class = "btn btn-primary btn-block mb-2"),
      downloadButton("savePlotPdf", "PDF", icon = icon("file-pdf"), class = "btn btn-primary btn-block"),
      downloadButton("savePed", "PED", icon = icon("table"), class = "btn btn-primary btn-block mt-2")
    ),
    mainPanel(
      width = 7,
      div(
        class = "box",
        div(class = "box-header", "Pedigree"),
        plotOutput("plot", height = "520px", click = "ped_click")
      ),
      br(),
      absolutePanel(
        style = "background: rgba(214, 219, 225, 0.52);
border-radius: 16px;
box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
backdrop-filter: blur(20px);
-webkit-backdrop-filter: blur(20px);
border: 1px solid rgba(214, 219, 225, 0.7); ",
        draggable = TRUE, cursor = "move",
        card(style = "padding:12px;", DT::dataTableOutput("pedTableDT"))
      )
    )
  )
)

# ------------------ SERVER ------------------
server <- function(input, output, session) {
  # --------- Réactifs principaux ---------
  selectedIndiv <- reactiveValues(row = NULL, index = NULL)
  pedigree <- reactiveValues(ped = NULL, twins = NULL, miscarriage = NULL)
  sel <- reactiveVal(character(0))
  relText <- reactiveVal(NULL)
  styles <- reactiveValues(
    hatched = NULL, carrier = NULL, deceased = NULL, proband = NULL,
    adopted = NULL, # <- utilisé aussi pour dessiner les crochets
    aff = NULL, starred = NULL, title = NULL, fill = NULL, dashed = NULL
  )
  values <- reactiveValues(pedData = NULL)
  textAnnot <- reactiveVal(NULL)
  viewState <- reactiveValues(backup = NULL)
  randomVars <- reactiveValues(ped = NULL)
  modalVars <- reactiveValues(previewPed = NULL, pedChoice = "", pedTitle = "")
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
  # Helper générique: ajoute/retire un id d'un vecteur
  toggleId <- function(vec, id) {
    v <- vec %||% character(0)
    if (id %in% v) setdiff(v, id) else union(v, id)
  }
  # --- Caches & Ordres personnalisés (par individu sélectionné) ---

  phenotypes <- reactiveValues(list = list(), assign = list())
  pheno_editing <- reactiveVal(NULL)
  # --- util : récupère l'id depuis un label "ID, NOM, Prénom ..."

  # ========= Paramètres visuels des “crochets” (brackets) =========
  bracketParams <- reactiveValues(
    col = "#f59e0b", # amber
    lwd = 2,
    scale_cex = 1.35,
    offsetX = -0.02,
    offsetY = 0,
    gap_factor = 0.28,
    radius_factor = 0.18,
    vertical_factor = 1
  )

  ## Modale : sélection d'un template -------------
  observeEvent(input$select_list, {
    modalVars$previewPed <- NULL
    modalVars$pedChoice <- ""
    modalVars$pedTitle <- ""
    showModal(createPedigreeModal())
  })

  observeEvent(input$modalPedChoice, {
    req(nzchar(input$modalPedChoice))
    modalVars$pedChoice <- input$modalPedChoice
    pedigree_fun <- pedigree_list[[input$modalPedChoice]]
    if (is.function(pedigree_fun)) {
      preview <- tryCatch(pedigree_fun(), error = function(e) NULL)
      if (is.null(preview)) showNotification("Error generating preview for this pedigree type.", type = "error")
      modalVars$previewPed <- preview
    } else {
      showNotification("Unknown pedigree model selected.", type = "error")
      modalVars$previewPed <- NULL
    }
  })

  output$modalPreviewPed <- renderPlot({
    req(modalVars$previewPed)
    preview_ped <- tryCatch(pedtools::relabel(modalVars$previewPed, new = "generations"), error = function(e) NULL)
    req(preview_ped)
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mar = c(1, 1, 2, 1))
    plot(preview_ped, cex = 1.2)
    safe_title <- sanitizeTitle(input$modalPedTitle)
    if (nzchar(safe_title)) title(main = safe_title, cex.main = 1.1, col.main = "#3c8dbc")
  })

  observeEvent(input$modalConfirmPed, {
    req(modalVars$previewPed)
    safe_title <- sanitizeTitle(input$modalPedTitle)
    if (isTRUE(setPedigree(modalVars$previewPed, safe_title))) {
      removeModal()
      sel(character(0))
      selectedIndiv$row <- NULL
      selectedIndiv$index <- NULL
    }
  })
  # ---------- REROLL (regenère un pedigree aléatoire dans la modale) ----------
  observeEvent(input$reroll_btn, {
    ped <- NULL
    # essaie quelques fois pour éviter un échec ponctuel
    for (tries in 1:5) {
      ped <- tryCatch(
        pedtools::randomPed(n = sample(5:10, 1), founders = sample(1:3, 1)),
        error = function(e) NULL
      )
      if (!is.null(ped)) break
    }
    if (is.null(ped)) {
      showNotification("Random pedigree generation failed.", type = "error")
      return(invisible(NULL))
    }
    # met à jour l'objet réactif -> le plot de prévisualisation se repeint
    randomVars$ped <- ped
  })

  ## Modale : génération aléatoire --------
  observeEvent(input$randomPed, {
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


  # --------- Données du pedigree ---------
  updatePedData <- function() {
    ped <- pedigree$ped
    if (is.null(ped)) {
      return()
    }
    n <- length(labels(ped))
    deceased <- rep(FALSE, n)
    if (!is.null(values$pedData) && "deceased" %in% colnames(values$pedData)) {
      old <- values$pedData
      match_idx <- match(labels(ped), old$id)
      deceased <- ifelse(!is.na(match_idx), old$deceased[match_idx], FALSE)
    }
    values$pedData <- data.frame(
      id = labels(ped),
      sex = ped$SEX,
      deceased = deceased,
      stringsAsFactors = FALSE
    )
  }

  updatePedigree <- function(newped) {
    pedigree$ped <- newped
    pedigree$ped <- relabel(pedigree$ped, new = "generations")
    updatePedData()
  }

  # --------- Sélection du pedigree ---------
  observeEvent(input$pedChoice, {
    req(input$pedChoice != "")
    pedigree_fun <- pedigree_list[[input$pedChoice]]
    ped <- pedigree_fun()
    updatePedigree(ped)
    sel(character(0))
    selectedIndiv$row <- NULL
    selectedIndiv$index <- NULL
  })
  # Transforme un pedtools en data.frame enrichi
  makePedData <- function(ped) {
    if (is.null(ped)) {
      return(NULL)
    }
    df <- as.data.frame(ped, stringsAsFactors = FALSE)
    extra_cols <- c("first_name", "last_name", "date_of_birth", "deceased", "date_of_death", "age", "comments")
    for (col in extra_cols) if (!(col %in% names(df))) df[[col]] <- if (col == "deceased") FALSE else ""
    df
  }
  # --------- Plot helpers (internes pedtools) ---------
  plotAlignment <- reactive({
    req(pedigree$ped)
    .pedAlignment(
      pedigree$ped,
      proband = styles$proband,
      twins = pedigree$twins,
      miscarriage = pedigree$miscarriage,
      arrows = FALSE,
      align = c(1.5, 2)
    )
  })
  updatePedData <- function() {
    values$pedData <- if (is.null(pedigree$ped)) NULL else makePedData(pedigree$ped)
  }

  plotLabs <- reactive({
    req(pedigree$ped)
    breakLabs(pedigree$ped)
  })

  plotAnnotation <- reactive({
    req(pedigree$ped)
    # jeu d’IDs à surligner si toggle actif et sélection présente


    # couleur de remplissage: on ajoute une entrée nommée dans styles$fill
    fill_map <- list()

    annot <- .pedAnnotation(
      pedigree$ped,
      labs = plotLabs(),
      hatched = styles$hatched,
      hatchDensity = 20,
      carrier = styles$carrier,
      proband = styles$proband,
      deceased = styles$deceased,
      starred = styles$starred,
      # adopted = styles$adopted,  # décommente si ta version de pedtools supporte ce champ
      textAnnot = formatAnnot(textAnnot(), 1.2),
      col = list("#3c8dbc" = sel()),
      fill = if (length(styles$fill) > 0) unlist(styles$fill) else NA,
      lty = list(dashed = styles$dashed),
      lwd = list(`3` = sel(), `1.5` = setdiff(styles$dashed, sel()))
    )
    annot
  })

  plotScaling <- reactive({
    req(pedigree$ped)
    .pedScaling(plotAlignment(), plotAnnotation(), cex = 1.35, symbolsize = 1, margins = rep(3, 4))
  })

  # centres + dimensions boîtes (pour dessiner les crochets)
  positionDf <- reactive({
    req(pedigree$ped)
    al <- plotAlignment()
    sc <- plotScaling()
    data.frame(
      id_plot = al$plotord,
      xc = al$xall + sc$boxw / 2,
      yc = al$yall + sc$boxh / 2,
      boxw = sc$boxw,
      boxh = sc$boxh
    )
  })

  # utilitaire: quart d’arc (polyligne)
  draw_arc <- function(cx, cy, r, from, to, n = 20, col = "red", lwd = 2) {
    n <- as.integer(n)
    if (is.na(n) || n < 2) n <- 20
    r <- as.numeric(r)
    if (!is.finite(r) || r <= 0) {
      return(invisible(NULL))
    }
    ang <- seq(from, to, length.out = n)
    lines(cx + r * cos(ang), cy + r * sin(ang), col = col, lwd = lwd, xpd = NA)
  }

  # crochets arrondis
  draw_rounded_brackets <- function(xc, yc, bw, bh,
                                    scale_cex = 1, col = "red", lwd = 2,
                                    offsetX = -0.02, offsetY = 0,
                                    gap_factor = 0.28, radius_factor = 0.18, vertical_factor = 1) {
    gap <- gap_factor * bw * scale_cex
    r <- min(radius_factor * min(bw, bh) * scale_cex, (bh * 0.65))
    xL <- (xc + offsetX) - (bw / 2 + gap)
    xR <- (xc + offsetX) + (bw / 2 + gap)
    yB <- (yc + offsetY) - (bh / 2 + gap)
    yT <- (yc + offsetY) + (bh / 2 + gap)
    midY <- (yB + yT) / 2
    halfH <- (yT - yB) / 2 * vertical_factor
    y1 <- midY - halfH + r
    y2 <- midY + halfH - r
    segments(xL, y1, xL, y2, col = col, lwd = lwd, xpd = NA)
    segments(xR, y1, xR, y2, col = col, lwd = lwd, xpd = NA)
    draw_arc(cx = xL + r, cy = y2, r = r, from = pi, to = pi / 2, col = col, lwd = lwd)
    draw_arc(cx = xL + r, cy = y1, r = r, from = -pi / 2, to = -pi, col = col, lwd = lwd)
    draw_arc(cx = xR - r, cy = y2, r = r, from = 0, to = pi / 2, col = col, lwd = lwd)
    draw_arc(cx = xR - r, cy = y1, r = r, from = 0, to = -pi / 2, col = col, lwd = lwd)
  }

  # --------- Affichage du pedigree ---------
  output$plot <- renderPlot({
    req(pedigree$ped)
    align <- plotAlignment()
    annot <- plotAnnotation()
    sc <- plotScaling()
    drawPed(align, annotation = annot, scaling = sc)

    # Dessin des “crochets” pour TOUS les Adopted (styles$adopted)
    ids_to_bracket <- intersect(styles$adopted %||% character(0), labels(pedigree$ped))
    if (length(ids_to_bracket) > 0) {
      al <- plotAlignment()
      df <- positionDf()
      label_to_plotidx <- match(ids_to_bracket, labels(pedigree$ped))
      plot_row <- match(label_to_plotidx, al$plotord)
      for (k in which(!is.na(plot_row))) {
        row <- df[plot_row[k], ]
        draw_rounded_brackets(
          xc = row$xc, yc = row$yc,
          bw = row$boxw, bh = row$boxh,
          scale_cex = bracketParams$scale_cex,
          col = bracketParams$col,
          lwd = bracketParams$lwd,
          offsetX = bracketParams$offsetX,
          offsetY = bracketParams$offsetY,
          gap_factor = bracketParams$gap_factor,
          radius_factor = bracketParams$radius_factor,
          vertical_factor = bracketParams$vertical_factor
        )
      }
    }
  })

  # --------- Affichage individu sélectionné ---------
  output$selectedIndividual <- renderText({
    req(pedigree$ped)
    ids <- sel()
    if (length(ids) == 0) "Aucun individu sélectionné." else paste("Individu(s) sélectionné(s) :", paste(ids, collapse = ", "))
  })
  observeEvent(input$Starred, {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    before <- id %in% (styles$starred %||% character(0))
    styles$starred <- if (before) setdiff(styles$starred, id) else union(styles$starred %||% character(0), id)
    if (!before) {
      showNotification(paste("Étoile (starred) appliquée à", id), type = "message")
    } else {
      showNotification(paste("Étoile (starred) retirée de", id), type = "default")
    }
  })
  observeEvent(input$half, {
    req(pedigree$ped, selectedIndiv$row)
    id <- as.character(selectedIndiv$row$id)

    # On regarde quels parents sont connus pour restreindre les choix
    f_id <- pedtools::father(pedigree$ped, id, internal = FALSE)
    m_id <- pedtools::mother(pedigree$ped, id, internal = FALSE)

    choices_side <- c()
    if (!is.na(f_id) && nzchar(f_id)) choices_side <- c(choices_side, "Paternel (partage le père)" = "paternal")
    if (!is.na(m_id) && nzchar(m_id)) choices_side <- c(choices_side, "Maternel (partage la mère)" = "maternal")

    if (length(choices_side) == 0) {
      showNotification("Impossible d’ajouter un demi-sibling : aucun parent connu pour l’individu sélectionné.", type = "error")
      return(NULL)
    }

    showModal(modalDialog(
      title = sprintf("Ajouter un demi-frère / demi-sœur à %s", id),
      selectInput("half_side", "Côté du demi-sibling :", choices = choices_side),
      selectInput("half_sex", "Sexe :", choices = c("♂ Garçon" = 1, "♀ Fille" = 2, "Inconnu" = 0), selected = 0),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_add_half", "Ajouter", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  })
  observeEvent(input$confirm_add_half, {
    req(pedigree$ped, selectedIndiv$row, input$half_side, input$half_sex)
    removeModal()

    id <- as.character(selectedIndiv$row$id)
    side <- as.character(input$half_side)
    childSex <- as.integer(input$half_sex)

    # Récupère parents connus
    f_id <- pedtools::father(pedigree$ped, id, internal = FALSE)
    m_id <- pedtools::mother(pedigree$ped, id, internal = FALSE)

    # On ajoute un enfant à UN seul parent connu ; l’autre parent = NULL (sera créé automatiquement)
    # (comportement documenté de addChildren/addChild) :contentReference[oaicite:3]{index=3}
    newped <- tryCatch(
      {
        if (side == "paternal") {
          if (is.na(f_id) || !nzchar(f_id)) stop("Père inconnu : demi-paternel impossible.")
          pedtools::addChildren(pedigree$ped, father = f_id, mother = NULL, sex = childSex, verbose = FALSE)
        } else {
          if (is.na(m_id) || !nzchar(m_id)) stop("Mère inconnue : demi-maternel impossible.")
          pedtools::addChildren(pedigree$ped, father = NULL, mother = m_id, sex = childSex, verbose = FALSE)
        }
      },
      error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error")
        NULL
      }
    )

    if (is.null(newped)) {
      return(NULL)
    }

    updatePedigree(newped)
    msg <- if (side == "paternal") "Demi-frère/soeur paternel(le) ajouté(e)." else "Demi-frère/soeur maternel(le) ajouté(e)."
    showNotification(msg, type = "message")
  })

  # --------- Sélection à la souris ---------
  observeEvent(input$ped_click, {
    req(pedigree$ped)
    al <- plotAlignment()
    sc <- plotScaling()
    centers <- data.frame(
      x = al$xall + sc$boxw / 2,
      y = al$yall + sc$boxh / 2,
      id_plot = al$plotord
    )
    hit_id <- nearPoints(centers, input$ped_click, xvar = "x", yvar = "y", threshold = 20, maxpoints = 1)$id_plot
    req(length(hit_id) == 1)
    hit_label_index <- al$plotord[al$plotord == hit_id]
    that <- labels(pedigree$ped)[hit_label_index]
    curr <- sel()
    if (length(curr) == 1 && curr == that) {
      sel("")
      selectedIndiv$row <- NULL
      selectedIndiv$index <- NULL
    } else {
      sel(that)
      ind_row <- which(values$pedData$id == that)
      if (length(ind_row) > 0) {
        selectedIndiv$row <- values$pedData[ind_row, ]
        selectedIndiv$index <- ind_row
      }
    }
  })

  observe({
    req(values$pedData)
    # Mets à jour styles$deceased à chaque modification de la table
    ids_deceased <- as.character(values$pedData$id[which(values$pedData$deceased == TRUE | values$pedData$deceased == "✝️")])
    styles$deceased <- ids_deceased
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
        birth_estimate <- tryCatch(
          compute_relative_date(Sys.Date(), n_years, n_months, n_days, "backward"),
          error = function(e) NA
        )
        df$date_of_birth[i] <- format(birth_estimate, "%d-%m-%Y")
      }
      if (age_entered && deceased && !is.na(dob) && dob != "" && (is.na(dod) || dod == "" || dod == "NA")) {
        death_estimate <- tryCatch(
          compute_relative_date(as.Date(dob, "%d-%m-%Y"), n_years, n_months, n_days, "forward"),
          error = function(e) NA
        )
        df$date_of_death[i] <- format(death_estimate, "%d-%m-%Y")
      }
      if (age_entered && deceased && !is.na(dod) && dod != "" && (is.na(dob) || dob == "" || dob == "NA")) {
        birth_estimate <- tryCatch(
          compute_relative_date(as.Date(dod, "%d-%m-%Y"), n_years, n_months, n_days, "backward"),
          error = function(e) NA
        )
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
  output$pedTableDT <- DT::renderDT({
    req(values$pedData)
    df <- values$pedData
    # Filtre "branche = ancêtre commun avec la sélection"
    if (isTRUE(input$branch_filter_table) && length(sel()) == 1 && nzchar(sel()[1])) {
      ids_branch <- getCommonAncestorSet(pedigree$ped, sel()[1])
      df <- df[df$id %in% ids_branch, , drop = FALSE]
    }

    # Formatage des noms
    if ("last_name" %in% names(df)) df$last_name <- stringr::str_to_upper(df$last_name)
    if ("first_name" %in% names(df)) df$first_name <- stringr::str_to_title(df$first_name)

    # Sexe en clair
    df$Sex <- dplyr::recode(
      as.character(df$sex),
      "1" = "♂ Male",
      "2" = "♀ Female",
      "0" = "⚧ Unknown"
    )

    # Dates
    format_date <- function(x) {
      if (is.null(x) || is.na(x) || x == "" || x == "NA") {
        return("")
      }
      tryCatch(format(as.Date(x, "%d-%m-%Y"), "%d/%m/%Y"), error = function(e) "")
    }
    df$Birth <- vapply(df$date_of_birth, format_date, character(1))
    df$Death <- vapply(df$date_of_death, format_date, character(1))

    # Statut décès
    df$Deceased <- ifelse(
      !is.na(df$deceased) & df$deceased == TRUE,
      '<span class="material-symbols-outlined" style="color:#b51e1e;">frame_person_off</span>',
      '<span class="material-symbols-outlined" style="color:#1976d2;">person</span>'
    )

    # Bouton select (très important de mettre inputId unique sinon bug !)
    df$Select <- vapply(df$id, function(id) {
      as.character(
        actionButton(
          inputId = paste0("select_", id),
          label = "Select",
          icon = icon("mouse-pointer"),
          class = "btn-success btn-xs",
          onclick = sprintf('Shiny.setInputValue("select_indiv", "%s", {priority: "event"})', id)
        )
      )
    }, character(1))
    # ⭐️ Bouton Delete : même logique
    df$Delete <- vapply(df$id, function(id) {
      as.character(
        actionButton(
          inputId = paste0("delete_", id),
          label = "Delete",
          icon = icon("trash"),
          class = "btn-danger btn-xs",
          onclick = sprintf('Shiny.setInputValue("delete_indiv", "%s", {priority: "event"})', id)
        )
      )
    }, character(1))

    # PARENTS
    parent_fun <- function(id, df) {
      if (is.na(id) || id == "" || id == "NA") {
        return("")
      }
      p <- df[df$id == id, , drop = FALSE]
      if (nrow(p) == 0) {
        return(as.character(id))
      }
      paste(
        id,
        if ("last_name" %in% names(p)) stringr::str_to_upper(p$last_name) else "",
        if ("first_name" %in% names(p)) stringr::str_to_title(p$first_name) else ""
      )
    }
    if ("fid" %in% names(df)) df$Father <- mapply(parent_fun, df$fid, MoreArgs = list(df = df))
    if ("mid" %in% names(df)) df$Mother <- mapply(parent_fun, df$mid, MoreArgs = list(df = df))

    # 💡 *** AJOUTER "Select" DANS show_cols ! ***
    show_cols <- c(
      "id", "last_name", "first_name", "Sex", "Father", "Mother",
      "Birth", "Deceased", "Death", "age", "comments", "Select", "Delete"
    )
    show_cols <- intersect(show_cols, names(df))
    table_final <- df[, show_cols, drop = FALSE]

    DT::datatable(
      table_final,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      options = list(
        dom = "t",
        pageLength = 10,
        ordering = FALSE,
        autoWidth = FALSE
      ),
      class = "compact stripe hover"
    )
  })
  observeEvent(input$select_indiv, {
    req(values$pedData)
    id_selected <- input$select_indiv
    ped_data <- values$pedData

    # Recherche la bonne colonne id
    id_col <- NULL
    if ("id" %in% colnames(ped_data)) {
      id_col <- "id"
    } else if ("ID" %in% colnames(ped_data)) {
      id_col <- "ID"
    } else if ("label" %in% colnames(ped_data)) {
      id_col <- "label"
    }

    if (!is.null(id_col)) {
      idx <- which(as.character(ped_data[[id_col]]) == as.character(id_selected))
      if (length(idx) > 0) {
        sel(id_selected)
        selectedIndiv$row <- ped_data[idx[1], , drop = FALSE]
        selectedIndiv$index <- idx[1]
        showNotification(paste("Individual", id_selected, "selected."), type = "message")
      }
    }
  })
  # -- utilitaire local : formatage de date comme dans le grand tableau --
  .format_date <- function(x) {
    if (is.null(x) || is.na(x) || x == "" || x == "NA") {
      return("")
    }
    tryCatch(format(as.Date(x, "%d-%m-%Y"), "%d/%m/%Y"), error = function(e) "")
  }

  # -- utilitaire : restitution d'une ligne affichable à partir d'un id de pedigree --
  .build_row_from_id <- function(id_chr, pedData) {
    if (is.null(id_chr) || is.na(id_chr) || id_chr == "" || id_chr == "NA") {
      return(NULL)
    }

    # identifier la colonne id
    id_col <- if ("id" %in% names(pedData)) "id" else if ("ID" %in% names(pedData)) "ID" else if ("label" %in% names(pedData)) "label" else NULL
    if (is.null(id_col)) {
      return(NULL)
    }

    row <- pedData[which(as.character(pedData[[id_col]]) == as.character(id_chr)), , drop = FALSE]
    if (nrow(row) == 0) {
      # si l'id n'est pas dans pedData, on renvoie une ligne minimale avec l'id
      out <- data.frame(
        id = id_chr,
        last_name = "",
        first_name = "",
        Sex = "⚧ Unknown",
        Birth = "",
        Deceased = '<span class="material-symbols-outlined" style="color:#1976d2;">person</span>',
        Death = "",
        age = "",
        Select = as.character(
          actionButton(
            inputId = paste0("select_", id_chr), label = "Select", icon = icon("mouse-pointer"),
            class = "btn-success btn-xs",
            onclick = sprintf('Shiny.setInputValue("select_indiv", "%s", {priority: "event"})', id_chr)
          )
        ),
        Delete = as.character(
          actionButton(
            inputId = paste0("delete_", id_chr), label = "Delete", icon = icon("trash"),
            class = "btn-danger btn-xs",
            onclick = sprintf('Shiny.setInputValue("delete_indiv", "%s", {priority: "event"})', id_chr)
          )
        ),
        stringsAsFactors = FALSE
      )
      return(out)
    }

    # Colonnes sûres (tolérant aux absences)
    last_name <- if ("last_name" %in% names(row)) stringr::str_to_upper(row$last_name[1]) else ""
    first_name <- if ("first_name" %in% names(row)) stringr::str_to_title(row$first_name[1]) else ""

    sex_lbl <- dplyr::recode(
      as.character(if ("sex" %in% names(row)) row$sex[1] else NA),
      "1" = "♂ Male", "2" = "♀ Female", "0" = "⚧ Unknown", .default = "⚧ Unknown"
    )

    birth_fmt <- if ("date_of_birth" %in% names(row)) .format_date(row$date_of_birth[1]) else ""
    death_fmt <- if ("date_of_death" %in% names(row)) .format_date(row$date_of_death[1]) else ""

    deceased_icon <- if ("deceased" %in% names(row) && isTRUE(row$deceased[1])) {
      '<span class="material-symbols-outlined" style="color:#b51e1e;">frame_person_off</span>'
    } else {
      '<span class="material-symbols-outlined" style="color:#1976d2;">person</span>'
    }

    age_txt <- if ("age" %in% names(row) && !is.na(row$age[1])) as.character(row$age[1]) else ""

    # Boutons identiques à ceux du grand tableau
    select_btn <- as.character(
      actionButton(
        inputId = paste0("select_", id_chr),
        label = "Select",
        icon = icon("mouse-pointer"),
        class = "btn-success btn-xs",
        onclick = sprintf('Shiny.setInputValue("select_indiv", "%s", {priority: "event"})', id_chr)
      )
    )
    delete_btn <- as.character(
      actionButton(
        inputId = paste0("delete_", id_chr),
        label = "Delete",
        icon = icon("trash"),
        class = "btn-danger btn-xs",
        onclick = sprintf('Shiny.setInputValue("delete_indiv", "%s", {priority: "event"})', id_chr)
      )
    )

    data.frame(
      id = as.character(id_chr),
      last_name = last_name,
      first_name = first_name,
      Sex = sex_lbl,
      Birth = birth_fmt,
      Deceased = deceased_icon,
      Death = death_fmt,
      age = age_txt,
      Select = select_btn,
      Delete = delete_btn,
      stringsAsFactors = FALSE
    )
  }
  # -- utilitaire : appelle une fonction pedtools en capturant les erreurs, renvoie caractère() si rien
  # — util : appel pedtools sécurisé, renvoie character(0) si rien
  .safe_ped_call <- function(fun, ...) {
    out <- tryCatch(fun(...), error = function(e) NULL)
    if (is.null(out) || length(out) == 0) character(0) else as.character(out)
  }

  # — util : NA/"" -> NULL
  .nz <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NULL)
    }
    x <- as.character(x)
    x[is.na(x) | x == "" | x == "NA"] <- NA
    x <- x[!is.na(x)]
    if (length(x) == 0) NULL else x
  }

  # — util : teste l’égalité d’ID en tolérant types
  .id_eq <- function(a, b) {
    if (is.null(a) || is.null(b)) {
      return(FALSE)
    }
    as.character(a) == as.character(b)
  }

  # --------- TOGGLES Traits & Status ---------
  observeEvent(input$Proband, {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    before <- id %in% (styles$proband %||% character(0))
    styles$proband <- toggleId(styles$proband, id)
    if (!before) {
      showNotification(paste("Statut 'Proband' appliqué à", id), type = "message")
    } else {
      showNotification(paste("Statut 'Proband' retiré de", id), type = "default")
    }
  })

  observeEvent(input$Carrier, {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    before <- id %in% (styles$carrier %||% character(0))
    styles$carrier <- toggleId(styles$carrier, id)
    if (!before) {
      showNotification(paste("Phénotype 'Carrier' appliqué à", id), type = "message")
    } else {
      showNotification(paste("Phénotype 'Carrier' retiré de", id), type = "default")
    }
  })

  # ⚠️ Adopted = toggle + active/désactive les crochets
  observeEvent(input$Adopted, {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    before <- id %in% (styles$adopted %||% character(0))
    styles$adopted <- toggleId(styles$adopted, id)
    if (!before) {
      showNotification(paste("Statut 'Adopted' appliqué à", id, "(brackets)"), type = "message")
    } else {
      showNotification(paste("Statut 'Adopted' retiré de", id), type = "default")
    }
  })

  observeEvent(input$Miscarriage, {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    before <- id %in% (pedigree$miscarriage %||% character(0))
    pedigree$miscarriage <- toggleId(pedigree$miscarriage, id)
    if (!before) {
      showNotification(paste("Individu", id, "marqué 'Miscarriage'."), type = "message")
    } else {
      showNotification(paste("Statut 'Miscarriage' retiré de", id), type = "default")
    }
  })

  observeEvent(input$add_Miscarriage, {
    req(pedigree$ped, selectedIndiv$row)
    parent_id <- as.character(selectedIndiv$row$id)

    # Garde-fou : pedtools doit savoir si le parent est père(1) ou mère(2)
    psex <- pedtools::getSex(pedigree$ped, parent_id)
    if (is.na(psex) || psex == 0) {
      showNotification("Impossible d’ajouter : sexe du parent sélectionné inconnu.", type = "error")
      return(invisible(NULL))
    }

    ## 1) Enfants AVANT (même format que le plot)
    kids_before <- pedtools::children(pedigree$ped, parent_id, internal = FALSE)
    print(list(`print 1 - children(before)` = kids_before, parent = parent_id))
    cat("print 1 - children(before):", if (length(kids_before)) paste(kids_before, collapse = ", ") else "(none)", "\n")

    tryCatch(
      {
        ## 2) Ajout de l'enfant sex=0 (même logique que child_unknown)
        partner <- getPartner(pedigree$ped, parent_id) # unique partenaire sinon NULL
        newped <- addChildWithPartner(pedigree$ped, parent_id, partner = partner, childSex = 0)

        ## IMPORTANT : on met à jour d'abord (relabel 'generations') pour garder la cohérence d'affichage
        updatePedigree(newped)

        ## 3) Enfants APRÈS (sur l'objet mis à jour)
        kids_after <- pedtools::children(pedigree$ped, parent_id, internal = FALSE)
        print(list(`print 2 - children(after)` = kids_after, parent = parent_id))
        cat("print 2 - children(after):", if (length(kids_after)) paste(kids_after, collapse = ", ") else "(none)", "\n")

        ## 4) ID du NOUVEL enfant — DIFF ciblée uniquement sur les enfants du parent
        new_child <- setdiff(kids_after, kids_before)

        ## 5) Marquage automatique dans pedigree$miscarriage (si un seul ID)
        mark_msg <- "(non déterminé)"
        if (length(new_child) == 1) {
          pedigree$miscarriage <- union(pedigree$miscarriage %||% character(0), new_child)
          mark_msg <- sprintf("%s (marqué 'Miscarriage')", new_child)
          showNotification(sprintf("ID %s marqué 'Miscarriage'.", new_child), type = "message")
        } else if (length(new_child) > 1) {
          mark_msg <- paste0("Ambigu (", paste(new_child, collapse = ", "), ")")
        }

        ## 6) Info cohérente avec child_unknown
        msg <- if (is.null(partner)) {
          "Enfant (sexe inconnu) ajouté."
        } else {
          "Enfant (sexe inconnu) ajouté avec le partenaire existant."
        }
        showNotification(msg, type = "message")

        ## 7) Modale : prints + ID ajouté
        fmt <- function(v) if (length(v)) paste(v, collapse = ", ") else "(none)"
        showModal(modalDialog(
          title = "Enfants du parent (avant / après)",
          tags$h5("print 1 — before"),
          tags$pre(fmt(kids_before)),
          tags$h5("print 2 — after"),
          tags$pre(fmt(kids_after)),
          tags$h5("ID ajouté"),
          tags$pre(mark_msg),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      },
      error = function(e) {
        showModal(modalDialog(title = "Erreur", e$message, easyClose = TRUE))
      }
    )
  })


  # Deceased (toggle)
  observeEvent(input$Deceased, {
    req(selectedIndiv$row, pedigree$ped, values$pedData)
    id <- as.character(selectedIndiv$row$id)
    i <- which(values$pedData$id == id)
    if (length(i) == 1) {
      is_dead <- isTRUE(values$pedData$deceased[i])
      values$pedData$deceased[i] <- !is_dead
      if (!is_dead) {
        styles$deceased <- union(styles$deceased, id)
        showNotification(paste("Individu", id, "marqué comme décédé."), type = "message")
      } else {
        styles$deceased <- setdiff(styles$deceased, id)
        showNotification(paste("Individu", id, "désormais marqué comme vivant."), type = "message")
      }
    }
  })

  # --------- Changement de sexe ---------
  observeEvent(input$Male, {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    new_ped <- tryCatch(changeSex(pedigree$ped, id, sex = 1),
      error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error")
        NULL
      }
    )
    if (!is.null(new_ped)) {
      pedigree$ped <- new_ped
      values$pedData[values$pedData$id == id, "sex"] <- 1
      showNotification("Gender changed to 'Male'.", type = "message")
    }
  })

  observeEvent(input$Female, {
    req(selectedIndiv$row, pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    new_ped <- tryCatch(changeSex(pedigree$ped, id, sex = 2),
      error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error")
        NULL
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
    new_ped <- tryCatch(changeSex(pedigree$ped, id, sex = 0),
      error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error")
        NULL
      }
    )
    if (!is.null(new_ped)) {
      pedigree$ped <- new_ped
      values$pedData[values$pedData$id == id, "sex"] <- 0
      showNotification("Gender has been changed to 'Unknown'.", type = "message")
    }
  })

  ### PHENOTYPE: Modal création-------
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
  # --------- Ajouts parents / siblings / enfants ---------
  observeEvent(input$addparents, {
    req(pedigree$ped, selectedIndiv$row)
    child_id <- as.character(selectedIndiv$row$id)
    idx <- which(labels(pedigree$ped) == child_id)
    father_exists <- !is.na(pedigree$ped$FIDX[idx]) && pedigree$ped$FIDX[idx] != 0
    mother_exists <- !is.na(pedigree$ped$MIDX[idx]) && pedigree$ped$MIDX[idx] != 0
    if (father_exists && mother_exists) {
      showModal(modalDialog(title = "Ajout impossible", "Cet individu a déjà des parents.", easyClose = TRUE))
      return(NULL)
    }
    new_ped <- tryCatch(addPar(pedigree$ped, child_id),
      error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error")
        NULL
      }
    )
    if (!is.null(new_ped)) {
      updatePedigree(new_ped)
      showNotification("Parents ajoutés à l'individu sélectionné.", type = "message")
    }
  })

  observeEvent(input$brother, {
    req(pedigree$ped, selectedIndiv$row)
    ind <- as.character(selectedIndiv$row$id)
    tryCatch(
      {
        newped <- addSib(pedigree$ped, ind, sex = 1, side = "right")
        updatePedigree(newped)
        showNotification("Frère ajouté.", type = "message")
      },
      error = function(e) {
        showModal(modalDialog(title = "Erreur lors de l’ajout d’un frère", paste("Impossible d’ajouter le sibling :", e$message), easyClose = TRUE))
      }
    )
  })

  observeEvent(input$sister, {
    req(pedigree$ped, selectedIndiv$row)
    ind <- as.character(selectedIndiv$row$id)
    tryCatch(
      {
        newped <- addSib(pedigree$ped, ind, sex = 2, side = "right")
        updatePedigree(newped)
        showNotification("Sœur ajoutée.", type = "message")
      },
      error = function(e) {
        showModal(modalDialog(title = "Erreur lors de l’ajout d’une sœur", paste("Impossible d’ajouter le sibling :", e$message), easyClose = TRUE))
      }
    )
  })

  observeEvent(input$sib_unknown, {
    req(pedigree$ped, selectedIndiv$row)
    ind <- as.character(selectedIndiv$row$id)
    tryCatch(
      {
        newped <- addSib(pedigree$ped, ind, sex = 0, side = "right")
        updatePedigree(newped)
        showNotification("Sibling (sexe inconnu) ajouté.", type = "message")
      },
      error = function(e) {
        showModal(modalDialog(title = "Erreur lors de l’ajout d’un sibling (sexe inconnu)", paste("Impossible d’ajouter le sibling :", e$message), easyClose = TRUE))
      }
    )
  })

  # ------------------ Twins ------------------
  observeEvent(input$Twins, {
    req(pedigree$ped)
    infos <- selectedIndiv$row
    req(!is.null(infos))
    ind <- as.character(infos$id)
    sexe_ind <- infos$sex
    labelSexe <- switch(as.character(sexe_ind),
      "1" = "Male",
      "2" = "Female",
      "0" = "Unknown",
      "Unknown"
    )

    showModal(modalDialog(
      title = sprintf("Ajouter un jumeau à %s", ind),
      uiOutput("twinSexUI"),
      selectInput("twin_type", "Twin type :", choices = c("Monozygote (MZ)" = 1, "Dizygote (DZ)" = 2), selected = 2),
      footer = tagList(modalButton("Annuler"), actionButton("confirm_add_twin", "Add twin", class = "btn btn-primary")),
      easyClose = TRUE
    ))

    updateSelectInput(session, "twin_sex", selected = sexe_ind)
    output$twinSexUI <- renderUI({
      req(input$twin_type)
      if (as.numeric(input$twin_type) == 2) {
        selectInput("twin_sex", "Sexe du jumeau :", choices = c("Male" = 1, "Female" = 2, "Inconnu" = 0), selected = sexe_ind)
      } else {
        tags$p(HTML(sprintf("Sexe imposé : <strong>%s</strong> (monozygote)", labelSexe)), style = "margin-bottom: 0.5em;")
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

    new_ped <- tryCatch(addSib(pedigree$ped, id = ind, sex = sex_sibling, side = "right"),
      error = function(e) {
        showModal(modalDialog(title = "Erreur lors de l'ajout du sibling", e$message, easyClose = TRUE))
        NULL
      }
    )
    if (is.null(new_ped)) {
      return(NULL)
    }

    ids_apres <- labels(new_ped)
    id_sibling <- setdiff(ids_apres, ids_avant)
    if (length(id_sibling) != 1) {
      showModal(modalDialog(title = "Erreur", "Impossible de déterminer le nouvel ID du sibling.", easyClose = TRUE))
      return(NULL)
    }

    ids_jumeaux <- sort.default(c(ind, id_sibling))
    new_twins <- rbind(pedigree$twins, data.frame(id1 = ids_jumeaux[1], id2 = ids_jumeaux[2], code = twin_code))
    pedigree$ped <- new_ped
    pedigree$twins <- new_twins
    updatePedData()
    showNotification(sprintf("Jumeau ajouté à %s (%s)", ind, ifelse(twin_code == 1, "MZ", "DZ")), type = "message")
  })

  # ------------------ Triplets ------------------
  observeEvent(input$Triplets, {
    req(pedigree$ped)
    infos <- selectedIndiv$row
    req(!is.null(infos))
    selected_id <- as.character(infos$id)
    tryCatch(
      {
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
        showModal(modalDialog(title = "Erreur lors de l’ajout de triplés", paste("Impossible d’ajouter les triplets :", e$message), easyClose = TRUE))
      }
    )
  })

  # --------- Ajouts enfant (rapides) ---------
  observeEvent(input$child_son, {
    req(pedigree$ped, selectedIndiv$row)
    id <- as.character(selectedIndiv$row$id)
    tryCatch(
      {
        partner <- getPartner(pedigree$ped, id)
        newped <- addChildWithPartner(pedigree$ped, id, partner = partner, childSex = 1)
        updatePedigree(newped)
        msg <- if (is.null(partner)) "Fils ajouté (partenaire inconnu/créé)." else "Fils ajouté avec le partenaire existant."
        showNotification(msg, type = "message")
      },
      error = function(e) showModal(modalDialog(title = "Erreur", e$message))
    )
  })

  observeEvent(input$child_daughter, {
    req(pedigree$ped, selectedIndiv$row)
    id <- as.character(selectedIndiv$row$id)
    tryCatch(
      {
        partner <- getPartner(pedigree$ped, id)
        newped <- addChildWithPartner(pedigree$ped, id, partner = partner, childSex = 2)
        updatePedigree(newped)
        msg <- if (is.null(partner)) "Fille ajoutée (partenaire inconnu/créé)." else "Fille ajoutée avec le partenaire existant."
        showNotification(msg, type = "message")
      },
      error = function(e) showModal(modalDialog(title = "Erreur", e$message))
    )
  })

  observeEvent(input$child_unknown, {
    req(pedigree$ped, selectedIndiv$row)
    id <- as.character(selectedIndiv$row$id)
    tryCatch(
      {
        partner <- getPartner(pedigree$ped, id)
        newped <- addChildWithPartner(pedigree$ped, id, partner = partner, childSex = 0)
        updatePedigree(newped)
        msg <- if (is.null(partner)) "Enfant (sexe inconnu) ajouté." else "Enfant (sexe inconnu) ajouté avec le partenaire existant."
        showNotification(msg, type = "message")
      },
      error = function(e) showModal(modalDialog(title = "Erreur", e$message))
    )
  })

  # ---- Modale choisir partenaire + sexe de l'enfant ----
  observeEvent(input$choose_partner, {
    req(!is.null(selectedIndiv$row), pedigree$ped)
    id <- as.character(selectedIndiv$row$id)
    partners <- getPartners(pedigree$ped, id)
    partner_choices <- if (!is.null(partners) && length(partners) > 0) setNames(partners, partners) else c()
    partner_choices <- c(partner_choices, "Nouveau partenaire" = "new_partner")

    showModal(modalDialog(
      title = paste("Ajouter un enfant avec un partenaire pour", id),
      selectInput("partner_modal_choice", "Choisissez le partenaire :", choices = partner_choices),
      selectInput("child_sex_modal", "Sexe de l'enfant à ajouter :", choices = c("Fils" = 1, "Fille" = 2, "Sexe inconnu" = 0)),
      footer = tagList(modalButton("Annuler"), actionButton("validate_partner_modal", "Valider", class = "btn btn-success")),
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

  output$savePed <- downloadHandler(
    filename = "quickped.ped",
    content = function(con) {
      inclHead <- "head" %in% input$include
      inclFamid <- "famid" %in% input$include
      inclAff <- "aff" %in% input$include

      ped <- pedigree$ped
      df <- as.data.frame(ped)
      if (inclFamid) {
        df <- cbind(famid = 1, df)
      }
      if (inclAff) {
        aff <- union(styles$hatched, names(styles$fill))
        df <- cbind(df, aff = ifelse(labels(ped) %in% aff, 2, 1))
      }

      # Ajouter les colonnes supplémentaires depuis pedData, y compris "commentaire"
      additional_cols <- setdiff(names(values$pedData), names(df))
      df[additional_cols] <- values$pedData[additional_cols]

      write.table(df,
        file = con, col.names = inclHead, row.names = FALSE,
        quote = FALSE, sep = "\t", fileEncoding = "UTF-8"
      )
    }
  )
  # PNG export
  output$savePlotPng <- downloadHandler(
    filename = function() sprintf("quickped_%s.png", Sys.Date()),
    content = function(con) {
      # tailles sûres (px)
      w <- if (!is.null(input$export_w)) input$export_w else 1600
      h <- if (!is.null(input$export_h)) input$export_h else 1000

      align <- plotAlignment()
      scale <- plotScaling()
      annot <- plotAnnotation()

      png(con, width = w, height = h, res = 144) # res un peu plus élevée
      drawPed(align, annotation = annot, scaling = scale)
      dev.off()
    },
    contentType = "image/png"
  )

  # PDF export
  output$savePlotPdf <- downloadHandler(
    filename = function() sprintf("quickped_%s.pdf", Sys.Date()),
    content = function(file) {
      # tailles sûres (pouces)
      pw <- 11
      ph <- 8.5

      align <- plotAlignment()
      scale <- plotScaling()
      annot <- plotAnnotation()

      pdf(file, width = pw, height = ph, paper = "special", onefile = TRUE)
      drawPed(align, annotation = annot, scaling = scale)
      dev.off()
    },
    contentType = "application/pdf"
  )
  output$savePed <- downloadHandler(
    filename = function() sprintf("quickped_%s.ped", Sys.Date()),
    content = function(con) {
      inclHead <- FALSE
      inclFamid <- FALSE
      inclAff <- FALSE

      ped <- pedigree$ped
      df <- as.data.frame(ped)
      if (inclFamid) df <- cbind(famid = 1, df)
      if (inclAff) {
        aff <- union(styles$hatched, names(styles$fill))
        df <- cbind(df, aff = ifelse(labels(ped) %in% aff, 2, 1))
      }

      # colonnes additionnelles si dispo
      if (!is.null(values$pedData)) {
        add_cols <- setdiff(names(values$pedData), names(df))
        if (length(add_cols)) df[add_cols] <- values$pedData[add_cols]
      }

      write.table(df,
        file = con, col.names = inclHead, row.names = FALSE,
        quote = FALSE, sep = "\t", fileEncoding = "UTF-8"
      )
    }
  )
  # ---------- UI : réorganisation FRATRIE (sans suppression) ----------
  output$reorga_sib_ui <- renderUI({
    req(pedigree$ped)
    infos <- selectedIndiv$row
    if (is.null(infos)) {
      return(NULL)
    }

    sel_id <- as.character(infos$id)
    parents_sel <- pedtools::parents(pedigree$ped, sel_id) # (father, mother) ou NA
    if (any(is.na(parents_sel))) {
      return(div(
        class = "alert alert-warning",
        "L’individu sélectionné n’a pas de parents connus, donc pas de fratrie à réorganiser."
      ))
    }

    # Fratrie complète = tous les enfants du couple des parents
    sibs <- pedtools::children(pedigree$ped, parents_sel)
    ped_data <- values$pedData
    labels_vec <- vapply(sibs, .makeRichLabel, character(1), ped_data = ped_data)
    names(sibs) <- labels_vec
    moveCache$siblings <- sibs

    # Ordre courant (persistance par individu)
    current_order <- siblingsOrder[[sel_id]]
    if (is.null(current_order) || !setequal(current_order, names(sibs))) {
      current_order <- names(sibs)
      siblingsOrder[[sel_id]] <- current_order
    }

    div(
      h5(sprintf(
        "Fratrie de %s — glisser pour réordonner",
        .makeRichLabel(sel_id, ped_data)
      )),
      sortable::bucket_list(
        header = NULL,
        group_name = "fratrie",
        orientation = "horizontal",
        add_rank_list(
          text = NULL,
          labels = current_order, # libellés affichés (rich labels)
          input_id = "auto_order_sib_list" # input unique (plus de liste de suppression)
        )
      ),
      uiOutput("reorga_sib_message")
    )
  })

  # ---------- Observer : applique le nouvel ordre (sans suppression) ----------
  observeEvent(input$auto_order_sib_list, {
    infos <- selectedIndiv$row
    if (is.null(infos)) {
      return(NULL)
    }
    sel_id <- as.character(infos$id)

    siblings <- moveCache$siblings
    if (is.null(siblings)) {
      return(NULL)
    }

    main_labels <- input$auto_order_sib_list %||% character(0)
    # sécurité : vérifier qu'on a bien un réarrangement des items d'origine
    if (!setequal(main_labels, names(siblings)) || length(main_labels) == 0) {
      return(NULL)
    }

    # Persistance : sauvegarde de l'ordre choisi (labels riches)
    siblingsOrder[[sel_id]] <- main_labels

    # Convertit labels -> IDs
    all_ids <- labels(pedigree$ped)
    sib_ids <- vapply(names(siblings), .get_id_from_label, character(1))
    sib_idx <- which(all_ids %in% sib_ids)
    new_sib_order_ids <- vapply(main_labels, .get_id_from_label, character(1))

    # Remplacement du sous-ordre fratrie aux positions globales
    new_order <- all_ids
    new_order[sib_idx] <- new_sib_order_ids

    new_ped <- tryCatch(
      pedtools::reorderPed(pedigree$ped, new_order),
      error = function(e) {
        output$reorga_sib_message <- renderUI({
          div(
            class = "alert alert-danger",
            paste("Impossible de réorganiser la fratrie :", e$message)
          )
        })
        NULL
      }
    )

    if (!is.null(new_ped)) {
      pedigree$ped <- new_ped
      updatePedData()
      output$reorga_sib_message <- renderUI({
        div(class = "alert alert-success", "Nouvel ordre de la fratrie appliqué.")
      })
    }
  })
}

# ------------------ LANCEMENT APPLICATION ------------------
shinyApp(ui = ui, server = server)
