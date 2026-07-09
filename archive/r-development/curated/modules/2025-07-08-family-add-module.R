# Archived R development file
# Original path: 🧩 Versions_Support/Module/ajout_visule_family/module_add_family.R
# Original created: 2025-07-08 16:00:19
# Original modified: 2025-07-08 17:47:17
# Archive rationale: Visual family-add module prototype.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# -------------------- LIBRAIRIES -----------------------
library(shiny)
library(shinyBS)
library(shinyjs)
library(pedtools)
library(ribd)
library(verbalisr)
library(ggplot2)
library(ggrepel)
library(glue)
library(lubridate)
library(rhandsontable)
library(data.table)
library(jsonlite)
library(bslib)
library(shinyWidgets)
library(colourpicker)
library(officer)
library(rvg)
library(markdown)

# ----------------- FONCTIONS UTILES --------------------

# Alignement des textes selon la position
txtAlign <- c(
  top         = "top",
  topright    = "topright",
  right       = "right",
  bottomright = "bottomright",
  bottom      = "bottom",
  bottomleft  = "bottomleft",
  left        = "left",
  topleft     = "topleft",
  inside      = "center"
)

# Namespace pour annotation de texte
ns <- NS("textAnnot")

# Fonction d'entrée de texte pour annotation
txtInp <- function(pos, id, ann, width = "100px") {
  currVec <- ann[[pos]]
  val <- if (id %in% names(currVec)) currVec[id] else ""
  tags$input(
    id = ns(pos),
    type = "text",
    class = "shiny-input-text form-control",
    style = sprintf(
      "text-align: %s; font-weight: bold; width: %s; margin: auto; padding: 5px",
      txtAlign[[pos]], width
    ),
    value = val,
    placeholder = pos
  )
}

# Modal pour annotation textuelle d'un individu
showAnnotationModal <- function(input, output, session, id, annot) {
  currAnn <- annot()
  showModal(
    modalDialog(
      title = paste("Text annotation for individual", id),
      tags$style(
        type = "text/css",
        "
        #grid-container {
          display: grid;
          gap: 3px;
          justify-content: center;
          align-items: center;
          margin-top: 20px;
        }
        #symbol {
          grid-column: 2;
          grid-row: 2;
          border: none;
          background: lightgray;
          width: 200px;
          height: 200px;
          display: flex;
        }
        "
      ),
      div(
        id = "grid-container",
        div(txtInp("topleft", id, currAnn), style = "grid-area: 1 / 1 / 2 / 2;"),
        div(txtInp("top", id, currAnn), style = "grid-area: 1 / 2 / 2 / 3;"),
        div(txtInp("topright", id, currAnn), style = "grid-area: 1 / 3 / 2 / 4;"),
        div(txtInp("left", id, currAnn), style = "grid-area: 2 / 1 / 3 / 2;"),
        div(id = "symbol", txtInp("inside", id, currAnn)),
        div(txtInp("right", id, currAnn), style = "grid-area: 2 / 3 / 3 / 4;"),
        div(txtInp("bottomleft", id, currAnn), style = "grid-area: 3 / 1 / 4 / 2;"),
        div(txtInp("bottom", id, currAnn), style = "grid-area: 3 / 2 / 4 / 3;"),
        div(txtInp("bottomright", id, currAnn), style = "grid-area: 3 / 3 / 4 / 4;")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("save"), "Save", class = "btn btn-primary")
      )
    )
  )

  observeEvent(input[[ns("save")]],
    {
      ann <- annot()
      for (p in names(txtAlign)) {
        oldvec <- ann[[p]] %||% character(0)
        oldvec[id] <- input[[ns(p)]]
        ann[[p]] <- oldvec[nzchar(oldvec)]
      }
      ann <- ann[lengths(ann) > 0]
      annot(ann)
      removeModal()
    },
    ignoreInit = TRUE,
    once = TRUE
  )
}

# Opérateur %||% (valeur par défaut si NULL)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Fonction pour couper les labels aux espaces doubles
breakLabs <- function(x, breakAt = "  ") {
  labs <- labels(x)
  names(labs) <- gsub(breakAt, "\n", labs)
  labs
}

# Formater les annotations textuelles
formatAnnot <- function(textAnnot, cex, font = 2, col = "blue") {
  if (is.null(textAnnot)) {
    return(NULL)
  }
  lapply(textAnnot, function(b) list(b, cex = cex, font = font, col = col))
}

# ----------------- INTERFACE UTILISATEUR -----------------

ui <- fluidPage(
  titlePanel(
    div(HTML("<h2><span style='color:#3c8dbc'>🧬 PEDIGREE CREATOR</span></h2>"), align = "center")
  ),
  br(),
  fluidRow(
    column(
      4,
      offset = 4,
      wellPanel(
        selectInput(
          "pedChoice",
          "Sélectionner un pedigree prédéfini :",
          choices = c(
            "", "Single Unknown", "Single Female", "Single Male", "Trio",
            "Full siblings", "Grandparent", "Great-grandparent",
            "Half siblings (mat)", "Half siblings (pat)", "Avuncular"
          )
        ),
        actionButton("randomPed", "🔀 Générer un pedigree aléatoire", class = "btn btn-info"),
        actionButton("reset", "🔁 Réinitialiser", class = "btn btn-danger")
      )
    )
  ),
  fluidRow(
    column(
      12,
      textInput("title", "Title", value = "Your title here", width = "100%"),
      textOutput("selectedIndividual"),
      hr(),
      plotOutput("plot", click = "ped_click", dblclick = "ped_dblclick"),
      p("Double-click on an individual to add text", class = "text-muted"),
      hr(),
      uiOutput("floating_panel"),
      h4("📋 Données du pedigree :"),
      rHandsontableOutput("pedTable")
    )
  )
)

# ---------------------- SERVEUR --------------------------

server <- function(input, output, session) {
  # --- Variables réactives principales ---
  pedigree <- reactiveValues(ped = NULL)
  styles <- reactiveValues(hatched = NULL, carrier = NULL, deceased = NULL, dashed = NULL, miscarriage = NULL, fill = NULL)
  textAnnot <- reactiveVal(NULL)
  previousStack <- reactiveVal(list())
  sel <- reactiveVal(character(0))
  values <- reactiveValues(
    pedData = NULL,
    pedNames = c("id", "fid", "mid", "sex", "prénom", "nom")
  )

  # Mise à jour du tableau pedigree à chaque changement
  observeEvent(pedigree$ped, {
    df <- if (is.null(pedigree$ped)) NULL else as.data.frame(pedigree$ped)
    if (is.null(df) || nrow(df) == 0) {
      values$pedData <- NULL
    } else {
      df$prénom <- NA_character_
      df$nom <- NA_character_
      df$commentaire <- NA_character_
      values$pedData <- data.table::data.table(df)
    }
    sel(character(0))
  })

  # Table éditable rhandsontable
  output$pedTable <- renderRHandsontable({
    req(values$pedData)
    df <- values$pedData
    readOnly <- intersect(c("id", "fid", "mid", "sex", "age"), colnames(df))
    ht <- if (nrow(df) > 6) 175 else NULL
    rh <- rhandsontable(
      df,
      useTypes = TRUE,
      manualColumnResize = TRUE,
      rowHeaders = NULL,
      height = ht,
      colHeaders = unique(c(values$pedNames, "commentaire")),
      overflow = "visible",
      selectCallback = TRUE
    )
    for (col in readOnly) rh <- rh %>% hot_col(col, readOnly = TRUE)
    if ("prénom" %in% colnames(df)) rh <- rh %>% hot_col("prénom", type = "text", colWidths = "110px")
    if ("nom" %in% colnames(df)) rh <- rh %>% hot_col("nom", type = "text", colWidths = "110px")
    if ("commentaire" %in% colnames(df)) rh <- rh %>% hot_col("commentaire", type = "text", colWidths = "150px")
    rh
  })

  # Synchronisation des modifications de la table
  observeEvent(input$pedTable$changes$changes, {
    req(input$pedTable)
    values$pedData <- data.table::data.table(hot_to_r(input$pedTable))
  })

  # --- Gestion annotation texte individuelle ---
  textAnnotTemp <- reactiveVal()
  observeEvent(input$ped_dblclick, {
    hit <- nearPoints(positionDf(), input$ped_dblclick, xvar = "x", yvar = "y", threshold = 20, maxpoints = 1)$idInt
    req(hit)
    id <- labels(pedigree$ped)[hit]
    textAnnotTemp(textAnnot())
    showAnnotationModal(input, output, session, id, textAnnotTemp)
  })

  observeEvent(textAnnotTemp(), {
    updatePed(textAnnot = textAnnotTemp(), clearSel = FALSE)
  })

  # --- Gestion de l'historique pour undo ---
  observe({
    if (length(previousStack()) > 0) enable("undo") else disable("undo")
  })

  addCurrentToStack <- function() {
    stack <- previousStack()
    curr <- c(
      reactiveValuesToList(pedigree),
      reactiveValuesToList(styles),
      list(textAnnot = textAnnot())
    )
    if (length(stack) == 0 || !identical(curr, stack[[length(stack)]])) {
      previousStack(append(stack, list(curr)))
    }
  }

  # --- Fonction de mise à jour du pedigree / reset ---
  updatePed <- function(..., addToStack = TRUE, clearSel = TRUE) {
    args <- list(...)
    if (addToStack) addCurrentToStack()
    if ("ped" %in% names(args)) pedigree$ped <- args$ped
    if ("twins" %in% names(args)) pedigree$twins <- args$twins
    if ("miscarriage" %in% names(args)) pedigree$miscarriage <- args$miscarriage # <-- Ajout
    for (nm in intersect(names(styles), names(args))) {
      styles[[nm]] <- args[[nm]]
    }
    if ("textAnnot" %in% names(args)) textAnnot(args$textAnnot)
    if (clearSel) sel(character(0))
  }

  # --- Gestion des choix de pedigree prédéfinis ou aléatoires ---
  observeEvent(input$pedChoice, {
    req(input$pedChoice != "")
    ped <- switch(input$pedChoice,
      "Single Unknown"      = singleton(id = "1", sex = 0),
      "Single Female"       = singleton(id = "1", sex = 2),
      "Single Male"         = singleton(id = "1", sex = 1),
      "Trio"                = nuclearPed(1),
      "Full siblings"       = nuclearPed(2, sex = 1:2),
      "Grandparent"         = ancestralPed(2),
      "Great-grandparent"   = ancestralPed(3),
      "Half siblings (mat)" = halfSibPed(1, 1, type = "maternal"),
      "Half siblings (pat)" = halfSibPed(1, 1, type = "paternal"),
      "Avuncular"           = avuncularPed()
    )
    values$previewPed <- ped
    showModal(
      modalDialog(
        title = "Aperçu",
        plotOutput("previewPlot", height = "300px"),
        footer = tagList(
          modalButton("Annuler"),
          actionButton("confirmPed", "Valider", class = "btn btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  observeEvent(input$randomPed, {
    ped <- NULL
    while (is.null(ped)) {
      ped <- tryCatch(
        randomPed(n = sample(5:10, 1), founders = sample(1:3, 1)) |> relabel(),
        error = function(e) NULL
      )
    }
    values$previewPed <- ped
    showModal(
      modalDialog(
        title = "Aperçu aléatoire",
        plotOutput("previewPlot", height = "300px"),
        footer = tagList(
          modalButton("Annuler"),
          actionButton("confirmPed", "Valider", class = "btn btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })

  output$previewPlot <- renderPlot({
    req(values$previewPed)
    al <- .pedAlignment(values$previewPed, twins = NULL)
    an <- .pedAnnotation(values$previewPed)
    sc <- .pedScaling(al, an)
    drawPed(al, annotation = an, scaling = sc)
  })

  observeEvent(input$confirmPed, {
    req(values$previewPed)
    removeModal()
    resetPed(values$previewPed)
  })

  # --- Réinitialisation globale du pedigree ---
  resetPed <- function(ped = NULL) {
    styles$hatched <- styles$carrier <- styles$deceased <- NULL
    pedigree$miscarriage <- NULL
    pedigree$ped <- ped
  }

  observeEvent(input$reset, {
    resetPed(NULL)
  })

  # --- Affichage du graphique du pedigree ---
  plotLabs <- reactive({
    breakLabs(pedigree$ped)
  })

  plotAlignment <- reactive({
    .pedAlignment(
      pedigree$ped,
      twins = pedigree$twins,
      miscarriage = pedigree$miscarriage,
      arrows = FALSE,
      align = c(1.5, 2)
    )
  })

  plotAnnotation <- reactive({
    ann <- .pedAnnotation(
      req(pedigree$ped),
      labs = plotLabs(),
      textAnnot = formatAnnot(textAnnot(), input$cex - 0.2),
      col = list("#C0392B88" = sel())
    )
    # Ajout prénom/nom/commentaire sous chaque id depuis le tableau
    if (!is.null(values$pedData)) {
      df <- values$pedData
      ids <- labels(pedigree$ped)
      for (i in seq_along(ids)) {
        id <- ids[i]
        parts <- list(as.character(id))
        nom <- df$nom[i]
        pre <- df$prénom[i]
        full <- paste(na.omit(c(nom, pre)), collapse = " ")
        if (nzchar(full)) parts <- c(parts, full)
        cm <- df$commentaire[i]
        if (!is.na(cm) && nzchar(cm)) parts <- c(parts, cm)
        ann$textUnder[[as.character(id)]] <- paste(parts, collapse = "\n")
      }
    }
    ann
  })

  plotScaling <- reactive({
    .pedScaling(
      req(plotAlignment()),
      plotAnnotation(),
      cex = 1.4,
      symbolsize = 1,
      margins = rep(3, 4)
    )
  })

  output$plot <- renderPlot({
    req(pedigree$ped)
    align <- withCallingHandlers(
      plotAlignment(),
      warning = function(w) if (startsWith(w$message, "Unexpected")) invokeRestart("muffleWarning")
    )
    drawPed(align, annotation = plotAnnotation(), scaling = plotScaling())
    if (!is.null(input$plotTitle) && nzchar(input$plotTitle)) {
      title(main = input$plotTitle, cex.main = 1.6, col.main = "#3c8dbc", line.main = 0.5)
    }
  })

  # --- Sélection d'un individu par clic ---
  positionDf <- reactive({
    al <- plotAlignment()
    sc <- plotScaling()
    data.frame(x = al$xall, y = al$yall + sc$boxh / 2, idInt = al$plotord)
  })
  observeEvent(input$ped_click, {
    hit <- nearPoints(positionDf(), input$ped_click, xvar = "x", yvar = "y", threshold = 20, maxpoints = 1)$idInt
    req(hit)
    that <- labels(pedigree$ped)[hit]
    floatingPanel$show <- TRUE
    floatingPanel$id <- that
    # Sélection reste gérée comme avant si besoin
    curr <- sel()
    sel(if (that %in% curr) setdiff(curr, that) else c(curr, that))
  })



  floatingPanel <- reactiveValues(show = FALSE, id = NULL)
  # Bouton pour fermer le panneau
  observeEvent(input$close_panel, {
    floatingPanel$show <- FALSE
    floatingPanel$id <- NULL
  })

  output$floating_panel <- renderUI({
    req(floatingPanel$show, floatingPanel$id)
    infos <- NULL
    if (!is.null(values$pedData)) {
      df <- values$pedData
      idx <- which(as.character(df$id) == as.character(floatingPanel$id))
      if (length(idx) == 1) {
        infos <- list(
          prénom      = df$prénom[idx],
          nom         = df$nom[idx],
          commentaire = df$commentaire[idx]
        )
      }
    }
    # --- Récupération parents
    pere_id <- NA
    mere_id <- NA
    pere_infos <- NULL
    mere_infos <- NULL
    if (!is.null(pedigree$ped) && !is.null(floatingPanel$id)) {
      pere_id <- father(pedigree$ped, floatingPanel$id)
      mere_id <- mother(pedigree$ped, floatingPanel$id)
    }
    if (!is.null(values$pedData)) {
      df <- values$pedData
      if (!is.na(pere_id)) {
        idx_p <- which(as.character(df$id) == as.character(pere_id))
        if (length(idx_p) == 1) {
          pere_infos <- list(
            id = df$id[idx_p],
            prénom = df$prénom[idx_p],
            nom = df$nom[idx_p]
          )
        }
      }
      if (!is.na(mere_id)) {
        idx_m <- which(as.character(df$id) == as.character(mere_id))
        if (length(idx_m) == 1) {
          mere_infos <- list(
            id = df$id[idx_m],
            prénom = df$prénom[idx_m],
            nom = df$nom[idx_m]
          )
        }
      }
    }
    # Pour chaque apparenté, observer le clic et afficher la fiche correspondante
    observe({
      # Parents
      if (!is.null(pere_infos)) {
        observeEvent(input[[paste0("show_indiv_", pere_infos$id)]],
          {
            floatingPanel$id <- pere_infos$id
          },
          ignoreInit = TRUE
        )
      }
      if (!is.null(mere_infos)) {
        observeEvent(input[[paste0("show_indiv_", mere_infos$id)]],
          {
            floatingPanel$id <- mere_infos$id
          },
          ignoreInit = TRUE
        )
      }
      # Siblings
      if (!is.null(pedigree$ped)) {
        all_sibs <- unique(c(
          siblings(pedigree$ped, floatingPanel$id, half = NA),
          children(pedigree$ped, floatingPanel$id)
        ))
        for (sib_id in all_sibs) {
          observeEvent(input[[paste0("show_indiv_", sib_id)]],
            {
              floatingPanel$id <- sib_id
            },
            ignoreInit = TRUE
          )
        }
      }
      # Enfants
      if (!is.null(pedigree$ped)) {
        enfants_ids <- children(pedigree$ped, floatingPanel$id)
        for (enf_id in enfants_ids) {
          observeEvent(input[[paste0("show_indiv_", enf_id)]],
            {
              floatingPanel$id <- enf_id
            },
            ignoreInit = TRUE
          )
        }
      }
    })

    # --- Affichage panneau
    absolutePanel(
      bottom = 20, right = 20, width = 320,
      draggable = TRUE,
      wellPanel(
        style = "background: #F7FAFF; margin-bottom: 0; border-radius: 10px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;",
          strong(paste("Individu :", floatingPanel$id)),
          actionButton("close_panel", label = NULL, icon = icon("times"), style = "color: #C0392B; background: none; border: none; font-size: 20px;")
        ),
        if (!is.null(infos)) {
          tagList(
            p(strong("Prénom : "), ifelse(is.na(infos$prénom), "-", infos$prénom)),
            p(strong("Nom : "), ifelse(is.na(infos$nom), "-", infos$nom)),
            p(strong("Commentaire : "), ifelse(is.na(infos$commentaire), "-", infos$commentaire))
          )
        } else {
          p("Aucune information disponible pour cet individu.")
        },
        hr(),
        div(
          h4("Parents"),
          if (!is.null(pere_infos)) {
            p(
              strong("Père : "),
              paste0(
                "ID: ", pere_infos$id,
                ifelse(!is.na(pere_infos$prénom) && nzchar(pere_infos$prénom), paste0(" - ", pere_infos$prénom), ""),
                ifelse(!is.na(pere_infos$nom) && nzchar(pere_infos$nom), paste0(" ", pere_infos$nom), "")
              )
            )
          } else {
            p(strong("Père : "), "-")
          },
          if (!is.null(mere_infos)) {
            p(
              strong("Mère : "),
              paste0(
                "ID: ", mere_infos$id,
                ifelse(!is.na(mere_infos$prénom) && nzchar(mere_infos$prénom), paste0(" - ", mere_infos$prénom), ""),
                ifelse(!is.na(mere_infos$nom) && nzchar(mere_infos$nom), paste0(" ", mere_infos$nom), "")
              )
            )
          } else {
            p(strong("Mère : "), "-")
          }
        ),
        hr(),
        div(
          h4("Siblings"),
          radioButtons(
            inputId = "sibling_filter",
            label = NULL,
            choices = c("Tous" = "all", "Plein (full)" = "full", "Demi (half)" = "half"),
            selected = "all",
            inline = TRUE
          ),
          uiOutput("sibling_list")
        ),
        hr(),
        div(
          h4("Enfants"),
          uiOutput("children_list")
        )
      )
    )
  })
  output$children_list <- renderUI({
    req(floatingPanel$show, floatingPanel$id)
    enfants_ids <- NULL
    if (!is.null(pedigree$ped)) {
      enfants_ids <- children(pedigree$ped, floatingPanel$id)
    }
    if (!is.null(enfants_ids) && length(enfants_ids) > 0) {
      df <- values$pedData
      tagList(
        lapply(enfants_ids, function(enf_id) {
          info <- NULL
          if (!is.null(df)) {
            idx <- which(as.character(df$id) == as.character(enf_id))
            if (length(idx) == 1) {
              info <- paste0(
                "ID: ", df$id[idx],
                ifelse(!is.na(df$prénom[idx]) && nzchar(df$prénom[idx]), paste0(" - ", df$prénom[idx]), ""),
                ifelse(!is.na(df$nom[idx]) && nzchar(df$nom[idx]), paste0(" ", df$nom[idx]), "")
              )
            } else {
              info <- paste("ID:", enf_id)
            }
          } else {
            info <- paste("ID:", enf_id)
          }
          p(info)
        })
      )
    } else {
      p(em("Aucun enfant trouvé pour cet individu."))
    }
  })

  # Valeur réactive pour mémoriser le filtre sélectionné
  siblingFilter <- reactiveVal("all")

  observeEvent(input$sibling_filter, {
    siblingFilter(input$sibling_filter)
  })

  output$sibling_list <- renderUI({
    req(floatingPanel$show, floatingPanel$id)
    sib_ids <- NULL
    # Selon le filtre, on utilise la bonne fonction pedtools
    if (!is.null(pedigree$ped)) {
      if (siblingFilter() == "all") {
        sib_ids <- siblings(pedigree$ped, floatingPanel$id, half = NA)
      } else if (siblingFilter() == "full") {
        sib_ids <- siblings(pedigree$ped, floatingPanel$id, half = FALSE)
      } else if (siblingFilter() == "half") {
        sib_ids <- siblings(pedigree$ped, floatingPanel$id, half = TRUE)
      }
    }
    # Affichage de la liste
    if (!is.null(sib_ids) && length(sib_ids) > 0) {
      df <- values$pedData
      tagList(
        lapply(sib_ids, function(sib_id) {
          info <- NULL
          if (!is.null(df)) {
            idx <- which(as.character(df$id) == as.character(sib_id))
            if (length(idx) == 1) {
              info <- paste0(
                "ID: ", df$id[idx],
                ifelse(!is.na(df$prénom[idx]) && nzchar(df$prénom[idx]), paste0(" - ", df$prénom[idx]), ""),
                ifelse(!is.na(df$nom[idx]) && nzchar(df$nom[idx]), paste0(" ", df$nom[idx]), "")
              )
            } else {
              info <- paste("ID:", sib_id)
            }
          } else {
            info <- paste("ID:", sib_id)
          }
          p(info)
        })
      )
    } else {
      p(em("Aucun sibling trouvé pour ce filtre."))
    }
  })
}


# ------------------ LANCEMENT APPLICATION ------------------
shinyApp(ui = ui, server = server)
