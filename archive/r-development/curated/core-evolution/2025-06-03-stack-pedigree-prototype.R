# Archived R development file
# Original path: version obsolette/stacktry/app.R
# Original created: 2025-06-03 20:10:23
# Original modified: 2025-08-29 09:01:21
# Archive rationale: Stack-based pedigree editing prototype with undo-like workflow ideas.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# LIBRARY ----------------------------------------------------

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

source("R/fonctions.R")


# --------------------------------- INTERFACE UTILISATEUR ---------------------------------
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
 .  /* Panel latéral général */
  .side-panel {
    background: #fff;
    border-left: 2px solid #e0e0e0;
    min-height: 650px;
    padding: 28px 18px 18px 18px;
    margin-bottom: 0 ;
    box-shadow: 4px 0 18px #e5e7eb;
    z-index: 10;
    border-radius: 0 18px 18px 0;
  }
  /* Grille centrale harmonisée */
  .genea-grid-container {
    display: grid;
    grid-template-columns: 100x 120px 120px;
    grid-template-rows: 60px 120px 80px;
    gap: 12px;
    justify-content: center;
    align-items: center;
    width: auto;
    margin: 0 auto;
    background: #fafbfc;
    border-radius: 5px;
    padding: 50px 0;
  }
  /* Case centrale */
  .genea-center-box {
    border: none;
    width: 120px;
    height: 120px;
    display: flex;
    align-items: center;
    justify-content: center;
    font-weight: 600;
    font-size: 20px;
    border-radius: 16px;
    background: #e9f6fb;
    box-shadow: 0 1px 6px #b3c9db;
    margin: 0 auto;
  }
  /* Titres et labels */
  .side-panel h3 {
    font-size: 1.3rem;
    font-weight: 700;
    color: #2376ab;
    margin-bottom: 24px;
  }
  .genea-grid-container div[style*='text-align:center;'] > div:first-child {
    font-size: 1.08em;
    font-weight: 600;
    color: #267cb3;
    margin-bottom: 6px;
  }
  .sib-labels {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    margin-top: 6px;
    font-size: 0.92em;
    color: #555;
    font-weight: 400;
  }
  /* Groupe de boutons SVG */
  .sib-btn-group {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 8px;
    margin-top: 0;
    margin-bottom: 0;
  }
  .sib-btn {
    background: #fff;
    border: 1.2px solid #b6bec6;
    border-radius: 8px;
    padding: 4px;
    margin: 0 2px;
    transition: box-shadow 0.18s, border 0.2s;
    box-shadow: 0 1px 4px #e3e8ed;
    min-width: 44px;
    min-height: 44px;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  .sib-btn svg { display:block; margin: 0 auto; width:34px; height:34px;}
  .sib-btn:hover, .sib-btn:focus {
    background: #e6f2fa;
    border: 1.6px solid #2b93d4;
    box-shadow: 0 3px 12px #b8def6;
  }
  /* Boutons standards grid */
  .genea-btn {
    width: 100%;
    min-height: 40px;
    font-size: 15px;
    background: #f7fafd;
    border: 1.2px solid #c5d5e2;
    border-radius: 6px;
    text-align: center;
    font-weight: 500;
    transition: background 0.18s, box-shadow 0.18s;
    box-shadow: 0 1px 3px #d0e4f1;
    margin-bottom: 0;
  }
  .genea-btn:hover {
    background: #e2f0fa;
    box-shadow: 0 3px 8px #b5d7f1;
  }
  /* Contrôles radio, switchs, labels au-dessus du grid */
  .side-panel .btn-group, .side-panel .form-group {
    margin-bottom: 18px;
  }
  .side-panel label {
    font-size: 1.02em;
    font-weight: 500;
    color: #1a3551;
    margin-bottom: 3px;
  }
  /* Pour le bouton de fermeture du panneau */
  #close_panel {
    float: right;
    margin-bottom: 10px;
    background: #f6f6f6;
    border: 1px solid #b6bec6;
    color: #2376ab;
    font-weight: 500;
    border-radius: 8px;
    padding: 5px 18px;
    transition: background 0.16s;
  }
  #close_panel:hover {
    background: #f3e3e2;
    color: #b04c4c;
    border: 1px solid #e8b2b1;
  }
"))
  ),
  # TITRE -----------------
  titlePanel(div(HTML("<h2><span style='font-family: Helvetica Neue;font-weight: 100; color:#4E4A4A'>🧬 PEDIGREE CREATOR</span></h2>"), align = "center")),
  fluidRow(
    column(
      6,
      selectInput("pedChoice", "Sélectionner un pedigree prédéfini :", choices = c(
        "", "Single Unknown", "Single Female", "Single Male", "Trio",
        "Full siblings", "Grandparent", "Great-grandparent",
        "Half siblings (mat)", "Half siblings (pat)", "Avuncular"
      ))
    ),
    column(
      6,
      br(),
      actionButton("randomPed", "🔀 Générer un pedigree aléatoire", class = "btn btn-info"),
      actionButton("reset", "🔁 Réinitialiser", class = "btn btn-danger")
    )
  ),
  wellPanel(
    # Section dépliable correcte avec `box()` et `collapsible = TRUE`
    bsCollapse(
      id = "collapseSection", open = NULL,
      bsCollapsePanel(
        "Cliquez pour ouvrir/fermer",
        h4("📋 Données du pedigree :"),
        rHandsontableOutput("pedTable")
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      8,
      textInput("plotTitle", "Titre du pedigree", placeholder = "Titre ici"),
      hr(),
      plotOutput("plot", click = "plot_click"),
      textOutput("selectedIndividual")
    ),
    column(
      4,
      wellPanel(uiOutput("sidePanelUI"))
    )
  )
)
# SERVER ----------------------------------------------------------------------------
server <- function(input, output, session) {
  # 1) Réactifs principaux
  pedigree <- reactiveValues(ped = NULL, twins = NULL)
  styles <- reactiveValues(
    hatched = NULL, carrier = NULL,
    deceased = NULL, dashed = NULL, miscarriage = NULL, fill = NULL
  )
  textAnnot <- reactiveVal(NULL)
  sel <- reactiveVal(character(0))
  values <- reactiveValues(
    pedData = NULL,
    pedNames = c("id", "fid", "mid", "sex", "prénom", "nom")
  )
  previousStack <- reactiveVal(list())
  pendingAction <- reactiveValues(type = NULL, id = NULL)


  # TABLEAU ------------------
  # 2) Dès qu’on met à jour pedigree$ped, on reconstruit le tableau
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

  # 3) Table éditable
  output$pedTable <- renderRHandsontable({
    req(values$pedData)
    df <- values$pedData
    readOnly <- intersect(c("id", "fid", "mid", "sex", "age"), colnames(df))
    ht <- if (nrow(df) > 6) 175 else NULL
    rh <- rhandsontable(df,
      useTypes = TRUE, manualColumnResize = TRUE,
      rowHeaders = NULL, height = ht,
      colHeaders = unique(c(values$pedNames, "commentaire")),
      overflow = "visible", selectCallback = TRUE
    )
    for (col in readOnly) rh <- rh %>% hot_col(col, readOnly = TRUE)
    if ("prénom" %in% colnames(df)) rh <- rh %>% hot_col("prénom", type = "text", colWidths = "110px")
    if ("nom" %in% colnames(df)) rh <- rh %>% hot_col("nom", type = "text", colWidths = "110px")
    if ("commentaire" %in% colnames(df)) rh <- rh %>% hot_col("commentaire", type = "text", colWidths = "150px")
    rh
  })
  observeEvent(input$pedTable$changes$changes, {
    req(input$pedTable)
    values$pedData <- data.table::data.table(hot_to_r(input$pedTable))
  })


  # BOUTON RESET -------------------
  # 4) Historique pour undo
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

  # 5) updatePed / resetPed
  updatePed <- function(..., addToStack = TRUE, clearSel = TRUE) {
    args <- list(...)

    if (addToStack) addCurrentToStack()

    if ("ped" %in% names(args)) pedigree$ped <- args$ped
    if ("twins" %in% names(args)) pedigree$twins <- args$twins
    if ("miscarriage" %in% names(args)) pedigree$miscarriage <- args$miscarriage # <--- AJOUT ESSENTIEL

    for (nm in intersect(names(styles), names(args))) {
      styles[[nm]] <- args[[nm]]
    }

    if ("textAnnot" %in% names(args)) textAnnot(args$textAnnot)
    if (clearSel) sel(character(0))
  }

  resetPed <- function(ped = NULL) {
    styles$hatched <- styles$carrier <- styles$deceased <- NULL
    styles$dashed <- styles$fill <- NULL
    pedigree$miscarriage <- NULL # <--- AJOUT
    textAnnot(NULL)
    updatePed(ped = ped, addToStack = FALSE, clearSel = TRUE)
  }

  # SELECTION PED ----------------
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
    showModal(modalDialog(
      title = "Aperçu", plotOutput("previewPlot", height = "300px"),
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
      ped <- tryCatch(randomPed(n = sample(5:10, 1), founders = sample(1:3, 1)) |> relabel(),
        error = function(e) NULL
      )
    }
    values$previewPed <- ped
    showModal(modalDialog(
      title = "Aperçu aléatoire", plotOutput("previewPlot", height = "300px"),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirmPed", "Valider", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
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

  # 10) Undo et reset global
  observeEvent(input$undo, {
    stack <- previousStack()
    if (length(stack) == 0) {
      return()
    }
    args <- c(stack[[length(stack)]], list(addToStack = FALSE))
    do.call(updatePed, args)
    previousStack(stack[-length(stack)])
  })
  observeEvent(input$reset, {
    resetPed(NULL) # remet ped à NULL → plus rien d’affiché
  })

  # SECTION --------------
  # clic pour sélectionner un individu ----------
  ## PLOT RENDERING (Pedigree plot output) -----------------------
  output$plot <- renderPlot({
    req(pedigree$ped)
    align <- withCallingHandlers(
      plotAlignment(),
      warning = function(w) if (startsWith(w$message, "Unexpected")) invokeRestart("muffleWarning")
    )
    annot <- plotAnnotation()
    sc <- plotScaling()
    drawPed(align, annotation = annot, scaling = sc)
    # Add plot title if provided
    if (!is.null(input$plotTitle) && nzchar(input$plotTitle)) {
      title(main = input$plotTitle, cex.main = 1.7, col.main = "#3c8dbc", line.main = 0.3, font.main = 1)
    }
  })

  ## SELECTED INDIVIDUAL DISPLAY (Text output for selection) -----
  output$selectedIndividual <- renderText({
    req(pedigree$ped)
    ids <- sel()
    if (length(ids) == 0 || ids == "") {
      "No individual selected."
    } else {
      paste("Selected individual(s):", paste(ids, collapse = ", "))
    }
  })

  # Panneau latéral contextuel avec Add Sib left & right (avec SVG et labels) ------------
  output$sidePanelUI <- renderUI({
    req(values$selectedIndForModal)
    selected <- values$selectedIndForModal
    ped <- values$pedObj
    req(!is.null(ped))
    idx <- which(labels(ped) == selected)
    sexe <- ped$SEX[idx]
    div(
      class = "side-panel",
      div(uiOutput("header_section")),
      div(
        class = "genea-grid-container",

        # Add Sib left
        div(
          style = "grid-area: 2 / 1 / 3 / 2; text-align:center;",
          div("Add Sib left", style = "font-weight:600;"),
          div(
            class = "sib-btn-group",
            actionButton("sib_left_male", label = HTML('<svg width="34" height="34"><rect x="5" y="5" width="24" height="24" fill="none" stroke="#222" stroke-width="2"/></svg>'), class = "sib-btn"),
            actionButton("sib_left_unknown", label = HTML('<svg width="34" height="34"><polygon points="17,4 30,17 17,30 4,17" fill="none" stroke="#222" stroke-width="2"/></svg>'), class = "sib-btn"),
            actionButton("sib_left_female", label = HTML('<svg width="34" height="34"><circle cx="17" cy="17" r="12" fill="none" stroke="#222" stroke-width="2"/></svg>'), class = "sib-btn")
          ),
          div(
            class = "sib-labels",
            div("Male"), div("Unknown"), div("Female")
          )
        ),

        # Centre : remplace generateIndividualSVG(sexe) par un SVG carré générique

        div(
          class = "genea-er-box",
          style = "grid-area: 2 / 2 / 3 / 3;", # important !
          generateIndividualSVG(sexe), # ton SVG de fond
        ),

        # Add Sib right
        div(
          style = "grid-area: 2 / 3 / 3 / 4; text-align:center;",
          div("Add Sib right", style = "font-weight:600;"),
          div(
            class = "sib-btn-group",
            actionButton("sib_right_male", label = HTML('<svg width="34" height="34"><rect x="5" y="5" width="24" height="24" fill="none" stroke="#222" stroke-width="2"/></svg>'), class = "sib-btn"),
            actionButton("sib_right_unknown", label = HTML('<svg width="34" height="34"><polygon points="17,4 30,17 17,30 4,17" fill="none" stroke="#222" stroke-width="2"/></svg>'), class = "sib-btn"),
            actionButton("sib_right_female", label = HTML('<svg width="34" height="34"><circle cx="17" cy="17" r="12" fill="none" stroke="#222" stroke-width="2"/></svg>'), class = "sib-btn")
          ),
          div(
            class = "sib-labels",
            div("Male"), div("Unknown"), div("Female")
          )
        ),

        # Top: Add parent
        div(
          actionButton("addparents", "Add parent", class = "genea-btn"),
          style = "grid-area: 1 / 2 / 2 / 3;"
        ),

        # Bottom: Add child
        div(
          style = "grid-area: 3 / 2 / 4 / 3; text-align:center;",
          div("Add child", style = "font-weight:600;"),
          div(
            class = "sib-btn-group",
            actionButton("child_son", label = HTML('<svg width="34" height="34"><rect x="5" y="5" width="24" height="24" fill="none" stroke="#222" stroke-width="2"/></svg>'), class = "sib-btn"),
            actionButton("child_unknown", label = HTML('<svg width="34" height="34"><polygon points="17,4 30,17 17,30 4,17" fill="none" stroke="#222" stroke-width="2"/></svg>'), class = "sib-btn"),
            actionButton("child_daughter", label = HTML('<svg width="34" height="34"><circle cx="17" cy="17" r="12" fill="none" stroke="#222" stroke-width="2"/></svg>'), class = "sib-btn"),
            actionButton("choose_partner", label = "👥 Partner", class = "sib-btn")
          ),
          div(
            class = "sib-labels",
            div("Son"), div("Unknown"), div("Daughter"), div("Partner")
          )
        )
      ),
      hr(),
      a(id = "toggleAdvanced", "Show/hide advanced info", href = "#"),
      shinyjs::hidden(
        div(
          id = "advanced",
          titlePanel("Formulaire avec DT et inputs dynamiques"),
          uiOutput("table_ui"),
          actionButton("add_row", "Ajouter un individu")
        )
      )
    )
  })

  row_count <- reactiveVal(1)

  observeEvent(input$add_row, {
    row_count(row_count() + 1)
  })
  output$header_section <- renderUI({
    selected <- values$selectedIndForModal
    req(selected)
    ped <- values$pedObj
    req(ped)
    idx <- which(labels(ped) == selected)
    current_sex <- ped$SEX[idx]

    # On vérifie si l'individu est marqué comme décédé
    is_deceased <- selected %in% values$deceased

    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        h3(
          style = "margin-bottom: 0; font-size: 1.3rem; font-weight: 700; color: #2376ab;",
          paste("Options pour l'individu :", selected)
        ),
        actionButton(
          "close_panel", "Fermer le panneau",
          style = "margin-bottom:0; margin-left: 16px; background: #f6f6f6; border: 1px solid #b6bec6; color: #2376ab; font-weight: 500; border-radius: 8px; padding: 5px 18px;"
        )
      ),
      hr(),
      fluidRow(
        column(
          6,
          actionButton("sex1", "Homme", class = "btn btn-outline-primary btn-sm w-100"),
          actionButton("sex2", "Femme", class = "btn btn-outline-primary btn-sm w-100"),
          actionButton("sex0", "Inconnu", class = "btn btn-outline-primary btn-sm w-100"),
          actionButton("sex3", "Miscarriage", class = "btn btn-outline-primary btn-sm w-100")
        ),
        column(
          6,
          prettySwitch(
            inputId = "deceased_switch",
            label = "Décédé",
            value = is_deceased,
            status = "danger"
          ),
          actionButton("clearsel", "Réinitialiser sélection", icon = icon("times-circle"), class = "btn btn-outline-secondary btn-sm w-100")
        )
      ),
      hr(),
      fluidRow(
        actionButton("twinstatus", "MZ / DZ", icon = icon("users"), class = "btn btn-outline-info btn-sm w-100"),
        actionButton("clean", "Nettoyer", icon = icon("eraser"), class = "btn btn-outline-secondary btn-sm"),
        actionButton("hatched", "Hachuré", icon = icon("fill-drip"), class = "btn btn-outline-secondary btn-sm"),
        actionButton("carrier", "Porteur", icon = icon("circle"), class = "btn btn-outline-secondary btn-sm"),
        actionButton("deceased", "Décédé", icon = icon("times"), class = "btn btn-outline-secondary btn-sm"),
        actionButton("dashed", "Pointillé", icon = icon("minus"), class = "btn btn-outline-secondary btn-sm"),
        colourInput("colorPicker", "", value = "#ffffff", showColour = "background"),
        actionButton("applyColor", "Appliquer couleur", icon = icon("paint-brush"), class = "btn btn-primary btn-sm w-100")
      ),
      hr()
    )
  })



  output$table_ui <- renderUI({
    rows <- lapply(1:row_count(), function(i) {
      fluidRow(
        column(4, textInput(paste0("prenom_", i), "Prénom")),
        column(4, airDatepickerInput(paste0("Date_", i),
          inputId = "Id114",
          label = "Language & format :",
          value = NULL,
          dateFormat = "dd/MM/yyyy",
          language = "fr",
          width = "100%"
        )),
        column(4, virtualSelectInput(paste0("Add_", i),
          inputId = "Id099",
          label = "Search :",
          choices = list(
            Parents = c("Mother", "Father"),
            Sibbling = c("Sister", "Brother"),
            Child = c("Dauther", "Son")
          ),
          multiple = FALSE,
          disableOptionGroupCheckbox = TRUE,
          search = TRUE,
          markSearchResults = TRUE,
          width = "100%",
          dropboxWrapper = "body"
        ))
      )
    })
    do.call(tagList, rows)
  })
  shinyjs::onclick(
    "toggleAdvanced",
    shinyjs::toggle(id = "advanced", anim = TRUE)
  )

  # Fermeture du panneau latéral
  observeEvent(input$close_panel, {
    values$selectedIndForModal <- NULL
  })


  # Labels to be displayed for each individual (supports multi-line)
  plotLabs <- reactive({
    req(pedigree$ped)
    breakLabs(pedigree$ped)
  })

  plotAlignment <- reactive({
    .pedAlignment(pedigree$ped,
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
      hatched = styles$hatched,
      hatchDensity = 20,
      carrier = styles$carrier,
      deceased = styles$deceased,
      miscarriage = styles$miscarriage,
      textAnnot = formatAnnot(textAnnot(), input$cex - 0.2),
      col = list("#C0392B88" = sel()), # <- c’est ici que passe la sélection !
      fill = styles$fill %||% NA,
      lty = list(dashed = styles$dashed),
      lwd = list(
        `3` = sel(),
        `1.5` = setdiff(styles$dashed, sel())
      )
    )
    # on ajoute prénom/nom/commentaire sous chaque id
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

  observeEvent(input$ped_click, {
    req(pedigree$ped)

    # Detect if a node (individual) was clicked using proximity threshold
    hit <- nearPoints(
      positionDf(), input$ped_click,
      xvar = "x", yvar = "y",
      threshold = 20, maxpoints = 1
    )$idInt
    req(hit)

    # Retrieve individual label by index
    selected_id <- labels(pedigree$ped)[hit]
    current_selection <- sel()

    if (length(current_selection) == 1 && current_selection == selected_id) {
      # If the clicked individual is already selected, deselect
      sel("")
      selectedIndiv$row <- NULL
      selectedIndiv$index <- NULL
    } else {
      # Else, select the clicked individual
      sel(selected_id)
      ind_row <- which(values$pedData$id == selected_id)
      if (length(ind_row) > 0) {
        selectedIndiv$row <- values$pedData[ind_row, ]
        selectedIndiv$index <- ind_row
      }
    }
  })

  plotScaling <- reactive({
    .pedScaling(
      req(plotAlignment()),
      plotAnnotation(),
      cex        = 1.4, # valeur par défaut
      symbolsize = 1, # valeur par défaut
      margins    = rep(3, 4) # valeur par défaut
    )
  })
  output$plot <- renderPlot({
    req(pedigree$ped)
    align <- withCallingHandlers(plotAlignment(),
      warning = function(w) if (startsWith(w$message, "Unexpected")) invokeRestart("muffleWarning")
    )
    drawPed(align,
      annotation = plotAnnotation(),
      scaling = plotScaling()
    )
  })


  # 13) Clics / double-clic pour sélection et annotation --------------------
  positionDf <- reactive({
    al <- plotAlignment()
    sc <- plotScaling()
    data.frame(x = al$xall, y = al$yall + sc$boxh / 2, idInt = al$plotord)
  })
  observeEvent(input$ped_click, {
    hit <- nearPoints(positionDf(),
      input$ped_click,
      xvar = "x", yvar = "y",
      threshold = 20, maxpoints = 1
    )$idInt
    req(hit)
    that <- labels(pedigree$ped)[hit]
    curr <- sel()
    sel(if (that %in% curr) setdiff(curr, that) else c(curr, that))
  })
  textAnnotTemp <- reactiveVal()
  observeEvent(input$ped_dblclick, {
    hit <- nearPoints(positionDf(),
      input$ped_dblclick,
      xvar = "x", yvar = "y",
      threshold = 20, maxpoints = 1
    )$idInt
    req(hit)
    id <- labels(pedigree$ped)[hit]
    textAnnotTemp(textAnnot())
    showAnnotationModal(input, output, session, id, textAnnotTemp)
  })
  observeEvent(textAnnotTemp(), {
    updatePed(textAnnot = textAnnotTemp(), clearSel = FALSE)
  })
  # ==================== OBSERVE EVENTS - MODIFICATIONS DU PEDIGREE ====================

  # Fonction utilitaire pour afficher les erreurs dans une boîte modale
  showError <- function(e) {
    showModal(modalDialog(
      title = "❌ Erreur",
      paste("Message :", conditionMessage(e)),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  }

  # MODIFICATION IND ------------
  observeEvent(input$deceased_switch, {
    req(values$selectedIndForModal)
    ind <- values$selectedIndForModal
    if (isTRUE(input$deceased_switch)) {
      values$deceased <- unique(c(values$deceased, ind))
    } else {
      values$deceased <- setdiff(values$deceased, ind)
    }
  })

  # AJOUT APPARENTES --------
  # Fonction pour ajouter des parents (inchangée)
  addPar <- function(ped, ids) {
    n <- length(ids)
    if (n == 1) {
      pedtools::addParents(ped, ids, verbose = FALSE)
    } else if (n == 2 || n == 3) {
      child <- ids[1]
      pars <- ids[-1]
      parsex <- pedtools::getSex(ped, pars)
      fa <- mo <- NULL

      if (length(pars) == 2) {
        if (parsex[1] == 1 && parsex[2] == 2) {
          fa <- pars[1]
          mo <- pars[2]
        } else if (parsex[1] == 2 && parsex[2] == 1) {
          fa <- pars[2]
          mo <- pars[1]
        } else {
          stop("Sexes incompatibles des parents sélectionnés.")
        }
      } else if (length(pars) == 1) {
        if (parsex[1] == 1) {
          fa <- pars[1]
        } else if (parsex[1] == 2) {
          mo <- pars[1]
        } else {
          stop("Impossible d'utiliser un individu de sexe inconnu comme parent.")
        }
      }
      pedtools::addParents(ped, child, father = fa, mother = mo, verbose = FALSE)
    } else {
      stop("Sélection invalide : il faut sélectionner 1 à 3 individus maximum.")
    }
  }
  # Ajouter un fils
  observeEvent(input$child_son, {
    id <- req(values$selectedIndForModal)
    tryCatch(
      {
        if (any(id %in% pedigree$miscarriage)) {
          stop("Impossible d’ajouter un enfant à une fausse couche.")
        }
        partners <- getPartners(values$pedObj, id)
        if (is.null(partners)) {
          # Aucun partenaire existant
          values$pedObj <- addChildWithPartner(values$pedObj, id, partner = NULL, childSex = 1)
        } else if (length(partners) == 1) {
          values$pedObj <- addChildWithPartner(values$pedObj, id, partner = partners[1], childSex = 1)
        } else {
          pendingAction$type <- "addson"
          pendingAction$id <- id
          showPartnerModal(id, partners)
        }
      },
      error = function(e) showModal(modalDialog(title = "Erreur", e$message))
    )
  })

  observeEvent(input$child_daughter, {
    id <- req(values$selectedIndForModal)
    tryCatch(
      {
        if (any(id %in% pedigree$miscarriage)) {
          stop("Impossible d’ajouter un enfant à une fausse couche.")
        }
        partners <- getPartners(values$pedObj, id)
        if (is.null(partners)) {
          values$pedObj <- addChildWithPartner(values$pedObj, id, partner = NULL, childSex = 2)
        } else if (length(partners) == 1) {
          values$pedObj <- addChildWithPartner(values$pedObj, id, partner = partners[1], childSex = 2)
        } else {
          pendingAction$type <- "adddaughter"
          pendingAction$id <- id
          showPartnerModal(id, partners)
        }
      },
      error = function(e) showModal(modalDialog(title = "Erreur", e$message))
    )
  })

  observeEvent(input$child_unknown, {
    id <- req(values$selectedIndForModal)
    tryCatch(
      {
        partners <- getPartners(values$pedObj, id)
        if (is.null(partners)) {
          values$pedObj <- addChildWithPartner(values$pedObj, id, partner = NULL, childSex = 0)
        } else if (length(partners) == 1) {
          values$pedObj <- addChildWithPartner(values$pedObj, id, partner = partners[1], childSex = 0)
        } else {
          pendingAction$type <- "addunknown"
          pendingAction$id <- id
          showPartnerModal(id, partners)
        }
      },
      error = function(e) showModal(modalDialog(title = "Erreur", e$message))
    )
  })
  observeEvent(input$choose_partner, {
    id <- values$selectedIndForModal
    req(!is.null(id), values$pedObj)
    partners <- getPartners(values$pedObj, id)
    partner_choices <- c()
    if (!is.null(partners) && length(partners) > 0) {
      # Affichage: prénom+nom si présents, sinon juste l’ID
      partner_choices <- setNames(partners, partners)
    }
    partner_choices <- c(partner_choices, "Nouveau partenaire" = "new_partner")

    showModal(modalDialog(
      title = paste("Ajouter un enfant avec un partenaire pour", id),
      selectInput("partner_modal_choice", "Choisissez le partenaire :", choices = partner_choices),
      selectInput("child_sex_modal", "Sexe de l'enfant à ajouter :",
        choices = c("Fils" = 1, "Fille" = 2, "Sexe inconnu" = 0)
      ),
      # Si tu veux demander le sexe du nouveau partenaire, c’est purement “cosmétique”
      # pedtools sait déjà que s’il crée un nouveau conjoint, il sera de sexe opposé à celui sélectionné
      footer = tagList(
        modalButton("Annuler"),
        actionButton("validate_partner_modal", "Valider", class = "btn btn-success")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$validate_partner_modal, {
    id <- values$selectedIndForModal
    ped <- values$pedObj
    req(id, ped)
    partner_choice <- input$partner_modal_choice
    child_sex <- as.integer(input$child_sex_modal)

    if (partner_choice == "new_partner") {
      # Cas du nouveau partenaire : ne RIEN spécifier, pedtools fait tout
      ped <- addChildWithPartner(ped, id, partner = NULL, childSex = child_sex)
      values$pedObj <- ped
      removeModal()
      showNotification("Enfant et nouveau partenaire ajoutés.", type = "message")
    } else {
      # Partenaire déjà existant
      ped <- addChildWithPartner(ped, id, partner = partner_choice, childSex = child_sex)
      values$pedObj <- ped
      removeModal()
      showNotification("Enfant ajouté avec le partenaire sélectionné.", type = "message")
    }
  })


  # Handler pour la validation du partenaire (modale)
  observeEvent(input$validate_partner, {
    id <- pendingAction$id
    partner <- input$partner_choice
    if (pendingAction$type == "addson") {
      values$pedObj <- addChildWithPartner(values$pedObj, id, partner = partner, childSex = 1)
    } else if (pendingAction$type == "adddaughter") {
      values$pedObj <- addChildWithPartner(values$pedObj, id, partner = partner, childSex = 2)
    } else if (pendingAction$type == "addunknown") {
      values$pedObj <- addChildWithPartner(values$pedObj, id, partner = partner, childSex = 0)
    }
    removeModal()
    pendingAction$type <- NULL
    pendingAction$id <- NULL
  })
  # Ajouter un frère (male) à droite
  observeEvent(input$sib_right_male, {
    req(values$pedObj, values$selectedIndForModal)
    ind <- values$selectedIndForModal
    tryCatch(
      {
        newped <- addSib(values$pedObj, ind, sex = 1, side = "right")
        values$pedObj <- newped
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

  # Ajouter une sœur (female) à droite
  observeEvent(input$sib_right_female, {
    req(values$pedObj, values$selectedIndForModal)
    ind <- values$selectedIndForModal
    tryCatch(
      {
        newped <- addSib(values$pedObj, ind, sex = 2, side = "right")
        values$pedObj <- newped
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

  # Ajouter un sibling au sexe inconnu à droite
  observeEvent(input$sib_right_unknown, {
    req(values$pedObj, values$selectedIndForModal)
    ind <- values$selectedIndForModal
    tryCatch(
      {
        newped <- addSib(values$pedObj, ind, sex = 0, side = "right")
        values$pedObj <- newped
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

  # Ajouter un frère (male) à gauche
  observeEvent(input$sib_left_male, {
    req(values$pedObj, values$selectedIndForModal)
    ind <- values$selectedIndForModal
    tryCatch(
      {
        newped <- addSib(values$pedObj, ind, sex = 1, side = "left")
        values$pedObj <- newped
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Erreur lors de l’ajout d’un frère à gauche",
          paste("Impossible d’ajouter le sibling :", e$message),
          easyClose = TRUE
        ))
      }
    )
  })

  # Ajouter une sœur (female) à gauche
  observeEvent(input$sib_left_female, {
    req(values$pedObj, values$selectedIndForModal)
    ind <- values$selectedIndForModal
    tryCatch(
      {
        newped <- addSib(values$pedObj, ind, sex = 2, side = "left")
        values$pedObj <- newped
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Erreur lors de l’ajout d’une sœur à gauche",
          paste("Impossible d’ajouter le sibling :", e$message),
          easyClose = TRUE
        ))
      }
    )
  })
  # Ajouter un sibling au sexe inconnu à gauche
  observeEvent(input$sib_left_unknown, {
    req(values$pedObj, values$selectedIndForModal)
    ind <- values$selectedIndForModal
    tryCatch(
      {
        newped <- addSib(values$pedObj, ind, sex = 0, side = "left")
        values$pedObj <- newped
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Erreur lors de l’ajout d’un sibling inconnu à gauche",
          paste("Impossible d’ajouter le sibling :", e$message),
          easyClose = TRUE
        ))
      }
    )
  })
  # Modifier le sexe : Homme
  observeEvent(input$sex1, {
    id <- req(sel())
    tryCatch(
      {
        ped <- changeSex(pedigree$ped, id, sex = 1, twins = pedigree$twins)
        updatePed(ped = ped)
      },
      error = showError
    )
  })

  # Modifier le sexe : Femme
  observeEvent(input$sex2, {
    id <- req(sel())
    tryCatch(
      {
        ped <- changeSex(pedigree$ped, id, sex = 2, twins = pedigree$twins)
        updatePed(ped = ped)
      },
      error = showError
    )
  })

  # Modifier le sexe : Inconnu
  observeEvent(input$sex0, {
    id <- req(sel())
    tryCatch(
      {
        ped <- changeSex(pedigree$ped, id, sex = 0, twins = pedigree$twins)
        updatePed(ped = ped)
      },
      error = showError
    )
  })

  # Marquer comme fausse couche
  observeEvent(input$sex3, {
    id <- req(sel())
    tryCatch(
      {
        if (!all(id %in% leaves(pedigree$ped))) {
          stop2("Un parent ne peut pas être marqué comme une fausse couche.")
        }
        updatePed(miscarriage = union(pedigree$miscarriage, id), clearSel = TRUE)
      },
      error = showError
    )
  })

  # Nettoyer tous les styles
  observeEvent(input$clean, {
    id <- req(sel())
    addCurrentToStack()
    styles$hatched <- setdiff(styles$hatched, id)
    styles$carrier <- setdiff(styles$carrier, id)
    styles$deceased <- setdiff(styles$deceased, id)
    styles$dashed <- setdiff(styles$dashed, id)
  })

  # Appliquer les styles
  observeEvent(input$hatched, {
    updatePed(hatched = union(styles$hatched, req(sel())))
  })

  observeEvent(input$carrier, {
    updatePed(carrier = union(styles$carrier, req(sel())))
  })

  observeEvent(input$deceased, {
    updatePed(deceased = union(styles$deceased, req(sel())))
  })

  observeEvent(input$dashed, {
    updatePed(dashed = union(styles$dashed, req(sel())))
  })
  # Définir ou modifier le lien de jumeaux
  observeEvent(input$twinstatus, {
    ids <- req(sel())
    tryCatch(
      {
        updatePed(twins = updateTwins(pedigree$ped, pedigree$twins, ids))
      },
      error = showError
    )
  })

  # Réinitialiser la sélection
  observeEvent(input$clearsel, {
    sel(character(0))
  })

  # Annuler la dernière action
  observeEvent(input$undo, {
    stack <- previousStack()
    len <- length(stack)
    if (len == 0) {
      return()
    }
    args <- c(stack[[len]], list(addToStack = FALSE))
    do.call(updatePed, args)
    previousStack(stack[-len])
  })
  observeEvent(input$ped_click, {
    req(pedigree$ped)

    # Detect if a node (individual) was clicked using proximity threshold
    hit <- nearPoints(
      positionDf(), input$ped_click,
      xvar = "x", yvar = "y",
      threshold = 20, maxpoints = 1
    )$idInt
    req(hit)

    # Retrieve individual label by index
    selected_id <- labels(pedigree$ped)[hit]
    current_selection <- sel()

    if (length(current_selection) == 1 && current_selection == selected_id) {
      # If the clicked individual is already selected, deselect
      sel("")
      selectedIndiv$row <- NULL
      selectedIndiv$index <- NULL
    } else {
      # Else, select the clicked individual
      sel(selected_id)
      ind_row <- which(values$pedData$id == selected_id)
      if (length(ind_row) > 0) {
        selectedIndiv$row <- values$pedData[ind_row, ]
        selectedIndiv$index <- ind_row
      }
    }
  })
}
# Lancer l'application Shiny-----------
shinyApp(ui = ui, server = server)
