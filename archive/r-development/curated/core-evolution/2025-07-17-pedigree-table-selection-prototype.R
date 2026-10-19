# Archived R development file
# Original path: 🧩 Versions_Support/Base/PED_TABLE_SELEC.R
# Original created: 2025-07-17 17:19:03
# Original modified: 2025-08-07 00:54:57
# Archive rationale: Prototype for table-driven pedigree selection and editing.
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
library(DT)
library(dplyr)
library(stringr)
# ====================== FONCTIONS ======================
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

extract_units <- function(txt, unit) {
  pattern <- paste0("([0-9]+)\\s*", unit)
  match <- regmatches(txt, regexpr(pattern, txt, perl = TRUE))
  if (length(match) > 0 && nchar(match[1]) > 0) as.integer(gsub("\\D", "", match[1])) else 0
}
compute_relative_date <- function(reference, years, months, days, direction = "backward") {
  if (direction == "backward") {
    reference %m-% years(years) %m-% months(months) - days
  } else {
    reference %m+% years(years) %m+% months(months) + days
  }
}

extra_cols <- c("prénom", "nom", "date_of_birth", "deceased", "date_of_death", "age", "commentaire")

formatAnnot <- function(textAnnot, cex, font = 2, col = "blue") {
  if (is.null(textAnnot)) {
    return(NULL)
  }
  lapply(textAnnot, function(b) list(b, cex = cex, font = font, col = col))
}

# ------------------ PEDIGREE MODEL LIST -----------------
pedigree_list <- list(
  "Single Unknown" = function() singletons(id = 1, sex = 0),
  "Single Female" = function() singletons(id = 1, sex = 2),
  "Single Male" = function() singletons(id = 1, sex = 1),
  "Trio" = function() nuclearPed(1),
  "Full siblings" = function() nuclearPed(2, sex = c(1, 2)),
  "Grandparent" = function() ancestralPed(2),
  "Great-grandparent" = function() ancestralPed(3),
  "Maternal half siblings" = function() halfSibPed(1, 1, type = "maternal"),
  "Paternal half siblings" = function() halfSibPed(1, 1, type = "paternal"),
  "Avuncular" = function() avuncularPed()
)

# --------- MODAL UI COMPONENTS ---------
createPedigreeModal <- function(title_value = "", selected_model = "") {
  modalDialog(
    title = h4("Sélection d'un modèle de pedigree", style = "font-weight:600;margin-bottom:3px;"),
    div(
      style = "display:flex;flex-direction:column;gap:16px;",
      textInput(
        "modalPedTitle",
        "Titre du pedigree :",
        value = title_value,
        placeholder = "Saisir un titre pour le pedigree"
      ),
      selectInput(
        "modalPedChoice",
        label = "Choisir un modèle à afficher :",
        choices = c("--- Sélectionner ---" = "", names(pedigree_list)),
        selected = selected_model
      ),
      div(
        style = "background:rgba(255,255,255,0.46);border-radius:14px;box-shadow:0 2px 9px rgba(120,140,200,0.09);padding:15px;",
        h5("Aperçu du modèle sélectionné :"),
        plotOutput("modalPreviewPed", height = "180px")
      )
    ),
    footer = tagList(
      modalButton("Annuler"),
      actionButton("modalConfirmPed", "Valider", class = "btn btn-primary")
    ),
    easyClose = TRUE
  )
}

createRandomPedigreeModal <- function() {
  modalDialog(
    title = "Aperçu du pedigree aléatoire",
    textInput(
      inputId = "randomPedTitle",
      label = "Titre du pedigree :",
      value = "",
      width = "100%",
      placeholder = "Saisir un titre pour ce pedigree"
    ),
    plotOutput("previewPlot"),
    footer = tagList(
      modalButton("Annuler"),
      actionButton("confirmPed", "Valider", class = "btn btn-primary")
    ),
    easyClose = TRUE
  )
}
styles_css <- "
body {
  min-height: 100vh;
  background: linear-gradient(135deg, rgba(255,255,255,0.8) 0%, rgba(212,234,255,0.1) 100%);
   backdrop-filter: blur(6px);
  font-family: 'Helvetica Neue';
}
.btn_group_menu {
  display: flex;
  justify-content: center;
  align-items: center;
  backdrop-filter: blur(16px) saturate(180%);
  -webkit-backdrop-filter: blur(16px) saturate(180%);
  background-color: rgba(255, 255, 255, 0.75);
  border-radius: 12px;
  border: 1px solid rgba(209, 213, 219, 0.3);

  padding: 7px 0 3px 0;
  margin-top: 10px;
  width: 560px;
  gap: 20px;
  margin-left: auto;
  margin-right: auto;
}
.btn_group_menu .btn {
width : 60px;
 background: transparent ;
  display: flex;
  flex-direction: column;
  align-items: center;
  font-family: 'Helvetica Neue';
  font-weight:200;
border:0;
}
.icon{
color:#3E3E3D;
font-size:20px;
}
.tab-label{
color:#3E3E3D;
font-size:12px;
}
"

# ----------------- INTERFACE UTILISATEUR -----------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(styles_css)),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css")
  ),
  div(
    style = "
      font-family:'Helvetica Neue';
      font-size: 35px;
      font-weight:100;
      color: rgba(10,10,10, 0.7);
      text-align: center;
      padding-top:15px;
    ",
    "🧬 PEDIGREE CREATOR"
  ),
  hr(style = "border-top: 0.5px ridge rgba(133, 146, 158, 0.8);"),
  fluidRow(
    column(
      12,
      div(
        class = "btn_group_menu",
        actionButton("create", label = HTML('<span class="icon"><i class="bi bi-pencil-square tabicon"></i></span><span class="tab-label">Nouveau</span>')),
        actionButton("select_list", label = HTML('<span class="icon"><i class="bi bi-diagram-3-fill tabicon"></i></span><span class="tab-label">Modèle</span>')),
        actionButton("randomPed", label = HTML('<span class="icon"><i class="bi bi-dice-5 tabicon"></i></span><span class="tab-label">Aléatoire</span>')),
        actionButton("clear", label = HTML('<span class="icon"><i class="bi bi-trash tabicon"></i></span><span class="tab-label">Réinitialiser</span>')),
        actionButton("help", label = HTML('<span class="icon"><i class="bi bi-question-octagon tabicon"></i></span><span class="tab-label">Aide</span>')),
        actionButton("other_app", label = HTML('<span class="icon"><i class="bi bi-grid-3x3-gap-fill tabicon"></i></span><span class="tab-label">Autre</span>'))
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      12,
      textInput("plotTitle", "Titre du pedigree :", value = "", width = "100%"),
      textOutput("selectedIndividual"),
      plotOutput("plot", height = "500px", click = "ped_click", dblclick = "ped_dblclick"),
      p("Double-cliquez sur un individu pour ajouter du texte ou un commentaire", class = "text-muted"),
      hr(),
      h4("📋 Données du pedigree :"),
      rHandsontableOutput("pedTable"),
      DTOutput("pedDT")
    )
  )
)

# ---------------------- SERVEUR --------------------------
server <- function(input, output, session) {
  pedigree <- reactiveValues(
    ped = NULL,
    twins = NULL,
    miscarriage = NULL,
    title = NULL
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
  sel <- reactiveVal(character(0))
  values <- reactiveValues(
    previewPed = NULL,
    pedData = NULL
  )
  modalVars <- reactiveValues(previewPed = NULL, pedChoice = "", pedTitle = "")

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

  # --------- Modal logique -----------
  observeEvent(input$select_list, {
    modalVars$previewPed <- NULL
    modalVars$pedChoice <- ""
    modalVars$pedTitle <- ""
    showModal(createPedigreeModal())
  })

  observeEvent(input$modalPedChoice, {
    req(input$modalPedChoice != "")
    modalVars$pedChoice <- input$modalPedChoice
    pedigree_fun <- pedigree_list[[input$modalPedChoice]]
    if (!is.null(pedigree_fun) && is.function(pedigree_fun)) {
      preview <- tryCatch(pedigree_fun(), error = function(e) NULL)
      modalVars$previewPed <- preview
    } else {
      showNotification("Modèle inconnu.", type = "error")
      modalVars$previewPed <- NULL
    }
  })

  output$modalPreviewPed <- renderPlot({
    req(modalVars$previewPed)
    par(mar = c(1, 1, 2, 1))
    plot(modalVars$previewPed, cex = 1.2)
    safe_title <- input$modalPedTitle
    if (nzchar(safe_title)) title(main = safe_title, cex.main = 1.1, col.main = "#3c8dbc")
  })

  observeEvent(input$modalConfirmPed, {
    req(modalVars$previewPed)
    pedigree$ped <- modalVars$previewPed
    updateTextInput(session, "plotTitle", value = input$modalPedTitle)
    removeModal()
    updatePedData()
    sel(character(0))
    textAnnot(NULL)
  })

  observeEvent(input$create, {
    # Nouveau pedigree vide
    pedigree$ped <- singletons(id = 1, sex = 0)
    updateTextInput(session, "plotTitle", value = "Nouveau pedigree")
    updatePedData()
    sel(character(0))
    textAnnot(NULL)
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
    showModal(createRandomPedigreeModal())
  })

  output$previewPlot <- renderPlot({
    req(values$previewPed)
    par(mar = c(1, 1, 1, 1))
    plot(values$previewPed, cex = 1.2)
    safe_title <- input$randomPedTitle
    if (nzchar(safe_title)) title(main = safe_title, cex.main = 1.1, col.main = "#3c8dbc")
  })

  observeEvent(input$confirmPed, {
    req(values$previewPed)
    pedigree$ped <- values$previewPed
    updateTextInput(session, "plotTitle", value = input$randomPedTitle)
    removeModal()
    updatePedData()
    sel(character(0))
    textAnnot(NULL)
  })

  observeEvent(input$clear, {
    pedigree$ped <- NULL
    sel(character(0))
    textAnnot(NULL)
    values$pedData <- NULL
    updateTextInput(session, "plotTitle", value = "")
    showNotification("Réinitialisé.", type = "message")
  })

  observeEvent(input$help, {
    showModal(
      modalDialog(
        title = "Aide",
        "Cette application permet de créer, éditer et annoter des pedigrees.
        Utilisez le bandeau pour choisir ou générer un pedigree, sélectionnez un individu pour afficher ses données, double-cliquez pour annoter.",
        easyClose = TRUE,
        footer = modalButton("Fermer")
      )
    )
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
      values$pedData <- df
      # --- MISE À JOUR DYNAMIQUE DU STATUT DÉCÉDÉ ---
      if (!is.null(df$id)) {
        ped_ids <- as.character(labels(pedigree$ped))
        dec_ids <- as.character(df$id)[which(as.logical(df$deceased))]
        styles$deceased <- intersect(dec_ids, ped_ids)
      }
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
      hatched = styles$hatched, hatchDensity = 20,
      carrier = styles$carrier,
      deceased = styles$deceased,
      textAnnot = formatAnnot(textAnnot(), 1.2),
      col = list("#3c8dbc" = sel()),
      fill = styles$fill %||% NA,
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
        if (!is.na(id) && nzchar(id)) text_parts <- c(text_parts, as.character(id))
        full_name <- paste(na.omit(c(pedData$nom[i], pedData$prénom[i])), collapse = " ")
        if (nzchar(full_name)) text_parts <- c(text_parts, full_name)
        dob <- pedData$date_of_birth[i]
        dod <- pedData$date_of_death[i]
        age <- pedData$age[i]
        deceased <- isTRUE(pedData$deceased[i])
        date_info <- ""
        if (nzchar(dob)) date_info <- dob
        if (deceased && nzchar(dod)) date_info <- paste0(date_info, " †", dod)
        if (nzchar(age)) date_info <- paste0(date_info, " (", age, ")")
        if (nzchar(date_info)) text_parts <- c(text_parts, date_info)
        commentaire <- pedData$commentaire[i]
        if (nzchar(commentaire)) text_parts <- c(text_parts, commentaire)
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
      title(main = input$plotTitle, cex.main = 1.6, col.main = "#3c8dbc", line.main = 0.5)
    }
  })

  observeEvent(input$plotTitle, {
    pedigree$title <- input$plotTitle
  })

  observeEvent(input$ped_click, {
    req(pedigree$ped)
    hit <- nearPoints(positionDf(), input$ped_click, xvar = "x", yvar = "y", threshold = 20, maxpoints = 1)$idInt
    req(hit)
    that <- labels(pedigree$ped)[hit]
    curr <- sel()
    sel(if (that %in% curr) setdiff(curr, that) else c(curr, that))
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

  observeEvent(input$ped_dblclick, {
    # Ajout annotation par double-clic
    req(pedigree$ped)
    pos <- nearPoints(positionDf(), input$ped_dblclick, xvar = "x", yvar = "y", threshold = 20, maxpoints = 1)$idInt
    req(pos)
    id <- labels(pedigree$ped)[pos]
    showModal(modalDialog(
      title = sprintf("Ajouter une annotation pour l'individu %s", id),
      textAreaInput("newAnnot", "Texte de l'annotation :", value = "", width = "100%"),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirmAnnot", "Enregistrer", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
    observeEvent(input$confirmAnnot,
      {
        annots <- textAnnot()
        annots[[id]] <- input$newAnnot
        textAnnot(annots)
        removeModal()
      },
      ignoreInit = TRUE,
      once = TRUE
    )
  })
  output$pedDT <- renderDT({
    req(!is.null(values$pedData))
    df <- values$pedData

    df$nom <- toupper(df$nom)
    df$prénom <- stringr::str_to_title(df$prénom)
    df$Sexe <- dplyr::recode(as.character(df$sex), "1" = "♂️ Homme", "2" = "♀️ Femme", "0" = "⚧ Inconnu")

    parent_fun <- function(id, df) {
      if (is.na(id) || id == "" || id == "NA") {
        return("")
      }
      p <- df[df$id == id, , drop = FALSE]
      if (nrow(p) == 0) {
        return(as.character(id))
      }
      nom <- ifelse(!is.na(p$nom), toupper(p$nom), "")
      prenom <- ifelse(!is.na(p$prénom), stringr::str_to_title(p$prénom), "")
      paste(id, nom, prenom)
    }
    df$Père <- mapply(parent_fun, df$fid, MoreArgs = list(df = df))
    df$Mère <- mapply(parent_fun, df$mid, MoreArgs = list(df = df))

    # CORRECTION : Décédé vecteur logique, géré par ligne
    df$Décédé <- ifelse(
      !is.na(df$deceased) & df$deceased == TRUE,
      '<span class="material-symbols-outlined" style="color:#b51e1e;">frame_person_off</span>',
      '<span class="material-symbols-outlined" style="color:#1976d2;">clinical_notes</span>'
    )

    format_date <- function(x) {
      if (is.na(x) || x == "" || x == "NA") {
        return("")
      }
      tryCatch(format(as.Date(x, "%d-%m-%Y"), "%d/%m/%Y"), error = function(e) "")
    }
    df$Naissance <- vapply(df$date_of_birth, format_date, character(1))
    df$Décès <- vapply(df$date_of_death, format_date, character(1))
    table_finale <- df %>%
      dplyr::transmute(
        ID = id,
        Nom = nom,
        Prénom = prénom,
        Sexe,
        Père,
        Mère,
        Naissance,
        Décédé,
        Décès,
        Âge = age,
        Commentaire = commentaire
      )
    DT::datatable(
      table_finale,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", pageLength = 10, ordering = FALSE)
    )
  })
}

# ------------------ LANCEMENT APPLICATION ------------------
shinyApp(ui = ui, server = server)
