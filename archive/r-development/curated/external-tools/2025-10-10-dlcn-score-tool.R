# Archived R development file
# Original path: module autre/scoreDLCN.R
# Original created: 2025-10-10 18:16:10
# Original modified: 2025-10-10 18:16:11
# Archive rationale: Standalone DLCN score calculator kept as non-integrated genetics support tool.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# app.R — Calculateur du score DLCN (FH)
# Auteur : Assistant
# Description : Application Shiny pour calculer le score Dutch Lipid Clinic Network (DLCN)
# Note : Outil d'aide à la décision réservé aux professionnel·le·s de santé. Ne remplace pas le jugement clinique.

# --- Dépendances ---
# Installez-les au besoin :
# install.packages(c("shiny", "bslib"))

library(shiny)
library(bslib)

# --- Fonctions utilitaires ---
convert_to_mgdl <- function(value, unit) {
  if (is.na(value) || value == "") {
    return(NA_real_)
  }
  value <- as.numeric(value)
  if (is.na(value)) {
    return(NA_real_)
  }
  if (unit == "mmol/L") {
    return(value * 38.67) # Conversion LDL-C mmol/L -> mg/dL
  } else {
    return(value)
  }
}

ldl_points <- function(ldl_mgdl) {
  # Retourne les points DLCN pour l'intervalle LDL-C
  if (is.na(ldl_mgdl)) {
    return(0)
  }
  if (ldl_mgdl >= 325) {
    return(8)
  }
  if (ldl_mgdl >= 251 && ldl_mgdl <= 325) {
    return(5)
  }
  if (ldl_mgdl >= 191 && ldl_mgdl <= 250) {
    return(3)
  }
  if (ldl_mgdl >= 155 && ldl_mgdl <= 190) {
    return(1)
  }
  return(0)
}

classify_dlc <- function(total) {
  if (is.na(total)) {
    return(list(label = "—", class = "secondary"))
  }
  if (total <= 2) {
    return(list(label = "FH improbable (0–2)", class = "success"))
  }
  if (total <= 5) {
    return(list(label = "FH possible (3–5)", class = "warning"))
  }
  if (total <= 8) {
    return(list(label = "FH probable (6–8)", class = "danger"))
  }
  return(list(label = "FH certaine (>8)", class = "danger"))
}

badge <- function(text, class = "primary") {
  tags$span(class = paste0("badge text-bg-", class), text)
}

# --- UI ---
ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Calculateur DLCN (FH)",
  layout_columns(
    col_widths = c(6, 6),

    # Colonne 1 — Données patient et antécédents
    card(
      card_header("Identité du patient (optionnel)"),
      textInput("nom", "Nom", placeholder = "Dupont"),
      textInput("prenom", "Prénom", placeholder = "Marie"),
      dateInput("naissance", "Date de naissance", value = NA)
    ),
    card(
      card_header("Paramètres et actions"),
      radioButtons("unite", "Unité LDL-C", choices = c("mg/dL", "mmol/L"), inline = TRUE),
      actionButton("btn_reset", "Réinitialiser", icon = icon("arrow-rotate-left"), class = "btn-outline-secondary"),
      actionButton("btn_demo", "Charger un exemple", icon = icon("wand-magic-sparkles"), class = "btn-outline-primary"),
      downloadButton("dl_rapport", "Télécharger le rapport (HTML)")
    ),
    card(
      card_header("1) Antécédents familiaux"),
      helpText("Cochez toutes les affirmations vraies ; seul le score le plus élevé de cette catégorie sera retenu."),
      checkboxInput("fam1a", "Apparenté au 1er degré avec coronaropathie prématurée (H < 55 ans, F < 60 ans)", value = FALSE),
      checkboxInput("fam1b", "Apparenté au 1er degré avec LDL-C > 95e percentile pour l'âge/sex", value = FALSE),
      checkboxInput("fam1c", "Apparenté au 1er degré avec xanthome tendineux et/ou arc cornéen", value = FALSE),
      checkboxInput("fam1d", "Enfant(s) < 18 ans avec LDL-C > 95e percentile", value = FALSE)
    ),
    card(
      card_header("2) Anamnèse clinique personnelle"),
      checkboxInput("cli2a", "Coronaropathie prématurée (H < 55 ans, F < 60 ans)", value = FALSE),
      checkboxInput("cli2b", "Maladie vasculaire cérébrale ou périphérique prématurée (H < 55 ans, F < 60 ans)", value = FALSE)
    ),
    card(
      card_header("3) Examen physique"),
      checkboxInput("phy3a", "Présence de xanthome tendineux", value = FALSE),
      checkboxInput("phy3b", "Arc cornéen avant 45 ans", value = FALSE)
    ),
    card(
      card_header("4) Biologie — LDL-C"),
      textInput("ldl", "LDL-C", placeholder = "p.ex. 4.2 si mmol/L ou 190 si mg/dL"),
      helpText("Si possible, utiliser une valeur pré-traitement. La conversion est automatique selon l'unité choisie."),
      uiOutput("ldl_points_ui")
    ),
    card(
      card_header("5) Génétique (DNA)"),
      checkboxInput("dna5", "Mutation pathogène identifiée (LDLR, APOB, PCSK9)", value = FALSE)
    ),
    card(
      card_header("Résultats"),
      uiOutput("resume_cats"),
      tags$hr(),
      uiOutput("resume_total")
    )
  ),
  tags$footer(
    class = "text-center text-muted p-3",
    "DLCN — Outil d'aide à la décision clinique. © ", format(Sys.Date(), "%Y")
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  # Réinitialisation
  observeEvent(input$btn_reset, {
    updateTextInput(session, "nom", value = "")
    updateTextInput(session, "prenom", value = "")
    updateDateInput(session, "naissance", value = NA)
    updateRadioButtons(session, "unite", selected = "mg/dL")
    updateCheckboxInput(session, "fam1a", value = FALSE)
    updateCheckboxInput(session, "fam1b", value = FALSE)
    updateCheckboxInput(session, "fam1c", value = FALSE)
    updateCheckboxInput(session, "fam1d", value = FALSE)
    updateCheckboxInput(session, "cli2a", value = FALSE)
    updateCheckboxInput(session, "cli2b", value = FALSE)
    updateCheckboxInput(session, "phy3a", value = FALSE)
    updateCheckboxInput(session, "phy3b", value = FALSE)
    updateCheckboxInput(session, "dna5", value = FALSE)
    updateTextInput(session, "ldl", value = "")
  })

  # Exemple
  observeEvent(input$btn_demo, {
    updateTextInput(session, "nom", value = "Durand")
    updateTextInput(session, "prenom", value = "Alex")
    updateDateInput(session, "naissance", value = as.Date("1985-07-12"))
    updateRadioButtons(session, "unite", selected = "mmol/L")
    updateCheckboxInput(session, "fam1a", value = TRUE)
    updateCheckboxInput(session, "fam1b", value = FALSE)
    updateCheckboxInput(session, "fam1c", value = FALSE)
    updateCheckboxInput(session, "fam1d", value = FALSE)
    updateCheckboxInput(session, "cli2a", value = FALSE)
    updateCheckboxInput(session, "cli2b", value = TRUE)
    updateCheckboxInput(session, "phy3a", value = FALSE)
    updateCheckboxInput(session, "phy3b", value = FALSE)
    updateCheckboxInput(session, "dna5", value = FALSE)
    updateTextInput(session, "ldl", value = "5.3")
  })

  # Calculs par catégorie (max des sous-items)
  cat1_points <- reactive({
    vals <- c(
      if (isTRUE(input$fam1a)) 1 else 0,
      if (isTRUE(input$fam1b)) 1 else 0,
      if (isTRUE(input$fam1c)) 2 else 0,
      if (isTRUE(input$fam1d)) 2 else 0
    )
    max(vals)
  })

  cat2_points <- reactive({
    vals <- c(
      if (isTRUE(input$cli2a)) 2 else 0,
      if (isTRUE(input$cli2b)) 1 else 0
    )
    max(vals)
  })

  cat3_points <- reactive({
    vals <- c(
      if (isTRUE(input$phy3a)) 6 else 0,
      if (isTRUE(input$phy3b)) 4 else 0
    )
    max(vals)
  })

  ldl_mgdl <- reactive({
    convert_to_mgdl(input$ldl, input$unite)
  })

  cat4_points <- reactive({
    ldl_points(ldl_mgdl())
  })

  cat5_points <- reactive({
    if (isTRUE(input$dna5)) 8 else 0
  })

  total_points <- reactive({
    sum(c(cat1_points(), cat2_points(), cat3_points(), cat4_points(), cat5_points()))
  })

  # Détails LDL
  output$ldl_points_ui <- renderUI({
    req(input$ldl)
    mgdl <- ldl_mgdl()
    if (is.na(mgdl)) {
      return(tags$div(badge("Valeur LDL-C non valide", "secondary")))
    }
    tags$div(
      tags$p(HTML(paste0("LDL-C interprété : <b>", sprintf("%.0f", mgdl), " mg/dL</b>"))),
      tags$p(HTML(paste0("Points LDL-C : ", badge(ldl_points(mgdl), "info"))))
    )
  })

  # Résumé catégories
  output$resume_cats <- renderUI({
    tags$div(
      tags$p(HTML(paste0("1) Antécédents familiaux : ", badge(cat1_points(), "primary"))), class = "mb-1"),
      tags$p(HTML(paste0("2) Anamnèse clinique : ", badge(cat2_points(), "primary"))), class = "mb-1"),
      tags$p(HTML(paste0("3) Examen physique : ", badge(cat3_points(), "primary"))), class = "mb-1"),
      tags$p(HTML(paste0("4) LDL-C : ", badge(cat4_points(), "primary"))), class = "mb-1"),
      tags$p(HTML(paste0("5) ADN : ", badge(cat5_points(), "primary"))), class = "mb-1")
    )
  })

  # Résumé total + interprétation
  output$resume_total <- renderUI({
    tot <- total_points()
    cls <- classify_dlc(tot)
    tags$div(
      tags$h4(HTML(paste0("Score total DLCN : ", badge(tot, "dark")))),
      tags$p(HTML(paste0("Interprétation : ", badge(cls$label, cls$class))))
    )
  })

  # Rapport HTML à télécharger
  output$dl_rapport <- downloadHandler(
    filename = function() {
      nom <- ifelse(nchar(input$nom) > 0, input$nom, "patient")
      paste0("rapport_DLCN_", nom, "_", format(Sys.time(), "%Y%m%d-%H%M"), ".html")
    },
    content = function(file) {
      # Construire un petit rapport HTML autonome
      tot <- total_points()
      cls <- classify_dlc(tot)
      ldl_val <- ifelse(is.na(ldl_mgdl()), "ND", sprintf("%0.0f mg/dL", ldl_mgdl()))
      html <- paste0(
        "<html><head><meta charset='utf-8'><title>Rapport DLCN</title>",
        "<style>body{font-family:Arial,Helvetica,sans-serif;margin:2rem;}h1{margin-top:0;}table{border-collapse:collapse;}td,th{border:1px solid #ddd;padding:6px 10px;} .k{font-weight:bold;}</style>",
        "</head><body>",
        "<h1>Rapport DLCN (FH)</h1>",
        "<p><b>Nom:</b> ", htmlEscape(input$nom), " ", htmlEscape(input$prenom), "<br>",
        "<b>Date de naissance:</b> ", ifelse(!is.na(input$naissance), as.character(input$naissance), "—"), "<br>",
        "<b>Date du rapport:</b> ", format(Sys.time(), "%Y-%m-%d %H:%M"), "</p>",
        "<h2>Résultats</h2>",
        "<table>",
        "<tr><th>Catégorie</th><th>Points</th></tr>",
        "<tr><td>Antécédents familiaux</td><td>", cat1_points(), "</td></tr>",
        "<tr><td>Anamnèse clinique</td><td>", cat2_points(), "</td></tr>",
        "<tr><td>Examen physique</td><td>", cat3_points(), "</td></tr>",
        "<tr><td>LDL-C (", ldl_val, ")</td><td>", cat4_points(), "</td></tr>",
        "<tr><td>Génétique</td><td>", cat5_points(), "</td></tr>",
        "<tr><th>Total DLCN</th><th>", tot, "</th></tr>",
        "</table>",
        "<p><b>Interprétation :</b> ", cls$label, "</p>",
        "<p style='font-size:90%;color:#666'>Avertissement : ce score est un outil d'aide à la décision et ne remplace pas l'évaluation clinique complète. ",
        "En cas de données manquantes, le score peut être sous-estimé.</p>",
        "</body></html>"
      )
      writeLines(html, file, useBytes = TRUE)
    }
  )
}

shinyApp(ui, server)
