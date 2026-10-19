# Archived R development file
# Original path: 🧩 Versions_Support/Module/swapindividual.R
# Original created: 2025-06-11 22:53:00
# Original modified: 2025-09-22 02:32:50
# Archive rationale: Prototype for moving or swapping individuals in pedigree tables.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

library(shiny)
library(pedtools)
library(rhandsontable)
library(data.table)

# ------------ Utils de conversion ------------
pedigreeToTable <- function(ped) {
  df <- as.data.frame(ped)
  df$Sexe <- c("Inconnu", "Homme", "Femme")[df$sex + 1]
  df$Pere <- df$fid
  df$Mere <- df$mid
  df$Decede <- FALSE
  df <- df[, c("id", "Sexe", "Pere", "Mere", "Decede")]
  colnames(df)[1] <- "ID"
  # Colonnes "custom" affichables sous le symbole
  df$`prénom` <- NA_character_
  df$nom <- NA_character_
  df$commentaire <- NA_character_
  df
}

# Validation + reconstruction
tableToPedigree <- function(df) {
  need <- c("ID", "Pere", "Mere", "Sexe")
  miss <- setdiff(need, names(df))
  if (length(miss)) {
    showNotification(paste("Colonnes manquantes :", paste(miss, collapse = ", ")), type = "error")
    return(NULL)
  }

  # Nettoyage minimal
  df$ID <- as.character(df$ID)
  df$Pere <- ifelse(is.na(df$Pere) | df$Pere == "", NA, as.character(df$Pere))
  df$Mere <- ifelse(is.na(df$Mere) | df$Mere == "", NA, as.character(df$Mere))
  df$Sexe <- as.character(df$Sexe)

  # Contrôle: parents doivent exister (si non NA)
  parents <- unique(c(df$Pere, df$Mere))
  parents <- parents[!is.na(parents)]
  manquants <- setdiff(parents, df$ID)
  if (length(manquants)) {
    showNotification(
      paste0("❌ Parent(s) inexistant(s) dans ID : ", paste(manquants, collapse = ", ")),
      type = "error"
    )
    return(NULL)
  }

  # Mapping sexe
  df$sex <- match(df$Sexe, c("Inconnu", "Homme", "Femme")) - 1
  if (any(is.na(df$sex))) {
    showNotification("❌ Valeurs 'Sexe' invalides (utilisez Inconnu/Homme/Femme).", type = "error")
    return(NULL)
  }

  # Reconstruction
  tryCatch(
    ped(id = df$ID, fid = df$Pere, mid = df$Mere, sex = df$sex),
    error = function(e) {
      showNotification(paste("❌ Erreur de reconstruction :", e$message), type = "error")
      NULL
    }
  )
}

# Construit les étiquettes multilignes pour plot()
buildLabsFromTable <- function(df, idOrder) {
  m <- match(idOrder, df$ID)
  labs <- vapply(seq_along(idOrder), function(i) {
    r <- df[m[i], , drop = FALSE]
    parts <- c()

    # Ligne 1 : ID
    parts <- c(parts, as.character(idOrder[i]))

    # Ligne 2 : Nom + Prénom
    full <- paste(na.omit(c(r$nom, r$`prénom`)), collapse = " ")
    if (!is.null(full) && nzchar(full)) parts <- c(parts, full)

    # Ligne 3 : Commentaire
    cm <- r$commentaire
    if (!is.null(cm) && !is.na(cm) && nzchar(cm)) parts <- c(parts, cm)

    paste(parts, collapse = "\n")
  }, character(1))
  names(labs) <- idOrder
  labs
}

# ------------ UI ------------
ui <- fluidPage(
  titlePanel("Test réorganisation des individus dans un pedigree aléatoire"),
  sidebarLayout(
    sidebarPanel(
      h4("Générer un pedigree aléatoire"),
      actionButton("randomPed", "🔀 Générer un pedigree aléatoire", class = "btn btn-info"),
      hr(),
      selectInput("selected_id", "Sélectionner un individu :", choices = NULL),
      h4("Réorganiser les individus (glisser l'ordre) :"),
      selectizeInput(
        "new_order", "Nouvel ordre :",
        choices = NULL, multiple = TRUE,
        options = list(plugins = list("drag_drop"))
      ),
      actionButton("apply_order", "Appliquer l'ordre")
    ),
    mainPanel(
      h4("Affichage du pedigree :"),
      plotOutput("pedPlot", height = 350),
      verbatimTextOutput("order_text"),
      verbatimTextOutput("selected_indiv"),
      hr(),
      uiOutput("ped_info"),
      hr(),
      h4("📋 Données du pedigree :"),
      rHandsontableOutput("pedigreeTable")
    )
  )
)

# ------------ Server ------------
server <- function(input, output, session) {
  # Pedigree courant (source de vérité)
  currentPed <- reactiveVal(randomPed(n = 5, founders = 2))

  # Données tabulaires éditables (inclut colonnes custom)
  values <- reactiveValues(pedLoaded = NULL)

  # Init / sync des données tabulaires quand le pedigree change
  observe({
    ped <- currentPed()
    values$pedLoaded <- as.data.table(pedigreeToTable(ped))
  })

  # Génération aléatoire
  observeEvent(input$randomPed, {
    ped <- randomPed(n = 5, founders = 2)
    currentPed(ped)
    updateSelectizeInput(session, "new_order",
      choices = labels(ped), selected = labels(ped), server = TRUE
    )
    updateSelectInput(session, "selected_id",
      choices = labels(ped), selected = labels(ped)[1]
    )
  })

  # Texte d'ordre
  output$order_text <- renderPrint({
    paste("Ordre courant des individus :", paste(labels(currentPed()), collapse = "  »  "))
  })

  # ---- PLOT avec affichage des nouvelles colonnes ----
  output$pedPlot <- renderPlot({
    req(currentPed(), values$pedLoaded)
    ped <- currentPed()
    df <- as.data.frame(values$pedLoaded)

    # Labels multi-lignes
    idOrder <- labels(ped)
    labs <- buildLabsFromTable(df, idOrder)

    # Option : marquer les décédés à partir de la colonne 'Decede'
    decedes <- df$ID[which(df$Decede %in% TRUE)]

    # Tracé
    plot(
      ped,
      labs = labs, # ID / Nom-Prénom / Commentaire
      margin = c(2, 2, 2, 2),
      cex = 1.2,
      deceased = decedes # si votre version de pedtools le supporte
    )
    title("Affichage pedigree", col.main = "#3c8dbc", cex.main = 1.4)
  })

  # Appliquer un nouvel ordre -> internal = TRUE (indices internes)
  observeEvent(input$apply_order, {
    req(input$new_order)
    ped <- currentPed()
    all_ids <- labels(ped)
    new_ids <- input$new_order

    if (!setequal(new_ids, all_ids)) {
      showNotification("Incluez tous les individus dans l'ordre !", type = "error")
      return(NULL)
    }

    # Convertir l'ordre d'IDs en indices internes
    idx_order <- match(new_ids, all_ids)

    # Réordonner via indices internes
    ped2 <- reorderPed(ped, idx_order, internal = TRUE)
    currentPed(ped2) # met à jour plot + table via observers
  })

  # Sélecteurs synchronisés
  observe({
    ped <- currentPed()
    updateSelectizeInput(session, "new_order",
      choices = labels(ped), selected = labels(ped), server = TRUE
    )
    updateSelectInput(session, "selected_id",
      choices = labels(ped), selected = labels(ped)[1]
    )
  })

  # Individu sélectionné (feedback)
  output$selected_indiv <- renderPrint({
    req(input$selected_id)
    paste("Individu sélectionné :", input$selected_id)
  })

  # Infos familiales
  output$ped_info <- renderUI({
    req(input$selected_id)
    ped <- currentPed()
    id <- input$selected_id

    pere <- father(ped, id)
    mere <- mother(ped, id)
    fratrie <- siblings(ped, id, half = NA)
    enfants <- children(ped, id)

    tagList(
      h4(paste("Famille de l’individu sélectionné :", id)),
      tags$ul(
        tags$li(
          strong("Parents : "),
          paste0(
            ifelse(!is.na(pere), paste("Père =", pere), "Père inconnu"), ", ",
            ifelse(!is.na(mere), paste("Mère =", mere), "Mère inconnue")
          )
        ),
        tags$li(
          strong("Frères/Sœurs : "),
          if (length(fratrie) > 0) paste(fratrie, collapse = ", ") else "Aucun frère/sœur"
        ),
        tags$li(
          strong("Enfants : "),
          if (length(enfants) > 0) paste(enfants, collapse = ", ") else "Aucun enfant"
        )
      )
    )
  })

  # ---- Table éditable ----
  output$pedigreeTable <- renderRHandsontable({
    req(values$pedLoaded)
    df <- values$pedLoaded
    rh <- rhandsontable(
      df,
      useTypes = TRUE,
      manualColumnResize = TRUE,
      rowHeaders = NULL,
      colHeaders = colnames(df),
      selectCallback = TRUE
    ) %>%
      hot_col("Sexe", type = "dropdown", source = c("Inconnu", "Homme", "Femme")) %>%
      hot_col("Decede", type = "checkbox") %>%
      hot_col("prénom", type = "text", colWidths = "110px") %>%
      hot_col("nom", type = "text", colWidths = "110px") %>%
      hot_col("commentaire", type = "text", colWidths = "150px") %>%
      hot_col("ID", readOnly = TRUE) %>% # éviter de casser les correspondances
      hot_col("Pere", type = "text") %>%
      hot_col("Mere", type = "text")
    rh
  })

  # Sync des éditions -> reconstruction + replot
  observeEvent(input$pedigreeTable, {
    new_data <- hot_to_r(input$pedigreeTable)
    if (is.null(new_data)) {
      return()
    }

    # Mémoriser la table (pour les labels)
    values$pedLoaded <- as.data.table(new_data)

    # Reconstruire la structure pedigree (parents/sexes) à partir des colonnes dédiées
    ped2 <- tableToPedigree(new_data)
    if (!is.null(ped2)) currentPed(ped2)
  })
}

shinyApp(ui, server)
