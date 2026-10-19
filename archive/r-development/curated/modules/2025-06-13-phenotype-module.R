# Archived R development file
# Original path: 🧩 Versions_Support/Module/Module_pheno.R
# Original created: 2025-06-13 16:14:46
# Original modified: 2025-06-13 16:14:46
# Archive rationale: Standalone phenotype module prototype.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

library(shiny)
library(pedtools)

ui <- fluidPage(
  titlePanel("Pedigree & Phénotypes graphiques"),
  sidebarLayout(
    sidebarPanel(
      h4("Individus du pedigree"),
      uiOutput("individualsUI"),
      actionButton("newPheno", "Créer un phénotype"),
      hr(),
      h4("Phénotypes sauvegardés"),
      uiOutput("phenoButtonsUI")
    ),
    mainPanel(
      h3("Pedigree principal"),
      plotOutput("mainPedPlot", height = "350px", click = "plot_click")
    )
  )
)

server <- function(input, output, session) {
  # Pedigree principal
  ped <- reactiveVal(
    nuclearPed(
      nch = 2,
      sex = c(1, 2),
      father = "Father",
      mother = "Mother",
      children = c("Boy", "Girl")
    )
  )
  indList <- reactive({ labels(ped()) })
  
  # Suivi de l'individu sélectionné
  selectedInd <- reactiveVal(NULL)
  
  # Styles graphiques appliqués à chaque individu
  styles <- reactiveValues(
    fill = NULL,
    lty = NULL,
    col = 2,
    hatched = character(0)
  )
  
  # Initialisation des styles au démarrage ou si indList change
  observeEvent(indList(), {
    styles$fill <- setNames(rep("white", length(indList())), indList())
    styles$col <- setNames(rep("black", length(indList())), indList())
    styles$lty <- setNames(rep("solid", length(indList())), indList())
    styles$hatched <- character(0)
    # réinitialiser la sélection si les IDs changent
    selectedInd(NULL)
  }, ignoreInit = FALSE)
  
  # Liste des phénotypes sauvegardés
  phenotypes <- reactiveValues(list = list())
  
  # UI dynamique des individus
  output$individualsUI <- renderUI({
    radioButtons("selectedIndRadio", "Sélectionner un individu",
                 choices = indList(), selected = selectedInd())
  })
  
  # MAJ de la sélection d'individu
  observeEvent(input$selectedIndRadio, {
    selectedInd(input$selectedIndRadio)
  })
  
  # Boutons de phénotype dynamiques
  # Affichage boutons et mini-légende pour chaque phénotype
  output$phenoButtonsUI <- renderUI({
    if (length(phenotypes$list) == 0) return("Aucun phénotype défini.")
    lapply(names(phenotypes$list), function(nom) {
      fluidRow(
        column(6,
               actionButton(paste0("applypheno_", nom), label = nom, width = "100%")
        ),
        column(6,
               plotOutput(paste0("legendplot_", nom), height = "40px", width = "70px")
        )
      )
    })
  })
  
  # Mini-apercu visuel pour chaque phénotype (légende)
  observe({
    lapply(names(phenotypes$list), function(nom) {
      output[[paste0("legendplot_", nom)]] <- renderPlot({
        ph <- phenotypes$list[[nom]]
        y <- singletons("leg", sex = 0)
        hach <- if (isTRUE(ph$hatched)) "leg" else NULL
        par(mar = c(0, 0, 0, 0))   
        plot(y,
             fill = ph$fill,
             col = ph$col,
             lty = ph$lty,
             hatched = hach,
             symbolsize = 0.9, cex = 15.8, main = "", axes = FALSE, labs = NA)
      })
    })
  })
  
  
  # Affichage du pedigree principal
  output$mainPedPlot <- renderPlot({
  fillVec <- styles$fill
    ltyVec <- styles$lty
    colVec <- styles$col
    hatchedVec <- styles$hatched
    plot(ped(), fill = fillVec, lty = ltyVec, col = colVec , hatched = hatchedVec,
         symbolsize = 2, cex = 1.5, main = "")
  })
  
  # --- PHÉNOTYPE CREATION MODALE ---
  observeEvent(input$newPheno, {
    showModal(
      modalDialog(
        title = "Créer un nouveau phénotype",
        fluidRow(
          column(6,
                 h5("Prévisualisation :"),
                 plotOutput("previewPheno", height = "200px")
          ),
          column(6,
                 selectInput("pheno_fill", "Couleur du fond", choices = colors(), selected = "blue"),
                 selectInput("pheno_col", "Couleur du contour", choices = colors(), selected = "black"),
                  selectInput("pheno_lty", "Motifs",
                             choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dashed"),
                 checkboxInput("pheno_hatched", "Motifs fond", value = FALSE),
                 textInput("pheno_name", "Nom du phénotype")
          )
        ),
        footer = tagList(
          modalButton("Annuler"),
          actionButton("savePheno", "Sauvegarder")
        ),
        size = "l", easyClose = TRUE
      )
    )
  })
  
  # Prévisualisation dans la modale
  output$previewPheno <- renderPlot({
    y <- singletons("test", sex = 0)
    hach <- if (isTRUE(input$pheno_hatched)) "test" else NULL
    plot(y,
         fill = input$pheno_fill %||% "blue",
         col = input$pheno_col %||% "black",
         lty = input$pheno_lty %||% "solid",
         hatched = hach,
         symbolsize = 2, cex = 1.5, main = "")
  })
  
  # Sauvegarde du phénotype
  observeEvent(input$savePheno, {
    req(input$pheno_name, nzchar(input$pheno_name))
    nom <- input$pheno_name
    # On évite les noms en double
    if (nom %in% names(phenotypes$list)) {
      showNotification("Nom de phénotype déjà utilisé.", type = "error")
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
  
  # Application d'un phénotype à l'individu sélectionné
  observe({
    req(selectedInd())
    lapply(names(phenotypes$list), function(nom) {
      observeEvent(input[[paste0("applypheno_", nom)]], {
        ph <- phenotypes$list[[nom]]
        # MAJ du style de l'individu sélectionné
        styles$fill[selectedInd()] <- ph$fill
        styles$col[selectedInd()] <- ph$col
        styles$lty[selectedInd()] <- ph$lty
        if (isTRUE(ph$hatched)) {
          styles$hatched <- unique(c(styles$hatched, selectedInd()))
        } else {
          styles$hatched <- setdiff(styles$hatched, selectedInd())
        }
      }, ignoreInit = TRUE)
    })
  })
}

shinyApp(ui, server)
