# Archived R development file
# Original path: version obsolette/basicped.R
# Original created: 2024-08-18 01:51:12
# Original modified: 2024-08-18 01:51:12
# Archive rationale: Early minimal pedigree builder prototype.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

library(shiny)
library(pedtools)
library(rhandsontable)
library(shinyWidgets)
library(data.table)

# Module pour gérer les pedigrees
pedigreeBoxServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$pedCases, {
      if (length(input$pedCases) == 1) {  # Vérifie que pedCases est une valeur unique
        pedLoaded <- switch(
          input$pedCases,
          "Trio" = nuclearPed(1),
          "Full siblings" = nuclearPed(2, sex = 1:2),
          "Grandparent" = ancestralPed(g = 2),
          "Great-grandparent" = ancestralPed(g = 3),
          "Half siblings (mat)" = halfSibPed(1, 1, sex2 = 2, type = "maternal"),
          "Half siblings (pat)" = halfSibPed(1, 1, sex2 = 2, type = "paternal"),
          "Avuncular" = avuncularPed()
        )
        
        if (!is.null(pedLoaded)) {
          pedData <- as.data.table(as.data.frame(pedLoaded))
          pedData[, Decede := FALSE]  # Ajouter une colonne pour indiquer si l'individu est décédé
          values$pedLoaded <- pedData
          values$pedObj <- pedLoaded  # Stocke l'objet pedigree pour le graphe
        }
      } else {
        print("input$pedCases does not have a length of 1.")
      }
    })
  })
}
# Fonction pour trouver l'individu le plus proche du clic
nearestIndividual <- function(click_data, ped) {
  coords <- locator2(ped)
  dist <- sqrt((coords$x - click_data$x)^2 + (coords$y - click_data$y)^2)
  closest <- which.min(dist)
  ids <- coords$id
  ids[closest]
}

# Fonction pour obtenir les coordonnées des individus sur le graphique
locator2 <- function(ped) {
  plot_data <- plot(ped, id = TRUE, plot = FALSE)
  coords <- data.frame(
    x = plot_data$x,
    y = plot_data$y,
    id = labels(ped)
  )
  coords
}
# Interface utilisateur
ui <- fluidPage(
  titlePanel("Application Shiny avec pedtools et rhandsontable"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Sélectionner le type de pedigree"),
      pickerInput(
        inputId = "pedCases",
        label = "Choisissez un pedigree:",
        choices = c("Trio", "Full siblings", "Grandparent", 
                    "Great-grandparent", "Half siblings (mat)", 
                    "Half siblings (pat)", "Avuncular"),
        selected = "Trio"
      ),
      h4("Arbre généalogique"),
      plotOutput("pedigreePlot", click = "plot_click")
    ),
    
    mainPanel(
      h4("Données des individus"),
      rHandsontableOutput("pedigreeTable"),
      textOutput("selectedIndividual")
    )
  )
)

# Serveur complet
server <- function(input, output, session) {
  
  values <- reactiveValues()
  
  # Stocker l'individu sélectionné
  selected_ind <- reactiveVal(NULL)
  
  # Observer les changements dans le pickerInput et mettre à jour le pedigree
  observeEvent(input$pedCases, {
    if (length(input$pedCases) == 1) {  # Vérifie que pedCases est une valeur unique
      pedLoaded <- switch(
        input$pedCases,
        "Trio" = nuclearPed(1),
        "Full siblings" = nuclearPed(2, sex = 1:2),
        "Grandparent" = ancestralPed(g = 2),
        "Great-grandparent" = ancestralPed(g = 3),
        "Half siblings (mat)" = halfSibPed(1, 1, sex2 = 2, type = "maternal"),
        "Half siblings (pat)" = halfSibPed(1, 1, sex2 = 2, type = "paternal"),
        "Avuncular" = avuncularPed()
      )
      
      if (!is.null(pedLoaded)) {
        pedData <- as.data.table(as.data.frame(pedLoaded))
        pedData[, Decede := FALSE]  # Ajouter une colonne pour indiquer si l'individu est décédé
        values$pedLoaded <- pedData
        values$pedObj <- pedLoaded  # Stocke l'objet pedigree pour le graphe
      }
      
      # Debugging: Print the pedLoaded object
      print(values$pedObj)
    } else {
      print("input$pedCases does not have a length of 1.")
    }
  })
  
  # Observer le clic sur le graphique pour sélectionner un individu
  observeEvent(input$plot_click, {
    click_data <- input$plot_click
    
    if (!is.null(values$pedObj)) {
      ped <- values$pedObj
      selected <- nearestIndividual(click_data, ped)
      
      if (!is.null(selected_ind()) && selected == selected_ind()) {
        selected_ind(NULL)  # Désélectionner si le même individu est cliqué
      } else {
        selected_ind(selected)  # Mettre à jour l'individu sélectionné
      }
    }
  })
  
  # Affichage de l'arbre généalogique avec indication de la sélection
  output$pedigreePlot <- renderPlot({
    req(values$pedObj)  # S'assurer qu'un pedigree est chargé
    
    ped <- values$pedObj
    
    if (!is.null(selected_ind())) {
      plot(ped, col = ifelse(labels(ped) == selected_ind(), "blue", "black"), lwd = 2, deceased = values$pedLoaded[Decede == TRUE, id])
    } else {
      plot(ped, col = "black", lwd = 2, deceased = values$pedLoaded[Decede == TRUE, id])
    }
  })
  
  # Affichage du tableau des données
  output$pedigreeTable <- renderRHandsontable({
    req(values$pedLoaded)  # S'assurer que les données sont chargées
    rhandsontable(values$pedLoaded) %>% 
      hot_col("Decede", type = "checkbox")  # Ajouter une case à cocher pour la colonne "Decede"
  })
  
  # Afficher l'individu sélectionné
  output$selectedIndividual <- renderText({
    if (!is.null(selected_ind())) {
      paste("Individu sélectionné:", selected_ind())
    } else {
      "Aucun individu sélectionné."
    }
  })
  
  # Observer les changements dans le tableau et mettre à jour le pedigree
  observeEvent(input$pedigreeTable, {
    if (!is.null(input$pedigreeTable)) {
      new_data <- hot_to_r(input$pedigreeTable)
      values$pedLoaded <- new_data
      
      # Mettre à jour l'objet pedigree en fonction des nouvelles données
      updated_ped <- as.ped(new_data[, -"Decede", with = FALSE])  # Ignore la colonne "Decede"
      values$pedObj <- updated_ped
    }
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
