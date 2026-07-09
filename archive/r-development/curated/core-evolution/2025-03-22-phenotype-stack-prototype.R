# Archived R development file
# Original path: version obsolette/ped_stackPheno.R
# Original created: 2025-03-22 10:35:10
# Original modified: 2025-03-22 10:35:10
# Archive rationale: Prototype for stacking and displaying phenotype information.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

library(shiny)
library(pedtools)
library(rhandsontable)
library(shinyWidgets)
library(data.table)
library(colourpicker)

ui <- fluidPage(
  titlePanel("Shiny Application with pedtools and rhandsontable"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Pedigree Type"),
      pickerInput(
        inputId = "pedCases",
        label = "Choose a pedigree:",
        choices = c("Trio", "Full siblings", "Grandparent", 
                    "Great-grandparent", "Half siblings (mat)", 
                    "Half siblings (pat)", "Avuncular"),
        selected = "Trio"
      ),
      h4("Family Tree"),
      plotOutput("pedigreePlot", click = "plot_click"),
      
      h4("Available Actions"),
      uiOutput("selectedIndividual"),
      
      hr(),
      h4("Define Phenotypes"),
      textInput("phenoName", "Phenotype Name"),
      colourInput("phenoColor", "Phenotype Color", value = "#000000"),
      pickerInput("phenoPattern", "Phenotype Pattern", 
                  choices = c("solid", "stripe", "dotted")),
      actionButton("addPheno", "Add Phenotype")
    ),
    
    mainPanel(
      h4("Individual Data"),
      rHandsontableOutput("pedigreeTable"),
      hr(),
      h4("Defined Phenotypes"),
      tableOutput("phenoTable")
    )
  )
)

# Serveur complet
server <- function(input, output, session) {
  
  values <- reactiveValues(
    pedData = NULL, 
    pedLoaded = NULL, 
    pedTotal = 0, 
    pedCurrent = 1, 
    customShapes = list(), 
    phenotypes = list(), 
    individualPhenos = list()
  )
  selected_ind <- reactiveVal(NULL)
  
  # Fonction pour changer le sexe
  changeSex <- function(ped, ids, sex, twins = NULL) {
    if (sex == 0) {
      if (!all(ids %in% leaves(ped)))
        stop("Only individuals without children can have unknown sex")
      newped <- setSex(ped, ids, sex = 0)
      return(newped)
    }
    
    currentSex <- getSex(ped, ids)
    
    newped <- ped |>
      swapSex(ids[currentSex == (3 - sex)], verbose = FALSE) |>
      setSex(ids[currentSex == 0], sex = sex)
    
    if (!is.null(twins)) {
      mz <- twins[twins$code == 1, , drop = FALSE]
      if (nrow(mz) > 0) {
        sx1 <- getSex(newped, mz$id1)
        sx2 <- getSex(newped, mz$id2)
        if (any(sx1 > 0 & sx2 > 0 & sx1 != sx2))
          stop("Cannot change sex of one MZ twin")
      }
    }
    
    newped
  }
  
  # Chargement du pedigree basé sur la sélection
  observeEvent(input$pedCases, {
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
      values$pedLoaded <- pedLoaded
      values$pedData <- pedData
    }
  }) 
  
  # Affichage du pedigree avec les formes personnalisées et phénotypes
  output$pedigreePlot <- renderPlot({
    req(values$pedLoaded)
    
    ped <- values$pedLoaded
    
    # Initialisation des couleurs et formes
    col <- rep("black", length(labels(ped)))
    shapes <- rep(NA, length(labels(ped)))
    
    # Application des phénotypes aux individus
    for (ind in names(values$individualPhenos)) {
      pheno <- values$phenotypes[[values$individualPhenos[[ind]]]]
      idx <- which(labels(ped) == ind)
      if (!is.null(pheno$color)) {
        col[idx] <- pheno$color
      }
      if (!is.null(pheno$pattern)) {
        shapes[idx] <- pheno$pattern
      }
    }
    
    # Mise en surbrillance de l'individu sélectionné
    if (!is.null(selected_ind())) {
      col[which(labels(ped) == selected_ind())] <- "#71C7E0"
    }
    
    # Tracé du pedigree
    plot(ped, col = col, id = TRUE, shape = shapes)
  })
  
  # Affichage du tableau des données du pedigree
  output$pedigreeTable <- renderRHandsontable({
    req(values$pedData)
    rhandsontable(values$pedData)
  })
  
  # Synchronisation des changements depuis le tableau vers le graphique du pedigree
  observeEvent(input$pedigreeTable$changes$changes, {
    new_data <- hot_to_r(input$pedigreeTable)
    values$pedData <- new_data
    tryCatch({
      updated_ped <- as.ped(new_data)
      updatePed(updated_ped)
    }, error = function(e) {
      showModal(modalDialog(title = "Error", e$message))
    })
  })
  
  # Sélection d'un individu sur le graphique
  observeEvent(input$plot_click, {
    click_data <- input$plot_click
    ped <- values$pedLoaded
    selected <- nearestIndividual(click_data, ped)
    
    if (!is.null(selected)) {
      selected_ind(selected)
    } else {
      selected_ind(NULL)
    }
  })
  
  # Affichage des boutons et détails de l'individu sélectionné
  output$selectedIndividual <- renderUI({
    if (!is.null(selected_ind())) {
      tagList(
        h4(paste("Selected Individual:", selected_ind())),
        actionButton("addson", "Add Son"),
        actionButton("adddaughter", "Add Daughter"),
        actionButton("addsibRight", "Add Sibling to the Right"),
        actionButton("addsibLeft", "Add Sibling to the Left"),
        actionButton("addparents", "Add Parents"),
        hr(),
        h4("Change Symbol:"),
        actionButton("setMale", "Male"),
        actionButton("setFemale", "Female"),
        actionButton("setUnknown", "Unknown"),
        hr(),
        h4("Assign Phenotype"),
        uiOutput("phenotypeSelector")
      )
    } else {
      h4("No individual selected.")
    }
  })
  
  # Sélecteur de phénotype
  output$phenotypeSelector <- renderUI({
    if (length(values$phenotypes) > 0) {
      selectInput("selectPheno", "Select Phenotype", 
                  choices = names(values$phenotypes))
    }
  })
  
  # Appliquer un phénotype à l'individu sélectionné
  observeEvent(input$selectPheno, {
    pheno <- input$selectPheno
    ind <- selected_ind()
    if (!is.null(pheno) && !is.null(ind)) {
      values$individualPhenos[[ind]] <- pheno
      
      # Re-rendre le graphique du pedigree après avoir appliqué le phénotype
      output$pedigreePlot <- renderPlot({
        ped <- values$pedLoaded
        col <- rep("black", length(labels(ped)))
        shapes <- rep(NA, length(labels(ped)))
        
        for (ind in names(values$individualPhenos)) {
          pheno <- values$phenotypes[[values$individualPhenos[[ind]]]]
          idx <- which(labels(ped) == ind)
          col[idx] <- pheno$color
          shapes[idx] <- pheno$pattern
        }
        
        if (!is.null(selected_ind())) {
          col[which(labels(ped) == selected_ind())] <- "#71C7E0"
        }
        
        plot(ped, col = col, id = TRUE, shape = shapes)
      })
    }
  })
  
  # Ajout d'un phénotype à la liste
  observeEvent(input$addPheno, {
    newPheno <- list(
      name = input$phenoName,
      color = input$phenoColor,
      pattern = input$phenoPattern
    )
    values$phenotypes[[input$phenoName]] <- newPheno
  })
  
  # Affichage des phénotypes définis
  output$phenoTable <- renderTable({
    if (length(values$phenotypes) > 0) {
      do.call(rbind, lapply(values$phenotypes, as.data.frame))
    }
  })
  
  # Fonction pour mettre à jour le pedigree et les données
  updatePed <- function(new_ped) {
    values$pedLoaded <- new_ped
    values$pedData <- as.data.table(as.data.frame(new_ped))
  }
  
  # Fonction pour gérer les changements de sexe
  observeEvent(input$setMale, {
    id <- req(selected_ind())
    tryCatch({
      ped <- changeSex(values$pedLoaded, id, sex = 1)
      updatePed(ped)
    }, error = function(e) {
      showModal(modalDialog(title = "Error", e$message))
    })
  })
  
  observeEvent(input$setFemale, {
    id <- req(selected_ind())
    tryCatch({
      ped <- changeSex(values$pedLoaded, id, sex = 2)
      updatePed(ped)
    }, error = function(e) {
      showModal(modalDialog(title = "Error", e$message))
    })
  })
  
  observeEvent(input$setUnknown, {
    id <- req(selected_ind())
    tryCatch({
      ped <- changeSex(values$pedLoaded, id, sex = 0)
      updatePed(ped)
    }, error = function(e) {
      showModal(modalDialog(title = "Error", e$message))
    })
  })
  
  
  # Ajouter un fils
  observeEvent(input$addson, {
    id <- req(selected_ind())
    tryCatch({
      updatePed(addSon(values$pedLoaded, id, verbose = FALSE))
    }, error = function(e) {
      showModal(modalDialog(title = "Error", e$message))
    })
  })
  
  # Ajouter une fille
  observeEvent(input$adddaughter, {
    id <- req(selected_ind())
    tryCatch({
      updatePed(addDaughter(values$pedLoaded, id, verbose = FALSE))
    }, error = function(e) {
      showModal(modalDialog(title = "Error", e$message))
    })
  })
  
  # Ajouter un frère ou une sœur à droite
  observeEvent(input$addsibRight, {
    id <- req(selected_ind())
    tryCatch({
      updatePed(addSib(values$pedLoaded, id, side = "right"))
    }, error = function(e) {
      showModal(modalDialog(title = "Error", e$message))
    })
  })
  
  
  # Ajouter un frère ou une sœur à gauche
  observeEvent(input$addsibLeft, {
    id <- req(selected_ind())
    tryCatch({
      updatePed(addSib(values$pedLoaded, id, side = "left"))
    }, error = function(e) {
      showModal(modalDialog(title = "Error", e$message))
    })
  })
  
  
  # Ajouter des parents
  observeEvent(input$addparents, {
    ids <- req(selected_ind())
    tryCatch({
      updatePed(addPar(values$pedLoaded, ids))
    }, error = function(e) {
      showModal(modalDialog(title = "Error", e$message))
    })
  })
  
  # Synchronisation des changements depuis le tableau vers le graphique du pedigree
  observeEvent(input$pedigreeTable$changes$changes, {
    new_data <- hot_to_r(input$pedigreeTable)
    values$pedData <- new_data
    tryCatch({
      updated_ped <- as.ped(new_data)
      updatePed(updated_ped)
    }, error = function(e) {
      showModal(modalDialog(title = "Error", e$message))
    })
  })
  
  # Fonctions auxiliaires
  
  nearestIndividual <- function(click_data, ped) {
    coords <- locator2(ped)
    dist <- sqrt((coords$x - click_data$x)^2 + (coords$y - click_data$y)^2)
    closest <- which.min(dist)
    ids <- coords$id
    ids[closest]
  }
  
  locator2 <- function(ped) {
    plot_data <- plot(ped, id = TRUE, plot = FALSE)
    coords <- data.frame(
      x = plot_data$x,
      y = plot_data$y,
      id = labels(ped)
    )
    coords
  }
  
  addRelative <- function(pedData, ind, relation, values) {
    ped <- as.ped(pedData[pedData$ped == values$pedCurrent, c("id", "fid", "mid", "sex")])
    
    if (relation == "father") {
      ped <- addParents(ped, id = ind, father = generateLabs(labels(ped), n = 1))
    } else if (relation == "mother") {
      ped <- addParents(ped, id = ind, mother = generateLabs(labels(ped), n = 1))
    } else if (relation == "sibling") {
      ped <- addChildren(ped, father = pedData$fid[pedData$id == ind], mother = pedData$mid[pedData$id == ind], nch = 1, sex = sample(1:2, 1))
    } else if (relation == "child") {
      if (pedData$sex[pedData$id == ind] == 1) {
        ped <- addChildren(ped, father = ind, nch = 1, sex = sample(1:2, 1))
      } else {
        ped <- addChildren(ped, mother = ind, nch = 1, sex = sample(1:2, 1))
      }
    } else if (relation == "spouse") {
      if (pedData$sex[pedData$id == ind] == 1) {
        ped <- addParents(ped, id = generateLabs(labels(ped), n = 1), mother = ind)
      } else {
        ped <- addParents(ped, id = generateLabs(labels(ped), n = 1), father = ind)
      }
    }
    
    new_pedData <- as.data.table(ped)
    new_pedData$ped <- values$pedCurrent
    new_pedData
    
  }
  generateLabs <- function(existingLabels, n) {
    newLabels <- NULL
    start <- max(as.numeric(existingLabels), na.rm = TRUE) + 1
    for (i in seq_len(n)) {
      newLabels <- c(newLabels, as.character(start))
      start <- start + 1
    }
    newLabels
  }
}
shinyApp(ui = ui, server = server)