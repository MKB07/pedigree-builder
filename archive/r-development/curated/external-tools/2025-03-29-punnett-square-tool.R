# Archived R development file
# Original path: module autre/Punnett.R
# Original created: 2025-03-29 18:13:46
# Original modified: 2025-03-29 18:13:46
# Archive rationale: Standalone Punnett square educational tool that was not integrated into the main app.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

library(shiny)

# ---- Fonctions de génétique ----

generate_gametes_multi_parent <- function(alleles, parent_label) {
  expand.grid(lapply(seq_along(alleles), function(i) paste0(alleles[[i]], "<sup>", parent_label, "</sup>"))) |>
    apply(1, paste, collapse = " ")
}

create_punnett_multi_exposant <- function(gametes1, gametes2) {
  outer(gametes1, gametes2, Vectorize(function(g1, g2) {
    alleles_combined <- c(strsplit(g1, " ")[[1]], strsplit(g2, " ")[[1]])
    paste(sort(alleles_combined), collapse = " ")
  })) |> matrix(nrow = length(gametes1), dimnames = list(gametes1, gametes2))
}

extract_phenotype <- function(genotype, genes, dominance, allele_norm, allele_mut) {
  alleles_raw <- unlist(strsplit(gsub("<sup>P[12]</sup>", "", genotype), " "))
  n_traits <- length(genes)
  phenotype <- character(n_traits)
  phenotype_state <- character(n_traits)
  mutation_present <- logical(n_traits)
  
  for (i in seq_len(n_traits)) {
    idx1 <- (2 * i) - 1
    idx2 <- 2 * i
    a1 <- alleles_raw[idx1]
    a2 <- alleles_raw[idx2]
    
    dom <- dominance[i]
    mut <- allele_mut[i]
    norm <- allele_norm[i]
    disease_label <- genes[i]
    
    if (dom == "Dominant") {
      if (a1 == mut || a2 == mut) {
        phenotype[i] <- paste0("[", mut, "]")
        phenotype_state[i] <- disease_label
        mutation_present[i] <- TRUE
      } else {
        phenotype[i] <- paste0("[", norm, "]")
        phenotype_state[i] <- paste("Healthy for", disease_label)
        mutation_present[i] <- FALSE
      }
    } else {
      if (a1 == mut && a2 == mut) {
        phenotype[i] <- paste0("[", mut, "]")
        phenotype_state[i] <- disease_label
        mutation_present[i] <- TRUE
      } else if ((a1 == mut && a2 == norm) || (a1 == norm && a2 == mut)) {
        phenotype[i] <- paste0("[", norm, "/", mut, "]")
        phenotype_state[i] <- paste("Healthy Carrier for", disease_label)
        mutation_present[i] <- TRUE
      } else {
        phenotype[i] <- paste0("[", norm, "]")
        phenotype_state[i] <- paste("Healthy for", disease_label)
        mutation_present[i] <- FALSE
      }
    }
  }
  
  list(
    text = paste(phenotype, collapse = " "),
    state = paste(phenotype_state, collapse = " + "),
    mutated = any(mutation_present),
    mutated_traits = if (any(mutation_present)) paste(phenotype_state[mutation_present], collapse = ", ") else paste("Healthy for", paste(genes, collapse = ", "))
  )
}

# ---- Couleurs disponibles ----

color_choices <- c("Aucune" = "none",
                   "Rouge" = "#f8d7da",
                   "Orange" = "#fff3cd",
                   "Vert" = "#d4edda",
                   "Bleu" = "#d1ecf1",
                   "Violet" = "#e2d5f9")

# ---- UI ----

ui <- fluidPage(
  titlePanel("Punnett Square Calculator - Couleurs personnalisées par génotype"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_traits", "Nombre de traits :", min = 1, max = 3, value = 1),
      uiOutput("gene_inputs"),
      h4("Parent 1 :"),
      uiOutput("parent1_alleles"),
      h4("Parent 2 :"),
      uiOutput("parent2_alleles"),
      actionButton("go", "Afficher la Grille"),
      checkboxInput("show_freq", "Afficher les Fréquences", TRUE)
    ),
    
    mainPanel(
      h4("Carré de Punnett"),
      uiOutput("punnett"),  # tableau dynamique
      
      conditionalPanel(
        condition = "input.show_freq",
        h4("Fréquences des génotypes"),
        uiOutput("genotype_freq_ui"),  # tableau avec selectInputs intégrés
        
        tabsetPanel(
          tabPanel("Génotypes (détaillés)",
                   h4("Liste des génotypes détaillés"),
                   tableOutput("genotype_detailed")
          )
        ),
        h4("Fréquences des phénotypes"),
        tableOutput("phenotype_freq")
      )
    )
  )
)

# ---- SERVER ----

server <- function(input, output, session) {
  
  selected_colors <- reactiveValues()
  
  output$gene_inputs <- renderUI({
    lapply(1:input$n_traits, function(i) {
      wellPanel(
        tags$b(paste("Trait", i)),
        textInput(paste0("gene_name_", i), "Pathologie :", paste0("Disease")),
        radioButtons(paste0("dominant_", i), "Transmission :", choices = c("Dominant", "Récessif"), selected = "Dominant", inline = TRUE),
        textInput(paste0("allele_normal_", i), "Allèle Normal :", "A"),
        textInput(paste0("allele_mut_", i), "Allèle Muté :", "a")
      )
    })
  })
  
  output$parent1_alleles <- renderUI({
    lapply(1:input$n_traits, function(i) {
      select1 <- selectInput(paste0("p1_a1_", i), "Allèle 1", choices = c())
      select2 <- selectInput(paste0("p1_a2_", i), "Allèle 2", choices = c())
      wellPanel(tags$b(paste("Trait", i)), select1, select2)
    })
  })
  
  output$parent2_alleles <- renderUI({
    lapply(1:input$n_traits, function(i) {
      select1 <- selectInput(paste0("p2_a1_", i), "Allèle 1", choices = c())
      select2 <- selectInput(paste0("p2_a2_", i), "Allèle 2", choices = c())
      wellPanel(tags$b(paste("Trait", i)), select1, select2)
    })
  })
  
  observe({
    lapply(1:input$n_traits, function(i) {
      choices <- c(input[[paste0("allele_normal_", i)]], input[[paste0("allele_mut_", i)]])
      updateSelectInput(session, paste0("p1_a1_", i), choices = choices)
      updateSelectInput(session, paste0("p1_a2_", i), choices = choices)
      updateSelectInput(session, paste0("p2_a1_", i), choices = choices)
      updateSelectInput(session, paste0("p2_a2_", i), choices = choices)
    })
  })
  
  observeEvent(input$go, {
    traits <- 1:input$n_traits
    genes <- sapply(traits, function(i) input[[paste0("gene_name_", i)]])
    dominance <- sapply(traits, function(i) input[[paste0("dominant_", i)]])
    allele_norm <- sapply(traits, function(i) input[[paste0("allele_normal_", i)]])
    allele_mut <- sapply(traits, function(i) input[[paste0("allele_mut_", i)]])
    
    parent1_list <- lapply(traits, function(i) {
      c(input[[paste0("p1_a1_", i)]], input[[paste0("p1_a2_", i)]])
    })
    
    parent2_list <- lapply(traits, function(i) {
      c(input[[paste0("p2_a1_", i)]], input[[paste0("p2_a2_", i)]])
    })
    
    gametes1 <- generate_gametes_multi_parent(parent1_list, "P1")
    gametes2 <- generate_gametes_multi_parent(parent2_list, "P2")
    
    punnett <- create_punnett_multi_exposant(gametes1, gametes2)
    
    raw_genotypes <- c(punnett)
    clean_genotypes <- gsub("<sup>P[12]</sup>", "", raw_genotypes)
    standardized_genotypes <- sapply(strsplit(clean_genotypes, " "), function(x) {
      paste(sort(x), collapse = " ")
    })
    
    genotype_freq <- table(standardized_genotypes) |> as.data.frame()
    names(genotype_freq) <- c("Génotype", "Count")
    genotype_freq$Percent <- round(genotype_freq$Count / sum(genotype_freq$Count) * 100, 2)
    
    # ✅ Bloc corrigé sans "div row"
    output$genotype_freq_ui <- renderUI({
      genos <- as.character(genotype_freq$Génotype)
      
      rows <- lapply(seq_along(genos), function(i) {
        fluidRow(
          column(3, strong(genos[i])),
          column(2, genotype_freq$Count[i]),
          column(2, paste0(genotype_freq$Percent[i], " %")),
          column(4, selectInput(inputId = paste0("color_", i),
                                label = NULL,
                                choices = color_choices,
                                selected = selected_colors[[genos[i]]] %||% "none"))
        )
      })
      
      do.call(tagList, c(
        list(
          fluidRow(
            column(3, strong("Génotype")),
            column(2, strong("Count")),
            column(2, strong("Percent")),
            column(4, strong("Couleur"))
          )
        ),
        rows
      ))
    })
    
    # Enregistre les sélections de couleurs
    observe({
      genos <- as.character(genotype_freq$Génotype)
      lapply(seq_along(genos), function(i) {
        observeEvent(input[[paste0("color_", i)]], {
          selected_colors[[genos[i]]] <- input[[paste0("color_", i)]]
        }, ignoreInit = TRUE)
      })
    })
    
    # Affichage du carré de Punnett avec couleurs personnalisées
    output$punnett <- renderUI({
      html <- "<table class='table table-bordered' style='text-align:center;'>"
      html <- paste0(html, "<thead><tr><th>Gamète</th>")
      for (col in colnames(punnett)) {
        html <- paste0(html, "<th>", col, "</th>")
      }
      html <- paste0(html, "</tr></thead><tbody>")
      
      for (i in seq_len(nrow(punnett))) {
        html <- paste0(html, "<tr><th>", rownames(punnett)[i], "</th>")
        for (j in seq_len(ncol(punnett))) {
          geno_html <- punnett[i, j]
          geno_clean <- gsub("<sup>P[12]</sup>", "", geno_html)
          geno_key <- paste(sort(strsplit(geno_clean, " ")[[1]]), collapse = " ")
          color <- selected_colors[[geno_key]]
          cell_style <- if (!is.null(color) && color != "none") paste0("background-color:", color, ";") else ""
          html <- paste0(html, "<td style='", cell_style, "'>", geno_html, "</td>")
        }
        html <- paste0(html, "</tr>")
      }
      
      html <- paste0(html, "</tbody></table>")
      HTML(html)
    })
    
    output$genotype_detailed <- renderTable({
      df <- data.frame(Génotype = c(punnett))
      df
    }, sanitize.text.function = function(x) x)
    
    output$phenotype_freq <- renderTable({
      phenos <- lapply(as.character(genotype_freq$Génotype), extract_phenotype,
                       genes = genes, dominance = dominance,
                       allele_norm = allele_norm, allele_mut = allele_mut)
      genotype_freq$Phénotype <- sapply(phenos, function(x) x$text)
      genotype_freq$Etat <- sapply(phenos, function(x) x$state)
      phenotype_freq <- aggregate(Count ~ Phénotype + Etat, data = genotype_freq, sum)
      phenotype_freq$Percent <- round(phenotype_freq$Count / sum(phenotype_freq$Count) * 100, 2)
      phenotype_freq
    })
  })
}


shinyApp(ui = ui, server = server)
