# Archived R development file
# Original path: version obsolette/legend_kinship.R
# Original created: 2024-08-17 23:41:20
# Original modified: 2024-08-17 23:41:20
# Archive rationale: Early kinship legend and relationship display experiment.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# Charger les bibliothèques nécessaires
library(shiny)
library(kinship2)

ui <- fluidPage(
  titlePanel("Pedigree Plot with Customizable Legend"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("show_legend", "Show Legend", value = TRUE),
      textInput("legend_labels", "Legend Labels (comma separated)", "Affected, Unaffected"),
      textInput("legend_colors", "Legend Colors (comma separated)", "red, blue"),
      numericInput("legend_radius", "Legend Radius", value = 0.8, min = 0.1, max = 2, step = 0.1),
      numericInput("legend_density", "Legend Density", value = -1, step = 1),
      numericInput("legend_angle", "Legend Angle", value = 90, step = 1),
      selectInput("legend_location", "Legend Location", choices = c("bottomright", "bottomleft", "topleft", "topright"))
    ),
    mainPanel(
      plotOutput("pedplot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  # Segregation plot
  output$pedplot <- renderPlot({
    # Charger un exemple de données de pedigree
    data(sample.ped)
    
    # Filtrer pour une famille spécifique
    fam <- sample.ped[sample.ped$ped == 2, ]
    
    # Créer un objet pedigree
    ped <- with(fam, pedigree(id, father, mother, sex, affected = cbind(avail, affected)))
    
    # Tracer le pedigree
    plot(ped)
    
    # Ajouter la légende si l'option est cochée
    if (input$show_legend) {
      legend_labels <- strsplit(input$legend_labels, ",")[[1]]
      legend_colors <- strsplit(input$legend_colors, ",")[[1]]
      
      # Définir les états affectés pour la légende
      legend_states <- unique(as.vector(ped$affected))
      legend_labels <- legend_labels[1:length(legend_states)]
      legend_colors <- legend_colors[1:length(legend_states)]
      
      legend(
        input$legend_location,
        legend = legend_labels,
        fill = legend_colors,
        bty = "n",
        cex = 1.2
      )
    }
  }, execOnResize = TRUE)
  
}

shinyApp(ui = ui, server = server)
