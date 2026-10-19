# Archived R development file
# Original path: module autre/BMI.R
# Original created: 2025-03-29 19:16:54
# Original modified: 2025-03-29 19:16:54
# Archive rationale: Standalone BMI tool kept as non-integrated learning/support module.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Lato")),
  
  titlePanel("💪 Calculateur d'IMC (BMI)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("weight", "Poids (kg)", value = 70, min = 30, max = 200),
      numericInput("height", "Taille (cm)", value = 170, min = 100, max = 250),
      actionButton("calcButton", "Calculer", class = "btn btn-primary w-100")
    ),
    
    mainPanel(
      h4("Résultat"),
      br(),
      div(style = "font-size: 1.5em;", textOutput("bmi_value")),
      div(style = "font-size: 1.2em; font-weight: bold;", textOutput("bmi_category")),
      br(),
      plotOutput("bmi_graph", height = "220px")
    )
  )
)

server <- function(input, output) {
  
  bmi_calc <- eventReactive(input$calcButton, {
    height_m <- input$height / 100
    bmi <- input$weight / (height_m^2)
    round(bmi, 1)
  })
  
  output$bmi_value <- renderText({
    paste("Votre BMI est :", bmi_calc())
  })
  
  output$bmi_category <- renderText({
    bmi <- bmi_calc()
    if (bmi < 18.5) {
      "Catégorie : Sous-poids"
    } else if (bmi < 25) {
      "Catégorie : Poids normal"
    } else if (bmi < 30) {
      "Catégorie : Surpoids"
    } else {
      "Catégorie : Obésité"
    }
  })
  
  output$bmi_graph <- renderPlot({
    req(input$calcButton)
    h <- input$height / 100
    poids_user <- input$weight
    bmi_user <- bmi_calc()
    
    seuils <- data.frame(
      category = c("Sous-poids", "Poids normal", "Surpoids", "Obésité"),
      bmi_min = c(0, 18.5, 25, 30),
      bmi_max = c(18.5, 25, 30, 40),
      color = c("#E74C3C", "#27AE60", "#3498DB", "#F39C12")
    )
    
    seuils$weight_min <- round(seuils$bmi_min * h^2, 1)
    seuils$weight_max <- round(seuils$bmi_max * h^2, 1)
    seuils$label <- paste0(
      seuils$category, "\n", 
      seuils$weight_min, "–", seuils$weight_max, " kg"
    )
    
    poids_max_affiche <- max(seuils$weight_max, poids_user + 10)
    
    ggplot(seuils) +
      geom_rect(aes(xmin = weight_min, xmax = weight_max, ymin = 0, ymax = 1, fill = category), alpha = 0.8) +
      geom_text(aes(x = (weight_min + weight_max)/2, y = 0.5, label = label), size = 4, color = "white") +
      geom_vline(xintercept = poids_user, color = "black", linewidth = 1.5) +
      scale_fill_manual(values = seuils$color) +
      scale_x_continuous(
        breaks = seq(0, poids_max_affiche, by = 5),
        expand = expansion(mult = c(0.01, 0.01))
      ) +
      coord_cartesian(ylim = c(0, 1.1)) +
      labs(x = "Poids (kg)", y = NULL, fill = "Catégorie") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
  })
}

shinyApp(ui = ui, server = server)
