# Archived R development file
# Original path: 🧩 Versions_Support/Module/ajout Twins.R
# Original created: 2025-06-12 06:09:16
# Original modified: 2025-06-12 06:09:16
# Archive rationale: Prototype for adding and tracking twins.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

library(shiny)
library(pedtools)

# Fonctions utilitaires données dans la consigne -----------------------------------

stop2 = function (...) {
  a = lapply(list(...), toString)
  a = append(a, list(call. = FALSE))
  do.call(stop, a)
}

sortIds = function(x, ids) {
  intern = internalID(x, ids)
  ids[order(intern)]
}

addSib = function(x, id, sex = 1, side = c("right", "left")) {
  if(length(id) > 1)
    stop2("To add a sibling, please select exactly one individual. Current selection: ", sortIds(x, id))
  if(!is.ped(x))
    stop2("Cannot add sibling to disconnected pedigree")
  if(id %in% founders(x))
    x = addParents(x, id, verbose = FALSE)
  newped = addChild(x, parents(x, id), sex = sex, verbose = FALSE)
  idInt = internalID(x, id)
  n = length(x$ID)
  ord = switch(match.arg(side),
               left = c(seq_len(idInt-1), n+1, idInt:n),
               right = c(seq_len(idInt), n+1, if(idInt < n) seq.int(idInt+1, n)))
  reorderPed(newped, ord)
}

updateTwins = function(ped, twins, ids) {
  if(length(ids) != 2)
    stop2("To change twin status, please select exactly 2 individuals")
  ids = sort.default(ids)
  id1 = ids[1]
  id2 = ids[2]
  if(!identical(parents(ped, id1), parents(ped, id2)))
    stop2("Twins must have the same parents")
  if(id1 %in% founders(ped))
    stop2("Founders cannot be twins")
  sameSex = getSex(ped, id1) == getSex(ped, id2)
  rw = match(TRUE, nomatch = 0, twins$id1 == id1 & twins$id2 == id2)
  if(rw == 0)
    twins = rbind(twins, data.frame(id1 = id1, id2 = id2, code = 2 - sameSex))
  else if (twins$code[rw] < 3)
    twins$code[rw] = twins$code[rw] + 1
  else
    twins = twins[-rw, , drop = FALSE]
  if(nrow(twins)) twins else NULL
}

# ------------------------------------------------------------------------

# Pedigree initial selon la consigne
init_ped <- nuclearPed(
  nch = 2,
  sex = c(1, 2),
  father = "Father",
  mother = "Mother",
  children = c("Boy", "Girl")
)

ui <- fluidPage(
  titlePanel("Ajout de jumeau et de fausse couche dans un pedigree"),
  sidebarLayout(
    sidebarPanel(
      h4("Ajouter un jumeau (via sibling)"),
      selectInput("selected_id", "Sélectionner un individu :", choices = labels(init_ped)),
      selectInput("sibling_sex", "Sexe du sibling à ajouter :", choices = c("Garçon" = 1, "Fille" = 2)),
      selectInput("twin_type", "Type de jumeaux :", choices = c("Monozygote (MZ)" = 1, "Dizygote (DZ)" = 2)),
      actionButton("add_twin", "Ajouter un jumeau (via sibling)"),
      
      br(), br(),
      verbatimTextOutput("log")
    ),
    mainPanel(
      plotOutput("pedigree_plot", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  vals <- reactiveValues(
    ped = init_ped,
    twins = data.frame(id1=character(), id2=character(), code=integer()),
    log = ""
  )
  observeEvent(vals$ped, {
    updateSelectInput(session, "selected_id", choices = labels(vals$ped))
  })
  observeEvent(input$add_twin, {
    req(input$selected_id)
    ids_avant <- labels(vals$ped)
    sex_sibling <- as.numeric(input$sibling_sex)
    new_ped <- tryCatch({
      addSib(vals$ped, id = input$selected_id, sex = sex_sibling, side = "right")
    }, error = function(e) {
      vals$log <- paste("Erreur lors de l'ajout du sibling :", e$message)
      return(NULL)
    })
    if (is.null(new_ped)) return(NULL)
    ids_apres <- labels(new_ped)
    id_sibling <- setdiff(ids_apres, ids_avant)
    if (length(id_sibling) != 1) {
      vals$log <- "Erreur : Impossible de déterminer le nouvel ID sibling."
      return(NULL)
    }
    ids_jumeaux <- sort.default(c(input$selected_id, id_sibling))
    new_twins <- rbind(
      vals$twins,
      data.frame(id1 = ids_jumeaux[1], id2 = ids_jumeaux[2], code = as.numeric(input$twin_type))
    )
    vals$ped <- new_ped
    vals$twins <- new_twins
    vals$log <- sprintf(
      "Sibling ajouté : %s\nJumeaux créés entre : %s et %s\nType : %s",
      id_sibling, ids_jumeaux[1], ids_jumeaux[2],
      if (input$twin_type == 1) "Monozygote (MZ)" else "Dizygote (DZ)"
    )
  })
  output$pedigree_plot <- renderPlot({
    ped <- vals$ped
    twins <- vals$twins
    if (is.null(ped)) return()
    plot(ped, twins = twins, title = "Pedigree actuel", margins = 1)
  })
  output$log <- renderText({
    vals$log
  })
}


shinyApp(ui, server)
