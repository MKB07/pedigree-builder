# Archived R development file
# Original path: version obsolette/select_person.R
# Original created: 2024-08-17 23:37:18
# Original modified: 2024-08-17 23:37:18
# Archive rationale: Early prototype for selecting and editing individuals in a pedigree.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# Chargement des bibliothèques nécessaires
suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(rhandsontable)
  library(shinyalert)
  library(data.table)
  library(pedtools)
  library(stringr)
  library(kinship2)
})

# Définition des fonctions utilitaires supplémentaires
stop2 = function(...) {
  a = lapply(list(...), toString)
  a = append(a, list(call. = FALSE))
  do.call(stop, a)
}

`%||%` = function(x,y) if(is.null(x)) y else x

mylink = function(text, href, .noWS = "outside", ...) {
  if(missing(href)) href = text
  shiny::a(text, href = href, .noWS = .noWS, target = "_blank", ...)
}

.myintersect = function(x, y) y[match(x, y, 0L)]

.mysetdiff = function(x, y) unique.default(x[match(x, y, 0L) == 0L])

checknum = function(a, var, min = 0, max = Inf) {
  if(is.na(a) || a < min || a > max)
    stop2(sprintf("Invalid value of '%s'", var))
  a
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

addPar = function(x, ids) {
  n = length(ids)
  pars = ids[-1]
  parsex = getSex(x, pars)
  fa = mo = NULL
  if(n == 3) {
    if(parsex[1] == 1 && parsex[2] == 2)      {fa = pars[1]; mo = pars[2]}
    else if(parsex[1] == 2 && parsex[2] == 1) {fa = pars[2]; mo = pars[1]}
    else
      stop2("Incompatible sex of selected parents: ", ids[2:3])
  }
  else if(n == 2) {
    if(parsex[1] == 1) fa = ids[2]
    else if(parsex[1] == 2) mo = ids[2]
    else
      stop2("Cannot use individuals of unknown sex as parent: ", ids[2])
  }
  else if(n != 1)
    stop2("Too many individuals selected")
  
  addParents(x, ids[1], father = fa, mother = mo, verbose = F)
}

removeSel = function(dat, ids, updown) {
  newped = removeIndividuals(dat$ped, ids, remove = updown, verbose = FALSE)
  
  if(is.null(newped))
    stop2(sprintf("Removing %s would leave an empty pedigree",
                  ifelse(length(ids) == 1, sprintf("'%s'", ids), "these individuals")))
  
  if(is.pedList(newped))
    stop2(sprintf("Removing %s would disconnect the pedigree, which is currently not supported",
                  ifelse(length(ids) == 1, sprintf("'%s'", ids), "these individuals")))
  
  newlabs = labels(newped)
  
  newtw = dat$twins
  newtw = newtw[newtw$id1 %in% newlabs & newtw$id2 %in% newlabs, , drop = FALSE]
  
  sty = c("hatched", "carrier", "dashed", "deceased") |> setNames(nm = _)
  newstyles = lapply(sty, function(s) .myintersect(dat[[s]], newlabs))
  
  fill = dat$fill
  newstyles$fill = fill[.myintersect(names(fill), newlabs)]
  
  newText = lapply(dat$textAnnot, function(v) v[.myintersect(names(v), newlabs)])
  newText = newText[lengths(newText) > 0]
  
  newdat = c(list(ped = newped, twins = newtw), newstyles, list(textAnnot = newText))
  
  newdat
}

sortIds = function(x, ids) {
  intern = internalID(x, ids)
  ids[order(intern)]
}

modifyVec = function(x, y, val = NULL) {
  if(!length(y))
    return(x)
  
  if(is.null(names(x)))
    length(x) = 0
  
  if(!is.null(val))
    y = rep(val, length(y)) |> setNames(y)
  
  res = c(x, y)
  res[!duplicated.default(names(res), fromLast = TRUE)]
}

changeSex = function(ped, ids, sex, twins = NULL) {
  if(sex == 0) {
    if(!all(ids %in% leaves(ped)))
      stop2("Only individuals without children can have unknown sex")
    newped = setSex(ped, ids, sex = 0)
    return(newped)
  }
  
  currentSex = getSex(ped, ids)
  
  newped = ped |>
    swapSex(ids[currentSex == (3-sex)], verbose = FALSE) |>
    setSex(ids[currentSex == 0], sex = sex)
  
  mz = twins[twins$code == 1, , drop = FALSE]
  if(!is.null(mz) && nrow(mz) > 0) {
    sx1 = getSex(newped, mz$id1)
    sx2 = getSex(newped, mz$id2)
    if(any(sx1 > 0 & sx2 > 0 & sx1 != sx2))
      stop2("Cannot change sex of one MZ twin")
  }
  
  newped
}

plotSegregation <- function(ped, affected = NULL, fill = NULL, unknown = NULL, proband = NULL, carriers = NULL, homozygous = NULL, noncarriers = NULL, labs = NULL, margins = c(2, 2, 2, 2)) {
  plot(ped, affected = affected, col.fill = fill, unknown = unknown, proband = proband, carrier = carriers, homozygous = homozygous, noncarrier = noncarriers, labs = labs, margin = margins)
}

w_pedCases <- function(id) {
  pickerInput(
    inputId = NS(id, "pedCases"),
    label = NULL,
    choices = c("Trio", "Full siblings", "Grandparent", "Great-grandparent", "Half siblings (mat)", "Half siblings (pat)","Avuncular"),
    multiple = TRUE,
    options = list(
      max_options = 1,
      none_selected_text = "Add basic pedigree",
      style = "action-button bttn bttn-jelly bttn-sm bttn-default bttn-no-outline shiny-bound-input"
    )
  )
}

pedigreeBoxUI <- function(id) {
  div(rHandsontableOutput(NS(id, "pedTable")), style = "margin-top: 1rem;")
}

pedigreeBoxServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
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
      
      
      values[["pedLoaded"]] <- as.data.table(pedLoaded)
      updatePickerInput(
        session = getDefaultReactiveDomain(),
        inputId = "pedCases",
        selected = ""
      )
    })
    
    output$pedToAdd <- renderPlot({
      req(values[["pedToAdd"]])
      par(family = "helvetica")
      
      all_phenotypes <- levels(droplevels(values[["pedToAdd"]][["phenotype"]]))
      phenotypes <- setdiff(all_phenotypes, c("", "nonaff"))
      phenoVector <- phenotypes
      phenoTotal <- length(phenotypes)
      
      affected <- !values[["pedToAdd"]][["phenotype"]] %in% c("", "nonaff")
      unknown <- values[["pedToAdd"]][["phenotype"]] == ""
      proband <- values[["pedToAdd"]][["proband"]] == 1
      carriers <- values[["pedToAdd"]][["carrier"]] == "het"
      homozygous <- values[["pedToAdd"]][["carrier"]] == "hom"
      noncarriers <- values[["pedToAdd"]][["carrier"]] == "neg"
      
      pal <- c("white", rep(c("#0072B2", "#D55E00", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#CC79A7"), ceiling(phenoTotal / 8)))
      names(pal) <- c("nonaff", phenoVector)
      fillcols <- pal[as.character(values[["pedToAdd"]][["phenotype"]])]
      
      idxs <- which(values[["pedToAdd"]][["ped"]] == values[["pedToAddCurrent"]] + values[["pedTotal"]])
      plotSegregation(
        as.ped(values[["pedToAdd"]][idxs, c("id", "fid", "mid", "sex")]),
        affected = NULL,
        fill = unname(fillcols[idxs]),
        unknown = which(unknown[idxs]),
        proband = which(proband[idxs]),
        if (length(which(carriers[idxs]) > 0)) carriers = which(carriers[idxs]),
        if (length(which(homozygous[idxs]) > 0)) homozygous = which(homozygous[idxs]),
        if (length(which(noncarriers[idxs]) > 0)) noncarriers = which(noncarriers[idxs]),
        labs = setNames(seq_along(idxs), paste(values[["pedToAdd"]][["firstname"]][idxs], values[["pedToAdd"]][["lastname"]][idxs], values[["pedToAdd"]][["age"]][idxs], sep = " ")),
        margins = c(2 + 3, 2, 2, 2)
      )
      legend(
        "bottomright",
        inset = c(-0.1, -0.15),
        legend = c("nonaff", phenoVector),
        fill = pal,
        ncol = phenoTotal + 1,
        bty = "n",
        cex = 1.2
      )
    }, execOnResize = TRUE)
    
    observeEvent(input$loadPed, {
      pedLoaded <- suppressWarnings(
        tryCatch(
          {
            fullData <- fread(input$loadPed$datapath, header = TRUE, na.strings = c(NA, "", "."))
            fullData[is.na(fullData)] <- ""
            if (exists("ped", fullData) & !any(is.na(fullData[["ped"]]))) {
              fullData$ped <- as.integer(factor(fullData$ped, levels = unique(fullData$ped)))
              pedList <- split(fullData[, c("id", "fid", "mid", "sex")], fullData[["ped"]])
            } else {
              pedList <- list(fullData[, c("id", "fid", "mid", "sex")])
            }
            pedList <- lapply(pedList, as.ped)
            if (!is.pedList(pedList)) {
              stop
            }
            colSelect <- intersect(c("ped", "id", "fid", "mid", "sex", "phenotype", "carrier", "proband", "age", "firstname", "lastname"), colnames(fullData))
            pedLoaded <- fullData[, ..colSelect]
            pedLoaded <- setDT(pedLoaded)
          },
          error = function(err) NULL
        )
      )
      if (is.null(pedLoaded)) {
        showNotification(
          HTML("<i class='fas fa-triangle-exclamation'></i> Invalid pedigree file."),
          type = "error",
          duration = 3
        )
      }
      
      values[["pedLoaded"]] <- pedLoaded
    })
    
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, values[["pedLoaded"]], {
      pedToAdd <- copy(values[["pedLoaded"]])
      
      within(pedToAdd, {
        if (!exists("ped", pedToAdd)) {
          pedToAdd[, ped := as.integer(values[["pedTotal"]] + 1)]
        } else {
          pedToAdd[, ped := as.integer(values[["pedTotal"]] + ped)]
        }
        if (!exists("phenotype")) {
          pedToAdd[, phenotype := factor("nonaff", levels = unique(c("", "nonaff", "aff", values[["phenoVector"]])))]
        } else {
          pedToAdd[, phenotype := factor(phenotype, levels = unique(c("", "nonaff", "aff", values[["phenoVector"]], phenotype)))]
        }
        if (!exists("carrier")) {
          pedToAdd[, carrier := factor("", levels = c("", "neg", "het", "hom"))]
        } else {
          pedToAdd[, carrier := factor(carrier, levels = c("", "neg", "het", "hom"))]
        }
        if (!exists("proband")) {
          pedToAdd[, proband := FALSE]
        } else {
          sapply(1:max(pedToAdd$ped), function(pedid) {
            idxs <- which(pedToAdd$ped == pedid)
            if (any(!proband[idxs] %in% c(NA, "", "0", "1")) || sum(as.numeric(proband[idxs]), na.rm = TRUE) > 1) {
              pedToAdd[idxs, proband := FALSE]
            } else {
              pedToAdd[idxs, proband := proband %in% 1]
            }
          })
          pedToAdd[, proband := as.logical(proband)]
        }
        if (!exists("age")) {
          pedToAdd[, age := as.integer(50)]
        } else {
          pedToAdd[!age %in% 1:100, age := NA_integer_]
          pedToAdd[, age := as.integer(age)]
        }
        if (!exists("firstname")) {
          pedToAdd[, firstname := "Unknown"]
        }
        if (!exists("lastname")) {
          pedToAdd[, lastname := "Unknown"]
        }
      })
      setcolorder(pedToAdd, c("ped", "id", "fid", "mid", "sex", "phenotype", "carrier", "proband", "age", "firstname", "lastname"))
      
      values[["pedToAdd"]] <- pedToAdd
      values[["pedToAddCurrent"]] <- 1
      values[["pedLoaded"]] <- NULL
    })
    
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, values[["pedToAdd"]], {
      shinyalert(
        html = TRUE,
        text = tagList(
          div(
            "The following pedigree(s) will be added:",
            plotOutput(outputId = NS(id, "pedToAdd"), height = "300px", width = "85%"),
            align = "center"
          )
        ),
        animation = "slide-from-bottom",
        confirmButtonCol = "#39A0ED",
        showCancelButton = TRUE,
        size = "s",
        callbackR = function(x) {
          if (x) {
            values[["pedData"]] <- rbind(values[["pedData"]], values[["pedToAdd"]])
            values[["pedTotal"]] <- max(values[["pedData"]][["ped"]])
            values[["lastProband"]] <- values[["pedData"]][["proband"]]
            values[["pedCurrent"]] <- values[["pedTotal"]]
          }
          values[["pedToAdd"]] <- NULL
          pedToAdd$suspend()
        }
      )
      if (length(unique(values[["pedToAdd"]][["ped"]])) > 1) {
        pedToAdd$resume()
      }
    })
    
    pedToAdd <- observe(suspended = TRUE, {
      invalidateLater(2000)
      isolate({
        if (values[["pedToAddCurrent"]] < length(unique(values[["pedToAdd"]][["ped"]]))) {
          values[["pedToAddCurrent"]] <- values[["pedToAddCurrent"]] + 1
        } else {
          values[["pedToAddCurrent"]] <- 1
        }
      })
    })
    
    observeEvent(input$rmvPed, {
      req(values[["pedTotal"]] > 0)
      shinyalert(
        html = TRUE,
        text = tagList(
          div(
            "The family currently displayed will be removed. (Note that this will delete all of its associated data)",
            align = "center"
          )
        ),
        animation = "slide-from-bottom",
        confirmButtonCol = "#39A0ED",
        showCancelButton = TRUE,
        size = "s",
        callbackR = function(x) {
          if (x) {
            pedData <- values[["pedData"]][ped != values[["pedCurrent"]], ]
            if (nrow(pedData) > 0) {
              pedData[["ped"]] <- as.integer(as.factor(pedData[["ped"]]))
            } else {
              pedData <- NULL
            }
            values[["pedData"]] <- pedData
            values[["pedCurrent"]] <- values[["pedTotal"]] - 1
            values[["pedTotal"]] <- values[["pedTotal"]] - 1
            if (values[["pedTotal"]] == 0) values[["phenoTotal"]] <- 0
          }
        }
      )
    })
    
    values[["pedNames"]] <- c("ped", "id", "fid", "mid", "sex", "phenotype", "carrier", "proband", "age", "firstname", "lastname")
    output$pedTable <- renderRHandsontable({
      req(values[["pedData"]])
      rhandsontable(
        values[["pedData"]],
        useTypes = TRUE,
        manualColumnResize = TRUE,
        rowHeaders = NULL,
        height = if (nrow(values[["pedData"]]) > 6) 175 else NULL,
        colHeaders = values[["pedNames"]],
        overflow = "visible",
        selectCallback = TRUE
      ) %>%
        hot_validate_numeric(col = 9, min = 1, max = 100, allowInvalid = FALSE) %>%
        hot_col(c("ped", "id", "fid", "mid", "sex"), readOnly = TRUE) %>%
        hot_col(6, type = "autocomplete", strict = FALSE, colWidths = "110px") %>%
        hot_col(7, colWidths = "75px") %>%
        hot_col(8, colWidths = "90px", halign = "htCenter") %>%
        hot_col(10, colWidths = "110px") %>%
        hot_col(11, colWidths = "110px")
    })
    
    observeEvent(input$pedTable$changes$changes, {
      before <- input$pedTable$changes$changes[[1]][[3]]
      after <- input$pedTable$changes$changes[[1]][[4]]
      if (is.null(before) || before != after) {
        temp <- hot_to_r(input$pedTable)
        
        if (input$pedTable$changes$changes[[1]][[2]] == 7) {
          lapply(1:values[["pedTotal"]], function(pedid) {
            idxs <- which(temp[["ped"]] == pedid)
            currentproband <- temp[["proband"]][idxs]
            if (!is.null(values[["lastProband"]][idxs]) & sum(currentproband) > 1) {
              newproband <- values[["lastProband"]][idxs] != currentproband
            } else {
              newproband <- currentproband
            }
            values[["lastProband"]][idxs] <- newproband
            temp[idxs, proband := newproband]
          })
        }
        
        values[["pedData"]] <- temp
      }
    })
    
    observeEvent(ignoreNULL = FALSE, values[["pedData"]], {
      if (!is.null(values[["pedData"]])) {
        all_phenotypes <- levels(droplevels(values[["pedData"]][["phenotype"]]))
        phenotypes <- setdiff(all_phenotypes, c("", "nonaff"))
        values[["phenoVector"]] <- unique(c(phenotypes, values[["extraPheno"]]))
        values[["phenoTotal"]] <- length(values[["phenoVector"]])
        
        values[["affected"]] <- !values[["pedData"]][["phenotype"]] %in% c("", "nonaff")
        values[["unknown"]] <- values[["pedData"]][["phenotype"]] == ""
        values[["proband"]] <- values[["pedData"]][["proband"]] == 1
        values[["carriers"]] <- values[["pedData"]][["carrier"]] == "het"
        values[["homozygous"]] <- values[["pedData"]][["carrier"]] == "hom"
        values[["noncarriers"]] <- values[["pedData"]][["carrier"]] == "neg"
        
        lclasses <- copy(values[["pedData"]][, c("sex", "phenotype", "age")])
        lclasses[, sex := factor(sex, levels = 1:2, labels = c("male", "female"))]
        values[["lclasses"]] <- lclasses
        values[["okAge"]] <- !any(is.na(lclasses[["age"]]))
      } else {
        values[["extraPheno"]] <- NULL
        values[["phenoVector"]] <- NULL
        values[["phenoTotal"]] <- 0
        values[["pedTotal"]] <- 0
      }
    })
  })
}

# Interface utilisateur (UI)
ui <- fluidPage(
  titlePanel("Pedigree Viewer"),
  sidebarLayout(
    sidebarPanel(
      w_pedCases("pedModule"),
      actionButton("rmvPed", "Supprimer l'arbre actuel", class = "btn btn-danger"),
      actionButton("addPed", "Ajouter l'arbre actuel", class = "btn btn-primary"),
      selectInput("pedigreeType", "Sélectionnez un type de pedigree :", choices = c(
        "Trio" = "nuclearPed(1)",
        "Full siblings" = "nuclearPed(2, sex = 1:2)",
        "Grandparent" = "ancestralPed(g = 2)",
        "Great-grandparent" = "ancestralPed(g = 3)",
        "Half siblings (mat)" = "halfSibPed(1, 1, sex2 = 2, type = 'maternal')",
        "Half siblings (pat)" = "halfSibPed(1, 1, sex2 = 2, type = 'paternal')",
        "Avuncular" = "avuncularPed()"
      )),
      helpText("Cliquez sur un individu pour voir ses détails."),
      uiOutput("modifyButtons")
    ),
    mainPanel(
      plotOutput("pedigreePlot", click = "plot_click"),
      uiOutput("detailsPanel"),
      pedigreeBoxUI("pedModule"),
      plotOutput("mainPlot", height = "600px")
    )
  )
)

# Fonction pour trouver l'individu le plus proche du clic
nearestIndividual <- function(click_data, ped) {
  coords <- locator2(ped)
  dist <- sqrt((coords$x - click_data$x)^2 + (coords$y - click_data$y)^2)
  closest <- which.min(dist)
  ids <- coords$id
  ids[closest]
}

# Fonction pour obtenir les coordonnées des individus dans le plot
locator2 <- function(ped) {
  plot_data <- plot(ped, id = TRUE, plot = FALSE)
  usr <- par("usr")
  plt <- par("plt")
  
  coords <- data.frame(
    x = plot_data$x,
    y = plot_data$y,
    id = labels(ped)
  )
  
  coords
}

# Serveur
server <- function(input, output, session) {
  values <- reactiveValues(pedData = NULL, pedTotal = 0, pedToAdd = NULL, pedToAddCurrent = 1)
  selected_ind <- reactiveVal(NULL)
  pedigree <- reactiveVal(nuclearPed(1))
  individual_names <- reactiveVal(list())
  
  observeEvent(input$pedigreeType, {
    pedigree(eval(parse(text = input$pedigreeType)))
    selected_ind(NULL)
    individual_names(list())
  })
  
  renderPedigreePlot <- function() {
    ped_copy <- pedigree()
    col <- rep("black", length(labels(ped_copy)))  # Couleur par défaut pour tous les individus
    
    if (!is.null(selected_ind())) {
      col[which(labels(ped_copy) == selected_ind())] <- "#71C7E0"  # Appliquer la couleur à l'individu sélectionné
    }
    
    plot(ped_copy, col = col, id = TRUE, lab = sapply(labels(ped_copy), function(id) {
      names <- individual_names()
      if (is.null(names[[id]])) {
        return(id)
      } else {
        return(paste0(names[[id]]$firstname, " ", names[[id]]$lastname))
      }
    }))
  }
  
  output$pedigreePlot <- renderPlot({
    renderPedigreePlot()
  })
  
  observeEvent(input$plot_click, {
    click_data <- input$plot_click
    selected <- nearestIndividual(click_data, pedigree())
    
    if (!is.null(selected_ind()) && selected == selected_ind()) {
      selected_ind(NULL)  # Désélectionne l'individu
    } else {
      selected_ind(selected)  # Sélectionne le nouvel individu
    }
  })
  
  output$detailsPanel <- renderUI({
    if (!is.null(selected_ind())) {
      ind <- selected_ind()
      names <- individual_names()[[ind]] %||% list(firstname = "", lastname = "")
      tagList(
        h3("Détails de l'individu sélectionné"),
        p(paste("ID :", ind)),
        textInput("firstname", "Prénom", value = names$firstname),
        textInput("lastname", "Nom de famille", value = names$lastname),
        actionButton("saveName", "Enregistrer le nom"),
        actionButton("addson", "Ajouter un fils"),
        actionButton("adddaughter", "Ajouter une fille"),
        actionButton("addsibRight", "Ajouter un frère/soeur (droite)"),
        actionButton("addsibLeft", "Ajouter un frère/soeur (gauche)"),
        actionButton("addparents", "Ajouter des parents"),
        actionButton("sex1", "Changer le sexe en homme"),
        actionButton("sex2", "Changer le sexe en femme"),
        actionButton("sex0", "Changer le sexe en inconnu"),
        actionButton("removeDown", "Supprimer descendants"),
        actionButton("removeUp", "Supprimer ancêtres")
      )
    } else {
      h3("Cliquez sur un individu pour voir ses détails.")
    }
  })
  
  observeEvent(input$saveName, {
    id <- req(selected_ind())
    names <- individual_names()
    names[[id]] <- list(firstname = input$firstname, lastname = input$lastname)
    individual_names(names)
    renderPedigreePlot()
  })
  
  observeEvent(input$addson, {
    id <- req(selected_ind())
    tryCatch({
      pedigree(addSon(pedigree(), id, verbose = FALSE))
      renderPedigreePlot()
    }, error = function(e) { showNotification(paste("Erreur :", e$message), type = "error") })
  })
  
  observeEvent(input$adddaughter, {
    id <- req(selected_ind())
    tryCatch({
      pedigree(addDaughter(pedigree(), id, verbose = FALSE))
      renderPedigreePlot()
    }, error = function(e) { showNotification(paste("Erreur :", e$message), type = "error") })
  })
  
  observeEvent(input$addsibRight, {
    id <- req(selected_ind())
    tryCatch({
      pedigree(addSib(pedigree(), id, side = "right"))
      renderPedigreePlot()
    }, error = function(e) { showNotification(paste("Erreur :", e$message), type = "error") })
  })
  
  observeEvent(input$addsibLeft, {
    id <- req(selected_ind())
    tryCatch({
      pedigree(addSib(pedigree(), id, side = "left"))
      renderPedigreePlot()
    }, error = function(e) { showNotification(paste("Erreur :", e$message), type = "error") })
  })
  
  observeEvent(input$addparents, {
    ids <- req(selected_ind()) # If multiple, the first is interpreted as child, followed by parents
    tryCatch({
      pedigree(addPar(pedigree(), ids))
      renderPedigreePlot()
    }, error = function(e) { showNotification(paste("Erreur :", e$message), type = "error") })
  })
  
  observeEvent(input$sex1, {
    id <- req(selected_ind())
    tryCatch({
      pedigree(changeSex(pedigree(), id, sex = 1))
      renderPedigreePlot()
    }, error = function(e) { showNotification(paste("Erreur :", e$message), type = "error") })
  })
  
  observeEvent(input$sex2, {
    id <- req(selected_ind())
    tryCatch({
      pedigree(changeSex(pedigree(), id, sex = 2))
      renderPedigreePlot()
    }, error = function(e) { showNotification(paste("Erreur :", e$message), type = "error") })
  })
  
  observeEvent(input$sex0, {
    id <- req(selected_ind())
    tryCatch({
      pedigree(changeSex(pedigree(), id, sex = 0))
      renderPedigreePlot()
    }, error = function(e) { showNotification(paste("Erreur :", e$message), type = "error") })
  })
  
  observeEvent(input$removeDown, {
    id <- req(selected_ind())
    tryCatch({
      new_ped <- removeIndividuals(pedigree(), id, direction = "descendants")
      pedigree(new_ped)
      renderPedigreePlot()
    }, error = function(e) { showNotification(paste("Erreur :", e$message), type = "error") })
  })
  
  observeEvent(input$removeUp, {
    id <- req(selected_ind())
    tryCatch({
      new_ped <- removeIndividuals(pedigree(), id, direction = "ancestors")
      pedigree(new_ped)
      renderPedigreePlot()
    }, error = function(e) { showNotification(paste("Erreur :", e$message), type = "error") })
  })
  
  pedigreeBoxServer("pedModule", values)
  
  observeEvent(input$addPed, {
    showModal(modalDialog(
      title = "Ajouter un arbre généalogique",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirmAdd", "Ajouter", class = "btn btn-primary")
      ),
      plotOutput("pedToAdd", height = "300px")
    ))
  })
  
  observeEvent(input$confirmAdd, {
    values$pedData <- rbind(values$pedData, values$pedToAdd)
    values$pedTotal <- max(values$pedData$ped)
    removeModal()
  })
  
  observeEvent(input$rmvPed, {
    req(values$pedTotal > 0)
    shinyalert(
      title = "Confirmation",
      text = "L'arbre actuellement affiché sera supprimé. (Cela supprimera toutes ses données associées)",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Oui, supprimer",
      cancelButtonText = "Annuler",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      callbackR = function(confirm) {
        if (confirm) {
          pedData <- values$pedData[ped != values$pedCurrent, ]
          if (nrow(pedData) > 0) {
            pedData$ped <- as.integer(as.factor(pedData$ped))
          } else {
            pedData <- NULL
          }
          values$pedData <- pedData
          values$pedCurrent <- values$pedTotal - 1
          values$pedTotal <- values$pedTotal - 1
          if (values$pedTotal == 0) values$phenoTotal <- 0
        }
      }
    )
  })
  
  output$mainPlot <- renderPlot({
    req(values$pedData)
    par(family = "helvetica")
    
    all_phenotypes <- levels(droplevels(values$pedData[["phenotype"]]))
    phenotypes <- setdiff(all_phenotypes, c("", "nonaff"))
    phenoVector <- phenotypes
    phenoTotal <- length(phenotypes)
    
    affected <- !values$pedData[["phenotype"]] %in% c("", "nonaff")
    unknown <- values$pedData[["phenotype"]] == ""
    proband <- values$pedData[["proband"]] == 1
    carriers <- values$pedData[["carrier"]] == "het"
    homozygous <- values$pedData[["carrier"]] == "hom"
    noncarriers <- values$pedData[["carrier"]] == "neg"
    
    pal <- c("white", rep(c("#0072B2", "#D55E00", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#CC79A7"), ceiling(phenoTotal / 8)))
    names(pal) <- c("nonaff", phenoVector)
    fillcols <- pal[as.character(values$pedData[["phenotype"]])]
    
    idxs <- which(values$pedData[["ped"]] == values$pedCurrent)
    plotSegregation(
      as.ped(values$pedData[idxs, c("id", "fid", "mid", "sex")]),
      affected = NULL,
      fill = unname(fillcols[idxs]),
      unknown = which(unknown[idxs]),
      proband = which(proband[idxs]),
      if (length(which(carriers[idxs]) > 0)) carriers = which(carriers[idxs]),
      if (length(which(homozygous[idxs]) > 0)) homozygous = which(homozygous[idxs]),
      if (length(which(noncarriers[idxs]) > 0)) noncarriers = which(noncarriers[idxs]),
      labs = setNames(seq_along(idxs), paste(values$pedData[["firstname"]][idxs], values$pedData[["lastname"]][idxs], values$pedData[["age"]][idxs], sep = " ")),
      margins = c(2 + 3, 2, 2, 2)
    )
    legend(
      "bottomright",
      inset = c(-0.1, -0.15),
      legend = c("nonaff", phenoVector),
      fill = pal,
      ncol = phenoTotal + 1,
      bty = "n",
      cex = 1.2
    )
  }, execOnResize = TRUE)
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)
