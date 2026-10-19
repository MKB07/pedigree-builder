# Archived R development file
# Original path: apprentissage_test /fonction/coefissiant.R
# Original created: 2025-09-17 00:01:38
# Original modified: 2025-09-20 11:10:07
# Archive rationale: Prototype focused on kinship coefficients and relationship analysis.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# -------------------- LIBRAIRIES -----------------------
library(shiny)
library(bslib)
library(pedtools)
library(ribd) # inbreeding(), coeffTable(), kinship()
library(DT)
library(shinyWidgets) # switchInput


# ------------------ LISTE DES PEDIGREES -----------------
pedigree_list <- list(
  "Trio" = function() nuclearPed(1),
  "Siblings" = function() nuclearPed(2),
  "Sibship of 3" = function() nuclearPed(3, sex = c(1, 2, 1)),
  "Half-sibs, maternal" = function() halfSibPed(1, 1, type = "maternal"),
  "Half-sibs, paternal" = function() halfSibPed(1, 1),
  "Avuncular" = function() avuncularPed(),
  "Grandparent" = function() ancestralPed(2),
  "Great-grandparent" = function() ancestralPed(3),
  "1st cousins" = function() cousinPed(1, symmetric = TRUE),
  "1st cousins + child" = function() cousinPed(1, symmetric = TRUE, child = TRUE),
  "2nd cousins" = function() cousinPed(2, symmetric = TRUE),
  "2nd cousins + child" = function() cousinPed(2, symmetric = TRUE, child = TRUE)
)

# ------------------ UTILITAIRES -----------------
`%||%` <- function(x, y) if (is.null(x)) y else x
breakLabs <- function(x, breakAt = "  ") {
  labs <- labels(x)
  names(labs) <- gsub(breakAt, "\n", labs)
  labs
}
round3 <- function(x) ifelse(is.na(x), NA, formatC(x, digits = 3, format = "f"))
fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), NA,
    paste0(formatC(100 * x, digits = digits, format = "f"), "%")
  )
}

# ===================== UI ======================
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  br(),
  fluidRow(
    column(
      4,
      card(
        card_header("Sélection du pedigree"),
        selectInput(
          "pedChoice",
          label = "Choisissez un pedigree :",
          choices = c("--- Sélectionner ---" = "", names(pedigree_list)),
          selected = ""
        ),
        checkboxInput("includeSelf", "Inclure les paires (i,i)", value = FALSE),
        br(),
        # ---- SWITCHS d'overlay ----
        shinyWidgets::switchInput(
          inputId = "showF",
          label = "Afficher f sur le graphe",
          value = FALSE, onLabel = "ON", offLabel = "OFF", size = "small"
        ),
        shinyWidgets::switchInput(
          inputId = "showR",
          label = "Afficher %R (vs sélection)",
          value = FALSE, onLabel = "ON", offLabel = "OFF", size = "small"
        ),
        checkboxInput("hideR0", "Masquer R = 0%", value = TRUE),
        helpText("(%R = 2×φ, entre la personne sélectionnée et les autres)"),
        # ---- SWITCH Degré + options ----
        shinyWidgets::switchInput(
          inputId = "showDeg",
          label = "Afficher degré de parenté (vs sélection)",
          value = FALSE, onLabel = "ON", offLabel = "OFF", size = "small"
        ),
        checkboxInput("hideDegInf", "Masquer deg = Inf (non apparentés)", value = TRUE),
        helpText("Degré = degré de parenté au sens pedigree (pairs non apparentées : Inf)"),
        # ---- Options communes d'affichage ----
        radioButtons(
          "whichChrom", "Chromosome :", c("Autosomal" = "A", "X" = "X"),
          inline = TRUE, selected = "A"
        ),
        radioButtons(
          "fPos", "Position des étiquettes :",
          c(
            "Dessous" = "under", "Dessus" = "top", "Gauche" = "left",
            "Droite" = "right", "Centre" = "center"
          ),
          inline = TRUE, selected = "under"
        ),
        helpText("Astuce : cliquez sur un individu dans le graphe pour le sélectionner/désélectionner.")
      )
    ),
    column(
      8,
      card(
        card_header("Pedigree"),
        textOutput("selectedIndividual"),
        plotOutput("plot", height = "520px", click = "ped_click")
      )
    )
  ),
  br(),
  card(
    card_header("coeffTable — Autosomal + X"),
    navset_pill(
      nav_panel("Autosomal", DTOutput("coeffTableAuto")),
      nav_panel("Chromosome X", DTOutput("coeffTableX"))
    )
  ),
  br()
)

# =================== SERVER ===================
server <- function(input, output, session) {
  pedigree <- reactiveValues(ped = NULL)
  sel <- reactiveVal(character(0))

  observeEvent(input$pedChoice, {
    req(input$pedChoice != "")
    pedigree$ped <- relabel(pedigree_list[[input$pedChoice]](), new = "generations")
    sel(character(0))
  })

  degVectorFromCoeff <- function(base, ids, coeffDf) {
    if (is.null(coeffDf)) {
      return(rep(NA_real_, length(ids)))
    }
    nm <- names(coeffDf)
    col_id1 <- grep("^id1$", nm, ignore.case = TRUE, value = TRUE)[1]
    col_id2 <- grep("^id2$", nm, ignore.case = TRUE, value = TRUE)[1]
    col_deg <- grep("^deg$", nm, ignore.case = TRUE, value = TRUE)[1]
    if (any(is.na(c(col_id1, col_id2, col_deg)))) {
      return(rep(NA_real_, length(ids)))
    }
    i1 <- as.character(coeffDf[[col_id1]])
    i2 <- as.character(coeffDf[[col_id2]])
    d <- coeffDf[[col_deg]]
    v <- rep(NA_real_, length(ids))
    for (k in seq_along(ids)) {
      other <- ids[k]
      if (identical(other, base)) next
      hit <- d[(i1 == base & i2 == other) | (i1 == other & i2 == base)]
      v[k] <- if (length(hit)) hit[1] else NA_real_
    }
    v
  }

  plotAlignment <- reactive({
    req(pedigree$ped)
    .pedAlignment(pedigree$ped, arrows = FALSE, align = c(1.5, 2))
  })
  plotAnnotation <- reactive({
    req(pedigree$ped)
    .pedAnnotation(pedigree$ped,
      labs = breakLabs(pedigree$ped),
      col = list("#3c8dbc" = sel())
    )
  })
  plotScaling <- reactive({
    req(pedigree$ped)
    .pedScaling(plotAlignment(), plotAnnotation(),
      cex = 1.3, symbolsize = 1,
      margins = rep(3, 4)
    )
  })

  positionDf <- reactive({
    req(pedigree$ped)
    al <- plotAlignment()
    sc <- plotScaling()
    data.frame(
      id = labels(pedigree$ped)[al$plotord],
      x = al$xall,
      y = al$yall + sc$boxh / 2,
      stringsAsFactors = FALSE
    )
  })

  pos_map <- c(under = 1, top = 3, left = 2, right = 4)

  coeffTableBoth <- reactive({
    req(pedigree$ped)
    all_ids <- labels(pedigree$ped)
    coeffTable(
      x = pedigree$ped,
      ids = all_ids,
      coeff = c("f", "phi", "deg", "kappa"),
      self = isTRUE(input$includeSelf),
      Xchrom = NA
    )
  })

  autoTable <- reactive({
    df <- coeffTableBoth()
    req(df)
    keep <- !grepl("\\.X$", names(df))
    df[, keep, drop = FALSE]
  })
  xTable <- reactive({
    df <- coeffTableBoth()
    req(df)
    xcols <- grepl("\\.X$", names(df))
    if (!any(xcols)) {
      return(data.frame(Message = "Pas de colonnes X disponibles."))
    }
    xdf <- df[, c(1:2, which(xcols)), drop = FALSE]
    names(xdf) <- sub("\\.X$", "", names(xdf))
    xdf
  })

  output$plot <- renderPlot({
    req(pedigree$ped)
    al <- plotAlignment()
    sc <- plotScaling()
    drawPed(al, annotation = plotAnnotation(), scaling = sc)

    ids <- labels(pedigree$ped)
    chromX <- identical(input$whichChrom, "X")
    pos <- subset(positionDf(), id %in% ids)
    where <- input$fPos

    if (isTRUE(input$showF)) {
      fvals <- tryCatch(inbreeding(pedigree$ped, ids = ids, Xchrom = chromX),
        error = function(e) rep(NA_real_, length(ids))
      )
      labsF <- paste0("f=", round3(fvals))
      names(labsF) <- ids
      txtF <- labsF[pos$id]
      if (identical(where, "center")) {
        text(pos$x, pos$y, labels = txtF, cex = 0.95)
      } else {
        text(pos$x, pos$y,
          labels = txtF,
          pos = unname(pos_map[where]), xpd = NA, cex = 0.95, offset = 0.8
        )
      }
    }

    if (isTRUE(input$showDeg)) {
      base <- sel()[1]
      if (is.null(base) || is.na(base) || !nzchar(base)) {
        showNotification("Sélectionnez d’abord un individu pour afficher le degré.", type = "message")
      } else {
        dfdeg <- autoTable()
        ids_plot <- pos$id
        dvec <- tryCatch(degVectorFromCoeff(base, ids_plot, dfdeg),
          error = function(e) rep(NA_real_, length(ids_plot))
        )
        labsD <- ifelse(is.infinite(dvec), "deg=Inf",
          ifelse(is.na(dvec), NA, paste0("deg=", dvec))
        )
        labsD[ids_plot == base] <- NA
        if (isTRUE(input$hideDegInf)) labsD[labsD == "deg=Inf"] <- NA
        keep <- !is.na(labsD)
        if (any(keep)) {
          if (identical(where, "center")) {
            text(pos$x[keep], pos$y[keep], labels = labsD[keep], cex = 0.95)
          } else {
            text(pos$x[keep], pos$y[keep],
              labels = labsD[keep],
              pos = unname(pos_map[where]), xpd = NA, cex = 0.95, offset = 0.8
            )
          }
        }
      }
    }

    if (isTRUE(input$showR)) {
      base <- sel()[1]
      if (is.null(base) || is.na(base) || !nzchar(base)) {
        showNotification("Sélectionnez d’abord un individu pour afficher %R.", type = "message")
      } else {
        ids_all <- labels(pedigree$ped)
        chromX <- identical(input$whichChrom, "X")
        phi <- tryCatch(ribd::kinship(pedigree$ped, ids = unique(c(base, ids_all)), Xchrom = chromX),
          error = function(e) NULL
        )
        if (is.null(phi) || !(base %in% rownames(phi))) {
          showNotification("Impossible de calculer φ pour %R.", type = "error")
        } else {
          ids_plot <- pos$id
          phiVec <- phi[base, ids_plot, drop = TRUE]
          Rvec <- pmin(pmax(2 * phiVec, 0), 1)
          labsR <- paste0(
            "R=",
            ifelse(is.na(Rvec), NA, paste0(formatC(100 * Rvec, digits = 1, format = "f"), "%"))
          )
          labsR[ids_plot == base] <- NA
          if (isTRUE(input$hideR0)) {
            labsR[!is.na(labsR) & grepl("R=0(\\.0+)?%", labsR)] <- NA
          }
          keep <- !is.na(labsR)
          if (any(keep)) {
            if (identical(where, "center")) {
              text(pos$x[keep], pos$y[keep], labels = labsR[keep], cex = 0.95)
            } else {
              text(pos$x[keep], pos$y[keep],
                labels = labsR[keep],
                pos = unname(pos_map[where]), xpd = NA, cex = 0.95, offset = 0.8
              )
            }
          }
        }
      }
    }
  })

  observeEvent(input$ped_click, {
    req(pedigree$ped)
    pts <- positionDf()
    hit <- nearPoints(pts, input$ped_click,
      xvar = "x", yvar = "y",
      threshold = 20, maxpoints = 1
    )$id
    req(length(hit) == 1)
    if (identical(sel(), hit)) sel(character(0)) else sel(hit)
  })

  output$selectedIndividual <- renderText({
    req(pedigree$ped)
    who <- sel()
    if (length(who) == 0 || !nzchar(who)) {
      "Aucun individu sélectionné."
    } else {
      paste("Individu sélectionné :", who)
    }
  })

  output$coeffTableAuto <- renderDT({
    req(autoTable())
    datatable(autoTable(),
      options = list(scrollX = TRUE, dom = "tip", pageLength = 10),
      rownames = FALSE
    )
  })
  output$coeffTableX <- renderDT({
    req(xTable())
    datatable(xTable(),
      options = list(scrollX = TRUE, dom = "tip", pageLength = 10),
      rownames = FALSE
    )
  })
}

# ------------------ LANCEMENT APPLICATION ------------------
shinyApp(ui = ui, server = server)
