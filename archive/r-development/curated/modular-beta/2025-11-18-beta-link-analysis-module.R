# Archived R development file
# Original path: 🧩 Versions_Support/Support de test/Version beta TEST/R/Mod_link_analysis.R
# Original created: 2025-11-18 04:24:27
# Original modified: 2025-11-18 04:24:39
# Archive rationale: Relationship and link analysis module from the beta modular experiment.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# =========================
# MODULE: Link Analysis (Relationship Analysis)
# =========================
# Analyse des relations génétiques entre deux individus du pedigree
# Utilise: ribd (kinship, IBD), verbalisr (description textuelle)

library(shiny)
library(ribd)
library(verbalisr)
library(pedtools)

# ====================== HELPER FUNCTIONS ======================

#' Formater un nombre pour l'affichage (4 décimales)
#' @param x Nombre à formater
#' @return Chaîne formatée ou NA
fmt_num <- function(x) {
  ifelse(is.na(x), NA_character_, format(round(x, 4), nsmall = 4))
}

#' Calculer les métriques de relation pour une paire d'individus
#' @param ped Objet pedigree (pedtools)
#' @param id Premier individu
#' @param ref Second individu (référence)
#' @return Liste avec relation, f, phi, delta
pair_metrics <- function(ped, id, ref) {
  if (!isTruthy(ref) || is.null(ped)) {
    return(list(
      relation = NA_character_,
      f = NA_real_,
      phi = NA_real_,
      delta = NA_character_
    ))
  }

  # 1) Description textuelle de la relation
  rel_txt <- tryCatch(
    pedtools::relation(ped, from = id, to = ref),
    error = function(e) NA_character_
  )

  if (is.na(rel_txt)) {
    # Fallback avec verbalisr
    rel_txt <- tryCatch(
      {
        txt <- format(verbalisr::verbalise(ped, c(id, ref)))
        gsub("([[:graph:]])  ([[:graph:]])", "\\1 \\2", txt) # nettoie doubles espaces
      },
      error = function(e) NA_character_
    )
  }

  # 2) Consanguinité de l'individu
  fi <- tryCatch(
    ribd::inbreeding(ped, id),
    error = function(e) NA_real_
  )

  # 3) Kinship φ(id, ref)
  phi <- if (identical(id, ref)) {
    # Auto-kinship: φ(i,i) = (1 + f_i) / 2
    if (is.na(fi)) NA_real_ else (1 + fi) / 2
  } else {
    tryCatch(
      ribd::kinship(ped, c(id, ref)),
      error = function(e) NA_real_
    )
  }

  # 4) Coefficients IBD: κ si non-consanguins, Δ sinon
  inb_pair <- tryCatch(
    ribd::inbreeding(ped, c(id, ref)),
    error = function(e) c(NA_real_, NA_real_)
  )

  if (!any(is.na(inb_pair)) && all(inb_pair == 0) && !identical(id, ref)) {
    # Pas de consanguinité → utiliser kappa
    kap <- tryCatch(
      ribd::kappaIBD(ped, ids = c(id, ref)),
      error = function(e) c(NA, NA, NA)
    )
    delta_str <- if (any(is.na(kap))) {
      NA_character_
    } else {
      paste0("κ=(", paste(fmt_num(kap), collapse = ", "), ")")
    }
  } else if (!identical(id, ref)) {
    # Consanguinité présente → utiliser Δ1..Δ9
    del <- tryCatch(
      ribd::condensedIdentity(ped, c(id, ref)),
      error = function(e) rep(NA_real_, 9)
    )
    delta_str <- if (all(is.na(del))) {
      NA_character_
    } else {
      paste0("Δ=(", paste(fmt_num(as.numeric(del)), collapse = ", "), ")")
    }
  } else {
    # Même individu
    delta_str <- "-"
  }

  list(
    relation = rel_txt,
    f = fi,
    phi = phi,
    delta = delta_str
  )
}

#' Construire une matrice de relations pour toutes les paires du pedigree
#' @param ped Objet pedigree
#' @return data.frame avec toutes les paires et leurs métriques
build_relationship_matrix <- function(ped) {
  if (is.null(ped)) {
    return(NULL)
  }

  ids <- labels(ped)
  n <- length(ids)
  results <- list()
  idx <- 1

  for (i in 1:n) {
    for (j in i:n) {
      id1 <- ids[i]
      id2 <- ids[j]

      # Relation textuelle
      if (id1 == id2) {
        rel_txt <- "Self"
      } else {
        rel_txt <- tryCatch(
          pedtools::relation(ped, from = id1, to = id2),
          error = function(e) NA_character_
        )
        if (is.na(rel_txt)) {
          rel_txt <- tryCatch(
            {
              txt <- format(verbalisr::verbalise(ped, c(id1, id2)))
              gsub("([[:graph:]])  ([[:graph:]])", "\\1 \\2", txt)
            },
            error = function(e) NA_character_
          )
        }
      }

      # Kinship phi
      phi <- if (id1 == id2) {
        f_i <- tryCatch(ribd::inbreeding(ped, id1), error = function(e) NA_real_)
        if (is.na(f_i)) NA_real_ else (1 + f_i) / 2
      } else {
        tryCatch(ribd::kinship(ped, c(id1, id2)), error = function(e) NA_real_)
      }

      # Degré
      deg <- tryCatch(
        ribd::kin2deg(phi, unrelated = NA),
        error = function(e) NA_real_
      )

      results[[idx]] <- data.frame(
        id1 = id1,
        id2 = id2,
        phi = round(phi, 4),
        deg = round(deg, 4),
        relationships = if (is.na(rel_txt)) "" else as.character(rel_txt),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }

  do.call(rbind, results)
}

# ====================== MODULE UI ======================

linkAnalysisUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .link-section {
        background: #ffffff;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
      .link-title {
        font-size: 18px;
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 15px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      .metric-box {
        background: #f8f9fa;
        border-left: 4px solid #3498db;
        padding: 12px;
        margin: 10px 0;
        border-radius: 4px;
      }
      .metric-label {
        font-weight: 600;
        color: #555;
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .metric-value {
        font-size: 16px;
        color: #2c3e50;
        margin-top: 5px;
        font-family: 'Courier New', monospace;
      }
      .selector-box {
        background: #ecf0f1;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 15px;
      }
      .compute-btn {
        width: 100%;
        margin: 15px 0;
        padding: 12px;
        font-size: 16px;
        font-weight: 600;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border: none;
        color: white;
        border-radius: 8px;
        transition: all 0.3s ease;
      }
      .compute-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
      }
      .kappa-table {
        width: 100%;
        border-collapse: collapse;
        margin: 10px 0;
      }
      .kappa-table th {
        background: #3498db;
        color: white;
        padding: 10px;
        text-align: left;
        font-weight: 600;
      }
      .kappa-table td {
        padding: 8px 10px;
        border-bottom: 1px solid #ecf0f1;
      }
      .kappa-table tr:hover {
        background: #f8f9fa;
      }
    ")),
    div(
      class = "link-section",
      div(
        class = "link-title",
        icon("link"),
        "Analyse du lien génétique"
      ),

      # Sélecteurs A et B
      div(
        class = "selector-box",
        fluidRow(
          column(
            6,
            selectInput(ns("idA"),
              "👤 Individu A",
              choices = NULL,
              width = "100%"
            )
          ),
          column(
            6,
            selectInput(ns("idB"),
              "👤 Individu B",
              choices = NULL,
              width = "100%"
            )
          )
        )
      ),

      # Bouton de calcul
      actionButton(ns("computeRel"),
        "🔬 Analyser la relation",
        class = "compute-btn"
      ),
      tags$hr(style = "margin: 20px 0; border-color: #ecf0f1;"),

      # Résultats: Relation textuelle
      div(
        class = "metric-box",
        div(class = "metric-label", "📝 Relation (description)"),
        div(
          class = "metric-value",
          textOutput(ns("rel_text"))
        )
      ),

      # Résultats: Coefficients
      div(
        class = "link-title",
        icon("calculator"),
        "Coefficients génétiques"
      ),
      tableOutput(ns("rel_kappa")),
      tags$hr(style = "margin: 20px 0; border-color: #ecf0f1;"),

      # Option phrase canonique
      checkboxInput(
        ns("showCanonical"),
        "📖 Afficher les scénarios canoniques (constructPedigree)",
        FALSE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("showCanonical")),
        div(
          class = "metric-box",
          verbatimTextOutput(ns("rel_canonical"))
        )
      )
    )
  )
}

# ====================== MODULE SERVER ======================

linkAnalysisServer <- function(id, ped_reactive, selected_ids_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Mettre à jour les choix des sélecteurs quand le pedigree change
    observe({
      req(ped_reactive())
      id_choices <- labels(ped_reactive())

      updateSelectInput(session, "idA", choices = id_choices)
      updateSelectInput(session, "idB", choices = id_choices)
    })

    # Synchroniser avec la sélection externe (si disponible)
    observe({
      req(ped_reactive())
      ids <- selected_ids_reactive()

      if (length(ids) >= 1 && !identical(input$idA, ids[1])) {
        updateSelectInput(session, "idA", selected = ids[1])
      }
      if (length(ids) >= 2 && !identical(input$idB, ids[2])) {
        updateSelectInput(session, "idB", selected = ids[2])
      }
    })

    # Calcul de la relation
    observeEvent(input$computeRel, {
      req(ped_reactive())

      id1 <- input$idA
      id2 <- input$idB

      # Validation
      if (!isTruthy(id1) || !isTruthy(id2)) {
        output$rel_text <- renderText("⚠️ Sélectionnez deux individus.")
        output$rel_kappa <- renderTable(data.frame())
        output$rel_canonical <- renderText("")
        return()
      }

      if (identical(id1, id2)) {
        output$rel_text <- renderText("⚠️ Choisissez deux individus distincts.")
        output$rel_kappa <- renderTable(data.frame())
        output$rel_canonical <- renderText("")
        return()
      }

      ped <- ped_reactive()

      # 1) Description textuelle
      rel_pedtools <- tryCatch(
        pedtools::relation(ped, from = id1, to = id2),
        error = function(e) NA_character_
      )

      rel_verbal <- tryCatch(
        {
          txt <- format(verbalisr::verbalise(ped, c(id1, id2)))
          gsub("([[:graph:]])  ([[:graph:]])", "\\1 \\2", txt)
        },
        error = function(e) NULL
      )

      output$rel_text <- renderText({
        paste0(
          "🔗 ", id1, " ↔ ", id2, " : ",
          if (!is.na(rel_pedtools)) {
            paste0(rel_pedtools, if (!is.null(rel_verbal)) "  |  " else "")
          } else {
            ""
          },
          if (!is.null(rel_verbal)) rel_verbal else "(description indisponible)"
        )
      })

      # 2) Coefficients
      inb <- tryCatch(
        ribd::inbreeding(ped, c(id1, id2)),
        error = function(e) c(NA_real_, NA_real_)
      )

      phi <- tryCatch(
        ribd::kinship(ped, c(id1, id2)),
        error = function(e) NA_real_
      )

      deg <- tryCatch(
        ribd::kin2deg(phi, unrelated = NA),
        error = function(e) NA_real_
      )

      # κ si non-consanguins, sinon Δ1..Δ9
      if (all(!is.na(inb)) && all(inb == 0)) {
        # Pas de consanguinité → kappa
        kap <- tryCatch(
          ribd::kappaIBD(ped, c(id1, id2)),
          error = function(e) c(NA_real_, NA_real_, NA_real_)
        )

        df <- data.frame(
          Individu_1 = id1,
          Individu_2 = id2,
          f1 = round(inb[1], 4),
          f2 = round(inb[2], 4),
          `φ (phi)` = round(phi, 4),
          `Degré` = round(deg, 4),
          `κ0` = round(kap[1], 4),
          `κ1` = round(kap[2], 4),
          `κ2` = round(kap[3], 4),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      } else {
        # Consanguinité présente → Δ1..Δ9
        delta <- tryCatch(
          ribd::condensedIdentity(ped, c(id1, id2)),
          error = function(e) rep(NA_real_, 9)
        )

        names(delta) <- paste0("Δ", 1:9)

        df <- data.frame(
          Individu_1 = id1,
          Individu_2 = id2,
          f1 = round(inb[1], 4),
          f2 = round(inb[2], 4),
          `φ (phi)` = round(phi, 4),
          `Degré` = round(deg, 4),
          t(round(as.numeric(delta), 4)),
          check.names = FALSE,
          row.names = NULL
        )
        colnames(df)[7:15] <- paste0("Δ", 1:9)
      }

      output$rel_kappa <- renderTable(df, striped = TRUE, hover = TRUE)

      # 3) Scénarios canoniques (constructPedigree)
      output$rel_canonical <- renderText({
        if (!isTRUE(input$showCanonical)) {
          return("")
        }

        if (!(all(!is.na(inb)) && all(inb == 0))) {
          return("⚠️ constructPedigree non pertinent (individus consanguins).")
        }

        kap <- tryCatch(
          ribd::kappaIBD(ped, c(id1, id2)),
          error = function(e) c(NA, NA, NA)
        )

        if (any(is.na(kap))) {
          return("⚠️ Kappa introuvable.")
        }

        txt <- capture.output(
          ribd::constructPedigree(kappa = kap, describe = TRUE)
        )
        paste(txt[txt != ""], collapse = "\n")
      })
    })

    # Retourner les IDs sélectionnés pour synchronisation externe
    return(reactive({
      list(idA = input$idA, idB = input$idB)
    }))
  })
}

# ====================== EXEMPLE D'UTILISATION ======================

# Dans votre app.R principal:
#
# UI:
# tabPanel("LINK", linkAnalysisUI("link_module"))
#
# Server:
# linkAnalysisServer("link_module",
#                    ped_reactive = reactive(pedigree$ped),
#                    selected_ids_reactive = sel)
