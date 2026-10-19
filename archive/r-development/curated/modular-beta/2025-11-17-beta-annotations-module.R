# Archived R development file
# Original path: 🧩 Versions_Support/Support de test/Version beta TEST/R/Mod_annotations.R
# Original created: 2025-11-17 16:23:47
# Original modified: 2025-11-17 16:30:42
# Archive rationale: Annotation module from the beta modular experiment.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# ================================================================================
# MODULE: ANNOTATIONS & STATUTS
# ================================================================================
# Ce module gère les annotations et statuts des individus
# (Deceased, Proband, Adopted, Miscarriage)
# ================================================================================

#' UI du module annotations et statuts
#'
#' @param id Namespace ID pour le module
#'
#' @return Un wellPanel contenant les contrôles d'annotations
#'
#' @export
annotationsUI <- function(id) {
  ns <- NS(id)

  wellPanel(
    h4(icon("heartbeat"), " Status & Annotations"),
    div(
      class = "btn-group-vertical",
      uiOutput(ns("annotations_ui"))
    )
  )
}

#' Serveur du module annotations et statuts
#'
#' @param id Namespace ID pour le module
#' @param pedigree reactiveValues contenant ped et title
#' @param values reactiveValues contenant pedData
#' @param styles reactiveValues contenant les styles
#' @param sel reactiveVal contenant l'ID sélectionné
#' @param miscarriage reactiveVal pour les fausses couches
#' @param canPerformAction fonction de debouncing
#' @param saveToHistory fonction pour sauvegarder l'historique
#'
#' @return NULL (le module gère les événements de manière autonome)
#'
#' @export
annotationsServer <- function(id, pedigree, values, styles, sel, miscarriage,
                              canPerformAction, saveToHistory) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ─────────────────────────────────────────────────────────────────────────
    # UI DYNAMIQUE: BOUTONS D'ANNOTATIONS
    # ─────────────────────────────────────────────────────────────────────────

    output$annotations_ui <- renderUI({
      req(pedigree$ped, length(sel()) > 0)

      id <- sel()[1]

      is_deceased <- id %in% c(
        styles$deceased,
        labels(pedigree$ped)[values$pedData$deceased == TRUE]
      )

      is_proband <- length(styles$proband) > 0 && styles$proband == id

      is_adopted <- id %in% (styles$adopted %||% character(0))

      is_miscarriage <- id %in% miscarriage()

      has_children <- length(tryCatch(
        pedtools::children(pedigree$ped, id),
        error = function(e) character(0)
      )) > 0

      tagList(
        actionButton(
          ns("btn_toggle_deceased"),
          if (is_deceased) "✝ Deceased (ON)" else "✝ Deceased (OFF)",
          class = if (is_deceased) "toggle-btn active" else "toggle-btn",
          style = "width: 100%;"
        ),
        tags$div(
          style = "margin-top: 10px;",
          actionButton(
            ns("btn_toggle_proband"),
            if (is_proband) "▣ Proband (ON)" else "▣ Proband (OFF)",
            class = if (is_proband) "toggle-btn active" else "toggle-btn",
            style = "width: 100%;"
          )
        ),
        tags$div(
          style = "margin-top: 10px;",
          actionButton(
            ns("Adopted"),
            if (is_adopted) "[ ] Adopted (ON)" else "[ ] Adopted (OFF)",
            class = if (is_adopted) "toggle-btn active" else "toggle-btn",
            style = "width: 100%;"
          )
        ),
        tags$div(
          style = "margin-top: 10px;",
          if (has_children) {
            div(
              class = "warning-box",
              tags$strong("⚠️ Cannot mark as miscarriage"),
              tags$br(),
              "Individual has children."
            )
          } else {
            actionButton(
              ns("btn_toggle_miscarriage"),
              if (is_miscarriage) "△ Miscarriage (ON)" else "△ Miscarriage (OFF)",
              class = if (is_miscarriage) "toggle-btn active" else "toggle-btn",
              style = "width: 100%;"
            )
          }
        )
      )
    })

    # ─────────────────────────────────────────────────────────────────────────
    # ACTION: TOGGLE DECEASED
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$btn_toggle_deceased, {
      req(pedigree$ped, length(sel()) > 0)

      id <- sel()[1]
      current_deceased <- c(styles$deceased, labels(pedigree$ped)[values$pedData$deceased == TRUE])
      current_deceased <- unique(current_deceased)

      if (id %in% miscarriage()) {
        showNotification(
          "❌ Cannot mark miscarriage as deceased (use miscarriage status)",
          type = "error",
          duration = 3
        )
        return()
      }

      saveToHistory()

      if (id %in% current_deceased) {
        styles$deceased <- setdiff(styles$deceased, id)

        if (!is.null(values$pedData)) {
          idx <- which(values$pedData$id == id)
          if (length(idx) > 0) {
            values$pedData$deceased[idx] <- FALSE
          }
        }

        showNotification(
          sprintf("✅ Deceased status removed from %s", id),
          type = "message",
          duration = 3
        )
      } else {
        styles$deceased <- unique(c(styles$deceased, id))

        if (!is.null(values$pedData)) {
          idx <- which(values$pedData$id == id)
          if (length(idx) > 0) {
            values$pedData$deceased[idx] <- TRUE
          }
        }

        showNotification(
          sprintf("✅ %s marked as deceased", id),
          type = "message",
          duration = 3
        )
      }
    })

    # ─────────────────────────────────────────────────────────────────────────
    # ACTION: TOGGLE PROBAND
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$btn_toggle_proband, {
      req(pedigree$ped, length(sel()) > 0)

      id <- sel()[1]
      current_proband <- styles$proband

      saveToHistory()

      if (length(current_proband) > 0 && current_proband == id) {
        styles$proband <- character(0)
        showNotification(
          sprintf("✅ Proband status removed from %s", id),
          type = "message",
          duration = 3
        )
      } else {
        if (length(current_proband) > 0) {
          showNotification(
            sprintf("ℹ️ Proband changed from %s to %s", current_proband, id),
            type = "message",
            duration = 3
          )
        } else {
          showNotification(
            sprintf("✅ %s marked as proband", id),
            type = "message",
            duration = 3
          )
        }
        styles$proband <- id
      }
    })

    # ─────────────────────────────────────────────────────────────────────────
    # ACTION: TOGGLE ADOPTED
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$Adopted, {
      req(pedigree$ped, length(sel()) > 0)

      id <- sel()[1]
      current_adopted <- styles$adopted %||% character(0)

      saveToHistory()

      if (id %in% current_adopted) {
        styles$adopted <- setdiff(current_adopted, id)
        showNotification(
          sprintf("✅ Individual %s unmarked as Adopted", id),
          type = "message",
          duration = 2
        )
      } else {
        styles$adopted <- c(current_adopted, id)
        showNotification(
          sprintf("✅ Individual %s marked as Adopted", id),
          type = "message",
          duration = 2
        )
      }
    })

    # ─────────────────────────────────────────────────────────────────────────
    # ACTION: TOGGLE MISCARRIAGE
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$btn_toggle_miscarriage, {
      req(pedigree$ped, length(sel()) > 0)

      id <- sel()[1]
      current_misc <- miscarriage()

      has_children <- length(tryCatch(
        pedtools::children(pedigree$ped, id),
        error = function(e) character(0)
      )) > 0

      if (has_children) {
        showNotification(
          "❌ Cannot mark parent as miscarriage",
          type = "error",
          duration = 5
        )
        return()
      }

      saveToHistory()

      if (id %in% current_misc) {
        miscarriage(setdiff(current_misc, id))
        showNotification(
          sprintf("✅ Miscarriage status removed from %s", id),
          type = "message",
          duration = 3
        )
      } else {
        miscarriage(unique(c(current_misc, id)))
        showNotification(
          sprintf("✅ %s marked as miscarriage", id),
          type = "message",
          duration = 3
        )
      }
    })

    return(NULL)
  })
}
