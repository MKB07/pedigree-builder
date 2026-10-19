# Archived R development file
# Original path: 🧩 Versions_Support/Support de test/Version beta TEST/R/Mod_context_menu.R
# Original created: 2025-11-17 16:22:52
# Original modified: 2025-11-17 16:30:42
# Archive rationale: Context menu module from the beta modular experiment.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# ================================================================================
# MODULE: MENU CONTEXTUEL (CONTEXT MENU)
# ================================================================================
# Ce module gère le menu contextuel qui s'affiche au clic droit sur un individu
# et permet d'accéder rapidement à certaines actions
# ================================================================================

#' UI du module menu contextuel
#'
#' @param id Namespace ID pour le module
#'
#' @return Un uiOutput pour le menu contextuel
#'
#' @export
contextMenuUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("context_menu"))
}

#' Serveur du module menu contextuel
#'
#' @param id Namespace ID pour le module
#' @param pedigree reactiveValues contenant ped et title
#' @param values reactiveValues contenant pedData
#' @param carrier reactiveVal pour les porteurs (dot)
#' @param starred reactiveVal pour les étoilés (*)
#' @param text_annotations reactiveVal pour les annotations textuelles
#' @param show_context_menu reactiveVal pour afficher/masquer le menu
#' @param context_menu_data reactiveVal contenant les données du menu
#'
#' @return NULL (le module gère les événements de manière autonome)
#'
#' @export
contextMenuServer <- function(id, pedigree, values, carrier, starred,
                              text_annotations, show_context_menu,
                              context_menu_data, session_main) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ─────────────────────────────────────────────────────────────────────────
    # RENDU DU MENU CONTEXTUEL
    # ─────────────────────────────────────────────────────────────────────────

    output$context_menu <- renderUI({
      if (!show_context_menu()) {
        return(NULL)
      }

      data <- context_menu_data()
      req(data)

      div(
        id = ns("context_menu_div"),
        class = "context-menu",
        style = sprintf("left: %dpx; top: %dpx;", data$menuX, data$menuY),
        div(
          class = "context-menu-header",
          sprintf("Individual: %s", data$id)
        ),
        div(class = "context-menu-section", "Information"),
        div(
          style = "padding: 8px 12px; font-size: 13px; line-height: 1.8;",
          HTML(sprintf(
            "<strong>Sex:</strong> %s<br/>
             <strong>Father:</strong> %s<br/>
             <strong>Mother:</strong> %s<br/>
             <strong>Children:</strong> %s",
            data$sex, data$father, data$mother, data$children
          ))
        ),
        tags$hr(),
        div(class = "context-menu-section", "Actions"),
        actionButton(
          ns("ctx_carrier"),
          if (data$isCarrier) "✓ Carrier (dot)" else "○ Carrier (dot)",
          class = "btn btn-link"
        ),
        actionButton(
          ns("ctx_starred"),
          if (data$isStarred) "✓ Starred (*)" else "○ Starred (*)",
          class = "btn btn-link"
        ),
        actionButton(
          ns("ctx_text_annot"),
          "📝 Add Text Annotation...",
          class = "btn btn-link"
        ),
        tags$hr(),
        actionButton(ns("ctx_close"), "✖ Close", class = "btn btn-link btn-close-menu")
      )
    })

    # ─────────────────────────────────────────────────────────────────────────
    # ACTION: FERMER LE MENU
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$ctx_close, {
      show_context_menu(FALSE)
    })

    # ─────────────────────────────────────────────────────────────────────────
    # ACTION: TOGGLE CARRIER (DOT)
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$ctx_carrier, {
      req(context_menu_data())
      id <- context_menu_data()$id

      current_carrier <- carrier()
      if (id %in% current_carrier) {
        carrier(setdiff(current_carrier, id))
        showNotification(
          sprintf("✅ Carrier status removed from %s", id),
          type = "message",
          duration = 3
        )
      } else {
        carrier(unique(c(current_carrier, id)))
        showNotification(
          sprintf("✅ %s marked as carrier (dot)", id),
          type = "message",
          duration = 3
        )
      }

      show_context_menu(FALSE)
    })

    # ─────────────────────────────────────────────────────────────────────────
    # ACTION: TOGGLE STARRED (*)
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$ctx_starred, {
      req(context_menu_data())
      id <- context_menu_data()$id

      current_starred <- starred()
      if (id %in% current_starred) {
        starred(setdiff(current_starred, id))
        showNotification(
          sprintf("✅ Star removed from %s", id),
          type = "message",
          duration = 3
        )
      } else {
        starred(unique(c(current_starred, id)))
        showNotification(
          sprintf("✅ %s marked with star (*)", id),
          type = "message",
          duration = 3
        )
      }

      show_context_menu(FALSE)
    })

    # ─────────────────────────────────────────────────────────────────────────
    # ACTION: AJOUTER ANNOTATION TEXTUELLE
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$ctx_text_annot, {
      req(context_menu_data())
      id <- context_menu_data()$id

      current_annots <- text_annotations()
      existing_text <- ""

      if (!is.null(current_annots)) {
        annot_list <- list()

        for (pos_name in names(current_annots)) {
          if (!is.null(current_annots[[pos_name]][[1]])) {
            vec <- current_annots[[pos_name]][[1]]
            if (id %in% names(vec)) {
              annot_list[[pos_name]] <- vec[id]
            }
          }
        }

        if (length(annot_list) > 0) {
          existing_text <- paste(
            sprintf("<strong>%s:</strong> %s", names(annot_list), unlist(annot_list)),
            collapse = "<br/>"
          )
        }
      }

      show_context_menu(FALSE)

      showModal(modalDialog(
        title = sprintf("Add Text Annotation for %s", id),
        size = "m",
        easyClose = TRUE,
        if (nchar(existing_text) > 0) {
          div(
            style = "background: #e3f2fd; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
            tags$strong("Existing annotations:"),
            tags$br(),
            HTML(existing_text)
          )
        },
        selectInput(
          ns("annot_position"),
          "Position:",
          choices = c(
            "Top" = "top",
            "Top Left" = "topleft",
            "Top Right" = "topright",
            "Left" = "left",
            "Right" = "right",
            "Bottom" = "bottom",
            "Bottom Left" = "bottomleft",
            "Bottom Right" = "bottomright",
            "Inside" = "inside"
          ),
          selected = "topright"
        ),
        textInput(
          ns("annot_text"),
          "Text:",
          value = "",
          placeholder = "Enter annotation text"
        ),
        tags$div(
          style = "margin-top: 10px; padding: 8px; background: #fff3cd; border-radius: 4px; font-size: 12px;",
          tags$strong("💡 Tip:"),
          " You can add multiple annotations at different positions."
        ),
        footer = tagList(
          if (nchar(existing_text) > 0) {
            actionButton(
              ns("btn_clear_all_annot"),
              "Clear All",
              class = "btn btn-warning"
            )
          },
          modalButton("Cancel"),
          actionButton(
            ns("btn_add_annot_confirm"),
            "Add",
            class = "btn btn-primary"
          )
        )
      ))

      session$userData$temp_annot_id <- id
    })

    # ─────────────────────────────────────────────────────────────────────────
    # CONFIRMER L'AJOUT D'ANNOTATION
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$btn_add_annot_confirm, {
      req(input$annot_text, input$annot_position)

      id <- session$userData$temp_annot_id

      if (is.null(id) || !nzchar(input$annot_text)) {
        showNotification(
          "⚠️ Please enter annotation text",
          type = "warning"
        )
        return()
      }

      current_annots <- text_annotations()
      if (is.null(current_annots)) {
        current_annots <- list()
      }

      position <- input$annot_position

      if (is.null(current_annots[[position]])) {
        current_annots[[position]] <- list(setNames(input$annot_text, id))
      } else {
        if (length(current_annots[[position]]) == 0) {
          current_annots[[position]][[1]] <- setNames(input$annot_text, id)
        } else {
          current_text <- current_annots[[position]][[1]]

          if (is.null(current_text)) {
            current_annots[[position]][[1]] <- setNames(input$annot_text, id)
          } else {
            current_text[id] <- input$annot_text
            current_annots[[position]][[1]] <- current_text
          }
        }
      }

      text_annotations(current_annots)

      removeModal()
      showNotification(
        sprintf("✅ Annotation added to %s at %s", id, position),
        type = "message",
        duration = 3
      )

      session$userData$temp_annot_id <- NULL
    })

    # ─────────────────────────────────────────────────────────────────────────
    # EFFACER TOUTES LES ANNOTATIONS D'UN INDIVIDU
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$btn_clear_all_annot, {
      id <- session$userData$temp_annot_id
      req(id)

      current_annots <- text_annotations()

      if (!is.null(current_annots)) {
        for (pos_name in names(current_annots)) {
          if (!is.null(current_annots[[pos_name]][[1]])) {
            vec <- current_annots[[pos_name]][[1]]

            if (id %in% names(vec)) {
              vec <- vec[names(vec) != id]

              if (length(vec) == 0) {
                current_annots[[pos_name]] <- NULL
              } else {
                current_annots[[pos_name]][[1]] <- vec
              }
            }
          }
        }

        text_annotations(current_annots)
      }

      removeModal()
      showNotification(
        sprintf("✅ All annotations cleared for %s", id),
        type = "message",
        duration = 3
      )

      session$userData$temp_annot_id <- NULL
    })

    return(NULL)
  })
}
