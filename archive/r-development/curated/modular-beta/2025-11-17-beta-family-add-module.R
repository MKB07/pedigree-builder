# Archived R development file
# Original path: 🧩 Versions_Support/Support de test/Version beta TEST/R/Mod_family_add.R
# Original created: 2025-11-17 16:20:26
# Original modified: 2025-11-17 16:41:58
# Archive rationale: Family-add module from the beta modular experiment.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# ================================================================================
# MODULE: AJOUT DE MEMBRES DE LA FAMILLE
# ================================================================================
# Ce module gère l'ajout de nouveaux membres à la famille (parents, enfants,
# frères/sœurs, demi-frères/sœurs, jumeaux, partenaires)
# ================================================================================

#' UI du module d'ajout de membres de famille
#'
#' @param id Namespace ID pour le module
#'
#' @return Un wellPanel contenant tous les contrôles d'ajout de famille
#'
#' @export
familyAddUI <- function(id) {
  ns <- NS(id)

  wellPanel(
    h4(icon("users"), " Add Family Members"),
    div(
      class = "btn-group-vertical",
      tags$div(
        style = "margin-bottom: 5px; font-weight: 600; font-size: 12px; color: #666; text-transform: uppercase;",
        "Parents & Children"
      ),
      actionButton(
        ns("addParents"),
        "Add Parents",
        icon = icon("user-plus"),
        class = "btn-success btn-sm"
      ),
      br(),
      uiOutput(ns("partner_selection_ui")),
      div(
        class = "control-panel",
        div(
          class = "control-section",
          div(class = "section-title", "➕ Add Relatives"),
          div(
            class = "tab-buttons",
            tags$button(
              id = ns("tab_children"),
              class = "tab-btn active",
              "👶 Children",
              onclick = sprintf("Shiny.setInputValue('%s', 'children', {priority: 'event'}); $('.tab-btn').removeClass('active'); $(this).addClass('active');", ns("active_tab"))
            ),
            tags$button(
              id = ns("tab_siblings"),
              class = "tab-btn",
              "👥 Siblings",
              onclick = sprintf("Shiny.setInputValue('%s', 'siblings', {priority: 'event'}); $('.tab-btn').removeClass('active'); $(this).addClass('active');", ns("active_tab"))
            ),
            tags$button(
              id = ns("tab_half_siblings"),
              class = "tab-btn",
              "👥½ Half",
              onclick = sprintf("Shiny.setInputValue('%s', 'half_siblings', {priority: 'event'}); $('.tab-btn').removeClass('active'); $(this).addClass('active');", ns("active_tab"))
            )
          ),
          uiOutput(ns("relatives_content"))
        )
      ),
      hr(style = "margin: 10px 0;"),
      tags$div(
        style = "margin-bottom: 5px; font-weight: 600; font-size: 12px; color: #666; text-transform: uppercase;",
        "Twins & Triplets"
      ),
      uiOutput(ns("twins_triplets_ui"))
    )
  )
}

#' Serveur du module d'ajout de membres de famille
#'
#' @param id Namespace ID pour le module
#' @param pedigree reactiveValues contenant ped et title
#' @param values reactiveValues contenant pedData
#' @param sel reactiveVal contenant l'ID sélectionné
#' @param twins_df reactiveVal contenant le dataframe des jumeaux
#' @param canPerformAction fonction de debouncing
#' @param saveToHistory fonction pour sauvegarder l'historique
#' @param cleanOrphanPhenotypes fonction pour nettoyer les phénotypes orphelins
#'
#' @return Liste de reactives exposés pour l'app principale
#'
#' @export
familyAddServer <- function(id, pedigree, values, sel, twins_df,
                            canPerformAction, saveToHistory,
                            cleanOrphanPhenotypes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # États locaux
    num_relatives <- reactiveVal(1)
    sex_selected <- reactiveVal(1)
    twin_type_selected <- reactiveVal(2)
    twin_sex_selected <- reactiveVal(1)
    active_tab <- reactiveVal("children")

    # ─────────────────────────────────────────────────────────────────────────
    # GESTION DES ONGLETS
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$active_tab, {
      active_tab(input$active_tab)
      num_relatives(1)
    })

    observeEvent(input$sex_selected, {
      sex_selected(input$sex_selected)
    })

    observeEvent(input$increase_relatives, {
      num_relatives(min(num_relatives() + 1, 10))
    })

    observeEvent(input$decrease_relatives, {
      num_relatives(max(num_relatives() - 1, 1))
    })

    output$num_relatives_display <- renderText({
      as.character(num_relatives())
    })

    # ─────────────────────────────────────────────────────────────────────────
    # UI DYNAMIQUE: SÉLECTION DU PARTENAIRE
    # ─────────────────────────────────────────────────────────────────────────

    output$partner_selection_ui <- renderUI({
      req(pedigree$ped, length(sel()) > 0)

      id <- sel()[1]
      partners <- getPartners(pedigree$ped, id)

      if (length(partners) == 0) {
        return(NULL)
      }

      if (length(partners) == 1) {
        return(
          tags$div(
            style = "padding: 8px; background: #f0f9ff; border-radius: 4px; margin-bottom: 8px; font-size: 13px;",
            icon("info-circle", class = "text-info"),
            sprintf(" Partner: %s", partners[1])
          )
        )
      }

      tagList(
        tags$label(
          style = "font-size: 13px; color: #4b5563; margin-bottom: 4px; display: block;",
          "Select Partner:"
        ),
        selectInput(
          ns("selected_partner"),
          NULL,
          choices = setNames(partners, partners),
          selected = partners[1],
          width = "100%"
        )
      )
    })

    # ─────────────────────────────────────────────────────────────────────────
    # UI DYNAMIQUE: CONTENU DES ONGLETS
    # ─────────────────────────────────────────────────────────────────────────

    output$relatives_content <- renderUI({
      tab <- active_tab()

      if (tab == "children") {
        tagList(
          div(
            style = "margin-bottom: 10px;",
            tags$label(
              style = "font-size: 12px; color: #6b7280; margin-bottom: 5px; display: block;",
              "Gender:"
            ),
            div(
              class = "sex-buttons",
              tags$button(
                id = ns("sex_male"),
                class = if (sex_selected() == 1) "sex-btn active" else "sex-btn",
                "👨 Male",
                onclick = sprintf("Shiny.setInputValue('%s', 1, {priority: 'event'});", ns("sex_selected"))
              ),
              tags$button(
                id = ns("sex_female"),
                class = if (sex_selected() == 2) "sex-btn active" else "sex-btn",
                "👩 Female",
                onclick = sprintf("Shiny.setInputValue('%s', 2, {priority: 'event'});", ns("sex_selected"))
              ),
              tags$button(
                id = ns("sex_unknown"),
                class = if (sex_selected() == 0) "sex-btn active" else "sex-btn",
                "⚪ Unknown",
                onclick = sprintf("Shiny.setInputValue('%s', 0, {priority: 'event'});", ns("sex_selected"))
              )
            )
          ),
          div(
            style = "margin-bottom: 10px;",
            tags$label(
              style = "font-size: 12px; color: #6b7280; margin-bottom: 5px; display: block;",
              "Number of Children:"
            ),
            div(
              class = "number-stepper",
              tags$button(
                class = "stepper-btn",
                "−",
                onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'});", ns("decrease_relatives"))
              ),
              div(
                class = "stepper-value",
                textOutput(ns("num_relatives_display"), inline = TRUE)
              ),
              tags$button(
                class = "stepper-btn",
                "+",
                onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'});", ns("increase_relatives"))
              )
            )
          ),
          actionButton(
            ns("addChildren"),
            "Add Children",
            icon = icon("baby"),
            class = "btn-primary-action"
          )
        )
      } else if (tab == "siblings") {
        tagList(
          div(
            style = "margin-bottom: 10px;",
            tags$label(
              style = "font-size: 12px; color: #6b7280; margin-bottom: 5px; display: block;",
              "Gender:"
            ),
            div(
              class = "sex-buttons",
              tags$button(
                id = ns("sex_male"),
                class = if (sex_selected() == 1) "sex-btn active" else "sex-btn",
                "👨 Male",
                onclick = sprintf("Shiny.setInputValue('%s', 1, {priority: 'event'});", ns("sex_selected"))
              ),
              tags$button(
                id = ns("sex_female"),
                class = if (sex_selected() == 2) "sex-btn active" else "sex-btn",
                "👩 Female",
                onclick = sprintf("Shiny.setInputValue('%s', 2, {priority: 'event'});", ns("sex_selected"))
              ),
              tags$button(
                id = ns("sex_unknown"),
                class = if (sex_selected() == 0) "sex-btn active" else "sex-btn",
                "⚪ Unknown",
                onclick = sprintf("Shiny.setInputValue('%s', 0, {priority: 'event'});", ns("sex_selected"))
              )
            )
          ),
          div(
            style = "margin-bottom: 10px;",
            tags$label(
              style = "font-size: 12px; color: #6b7280; margin-bottom: 5px; display: block;",
              "Number of Siblings:"
            ),
            div(
              class = "number-stepper",
              tags$button(
                class = "stepper-btn",
                "−",
                onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'});", ns("decrease_relatives"))
              ),
              div(
                class = "stepper-value",
                textOutput(ns("num_relatives_display"), inline = TRUE)
              ),
              tags$button(
                class = "stepper-btn",
                "+",
                onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'});", ns("increase_relatives"))
              )
            )
          ),
          actionButton(
            ns("addSiblings"),
            "Add Full Siblings",
            icon = icon("users"),
            class = "btn-primary-action"
          )
        )
      } else if (tab == "half_siblings") {
        tagList(
          div(
            style = "margin-bottom: 10px;",
            tags$label(
              style = "font-size: 12px; color: #6b7280; margin-bottom: 5px; display: block;",
              "Gender:"
            ),
            div(
              class = "sex-buttons",
              tags$button(
                id = ns("sex_male"),
                class = if (sex_selected() == 1) "sex-btn active" else "sex-btn",
                "👨 Male",
                onclick = sprintf("Shiny.setInputValue('%s', 1, {priority: 'event'});", ns("sex_selected"))
              ),
              tags$button(
                id = ns("sex_female"),
                class = if (sex_selected() == 2) "sex-btn active" else "sex-btn",
                "👩 Female",
                onclick = sprintf("Shiny.setInputValue('%s', 2, {priority: 'event'});", ns("sex_selected"))
              ),
              tags$button(
                id = ns("sex_unknown"),
                class = if (sex_selected() == 0) "sex-btn active" else "sex-btn",
                "⚪ Unknown",
                onclick = sprintf("Shiny.setInputValue('%s', 0, {priority: 'event'});", ns("sex_selected"))
              )
            )
          ),
          div(
            style = "margin-bottom: 10px;",
            tags$label(
              style = "font-size: 12px; color: #6b7280; margin-bottom: 5px; display: block;",
              "Number of Half-Siblings:"
            ),
            div(
              class = "number-stepper",
              tags$button(
                class = "stepper-btn",
                "−",
                onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'});", ns("decrease_relatives"))
              ),
              div(
                class = "stepper-value",
                textOutput(ns("num_relatives_display"), inline = TRUE)
              ),
              tags$button(
                class = "stepper-btn",
                "+",
                onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'});", ns("increase_relatives"))
              )
            )
          ),
          div(
            style = "margin-bottom: 15px;",
            radioButtons(
              ns("half_sib_type"),
              "Shared Parent:",
              choices = c(
                "Paternal (same father)" = "father",
                "Maternal (same mother)" = "mother"
              ),
              selected = "father",
              inline = FALSE
            )
          ),
          actionButton(
            ns("addHalfSiblings"),
            "Add Half-Siblings",
            icon = icon("user-friends"),
            class = "btn-primary-action"
          )
        )
      }
    })

    # ─────────────────────────────────────────────────────────────────────────
    # UI DYNAMIQUE: JUMEAUX ET TRIPLETS
    # ─────────────────────────────────────────────────────────────────────────

    output$twins_triplets_ui <- renderUI({
      req(pedigree$ped, length(sel()) > 0)

      id <- sel()[1]
      siblings <- getSiblings(pedigree$ped, id)

      if (length(siblings) == 0) {
        return(
          tags$div(
            style = "padding: 8px; background: #fef3c7; border-radius: 4px; font-size: 12px; color: #92400e;",
            icon("info-circle"),
            " No siblings available for twin/triplet relationship"
          )
        )
      }

      tagList(
        selectInput(
          ns("twin_sibling"),
          "Select Sibling:",
          choices = setNames(siblings, siblings),
          width = "100%"
        ),
        div(
          style = "margin-bottom: 10px;",
          tags$label(
            style = "font-size: 12px; color: #6b7280; margin-bottom: 5px; display: block;",
            "Twin Type:"
          ),
          div(
            class = "twin-type-buttons",
            tags$button(
              class = if (twin_type_selected() == 1) "twin-btn active" else "twin-btn",
              "Monozygotic",
              onclick = sprintf("Shiny.setInputValue('%s', 1, {priority: 'event'});", ns("twin_type_selected"))
            ),
            tags$button(
              class = if (twin_type_selected() == 2) "twin-btn active" else "twin-btn",
              "Dizygotic",
              onclick = sprintf("Shiny.setInputValue('%s', 2, {priority: 'event'});", ns("twin_type_selected"))
            )
          )
        ),
        actionButton(
          ns("add_twin"),
          "Mark as Twin",
          icon = icon("users"),
          class = "btn-success btn-sm btn-block"
        ),
        actionButton(
          ns("remove_twin"),
          "Remove Twin Status",
          icon = icon("times"),
          class = "btn-warning btn-sm btn-block"
        )
      )
    })

    observeEvent(input$twin_type_selected, {
      twin_type_selected(input$twin_type_selected)
    })

    # ─────────────────────────────────────────────────────────────────────────
    # LOGIQUE: AJOUT DE PARENTS
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$addParents, {
      req(pedigree$ped, length(sel()) > 0)

      if (!canPerformAction("add_parents", 0.3)) {
        return()
      }

      id <- sel()[1]

      parents <- tryCatch(
        pedtools::parents(pedigree$ped, id, internal = FALSE),
        error = function(e) c(NA, NA)
      )

      if (!is.na(parents[1]) || !is.na(parents[2])) {
        showNotification(
          "❌ Individual already has parents",
          type = "error",
          duration = 3
        )
        return()
      }

      tryCatch(
        {
          saveToHistory()

          new_ped <- pedtools::addParents(
            pedigree$ped,
            id,
            father = 0,
            mother = 0,
            verbose = FALSE
          )

          new_ped <- improve_layout(new_ped)

          validation <- validatePedigree(new_ped)
          if (!validation$valid) {
            showNotification(
              paste("Error:", validation$message),
              type = "error",
              duration = 5
            )
            return()
          }

          pedigree$ped <- new_ped

          old_data <- values$pedData
          new_data <- makePedData(new_ped)

          if (!is.null(old_data)) {
            for (col in names(old_data)) {
              if (col %in% names(new_data)) {
                for (old_id in old_data$id) {
                  if (old_id %in% new_data$id) {
                    idx <- which(new_data$id == old_id)
                    new_data[[col]][idx] <- old_data[[col]][old_data$id == old_id]
                  }
                }
              }
            }
          }

          values$pedData <- new_data

          cleanOrphanPhenotypes()

          showNotification(
            sprintf("✅ Parents added to %s", id),
            type = "message",
            duration = 3
          )
        },
        error = function(e) {
          showNotification(
            paste("Error adding parents:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # ─────────────────────────────────────────────────────────────────────────
    # LOGIQUE: AJOUT D'ENFANTS
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$addChildren, {
      req(pedigree$ped, length(sel()) > 0)

      if (!canPerformAction("add_children", 0.3)) {
        return()
      }

      id <- sel()[1]
      n <- num_relatives()
      sex_val <- sex_selected()

      partners <- getPartners(pedigree$ped, id)

      if (length(partners) == 0) {
        showNotification(
          "❌ Individual must have a partner to add children",
          type = "error",
          duration = 4
        )
        return()
      }

      partner_id <- if (length(partners) == 1) {
        partners[1]
      } else {
        input$selected_partner
      }

      if (is.null(partner_id) || !nzchar(partner_id)) {
        showNotification(
          "⚠️ Please select a partner",
          type = "warning",
          duration = 3
        )
        return()
      }

      id_sex <- pedtools::getSex(pedigree$ped, id)
      partner_sex <- pedtools::getSex(pedigree$ped, partner_id)

      if (id_sex == 1) {
        father_id <- id
        mother_id <- partner_id
      } else if (id_sex == 2) {
        father_id <- partner_id
        mother_id <- id
      } else {
        showNotification(
          "❌ Cannot add children: sex of selected individual must be determined",
          type = "error",
          duration = 4
        )
        return()
      }

      tryCatch(
        {
          saveToHistory()

          new_ped <- pedtools::addChildren(
            pedigree$ped,
            father = father_id,
            mother = mother_id,
            nch = n,
            sex = sex_val,
            verbose = FALSE
          )

          new_ped <- improve_layout(new_ped)

          validation <- validatePedigree(new_ped)
          if (!validation$valid) {
            showNotification(
              paste("Error:", validation$message),
              type = "error",
              duration = 5
            )
            return()
          }

          pedigree$ped <- new_ped

          old_data <- values$pedData
          new_data <- makePedData(new_ped)

          if (!is.null(old_data)) {
            for (col in names(old_data)) {
              if (col %in% names(new_data)) {
                for (old_id in old_data$id) {
                  if (old_id %in% new_data$id) {
                    idx <- which(new_data$id == old_id)
                    new_data[[col]][idx] <- old_data[[col]][old_data$id == old_id]
                  }
                }
              }
            }
          }

          values$pedData <- new_data

          cleanOrphanPhenotypes()

          sex_text <- c("unknown sex", "male", "female")[sex_val + 1]

          showNotification(
            sprintf(
              "✅ Added %d %s child(ren) to %s and %s",
              n, sex_text, id, partner_id
            ),
            type = "message",
            duration = 3
          )
        },
        error = function(e) {
          showNotification(
            paste("Error adding children:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # ─────────────────────────────────────────────────────────────────────────
    # LOGIQUE: AJOUT DE FRÈRES/SŒURS
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$addSiblings, {
      req(pedigree$ped, length(sel()) > 0)

      if (!canPerformAction("add_siblings", 0.3)) {
        return()
      }

      id <- sel()[1]
      n <- num_relatives()
      sex_val <- sex_selected()

      tryCatch(
        {
          saveToHistory()

          new_ped <- pedtools::addSib(
            pedigree$ped,
            id,
            nch = n,
            sex = sex_val,
            verbose = FALSE
          )

          new_ped <- improve_layout(new_ped)

          validation <- validatePedigree(new_ped)
          if (!validation$valid) {
            showNotification(
              paste("Error:", validation$message),
              type = "error",
              duration = 5
            )
            return()
          }

          pedigree$ped <- new_ped

          old_data <- values$pedData
          new_data <- makePedData(new_ped)

          if (!is.null(old_data)) {
            for (col in names(old_data)) {
              if (col %in% names(new_data)) {
                for (old_id in old_data$id) {
                  if (old_id %in% new_data$id) {
                    idx <- which(new_data$id == old_id)
                    new_data[[col]][idx] <- old_data[[col]][old_data$id == old_id]
                  }
                }
              }
            }
          }

          values$pedData <- new_data

          cleanOrphanPhenotypes()

          sex_text <- c("unknown sex", "male", "female")[sex_val + 1]

          showNotification(
            sprintf("✅ Added %d %s full sibling(s) to %s", n, sex_text, id),
            type = "message",
            duration = 3
          )
        },
        error = function(e) {
          showNotification(
            paste("Error adding siblings:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # ─────────────────────────────────────────────────────────────────────────
    # LOGIQUE: AJOUT DE DEMI-FRÈRES/SŒURS
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$addHalfSiblings, {
      req(pedigree$ped, length(sel()) > 0)

      if (!canPerformAction("add_half_siblings", 0.3)) {
        return()
      }

      id <- sel()[1]
      n <- num_relatives()
      sex_val <- sex_selected()
      sib_type <- input$half_sib_type

      if (is.null(sib_type)) {
        sib_type <- "father"
      }

      tryCatch(
        {
          saveToHistory()

          parents <- pedtools::parents(pedigree$ped, id, internal = FALSE)

          if (all(is.na(parents))) {
            showNotification(
              "❌ Individual must have at least one parent to add half-siblings",
              type = "error",
              duration = 4
            )
            return()
          }

          if (sib_type == "father") {
            if (is.na(parents[1])) {
              showNotification(
                "❌ Individual must have a father to add paternal half-siblings",
                type = "error",
                duration = 4
              )
              return()
            }
            parent_id <- parents[1]
          } else {
            if (is.na(parents[2])) {
              showNotification(
                "❌ Individual must have a mother to add maternal half-siblings",
                type = "error",
                duration = 4
              )
              return()
            }
            parent_id <- parents[2]
          }

          parent_sex <- pedtools::getSex(pedigree$ped, parent_id)

          new_spouse_id <- max(as.numeric(labels(pedigree$ped))) + 1
          new_spouse_sex <- if (parent_sex == 1) 2 else 1

          new_ped <- pedtools::addChildren(
            pedigree$ped,
            father = if (parent_sex == 1) parent_id else new_spouse_id,
            mother = if (parent_sex == 2) parent_id else new_spouse_id,
            nch = n,
            sex = sex_val,
            verbose = FALSE
          )

          new_ped <- improve_layout(new_ped)

          validation <- validatePedigree(new_ped)
          if (!validation$valid) {
            showNotification(
              paste("Error:", validation$message),
              type = "error",
              duration = 5
            )
            return()
          }

          pedigree$ped <- new_ped

          old_data <- values$pedData
          new_data <- makePedData(new_ped)

          if (!is.null(old_data)) {
            for (col in names(old_data)) {
              if (col %in% names(new_data)) {
                for (old_id in old_data$id) {
                  if (old_id %in% new_data$id) {
                    idx <- which(new_data$id == old_id)
                    new_data[[col]][idx] <- old_data[[col]][old_data$id == old_id]
                  }
                }
              }
            }
          }

          values$pedData <- new_data

          cleanOrphanPhenotypes()

          sex_text <- c("unknown sex", "male", "female")[sex_val + 1]
          parent_type <- if (sib_type == "father") "paternal" else "maternal"

          showNotification(
            sprintf(
              "✅ Added %d %s %s half-sibling(s) to %s",
              n, sex_text, parent_type, id
            ),
            type = "message",
            duration = 3
          )
        },
        error = function(e) {
          showNotification(
            paste("Error adding half-siblings:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # ─────────────────────────────────────────────────────────────────────────
    # LOGIQUE: JUMEAUX
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$add_twin, {
      req(pedigree$ped, length(sel()) > 0, input$twin_sibling)

      if (!canPerformAction("add_twin", 0.3)) {
        return()
      }

      id <- sel()[1]
      sibling_id <- input$twin_sibling
      twin_code <- twin_type_selected()

      if (id == sibling_id) {
        showNotification(
          "❌ Cannot mark an individual as their own twin",
          type = "error",
          duration = 3
        )
        return()
      }

      tryCatch(
        {
          saveToHistory()

          current_twins <- twins_df()

          existing <- which(
            (current_twins$id1 == id & current_twins$id2 == sibling_id) |
              (current_twins$id1 == sibling_id & current_twins$id2 == id)
          )

          if (length(existing) > 0) {
            current_twins$code[existing] <- twin_code
            showNotification(
              sprintf("✅ Updated twin relationship: %s ↔ %s", id, sibling_id),
              type = "message",
              duration = 3
            )
          } else {
            new_row <- data.frame(
              id1 = id,
              id2 = sibling_id,
              code = twin_code,
              stringsAsFactors = FALSE
            )
            current_twins <- rbind(current_twins, new_row)

            twin_type <- if (twin_code == 1) "monozygotic" else "dizygotic"
            showNotification(
              sprintf("✅ Marked %s and %s as %s twins", id, sibling_id, twin_type),
              type = "message",
              duration = 3
            )
          }

          twins_df(current_twins)
        },
        error = function(e) {
          showNotification(
            paste("Error adding twin relationship:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    observeEvent(input$remove_twin, {
      req(pedigree$ped, length(sel()) > 0, input$twin_sibling)

      if (!canPerformAction("remove_twin", 0.3)) {
        return()
      }

      id <- sel()[1]
      sibling_id <- input$twin_sibling

      tryCatch(
        {
          saveToHistory()

          current_twins <- twins_df()

          to_remove <- which(
            (current_twins$id1 == id & current_twins$id2 == sibling_id) |
              (current_twins$id1 == sibling_id & current_twins$id2 == id)
          )

          if (length(to_remove) > 0) {
            current_twins <- current_twins[-to_remove, , drop = FALSE]
            twins_df(current_twins)

            showNotification(
              sprintf("✅ Removed twin relationship: %s ↔ %s", id, sibling_id),
              type = "message",
              duration = 3
            )
          } else {
            showNotification(
              "ℹ️ No twin relationship found to remove",
              type = "warning",
              duration = 3
            )
          }
        },
        error = function(e) {
          showNotification(
            paste("Error removing twin relationship:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # Retourner les valeurs nécessaires pour l'app principale
    return(
      list(
        num_relatives = num_relatives,
        sex_selected = sex_selected,
        active_tab = active_tab
      )
    )
  })
}
