# Archived R development file
# Original path: 🧩 Versions_Support/Support de test/Version beta TEST/R/Mod_phenotypes.R
# Original created: 2025-11-17 16:21:00
# Original modified: 2025-11-17 16:30:43
# Archive rationale: Phenotype module from the beta modular experiment.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# ================================================================================
# MODULE: GESTION DES PHÉNOTYPES
# ================================================================================
# Ce module gère la création, l'édition, la suppression et l'assignation
# de phénotypes aux individus du pedigree
# ================================================================================

#' UI du module de gestion des phénotypes
#'
#' @param id Namespace ID pour le module
#'
#' @return Un wellPanel contenant tous les contrôles de phénotypes
#'
#' @export
phenotypesUI <- function(id) {
  ns <- NS(id)

  wellPanel(
    div(
      class = "phenotype-section",
      h4(
        class = "phenotype-title",
        icon("palette"),
        " Phenotypes"
      ),
      actionButton(
        ns("btn_create_phenotype"),
        "Create New Phenotype",
        icon = icon("plus"),
        class = "btn-primary btn-block btn-sm",
        style = "margin-bottom: 1rem;"
      ),
      uiOutput(ns("phenotypeListUI"))
    )
  )
}

#' Serveur du module de gestion des phénotypes
#'
#' @param id Namespace ID pour le module
#' @param pedigree reactiveValues contenant ped et title
#' @param phenotypes reactiveValues contenant list et assign
#' @param styles reactiveValues contenant les styles visuels
#' @param sel reactiveVal contenant l'ID sélectionné
#' @param canPerformAction fonction de debouncing
#' @param saveToHistory fonction pour sauvegarder l'historique
#'
#' @return Liste de fonctions exposées pour l'app principale
#'
#' @export
phenotypesServer <- function(id, pedigree, phenotypes, styles, sel,
                             canPerformAction, saveToHistory) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # État local pour l'édition
    pheno_editing <- reactiveVal(NULL)

    # ─────────────────────────────────────────────────────────────────────────
    # REBUILD STYLES INTERNAL
    # ─────────────────────────────────────────────────────────────────────────

    rebuildStylesInternal <- function() {
      tryCatch(
        {
          saved_deceased <- styles$deceased
          saved_proband <- styles$proband
          saved_adopted <- styles$adopted

          styles$fill <- character(0)
          styles$hatched <- character(0)
          styles$col <- character(0)
          styles$lty <- list()

          if (length(phenotypes$assign) == 0) {
            styles$deceased <- saved_deceased
            styles$proband <- saved_proband
            styles$adopted <- saved_adopted
            return(invisible(NULL))
          }

          temp_fill <- character(0)
          temp_col <- character(0)
          temp_hatched <- character(0)
          temp_lty <- list()

          for (pheno_name in names(phenotypes$assign)) {
            assigned_ids <- phenotypes$assign[[pheno_name]]

            if (!pheno_name %in% names(phenotypes$list) || length(assigned_ids) == 0) {
              next
            }

            pheno_spec <- phenotypes$list[[pheno_name]]
            if (is.null(pheno_spec)) next

            for (id in assigned_ids) {
              if (!is.null(pheno_spec$fill) && nzchar(pheno_spec$fill)) {
                temp_fill[id] <- pheno_spec$fill
              }

              if (!is.null(pheno_spec$col) && nzchar(pheno_spec$col)) {
                temp_col[id] <- pheno_spec$col
              }
            }

            if (isTRUE(pheno_spec$hatched)) {
              temp_hatched <- unique(c(temp_hatched, assigned_ids))
            }

            if (!is.null(pheno_spec$lty) && nzchar(pheno_spec$lty)) {
              if (pheno_spec$lty == "dashed") {
                if (!"dashed" %in% names(temp_lty)) {
                  temp_lty$dashed <- character(0)
                }
                temp_lty$dashed <- unique(c(temp_lty$dashed, assigned_ids))
              } else if (pheno_spec$lty != "solid") {
                for (id in assigned_ids) {
                  temp_lty[[id]] <- pheno_spec$lty
                }
              }
            }
          }

          styles$fill <- temp_fill
          styles$col <- temp_col
          styles$hatched <- temp_hatched
          styles$lty <- temp_lty

          styles$deceased <- saved_deceased
          styles$proband <- saved_proband
          styles$adopted <- saved_adopted

          return(invisible(NULL))
        },
        error = function(e) {
          warning(paste("Error rebuilding styles:", e$message))
          return(invisible(NULL))
        }
      )
    }

    # ─────────────────────────────────────────────────────────────────────────
    # CRÉATION DE PHÉNOTYPE
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$btn_create_phenotype, {
      if (!canPerformAction("create_pheno_modal", 0.5)) {
        return()
      }

      showModal(modalDialog(
        title = tagList(icon("palette"), tags$strong(" Create New Phenotype")),
        size = "m",
        easyClose = TRUE,
        textInput(
          ns("pheno_name"),
          "Phenotype Name:",
          placeholder = "e.g., Affected, Carrier, Variant",
          value = ""
        ),
        fluidRow(
          column(
            6,
            colorPickerUI(ns("pheno_fill"), "Fill Color:", selected = "#FF6B6B")
          ),
          column(
            6,
            colorPickerUI(ns("pheno_col"), "Border Color:", selected = "#000000")
          )
        ),
        fluidRow(
          column(
            6,
            checkboxInput(ns("pheno_hatched"), "Hatched Pattern", value = FALSE)
          ),
          column(
            6,
            selectInput(
              ns("pheno_lty"),
              "Border Style:",
              choices = c(
                "Solid" = "solid",
                "Dashed" = "dashed",
                "Dotted" = "dotted",
                "Dot-Dash" = "dotdash"
              ),
              selected = "solid"
            )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("pheno_confirm"),
            "Create Phenotype",
            class = "btn btn-primary",
            icon = icon("check")
          )
        )
      ))
    })

    observeEvent(input$pheno_confirm, {
      req(input$pheno_name)

      if (!canPerformAction("confirm_pheno", 0.3)) {
        return()
      }

      name <- trimws(input$pheno_name)

      if (!nzchar(name)) {
        showNotification(
          "⚠️ Please enter a phenotype name",
          type = "warning",
          duration = 3
        )
        return()
      }

      if (name %in% names(phenotypes$list)) {
        showNotification(
          sprintf("⚠️ Phenotype '%s' already exists", name),
          type = "warning",
          duration = 3
        )
        return()
      }

      tryCatch(
        {
          phenotypes$list[[name]] <- list(
            fill = input$pheno_fill %||% "#FF6B6B",
            col = input$pheno_col %||% "#000000",
            hatched = isTRUE(input$pheno_hatched),
            lty = input$pheno_lty %||% "solid"
          )

          phenotypes$assign[[name]] <- character(0)

          removeModal()

          showNotification(
            sprintf("✅ Phenotype '%s' created", name),
            type = "message",
            duration = 3
          )
        },
        error = function(e) {
          showNotification(
            paste("Error creating phenotype:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # ─────────────────────────────────────────────────────────────────────────
    # LISTE DES PHÉNOTYPES
    # ─────────────────────────────────────────────────────────────────────────

    output$phenotypeListUI <- renderUI({
      if (length(phenotypes$list) == 0) {
        return(
          div(
            class = "no-selection",
            style = "margin-top: 10px;",
            "📋 No phenotypes created yet"
          )
        )
      }

      items <- lapply(names(phenotypes$list), function(pheno_name) {
        pheno_spec <- phenotypes$list[[pheno_name]]
        assigned_ids <- phenotypes$assign[[pheno_name]] %||% character(0)

        div(
          class = "phenotype-item",
          div(
            class = "phenotype-preview",
            style = sprintf(
              "width: 40px; height: 40px; border-radius: 6px; background: %s; border: 3px %s %s;",
              pheno_spec$fill,
              if (pheno_spec$lty == "dashed") "dashed" else "solid",
              pheno_spec$col
            )
          ),
          div(
            class = "phenotype-info",
            tags$a(
              class = "phenotype-name",
              pheno_name,
              href = "#",
              onclick = sprintf(
                'Shiny.setInputValue("%s", "%s", {priority:"event"}); return false;',
                ns("pheno_click"),
                pheno_name
              )
            ),
            div(
              class = "phenotype-count",
              sprintf("%d assigned", length(assigned_ids))
            )
          ),
          div(
            class = "phenotype-actions",
            actionButton(
              paste0(ns("pheno_edit_"), gsub("[^a-zA-Z0-9]", "_", pheno_name)),
              NULL,
              icon = icon("edit"),
              class = "icon-btn-small",
              onclick = sprintf(
                'Shiny.setInputValue("%s", "%s", {priority:"event"});',
                ns("pheno_edit"),
                pheno_name
              )
            ),
            actionButton(
              paste0(ns("pheno_delete_"), gsub("[^a-zA-Z0-9]", "_", pheno_name)),
              NULL,
              icon = icon("trash"),
              class = "icon-btn-small danger",
              onclick = sprintf(
                'Shiny.setInputValue("%s", "%s", {priority:"event"});',
                ns("pheno_delete"),
                pheno_name
              )
            )
          )
        )
      })

      tagList(items)
    })

    # ─────────────────────────────────────────────────────────────────────────
    # ASSIGNATION DE PHÉNOTYPE
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$pheno_click, {
      req(pedigree$ped, length(sel()) > 0)

      pheno_name <- input$pheno_click

      if (!pheno_name %in% names(phenotypes$list)) {
        return()
      }

      id <- sel()[1]
      current_assigned <- phenotypes$assign[[pheno_name]] %||% character(0)

      tryCatch(
        {
          saveToHistory()

          if (id %in% current_assigned) {
            phenotypes$assign[[pheno_name]] <- setdiff(current_assigned, id)
            showNotification(
              sprintf("✅ Removed '%s' from %s", pheno_name, id),
              type = "message",
              duration = 3
            )
          } else {
            phenotypes$assign[[pheno_name]] <- unique(c(current_assigned, id))
            showNotification(
              sprintf("✅ Assigned '%s' to %s", pheno_name, id),
              type = "message",
              duration = 3
            )
          }

          rebuildStylesInternal()
        },
        error = function(e) {
          showNotification(
            paste("Error assigning phenotype:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # ─────────────────────────────────────────────────────────────────────────
    # ÉDITION DE PHÉNOTYPE
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$pheno_edit, {
      pheno_name <- input$pheno_edit

      if (!pheno_name %in% names(phenotypes$list)) {
        return()
      }

      pheno_spec <- phenotypes$list[[pheno_name]]
      pheno_editing(pheno_name)

      showModal(modalDialog(
        title = tagList(icon("edit"), tags$strong(sprintf(" Edit Phenotype: %s", pheno_name))),
        size = "m",
        easyClose = TRUE,
        fluidRow(
          column(
            6,
            colorPickerUI(ns("pheno_edit_fill"), "Fill Color:", selected = pheno_spec$fill)
          ),
          column(
            6,
            colorPickerUI(ns("pheno_edit_col"), "Border Color:", selected = pheno_spec$col)
          )
        ),
        fluidRow(
          column(
            6,
            checkboxInput(
              ns("pheno_edit_hatched"),
              "Hatched Pattern",
              value = isTRUE(pheno_spec$hatched)
            )
          ),
          column(
            6,
            selectInput(
              ns("pheno_edit_lty"),
              "Border Style:",
              choices = c(
                "Solid" = "solid",
                "Dashed" = "dashed",
                "Dotted" = "dotted",
                "Dot-Dash" = "dotdash"
              ),
              selected = pheno_spec$lty %||% "solid"
            )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("pheno_edit_confirm"),
            "Save Changes",
            class = "btn btn-primary",
            icon = icon("check")
          )
        )
      ))
    })

    observeEvent(input$pheno_edit_confirm, {
      req(pheno_editing())

      pheno_name <- pheno_editing()

      tryCatch(
        {
          saveToHistory()

          phenotypes$list[[pheno_name]]$fill <- input$pheno_edit_fill %||% "#FF6B6B"
          phenotypes$list[[pheno_name]]$col <- input$pheno_edit_col %||% "#000000"
          phenotypes$list[[pheno_name]]$hatched <- isTRUE(input$pheno_edit_hatched)
          phenotypes$list[[pheno_name]]$lty <- input$pheno_edit_lty %||% "solid"

          rebuildStylesInternal()

          removeModal()
          pheno_editing(NULL)

          showNotification(
            sprintf("✅ Phenotype '%s' updated", pheno_name),
            type = "message",
            duration = 3
          )
        },
        error = function(e) {
          showNotification(
            paste("Error updating phenotype:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # ─────────────────────────────────────────────────────────────────────────
    # SUPPRESSION DE PHÉNOTYPE
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$pheno_delete, {
      pheno_name <- input$pheno_delete

      if (!pheno_name %in% names(phenotypes$list)) {
        return()
      }

      showModal(modalDialog(
        title = tagList(icon("warning", class = "text-warning"), tags$strong(" Delete Phenotype")),
        size = "m",
        easyClose = TRUE,
        tags$p(
          class = "text-danger",
          sprintf("Are you sure you want to delete the phenotype '%s'?", pheno_name)
        ),
        tags$p(
          class = "text-muted",
          "This will remove all assignments of this phenotype."
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("pheno_delete_confirm"),
            "Delete",
            class = "btn btn-danger",
            icon = icon("trash"),
            onclick = sprintf(
              'Shiny.setInputValue("%s", "%s", {priority:"event"});',
              ns("pheno_delete_name"),
              pheno_name
            )
          )
        )
      ))
    })

    observeEvent(input$pheno_delete_confirm, {
      req(input$pheno_delete_name)

      pheno_name <- input$pheno_delete_name

      tryCatch(
        {
          saveToHistory()

          phenotypes$list[[pheno_name]] <- NULL
          phenotypes$assign[[pheno_name]] <- NULL

          rebuildStylesInternal()

          removeModal()

          showNotification(
            sprintf("✅ Phenotype '%s' deleted", pheno_name),
            type = "message",
            duration = 3
          )
        },
        error = function(e) {
          showNotification(
            paste("Error deleting phenotype:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # ─────────────────────────────────────────────────────────────────────────
    # RETOUR DES FONCTIONS EXPOSÉES
    # ─────────────────────────────────────────────────────────────────────────

    return(
      list(
        rebuildStyles = rebuildStylesInternal,
        pheno_editing = pheno_editing
      )
    )
  })
}
