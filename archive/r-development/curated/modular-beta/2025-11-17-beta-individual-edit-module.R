# Archived R development file
# Original path: 🧩 Versions_Support/Support de test/Version beta TEST/R/Mod_individual_edit.R
# Original created: 2025-11-17 16:22:24
# Original modified: 2025-11-17 16:30:41
# Archive rationale: Individual editing module from the beta modular experiment.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# ================================================================================
# MODULE: ÉDITION D'INDIVIDU
# ================================================================================
# Ce module gère l'édition complète des informations d'un individu
# (nom, prénom, dates, genre, commentaires, etc.)
# ================================================================================

#' Serveur du module d'édition d'individu
#'
#' @param id Namespace ID pour le module
#' @param pedigree reactiveValues contenant ped et title
#' @param values reactiveValues contenant pedData
#' @param sel reactiveVal contenant l'ID sélectionné
#' @param styles reactiveValues contenant les styles
#' @param canPerformAction fonction de debouncing
#' @param saveToHistory fonction pour sauvegarder l'historique
#'
#' @return NULL (le module gère les événements de manière autonome)
#'
#' @export
individualEditServer <- function(id, pedigree, values, sel, styles,
                                 canPerformAction, saveToHistory) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ─────────────────────────────────────────────────────────────────────────
    # FONCTION: VÉRIFIER SI LE SEXE PEUT ÊTRE CHANGÉ
    # ─────────────────────────────────────────────────────────────────────────

    canChangeSex <- function(ped, id, new_sex) {
      tryCatch(
        {
          all_ids <- labels(ped)
          id_idx <- match(id, all_ids)

          if (is.na(id_idx)) {
            return(list(can = FALSE, reason = "Individual not found"))
          }

          children_as_father <- which(ped$FIDX == id_idx)
          children_as_mother <- which(ped$MIDX == id_idx)

          if (length(children_as_father) > 0 && new_sex != 1) {
            return(list(
              can = FALSE,
              reason = "Cannot change: individual is father of children"
            ))
          }

          if (length(children_as_mother) > 0 && new_sex != 2) {
            return(list(
              can = FALSE,
              reason = "Cannot change: individual is mother of children"
            ))
          }

          return(list(can = TRUE, reason = NULL))
        },
        error = function(e) {
          return(list(can = FALSE, reason = paste("Error:", e$message)))
        }
      )
    }

    safeSexChange <- function(ped, id, new_sex) {
      validation <- canChangeSex(ped, id, new_sex)

      if (!validation$can) {
        stop(validation$reason)
      }

      new_ped <- pedtools::setSex(ped, ids = id, sex = new_sex)
      return(new_ped)
    }

    # ─────────────────────────────────────────────────────────────────────────
    # OBSERVER: OUVRIR LA MODALE D'ÉDITION (DOUBLE-CLIC)
    # ─────────────────────────────────────────────────────────────────────────

    # Note: Cet observer doit être déclenché depuis l'app principale
    # via un input spécifique comme "open_edit_modal"

    # ─────────────────────────────────────────────────────────────────────────
    # FONCTION PUBLIQUE: OUVRIR LA MODALE D'ÉDITION
    # ─────────────────────────────────────────────────────────────────────────

    openEditModal <- function(individual_id) {
      tryCatch(
        {
          req(values$pedData)

          id <- individual_id
          row <- values$pedData[values$pedData$id == id, , drop = FALSE]

          if (!nrow(row)) {
            return()
          }

          asDate <- function(x) {
            if (!nzchar(x)) {
              return(NULL)
            }
            tryCatch(as.Date(x, "%d-%m-%Y"), error = function(e) NULL)
          }

          dob <- asDate(row$date_of_birth)
          dod <- asDate(row$date_of_death)

          showModal(modalDialog(
            title = tagList(
              icon("user-edit"),
              sprintf(" Edit Individual %s", id)
            ),
            size = "m",
            easyClose = TRUE,
            fluidRow(
              column(
                4,
                textInput(ns("ed_last"), "Last Name", value = row$last_name)
              ),
              column(
                4,
                textInput(ns("ed_first"), "First Name", value = row$first_name)
              ),
              column(
                4,
                selectInput(
                  ns("ed_gender"),
                  "Gender",
                  choices = c("Male" = "1", "Female" = "2", "Unknown" = "0"),
                  selected = as.character(row$sex)
                )
              )
            ),
            hr(),
            fluidRow(
              column(
                4,
                airDatepickerInput(
                  ns("ed_dob"),
                  "Date of Birth",
                  value = dob,
                  placeholder = "🗓️ Select date",
                  dateFormat = "dd/MM/yyyy",
                  language = "en",
                  autoClose = TRUE,
                  clearButton = TRUE
                )
              ),
              column(
                4,
                airDatepickerInput(
                  ns("ed_dod"),
                  "Date of Death",
                  value = dod,
                  placeholder = "🗓️ Select date",
                  dateFormat = "dd/MM/yyyy",
                  language = "en",
                  autoClose = TRUE,
                  clearButton = TRUE
                )
              ),
              column(
                2,
                checkboxInput(ns("ed_dead"), "Deceased", value = isTRUE(row$deceased))
              ),
              column(
                2,
                textInput(ns("ed_age"), "Age", value = row$age)
              )
            ),
            fluidRow(
              column(
                6,
                textInput(
                  ns("ed_aab"),
                  "Assigned at Birth (AAB)",
                  value = row$assigned_at_birth,
                  placeholder = "e.g., AFAB, AMAB"
                )
              ),
              column(6, NULL)
            ),
            textAreaInput(
              ns("ed_comments"),
              "Comments",
              value = row$comments,
              rows = 2,
              placeholder = "Additional notes..."
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                ns("ed_save"),
                "Save Changes",
                class = "btn btn-primary",
                icon = icon("save"),
                onclick = sprintf(
                  'Shiny.setInputValue("%s", "%s", {priority:"event"})',
                  ns("ed_target"),
                  id
                )
              )
            )
          ))
        },
        error = function(e) {
          showNotification(
            paste("Error opening edit dialog:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    }

    # ─────────────────────────────────────────────────────────────────────────
    # OBSERVER: SAUVEGARDER LES MODIFICATIONS
    # ─────────────────────────────────────────────────────────────────────────

    observeEvent(input$ed_save, {
      req(values$pedData, input$ed_target)

      if (!canPerformAction("save_edit", 0.5)) {
        return()
      }

      id <- input$ed_target
      i <- which(values$pedData$id == id)

      req(length(i) == 1)

      tryCatch(
        {
          saveToHistory()

          fmt <- function(d) {
            if (is.null(d) || is.na(d)) {
              return("")
            }
            safe_format_date(d)
          }

          values$pedData$last_name[i] <- input$ed_last %||% ""
          values$pedData$first_name[i] <- input$ed_first %||% ""
          values$pedData$assigned_at_birth[i] <- input$ed_aab %||% ""
          values$pedData$date_of_birth[i] <- fmt(input$ed_dob)
          values$pedData$date_of_death[i] <- fmt(input$ed_dod)
          values$pedData$deceased[i] <- isTRUE(input$ed_dead)
          values$pedData$age[i] <- input$ed_age %||% ""
          values$pedData$comments[i] <- input$ed_comments %||% ""
          values$pedData$sex[i] <- as.integer(input$ed_gender %||% 0)

          # Calcul automatique de l'âge
          if (!nzchar(values$pedData$age[i]) && nzchar(values$pedData$date_of_birth[i])) {
            values$pedData$age[i] <- calculate_age_text(
              values$pedData$date_of_birth[i],
              if (isTRUE(values$pedData$deceased[i])) {
                values$pedData$date_of_death[i]
              } else {
                ""
              }
            )
          }

          # Mise à jour du sexe dans le pedigree si nécessaire
          if (!is.null(pedigree$ped)) {
            current_sex <- pedtools::getSex(pedigree$ped, id)
            new_sex <- values$pedData$sex[i]

            if (current_sex != new_sex) {
              validation <- canChangeSex(pedigree$ped, id, new_sex)

              if (validation$can) {
                pedigree$ped <- pedtools::setSex(pedigree$ped, ids = id, sex = new_sex)
              } else {
                showNotification(
                  paste("Cannot change sex:", validation$reason),
                  type = "warning",
                  duration = 5
                )
                values$pedData$sex[i] <- current_sex
              }
            }
          }

          # Synchroniser le statut décédé
          if (isTRUE(input$ed_dead)) {
            if (!id %in% styles$deceased) {
              styles$deceased <- unique(c(styles$deceased, id))
            }
          } else {
            styles$deceased <- setdiff(styles$deceased, id)
          }

          removeModal()

          showNotification(
            sprintf("✅ Saved individual %s", id),
            type = "message",
            duration = 2
          )
        },
        error = function(e) {
          showNotification(
            paste("Error saving individual:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # ─────────────────────────────────────────────────────────────────────────
    # RETOUR DE LA FONCTION PUBLIQUE
    # ─────────────────────────────────────────────────────────────────────────

    return(
      list(
        openEditModal = openEditModal
      )
    )
  })
}
