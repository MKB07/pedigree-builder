# Archived R development file
# Original path: 🧩 Versions_Support/Support de test/Version beta TEST/app.R
# Original created: 2025-11-17 15:42:07
# Original modified: 2025-11-18 04:28:25
# Archive rationale: Beta modular application experiment.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# 🔲 LIBRARY ==================================================================
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyjqui)
library(shinyWidgets)
library(fontawesome)
library(htmltools)
library(pedtools)
library(dplyr)
library(DescTools)
library(RColorBrewer)
library(viridisLite)
library(DT)
library(lubridate)

rebuildStyles <- function(styles, phenotypes) {
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
        return(styles)
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

      return(styles)
    },
    error = function(e) {
      warning(paste("Error rebuilding styles:", e$message))
      return(styles)
    }
  )
}
source("R/fonctions.R")

# ️⭕️ USER INTERFACE ===================================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "script.js"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$meta(charset = "UTF-8")
  ),
  titlePanel(
    div(
      icon("dna", class = "fa-lg"),
      "Pedigree Editor v2.1 - With Right-Click Hover",
      style = "display: inline-flex; gap: 10px; align-items: center;"
    )
  ),
  fluidRow(
    wellPanel(
      h4(icon("folder-open"), " Initialize Pedigree"),
      div(
        style = "display: flex; gap: 10px; flex-wrap: wrap; margin-bottom: 1rem;",
        actionButton(
          "btn_create",
          "Create",
          icon = icon("plus"),
          class = "btn-success"
        ),
        actionButton(
          "btn_select",
          "Select Template",
          icon = icon("list"),
          class = "btn-success"
        ),
        actionButton(
          "btn_random",
          "Generate Random",
          icon = icon("shuffle"),
          class = "btn-info"
        ),
        actionButton(
          "btn_undo",
          "Undo",
          icon = icon("rotate-left"),
          class = "btn-info"
        ),
        actionButton(
          "btn_delete",
          "Delete All",
          icon = icon("trash"),
          class = "btn-danger"
        )
      ),
      hr(),
      textInput(
        "titleInput",
        "Pedigree Title:",
        placeholder = "Enter pedigree title",
        width = "100%"
      )
    )
  ),
  div(
    class = "layout",

    # ═══════════════════════════════════════════════════════════════════════════
    # COLONNE GAUCHE: Contrôles
    # ═══════════════════════════════════════════════════════════════════════════

    div(
      wellPanel(
        h4(icon("venus-mars"), " Gender"),
        div(
          class = "btn-group-vertical",
          actionButton(
            "Male",
            "Male",
            icon = icon("mars"),
            class = "btn-default btn-sm"
          ),
          actionButton(
            "Female",
            "Female",
            icon = icon("venus"),
            class = "btn-default btn-sm"
          ),
          actionButton(
            "Unknown",
            "Unknown",
            icon = icon("genderless"),
            class = "btn-default btn-sm"
          ),
          hr(style = "margin: 10px 0;"),
          tags$small(
            class = "text-muted",
            "Assigned at Birth:"
          ),
          actionButton(
            "AFAB",
            "AFAB — Assigned Female at Birth",
            class = "btn-default btn-sm"
          ),
          actionButton(
            "AMAB",
            "AMAB — Assigned Male at Birth",
            class = "btn-default btn-sm"
          ),
          actionButton(
            "UMAB",
            "UMAB — Undetermined at Birth",
            class = "btn-default btn-sm"
          )
        )
      ),
      wellPanel(
        h4(icon("heartbeat"), " Status & Annotations"),
        div(
          class = "btn-group-vertical",
          uiOutput("annotations_ui")
        )
      ),
      wellPanel(
        div(
          class = "phenotype-section",
          h4(
            class = "phenotype-title",
            icon("palette"),
            " Phenotypes"
          ),
          actionButton(
            "btn_create_phenotype",
            "Create New Phenotype",
            icon = icon("plus"),
            class = "btn-primary btn-block btn-sm",
            style = "margin-bottom: 1rem;"
          ),
          uiOutput("phenotypeListUI")
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════════════════════
    # COLONNE CENTRALE: Plot et Table
    # ═══════════════════════════════════════════════════════════════════════════

    div(
      wellPanel(
        div(
          style = "background: #e9ecef; padding: 10px; border-radius: 4px;",
          icon("info-circle"),
          uiOutput("selection_info", inline = TRUE),
          style = "font-size: 14px; color: #495057;"
        )
      ),
      div(
        class = "plot-shell",
        plotOutput(
          "plot",
          width = "100%",
          height = "550px",
          click = "ped_click",
          dblclick = "ped_dblclick",
          hover = hoverOpts(
            id = "ped_hover",
            delay = 50,
            delayType = "throttle"
          )
        ),
        uiOutput("hover_ui"),
        uiOutput("context_menu")
      ),
      wellPanel(
        h4(icon("chart-bar"), " Statistics"),
        uiOutput("pedigree_stats"),
        linkAnalysisUI("link_module")
      ),
      div(
        class = "table-container",
        DTOutput("pedTableDT")
      )
    ),

    # ═══════════════════════════════════════════════════════════════════════════
    # COLONNE DROITE: Ajout de membres & Info
    # ═══════════════════════════════════════════════════════════════════════════

    div(
      wellPanel(
        h4(icon("user"), " Selected Individual"),
        div(
          class = "info-row",
          span(class = "info-label", "ID"),
          span(class = "info-value", textOutput("individual_id", inline = TRUE))
        ),
        div(
          class = "info-row",
          span(class = "info-label", "Name"),
          span(class = "info-value", textOutput("individual_name", inline = TRUE))
        ),
        div(
          class = "info-row",
          span(class = "info-label", "Sex"),
          span(class = "info-value", textOutput("individual_sex", inline = TRUE))
        ),
        div(
          class = "info-row",
          span(class = "info-label", "Status"),
          span(class = "info-value", textOutput("individual_status", inline = TRUE))
        )
      ),
      wellPanel(
        h4(icon("users"), " Add Family Members"),
        div(
          class = "btn-group-vertical",
          tags$div(
            style = "margin-bottom: 5px; font-weight: 600; font-size: 12px; color: #666; text-transform: uppercase;",
            "Parents & Children"
          ),
          actionButton(
            "addParents",
            "Add Parents",
            icon = icon("user-plus"),
            class = "btn-success btn-sm"
          ),
          br(),
          uiOutput("partner_selection_ui"),
          div(
            class = "control-panel",
            div(
              class = "control-section",
              div(class = "section-title", "➕ Add Relatives"),
              div(
                class = "tab-buttons",
                tags$button(
                  id = "tab_children",
                  class = "tab-btn active",
                  "👶 Children",
                  onclick = "Shiny.setInputValue('active_tab', 'children'); $('.tab-btn').removeClass('active'); $(this).addClass('active');"
                ),
                tags$button(
                  id = "tab_siblings",
                  class = "tab-btn",
                  "👥 Siblings",
                  onclick = "Shiny.setInputValue('active_tab', 'siblings'); $('.tab-btn').removeClass('active'); $(this).addClass('active');"
                ),
                tags$button(
                  id = "tab_half_siblings",
                  class = "tab-btn",
                  "👥½ Half",
                  onclick = "Shiny.setInputValue('active_tab', 'half_siblings'); $('.tab-btn').removeClass('active'); $(this).addClass('active');"
                )
              ),
              uiOutput("relatives_content")
            )
          ),
          hr(style = "margin: 10px 0;"),
          tags$div(
            style = "margin-bottom: 5px; font-weight: 600; font-size: 12px; color: #666; text-transform: uppercase;",
            "Twins & Triplets"
          ),
          uiOutput("twins_triplets_ui")
        )
      ),
      wellPanel(
        h4(icon("download"), " Export"),
        downloadButton(
          "downloadPNG",
          "Download PNG",
          icon = icon("image"),
          class = "btn-default btn-block btn-sm"
        ),
        downloadButton(
          "downloadPED",
          "Download PED",
          icon = icon("file-export"),
          class = "btn-default btn-block btn-sm"
        )
      )
    )
  ),
  tags$footer(
    style = "text-align: center; padding: 2rem 0; color: #9ca3af; font-size: 0.875rem;",
    tags$p(
      "Pedigree Editor v2.1 © 2024 | Built with ",
      tags$a(
        href = "https://shiny.rstudio.com/",
        target = "_blank",
        "Shiny"
      ),
      " & ",
      tags$a(
        href = "https://cran.r-project.org/package=pedtools",
        target = "_blank",
        "pedtools"
      ),
      tags$br(),
      tags$em("✨ Now with right-click context menu and hover tooltips!")
    )
  )

  # 【 END 】
)

# ⭕️ SERVER ===================================================================
server <- function(input, output, session) {
  ## VALEURS RÉACTIVES --------------

  pedigree <- reactiveValues(ped = NULL, title = NULL)
  values <- reactiveValues(pedData = NULL)
  centersRV <- reactiveVal(NULL)
  plot_dimensions <- reactiveVal(NULL)
  sel <- reactiveVal(character(0))

  styles <- reactiveValues(
    deceased = NULL,
    proband = NULL,
    adopted = NULL,
    fill = character(0),
    hatched = character(0),
    col = character(0),
    lty = list()
  )

  miscarriage <- reactiveVal(character(0))
  carrier <- reactiveVal(character(0))
  starred <- reactiveVal(character(0))
  text_annotations <- reactiveVal(list())

  twins_df <- reactiveVal(
    data.frame(
      id1 = character(),
      id2 = character(),
      code = integer(),
      stringsAsFactors = FALSE
    )
  )

  phenotypes <- reactiveValues(
    list = list(),
    assign = list()
  )

  pheno_editing <- reactiveVal(NULL)

  history <- reactiveValues(
    stack = list(),
    maxSize = 20
  )

  num_relatives <- reactiveVal(1)
  sex_selected <- reactiveVal(1)
  twin_type_selected <- reactiveVal(2)
  twin_sex_selected <- reactiveVal(1)
  active_tab <- reactiveVal("children")

  debounce_state <- reactiveValues(
    last_action = NULL,
    timestamp = NULL
  )

  show_context_menu <- reactiveVal(FALSE)
  context_menu_data <- reactiveVal(NULL)

  # ───────────────────────────────────────────────────────────────────────────────

  ## FONCTIONS HELPER INTERNES -----------

  saveToHistoryInternal <- function() {
    tryCatch(
      {
        st <- list(
          ped = pedigree$ped,
          title = pedigree$title,
          pedData = if (!is.null(values$pedData)) {
            as.data.frame(values$pedData, stringsAsFactors = FALSE)
          } else {
            NULL
          },
          deceased = styles$deceased,
          proband = styles$proband,
          adopted = styles$adopted,
          miscarriage = miscarriage(),
          carrier = carrier(),
          starred = starred(),
          text_annotations = text_annotations(),
          twins_df = twins_df(),
          phenotypes_list = if (length(phenotypes$list) > 0) {
            lapply(phenotypes$list, function(x) as.list(x))
          } else {
            list()
          },
          phenotypes_assign = if (length(phenotypes$assign) > 0) {
            lapply(phenotypes$assign, function(x) x)
          } else {
            list()
          },
          timestamp = Sys.time()
        )

        history$stack <- c(list(st), history$stack)

        if (length(history$stack) > history$maxSize) {
          history$stack <- history$stack[1:history$maxSize]
        }

        return(TRUE)
      },
      error = function(e) {
        showNotification(
          paste("Error saving to history:", e$message),
          type = "error",
          duration = 5
        )
        return(FALSE)
      }
    )
  }

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

  cleanOrphanPhenotypesInternal <- function() {
    if (is.null(pedigree$ped)) {
      return()
    }

    current_ids <- labels(pedigree$ped)

    for (pheno_name in names(phenotypes$assign)) {
      assigned_ids <- phenotypes$assign[[pheno_name]]
      valid_ids <- intersect(assigned_ids, current_ids)

      if (length(valid_ids) != length(assigned_ids)) {
        phenotypes$assign[[pheno_name]] <- valid_ids
      }
    }

    rebuildStylesInternal()
  }

  canPerformAction <- function(action_name, min_interval = 0.3) {
    current_time <- Sys.time()

    if (is.null(debounce_state$last_action) || is.null(debounce_state$timestamp)) {
      debounce_state$last_action <- action_name
      debounce_state$timestamp <- current_time
      return(TRUE)
    }

    if (debounce_state$last_action == action_name) {
      time_diff <- as.numeric(difftime(current_time, debounce_state$timestamp, units = "secs"))

      if (time_diff < min_interval) {
        return(FALSE)
      }
    }

    debounce_state$last_action <- action_name
    debounce_state$timestamp <- current_time
    return(TRUE)
  }

  # ───────────────────────────────────────────────────────────────────────────────
  # Connectez le module avec vos reactives existants:
  link_selection <- linkAnalysisServer(
    "link_module",
    ped_reactive = reactive(pedigree$ped),
    selected_ids_reactive = sel # votre reactive de sélection existant
  )

  # OPTIONNEL: Synchroniser la sélection du module avec celle du plot
  observe({
    ids <- link_selection()
    if (!is.null(ids$idA) && !is.null(ids$idB)) {
      # Vous pouvez utiliser ces IDs si nécessaire
      # Par exemple, les mettre en surbrillance sur le plot
    }
  })
  ## SYNCHRONISATION UI -----------
  observeEvent(input$sex_selected, {
    sex_selected(input$sex_selected)
  })

  observeEvent(input$active_tab, {
    active_tab(input$active_tab)
  })

  observeEvent(input$twin_type_selected, {
    twin_type_selected(input$twin_type_selected)
  })

  observeEvent(input$twin_sex_selected, {
    twin_sex_selected(input$twin_sex_selected)
  })

  observeEvent(input$increase_relatives, {
    current <- num_relatives()
    if (current < 5) {
      num_relatives(current + 1)
    }
  })

  observeEvent(input$decrease_relatives, {
    current <- num_relatives()
    if (current > 1) {
      num_relatives(current - 1)
    }
  })

  output$num_relatives_display <- renderText({
    as.character(num_relatives())
  })

  observe({
    pedigree$title <- sanitize_title(input$titleInput)
  })

  observe({
    if (length(history$stack) > 0) {
      shinyjs::enable("btn_undo")
    } else {
      shinyjs::disable("btn_undo")
    }
  })

  # ───────────────────────────────────────────────────────────────────────────────

  ## TOOLTIP HOVER -------
  output$hover_ui <- renderUI({
    req(pedigree$ped, input$ped_hover)

    if (show_context_menu()) {
      return(NULL)
    }

    h <- input$ped_hover
    if (is.null(h$domain) || is.null(h$range) || is.null(h$x) || is.null(h$y)) {
      return(NULL)
    }

    centers_df <- centersRV()
    if (is.null(centers_df) || !nrow(centers_df)) {
      return(NULL)
    }

    hit <- nearPoints(centers_df, h, xvar = "x", yvar = "y", threshold = 25, maxpoints = 1)
    if (!nrow(hit)) {
      return(NULL)
    }

    id_plot <- hit$id_plot[1]
    lab <- labels(pedigree$ped)[id_plot]

    sx <- pedtools::getSex(pedigree$ped, lab)
    sxL <- c("Unknown", "Male", "Female")[match(sx, c(0, 1, 2), nomatch = 1)]

    pars <- tryCatch(pedtools::parents(pedigree$ped, lab), error = function(e) c(NA, NA))
    ch <- tryCatch(pedtools::children(pedigree$ped, lab), error = function(e) character(0))

    cssx <- h$coords_css$x %||% NA_real_
    cssy <- h$coords_css$y %||% NA_real_

    if (is.na(cssx) || is.na(cssy)) {
      return(NULL)
    }

    left <- as.integer(round(cssx + 15))
    top <- as.integer(round(cssy - 15))

    div(
      class = "hover-tooltip",
      style = sprintf("left: %dpx; top: %dpx;", left, top),
      HTML(sprintf(
        "<b>Individual: %s</b><br/>
         Sex: %s<br/>
         Father: %s<br/>
         Mother: %s<br/>
         Children: %s",
        lab,
        sxL,
        if (!is.na(pars[1])) pars[1] else "unknown",
        if (!is.na(pars[2])) pars[2] else "unknown",
        if (length(ch) > 0) paste(ch, collapse = ", ") else "none"
      ))
    )
  })

  # ───────────────────────────────────────────────────────────────────────────────

  ## MENU CONTEXTUEL AVEC DÉTECTION ROBUSTE -------

  observeEvent(input$ctx_request, {
    req(pedigree$ped)

    ctx <- input$ctx_request
    target_id <- NULL

    cat("📍 Context menu requested at", ctx$normX, ctx$normY, "\n")

    if (length(sel()) > 0) {
      target_id <- sel()[1]
      cat("✅ Using selected:", target_id, "\n")
    } else {
      centers_df <- centersRV()

      if (!is.null(centers_df) && nrow(centers_df) > 0) {
        id_plot <- find_nearest_individual(
          centers_df,
          ctx$normX,
          ctx$normY,
          max_distance = 0.15
        )

        if (!is.na(id_plot)) {
          target_id <- labels(pedigree$ped)[id_plot]
          sel(target_id)
          cat("✅ Detected:", target_id, "\n")
        }
      }
    }

    if (is.null(target_id)) {
      showNotification(
        "⚠️ No individual found. Click closer to a symbol (○ or □)",
        type = "warning",
        duration = 3
      )
      return()
    }

    is_carrier <- target_id %in% carrier()
    is_starred <- target_id %in% starred()

    sx <- pedtools::getSex(pedigree$ped, target_id)
    sxL <- c("Unknown", "Male", "Female")[match(sx, c(0, 1, 2), nomatch = 1)]
    pars <- tryCatch(pedtools::parents(pedigree$ped, target_id), error = function(e) c(NA, NA))
    ch <- tryCatch(pedtools::children(pedigree$ped, target_id), error = function(e) character(0))

    context_menu_data(list(
      id = target_id,
      sex = sxL,
      father = if (!is.na(pars[1])) pars[1] else "unknown",
      mother = if (!is.na(pars[2])) pars[2] else "unknown",
      children = if (length(ch) > 0) paste(ch, collapse = ", ") else "none",
      menuX = as.integer(ctx$menuX %||% 100),
      menuY = as.integer(ctx$menuY %||% 100),
      isCarrier = is_carrier,
      isStarred = is_starred
    ))

    show_context_menu(TRUE)
    cat("✅ Menu opened for:", target_id, "\n")
  })

  observeEvent(input$ctx_close_request, {
    show_context_menu(FALSE)
    # cat("❌ Menu closed\n")  # Commenté pour éviter spam
  })

  output$context_menu <- renderUI({
    if (!show_context_menu()) {
      return(NULL)
    }

    data <- context_menu_data()
    req(data)

    div(
      id = "context_menu_div",
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
        "ctx_carrier",
        if (data$isCarrier) "✓ Carrier (dot)" else "○ Carrier (dot)",
        class = "btn btn-link"
      ),
      actionButton(
        "ctx_starred",
        if (data$isStarred) "✓ Starred (*)" else "○ Starred (*)",
        class = "btn btn-link"
      ),
      actionButton(
        "ctx_text_annot",
        "📝 Add Text Annotation...",
        class = "btn btn-link"
      ),
      tags$hr(),
      actionButton("ctx_close", "✖ Close", class = "btn btn-link btn-close-menu")
    )
  })

  observeEvent(input$ctx_close, {
    show_context_menu(FALSE)
  })

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
        "annot_position",
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
        "annot_text",
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
            "btn_clear_all_annot",
            "Clear All",
            class = "btn btn-warning"
          )
        },
        modalButton("Cancel"),
        actionButton(
          "btn_add_annot_confirm",
          "Add",
          class = "btn btn-primary"
        )
      )
    ))

    session$userData$temp_annot_id <- id
  })

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

  # ───────────────────────────────────────────────────────────────────────────────

  ## VISUALISATION DU PEDIGREE --------
  output$plot <- renderPlot({
    req(pedigree$ped)

    tryCatch(
      {
        ped <- pedigree$ped
        selected_ids <- sel()

        outline_col <- if (length(selected_ids) > 0) {
          setNames("#667EEA", selected_ids)
        } else {
          NULL
        }

        fill_col <- if (length(selected_ids) > 0) {
          setNames("#e3e8ff", selected_ids)
        } else {
          NULL
        }

        misc_ids <- miscarriage()
        dec_ids <- c(styles$deceased, labels(ped)[values$pedData$deceased == TRUE])
        dec_ids <- unique(dec_ids)
        prob_id <- styles$proband
        carr_ids <- carrier()
        star_ids <- starred()
        txt_annot <- text_annotations()
        twins <- twins_df()

        al <- pedtools:::.pedAlignment(
          ped,
          miscarriage = misc_ids,
          twins = if (nrow(twins) > 0) twins else NULL
        )

        labs <- build_labs(ped)
        annotations <- prepare_annotation_data(ped, values$pedData)

        col_list <- list()

        if (length(styles$col) > 0) {
          for (id in names(styles$col)) {
            col_val <- styles$col[id]
            if (!col_val %in% names(col_list)) {
              col_list[[col_val]] <- character(0)
            }
            col_list[[col_val]] <- c(col_list[[col_val]], id)
          }
        }

        if (!is.null(outline_col)) {
          for (id in names(outline_col)) {
            col_val <- outline_col[id]
            if (!col_val %in% names(col_list)) {
              col_list[[col_val]] <- character(0)
            }
            col_list[[col_val]] <- c(col_list[[col_val]], id)
          }
        }

        an <- pedtools:::.pedAnnotation(
          ped,
          labs = labs,
          fill = if (length(styles$fill) > 0 || length(fill_col) > 0) {
            c(styles$fill, fill_col)
          } else {
            NULL
          },
          col = if (length(col_list) > 0) col_list else list(),
          lty = if (length(styles$lty) > 0) styles$lty else list(),
          deceased = dec_ids,
          proband = prob_id,
          carrier = carr_ids,
          starred = star_ids,
          textAnnot = txt_annot,
          hatched = if (length(styles$hatched) > 0) styles$hatched else NULL
        )

        sc <- pedtools:::.pedScaling(
          al, an,
          cex = 1.4,
          symbolsize = 1.2,
          margins = c(6, 3, 7, 3)
        )

        plot_xlim <- range(al$xall)
        plot_ylim <- range(al$yall)

        plot_dimensions(list(
          xlim = plot_xlim,
          ylim = plot_ylim,
          width = diff(plot_xlim),
          height = diff(plot_ylim),
          boxw = sc$boxw,
          boxh = sc$boxh
        ))

        pedtools:::drawPed(al, an, sc)
        pedtools:::.annotatePed(al, an, sc)
        draw_custom_annotations(al, annotations, sc)

        adopted_ids <- styles$adopted %||% character(0)

        if (length(adopted_ids) > 0) {
          labs_all <- labels(ped)
          lab_by_plot <- labs_all[al$plotord]

          sex_all <- pedtools::getSex(ped, labs_all)
          shape_by_lab <- setNames(
            ifelse(sex_all == 1, "square",
              ifelse(sex_all == 2, "circle", "diamond")
            ),
            labs_all
          )

          cx <- al$xall + sc$boxw / 2
          cy <- al$yall + sc$boxh / 2
          df_sel <- data.frame(
            lab = lab_by_plot,
            x = cx,
            y = cy,
            stringsAsFactors = FALSE
          )

          for (id_adopt in adopted_ids) {
            if (id_adopt %in% lab_by_plot) {
              rows_adopt <- df_sel[df_sel$lab == id_adopt, , drop = FALSE]

              if (nrow(rows_adopt) > 0) {
                for (k in seq_len(nrow(rows_adopt))) {
                  .draw_adoption_brackets(
                    shape = shape_by_lab[[id_adopt]] %||% "square",
                    x = rows_adopt$x[k],
                    y = rows_adopt$y[k],
                    w = sc$boxw,
                    h = sc$boxh,
                    lwd = ADOPTION_BRACKET_CONFIG$line_width,
                    col = ADOPTION_BRACKET_CONFIG$color
                  )
                }
              }
            }
          }
        }

        if (length(selected_ids) > 0) {
          markers <- c()

          if (selected_ids[1] %in% misc_ids) markers <- c(markers, "△ Miscarriage")
          if (selected_ids[1] %in% dec_ids) markers <- c(markers, "✝ Deceased")
          if (selected_ids[1] %in% prob_id) markers <- c(markers, "➤ Proband")
          if (selected_ids[1] %in% carr_ids) markers <- c(markers, "● Carrier")
          if (selected_ids[1] %in% star_ids) markers <- c(markers, "★ Starred")

          if (nrow(twins) > 0) {
            twin_rows <- twins[twins$id1 == selected_ids[1] | twins$id2 == selected_ids[1], ]
            if (nrow(twin_rows) > 0) {
              markers <- c(markers, "👥 Twin")
            }
          }

          title_text <- if (length(markers) > 0) {
            sprintf("Selected: %s (%s)", selected_ids[1], paste(markers, collapse = ", "))
          } else {
            sprintf("Selected: %s", selected_ids[1])
          }

          title(main = title_text, col.main = "#667eea", cex.main = 1.3)
        } else {
          ttl <- sanitize_title(pedigree$title)
          if (nzchar(ttl)) {
            old_xpd <- par("xpd")
            par(xpd = NA)

            usr <- par("usr")
            x_center <- mean(usr[1:2])
            y_height <- diff(usr[3:4])
            y_title <- usr[4] + y_height * 0.15

            DescTools::BoxedText(
              x = x_center,
              y = y_title,
              labels = ttl,
              bg = "#667EEA",
              col = "white",
              border = "#4C51BF",
              cex = 1.2,
              xpad = 1.8,
              ypad = 1.3,
              font = 2
            )

            par(xpd = old_xpd)
          }
        }

        centersRV(data.frame(
          x = al$xall + sc$boxw / 2,
          y = al$yall + sc$boxh / 2,
          id_plot = al$plotord,
          stringsAsFactors = FALSE
        ))
      },
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error rendering plot:\n", e$message), col = "red", cex = 1.2)
        message("Plot error: ", e$message)
      }
    )
  })

  # ───────────────────────────────────────────────────────────────────────────────

  ## INTERACTIONS - CLICS -----------
  observeEvent(input$ped_click, {
    req(pedigree$ped, input$ped_click)

    if (!canPerformAction("plot_click", 0.2)) {
      return()
    }

    tryCatch(
      {
        hit_idx <- nearest_by_centers(centersRV(), input$ped_click, px_thresh = 30)

        if (is.na(hit_idx)) {
          if (length(sel()) > 0) {
            sel(character(0))
            showNotification("Deselected", type = "message", duration = 1.5)
          }
          return(invisible(NULL))
        }

        hit_lbl <- labels(pedigree$ped)[hit_idx]

        if (length(sel()) == 1 && sel()[1] == hit_lbl) {
          sel(character(0))
          showNotification("Deselected", type = "message", duration = 1.5)
        } else {
          sel(hit_lbl)

          info_text <- sprintf("Selected: %s", hit_lbl)

          if (!is.null(values$pedData)) {
            row <- values$pedData[values$pedData$id == hit_lbl, , drop = FALSE]
            if (nrow(row) > 0) {
              ln <- row$last_name
              fn <- row$first_name
              if (nzchar(ln) || nzchar(fn)) {
                name_parts <- c()
                if (nzchar(ln)) name_parts <- c(name_parts, toupper(ln))
                if (nzchar(fn)) name_parts <- c(name_parts, tools::toTitleCase(fn))
                info_text <- sprintf("Selected: %s (%s)", hit_lbl, paste(name_parts, collapse = " "))
              }
            }
          }

          showNotification(info_text, type = "message", duration = 2)
        }
      },
      error = function(e) {
        message("Click error: ", e$message)
      }
    )
  })

  observeEvent(input$ped_dblclick, {
    req(pedigree$ped, values$pedData, input$ped_dblclick)

    if (!canPerformAction("plot_dblclick", 0.5)) {
      return()
    }

    tryCatch(
      {
        hit_idx <- nearest_by_centers(centersRV(), input$ped_dblclick, px_thresh = 25)

        if (is.na(hit_idx)) {
          return(invisible(NULL))
        }

        id <- labels(pedigree$ped)[hit_idx]
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
              textInput("ed_last", "Last Name", value = row$last_name)
            ),
            column(
              4,
              textInput("ed_first", "First Name", value = row$first_name)
            ),
            column(
              4,
              selectInput(
                "ed_gender",
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
                "ed_dob",
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
                "ed_dod",
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
              checkboxInput("ed_dead", "Deceased", value = isTRUE(row$deceased))
            ),
            column(
              2,
              textInput("ed_age", "Age", value = row$age)
            )
          ),
          fluidRow(
            column(
              6,
              textInput(
                "ed_aab",
                "Assigned at Birth (AAB)",
                value = row$assigned_at_birth,
                placeholder = "e.g., AFAB, AMAB"
              )
            ),
            column(6, NULL)
          ),
          textAreaInput(
            "ed_comments",
            "Comments",
            value = row$comments,
            rows = 2,
            placeholder = "Additional notes..."
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              "ed_save",
              "Save Changes",
              class = "btn btn-primary",
              icon = icon("save"),
              onclick = sprintf(
                'Shiny.setInputValue("ed_target", "%s", {priority:"event"})',
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
  })

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
        saveToHistoryInternal()

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

  # ────────────────────────────────────────────────────────────────────────────────────────
  ## CHANGEMENT DE SEXE ----------------
  observeEvent(input$Male, {
    req(pedigree$ped, length(sel()) > 0)

    if (!canPerformAction("set_male", 0.3)) {
      return()
    }

    id <- sel()[1]
    current_sex <- pedtools::getSex(pedigree$ped, id)

    if (current_sex == 1) {
      showNotification("ℹ️ Already male", type = "message", duration = 3)
      return()
    }

    new_ped <- tryCatch(
      {
        safeSexChange(pedigree$ped, id, 1)
      },
      error = function(e) {
        showNotification(paste("❌", e$message), type = "error", duration = 5)
        NULL
      }
    )

    if (!is.null(new_ped)) {
      saveToHistoryInternal()
      pedigree$ped <- new_ped

      if (!is.null(values$pedData)) {
        idx <- which(values$pedData$id == id)
        if (length(idx) > 0) {
          values$pedData$sex[idx] <- 1
        }
      }

      showNotification(sprintf("✅ %s is now male", id), type = "message", duration = 2)
    }
  })

  observeEvent(input$Female, {
    req(pedigree$ped, length(sel()) > 0)

    if (!canPerformAction("set_female", 0.3)) {
      return()
    }

    id <- sel()[1]
    current_sex <- pedtools::getSex(pedigree$ped, id)

    if (current_sex == 2) {
      showNotification("ℹ️ Already female", type = "message", duration = 3)
      return()
    }

    new_ped <- tryCatch(
      {
        safeSexChange(pedigree$ped, id, 2)
      },
      error = function(e) {
        showNotification(paste("❌", e$message), type = "error", duration = 5)
        NULL
      }
    )

    if (!is.null(new_ped)) {
      saveToHistoryInternal()
      pedigree$ped <- new_ped

      if (!is.null(values$pedData)) {
        idx <- which(values$pedData$id == id)
        if (length(idx) > 0) {
          values$pedData$sex[idx] <- 2
        }
      }

      showNotification(sprintf("✅ %s is now female", id), type = "message", duration = 2)
    }
  })

  observeEvent(input$Unknown, {
    req(pedigree$ped, length(sel()) > 0)

    if (!canPerformAction("set_unknown", 0.3)) {
      return()
    }

    id <- sel()[1]
    current_sex <- pedtools::getSex(pedigree$ped, id)

    if (current_sex == 0) {
      showNotification("ℹ️ Already unknown sex", type = "message", duration = 3)
      return()
    }

    partners <- getPartners(pedigree$ped, id)
    if (length(partners) > 0) {
      showNotification(
        sprintf("❌ Cannot change: individual has partner(s): %s", paste(partners, collapse = ", ")),
        type = "error",
        duration = 5
      )
      return()
    }

    new_ped <- tryCatch(
      {
        safeSexChange(pedigree$ped, id, 0)
      },
      error = function(e) {
        showNotification(paste("❌", e$message), type = "error", duration = 5)
        NULL
      }
    )

    if (!is.null(new_ped)) {
      saveToHistoryInternal()
      pedigree$ped <- new_ped

      if (!is.null(values$pedData)) {
        idx <- which(values$pedData$id == id)
        if (length(idx) > 0) {
          values$pedData$sex[idx] <- 0
        }
      }

      showNotification(sprintf("✅ Sex set to unknown for %s", id), type = "message", duration = 2)
    }
  })
  # ───────────────────────────────────────────────────────────────────────────────

  ### ASSIGNED AT BIRTH (AAB) ---------

  setAAB <- function(id, aab_value) {
    req(values$pedData)

    tryCatch(
      {
        idx <- which(values$pedData$id == id)

        if (length(idx) == 0) {
          showNotification(
            sprintf("❌ Individual %s not found in data", id),
            type = "error",
            duration = 3
          )
          return(FALSE)
        }

        current_aab <- toupper(trimws(values$pedData$assigned_at_birth[idx]))
        if (current_aab == aab_value) {
          showNotification(
            sprintf("ℹ️ %s is already marked as %s", id, aab_value),
            type = "message",
            duration = 2
          )
          return(FALSE)
        }

        saveToHistoryInternal()

        values$pedData$assigned_at_birth[idx] <- aab_value

        showNotification(
          sprintf("✅ %s marked as %s", id, aab_value),
          type = "message",
          duration = 2
        )

        return(TRUE)
      },
      error = function(e) {
        showNotification(
          paste("Error setting AAB:", e$message),
          type = "error",
          duration = 5
        )
        return(FALSE)
      }
    )
  }

  observeEvent(input$AFAB, {
    req(values$pedData, length(sel()) > 0)
    if (!canPerformAction("set_afab", 0.3)) {
      return()
    }
    setAAB(sel()[1], "AFAB")
  })

  observeEvent(input$AMAB, {
    req(values$pedData, length(sel()) > 0)
    if (!canPerformAction("set_amab", 0.3)) {
      return()
    }
    setAAB(sel()[1], "AMAB")
  })

  observeEvent(input$UMAB, {
    req(values$pedData, length(sel()) > 0)
    if (!canPerformAction("set_umab", 0.3)) {
      return()
    }
    setAAB(sel()[1], "UMAB")
  })

  # ───────────────────────────────────────────────────────────────────────────────

  ## UI DYNAMIQUE - ANNOTATIONS -------------

  output$annotations_ui <- renderUI({
    req(pedigree$ped, length(sel()) > 0)

    id <- sel()[1]
    is_deceased <- id %in% c(styles$deceased, miscarriage())
    is_proband <- id %in% styles$proband
    is_miscarriage <- id %in% miscarriage()

    has_children <- length(tryCatch(
      pedtools::children(pedigree$ped, id),
      error = function(e) character(0)
    )) > 0

    div(
      class = "control-panel",
      div(
        class = "control-section",
        div(class = "section-title", "🏷️ Annotations"),
        actionButton(
          "btn_toggle_deceased",
          if (is_deceased && !is_miscarriage) "✝ Deceased (ON)" else "✝ Deceased (OFF)",
          class = if (is_deceased && !is_miscarriage) "toggle-btn active" else "toggle-btn",
          style = "width: 100%;"
        ),
        tags$div(
          style = "margin-top: 10px;",
          actionButton(
            "btn_toggle_proband",
            if (is_proband) "➤ Proband (ON)" else "➤ Proband (OFF)",
            class = if (is_proband) "toggle-btn active" else "toggle-btn",
            style = "width: 100%;"
          )
        ),
        tags$div(
          style = "margin-top: 10px;",
          actionButton(
            "Adopted",
            if (id %in% (styles$adopted %||% character(0))) "[ ] Adopted (ON)" else "[ ] Adopted (OFF)",
            class = if (id %in% (styles$adopted %||% character(0))) "toggle-btn active" else "toggle-btn",
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
              "btn_toggle_miscarriage",
              if (is_miscarriage) "△ Miscarriage (ON)" else "△ Miscarriage (OFF)",
              class = if (is_miscarriage) "toggle-btn active" else "toggle-btn",
              style = "width: 100%;"
            )
          }
        )
      )
    )
  })

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

    saveToHistoryInternal()

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

  observeEvent(input$btn_toggle_proband, {
    req(pedigree$ped, length(sel()) > 0)

    id <- sel()[1]
    current_proband <- styles$proband

    saveToHistoryInternal()

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

  observeEvent(input$Adopted, {
    req(pedigree$ped, length(sel()) > 0)

    id <- sel()[1]
    current_adopted <- styles$adopted %||% character(0)

    saveToHistoryInternal()

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

    saveToHistoryInternal()

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

  # ───────────────────────────────────────────────────────────────────────────────

  ## INITIALISATION PEDIGREE - TEMPLATES

  observeEvent(input$btn_select, {
    if (!canPerformAction("select_modal", 0.5)) {
      return()
    }

    showModal(modalDialog(
      title = tagList(icon("list"), tags$strong(" Select Pedigree Template")),
      easyClose = TRUE,
      size = "l",
      fluidRow(
        column(
          5,
          textInput(
            "m_title",
            "Pedigree Title:",
            value = pedigree$title,
            placeholder = "Enter a title"
          ),
          selectInput(
            "m_choice",
            "Choose a template:",
            choices = c("--- Select a template ---" = "", names(pedigree_list))
          ),
          tags$small(
            class = "text-muted",
            icon("info-circle"),
            " Select a template to preview it on the right."
          )
        ),
        column(
          7,
          div(
            style = "padding: 8px; background: #f9fafb; border-radius: 8px; border: 1px solid #e5e7eb;",
            plotOutput("m_preview", height = "280px")
          )
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "m_confirm",
          "Create Pedigree",
          class = "btn btn-primary",
          icon = icon("check")
        )
      )
    ))
  })

  observeEvent(input$btn_create, {
    if (!canPerformAction("create_modal", 0.5)) {
      return()
    }

    showModal(modalDialog(
      title = tagList(icon("list"), tags$strong(" Select Pedigree Template")),
      easyClose = TRUE,
      size = "l",
      fluidRow(
        column(
          5,
          textInput(
            "m_title",
            "Pedigree Title:",
            value = pedigree$title,
            placeholder = "Enter a title"
          ),
          selectInput(
            "m_choice",
            "Choose a template:",
            choices = c("--- Select a template ---" = "", names(pedigree_list))
          )
        ),
        column(
          7,
          div(
            style = "padding: 8px; background: #f9fafb; border-radius: 8px;",
            plotOutput("m_preview", height = "280px")
          )
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "m_confirm",
          "Create Pedigree",
          class = "btn btn-primary",
          icon = icon("check")
        )
      )
    ))
  })

  chosen_preview <- reactiveVal(NULL)

  observeEvent(input$m_choice, {
    req(nzchar(input$m_choice))

    ped <- tryCatch(
      {
        pedigree_list[[input$m_choice]]()
      },
      error = function(e) {
        showNotification(
          paste("Error loading template:", e$message),
          type = "error",
          duration = 5
        )
        NULL
      }
    )

    chosen_preview(ped)
  })

  output$m_preview <- renderPlot(
    {
      if (is.null(chosen_preview())) {
        plot.new()
        text(0.5, 0.5, "Select a template to preview", cex = 1.2, col = "#9ca3af")
        return()
      }

      tryCatch(
        {
          ped <- relabel_generations(chosen_preview())
          plot(ped, cex = 1.2, margins = c(2, 2, 4, 2))

          ttl <- sanitize_title(input$m_title)
          if (nzchar(ttl)) {
            mtext(ttl, side = 3, line = 1, cex = 1.1, font = 2, col = "#667EEA")
          }
        },
        error = function(e) {
          plot.new()
          text(0.5, 0.5, "Preview unavailable", col = "#ef4444", cex = 1.2)
        }
      )
    },
    bg = "white"
  )

  observeEvent(input$m_confirm, {
    req(chosen_preview())

    if (!canPerformAction("confirm_template", 0.5)) {
      return()
    }

    tryCatch(
      {
        if (!is.null(pedigree$ped)) {
          saveToHistoryInternal()
        }

        new_ped <- relabel_generations(chosen_preview())

        validation <- validatePedigree(new_ped)
        if (!validation$valid) {
          showNotification(
            paste("Invalid pedigree:", validation$message),
            type = "error",
            duration = 5
          )
          return()
        }

        pedigree$ped <- new_ped
        pedigree$title <- sanitize_title(input$m_title)
        values$pedData <- makePedData(pedigree$ped)

        styles$deceased <- NULL
        styles$proband <- NULL
        styles$adopted <- NULL
        styles$fill <- character(0)
        styles$hatched <- character(0)
        styles$col <- character(0)
        styles$lty <- list()

        miscarriage(character(0))
        carrier(character(0))
        starred(character(0))
        text_annotations(list())
        twins_df(data.frame(
          id1 = character(),
          id2 = character(),
          code = integer(),
          stringsAsFactors = FALSE
        ))

        phenotypes$list <- list()
        phenotypes$assign <- list()

        sel(character(0))

        updateTextInput(session, "titleInput", value = pedigree$title)

        removeModal()

        showNotification(
          "✓ Pedigree created successfully!",
          type = "message",
          duration = 3
        )
      },
      error = function(e) {
        showNotification(
          paste("Error creating pedigree:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  # ───────────────────────────────────────────────────────────────────────────────

  ## PEDIGREE ALÉATOIRE --------

  random_ped <- reactiveVal(NULL)

  observeEvent(input$btn_random, {
    if (!canPerformAction("random_modal", 0.5)) {
      return()
    }

    ped <- tryCatch(
      {
        pedtools::randomPed(
          n = sample(3:10, 1),
          maxDirectGap = Inf,
          selfing = FALSE
        )
      },
      error = function(e) {
        showNotification(
          paste("Error generating random pedigree:", e$message),
          type = "error",
          duration = 5
        )
        NULL
      }
    )

    if (!is.null(ped)) {
      random_ped(ped)

      showModal(modalDialog(
        title = tagList(icon("shuffle"), tags$strong(" Generate Random Pedigree")),
        size = "l",
        easyClose = TRUE,
        div(
          style = "display: flex; gap: 10px; align-items: center; margin-bottom: 8px;",
          textInput(
            "r_title",
            NULL,
            placeholder = "Enter a title (optional)",
            width = "100%"
          ),
          actionButton(
            "r_reroll",
            "Reroll",
            class = "btn btn-secondary",
            icon = icon("sync")
          )
        ),
        div(
          style = "padding: 8px; background: #f9fafb; border-radius: 8px; border: 1px solid #e5e7eb;",
          plotOutput("r_preview", height = "300px")
        ),
        tags$small(
          class = "text-muted",
          style = "display: block; margin-top: 8px;",
          icon("info-circle"),
          " Click 'Reroll' to generate a different random pedigree."
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "r_confirm",
            "Use This Pedigree",
            class = "btn btn-primary",
            icon = icon("check")
          )
        )
      ))
    }
  })

  observeEvent(input$r_reroll, {
    if (!canPerformAction("reroll", 0.5)) {
      return()
    }

    ped <- tryCatch(
      {
        pedtools::randomPed(
          n = sample(3:10, 1),
          maxDirectGap = Inf,
          selfing = FALSE
        )
      },
      error = function(e) {
        showNotification(
          paste("Error generating random pedigree:", e$message),
          type = "error",
          duration = 5
        )
        NULL
      }
    )

    if (!is.null(ped)) {
      random_ped(ped)

      showNotification(
        "🎲 New random pedigree generated",
        type = "message",
        duration = 2
      )
    }
  })

  output$r_preview <- renderPlot(
    {
      req(random_ped())

      tryCatch(
        {
          ped <- relabel_generations(random_ped())
          plot(ped, cex = 1.2, margins = c(2, 2, 4, 2))

          ttl <- sanitize_title(input$r_title)
          if (nzchar(ttl)) {
            mtext(ttl, side = 3, line = 1, cex = 1.1, font = 2, col = "#667EEA")
          }

          mtext(
            sprintf("n = %d individuals", length(labels(ped))),
            side = 1,
            line = 0.5,
            cex = 0.9,
            col = "#6b7280"
          )
        },
        error = function(e) {
          plot.new()
          text(0.5, 0.5, "Preview unavailable", col = "#ef4444", cex = 1.2)
        }
      )
    },
    bg = "white"
  )

  observeEvent(input$r_confirm, {
    req(random_ped())

    if (!canPerformAction("confirm_random", 0.5)) {
      return()
    }

    tryCatch(
      {
        if (!is.null(pedigree$ped)) {
          saveToHistoryInternal()
        }

        new_ped <- relabel_generations(random_ped())

        validation <- validatePedigree(new_ped)
        if (!validation$valid) {
          showNotification(
            paste("Invalid pedigree:", validation$message),
            type = "error",
            duration = 5
          )
          return()
        }

        pedigree$ped <- new_ped
        pedigree$title <- sanitize_title(input$r_title)
        values$pedData <- makePedData(pedigree$ped)

        styles$deceased <- NULL
        styles$proband <- NULL
        styles$adopted <- NULL
        styles$fill <- character(0)
        styles$hatched <- character(0)
        styles$col <- character(0)
        styles$lty <- list()

        miscarriage(character(0))
        carrier(character(0))
        starred(character(0))
        text_annotations(list())
        twins_df(data.frame(
          id1 = character(),
          id2 = character(),
          code = integer(),
          stringsAsFactors = FALSE
        ))

        phenotypes$list <- list()
        phenotypes$assign <- list()

        sel(character(0))

        updateTextInput(session, "titleInput", value = pedigree$title)

        removeModal()

        showNotification(
          "✓ Random pedigree created successfully!",
          type = "message",
          duration = 3
        )
      },
      error = function(e) {
        showNotification(
          paste("Error creating pedigree:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  # ───────────────────────────────────────────────────────────────────────────────

  ## SUPPRESSION & UNDO

  observeEvent(input$btn_delete, {
    if (!canPerformAction("delete_all", 0.5)) {
      return()
    }

    showModal(modalDialog(
      title = tagList(
        icon("exclamation-triangle", style = "color: #ef4444;"),
        tags$strong(" Confirm Deletion")
      ),
      tags$p(
        "Are you sure you want to delete the entire pedigree?",
        style = "font-size: 1rem; margin-bottom: 1rem;"
      ),
      tags$p(
        class = "text-muted",
        icon("info-circle"),
        " This action will remove all individuals, phenotypes, and associated data.",
        tags$br(),
        "This action can be undone using the Undo button."
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "confirm_delete_all",
          "Delete Everything",
          class = "btn btn-danger",
          icon = icon("trash")
        )
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_delete_all, {
    if (!canPerformAction("confirm_delete", 0.5)) {
      return()
    }

    tryCatch(
      {
        saveToHistoryInternal()

        pedigree$ped <- NULL
        pedigree$title <- NULL
        values$pedData <- NULL

        styles$deceased <- NULL
        styles$proband <- NULL
        styles$adopted <- NULL
        styles$fill <- character(0)
        styles$hatched <- character(0)
        styles$col <- character(0)
        styles$lty <- list()

        miscarriage(character(0))
        carrier(character(0))
        starred(character(0))
        text_annotations(list())
        twins_df(data.frame(
          id1 = character(),
          id2 = character(),
          code = integer(),
          stringsAsFactors = FALSE
        ))

        phenotypes$list <- list()
        phenotypes$assign <- list()

        centersRV(NULL)
        sel(character(0))

        updateTextInput(session, "titleInput", value = "")

        removeModal()

        showNotification(
          "✓ Pedigree deleted. Use Undo to restore.",
          type = "message",
          duration = 4
        )
      },
      error = function(e) {
        showNotification(
          paste("Error deleting pedigree:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  observeEvent(input$btn_undo, {
    if (!canPerformAction("undo", 0.3)) {
      return()
    }

    if (length(history$stack) == 0) {
      showNotification(
        "⚠️ Nothing to undo",
        type = "warning",
        duration = 2
      )
      return()
    }

    tryCatch(
      {
        st <- history$stack[[1]]
        history$stack <- history$stack[-1]

        pedigree$ped <- st$ped
        pedigree$title <- st$title

        if (!is.null(st$pedData)) {
          values$pedData <- as.data.frame(st$pedData, stringsAsFactors = FALSE)
        } else {
          values$pedData <- NULL
        }

        styles$deceased <- st$deceased
        styles$proband <- st$proband
        styles$adopted <- st$adopted

        if (!is.null(st$miscarriage)) miscarriage(st$miscarriage)
        if (!is.null(st$carrier)) carrier(st$carrier)
        if (!is.null(st$starred)) starred(st$starred)
        if (!is.null(st$text_annotations)) text_annotations(st$text_annotations)
        if (!is.null(st$twins_df)) twins_df(st$twins_df)

        if (!is.null(st$phenotypes_list) && length(st$phenotypes_list) > 0) {
          phenotypes$list <- st$phenotypes_list
        } else {
          phenotypes$list <- list()
        }

        if (!is.null(st$phenotypes_assign) && length(st$phenotypes_assign) > 0) {
          phenotypes$assign <- st$phenotypes_assign
        } else {
          phenotypes$assign <- list()
        }

        rebuildStylesInternal()

        updateTextInput(session, "titleInput", value = pedigree$title %||% "")

        sel(character(0))

        time_str <- if (!is.null(st$timestamp)) {
          format(st$timestamp, "%H:%M:%S")
        } else {
          "previous state"
        }

        showNotification(
          sprintf("↶ Undone to %s", time_str),
          type = "message",
          duration = 3
        )
      },
      error = function(e) {
        showNotification(
          paste("Error during undo:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })


  # ────────────────────────────────────────────────────────────────────────────────────────

  ## AJOUT DE PARENTS ---------

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

    if (length(parents) == 2 && !any(is.na(parents))) {
      showNotification(
        sprintf("❌ %s already has parents: %s", id, paste(parents, collapse = ", ")),
        type = "error",
        duration = 4
      )
      return()
    }

    tryCatch(
      {
        saveToHistoryInternal()

        new_ped <- pedtools::addParents(pedigree$ped, id, verbose = FALSE)
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

        new_parents <- pedtools::parents(new_ped, id, internal = FALSE)

        showNotification(
          sprintf(
            "✅ Added parents for %s: %s (father), %s (mother)",
            id, new_parents[1], new_parents[2]
          ),
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

  # ────────────────────────────────────────────────────────────────────────────────────────

  ## UI DYNAMIQUE - SÉLECTION DU PARTENAIRE ---------

  output$partner_selection_ui <- renderUI({
    req(pedigree$ped, length(sel()) > 0)

    id <- sel()[1]
    partners <- getPartners(pedigree$ped, id)

    if (length(partners) == 0) {
      return(
        div(
          class = "warning-box",
          style = "margin: 10px 0;",
          tags$strong("ℹ️ No partner found"),
          tags$br(),
          "Individual must have a partner to add children."
        )
      )
    }

    if (length(partners) == 1) {
      return(
        div(
          style = "margin: 10px 0; padding: 10px; background: #e3f2fd; border-radius: 6px; border: 1px solid #90caf9;",
          tags$strong("Partner: "),
          tags$span(
            style = "color: #1976d2; font-weight: 600;",
            partners[1]
          )
        )
      )
    }

    tagList(
      tags$div(
        style = "margin: 10px 0;",
        tags$label(
          style = "font-size: 13px; font-weight: 600; color: #495057; margin-bottom: 5px; display: block;",
          "Select Partner:"
        ),
        selectInput(
          "selected_partner",
          NULL,
          choices = setNames(partners, paste("Partner:", partners)),
          width = "100%"
        )
      )
    )
  })

  # ────────────────────────────────────────────────────────────────────────────────────────

  ## UI DYNAMIQUE - AJOUT DE PROCHES (ENFANTS/SIBLINGS) -----------

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
              id = "sex_male",
              class = if (sex_selected() == 1) "sex-btn active" else "sex-btn",
              "👨 Male",
              onclick = "Shiny.setInputValue('sex_selected', 1, {priority: 'event'});"
            ),
            tags$button(
              id = "sex_female",
              class = if (sex_selected() == 2) "sex-btn active" else "sex-btn",
              "👩 Female",
              onclick = "Shiny.setInputValue('sex_selected', 2, {priority: 'event'});"
            ),
            tags$button(
              id = "sex_unknown",
              class = if (sex_selected() == 0) "sex-btn active" else "sex-btn",
              "❓ Unknown",
              onclick = "Shiny.setInputValue('sex_selected', 0, {priority: 'event'});"
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
              onclick = "Shiny.setInputValue('decrease_relatives', Math.random(), {priority: 'event'});"
            ),
            div(
              class = "stepper-value",
              textOutput("num_relatives_display", inline = TRUE)
            ),
            tags$button(
              class = "stepper-btn",
              "+",
              onclick = "Shiny.setInputValue('increase_relatives', Math.random(), {priority: 'event'});"
            )
          )
        ),
        actionButton(
          "addChildren",
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
              id = "sex_male",
              class = if (sex_selected() == 1) "sex-btn active" else "sex-btn",
              "👨 Male",
              onclick = "Shiny.setInputValue('sex_selected', 1, {priority: 'event'});"
            ),
            tags$button(
              id = "sex_female",
              class = if (sex_selected() == 2) "sex-btn active" else "sex-btn",
              "👩 Female",
              onclick = "Shiny.setInputValue('sex_selected', 2, {priority: 'event'});"
            ),
            tags$button(
              id = "sex_unknown",
              class = if (sex_selected() == 0) "sex-btn active" else "sex-btn",
              "❓ Unknown",
              onclick = "Shiny.setInputValue('sex_selected', 0, {priority: 'event'});"
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
              onclick = "Shiny.setInputValue('decrease_relatives', Math.random(), {priority: 'event'});"
            ),
            div(
              class = "stepper-value",
              textOutput("num_relatives_display", inline = TRUE)
            ),
            tags$button(
              class = "stepper-btn",
              "+",
              onclick = "Shiny.setInputValue('increase_relatives', Math.random(), {priority: 'event'});"
            )
          )
        ),
        actionButton(
          "addSiblings",
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
              id = "sex_male",
              class = if (sex_selected() == 1) "sex-btn active" else "sex-btn",
              "👨 Male",
              onclick = "Shiny.setInputValue('sex_selected', 1, {priority: 'event'});"
            ),
            tags$button(
              id = "sex_female",
              class = if (sex_selected() == 2) "sex-btn active" else "sex-btn",
              "👩 Female",
              onclick = "Shiny.setInputValue('sex_selected', 2, {priority: 'event'});"
            ),
            tags$button(
              id = "sex_unknown",
              class = if (sex_selected() == 0) "sex-btn active" else "sex-btn",
              "❓ Unknown",
              onclick = "Shiny.setInputValue('sex_selected', 0, {priority: 'event'});"
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
              onclick = "Shiny.setInputValue('decrease_relatives', Math.random(), {priority: 'event'});"
            ),
            div(
              class = "stepper-value",
              textOutput("num_relatives_display", inline = TRUE)
            ),
            tags$button(
              class = "stepper-btn",
              "+",
              onclick = "Shiny.setInputValue('increase_relatives', Math.random(), {priority: 'event'});"
            )
          )
        ),
        div(
          style = "margin-bottom: 15px;",
          radioButtons(
            "half_sib_type",
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
          "addHalfSiblings",
          "Add Half-Siblings",
          icon = icon("user-friends"),
          class = "btn-primary-action"
        )
      )
    }
  })

  # ────────────────────────────────────────────────────────────────────────────────────────

  ### AJOUT D'ENFANTS ---------------

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
        saveToHistoryInternal()

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

        cleanOrphanPhenotypesInternal()

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

  # ────────────────────────────────────────────────────────────────────────────────────────

  ### AJOUT DE SIBLINGS ---------

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
        saveToHistoryInternal()

        new_ped <- addFullSiblings(pedigree$ped, id, n_siblings = n, sex = sex_val, verbose = FALSE)
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

        cleanOrphanPhenotypesInternal()

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

  # ────────────────────────────────────────────────────────────────────────────────────────

  ### AJOUT DE HALF-SIBLINGS -----------

  observeEvent(input$addHalfSiblings, {
    req(pedigree$ped, length(sel()) > 0)

    if (!canPerformAction("add_half_siblings", 0.3)) {
      return()
    }

    id <- sel()[1]
    n <- num_relatives()
    sex_val <- sex_selected()
    shared_parent <- input$half_sib_type %||% "father"

    tryCatch(
      {
        saveToHistoryInternal()

        new_ped <- addHalfSiblings(
          pedigree$ped,
          id,
          n_siblings = n,
          sex = sex_val,
          shared_parent = shared_parent,
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

        cleanOrphanPhenotypesInternal()

        sex_text <- c("unknown sex", "male", "female")[sex_val + 1]
        parent_type <- if (shared_parent == "father") "paternal" else "maternal"

        showNotification(
          sprintf(
            "✅ Added %d %s half-sibling(s) (%s) to %s",
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

  # ────────────────────────────────────────────────────────────────────────────────────────

  ### UI DYNAMIQUE - JUMEAUX & TRIPLÉS --------------

  output$twins_triplets_ui <- renderUI({
    req(pedigree$ped, length(sel()) > 0)

    tagList(
      div(
        style = "margin-bottom: 10px;",
        tags$label(
          style = "font-size: 12px; color: #6b7280; margin-bottom: 5px; display: block;",
          "Twin Type:"
        ),
        div(
          class = "twin-type-selector",
          tags$button(
            id = "twin_mz",
            class = if (twin_type_selected() == 1) "twin-type-btn active" else "twin-type-btn",
            "MZ",
            onclick = "Shiny.setInputValue('twin_type_selected', 1, {priority: 'event'});",
            title = "Monozygotic (identical)"
          ),
          tags$button(
            id = "twin_dz",
            class = if (twin_type_selected() == 2) "twin-type-btn active" else "twin-type-btn",
            "DZ",
            onclick = "Shiny.setInputValue('twin_type_selected', 2, {priority: 'event'});",
            title = "Dizygotic (fraternal)"
          ),
          tags$button(
            id = "twin_uz",
            class = if (twin_type_selected() == 3) "twin-type-btn active" else "twin-type-btn",
            "UZ",
            onclick = "Shiny.setInputValue('twin_type_selected', 3, {priority: 'event'});",
            title = "Unknown zygosity"
          )
        )
      ),
      if (twin_type_selected() != 1) {
        div(
          style = "margin-bottom: 10px;",
          tags$label(
            style = "font-size: 12px; color: #6b7280; margin-bottom: 5px; display: block;",
            "Twin Gender:"
          ),
          div(
            class = "sex-buttons",
            tags$button(
              id = "twin_sex_male",
              class = if (twin_sex_selected() == 1) "sex-btn active" else "sex-btn",
              "👨 Male",
              onclick = "Shiny.setInputValue('twin_sex_selected', 1, {priority: 'event'});"
            ),
            tags$button(
              id = "twin_sex_female",
              class = if (twin_sex_selected() == 2) "sex-btn active" else "sex-btn",
              "👩 Female",
              onclick = "Shiny.setInputValue('twin_sex_selected', 2, {priority: 'event'});"
            ),
            tags$button(
              id = "twin_sex_unknown",
              class = if (twin_sex_selected() == 0) "sex-btn active" else "sex-btn",
              "❓ Unknown",
              onclick = "Shiny.setInputValue('twin_sex_selected', 0, {priority: 'event'});"
            )
          )
        )
      },
      actionButton(
        "addTwin",
        "Add Twin",
        icon = icon("user-friends"),
        class = "btn-success btn-block btn-sm"
      ),
      actionButton(
        "addTriplets",
        "Add Triplets",
        icon = icon("users"),
        class = "btn-success btn-block btn-sm"
      )
    )
  })

  # ────────────────────────────────────────────────────────────────────────────────────────

  ### AJOUT DE JUMEAUX ---------------

  observeEvent(input$addTwin, {
    req(pedigree$ped, length(sel()) > 0)

    if (!canPerformAction("add_twin", 0.3)) {
      return()
    }

    id <- sel()[1]
    zygosity <- twin_type_selected()
    twin_sex <- if (zygosity == 1) NULL else twin_sex_selected()

    tryCatch(
      {
        saveToHistoryInternal()

        result <- addTwin(pedigree$ped, id, sex = twin_sex, zygosity = zygosity)

        new_ped <- improve_layout(result$ped)

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

        current_twins <- twins_df()
        new_twin_row <- data.frame(
          id1 = result$original_id,
          id2 = result$twin_id,
          code = zygosity,
          stringsAsFactors = FALSE
        )

        twins_df(rbind(current_twins, new_twin_row))

        cleanOrphanPhenotypesInternal()

        zygosity_text <- c("MZ (identical)", "DZ (fraternal)", "UZ (unknown)")[zygosity]

        showNotification(
          sprintf(
            "✅ Added twin for %s: %s (%s)",
            result$original_id, result$twin_id, zygosity_text
          ),
          type = "message",
          duration = 3
        )
      },
      error = function(e) {
        showNotification(
          paste("Error adding twin:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  # ────────────────────────────────────────────────────────────────────────────────────────

  ### AJOUT DE TRIPLÉS -----------

  observeEvent(input$addTriplets, {
    req(pedigree$ped, length(sel()) > 0)

    if (!canPerformAction("add_triplets", 0.3)) {
      return()
    }

    id <- sel()[1]

    tryCatch(
      {
        saveToHistoryInternal()

        result <- addTriplets(pedigree$ped, id)

        new_ped <- improve_layout(result$ped)

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

        current_twins <- twins_df()

        triplet_ids <- result$triplet_ids
        new_rows <- data.frame(
          id1 = c(triplet_ids[1], triplet_ids[2]),
          id2 = c(triplet_ids[2], triplet_ids[3]),
          code = c(1, 1),
          stringsAsFactors = FALSE
        )

        twins_df(rbind(current_twins, new_rows))

        cleanOrphanPhenotypesInternal()

        showNotification(
          sprintf("✅ Added triplets: %s", paste(triplet_ids, collapse = ", ")),
          type = "message",
          duration = 3
        )
      },
      error = function(e) {
        showNotification(
          paste("Error adding triplets:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  # ────────────────────────────────────────────────────────────────────────────────────────

  ## SYSTÈME DE PHÉNOTYPES COMPLET ----------------------

  observeEvent(input$btn_create_phenotype, {
    if (!canPerformAction("create_pheno_modal", 0.5)) {
      return()
    }

    showModal(modalDialog(
      title = tagList(icon("palette"), tags$strong(" Create New Phenotype")),
      size = "m",
      easyClose = TRUE,
      textInput(
        "pheno_name",
        "Phenotype Name:",
        placeholder = "e.g., Affected, Carrier, Variant",
        value = ""
      ),
      fluidRow(
        column(
          6,
          colorPickerUI("pheno_fill", "Fill Color:", selected = "#FF6B6B")
        ),
        column(
          6,
          colorPickerUI("pheno_col", "Border Color:", selected = "#000000")
        )
      ),
      fluidRow(
        column(
          6,
          checkboxInput("pheno_hatched", "Hatched Pattern", value = FALSE)
        ),
        column(
          6,
          selectInput(
            "pheno_lty",
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
          "pheno_confirm",
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
              'Shiny.setInputValue("pheno_click", "%s", {priority:"event"}); return false;',
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
            paste0("pheno_edit_", gsub("[^a-zA-Z0-9]", "_", pheno_name)),
            NULL,
            icon = icon("edit"),
            class = "icon-btn-small",
            onclick = sprintf(
              'Shiny.setInputValue("pheno_edit", "%s", {priority:"event"});',
              pheno_name
            )
          ),
          actionButton(
            paste0("pheno_delete_", gsub("[^a-zA-Z0-9]", "_", pheno_name)),
            NULL,
            icon = icon("trash"),
            class = "icon-btn-small danger",
            onclick = sprintf(
              'Shiny.setInputValue("pheno_delete", "%s", {priority:"event"});',
              pheno_name
            )
          )
        )
      )
    })

    tagList(items)
  })

  observeEvent(input$pheno_click, {
    req(pedigree$ped, input$pheno_click, length(sel()) > 0)

    if (!canPerformAction("toggle_pheno", 0.2)) {
      return()
    }

    pheno_name <- input$pheno_click
    id <- sel()[1]

    if (!pheno_name %in% names(phenotypes$list)) {
      showNotification(
        sprintf("⚠️ Phenotype '%s' not found", pheno_name),
        type = "warning",
        duration = 3
      )
      return()
    }

    current_assigned <- phenotypes$assign[[pheno_name]] %||% character(0)

    if (id %in% current_assigned) {
      phenotypes$assign[[pheno_name]] <- setdiff(current_assigned, id)

      showNotification(
        sprintf("✅ Removed '%s' from %s", pheno_name, id),
        type = "message",
        duration = 2
      )
    } else {
      phenotypes$assign[[pheno_name]] <- unique(c(current_assigned, id))

      showNotification(
        sprintf("✅ Assigned '%s' to %s", pheno_name, id),
        type = "message",
        duration = 2
      )
    }

    rebuildStylesInternal()
  })

  observeEvent(input$pheno_edit, {
    req(input$pheno_edit)

    if (!canPerformAction("edit_pheno_modal", 0.5)) {
      return()
    }

    pheno_name <- input$pheno_edit

    if (!pheno_name %in% names(phenotypes$list)) {
      showNotification(
        sprintf("⚠️ Phenotype '%s' not found", pheno_name),
        type = "warning",
        duration = 3
      )
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
          colorPickerUI("pheno_edit_fill", "Fill Color:", selected = pheno_spec$fill)
        ),
        column(
          6,
          colorPickerUI("pheno_edit_col", "Border Color:", selected = pheno_spec$col)
        )
      ),
      fluidRow(
        column(
          6,
          checkboxInput("pheno_edit_hatched", "Hatched Pattern", value = isTRUE(pheno_spec$hatched))
        ),
        column(
          6,
          selectInput(
            "pheno_edit_lty",
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
          "pheno_edit_confirm",
          "Save Changes",
          class = "btn btn-primary",
          icon = icon("save")
        )
      )
    ))
  })

  observeEvent(input$pheno_edit_confirm, {
    req(pheno_editing())

    if (!canPerformAction("confirm_edit_pheno", 0.3)) {
      return()
    }

    pheno_name <- pheno_editing()

    tryCatch(
      {
        phenotypes$list[[pheno_name]] <- list(
          fill = input$pheno_edit_fill %||% "#FF6B6B",
          col = input$pheno_edit_col %||% "#000000",
          hatched = isTRUE(input$pheno_edit_hatched),
          lty = input$pheno_edit_lty %||% "solid"
        )

        rebuildStylesInternal()

        removeModal()

        showNotification(
          sprintf("✅ Phenotype '%s' updated", pheno_name),
          type = "message",
          duration = 3
        )

        pheno_editing(NULL)
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

  observeEvent(input$pheno_delete, {
    req(input$pheno_delete)

    if (!canPerformAction("delete_pheno", 0.3)) {
      return()
    }

    pheno_name <- input$pheno_delete

    if (!pheno_name %in% names(phenotypes$list)) {
      showNotification(
        sprintf("⚠️ Phenotype '%s' not found", pheno_name),
        type = "warning",
        duration = 3
      )
      return()
    }

    assigned_count <- length(phenotypes$assign[[pheno_name]] %||% character(0))

    showModal(modalDialog(
      title = tagList(
        icon("exclamation-triangle", style = "color: #ef4444;"),
        tags$strong(" Confirm Deletion")
      ),
      tags$p(
        sprintf("Are you sure you want to delete phenotype '%s'?", pheno_name),
        style = "font-size: 1rem; margin-bottom: 1rem;"
      ),
      if (assigned_count > 0) {
        div(
          class = "warning-box",
          tags$strong("⚠️ Warning:"),
          sprintf(" This phenotype is assigned to %d individual(s).", assigned_count)
        )
      },
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "pheno_delete_confirm",
          "Delete Phenotype",
          class = "btn btn-danger",
          icon = icon("trash"),
          onclick = sprintf(
            'Shiny.setInputValue("pheno_delete_target", "%s", {priority:"event"})',
            pheno_name
          )
        )
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$pheno_delete_confirm, {
    req(input$pheno_delete_target)

    if (!canPerformAction("confirm_delete_pheno", 0.3)) {
      return()
    }

    pheno_name <- input$pheno_delete_target

    tryCatch(
      {
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

  # ────────────────────────────────────────────────────────────────────────────────────────

  ## UI DYNAMIQUES - AFFICHAGE INFO & STATS --------------

  output$selection_info <- renderUI({
    if (length(sel()) == 0 || is.null(pedigree$ped)) {
      div(class = "no-selection", "⚠️ No individual selected. Click to select.")
    } else {
      id <- sel()[1]
      sex_val <- pedtools::getSex(pedigree$ped, id)
      sex_text <- switch(as.character(sex_val),
        "1" = "👨 Male",
        "2" = "👩 Female",
        "0" = "❓ Unknown"
      )

      is_deceased <- id %in% c(styles$deceased, miscarriage())
      is_proband <- id %in% styles$proband
      is_carrier <- id %in% carrier()
      is_starred <- id %in% starred()

      info_div <- div(
        class = "selected-info",
        tags$p(style = "margin: 0;", tags$strong("ID: "), id),
        tags$p(style = "margin: 5px 0 0 0;", tags$strong("Sex: "), sex_text)
      )

      if (is_deceased) {
        info_div <- tagList(info_div, div(class = "success-box", "✝ Marked as deceased"))
      }
      if (is_proband) {
        info_div <- tagList(info_div, div(class = "success-box", "➤ Marked as proband"))
      }
      if (is_carrier) {
        info_div <- tagList(info_div, div(class = "success-box", "● Marked as carrier"))
      }
      if (is_starred) {
        info_div <- tagList(info_div, div(class = "success-box", "★ Marked with star"))
      }

      info_div
    }
  })

  output$individual_id <- renderText({
    if (length(sel()) == 0 || is.null(pedigree$ped)) "—" else sel()[1]
  })

  output$individual_sex <- renderText({
    if (length(sel()) == 0 || is.null(pedigree$ped)) {
      "—"
    } else {
      sx <- pedtools::getSex(pedigree$ped, sel()[1])
      c("Unknown", "Male", "Female")[match(sx, c(0, 1, 2), nomatch = 1)]
    }
  })

  output$individual_name <- renderText({
    if (length(sel()) == 0 || is.null(pedigree$ped)) {
      "—"
    } else {
      id <- sel()[1]
      if (!is.null(values$pedData)) {
        row <- values$pedData[values$pedData$id == id, , drop = FALSE]
        if (nrow(row) > 0) {
          ln <- row$last_name
          fn <- row$first_name
          if (nzchar(ln) || nzchar(fn)) {
            name_parts <- c()
            if (nzchar(ln)) name_parts <- c(name_parts, toupper(ln))
            if (nzchar(fn)) name_parts <- c(name_parts, tools::toTitleCase(fn))
            return(paste(name_parts, collapse = " "))
          }
        }
      }
      id
    }
  })

  output$individual_status <- renderText({
    if (length(sel()) == 0 || is.null(pedigree$ped)) {
      "—"
    } else {
      id <- sel()[1]
      status <- c()
      if (id %in% c(styles$deceased, miscarriage())) status <- c(status, "Deceased")
      if (id %in% miscarriage()) status <- c(status, "Miscarriage")
      if (id %in% styles$proband) status <- c(status, "Proband")
      if (length(status) > 0) paste(status, collapse = ", ") else "Alive"
    }
  })

  output$pedigree_stats <- renderUI({
    if (is.null(pedigree$ped)) {
      div(
        style = "text-align: center; color: #6c757d; font-size: 13px;",
        "No pedigree"
      )
    } else {
      n_misc <- length(miscarriage())
      n_dec <- length(c(styles$deceased, labels(pedigree$ped)[values$pedData$deceased == TRUE]))
      n_dec <- length(unique(n_dec))
      n_prob <- length(styles$proband)
      twins <- twins_df()
      n_twin_pairs <- nrow(twins)

      tagList(
        div(
          class = "stats-grid",
          div(
            class = "stat-item",
            div(class = "stat-value", pedtools::pedsize(pedigree$ped)),
            div(class = "stat-label", "Individuals")
          ),
          div(
            class = "stat-item",
            div(class = "stat-value", length(pedtools::founders(pedigree$ped))),
            div(class = "stat-label", "Founders")
          ),
          div(
            class = "stat-item",
            div(class = "stat-value", n_dec),
            div(class = "stat-label", "Deceased")
          ),
          div(
            class = "stat-item",
            div(class = "stat-value", n_twin_pairs),
            div(class = "stat-label", "Twin pairs")
          )
        )
      )
    }
  })

  # ────────────────────────────────────────────────────────────────────────────────────────

  ## TABLE DE DONNÉES ----------

  output$pedTableDT <- renderDT({
    req(pedigree$ped, values$pedData)

    tryCatch(
      {
        df <- values$pedData

        display_df <- df %>%
          mutate(
            fidx = pedigree$ped$FIDX[match(id, labels(pedigree$ped))],
            midx = pedigree$ped$MIDX[match(id, labels(pedigree$ped))],
            father = ifelse(
              is.na(fidx) | fidx == 0,
              "",
              labels(pedigree$ped)[fidx]
            ),
            mother = ifelse(
              is.na(midx) | midx == 0,
              "",
              labels(pedigree$ped)[midx]
            )
          ) %>%
          select(
            ID = id,
            Sex = sex,
            Father = father,
            Mother = mother,
            `Last Name` = last_name,
            `First Name` = first_name,
            `Date of Birth` = date_of_birth,
            Deceased = deceased,
            `Date of Death` = date_of_death,
            Age = age,
            `Assigned at Birth` = assigned_at_birth,
            Comments = comments
          )

        display_df$Sex <- c("Unknown", "Male", "Female")[match(display_df$Sex, c(0, 1, 2), nomatch = 1)]
        display_df$Deceased <- ifelse(display_df$Deceased, "Yes", "No")

        datatable(
          display_df,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel"),
            language = list(
              search = "Search:",
              lengthMenu = "Show _MENU_ entries",
              info = "Showing _START_ to _END_ of _TOTAL_ individuals"
            )
          ),
          rownames = FALSE,
          class = "cell-border stripe hover",
          selection = "single"
        )
      },
      error = function(e) {
        datatable(data.frame(Error = paste("Unable to display table:", e$message)))
      }
    )
  })

  # ────────────────────────────────────────────────────────────────────────────────────────

  ## EXPORT PNG -------------

  output$downloadPNG <- downloadHandler(
    filename = function() {
      ttl <- sanitize_title(pedigree$title)
      if (nzchar(ttl)) {
        paste0(gsub(" ", "_", ttl), "_", format(Sys.Date(), "%Y%m%d"), ".png")
      } else {
        paste0("pedigree_", format(Sys.Date(), "%Y%m%d"), ".png")
      }
    },
    content = function(file) {
      req(pedigree$ped)

      tryCatch(
        {
          png(
            filename = file,
            width = 3000,
            height = 2000,
            res = 300,
            bg = "white"
          )

          ped <- pedigree$ped

          misc_ids <- miscarriage()
          dec_ids <- c(styles$deceased, labels(ped)[values$pedData$deceased == TRUE])
          dec_ids <- unique(dec_ids)
          prob_id <- styles$proband
          carr_ids <- carrier()
          star_ids <- starred()
          txt_annot <- text_annotations()
          twins <- twins_df()

          al <- pedtools:::.pedAlignment(
            ped,
            miscarriage = misc_ids,
            twins = if (nrow(twins) > 0) twins else NULL
          )

          labs <- build_labs(ped)
          annotations <- prepare_annotation_data(ped, values$pedData)

          col_list <- list()
          if (length(styles$col) > 0) {
            for (id in names(styles$col)) {
              col_val <- styles$col[id]
              if (!col_val %in% names(col_list)) {
                col_list[[col_val]] <- character(0)
              }
              col_list[[col_val]] <- c(col_list[[col_val]], id)
            }
          }

          an <- pedtools:::.pedAnnotation(
            ped,
            labs = labs,
            fill = if (length(styles$fill) > 0) styles$fill else NULL,
            col = if (length(col_list) > 0) col_list else list(),
            lty = if (length(styles$lty) > 0) styles$lty else list(),
            deceased = dec_ids,
            proband = prob_id,
            carrier = carr_ids,
            starred = star_ids,
            textAnnot = txt_annot,
            hatched = if (length(styles$hatched) > 0) styles$hatched else NULL
          )

          sc <- pedtools:::.pedScaling(
            al, an,
            cex = 1.4,
            symbolsize = 1.2,
            margins = c(6, 3, 7, 3)
          )

          pedtools:::drawPed(al, an, sc)
          pedtools:::.annotatePed(al, an, sc)
          draw_custom_annotations(al, annotations, sc)

          adopted_ids <- styles$adopted %||% character(0)

          if (length(adopted_ids) > 0) {
            labs_all <- labels(ped)
            lab_by_plot <- labs_all[al$plotord]

            sex_all <- pedtools::getSex(ped, labs_all)
            shape_by_lab <- setNames(
              ifelse(sex_all == 1, "square",
                ifelse(sex_all == 2, "circle", "diamond")
              ),
              labs_all
            )

            cx <- al$xall + sc$boxw / 2
            cy <- al$yall + sc$boxh / 2
            df_sel <- data.frame(
              lab = lab_by_plot,
              x = cx,
              y = cy,
              stringsAsFactors = FALSE
            )

            for (id_adopt in adopted_ids) {
              if (id_adopt %in% lab_by_plot) {
                rows_adopt <- df_sel[df_sel$lab == id_adopt, , drop = FALSE]

                if (nrow(rows_adopt) > 0) {
                  for (k in seq_len(nrow(rows_adopt))) {
                    .draw_adoption_brackets(
                      shape = shape_by_lab[[id_adopt]] %||% "square",
                      x = rows_adopt$x[k],
                      y = rows_adopt$y[k],
                      w = sc$boxw,
                      h = sc$boxh,
                      lwd = ADOPTION_BRACKET_CONFIG$line_width,
                      col = ADOPTION_BRACKET_CONFIG$color
                    )
                  }
                }
              }
            }
          }

          ttl <- sanitize_title(pedigree$title)
          if (nzchar(ttl)) {
            old_xpd <- par("xpd")
            par(xpd = NA)

            usr <- par("usr")
            x_center <- mean(usr[1:2])
            y_height <- diff(usr[3:4])
            y_title <- usr[4] + y_height * 0.15

            DescTools::BoxedText(
              x = x_center,
              y = y_title,
              labels = ttl,
              bg = "#667EEA",
              col = "white",
              border = "#4C51BF",
              cex = 1.2,
              xpad = 1.8,
              ypad = 1.3,
              font = 2
            )

            par(xpd = old_xpd)
          }

          dev.off()

          showNotification(
            "✅ PNG exported successfully!",
            type = "message",
            duration = 3
          )
        },
        error = function(e) {
          dev.off()
          showNotification(
            paste("Error exporting PNG:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    }
  )

  # ────────────────────────────────────────────────────────────────────────────────────────

  ## EXPORT PED --------

  output$downloadPED <- downloadHandler(
    filename = function() {
      ttl <- sanitize_title(pedigree$title)
      if (nzchar(ttl)) {
        paste0(gsub(" ", "_", ttl), "_", format(Sys.Date(), "%Y%m%d"), ".ped")
      } else {
        paste0("pedigree_", format(Sys.Date(), "%Y%m%d"), ".ped")
      }
    },
    content = function(file) {
      req(pedigree$ped, values$pedData)

      tryCatch(
        {
          df <- values$pedData

          ped_df <- data.frame(
            FID = rep("FAM001", nrow(df)),
            IID = df$id,
            PAT = sapply(df$id, function(x) {
              idx <- match(x, labels(pedigree$ped))
              fidx <- pedigree$ped$FIDX[idx]
              if (is.na(fidx) || fidx == 0) "0" else labels(pedigree$ped)[fidx]
            }),
            MAT = sapply(df$id, function(x) {
              idx <- match(x, labels(pedigree$ped))
              midx <- pedigree$ped$MIDX[idx]
              if (is.na(midx) || midx == 0) "0" else labels(pedigree$ped)[midx]
            }),
            SEX = df$sex,
            PHENOTYPE = ifelse(df$id %in% styles$deceased, 2, 1),
            stringsAsFactors = FALSE
          )

          write.table(
            ped_df,
            file = file,
            quote = FALSE,
            sep = "\t",
            row.names = FALSE,
            col.names = TRUE
          )

          showNotification(
            "✅ PED file exported successfully!",
            type = "message",
            duration = 3
          )
        },
        error = function(e) {
          showNotification(
            paste("Error exporting PED:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    }
  )

  # ────────────────────────────────────────────────────────────────────────────────────────

  ## NETTOYAGE PÉRIODIQUE ----------


  observe({
    req(pedigree$ped)
    invalidateLater(5000)
    cleanOrphanPhenotypesInternal()
  })

  # 【 END 】
}


# ✅ RUN APP ============================
shinyApp(ui = ui, server = server)
