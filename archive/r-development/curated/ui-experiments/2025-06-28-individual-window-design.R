# Archived R development file
# Original path: apprentissage_test /design/fenetreindiv.R
# Original created: 2025-06-28 02:10:33
# Original modified: 2025-06-28 02:10:33
# Archive rationale: Design experiment for individual detail windows.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

library(shiny)
library(shinyjs)

# ---------- CSS adapté (carré central large !) ----------
cardButtonCSS <- "
body { background: #f7f9fd; }
.glass-panel {
  background: rgba(255,255,255,0.93);
  border-radius: 22px;
  box-shadow: 0 6px 44px 0 #bcd2fa4a;
  border: none;
  padding: 20px 18px 13px 18px;
  max-width: 420px;
  margin: 45px auto 0 auto;
  position: relative;
  min-height: 410px;
  font-family: 'Inter', Arial, sans-serif;
}
.header-row {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 8px;
}
.details-title {
  font-size: 18px;
  font-weight: 600;
  color: #3252a8;
  margin-bottom: 0px;
  display: flex; align-items: center; gap: 8px;
}
.indiv-id {
  font-size: 17px; font-weight: 700; color: #497ddf; margin-left: 3px;
}
.close-btn-x {
  border:none; background:none; color:#b2b7c6; font-size:19px; cursor:pointer; transition:color .13s;
  margin-left: 8px;
}
.close-btn-x:hover { color:#ff4474;}
.main-panel-flex {
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  gap: 11px;
}
.siblings-col, .twins-col {
  display: flex;
  flex-direction: column;
  gap: 9px;
  margin-top: 0px;
  min-width: 92px;
}
.add-parents-center {
  display: flex;
  justify-content: center;
  margin-bottom: 6px;
  margin-top: 1px;
}
.add-parents-btn {
  display:flex; align-items:center; justify-content:center;
  border-radius:11px;
  border:2px solid #dde8f7; background: #f6f8fc;
  color:#3975d2; font-size:14px; font-weight:500; height:34px;
  gap:7px; transition:.14s;
  box-shadow:0 2px 8px #c5d5ee24;
  cursor:pointer;
  padding: 0 12px;
}
.add-parents-btn:hover { background:#eaf2fc; border:2px solid #6ea9e9;}
.central-square-wrap {
  display: flex; align-items: center; justify-content: center;
  min-height: 120px; min-width: 120px;
  margin: 0 10px;
}
.central-square {
  background: #f6f7fa;
  border:1px;
  width: 110px; height: 110px;
  border-radius: 13px;
  display: flex; align-items: center; justify-content: center;
box-shadow: rgba(0, 0, 0, 0.1) 0px 0px 5px 0px, rgba(0, 0, 0, 0.1) 0px 0px 1px 0px;
}
.children-row-abs {
  display: flex;
  flex-direction: row;
  gap: 15px;
  position: absolute;
  left: 0; right: 0;
  margin: 0 auto;
  top: 242px;
  width: 99%;
  justify-content: center;
  z-index: 1;
}
.card-action-btn {
  display: none;
}
.card-btn-label {
  display: flex;
  align-items: center;
  justify-content: flex-start;
  min-width: 84px; max-width: 112px;
  height: 31px;
  border-radius: 10px;
  border: 2px solid #d3def3;
  background: #fafdff;
  box-shadow: 0 2px 11px 0 rgba(31,38,135,0.08);
  cursor: pointer;
  padding: 2px 7px;
  font-size: 13px;
  font-weight: 500;
  color: #3360ad;
  gap: 7px;
  margin: 0;
  transition: border 0.15s, box-shadow 0.14s, color 0.11s, background 0.12s;
}
.card-btn-label:active, .card-btn-label.active {
  border: 2px solid #468af3;
  background: #f2f7fe;
  color: #2763bc;
  box-shadow: 0 2px 15px 0 #3b82f627;
}
.card-btn-label:hover {
  border: 2px solid #77aaff;
}
.card-btn-icon {
  width: 15px;
  height: 15px;
  display: flex;
  align-items: center;
  justify-content: center;
}

/* --- Spécial : boutons enfants ligne du bas --- */
.child-btn-vertical {
  flex-direction: column !important;
  min-width: 54px !important;
  max-width: 62px !important;
  height: 56px !important;
  padding: 4px 3px 2px 3px !important;
  gap: 0 !important;
  align-items: center !important;
  justify-content: center !important;
  font-size: 11px !important;
  border-radius: 11px !important;
}
.child-btn-vertical .card-btn-icon {
  margin-bottom: 3px;
  width: 18px; height: 18px;
}
.child-btn-vertical span:last-child {
  margin-top: 2px;
}
@media (max-width: 600px) {
  .glass-panel {max-width:99vw; padding:5vw 2vw;}
  .children-row-abs {top:232px;}
  .card-btn-label, .child-btn-vertical { font-size:10px;}
  .central-square, .central-square-wrap {width:75px !important; height:75px !important; min-width:75px !important; min-height:75px !important;}
}
"

# ---------- Utilitaire bouton carte ----------
cardActionButton <- function(id, label, svg, vertical = FALSE) {
  labelClass <- "card-btn-label"
  if (vertical) labelClass <- paste(labelClass, "child-btn-vertical")
  tags$span(
    tags$input(
      id = id,
      type = "button",
      class = "card-action-btn"
    ),
    tags$label(
      `for` = id,
      class = labelClass,
      tags$span(class = "card-btn-icon", HTML(svg)),
      tags$span(label)
    )
  )
}

# ---------- Icônes SVG (tailles adaptées petits boutons) ----------
icon_square <- '<svg width="17" height="17" viewBox="0 0 22 22" fill="none" stroke="#3157b0" stroke-width="2"><rect x="3" y="3" width="16" height="16" rx="2" fill="none"/></svg>'
icon_circle <- '<svg width="17" height="17" viewBox="0 0 22 22" fill="none" stroke="#b457a1" stroke-width="2"><circle cx="11" cy="11" r="8" fill="none"/></svg>'
icon_diamond <- '<svg width="17" height="17" viewBox="0 0 22 22" fill="none" stroke="#a084c8" stroke-width="2"><polygon points="11,3 20,11 11,19 2,11" fill="none"/></svg>'
icon_users <- '<svg width="17" height="17" viewBox="0 0 28 21" fill="none" stroke="#7193bd" stroke-width="1.5"><circle cx="8" cy="7" r="4"/><circle cx="20" cy="7" r="4"/><path d="M3 20c0-4 10-4 10 0"/><path d="M15 20c0-4 10-4 10 0"/></svg>'
icon_plus <- '<svg width="13" height="13" viewBox="0 0 18 18" fill="none" stroke="#3983e3" stroke-width="2"><line x1="9" y1="4" x2="9" y2="14"/><line x1="4" y1="9" x2="14" y2="9"/></svg>'
icon_sync <- '<svg width="15" height="15" viewBox="0 0 20 20" fill="none" stroke="#7193bd" stroke-width="2"><path d="M17 10a7 7 0 1 1-2-4"/><polyline points="16 4 17 10 11 9"/></svg>'
icon_triplet <- '<span style="display:inline-block;font-size:11px;background:#a2b7dd;color:#fff;border-radius:4px;padding:1px 5px;font-weight:bold;">3</span>'

# ---------- UI ----------
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(cardButtonCSS))),
  tags$script(HTML("
    $(document).on('click', '.card-action-btn + .card-btn-label', function() {
      var val = $(this).attr('for');
      Shiny.setInputValue(val, Math.random());
      $('.card-btn-label').removeClass('active'); $(this).addClass('active');
      setTimeout(function() { $('.card-btn-label').removeClass('active'); }, 200);
    });
  ")),
  br(),
  div(
    class = "glass-panel",
    div(
      class = "header-row",
      div(
        class = "details-title",
        HTML('<svg width="16" height="17" style="margin-bottom:-3px" fill="none" stroke="#b3bed7" stroke-width="2" viewBox="0 0 20 20"><rect x="4" y="4" width="12" height="14" rx="3"/><line x1="8" y1="8" x2="12" y2="8"/><line x1="8" y1="12" x2="12" y2="12"/></svg>'),
        "Individual", span(class = "indiv-id", "7")
      ),
      tags$button(class = "close-btn-x", icon("times"))
    ),
    div(
      class = "main-panel-flex",
      # Colonne gauche SIBLINGS
      div(
        class = "siblings-col", style = "padding-top:45px; margin-left:25px;",
        cardActionButton("add_frere", "Brother", icon_square),
        cardActionButton("add_soeur", "Sister", icon_circle),
        cardActionButton("add_inconnu", "Unknown", icon_diamond)
      ),
      # Centre vertical (grande zone centrale)
      div(
        style = "display:flex; flex-direction:column; align-items:center; min-width:90px;",
        div(
          class = "add-parents-center",
          tags$button(
            class = "add-parents-btn", id = "add_parents",
            HTML(paste(icon_plus, "Add Parents"))
          )
        ),
        div(
          class = "central-square-wrap",
          div(class = "central-square")
        )
      ),
      # Colonne droite SIBLINGS
      div(
        class = "twins-col", style = "padding-top:47px; margin-right:25px;",
        cardActionButton("add_jumeau", "Twin", icon_users),
        cardActionButton("add_triple", "Triplet", icon_triplet),
        cardActionButton("add_reorder", "Reorder", icon_sync)
      )
    ),
    # Ligne ENFANTS en bas : boutons verticaux (icône au-dessus du texte)
    div(
      class = "children-row-abs", style = "margin-top:-10px;",
      cardActionButton("add_fils", "Son", icon_square, vertical = TRUE),
      cardActionButton("add_inconnu2", "Unknown", icon_diamond, vertical = TRUE),
      cardActionButton("add_fille", "Daughter", icon_circle, vertical = TRUE),
      cardActionButton("add_partenaire", "Partner", icon_users, vertical = TRUE)
    ),
    tags$div(style = "height:90px;"), # réserve place sous les boutons enfants
    tags$hr(style = "border-color:#e0e7fa; margin-top:8px; margin-bottom:4px;"),
    strong("Apparentés ajoutés :"),
    tags$div(
      style = "min-height: 38px; margin-bottom:6px; font-size:13px;",
      uiOutput("liste_apparentes")
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  apparentés <- reactiveVal(data.frame(
    id = numeric(0),
    type = character(0),
    sexe = character(0),
    stringsAsFactors = FALSE
  ))
  observeEvent(input$add_frere, {
    lst <- apparentés()
    newrow <- data.frame(id = ifelse(nrow(lst) == 0, 1, max(lst$id) + 1), type = "Frère", sexe = "M", stringsAsFactors = FALSE)
    apparentés(rbind(lst, newrow))
  })
  observeEvent(input$add_soeur, {
    lst <- apparentés()
    newrow <- data.frame(id = ifelse(nrow(lst) == 0, 1, max(lst$id) + 1), type = "Sœur", sexe = "F", stringsAsFactors = FALSE)
    apparentés(rbind(lst, newrow))
  })
  observeEvent(input$add_inconnu, {
    lst <- apparentés()
    newrow <- data.frame(id = ifelse(nrow(lst) == 0, 1, max(lst$id) + 1), type = "Inconnu", sexe = "Inconnu", stringsAsFactors = FALSE)
    apparentés(rbind(lst, newrow))
  })
  observeEvent(input$add_jumeau, {
    lst <- apparentés()
    newrow <- data.frame(id = ifelse(nrow(lst) == 0, 1, max(lst$id) + 1), type = "Jumeau", sexe = "Inconnu", stringsAsFactors = FALSE)
    apparentés(rbind(lst, newrow))
  })
  observeEvent(input$add_triple, {
    lst <- apparentés()
    newrow <- data.frame(id = ifelse(nrow(lst) == 0, 1, max(lst$id) + 1), type = "Triplé", sexe = "Inconnu", stringsAsFactors = FALSE)
    apparentés(rbind(lst, newrow))
  })
  observeEvent(input$add_reorder, {
    showModal(modalDialog("Fonctionnalité de réorganisation à venir !"))
  })
  observeEvent(input$add_fils, {
    lst <- apparentés()
    newrow <- data.frame(id = ifelse(nrow(lst) == 0, 1, max(lst$id) + 1), type = "Fils", sexe = "M", stringsAsFactors = FALSE)
    apparentés(rbind(lst, newrow))
  })
  observeEvent(input$add_fille, {
    lst <- apparentés()
    newrow <- data.frame(id = ifelse(nrow(lst) == 0, 1, max(lst$id) + 1), type = "Fille", sexe = "F", stringsAsFactors = FALSE)
    apparentés(rbind(lst, newrow))
  })
  observeEvent(input$add_partenaire, {
    lst <- apparentés()
    newrow <- data.frame(id = ifelse(nrow(lst) == 0, 1, max(lst$id) + 1), type = "Partenaire", sexe = "Inconnu", stringsAsFactors = FALSE)
    apparentés(rbind(lst, newrow))
  })
  observeEvent(input$add_inconnu2, {
    lst <- apparentés()
    newrow <- data.frame(id = ifelse(nrow(lst) == 0, 1, max(lst$id) + 1), type = "Inconnu", sexe = "Inconnu", stringsAsFactors = FALSE)
    apparentés(rbind(lst, newrow))
  })
  observeEvent(input$add_parents, {
    lst <- apparentés()
    newrow <- data.frame(id = ifelse(nrow(lst) == 0, 1, max(lst$id) + 1), type = "Parent", sexe = "Inconnu", stringsAsFactors = FALSE)
    apparentés(rbind(lst, newrow))
  })
  output$liste_apparentes <- renderUI({
    lst <- apparentés()
    if (nrow(lst) == 0) {
      tags$em("Aucun apparenté ajouté pour le moment.")
    } else {
      lapply(1:nrow(lst), function(i) {
        type <- lst$type[i]
        sexe <- lst$sexe[i]
        couleur <- if (sexe == "M") "#92b4ec" else if (sexe == "F") "#ffb3c6" else "#bdbdbd"
        icon_txt <- if (type == "Frère" || type == "Fils") {
          HTML(icon_square)
        } else if (type == "Sœur" || type == "Fille") {
          HTML(icon_circle)
        } else if (type == "Inconnu") {
          HTML(icon_diamond)
        } else if (type == "Partenaire" || type == "Jumeau" || type == "Triplé" || type == "Parent") {
          HTML(icon_users)
        } else {
          "?"
        }
        tags$div(
          class = "liquid-apparente",
          style = sprintf("border-left: 6px solid %s;", couleur),
          icon_txt, tags$span(style = "margin-left:9px;", type),
          actionButton(paste0("del_", lst$id[i]), label = NULL, icon = icon("times"), class = "btn-xs btn-danger")
        )
      })
    }
  })
  observe({
    lst <- apparentés()
    if (nrow(lst) > 0) {
      lapply(lst$id, function(idx) {
        observeEvent(input[[paste0("del_", idx)]],
          {
            newlst <- lst[lst$id != idx, ]
            apparentés(newlst)
          },
          ignoreInit = TRUE
        )
      })
    }
  })
}

shinyApp(ui, server)
