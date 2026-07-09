# Archived R development file
# Original path: apprentissage_test /design/card_modif_fam.R
# Original created: 2025-07-15 09:58:02
# Original modified: 2025-07-28 05:17:35
# Archive rationale: Design experiment for family modification cards.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(bslib)

custom_css <- "
body {
  min-height: 100vh;
  background: linear-gradient(135deg, rgba(255,255,255,0.85) 0%, rgba(212,234,255,0.17) 100%);
  font-family: 'Helvetica Neue', Arial, sans-serif;
}

.header-row{
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  background: linear-gradient(135deg, rgba(255,255,255,0.29) 0%, rgba(212,234,255,0.22) 100%);
  border-radius: 16px;
  border: 1.5px solid rgba(255,255,255,0.26);
  backdrop-filter: blur(8px);
  box-shadow: 0 2px 16px rgba(92,152,255,0.07);
  margin: 48px auto 0 auto;
  width: 510px;
  padding: 20px 0 14px 0;
}

.header-row > .fullname {
  font-size: 20px;
  font-weight: 500;
  color: #3E3E3D;
    font-family: 'Helvetica Neue';
  margin-bottom: 2px;
  letter-spacing: 0.7px;
}
.header-row > .dates {
  color: #85929E;
  font-size: 15px;
  font-weight: 400;
  margin-bottom: 13px;
  margin-top: 0px;
}
.btn_group_menu {
  display: flex;
  justify-content: center;
  align-items: center;
  width: 94%;
  gap: 30px;
  padding: 7px 0 3px 0;
  margin-left: auto;
  margin-right: auto;
}
.btn_group_menu .btn {
  width: 54px;
  background: transparent;
  display: flex;
  flex-direction: column;
  align-items: center;
  font-family: 'Helvetica Neue', Arial, sans-serif;
  font-weight: 200;
  border: 0;
  box-shadow: none;
  outline: none;
  padding: 0;
}
.icon{
  color: #3E3E3D;
  font-size: 21px;
  margin-bottom: 0px;
}
.tab-label{
  color: #3E3E3D;
  font-size: 13px;
  margin-top: 2px;
  font-weight: 300;
  letter-spacing: 0.05em;
}
/* --- PILL TOGGLE STYLE --- */
.btn-group .btn {
  border-radius: 22px !important;
  background: #F4F6FA;
  color: #6A6A78;
  font-size: 14px;
  font-family: 'Helvetica Neue', Arial, sans-serif;
  font-weight: 500;
  border: none !important;
  box-shadow: none !important;
  transition: background 0.17s, color 0.15s;
  padding: 7px 23px 7px 23px;
}
.btn-group .btn.active,
.btn-group .btn:active,
.btn-group .btn:focus,
.btn-group .btn-primary.active {
  background: #656670 !important;
  color: #fff !important;
  border: none !important;
  outline: none !important;
}
.btn-group .btn:not(.active):hover {
  background: #e9eaf2 !important;
  color: #3E3E3D;
}

/* --- STRUCTURE FAMILY BLOCK DEMO --- */
.family-structure-block {
  margin-top: 35px;
  background: #fafdff;
  border-radius: 20px;
  border: 1.2px solid #dde3f2;
  padding: 15px 10px 18px 10px;
  width: 98%;
  box-shadow: 0 2px 8px #e7ecf7a8;
  display: flex;
  flex-direction: column;
  align-items: center;
}
.family-structure-grid {
  display: grid;
  grid-template-columns: 100px 1fr 100px;
  grid-template-rows: 40px 120px 44px;
  grid-gap: 0px 6px;
  width: 100%;
  align-items: center;
  justify-items: center;
  margin-bottom: 16px;
  position: relative;
}
.family-col-vert {
  display: flex;
  flex-direction: column;
  gap: 11px;
  align-items: center;
  justify-content: center;
}
.family-center-top {
  grid-column: 2/3;
  grid-row: 1/2;
  display: flex;
  justify-content: center;
  align-items: flex-end;
}
.family-center-middle {
  grid-column: 2/3;
  grid-row: 2/3;
  display: flex;
  justify-content: center;
  align-items: center;
}
.family-col-left { grid-column: 1/2; grid-row: 1/4;}
.family-col-right { grid-column: 3/4; grid-row: 1/4;}
.family-center-bottom {
  grid-column: 2/3;
  grid-row: 3/4;
  display: flex;
  justify-content: center;
  align-items: flex-start;
  margin-top: 6px;
}
.family-btn {
  border-radius: 13px;
  border: 1.7px solid #c2d1ec;
  background: #f7fafd;
  color: #4177cb;
  font-weight: 500;
  font-size: 15px;
  padding: 6px 18px;
  margin: 0px 0px;
  cursor: pointer;
  display: flex;
  align-items: center;
  gap: 7px;
  box-shadow: 0 2px 8px #e0ebff38;
  transition: border .14s, color .13s, background .14s;
}
.family-btn:hover, .family-btn:focus {
  border: 1.7px solid #7ab4ff;
  background: #eef6fe;
  color: #2366b7;
}
.family-block-center {
  display: flex;
  flex-direction: column;
  align-items: center;
}
.family-center-box {
  width: 108px;
  height: 108px;
  border-radius: 14px;
  background: #f2f7fd;
  border: 1.4px solid #e0e5f1;
  display: flex;
  align-items: center;
  justify-content: center;
  color: #b8b8c2;
  font-size: 15px;
  margin-bottom: 0px;
  margin-top: 0px;
  font-weight: 500;
}
.family-row-children {
  display: flex;
  justify-content: center;
  align-items: flex-end;
  gap: 18px;
  margin-top: 8px;
  width: 100%;
}
.family-child-btn {
  flex-direction: column;
  min-width: 60px;
  max-width: 72px;
  height: 59px;
  font-size: 12px;
  padding: 3px 3px 2px 3px;
  gap: 0px;
  align-items: center;
  justify-content: center;
  border-radius: 12px;
  display: flex;
  border: 1.7px solid #c2d1ec;
  background: #fafdff;
  color: #4177cb;
  margin-left: 0;
  margin-right: 0;
  margin-top: 0;
  box-shadow: 0 2px 8px #e0ebff38;
  transition: border .14s, color .13s, background .14s;
}
.family-child-btn:hover, .family-child-btn:focus {
  border: 1.7px solid #7ab4ff;
  background: #eef6fe;
  color: #2366b7;
}
.family-btn-icon {
  width: 18px; height: 18px; margin-right: 7px;
}
.family-child-btn .family-btn-icon { margin-right: 0; margin-bottom: 3px;}
"

# Icônes SVG en variables (pour exemple visuel simple)
icon_brother <- '<svg width="18" height="18" viewBox="0 0 22 22" fill="none" stroke="#3157b0" stroke-width="2"><rect x="3" y="3" width="16" height="16" rx="2" fill="none"/></svg>'
icon_sister <- '<svg width="18" height="18" viewBox="0 0 22 22" fill="none" stroke="#b457a1" stroke-width="2"><circle cx="11" cy="11" r="8" fill="none"/></svg>'
icon_unknown <- '<svg width="18" height="18" viewBox="0 0 22 22" fill="none" stroke="#a084c8" stroke-width="2"><polygon points="11,3 20,11 11,19 2,11" fill="none"/></svg>'
icon_twin <- '<svg width="18" height="18" viewBox="0 0 28 21" fill="none" stroke="#7193bd" stroke-width="1.5"><circle cx="8" cy="7" r="4"/><circle cx="20" cy="7" r="4"/><path d="M3 20c0-4 10-4 10 0"/><path d="M15 20c0-4 10-4 10 0"/></svg>'
icon_triplet <- '<span style="display:inline-block;font-size:12px;background:#a2b7dd;color:#fff;border-radius:4px;padding:1px 5px;font-weight:bold;">3</span>'
icon_reorder <- '<svg width="16" height="16" viewBox="0 0 20 20" fill="none" stroke="#7193bd" stroke-width="2"><path d="M17 10a7 7 0 1 1-2-4"/><polyline points="16 4 17 10 11 9"/></svg>'
icon_add_parents <- '<svg width="14" height="14" viewBox="0 0 18 18" fill="none" stroke="#3983e3" stroke-width="2"><line x1="9" y1="4" x2="9" y2="14"/><line x1="4" y1="9" x2="14" y2="9"/></svg>'
icon_son <- icon_brother
icon_daughter <- icon_sister
icon_partner <- icon_twin

ui <- fluidPage(
  tags$head(
    tags$style(HTML(custom_css)),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css")
  ),
  div(
    class = "header-row",
    span("LASTNAME, Firstname", class = "fullname"),
    span("12/12/1912 - 20/20/2020", class = "dates"),
    div(
      class = "btn_group_menu",
      actionButton(
        inputId = "ID",
        label = HTML('
          <span class="icon"><i class="bi bi-7-circle-fill"></i></span>
          <span class="tab-label">ID</span>
        ')
      ),
      actionButton(
        inputId = "Gender",
        label = HTML('
          <span class="icon"><i class="bi bi-gender-ambiguous"></i></span>
          <span class="tab-label">Gender</span>
        ')
      ),
      actionButton(
        inputId = "dead",
        label = HTML('
          <span class="icon"><i class="bi bi-file-excel-fill"></i></span>
          <span class="tab-label">dead</span>
        ')
      ),
      actionButton(
        inputId = "age",
        label = HTML('
          <span class="icon"><i class="bi bi-123"></i></span>
          <span class="tab-label">age</span>
        ')
      ),
      actionButton(
        inputId = "delete",
        label = HTML('
          <span class="icon"><i class="bi bi-person-x"></i></span>
          <span class="tab-label">Delete</span>
        ')
      )
    ),
    div(
      style = "margin-top: 20px; width: 74%;",
      radioGroupButtons(
        inputId = "settings_toggle",
        label = NULL,
        choices = c("Setting family", "Advanced settings"),
        selected = "Setting family",
        status = "primary",
        size = "normal",
        direction = "horizontal",
        justified = TRUE,
        individual = FALSE,
        checkIcon = list(
          yes = icon("check", style = "display:none"),
          no = icon(NULL)
        )
      )
    ),
    uiOutput("panel_choice")
  )
)

server <- function(input, output, session) {
  output$panel_choice <- renderUI({
    if (is.null(input$settings_toggle) || input$settings_toggle == "Setting family") {
      # Utilisation d'une grille CSS pour calquer le layout de l'image
      tagList(
        div(
          class = "family-structure-block",
          div(
            class = "family-structure-grid",
            # Colonne gauche
            div(
              class = "family-col-vert family-col-left",
              actionButton("brother", HTML(paste0('<span class="family-btn-icon">', icon_brother, "</span>Brother")), class = "family-btn"),
              actionButton("sister", HTML(paste0('<span class="family-btn-icon">', icon_sister, "</span>Sister")), class = "family-btn"),
              actionButton("unknown", HTML(paste0('<span class="family-btn-icon">', icon_unknown, "</span>Unknown")), class = "family-btn")
            ),
            # Centre haut : Add Parents
            div(
              class = "family-center-top",
              actionButton("add_parents", HTML(paste0('<span class="family-btn-icon">', icon_add_parents, "</span>Add Parents")), class = "family-btn", style = "font-size:16px; background:#f7fafd; color:#3975d2; border:1.5px solid #b5cfff;")
            ),
            # Centre : Dossier
            div(
              class = "family-center-middle",
              div(class = "family-center-box", "Dossier")
            ),
            # Colonne droite
            div(
              class = "family-col-vert family-col-right",
              actionButton("twin", HTML(paste0('<span class="family-btn-icon">', icon_twin, "</span>Twin")), class = "family-btn"),
              actionButton("triplet", HTML(paste0('<span class="family-btn-icon">', icon_triplet, "</span>Triplet")), class = "family-btn"),
              actionButton("reorder", HTML(paste0('<span class="family-btn-icon">', icon_reorder, "</span>Reorder")), class = "family-btn")
            ),
            # Centre bas : enfants
            div(
              class = "family-center-bottom",
              div(
                class = "family-row-children",
                actionButton("son", HTML(paste0('<span class="family-btn-icon">', icon_son, "</span>Son")), class = "family-child-btn"),
                actionButton("unknown_child", HTML(paste0('<span class="family-btn-icon">', icon_unknown, "</span>Unknown")), class = "family-child-btn"),
                actionButton("daughter", HTML(paste0('<span class="family-btn-icon">', icon_daughter, "</span>Daughter")), class = "family-child-btn"),
                actionButton("partner", HTML(paste0('<span class="family-btn-icon">', icon_partner, "</span>Partner")), class = "family-child-btn")
              )
            )
          )
        )
      )
    } else if (input$settings_toggle == "Advanced settings") {
      div(
        style = "margin-top:38px; text-align:center; color:#b1b8c7; font-size:16px;",
        tags$em("Advanced settings content coming soon…")
      )
    }
  })
}

shinyApp(ui, server)
