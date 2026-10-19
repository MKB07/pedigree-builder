# Archived R development file
# Original path: apprentissage_test /design/module_add_Member_2.R
# Original created: 2025-07-05 14:25:29
# Original modified: 2025-07-28 10:23:45
# Archive rationale: Design experiment for adding family members.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #ebebeb; }
      .container-center {
        height: 100vh;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .menu-fade-container {
        position: relative;
        min-width: 380px;
        max-width: 480px;
        min-height: 330px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .menu-bg {
        position: absolute;
        width: 100%;
        height: 100%;
        left: 0; top: 0;
        z-index: 0;
        border-radius: 14px;
        background: linear-gradient(160deg, #f3e5f5 0%, #b2ebf2 100%);
        box-shadow: 0 2px 30px 4px #b8c8e670, 0 2px 8px #aab7c320;
        transition: opacity 0.45s;
      }
      .menu-fade-container:hover .menu-bg,
      .menu-fade-container:focus-within .menu-bg {
        opacity: 0.08;
      }
      .menu-wait-text {
        position: absolute;
        z-index: 2;
        font-size: 1.3em;
        color: #27313a;
        font-weight: bold;
        text-align: center;
        letter-spacing: 0.12em;
        width: 100%;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        opacity: 1;
        transition: opacity 0.5s;
        pointer-events: none;
        user-select: none;
      }
      .menu-fade-container:hover .menu-wait-text,
      .menu-fade-container:focus-within .menu-wait-text {
        opacity: 0;
      }
      .menu-box {
        z-index: 3;
        min-width: 350px;
        max-width: 450px;
        border-radius: 12px;
        background: #fff;
        box-shadow: 0 2px 18px 0 #aab7c333;
        padding: 28px 20px 20px 20px;
        display: flex;
        flex-direction: column;
        gap: 22px;
        align-items: center;
        opacity: 0;
        pointer-events: none;
        transition: opacity 0.6s;
        position: relative;
      }
      .menu-fade-container:hover .menu-box,
      .menu-fade-container:focus-within .menu-box {
        opacity: 1;
        pointer-events: auto;
      }
      .main-btn {
        width: 100%;
        font-size: 1.3em;
        background: #f5f6f7;
        border: 2px solid #203640;
        border-radius: 8px;
        color: #1e1e1e;
        font-weight: bold;
        padding: 12px 0;
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 8px;
        margin-bottom: 0.5em;
        box-shadow: 0 2px 10px 0 #0001;
        transition: background 0.18s;
      }
      .main-btn:hover {
        background: #e6edf5;
      }
      .main-btn .main-icon {
        font-size: 1.3em;
      }
      .section-label {
        font-size: 1.02em;
        font-weight: 500;
        color: #203640;
        margin: 12px 0 6px 0;
        letter-spacing: 0.03em;
      }
      .btn-grid {
        display: grid;
        grid-template-columns: repeat(5, 48px); /* 5 colonnes pour 5 boutons */
        gap: 10px;
        margin-bottom: 6px;
        margin-top: 2px;
      }
      .icon-btn2 {
        background: #f8fafd;
        border: 2px solid #203640;
        border-radius: 8px;
        min-width: 46px;
        min-height: 46px;
        max-width: 52px;
        max-height: 52px;
        font-size: 1.65em;
        color: #444;
        display: flex;
        align-items: center;
        justify-content: center;
        box-shadow: 0 1px 6px #0a10141c;
        transition: background 0.18s, color 0.18s, border 0.14s;
        cursor: pointer;
        margin: 0;
        padding: 0;
      }
      .icon-btn2:focus {
        outline: none;
        border: 2.2px solid #00acc1;
      }
      .icon-btn2:hover {
        background: #cfe8fc;
        color: #1769aa;
        border-color: #1769aa;
      }
    "))
  ),
  div(
    class = "container-center",
    div(
      class = "menu-fade-container",
      div(class = "menu-bg"),
      div(class = "menu-wait-text", HTML("HOVER<br>FOR MENU")),
      div(
        class = "menu-box",
        # Premier bouton (Add Parents)
        actionButton("add_parents",
          tagList(span(class = "main-icon", "\U1F46A"), "Add Parents"),
          class = "main-btn"
        ),
        # Section Add Siblings
        div(class = "section-label", "Add siblings"),
        div(
          class = "btn-grid",
          actionButton("sib_male", "\U2642", class = "icon-btn2"), # ♂
          actionButton("sib_female", "\U2640", class = "icon-btn2"), # ♀
          actionButton("sib_couple", "\U26A5", class = "icon-btn2"), # ⚥
          actionButton("sib_dice1", "\U2680", class = "icon-btn2"), # ⚀
          actionButton("sib_dice2", "\U2681", class = "icon-btn2") # ⚁
        ),
        # Section Add Children
        div(class = "section-label", "Add Children"),
        div(
          class = "btn-grid",
          actionButton("child_male", "\U2642", class = "icon-btn2"),
          actionButton("child_female", "\U2640", class = "icon-btn2"),
          actionButton("child_couple", "\U26A5", class = "icon-btn2"),
          actionButton("child_cmd", "\U2318", class = "icon-btn2")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$add_parents, {
    showModal(modalDialog("Add Parents clicked"))
  })
  observeEvent(input$sib_male, {
    showModal(modalDialog("Add Male Sibling"))
  })
  observeEvent(input$sib_female, {
    showModal(modalDialog("Add Female Sibling"))
  })
  observeEvent(input$sib_couple, {
    showModal(modalDialog("Add Sibling Couple"))
  })
  observeEvent(input$sib_dice1, {
    showModal(modalDialog("Add Sibling (Dice ⚀)"))
  })
  observeEvent(input$sib_dice2, {
    showModal(modalDialog("Add Sibling (Dice ⚁)"))
  })
  observeEvent(input$child_male, {
    showModal(modalDialog("Add Male Child"))
  })
  observeEvent(input$child_female, {
    showModal(modalDialog("Add Female Child"))
  })
  observeEvent(input$child_couple, {
    showModal(modalDialog("Add Children Couple"))
  })
  observeEvent(input$child_cmd, {
    showModal(modalDialog("Add Special Child"))
  })
}

shinyApp(ui, server)
