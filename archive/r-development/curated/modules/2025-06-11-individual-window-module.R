# Archived R development file
# Original path: 🧩 Versions_Support/Module/individual_window.R
# Original created: 2025-06-11 15:19:48
# Original modified: 2025-10-12 06:31:26
# Archive rationale: Standalone individual window module prototype.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# ======================= LIBRARIES =======================
# Core Shiny + widgets + utils
library(shiny)
library(shinyWidgets) # airDatepickerInput / updateAirDateInput
library(lubridate) # date arithmetic (%m+%, %m-%, years(), months())
library(shinyjs) # toggle UI blocks
library(shinyBS) # bsCollapse / bsCollapsePanel

# ======================= HELPERS / UTILITIES =======================
# Extract a numeric unit (years, months, days) from a free-text age,
# e.g. "2 years 3 months 4 days" or "2 ans 3 mois 4 jours".
# 'unit' can be a regex alternation like "year|years" or "an|ans"
extract_units <- function(txt, unit) {
  # word boundary before and after the unit to reduce false positives
  pattern <- paste0("([0-9]+)\\s*(?:", unit, ")\\b")
  match <- regmatches(txt, regexpr(pattern, txt, perl = TRUE, ignore.case = TRUE))
  if (length(match) > 0 && nchar(match[1]) > 0) {
    as.integer(gsub("\\D", "", match[1]))
  } else {
    0L
  }
}

# Text alignment by logical position around a central symbol
txtAlign <- c(
  top         = "center",
  topright    = "right",
  right       = "right",
  bottomright = "right",
  bottom      = "center",
  bottomleft  = "left",
  left        = "left",
  topleft     = "left",
  inside      = "center"
)

# Local namespace for annotation inputs (works in module/modal)
ns_annot <- function(id) paste0("annot-", id, "-")

# Annotation text input factory with alignment + default width.
# 'ann' is a list of current annotations keyed by position.
txtInp <- function(pos, id, ann, width = "100px") {
  val <- if (!is.null(ann[[pos]]) && !is.null(ann[[pos]][id])) ann[[pos]][id] else ""
  tags$input(
    id = paste0(ns_annot(id), pos),
    type = "text",
    class = "shiny-input-text form-control",
    style = sprintf(
      "text-align:%s;font-weight:bold;width:%s;margin:auto;padding:5px",
      txtAlign[[pos]], width
    ),
    value = val,
    placeholder = pos
  )
}

# Return a central symbol container depending on gender type.
# For "Unknown", render a centered diamond using CSS rotate.
getSymbolDiv <- function(type, input_field) {
  if (type == "Male") {
    # Square
    div(
      id = "symbol",
      style = "background:#cecece;border-radius:0;",
      input_field
    )
  } else if (type == "Female") {
    # Circle
    div(
      id = "symbol",
      style = "background:#cecece;border-radius:100%;",
      input_field
    )
  } else if (type == "Unknown") {
    # Diamond (inner div rotated 45°)
    div(
      id = "symbol",
      style = "background:none;display:flex;align-items:center;justify-content:center;",
      div(
        style = "width:150px;height:150px;background:#cecece;transform:rotate(45deg);display:flex;align-items:center;justify-content:center;",
        div(
          style = "transform:rotate(-45deg);width:90%;height:90%;display:flex;align-items:center;justify-content:center;",
          input_field
        )
      )
    )
  } else {
    # Default square
    div(
      id = "symbol",
      style = "background:#cecece;border-radius:0;",
      input_field
    )
  }
}

# Compute a relative date by adding/subtracting Y/M/D from a reference.
# direction: "backward" subtracts; "forward" adds.
compute_relative_date <- function(reference, years, months, days, direction = "backward") {
  if (direction == "backward") {
    reference %m-% years(years) %m-% months(months) - days
  } else {
    reference %m+% years(years) %m+% months(months) + days
  }
}

# Map gender to geometric shape for SVG rendering
get_shape_from_gender <- function(gender) {
  switch(toupper(gender),
    "FEMALE"  = "circle",
    "MALE"    = "square",
    "UNKNOWN" = "diamond",
    "diamond"
  )
}

# Generate a simple inline SVG for the given shape
generate_svg_shape <- function(shape, size = 48) {
  if (shape == "circle") {
    sprintf(
      '<svg width="%d" height="%d"><circle cx="%d" cy="%d" r="%d" stroke="black" stroke-width="2" fill="white"/></svg>',
      size, size, size / 2, size / 2, size / 2 - 2
    )
  } else if (shape == "square") {
    sprintf(
      '<svg width="%d" height="%d"><rect x="2" y="2" width="%d" height="%d" stroke="black" stroke-width="2" fill="white"/></svg>',
      size, size, size - 4, size - 4
    )
  } else if (shape == "diamond") {
    half <- size / 2
    sprintf(
      '<svg width="%d" height="%d"><polygon points="%d,%d %d,%d %d,%d %d,%d" stroke="black" stroke-width="2" fill="white"/></svg>',
      size, size,
      half, 2, size - 2, half, half, size - 2, 2, half
    )
  } else {
    ""
  }
}

# ======================= AGE MODULE (UI + SERVER) =======================
# UI for birth/death dates and computed age (bi-directional: text <-> dates)
mod_age_calculator_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "display:flex;gap:10px;align-items:flex-end;flex-wrap:wrap;",
    useShinyjs(),
    div(
      style = "min-width:200px;max-width:280px;flex:1 1 280px;",
      airDatepickerInput(
        inputId = ns("birth"),
        label = "Birth date",
        value = NULL,
        placeholder = "🗓️ Select date",
        dateFormat = "dd/MM/yyyy",
        language = "en",
        autoClose = TRUE,
        addon = "none",
        clearButton = TRUE,
        width = "280px"
      )
    ),
    div(
      style = "min-width:80px;flex:0 0 10px;padding-bottom:10px;",
      checkboxInput(ns("deceased"), "Deceased", value = FALSE)
    ),
    div(
      style = "min-width:200px;max-width:280px;flex:1 1 280px;",
      id = ns("death_container"),
      airDatepickerInput(
        inputId = ns("death"),
        label = "Death date",
        value = NULL,
        placeholder = "🗓️ Select date",
        dateFormat = "dd/MM/yyyy",
        language = "en",
        autoClose = TRUE,
        addon = "none",
        clearButton = TRUE,
        width = "280px"
      )
    ),
    div(
      style = "min-width:100px;max-width:160px;flex:1 1 110px;",
      tags$div(
        style = "display:flex;align-items:center;",
        uiOutput(ns("age_cross")),
        textInput(
          inputId = ns("age"),
          label = "Age",
          value = "",
          width = "150px",
          placeholder = "e.g. 2 years 3 months"
        )
      )
    )
  )
}

# Server logic for age module
mod_age_calculator_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_updating <- reactiveVal(FALSE)

    # Show/hide death date when "Deceased" is checked
    observe({
      toggle(id = "death_container", condition = input$deceased)
    })

    # Cross symbol next to age when deceased
    output$age_cross <- renderUI({
      if (isTRUE(input$deceased)) {
        tags$span(style = "font-size:22px;color:#ad3d3d;margin-right:8px;", "\u271D")
      }
    })

    # Compute age text from birth/death dates
    observeEvent(c(input$birth, input$death), {
      req(!is_updating())
      birth <- input$birth
      death <- input$death
      if (is.null(birth) || is.na(birth)) {
        return(NULL)
      }

      is_updating(TRUE)
      today <- Sys.Date()
      days <- if (is.null(death) || is.na(death)) {
        as.integer(difftime(today, birth, units = "days"))
      } else {
        as.integer(difftime(death, birth, units = "days"))
      }

      txt <- if (is.na(days)) {
        "Computation not possible"
      } else if (days < 0) {
        "Invalid future date"
      } else if (days >= 365) {
        years <- floor(days / 365.25)
        months <- floor((days %% 365.25) / 30.44)
        rest_days <- round(days - (years * 365.25) - (months * 30.44))
        age_parts <- c(
          if (years > 0) paste0(years, " year", ifelse(years > 1, "s", "")),
          if (months > 0) paste0(months, " month", ifelse(months > 1, "s", "")),
          if (rest_days > 0) paste0(rest_days, " day", ifelse(rest_days > 1, "s", ""))
        )
        paste(age_parts, collapse = " ")
      } else if (days >= 30) {
        months <- floor(days / 30.44)
        rest_days <- round(days - months * 30.44)
        age_parts <- c(
          paste0(months, " month", ifelse(months > 1, "s", "")),
          if (rest_days > 0) paste0(rest_days, " day", ifelse(rest_days > 1, "s", ""))
        )
        paste(age_parts, collapse = " ")
      } else if (days >= 7) {
        weeks <- floor(days / 7)
        rest_days <- days - weeks * 7
        age_parts <- c(
          paste0(weeks, " week", ifelse(weeks > 1, "s", "")),
          if (rest_days > 0) paste0(rest_days, " day", ifelse(rest_days > 1, "s", ""))
        )
        paste(age_parts, collapse = " ")
      } else {
        paste0(days, " day", ifelse(days > 1, "s", ""))
      }

      updateTextInput(session, "age", value = txt)
      is_updating(FALSE)
    })

    # Compute missing dates from age free-text
    observeEvent(input$age, {
      req(!is_updating())
      age_txt <- tolower(trimws(input$age))
      if (nchar(age_txt) == 0) {
        return(NULL)
      }

      is_updating(TRUE)
      birth <- input$birth
      death <- input$death
      deceased <- input$deceased
      today <- Sys.Date()

      # Accept both English and French units
      n_years <- max(extract_units(age_txt, "year|years"), extract_units(age_txt, "an|ans"))
      n_months <- max(extract_units(age_txt, "month|months"), extract_units(age_txt, "mois"))
      n_days <- max(extract_units(age_txt, "day|days"), extract_units(age_txt, "jour|jours"))

      if (all(c(n_years, n_months, n_days) == 0)) {
        is_updating(FALSE)
        return(NULL)
      }

      if (!deceased && (is.null(birth) || is.na(birth))) {
        birth_estimate <- compute_relative_date(today, n_years, n_months, n_days, "backward")
        updateAirDateInput(session, "birth", value = birth_estimate)
        if (!is.null(death)) updateAirDateInput(session, "death", value = NULL)
      }

      if (deceased && !is.null(birth) && (is.null(death) || is.na(death))) {
        death_estimate <- compute_relative_date(birth, n_years, n_months, n_days, "forward")
        updateAirDateInput(session, "death", value = death_estimate)
      }

      if (deceased && !is.null(death) && (is.null(birth) || is.na(birth))) {
        birth_estimate <- compute_relative_date(death, n_years, n_months, n_days, "backward")
        updateAirDateInput(session, "birth", value = birth_estimate)
      }

      is_updating(FALSE)
    })
  })
}

# =================== USER INTERFACE ===================
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Add an individual (via modal window)"),
  br(),
  actionButton("open_modal", "Add individual", icon = icon("plus")),
  br(), br(),
  uiOutput("resume_individu")
)

# ======================= SERVER =======================
server <- function(input, output, session) {
  # Store last validated individual
  individu_vals <- reactiveVal(NULL)
  # Store annotation values for the modal (by position)
  annotations_modal <- reactiveVal(list())

  # Track all annotation inputs in the modal and persist them
  lapply(names(txtAlign), function(pos) {
    observeEvent(input[[paste0(ns_annot("modal"), pos)]],
      {
        ann <- annotations_modal()
        if (is.null(ann[[pos]])) ann[[pos]] <- character()
        ann[[pos]]["modal"] <- input[[paste0(ns_annot("modal"), pos)]]
        # keep non-empty only
        ann[[pos]] <- ann[[pos]][nzchar(ann[[pos]])]
        # remove empty positions
        ann <- ann[lengths(ann) > 0]
        annotations_modal(ann)
      },
      ignoreNULL = TRUE
    )
  })

  # Mount the age module (single, correct definition used)
  mod_age_calculator_server("modal_age")

  # Small SVG (badge) near the title according to gender
  output$modal_svg_symbol <- renderUI({
    req(input$modal_gender)
    shape <- get_shape_from_gender(input$modal_gender)
    HTML(generate_svg_shape(shape, 48))
  })

  # Gender Unicode (♀ / ♂ / ⚧)
  output$modal_gender_txt <- renderText({
    req(input$modal_gender)
    if (toupper(input$modal_gender) == "FEMALE") {
      "\u2640"
    } else if (toupper(input$modal_gender) == "MALE") {
      "\u2642"
    } else {
      "\u26A7"
    }
  })

  # Dynamic modal title (icon + name + age + cross if deceased)
  output$modal_title <- renderUI({
    gender <- input$modal_gender
    last_name <- input$modal_last_name
    first_name <- input$modal_first_name
    deceased <- input[["modal_age-deceased"]]
    age_txt <- input[["modal_age-age"]]
    symbol <- switch(toupper(gender),
      "FEMALE"  = "\u2640",
      "MALE"    = "\u2642",
      "UNKNOWN" = "\u26A7",
      "\u26A7"
    )
    death_symb <- if (isTRUE(deceased)) tags$span(style = "font-size:22px;color:#ad3d3d;margin-left:10px;", "\u271D") else NULL
    tags$div(
      style = "display:flex;align-items:center;gap:12px;",
      uiOutput("modal_svg_symbol"),
      tags$span(style = "font-size:22px;", symbol),
      tags$span(style = "font-weight:bold;font-size:20px;", last_name),
      tags$span(style = "font-size:20px;", first_name),
      death_symb,
      if (!is.null(age_txt) && nchar(age_txt) > 0) {
        tags$span(style = "margin-left:12px;color:#555;", age_txt)
      }
    )
  })

  # Modal builder: grid + central SVG + annotations + identity + age module
  show_ind_modal <- function() {
    showModal(
      modalDialog(
        title = uiOutput("modal_title"),
        bsCollapse(
          id = "collapseSection", open = NULL,
          bsCollapsePanel(
            "Annotations",
            # Grid style for annotation positions
            tags$style(
              type = "text/css",
              HTML("
                #grid-container {
                  display: grid;
                  grid-template-columns: 100px 200px 40px;
                  grid-template-rows: 60px 200px 60px;
                  gap: 10px;
                  justify-content: center;
                  align-items: center;
                  margin-top: 20px;
                }
                #symbol {
                  grid-column: 2;
                  grid-row: 2;
                  border: none;
                  width: 200px;
                  height: 200px;
                  display: flex;
                  align-items: center;
                  justify-content: center;
                }
              ")
            ),
            uiOutput("annotation_grid_modal")
          )
        ),
        div(
          style = "display:flex;gap:12px;align-items:flex-end;margin-bottom:16px;",
          # Last name
          div(
            style = "flex:1 1 0;min-width:160px;max-width:280px;",
            textInput("modal_last_name", "Last name", width = "100%")
          ),
          # First name
          div(
            style = "flex:1 1 0;min-width:160px;max-width:280px;",
            textInput("modal_first_name", "First name", width = "100%")
          ),
          # Gender (compact)
          div(
            style = "flex:0 0 110px;max-width:120px;",
            tags$div(
              style = "display:flex;flex-direction:column;",
              tags$label("Gender", `for` = "modal_gender", style = "font-weight:600;margin-bottom:3px;"),
              selectInput("modal_gender", NULL, c("Female", "Male", "Unknown"), selected = "Female", width = "100%")
            )
          )
        ),
        hr(),
        div(mod_age_calculator_ui("modal_age")),
        hr(),
        div(textAreaInput("modal_comment", "Comment", rows = 2, placeholder = "Optional comment...", width = "100%")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("validate_ind", "Validate")
        ),
        size = NULL, easyClose = FALSE
      )
    )
  }

  # Recompute center preview SVG when gender changes (for completeness)
  output$modal_grid_center_svg <- renderUI({
    req(input$modal_gender)
    shape <- get_shape_from_gender(input$modal_gender)
    div(class = "genea-center-box", HTML(generate_svg_shape(shape, size = 90)))
  })

  # Open modal & reset annotations
  observeEvent(input$open_modal, {
    annotations_modal(list())
    show_ind_modal()
  })

  # Annotation grid (9 positions around/inside the symbol)
  output$annotation_grid_modal <- renderUI({
    ind_id <- "modal"
    type <- input$modal_gender
    currAnn <- annotations_modal()

    div(
      id = "grid-container",
      div(txtInp("topleft", ind_id, currAnn), style = "grid-area:1 / 1 / 2 / 2;"),
      div(txtInp("top", ind_id, currAnn), style = "grid-area:1 / 2 / 2 / 3;"),
      div(txtInp("topright", ind_id, currAnn), style = "grid-area:1 / 3 / 2 / 4;"),
      div(txtInp("left", ind_id, currAnn), style = "grid-area:2 / 1 / 3 / 2;"),
      getSymbolDiv(type, txtInp("inside", ind_id, currAnn)),
      div(txtInp("right", ind_id, currAnn), style = "grid-area:2 / 3 / 3 / 4;"),
      div(txtInp("bottomleft", ind_id, currAnn), style = "grid-area:3 / 1 / 4 / 2;"),
      div(txtInp("bottom", ind_id, currAnn), style = "grid-area:3 / 2 / 4 / 3;"),
      div(txtInp("bottomright", ind_id, currAnn), style = "grid-area:3 / 3 / 4 / 4;")
    )
  })

  # Validate the individual and print/store the values
  observeEvent(input$validate_ind, {
    vals <- list(
      last_name = input$modal_last_name,
      first_name = input$modal_first_name,
      comment = input$modal_comment,
      gender = input$modal_gender,
      shape = get_shape_from_gender(input$modal_gender),
      birth = input[["modal_age-birth"]],
      death = input[["modal_age-death"]],
      deceased = input[["modal_age-deceased"]],
      age_txt = input[["modal_age-age"]],
      annotations = annotations_modal()
    )
    individu_vals(vals)
    removeModal()
    print("==== INDIVIDUAL CREATED ====")
    print(vals)
  })

  # Summary card of the last validated individual
  output$resume_individu <- renderUI({
    ind <- individu_vals()
    req(ind)
    tags$div(
      style = "border:1px solid #bbb;border-radius:12px;padding:18px;width:400px;background:#fafaff;",
      tags$div(
        style = "display:flex;align-items:center;margin-bottom:18px;",
        HTML(generate_svg_shape(ind$shape, 48)),
        tags$span(
          style = "font-size:22px;vertical-align:top;margin-left:16px;",
          toupper(ind$gender)
        )
      ),
      tags$b("Last name: "), ind$last_name, br(),
      tags$b("First name: "), ind$first_name, br(),
      tags$b("Comment: "), tags$em(ind$comment), br(),
      tags$b("Birth date: "), as.character(ind$birth), br(),
      tags$b("Deceased: "), if (isTRUE(ind$deceased)) "Yes" else "No", br(),
      tags$b("Death date: "), as.character(ind$death), br(),
      tags$b("Age: "),
      if (isFALSE(ind$deceased)) {
        ind$age_txt
      } else {
        tagList(tags$span(style = "font-size:22px;", "\u271D "), ind$age_txt)
      }
    )
  })
}

# ======================= APP LAUNCH =======================
shinyApp(ui, server)
