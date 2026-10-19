# =============================================================================
# Pedigree Builder - main Shiny application
# =============================================================================
#
# File map
#   1. Dependencies and module loading
#   2. Shared pedigree/domain helpers
#   3. Pedigree mutation helpers
#   4. Age, date and label helpers
#   5. Plot annotation and rendering helpers
#   6. Phenotype palette and visual motif helpers
#   7. Built-in example pedigrees
#   8. CSS theme and modal/UI helper functions
#   9. Shiny UI
#  10. Shiny server
#
# Clinical boundary
#   The application is intended for education and research only. It is not a
#   validated clinical device and must not be used for diagnosis, medical
#   decision-making, or patient care.
#
# Maintenance notes
#   - Keep package-specific calls namespaced when possible.
#   - Avoid adding new global state unless it is used by both UI and server.
#   - Prefer small helper functions for pedigree mutation and rendering logic.
# =============================================================================

# Dependencies ----------------------------------------------------------------
library(shiny)
library(shinyjs)
library(htmltools)
library(pedtools)
library(DT)
library(colourpicker)
library(ribd)
library(verbalisr)
library(httr)
library(jsonlite)
library(base64enc)
# Optional packages are called with explicit namespaces in the code:
#   ribd, verbalisr, httr, jsonlite, base64enc

# Module and resource discovery ------------------------------------------------
app_dir <- normalizePath(getwd(), mustWork = FALSE)
main_app_files <- c("app.R", "app2.R")
has_main_app <- function(path) {
  any(file.exists(file.path(path, main_app_files)))
}

if (!has_main_app(app_dir)) {
  clean_code_from_root <- file.path(app_dir, "App", "clean code")
  clean_code_from_app <- file.path(app_dir, "clean code")
  if (has_main_app(clean_code_from_root)) {
    app_dir <- normalizePath(clean_code_from_root, mustWork = TRUE)
  } else if (has_main_app(clean_code_from_app)) {
    app_dir <- normalizePath(clean_code_from_app, mustWork = TRUE)
  }
}

support_file <- function(filename) {
  app_parent_dir <- normalizePath(file.path(app_dir, ".."), mustWork = FALSE)
  candidates <- c(
    file.path(app_dir, filename),
    file.path(app_dir, "data", filename),
    file.path(app_parent_dir, filename),
    file.path(app_parent_dir, "data", filename),
    file.path(getwd(), filename),
    file.path(getwd(), "data", filename),
    file.path(getwd(), "draft_test", filename),
    file.path(getwd(), "draft_test", "data", filename),
    file.path(getwd(), "draft_test_publish", filename),
    file.path(getwd(), "draft_test_publish", "data", filename)
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) > 0) {
    return(normalizePath(found[[1]], mustWork = TRUE))
  }
  filename
}

source_app_helpers <- function() {
  helper_dir <- file.path(app_dir, "R")
  helper_files <- c(
    "Formatting_helpers.R",
    "Relationship_helpers.R",
    "scaling_helper.R",
    "edit_helpers.R",
    "annot.R",
    "phenotype.R",
    "select_ped.R",
    "modales.R"
  )

  missing_files <- helper_files[!file.exists(file.path(helper_dir, helper_files))]
  if (length(missing_files) > 0) {
    stop("Missing helper file(s): ", paste(missing_files, collapse = ", "))
  }

  for (helper_file in helper_files) {
    source(file.path(helper_dir, helper_file), local = globalenv())
  }
}

# Gene Explorer is sourced into an isolated environment so helper names from the
# module do not overwrite helpers in this main application file.
.gene_module_env <- new.env(parent = globalenv())
.gene_module_file <- support_file("app_gene_modul_API.R")
if (file.exists(.gene_module_file)) {
  source(.gene_module_file, local = .gene_module_env)
  geneExplorerUI <- .gene_module_env$geneExplorerUI
  geneExplorerServer <- .gene_module_env$geneExplorerServer
}

# GeneReviews Explorer uses a local mapping file and keeps its own UI/server
# helpers isolated in the same way as Gene Explorer.
.review_module_env <- new.env(parent = globalenv())
.review_module_file <- support_file("app_genereviewExplorer.R")
if (file.exists(.review_module_file)) {
  source(.review_module_file, local = .review_module_env)
  .review_module_env$LOCAL_FILE <- support_file("GRshortname_NBKid_genesymbol_dzname.txt")
  reviewExplorerUI <- .review_module_env$reviewExplorerUI
  reviewExplorerServer <- .review_module_env$reviewExplorerServer
}

# The report template may live in data/ in the source tree or beside app.R in
# older/deployed bundles.
report_template_file <- support_file("notes_app_general.html")
report_resource_dir <- if (file.exists(report_template_file)) {
  dirname(report_template_file)
} else {
  getwd()
}
addResourcePath("app_files", report_resource_dir)
addResourcePath("app_www", file.path(app_dir, "www"))


# =============================================================================
# Shiny UI
# =============================================================================

source_app_helpers()

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app_www/styles.css"),
    tags$script(src = "app_www/script.js")
  ),
  tags$nav(
    id = "homeNav",
    class = "navbar-island",
    tags$div(
      class = "navbar-brand",
      tags$span(class = "navbar-brand-icon", tags$i(class = "fa-solid fa-dna")),
      tags$span(class = "navbar-brand-title", "Pedigree Builder")
    ),
    tags$span(class = "navbar-preview", "Preview"),
    tags$div(
      class = "navbar-menu",
      
      tags$a(
        href = "https://genetics-tools.shinyapps.io/tutorial/",
        target = "_blank",
        rel = "noopener noreferrer",
        class = "navbar-help",
        title = "Open tutorial",
        tags$i(class = "fa-regular fa-circle-question")
      )
    )
  ),
  tags$section(
    id = "home",
    class = "page-section active",
    tags$div(
      class = "pedigree-app",
      tags$div(
        class = "pedigree-app__content",
        tags$div(
          class = "pedigree-hero",
          
          tags$h1(class = "pedigree-hero__title", tags$span("Genetic Pedigree")),
          tags$p(class = "pedigree-hero__description", hero_description)
        ),
        tags$div(
          class = "pedigree-actions",
          action_card(
            id = "btn-select",
            icon = "family_history",
            title = "Select",
            subtitle = "Start from a list of pedigree models",
            onclick = "Shiny.setInputValue('selectBtn', Math.random(), {priority: 'event'});"
          ),
          action_card(
            id = "btn-random",
            icon = "casino",
            title = "Random",
            subtitle = "Generate a random pedigree",
            onclick = "Shiny.setInputValue('randomBtn', Math.random(), {priority: 'event'});"
          ),
          action_card(
            id = "btn-load",
            icon = "folder_data",
            title = "Load",
            subtitle = "Load a saved pedigree",
            onclick = "Shiny.setInputValue('loadBtn', Math.random(), {priority: 'event'});"
          )
        )
      ),
      tags$div(
        class = "research-section",
        tags$p(class = "research-section__label", "Build • Annotate • Analyze"),
        tags$div(
          class = "lineup",
          research_item("btn_gene_module", "genetics", "Gene Module", "API Analysis"),
          research_item("btn_review_explorer", "travel_explore", "Review Explorer", "Literature Search"),
          research_item("btn_resources", "library_books", "Resources", "Reference Library"),
          research_item("btn_tools", "calculate", "Tools", "Risk calculators")
        )
      ),
      tags$div(
        class = "pedigree-disclaimer",
        tags$span(class = "material-symbols-outlined", style = "color: #FFFFFF;

text-shadow: 4px 9px 21px rgba(55,46,46,0.7); ", "lens_blur"),
        tags$p(class = "pedigree-disclaimer__text", "Disclaimer"),
        tags$p(
          class = "pedigree-disclaimer__body",
          "This tool is intended for educational and research purposes only. It is not designed for clinical use and should not be used to make medical decisions."
        )
      ),
      tags$footer(
        class = "pedigree-footer",
        tags$div(
          class = "pedigree-footer__content",
          tags$div(
            class = "pedigree-footer__brand",
            tags$div(class = "pedigree-footer__logo", tags$span(class = "material-symbols-outlined", "genetics")),
            tags$span(class = "pedigree-footer__brand-text", "Pedigree Builder")
          ),
          tags$div(
            class = "pedigree-footer__nav",
            tags$span(class = "pedigree-footer__link", actionLink(
              inputId = "btn_about",
              label = "About",
              class = "footer-link"
            )),
            tags$span(class = "pedigree-footer__separator"),
            tags$span(class = "pedigree-footer__link", actionLink(
              inputId = "btn_references",
              label = tagList(tags$i(class = "fa-regular fa-bookmark"), tags$span("References")),
              class = "footer-link"
            )),
            tags$span(class = "pedigree-footer__separator"),
            tags$span(class = "pedigree-footer__link", tags$a(
              href = "mailto:marie.bruneau@outlook.be",
              class = "footer-link",
              tagList(tags$i(class = "fa-regular fa-envelope"), tags$span("Contact"))
            ))
          )
        )
      )
    )
  ),
  tags$input(
    id = "loadRdsRaw",
    type = "file",
    accept = ".rds",
    style = "display: none;"
  ),
  tags$section( style =" background: linear-gradient(90deg, #e6e9f0, #eef1f5);",
                id = "pedigree",
                class = "page-section",
                div(
                  class = "toolbar",
                  tags$button(
                    class = "toolbar__btn",
                    onclick = "showPage('home');",
                    tagList(span(class = "material-symbols-outlined", "home"), span("Home"))
                  ),
                  tags$a(
                    id = "saveFile",
                    class = "toolbar__btn toolbar__btn--icon shiny-download-link",
                    href = "",
                    target = "_blank",
                    download = NA,
                    title = "Save",
                    span(class = "material-symbols-outlined", "save")
                  ),
                  div(
                    class = "toolbar__title",
                    textInput(
                      "pedigree_title_structure",
                      label = NULL,
                      value = "",
                      placeholder = "Enter pedigree title...",
                      width = "100%"
                    )
                  ),
                  div(
                    class = "toolbar__actions",
                    actionButton(
                      "btnUndo",
                      label = NULL,
                      icon = span(class = "material-symbols-outlined", "undo"),
                      class = "toolbar__btn toolbar__btn--icon",
                      title = "Undo"
                    ),
                    actionButton(
                      "btn_delete_selected",
                      label = NULL,
                      icon = span(class = "material-symbols-outlined", "delete"),
                      class = "toolbar__btn toolbar__btn--icon",
                      title = "Delete selected"
                    )
                  )
                ),
                div(
                  class = "layout surface",
                  div(
                    class = "pedigree-sidebar box",
                    h5("Legend / Options"),
                    div(class = "legend-label", "Gender"),
                    div(
                      class = "legend-group",
                      actionButton(
                        "btn_set_male",
                        tagList(span(class = "material-symbols-outlined", "crop_square"), "Male"),
                        class = "legend-btn"
                      ),
                      actionButton(
                        "btn_set_female",
                        tagList(span(class = "material-symbols-outlined", "circle"), "Female"),
                        class = "legend-btn"
                      ),
                      actionButton(
                        "btn_set_unknown",
                        tagList(span(class = "material-symbols-outlined", "thermostat_carbon"), "Undefined"),
                        class = "legend-btn"
                      )
                    ),
                    div(class = "legend-label", "Assigned at Birth"),
                    div(
                      class = "legend-group",
                      actionButton(
                        "AFAB",
                        tagList(span(class = "legend-badge", "AFAB"), "Assigned Female at Birth"),
                        class = "legend-btn"
                      ),
                      actionButton(
                        "AMAB",
                        tagList(span(class = "legend-badge", "AMAB"), "Assigned Male at Birth"),
                        class = "legend-btn"
                      ),
                      actionButton(
                        "UMAB",
                        tagList(span(class = "legend-badge", "UMAB"), "Undetermined at Birth"),
                        class = "legend-btn"
                      )
                    ),
                    div(class = "legend-label", "Status Markers"),
                    div(
                      class = "legend-group",
                      actionButton(
                        "btn_toggle_deceased",
                        tagList(span(class = "material-symbols-outlined", "person_off"), "Deceased"),
                        class = "legend-btn"
                      ),
                      actionButton(
                        "set_miscarriage",
                        tagList(span(class = "material-symbols-outlined", "change_history"), "Miscarriage"),
                        class = "legend-btn"
                      ),
                      actionButton(
                        "set_adopted",
                        tagList(span(class = "material-symbols-outlined", "data_array"), "Adopted"),
                        class = "legend-btn"
                      ),
                      actionButton(
                        "set_proband",
                        tagList(span(class = "material-symbols-outlined", "arrow_outward"), "Proband"),
                        class = "legend-btn"
                      ),
                      actionButton(
                        "fertility",
                        tagList(span(class = "material-symbols-outlined", "align_flex_end"), "Infertility"),
                        class = "legend-btn"
                      )
                    ),
                    div(
                      class = "phenotype-box box",
                      h5("Phenotypes"),
                      actionButton(
                        "newPheno",
                        tagList(span(class = "material-symbols-outlined", "add"), "Create Phenotype"),
                        class = "btn_pheno"
                      ),
                      div(style = "margin-top: 10px;"),
                      uiOutput("phenoButtonsUI")
                    )
                  ),
                  div(
                    class = "pedigree-canvas",
                    div(
                      class = "canvas__header",
                      div(
                        class = "canvas__left",
                        div(
                          class = "canvas__title",
                          span(class = "canvas__title-text", "Draw Pedigree"),
                          span(class = "dtl-line")
                        ),
                        tags$div(
                          class = "canvas__hint",
                          uiOutput("pedigreeDataSummary")
                        )
                      )
                    ),
                    div(
                      class = "pedigree-plot-shell",
                      plotOutput(
                        "pedPlot",
                        width = "100%",
                        height = "100%",
                        click = "pedClick",
                        dblclick = "pedDblClick",
                        hover = hoverOpts("pedHover", delay = 50, delayType = "debounce")
                      ),
                      uiOutput("tooltip"),
                      uiOutput("contextMenu")
                    ),
                    div(
                      class = "pedigree-table-shell",
                      uiOutput("pedigreeTableViewControls"),
                      uiOutput("pedigreeTableContent")
                    )
                  ),
                  div(
                    class = "pedigree-inspector",
                    br(),
                    br(),
                    style ="border-radius: 0px 8px 8px 0px;",
                    div(
                      class = "toggle-buttons-row",
                      div(
                        id = "privacy_button_wrapper",
                        class = "icon-button_bis",
                        `data-tooltip` = "Privacy Mode hides individual details in the side panel. Hover over pedigree symbols to read hidden information when needed.",
                        tags$button(
                          id = "btn_privacy_mode",
                          class = "action-btn_bis",
                          HTML('<span class="material-symbols-outlined">lock_person</span>')
                        ),
                        div(class = "icon-title_bis", "Privacy Mode"),
                        div(id = "privacy_toggle_status", class = "toggle-status", "OFF")
                      ),
                      div(
                        id = "btn2_wrapper",
                        class = "icon-button_bis",
                        `data-tooltip` = "Genetic Stats shows inbreeding, degree and relatedness values for the selected individual.",
                        tags$button(
                          id = "btn2",
                          class = "action-btn_bis",
                          HTML('<span class="material-symbols-outlined">monitoring</span>')
                        ),
                        div(class = "icon-title_bis", "Genetic Stats"),
                        div(id = "btn2_toggle_status", class = "toggle-status", "OFF")
                      ),
                      div(
                        class = "icon-button_bis",
                        actionButton(
                          "btn_information",
                          label = NULL,
                          class = "action-btn_bis",
                          icon = span(class = "material-symbols-outlined", "info")
                        ),
                        div(class = "icon-title_bis", "Information"),
                        div(class = "toggle-status", "Help")
                      ),
                      div(
                        class = "icon-button_bis",
                        actionButton(
                          "btn_plot_settings",
                          label = NULL,
                          class = "action-btn_bis",
                          icon = span(class = "material-symbols-outlined", "tune")
                        ),
                        div(class = "icon-title_bis", "Settings"),
                        div(class = "toggle-status", "Plot")
                      )
                    ),
                    uiOutput("individual_panel"),
                    uiOutput("family_panel"),
                    div(
                      class = "box2",
                      div(
                        class = "section-label-container",
                        span(class = "section-label", "Family"),
                        span(class = "section-badge", "Grid")
                      ),
                      div(
                        class = "family-grid",
                        div(
                          class = "family-grid__section",
                          div(class = "family-grid__section-header", "Parents"),
                          div(
                            class = "family-grid__row family-grid__row--wide",
                            actionButton(
                              "btn_add_parents",
                              tagList(span(class = "material-symbols-outlined", "family_restroom"), "Add Parents"),
                              class = "btn-icon btn-wide"
                            )
                          ),
                          div(class = "family-grid__zone-wrapper", uiOutput("zone_parents"))
                        ),
                        div(
                          class = "family-grid__section",
                          div(class = "family-grid__section-header", "Siblings"),
                          div(
                            class = "family-grid__row",
                            actionButton(
                              "btn_add_sister",
                              tagList(span(class = "material-symbols-outlined", "female"), div("Sister")),
                              class = "btn-icon"
                            ),
                            actionButton(
                              "btn_add_brother",
                              tagList(span(class = "material-symbols-outlined", "male"), div("Brother")),
                              class = "btn-icon"
                            ),
                            actionButton(
                              "btn_add_sibling_unknown",
                              tagList(span(class = "material-symbols-outlined", "radio_button_unchecked"), div("Unknown")),
                              class = "btn-icon"
                            ),
                            actionButton(
                              "btn_add_twins",
                              tagList(span(class = "material-symbols-outlined", "group"), div("Twins")),
                              class = "btn-icon"
                            )
                          ),
                          div(class = "family-grid__zone-wrapper", uiOutput("zone_siblings"))
                        ),
                        div(
                          class = "family-grid__section",
                          div(class = "family-grid__section-header", "Children"),
                          div(
                            class = "family-grid__row",
                            actionButton(
                              "btn_add_daughter",
                              tagList(span(class = "material-symbols-outlined", "female"), div("Daughter")),
                              class = "btn-icon"
                            ),
                            actionButton(
                              "btn_add_son",
                              tagList(span(class = "material-symbols-outlined", "male"), div("Son")),
                              class = "btn-icon"
                            ),
                            actionButton(
                              "btn_add_child_unknown",
                              tagList(span(class = "material-symbols-outlined", "radio_button_unchecked"), div("Unknown")),
                              class = "btn-icon"
                            ),
                            actionButton(
                              "btn_add_miscarriage",
                              tagList(span(class = "material-symbols-outlined", "change_history"), div("Miscarriage")),
                              class = "btn-icon"
                            )
                          ),
                          div(class = "family-grid__zone-wrapper", uiOutput("zone_children"))
                        )
                      )
                    )
                  )
                )
  ),
  uiOutput("individualFloatingWindow"),
  uiOutput("relationshipFloatingWindow"),
  uiOutput("informationMenu"),
  uiOutput("plotSettingsMenu")
)


# =============================================================================
# Shiny Server
# =============================================================================

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {
  # ---------------------------------------------------------------------------
  # Global information and resource modals
  # ---------------------------------------------------------------------------
  observeEvent(input$btn_about, {
    show_app_info_modal(about_modal_ui(), size = "l")
  })
  
  observeEvent(input$btn_references, {
    show_app_info_modal(references_modal_ui(), size = "l")
  })
  
  observeEvent(input$btn_tools, {
    shinyjs::runjs("window.open('https://genetics-tools.shinyapps.io/Punnett_Tools/', '_blank', 'noopener,noreferrer');")
  })
  
  observeEvent(input$btn_resources, {
    showModal(modalDialog(
      title = NULL,
      size = "l",
      easyClose = TRUE,
      footer = NULL,
      resources_modal_ui()
    ))
  })
  
  observeEvent(input$closeResourcesModal, {
    removeModal()
  })
  
  # ---------------------------------------------------------------------------
  # External modules: Gene Explorer and GeneReviews Explorer
  # ---------------------------------------------------------------------------
  if (exists("geneExplorerServer", mode = "function")) {
    geneExplorerServer("gene_explorer")
    observeEvent(input$btn_gene_module, {
      showModal(modalDialog(
        title = NULL,
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tags$style(HTML(
          ".modal-dialog:has(.gene-explorer-modal) { width: min(1280px, calc(100vw - 32px)) !important; max-width: 1280px !important; margin-top: 16px !important; }
           .modal-dialog:has(.gene-explorer-modal) .modal-body { max-height: calc(100vh - 120px); overflow-y: auto; padding: 0 !important; }
           .modal-dialog:has(.gene-explorer-modal) .modal-content { overflow: hidden; border-radius: 18px; }"
        )),
        tags$div(class = "gene-explorer-modal", geneExplorerUI("gene_explorer"))
      ))
    })
  } else {
    modal_observer(input, "btn_gene_module", "Gene Module", "Gene API module file not found.")
  }
  
  if (exists("reviewExplorerServer", mode = "function")) {
    reviewExplorerServer("review_explorer")
    observeEvent(input$btn_review_explorer, {
      showModal(modalDialog(
        title = NULL,
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tags$style(HTML(
          ".modal-dialog:has(.review-explorer-modal) { width: min(1280px, calc(100vw - 32px)) !important; max-width: 1280px !important; margin-top: 16px !important; }
           .modal-dialog:has(.review-explorer-modal) .modal-body { max-height: calc(100vh - 120px); overflow-y: auto; padding: 0 !important; }
           .modal-dialog:has(.review-explorer-modal) .modal-content { overflow: hidden; border-radius: 18px; }"
        )),
        tags$div(class = "review-explorer-modal", reviewExplorerUI("review_explorer"))
      ))
    })
  } else {
    modal_observer(input, "btn_review_explorer", "Review Explorer", "GeneReviews module file not found.")
  }
  
  # ---------------------------------------------------------------------------
  # Browser-side event bridges
  # ---------------------------------------------------------------------------
  # These handlers connect native browser events that Shiny does not expose
  # directly: local RDS loading, report synchronisation and carousel movement.
  shinyjs::runjs(
    "
    if (!window.pedigreeLoadRdsBound) {
      window.pedigreeLoadRdsBound = true;
      document.addEventListener('change', function(event) {
        var loadRdsInput = document.getElementById('loadRdsRaw');
        if (!loadRdsInput || event.target !== loadRdsInput) return;
        var file = loadRdsInput.files[0];
        if (!file) return;
        var reader = new FileReader();
        reader.onload = function(e) {
          var b64 = e.target.result.split(',')[1];
          Shiny.setInputValue('loadRdsData', {name: file.name, data: b64}, {priority: 'event'});
        };
        reader.readAsDataURL(file);
        loadRdsInput.value = '';
      });
    }
    if (!window.pedigreeReportSyncBound) {
      window.pedigreeReportSyncBound = true;
      Shiny.addCustomMessageHandler('syncPedigree', function(msg) {
        try {
          localStorage.setItem('pedigree_sync', JSON.stringify(msg));
        } catch(e) {}
      });
    }
    if (!window.pedigreeDataCarouselBound) {
      window.pedigreeDataCarouselBound = true;
      window.pedigreeDataCarouselIndex = 0;
      window.pedigreeDataCarouselMove = function(direction) {
        var track = document.getElementById('pedigreeDataCarouselTrack');
        if (!track || !track.parentElement) return;
        var cards = track.querySelectorAll('.pedigree-data-card');
        if (!cards.length) return;
        var viewport = track.parentElement;
        var cardWidth = cards[0].getBoundingClientRect().width;
        var gap = parseFloat(window.getComputedStyle(track).gap || '0') || 0;
        var step = Math.max(1, Math.floor((viewport.clientWidth + gap) / (cardWidth + gap)));
        var maxIndex = Math.max(0, cards.length - step);
        window.pedigreeDataCarouselIndex = Math.min(
          Math.max(0, window.pedigreeDataCarouselIndex + direction * step),
          maxIndex
        );
        var offset = window.pedigreeDataCarouselIndex * (cardWidth + gap);
        track.style.transform = 'translateX(-' + offset + 'px)';
      };
      window.addEventListener('resize', function() {
        window.pedigreeDataCarouselIndex = 0;
        var track = document.getElementById('pedigreeDataCarouselTrack');
        if (track) track.style.transform = 'translateX(0)';
      });
    }
    "
  )
  
  # ---------------------------------------------------------------------------
  # Consanguinity / inbreeding-loop inspection
  # ---------------------------------------------------------------------------
  inbreeding_details_open <- reactiveVal(FALSE)
  
  id_to_label <- function(ped_obj, ids) {
    if (length(ids) == 0) {
      return(character(0))
    }
    
    labs <- labels(ped_obj)
    
    out <- vapply(ids, function(z) {
      zi <- suppressWarnings(as.integer(z))
      if (!is.na(zi) && zi >= 1 && zi <= length(labs)) {
        labs[zi]
      } else {
        as.character(z)
      }
    }, character(1))
    
    unname(out)
  }
  output$consanguinityContent <- renderUI({
    req(ped())
    if (!isTRUE(inbreeding_details_open())) {
      return(NULL)
    }
    
    loops <- safe_inbreeding_loops(ped())
    if (length(loops) == 0) {
      return(NULL)
    }
    
    div(
      class = "inbreeding-details-panel",
      h3(class = "inbreeding-details-panel__title", "Inbreeding Loops"),
      describe_inbreeding_content_ui(ped())
    )
  })
  
  observeEvent(input$show_inbreeding_details, {
    req(ped())
    
    loops <- safe_inbreeding_loops(ped())
    if (length(loops) == 0) {
      inbreeding_details_open(FALSE)
      showNotification("No inbreeding loops detected.", type = "message", duration = 3)
      return()
    }
    
    inbreeding_details_open(!isTRUE(inbreeding_details_open()))
  }, ignoreInit = TRUE)
  
  observeEvent(ped(), {
    inbreeding_details_open(FALSE)
  }, ignoreInit = TRUE)
  
  personal_info_pane <- function() {
    div(
      class = "section-page",
      uiOutput("individual_panel")
    )
  }
  personal_family_pane <- function() {
    div(
      class = "section-page",
      uiOutput("family_panel"),
      div(
        class = "family-grid",
        # ── Section: Parents ──
        div(
          class = "family-grid__section",
          div(class = "family-grid__section-header", "Parents"),
          div(
            class = "family-grid__row family-grid__row--wide",
            actionButton(
              "btn_add_parents",
              tagList(
                tags$span(class = "material-symbols-outlined", "family_restroom"),
                "Add Parents"
              ),
              class = "btn-icon btn-wide"
            )
          ),
          div(class = "family-grid__zone-wrapper", uiOutput("zone_parents"))
        ),
        div(class = "family-grid__divider"),
        # ── Section: Siblings ──
        div(
          class = "family-grid__section",
          div(class = "family-grid__section-header", "Siblings"),
          div(
            class = "family-grid__row",
            actionButton(
              "btn_add_sister",
              tagList(tags$span(class = "material-symbols-outlined", "female"), div("Sister")),
              class = "btn-icon"
            ),
            actionButton(
              "btn_add_brother",
              tagList(tags$span(class = "material-symbols-outlined", "male"), div("Brother")),
              class = "btn-icon"
            ),
            actionButton(
              "btn_add_sibling_unknown",
              tagList(tags$span(class = "material-symbols-outlined", "agender"), div("Unknown")),
              class = "btn-icon"
            ),
            actionButton(
              "btn_add_twins",
              tagList(tags$span(class = "material-symbols-outlined", "group"), div("Twins")),
              class = "btn-icon"
            )
          ),
          div(class = "family-grid__zone-wrapper", uiOutput("zone_siblings"))
        ),
        div(class = "family-grid__divider"),
        # ── Section: Children ──
        div(
          class = "family-grid__section",
          div(class = "family-grid__section-header", "Children"),
          div(
            class = "family-grid__row",
            actionButton(
              "btn_add_daughter",
              tagList(tags$span(class = "material-symbols-outlined", "female"), div("Daughter")),
              class = "btn-icon"
            ),
            actionButton(
              "btn_add_son",
              tagList(tags$span(class = "material-symbols-outlined", "male"), div("Son")),
              class = "btn-icon"
            ),
            actionButton(
              "btn_add_child_unknown",
              tagList(tags$span(class = "material-symbols-outlined", "agender"), div("Unknown")),
              class = "btn-icon"
            ),
            actionButton(
              "btn_add_miscarriage",
              tagList(tags$span(class = "material-symbols-outlined", "change_history"), div("Miscarriage")),
              class = "btn-icon"
            )
          ),
          div(class = "family-grid__zone-wrapper", uiOutput("zone_children"))
        )
      )
    )
  }
  format_path_label <- function(ped_obj, top, path, bottom) {
    nodes <- c(top, path, bottom)
    paste(id_to_label(ped_obj, nodes), collapse = " \u2192 ")
  }
  observeEvent(input$anim_choice, {
    all_classes <- c(
      "anim-neumorphic",
      "anim-glow",
      "anim-bounce",
      "anim-underline",
      "anim-fill",
      "anim-rotate",
      "anim-border",
      "anim-glass",
      "anim-ripple",
      "anim-neon"
    )
    for (cls in all_classes) {
      removeCssClass("lineup_container", cls)
    }
    addCssClass("lineup_container", input$anim_choice)
    session$sendCustomMessage("resetToggles", list())
  })
  ped <- reactiveVal(NULL)
  sel <- reactiveVal(character(0))
  ctrs <- reactiveVal(NULL)
  ctx_id <- reactiveVal(NULL)
  hist <- reactiveValues(s = list())
  values <- reactiveValues(pedData = NULL)
  
  # ── Status markers ──
  deceased_ids <- reactiveVal(character(0))
  carrier_ids <- reactiveVal(character(0))
  starred_ids <- reactiveVal(character(0))
  proband_id <- reactiveVal(character(0))
  adopted_ids <- reactiveVal(character(0))
  miscarriage <- reactiveVal(character(0))
  infertility_ids <- reactiveVal(character(0))
  twins_df <- reactiveVal(data.frame(
    id1 = character(),
    id2 = character(),
    code = integer(),
    stringsAsFactors = FALSE
  ))
  
  # ── Assigned at birth ──
  afab_ids <- reactiveVal(character(0))
  amab_ids <- reactiveVal(character(0))
  umab_ids <- reactiveVal(character(0))
  
  # ── Text annotations (9 positions) ──
  text_annotations <- reactiveValues(
    top = list(),
    bottom = list(),
    left = list(),
    right = list(),
    topleft = list(),
    topright = list(),
    bottomleft = list(),
    bottomright = list(),
    inside = list()
  )
  # ── Text annotation style ──
  annot_style <- reactiveValues(
    cex = 1.0,
    font = 2, # 1=normal, 2=bold, 3=italic, 4=bold-italic
    col = "#1976D2"
  )
  
  # ── Plot display settings ──
  plot_settings <- reactiveValues(
    cex = 1.0,
    symbolsize = 1.0,
    mar = 5
  )
  information_menu_open <- reactiveVal(FALSE)
  plot_settings_menu_open <- reactiveVal(FALSE)
  
  # ── Phenotype state ──
  pheno_styles <- reactiveValues(
    fill = character(0),
    hatched = character(0),
    lty = character(0),
    col = character(0)
  )
  
  phenotypes <- reactiveValues(
    list = list(),
    assign = list()
  )
  
  pheno_editing <- reactiveVal(NULL)
  motif_config_mode <- reactiveVal(FALSE)
  motif_symbol_config <- reactiveVal(list())
  motif_config_preview <- reactiveVal(NULL)
  motif_config_symbol <- reactiveVal(NULL)
  
  # ── System 1 Quick Add state ──
  relative_mode <- reactiveVal(NULL)
  sibling_type <- reactiveVal("full")
  sibling_sex <- reactiveVal(1)
  sibling_number <- reactiveVal(1)
  shared_parent_val <- reactiveVal("father")
  twin_mode <- reactiveVal("twin")
  twin_type_val <- reactiveVal(2)
  twin_sex_val <- reactiveVal(1)
  triplet_sex2 <- reactiveVal(1)
  triplet_sex3 <- reactiveVal(2)
  child_sex <- reactiveVal(1)
  child_is_miscarriage <- reactiveVal(FALSE)
  child_number <- reactiveVal(1)
  
  # ── System 2 Family Grid state ──
  state <- reactiveValues(
    selected_type = NULL,
    click_count = 0,
    count = 1,
    sib_kinship = "full",
    half_shared_parent = "father",
    twins_zygosity = "unknown",
    twins_gender = "other",
    twins_mode = "twin",
    triplet_sex2 = "unknown",
    triplet_sex3 = "unknown",
    selected_partner = NULL
  )
  
  # ── Display modes ──
  display_classic_mode <- reactiveVal(TRUE) # TRUE = normal, FALSE = privacy ON
  display_stats_mode <- reactiveVal(FALSE) # TRUE = show genetic stats
  
  # ── Conditions UI ──
  output$hasSel <- reactive(length(sel()) > 0)
  outputOptions(output, "hasSel", suspendWhenHidden = FALSE)
  
  output$hasPh <- reactive(length(phenotypes$list) > 0)
  outputOptions(output, "hasPh", suspendWhenHidden = FALSE)
  
  # ── History ──
  annotation_positions <- c(
    "top",
    "bottom",
    "left",
    "right",
    "topleft",
    "topright",
    "bottomleft",
    "bottomright",
    "inside"
  )
  
  save_hist <- function() {
    snap <- list(
      ped = ped(),
      sel = sel(),
      pheno_list = phenotypes$list,
      pheno_assign = phenotypes$assign,
      deceased_ids = deceased_ids(),
      carrier_ids = carrier_ids(),
      starred_ids = starred_ids(),
      proband_id = proband_id(),
      adopted_ids = adopted_ids(),
      miscarriage = miscarriage(),
      infertility_ids = infertility_ids(),
      afab_ids = afab_ids(),
      amab_ids = amab_ids(),
      umab_ids = umab_ids(),
      twins_df = twins_df(),
      pedData = values$pedData,
      text_annotations = setNames(
        lapply(annotation_positions, function(pos) text_annotations[[pos]]),
        annotation_positions
      ),
      annot_style = list(
        cex = annot_style$cex,
        font = annot_style$font,
        col = annot_style$col
      ),
      plot_settings = list(
        cex = plot_settings$cex,
        symbolsize = plot_settings$symbolsize,
        mar = plot_settings$mar
      ),
      title = input$pedigree_title_structure %||% "",
      motif_symbol_config = motif_symbol_config()
    )
    hist$s <- c(list(snap), hist$s)
    if (length(hist$s) > 20) hist$s <- hist$s[1:20]
  }
  
  rand_ped <- function() {
    relabel_gen(randomPed(
      n = sample(3:8, 1),
      maxDirectGap = Inf,
      selfing = FALSE
    ))
  }
  
  # ── Rebuild pheno_styles from phenotypes ──
  rebuildPhenoStyles <- function() {
    pheno_styles$fill <- character(0)
    pheno_styles$hatched <- character(0)
    pheno_styles$lty <- character(0)
    pheno_styles$col <- character(0)
    
    if (length(phenotypes$assign) == 0) {
      return(invisible(NULL))
    }
    
    for (nm in names(phenotypes$assign)) {
      ids <- phenotypes$assign[[nm]] %||% character(0)
      if (!nm %in% names(phenotypes$list) || length(ids) == 0) {
        next
      }
      
      ph <- phenotypes$list[[nm]]
      
      if (!identical(ph$type, "motif")) {
        mode <- ph$mode %||% ph$type %||% "fill"
        
        if (mode %in% c("fill", "hatched") && !is.null(ph$fill)) {
          for (id in ids) {
            pheno_styles$fill[id] <- ph$fill
          }
        }
        
        if (mode %in% c("border", "hatched") && !is.null(ph$col)) {
          for (id in ids) {
            pheno_styles$col[id] <- ph$col
          }
        }
        
        if (isTRUE(ph$hatched) || identical(mode, "hatched")) {
          pheno_styles$hatched <- unique(c(pheno_styles$hatched, ids))
        }
        
        if (!is.null(ph$lty) && !identical(ph$lty, "solid")) {
          for (id in ids) {
            pheno_styles$lty[id] <- ph$lty
          }
        }
      }
    }
    
    invisible(NULL)
  }
  
  # ── relabel_and_update: master function for structural changes ──
  relabel_and_update <- function(old_ped, new_ped) {
    # Collect current styles (with IDs BEFORE relabeling)
    styles_list <- list(
      fill = NULL,
      hatched = NULL,
      carrier = if (length(carrier_ids())) carrier_ids() else NULL,
      aff = NULL,
      dashed = NULL,
      deceased = if (length(deceased_ids())) deceased_ids() else NULL,
      proband = if (length(proband_id())) proband_id() else NULL,
      starred = if (length(starred_ids())) starred_ids() else NULL,
      adopted = if (length(adopted_ids())) adopted_ids() else NULL,
      infertility = if (length(infertility_ids())) {
        infertility_ids()
      } else {
        NULL
      },
      afab = if (length(afab_ids())) afab_ids() else NULL,
      amab = if (length(amab_ids())) amab_ids() else NULL,
      umab = if (length(umab_ids())) umab_ids() else NULL
    )
    
    # Package pedigree with twins and miscarriage (IDs BEFORE relabeling)
    pedigree_list <- list(
      ped = new_ped,
      twins = if (nrow(twins_df()) > 0) twins_df() else NULL,
      miscarriage = if (length(miscarriage())) miscarriage() else NULL
    )
    
    # Relabel and remap everything
    updated <- updateLabelsData(
      pedigree = pedigree_list,
      styles = styles_list,
      textAnnot = NULL,
      new = "generations"
    )
    
    # Update pedigree
    ped(updated$ped)
    
    # Update twins
    if (!is.null(updated$twins)) {
      twins_df(updated$twins)
    } else {
      twins_df(data.frame(
        id1 = character(),
        id2 = character(),
        code = integer(),
        stringsAsFactors = FALSE
      ))
    }
    
    # Update miscarriage
    if (!is.null(updated$miscarriage)) {
      miscarriage(updated$miscarriage)
    } else {
      miscarriage(character(0))
    }
    
    # Update all marker vectors
    deceased_ids(updated$styles$deceased %||% character(0))
    carrier_ids(updated$styles$carrier %||% character(0))
    starred_ids(updated$styles$starred %||% character(0))
    proband_id(updated$styles$proband %||% character(0))
    adopted_ids(updated$styles$adopted %||% character(0))
    infertility_ids(updated$styles$infertility %||% character(0))
    afab_ids(updated$styles$afab %||% character(0))
    amab_ids(updated$styles$amab %||% character(0))
    umab_ids(updated$styles$umab %||% character(0))
    
    # Remap phenotype assignments
    if (!is.null(updated$idMap) && length(phenotypes$assign) > 0) {
      idMap <- updated$idMap
      for (nm in names(phenotypes$assign)) {
        old_ids <- phenotypes$assign[[nm]]
        if (length(old_ids) > 0) {
          new_ids <- as.character(idMap[as.character(old_ids)])
          new_ids <- new_ids[!is.na(new_ids)]
          phenotypes$assign[[nm]] <- if (length(new_ids) > 0) {
            new_ids
          } else {
            character(0)
          }
        }
      }
      rebuildPhenoStyles()
    }
    
    # Remap pedData
    if (!is.null(values$pedData) && !is.null(updated$idMap)) {
      idMap <- updated$idMap
      new_pedData <- init_pedData(updated$ped)
      for (i in seq_len(nrow(new_pedData))) {
        new_id <- new_pedData$id[i]
        old_id <- names(idMap)[idMap == new_id]
        if (length(old_id) == 1) {
          old_row <- values$pedData[
            values$pedData$id == old_id[1], ,
            drop = FALSE
          ]
          if (nrow(old_row) == 1) {
            for (col in c(
              "first_name",
              "last_name",
              "date_of_birth",
              "deceased",
              "date_of_death",
              "age",
              "age_unit",
              "comments",
              "assigned_at_birth"
            )) {
              if (
                col %in%
                names(old_row) &&
                col %in% names(new_pedData)
              ) {
                new_pedData[[col]][i] <- old_row[[col]][1]
              }
            }
          }
        }
      }
      values$pedData <- new_pedData
    } else {
      values$pedData <- init_pedData(updated$ped)
    }
    
    # Remap text annotations
    if (!is.null(updated$idMap)) {
      idMap <- updated$idMap
      for (pos in c(
        "top",
        "bottom",
        "left",
        "right",
        "topleft",
        "topright",
        "bottomleft",
        "bottomright",
        "inside"
      )) {
        if (length(text_annotations[[pos]]) > 0) {
          old_annots <- text_annotations[[pos]]
          new_annots <- list()
          for (old_id in names(old_annots)) {
            new_id <- as.character(idMap[old_id])
            if (!is.na(new_id)) {
              new_annots[[new_id]] <- old_annots[[old_id]]
            }
          }
          text_annotations[[pos]] <- new_annots
        }
      }
    }
    
    # Remap selection
    if (!is.null(updated$idMap) && length(sel()) > 0) {
      new_sel <- as.character(updated$idMap[as.character(sel())])
      new_sel <- new_sel[!is.na(new_sel)]
      if (length(new_sel) > 0) sel(new_sel) else sel(character(0))
    }
    
    return(updated$ped)
  }
  
  # ── Open phenotype modal ──
  openPhenoModal <- function(prefill = NULL) {
    default_name <- if (!is.null(prefill)) prefill$name else ""
    default_type <- if (!is.null(prefill)) {
      current_type <- prefill$mode %||% prefill$type %||% "fill"
      if (identical(current_type, "hatched")) "fill" else current_type
    } else {
      "fill"
    }
    default_fill <- if (!is.null(prefill)) {
      (prefill$fill %||% "#EF5350")
    } else {
      "#EF5350"
    }
    default_col <- if (!is.null(prefill)) {
      (prefill$col %||% "#000000")
    } else {
      "#000000"
    }
    default_lty <- if (!is.null(prefill)) {
      (prefill$lty %||% "solid")
    } else {
      "solid"
    }
    default_hat <- if (!is.null(prefill)) {
      (prefill$hatched %||% FALSE)
    } else {
      FALSE
    }
    default_sym <- if (!is.null(prefill)) {
      (prefill$symbol %||% "\u2605")
    } else {
      "\u2605"
    }
    default_mcol <- if (!is.null(prefill)) {
      (prefill$motif_color %||% "#D32F2F")
    } else {
      "#D32F2F"
    }
    default_pos <- if (!is.null(prefill)) {
      (prefill$position %||% "center")
    } else {
      "center"
    }
    if (!default_pos %in% unname(motif_position_choices)) {
      default_pos <- "center"
    }
    motif_symbol_choices <- motif_choice_labels(default_pos)
    if (!default_sym %in% unname(motif_symbol_choices)) {
      default_sym <- unname(motif_symbol_choices)[1]
    }
    modal_title <- if (is.null(prefill)) {
      "New Phenotype"
    } else {
      paste("Edit:", prefill$name)
    }
    
    showModal(modalDialog(
      title = tags$div(
        style = "display:flex; align-items:center; gap:10px;",
        icon("palette"),
        modal_title
      ),
      textInput(
        "pheno_name",
        "Phenotype Name",
        value = default_name,
        placeholder = "e.g., Affected, Carrier, Variant"
      ),
      div(
        class = "pheno-type-row",
        tags$label(class = "control-label", "Type:"),
        div(
          style = "display:none;",
          radioButtons(
            "pheno_type",
            NULL,
            choices = c(
              "fill" = "fill",
              "border" = "border",
              "motif" = "motif"
            ),
            selected = default_type,
            inline = TRUE
          )
        ),
        div(
          class = "segment-control",
          tags$button(
            type = "button",
            class = paste(
              "segment-btn",
              if (default_type == "fill") "active" else ""
            ),
            "Fill",
            onclick = paste0(
              "$(this).addClass('active').siblings('.segment-btn').removeClass('active');",
              "$('#pheno_type input[value=fill]').prop('checked',true).trigger('change');"
            )
          ),
          tags$button(
            type = "button",
            class = paste(
              "segment-btn",
              if (default_type == "border") "active" else ""
            ),
            "Border",
            onclick = paste0(
              "$(this).addClass('active').siblings('.segment-btn').removeClass('active');",
              "$('#pheno_type input[value=border]').prop('checked',true).trigger('change');"
            )
          ),
          tags$button(
            type = "button",
            class = paste(
              "segment-btn",
              if (default_type == "motif") "active" else ""
            ),
            "Motif",
            onclick = paste0(
              "$(this).addClass('active').siblings('.segment-btn').removeClass('active');",
              "$('#pheno_type input[value=motif]').prop('checked',true).trigger('change');"
            )
          )
        )
      ),
      div(
        class = "preview-container",
        div(class = "preview-label", "Preview"),
        uiOutput("previewPheno")
      ),
      conditionalPanel(
        "input.pheno_type == 'fill'",
        div(
          class = "pheno-fields",
          colourInput(
            "pheno_fill",
            "Fill Color",
            value = default_fill,
            palette = "limited",
            allowedCols = PHENO_COLOR_PALETTE,
            returnName = FALSE,
            showColour = "background",
            closeOnClick = TRUE
          ),
          checkboxInput(
            "pheno_hatched",
            "Hatched pattern",
            value = default_hat
          )
        )
      ),
      conditionalPanel(
        "input.pheno_type == 'border'",
        div(
          class = "pheno-fields",
          colourInput(
            "pheno_col",
            "Border Color",
            value = default_col,
            palette = "limited",
            allowedCols = PHENO_COLOR_PALETTE,
            returnName = FALSE,
            showColour = "background",
            closeOnClick = TRUE
          ),
          selectInput(
            "pheno_lty",
            "Border Style",
            choices = c(
              "Solid" = "solid",
              "Dashed" = "dashed",
              "Dotted" = "dotted",
              "Dot-Dash" = "dotdash"
            ),
            selected = default_lty
          )
        )
      ),
      conditionalPanel(
        "input.pheno_type == 'motif'",
        div(
          class = "pheno-fields",
          selectInput(
            "pheno_motif_pos",
            "Position",
            choices = motif_position_choices,
            selected = default_pos
          ),
          uiOutput("phenoMotifPicker"),
          colourInput(
            "pheno_motif_color",
            "Symbol Color",
            value = default_mcol,
            palette = "limited",
            allowedCols = PHENO_COLOR_PALETTE,
            returnName = FALSE,
            showColour = "background",
            closeOnClick = TRUE
          )
        )
      ),
      footer = tagList(
        actionButton(
          "cancelPheno",
          "Cancel",
          class = "btn btn-default"
        ),
        actionButton(
          "savePheno",
          if (is.null(prefill)) {
            HTML('<i class="fa fa-check"></i> Create')
          } else {
            HTML('<i class="fa fa-save"></i> Save')
          },
          class = "btn btn-primary"
        )
      ),
      size = "s",
      easyClose = FALSE
    ))
    shinyjs::runjs("$('.modal').addClass('pheno-modal');")
  }
  personal_relationship_pane <- function() {
    div(
      class = "section-page",
      div(
        class = "structure-card",
        p(class = "structure-card__label", "Relationship Calculation"),
        p(
          class = "structure-card__text",
          "Compare two individuals and inspect their family relationship, kinship and identity-by-descent coefficients."
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "structure-field",
            p(class = "structure-field__label", "Individual A"),
            div(
              class = "structure-field__shell",
              uiOutput("relationshipPersonAReadonly")
            )
          )
        ),
        column(
          6,
          div(
            class = "structure-field",
            p(class = "structure-field__label", "Individual B"),
            div(
              class = "structure-field__shell",
              selectInput(
                "person_b",
                label = NULL,
                choices = character(0),
                selected = NULL,
                width = "100%",
                selectize = FALSE
              )
            )
          )
        )
      ),
      div(
        class = "structure-actions",
        actionButton(
          "analyze_relationship",
          "Analyze Relationship",
          class = "btn btn-primary"
        )
      ),
      uiOutput("relationshipHints"),
      div(
        class = "structure-card",
        p(class = "structure-card__label", "Relationship Description"),
        verbatimTextOutput("rel_text_personal", placeholder = TRUE)
      ),
      div(
        class = "structure-card",
        p(class = "structure-card__label", "IBD / Kinship Coefficients"),
        tableOutput("rel_kappa_personal"),
        tags$details(
          class = "coef-legend",
          tags$summary("What do these coefficients mean?"),
          tags$dl(
            class = "coef-legend__list",
            tags$dt("f1, f2"), tags$dd("Inbreeding coefficients of Individual A and B — probability that their two alleles at a locus are identical by descent."),
            tags$dt(HTML("&phi; (phi)")), tags$dd("Kinship coefficient — probability that a random allele from A is IBD with a random allele from B. Equals the inbreeding coefficient of their hypothetical offspring."),
            tags$dt("deg"), tags$dd("Degree of relationship (e.g. 1 = parent/child or full sibs, 2 = grandparent or half-sibs, 3 = first cousins)."),
            tags$dt("k0, k1, k2"), tags$dd("IBD kappa coefficients — probabilities that a non-inbred pair shares 0, 1 or 2 alleles IBD at a random locus (k0 + k1 + k2 = 1).")
          )
        )
      ),
      conditionalPanel(
        condition = "input.showCanonicalPersonal == true",
        div(
          class = "structure-card",
          p(class = "structure-card__label", "Canonical Relationship"),
          verbatimTextOutput("rel_canonical_personal", placeholder = TRUE)
        )
      ),
      checkboxInput(
        "showCanonicalPersonal",
        "Show canonical relationship from constructPedigree",
        value = FALSE
      ),
      div(
        class = "structure-card",
        style = "display: none;",
        p(class = "structure-card__label", "Individual A Metrics"),
        p(
          class = "structure-card__text",
          "Genealogical and inbreeding metrics for the first selected individual."
        ),
        DTOutput("relInfoTable")
      ),
      div(
        class = "structure-card",
        p(class = "structure-card__label", "Top Related to Individual A"),
        p(
          class = "structure-card__text",
          "Individuals with the highest kinship values relative to Individual A."
        ),
        DTOutput("relTopRelatedTable")
      ),
      div(
        class = "structure-card",
        p(class = "structure-card__label", "Pairwise Relationship Metrics"),
        p(
          class = "structure-card__text",
          "Detailed pairwise relationship statistics between Individual A and Individual B."
        ),
        DTOutput("relPairwiseTable")
      )
    )
  }
  # ── Helper: initialize pedData for a pedigree ──
  init_pedData <- function(p) {
    ids <- labels(p)
    data.frame(
      id = ids,
      sex = pedtools::getSex(p, ids),
      first_name = "",
      last_name = "",
      assigned_at_birth = "",
      date_of_birth = "",
      date_of_death = "",
      age = "",
      age_unit = "years",
      deceased = FALSE,
      comments = "",
      stringsAsFactors = FALSE
    )
  }
  
  # ── Helper: reset all markers ──
  reset_markers <- function() {
    deceased_ids(character(0))
    carrier_ids(character(0))
    starred_ids(character(0))
    proband_id(character(0))
    adopted_ids(character(0))
    miscarriage(character(0))
    infertility_ids(character(0))
    afab_ids(character(0))
    amab_ids(character(0))
    umab_ids(character(0))
    twins_df(data.frame(
      id1 = character(),
      id2 = character(),
      code = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  reset_text_annotations <- function() {
    for (pos in annotation_positions) {
      text_annotations[[pos]] <- list()
    }
  }
  
  restore_snapshot <- function(snap) {
    req(!is.null(snap), !is.null(snap$ped))
    
    ped(snap$ped)
    sel(snap$sel %||% character(0))
    phenotypes$list <- snap$pheno_list %||% list()
    phenotypes$assign <- snap$pheno_assign %||% list()
    deceased_ids(snap$deceased_ids %||% character(0))
    carrier_ids(snap$carrier_ids %||% character(0))
    starred_ids(snap$starred_ids %||% character(0))
    proband_id(snap$proband_id %||% character(0))
    adopted_ids(snap$adopted_ids %||% character(0))
    miscarriage(snap$miscarriage %||% character(0))
    infertility_ids(snap$infertility_ids %||% character(0))
    afab_ids(snap$afab_ids %||% character(0))
    amab_ids(snap$amab_ids %||% character(0))
    umab_ids(snap$umab_ids %||% character(0))
    twins_df(snap$twins_df %||% data.frame(
      id1 = character(),
      id2 = character(),
      code = integer(),
      stringsAsFactors = FALSE
    ))
    values$pedData <- snap$pedData %||% init_pedData(snap$ped)
    
    reset_text_annotations()
    if (!is.null(snap$text_annotations)) {
      for (pos in intersect(names(snap$text_annotations), annotation_positions)) {
        text_annotations[[pos]] <- snap$text_annotations[[pos]]
      }
    }
    
    if (!is.null(snap$annot_style)) {
      annot_style$cex <- snap$annot_style$cex %||% annot_style$cex
      annot_style$font <- snap$annot_style$font %||% annot_style$font
      annot_style$col <- snap$annot_style$col %||% annot_style$col
    }
    
    if (!is.null(snap$plot_settings)) {
      plot_settings$cex <- snap$plot_settings$cex %||% plot_settings$cex
      plot_settings$symbolsize <- snap$plot_settings$symbolsize %||% plot_settings$symbolsize
      plot_settings$mar <- snap$plot_settings$mar %||% plot_settings$mar
      updateNumericInput(session, "plot_text_size", value = plot_settings$cex)
      updateNumericInput(session, "plot_symbol_size", value = plot_settings$symbolsize)
      updateNumericInput(session, "plot_margins", value = plot_settings$mar)
    }
    
    if (!is.null(snap$title)) {
      updateTextInput(session, "pedigree_title_structure", value = snap$title)
    }
    
    motif_symbol_config(snap$motif_symbol_config %||% motif_symbol_config())
    motif_config_mode(FALSE)
    motif_config_preview(NULL)
    motif_config_symbol(NULL)
    rebuildPhenoStyles()
    invisible(TRUE)
  }
  
  observeEvent(input$btnUndo, {
    if (length(hist$s) == 0) {
      showNotification("Nothing to undo", type = "warning", duration = 2)
      return()
    }
    
    snap <- hist$s[[1]]
    hist$s <- hist$s[-1]
    restore_snapshot(snap)
    showNotification("Undo applied", type = "message", duration = 2)
  })
  
  load_family_pedigree <- function(family_name) {
    req(family_name %in% names(families))
    new_ped <- families[[family_name]]()
    new_ped <- relabel_gen(new_ped)
    
    ped(new_ped)
    values$pedData <- init_pedData(new_ped)
    sel(character(0))
    reset_markers()
    reset_text_annotations()
    phenotypes$list <- list()
    phenotypes$assign <- list()
    pheno_styles$fill <- character(0)
    pheno_styles$hatched <- character(0)
    pheno_styles$lty <- character(0)
    pheno_styles$col <- character(0)
    hist$s <- list()
  }
  
  get_effective_motif_configs <- reactive({
    saved <- motif_symbol_config()
    preview <- motif_config_preview()
    if (!is.null(preview) && !is.null(preview$symbol)) {
      saved[[preview$symbol]] <- list(
        cex = preview$cex,
        dx = preview$dx,
        dy = preview$dy
      )
    }
    saved
  })
  
  # ── Default pedigree on app launch ──
  default_family_name <- "Ancestral: 3 gen back"
  load_family_pedigree(default_family_name)
  
  observeEvent(input$selectBtn, {
    first_template <- names(families)[1]
    first_preview <- pedigree_previews[[first_template]]
    filter_js <- paste0(
      "var filter=this.dataset.filter;",
      "var root=this.closest('.select-modal-container');",
      "root.querySelectorAll('.select-modal-filters button').forEach(function(btn){btn.classList.remove('active');});",
      "this.classList.add('active');",
      "var visibleCount=0;",
      "root.querySelectorAll('.select-card').forEach(function(card){",
      "var show=filter==='all'||card.dataset.category===filter;",
      "card.style.display=show?'flex':'none';",
      "if(show) visibleCount++;",
      "});",
      "var count=root.querySelector('#selectTemplateCount');",
      "if(count){count.textContent=visibleCount+' templates';}"
    )
    
    showModal(modalDialog(
      title = NULL,
      size = "l",
      easyClose = TRUE,
      tags$div(
        class = "select-modal-container",
        tags$aside(
          class = "select-modal-aside",
          tags$p(class = "select-modal-kicker", "Template library"),
          tags$h2(class = "select-modal-header__title", "Select a pedigree"),
          tags$p(
            class = "select-modal-header__subtitle",
            "Choose a starting structure, name the family if needed, then load it into the workspace."
          ),
          tags$div(
            class = "select-modal-header__input",
            tags$span(class = "material-symbols-outlined input-icon", "edit"),
            textInput(
              "create_title",
              NULL,
              placeholder = "Family name (optional)",
              width = "100%"
            )
          ),
          tags$div(
            class = "select-selected-preview",
            tags$p(class = "select-selected-preview__label", "Current Selection"),
            tags$div(
              id = "selectCurrentPreview",
              class = "select-selected-preview__art",
              if (!is.null(first_preview)) {
                tags$img(src = first_preview, alt = first_template)
              } else {
                tags$span(class = "material-symbols-outlined", "account_tree")
              }
            ),
            tags$p(id = "selectCurrentName", class = "select-selected-preview__name", first_template),
            tags$p(class = "select-selected-preview__meta", "Pedigree")
          )
        ),
        tags$input(
          type = "hidden",
          id = "create_ped_choice",
          class = "shiny-bound-input",
          value = first_template
        ),
        tags$main(
          class = "select-modal-main",
          tags$div(
            class = "select-modal-toolbar",
            tags$div(
              class = "select-modal-filters",
              tags$button(type = "button", class = "active", `data-filter` = "all", onclick = filter_js, "All"),
              tags$button(type = "button", `data-filter` = "nuclear", onclick = filter_js, "Nuclear"),
              tags$button(type = "button", `data-filter` = "extended", onclick = filter_js, "Extended")
            ),
            tags$span(
              id = "selectTemplateCount",
              class = "select-modal-count",
              sprintf("%d templates", length(families))
            )
          ),
          tags$div(
            class = "select-cards-grid",
            lapply(seq_along(families), function(i) {
              ped_name <- names(families)[i]
              preview_src <- pedigree_previews[[ped_name]]
              ped_name_js <- jsonlite::toJSON(ped_name, auto_unbox = TRUE)
              ped_category <- if (grepl("^Nuclear", ped_name)) "nuclear" else "extended"
              
              tags$div(
                class = paste("select-card", if (i == 1) "selected" else ""),
                `data-value` = ped_name,
                `data-category` = ped_category,
                onclick = sprintf(
                  paste0(
                    "document.querySelectorAll('.select-card').forEach(function(card) {",
                    "card.classList.remove('selected');",
                    "});",
                    "this.classList.add('selected');",
                    "document.getElementById('create_ped_choice').value = %s;",
                    "var preview = this.querySelector('.select-card__preview');",
                    "var name = this.querySelector('.select-card__name');",
                    "var currentPreview = document.getElementById('selectCurrentPreview');",
                    "var currentName = document.getElementById('selectCurrentName');",
                    "if (preview && currentPreview) currentPreview.innerHTML = preview.innerHTML;",
                    "if (name && currentName) currentName.textContent = name.textContent;",
                    "Shiny.setInputValue('create_ped_choice', %s, {priority: 'event'});"
                  ),
                  ped_name_js,
                  ped_name_js
                ),
                tags$div(
                  class = "select-card__preview",
                  if (!is.null(preview_src)) {
                    tags$img(src = preview_src, alt = ped_name)
                  } else {
                    tags$span(class = "material-symbols-outlined", "account_tree")
                  }
                ),
                tags$div(class = "select-card__name", ped_name),
                tags$div(class = "select-card__meta", "Pedigree")
              )
            })
          ),
          tags$div(
            class = "select-modal-footer",
            tags$div(
              class = "select-modal-footer__hint",
              tags$span(class = "material-symbols-outlined", "touch_app"),
              tags$span("Select a template to preview and confirm your choice.")
            ),
            tags$div(
              class = "select-modal-footer__actions",
              actionButton("create_cancel_btn", "Cancel", class = "select-cancel-btn"),
              actionButton(
                "create_confirm_btn",
                "Confirm",
                class = "select-confirm-btn"
              )
            )
          )
        )
      ),
      footer = NULL
    ))
  })
  
  observeEvent(input$create_cancel_btn, {
    removeModal()
  })
  
  observeEvent(input$create_confirm_btn, {
    choice <- input$create_ped_choice %||% names(families)[1]
    req(choice, choice %in% names(families))
    
    load_family_pedigree(choice)
    
    title_value <- input$create_title %||% ""
    if (nzchar(title_value)) {
      updateTextInput(session, "pedigree_title_structure", value = title_value)
    }
    
    removeModal()
    shinyjs::runjs("showPage('pedigree');")
  })
  
  preview_ped <- reactiveVal(NULL)
  
  observeEvent(input$randomBtn, {
    preview_ped(rand_ped())
    showModal(modalDialog(
      title = NULL,
      size = "l",
      easyClose = TRUE,
      tags$div(
        class = "random-modal-container",
        tags$aside(
          class = "random-modal-aside",
          tags$p(class = "random-modal-kicker", "Generator"),
          tags$h2(class = "random-modal-title", "Random Pedigree"),
          tags$p(
            class = "random-modal-subtitle",
            "Generate a new family structure, tune the size range, then confirm when the preview fits your scenario."
          ),
          tags$div(
            class = "random-name-input",
            tags$span(class = "material-symbols-outlined input-icon", "edit"),
            textInput(
              "random_title",
              NULL,
              placeholder = "Pedigree name (optional)",
              width = "100%"
            )
          ),
          tags$div(
            class = "random-settings-card",
            tags$p(class = "random-controls-title", "Generation settings"),
            tags$p(
              class = "random-controls-copy",
              "Adjust the family size range before creating another preview."
            ),
            tags$div(
              class = "random-slider-wrapper",
              tags$label(class = "random-slider-label", "Family size"),
              tags$div(
                class = "random-slider-track",
                sliderInput(
                  "random_n_range",
                  label = NULL,
                  min = 3,
                  max = 20,
                  value = c(3, 10),
                  step = 1,
                  width = "100%"
                )
              )
            )
          )
        ),
        tags$main(
          class = "random-modal-main",
          tags$div(
            class = "random-modal-toolbar",
            tags$span(
              class = "random-modal-badge",
              tags$span(class = "material-symbols-outlined", "casino"),
              "Random structure"
            ),
            tags$button(
              id = "random_shuffle_btn",
              class = "random-shuffle-btn action-button",
              HTML('<span class="material-symbols-outlined">shuffle</span>'),
              tags$span(class = "random-shuffle-btn-label", "Shuffle preview")
            )
          ),
          tags$div(
            class = "random-preview-section",
            tags$div(
              class = "random-preview-header",
              tags$label(class = "random-preview-label", "Preview"),
              tags$span(class = "random-preview-note", "Updated after shuffle")
            ),
            tags$div(
              class = "random-canvas-area",
              plotOutput("randomPedPreview", height = "380px", width = "100%")
            )
          ),
          tags$div(
            class = "random-modal-footer",
            tags$div(
              class = "random-modal-footer__hint",
              tags$span(class = "material-symbols-outlined", "refresh"),
              tags$span("Shuffle until the preview looks right, then confirm.")
            ),
            tags$div(
              class = "random-modal-footer__actions",
              actionButton(
                "random_cancel_btn",
                "Cancel",
                class = "random-cancel-btn"
              ),
              actionButton(
                "random_confirm_btn",
                "Confirm",
                class = "random-confirm-btn"
              )
            )
          )
        )
      ),
      footer = NULL
    ))
  })
  
  output$randomPedPreview <- renderPlot(
    {
      req(preview_ped())
      tryCatch(
        {
          p <- relabel_gen(preview_ped())
          par(bg = FALSE, mar = c(1, 1, 1, 1))
          plot(p, margins = c(1, 1, 1, 1), cex = 0.9)
        },
        error = function(e) {
          plot.new()
          text(0.5, 0.5, "Error generating preview", col = "red")
        }
      )
    },
    bg = FALSE
  )
  
  observeEvent(input$random_shuffle_btn, {
    rng <- input$random_n_range
    n_min <- if (!is.null(rng)) rng[1] else 3
    n_max <- if (!is.null(rng)) rng[2] else 10
    preview_ped(randomPed(
      n = sample(n_min:n_max, 1),
      maxDirectGap = Inf,
      selfing = FALSE
    ))
  })
  
  observeEvent(input$random_confirm_btn, {
    req(preview_ped())
    p <- relabel_gen(preview_ped())
    
    ped(p)
    values$pedData <- init_pedData(p)
    sel(character(0))
    reset_markers()
    reset_text_annotations()
    phenotypes$list <- list()
    phenotypes$assign <- list()
    pheno_styles$fill <- character(0)
    pheno_styles$hatched <- character(0)
    pheno_styles$lty <- character(0)
    pheno_styles$col <- character(0)
    hist$s <- list()
    
    title_value <- input$random_title %||% ""
    if (nzchar(title_value)) {
      updateTextInput(session, "pedigree_title_structure", value = title_value)
    }
    
    removeModal()
    shinyjs::runjs("showPage('pedigree');")
    showNotification("Random pedigree loaded", type = "message", duration = 3)
  })
  
  observeEvent(input$random_cancel_btn, {
    removeModal()
  })
  
  observeEvent(input$loadBtn, {
    showModal(modalDialog(
      title = NULL,
      size = "l",
      easyClose = TRUE,
      tags$div(
        class = "load-modal-container",
        tags$aside(
          class = "load-modal-aside",
          tags$p(class = "load-modal-kicker", "Saved Project"),
          tags$h2(class = "load-modal-title", "Load Pedigree"),
          tags$p(
            class = "load-modal-subtitle",
            "Import a saved pedigree project from your computer and continue editing it in the workspace."
          ),
          tags$div(
            class = "load-requirements-card",
            tags$p(class = "load-requirements-title", "File requirements"),
            tags$p(class = "load-requirements-copy", "Use a saved project exported from this app."),
            tags$div(
              class = "load-requirement",
              tags$span(class = "material-symbols-outlined", "check_circle"),
              tags$span(HTML("Accepted format: <strong>.rds</strong>"))
            ),
            tags$div(
              class = "load-requirement",
              tags$span(class = "material-symbols-outlined", "lock"),
              tags$span("The file stays local until you choose it.")
            )
          )
        ),
        tags$main(
          class = "load-modal-main",
          tags$div(
            class = "load-modal-toolbar",
            tags$span(
              class = "load-modal-badge",
              tags$span(class = "material-symbols-outlined", "folder_open"),
              "RDS import"
            )
          ),
          tags$div(
            class = "load-drop-panel",
            tags$div(
              class = "load-drop-zone",
              tags$div(
                tags$span(class = "material-symbols-outlined", "upload_file"),
                tags$p(class = "load-drop-title", "Choose a saved RDS file"),
                tags$p(
                  class = "load-drop-copy",
                  "The file should be a pedigree project previously exported from this application."
                ),
                tags$span(
                  class = "load-file-chip",
                  tags$span(class = "material-symbols-outlined", "description", style = "font-size:16px;"),
                  ".rds file"
                )
              )
            )
          ),
          tags$div(
            class = "load-modal-footer",
            tags$div(
              class = "load-modal-footer__hint",
              tags$span(class = "material-symbols-outlined", "touch_app"),
              tags$span("Select a local file to load your saved pedigree.")
            ),
            tags$div(
              class = "load-modal-footer__actions",
              actionButton(
                "load_cancel_btn",
                "Cancel",
                class = "load-cancel-btn"
              ),
              tags$button(
                id = "load_choose_file_btn",
                class = "load-file-btn action-button",
                onclick = "document.getElementById('loadRdsRaw').click();",
                "Choose file"
              )
            )
          )
        )
      ),
      footer = NULL
    ))
  })
  
  observeEvent(input$load_cancel_btn, {
    removeModal()
  })
  
  observeEvent(input$loadRdsData, {
    req(input$loadRdsData)
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    
    writeBin(base64enc::base64decode(input$loadRdsData$data), tmp)
    snap <- tryCatch(readRDS(tmp), error = function(e) NULL)
    if (is.null(snap) || is.null(snap$ped)) {
      showNotification("Invalid file", type = "error")
      return()
    }
    
    ped(snap$ped)
    sel(snap$sel %||% character(0))
    phenotypes$list <- snap$pheno_list %||% list()
    phenotypes$assign <- snap$pheno_assign %||% list()
    deceased_ids(snap$deceased_ids %||% character(0))
    carrier_ids(snap$carrier_ids %||% character(0))
    starred_ids(snap$starred_ids %||% character(0))
    proband_id(snap$proband_id %||% character(0))
    adopted_ids(snap$adopted_ids %||% character(0))
    miscarriage(snap$miscarriage %||% character(0))
    infertility_ids(snap$infertility_ids %||% character(0))
    afab_ids(snap$afab_ids %||% character(0))
    amab_ids(snap$amab_ids %||% character(0))
    umab_ids(snap$umab_ids %||% character(0))
    twins_df(snap$twins_df %||% data.frame(
      id1 = character(),
      id2 = character(),
      code = integer(),
      stringsAsFactors = FALSE
    ))
    values$pedData <- snap$pedData %||% init_pedData(snap$ped)
    
    reset_text_annotations()
    if (!is.null(snap$text_annotations)) {
      for (pos in names(snap$text_annotations)) {
        text_annotations[[pos]] <- snap$text_annotations[[pos]]
      }
    }
    if (!is.null(snap$annot_style)) {
      annot_style$cex <- snap$annot_style$cex %||% annot_style$cex
      annot_style$font <- snap$annot_style$font %||% annot_style$font
      annot_style$col <- snap$annot_style$col %||% annot_style$col
    }
    if (!is.null(snap$plot_settings)) {
      plot_settings$cex <- snap$plot_settings$cex %||% plot_settings$cex
      plot_settings$symbolsize <- snap$plot_settings$symbolsize %||% plot_settings$symbolsize
      plot_settings$mar <- snap$plot_settings$mar %||% plot_settings$mar
      updateNumericInput(session, "plot_text_size", value = plot_settings$cex)
      updateNumericInput(session, "plot_symbol_size", value = plot_settings$symbolsize)
      updateNumericInput(session, "plot_margins", value = plot_settings$mar)
    }
    if (!is.null(snap$title)) {
      updateTextInput(session, "pedigree_title_structure", value = snap$title)
    }
    
    rebuildPhenoStyles()
    removeModal()
    shinyjs::runjs("showPage('pedigree');")
    showNotification("Pedigree loaded", type = "message", duration = 3)
  })
  
  output$saveFile <- downloadHandler(
    filename = function() {
      title <- input$pedigree_title_structure
      if (is.null(title) || !nzchar(trimws(title))) {
        title <- "pedigree"
      }
      paste0(gsub("[^a-zA-Z0-9_-]", "_", title), ".rds")
    },
    content = function(file) {
      req(ped())
      annotation_positions <- c(
        "top",
        "bottom",
        "left",
        "right",
        "topleft",
        "topright",
        "bottomleft",
        "bottomright",
        "inside"
      )
      state <- list(
        ped = ped(),
        sel = sel(),
        pheno_list = phenotypes$list,
        pheno_assign = phenotypes$assign,
        deceased_ids = deceased_ids(),
        carrier_ids = carrier_ids(),
        starred_ids = starred_ids(),
        proband_id = proband_id(),
        adopted_ids = adopted_ids(),
        miscarriage = miscarriage(),
        infertility_ids = infertility_ids(),
        afab_ids = afab_ids(),
        amab_ids = amab_ids(),
        umab_ids = umab_ids(),
        twins_df = twins_df(),
        pedData = values$pedData,
        text_annotations = setNames(
          lapply(annotation_positions, function(pos) text_annotations[[pos]]),
          annotation_positions
        ),
        annot_style = list(
          cex = annot_style$cex,
          font = annot_style$font,
          col = annot_style$col
        ),
        plot_settings = list(
          cex = plot_settings$cex,
          symbolsize = plot_settings$symbolsize,
          mar = plot_settings$mar
        ),
        title = input$pedigree_title_structure %||% ""
      )
      saveRDS(state, file)
    }
  )
  
  # ── New phenotype button ──
  observeEvent(input$newPheno, {
    pheno_editing(NULL)
    openPhenoModal()
  })
  
  # ── Cancel modal ──
  observeEvent(input$cancelPheno, {
    removeModal()
    pheno_editing(NULL)
  })
  
  output$phenoMotifPicker <- renderUI({
    req(input$pheno_type == "motif")
    position <- input$pheno_motif_pos %||% "center"
    current_symbol <- input$pheno_motif_symbol
    motif_picker_ui(current_symbol, position)
  })
  
  # ── Preview in modal ──
  output$previewPheno <- renderUI({
    pheno_t <- input$pheno_type %||% "fill"
    
    if (pheno_t == "fill") {
      fill_color <- input$pheno_fill %||% "#EF5350"
      border_color <- "#475569"
      border_style <- "solid"
      is_hatched <- isTRUE(input$pheno_hatched)
      createPreviewShape(
        fill_color,
        border_color,
        border_style,
        is_hatched
      )
    } else if (pheno_t == "border") {
      border_color <- input$pheno_col %||% "#000000"
      border_style <- input$pheno_lty %||% "solid"
      createPreviewShape(
        "#ffffff",
        border_color,
        border_style,
        FALSE
      )
    } else {
      position <- input$pheno_motif_pos %||% "center"
      current_choices <- motif_choices_for_position(position)
      sym <- input$pheno_motif_symbol %||% unname(current_choices)[1]
      if (!sym %in% unname(current_choices)) {
        sym <- unname(current_choices)[1]
      }
      col <- input$pheno_motif_color %||% "#D32F2F"
      createPreviewMotif(sym, col)
    }
  })
  
  # ── Save phenotype (create or edit) ──
  observeEvent(input$savePheno, {
    req(input$pheno_name, nzchar(input$pheno_name))
    
    nm <- trimws(input$pheno_name)
    pheno_t <- input$pheno_type %||% "fill"
    
    if (pheno_t == "fill") {
      spec <- list(
        type = "fill",
        fill = input$pheno_fill %||% "#EF5350",
        col = NULL,
        lty = "solid",
        hatched = isTRUE(input$pheno_hatched),
        mode = if (isTRUE(input$pheno_hatched)) "hatched" else "fill",
        name = nm
      )
    } else if (pheno_t == "border") {
      spec <- list(
        type = "border",
        fill = NULL,
        col = input$pheno_col,
        lty = input$pheno_lty,
        hatched = FALSE,
        mode = "border",
        name = nm
      )
    } else {
      position <- input$pheno_motif_pos %||% "center"
      current_choices <- motif_choices_for_position(position)
      symbol <- input$pheno_motif_symbol %||% unname(current_choices)[1]
      if (!symbol %in% unname(current_choices)) {
        symbol <- unname(current_choices)[1]
      }
      spec <- list(
        type = "motif",
        symbol = symbol,
        motif_color = input$pheno_motif_color %||% "#D32F2F",
        position = position,
        mode = "motif",
        name = nm
      )
    }
    
    editing <- pheno_editing()
    
    if (is.null(editing)) {
      # Creation
      if (nm %in% names(phenotypes$list)) {
        showNotification(
          "This name already exists.",
          type = "error",
          duration = 3
        )
        return()
      }
      phenotypes$list[[nm]] <- spec
      phenotypes$assign[[nm]] <- character(0)
      showNotification(
        paste("Phenotype created:", nm),
        type = "message",
        duration = 2
      )
    } else {
      # Edit mode
      if (nm != editing && nm %in% names(phenotypes$list)) {
        showNotification(
          "This name already exists.",
          type = "error",
          duration = 3
        )
        return()
      }
      
      phenotypes$list[[nm]] <- spec
      phenotypes$assign[[nm]] <- phenotypes$assign[[editing]] %||%
        character(0)
      
      if (nm != editing) {
        phenotypes$list[[editing]] <- NULL
        phenotypes$assign[[editing]] <- NULL
      }
      
      showNotification(
        paste("Phenotype saved:", nm),
        type = "message",
        duration = 2
      )
      pheno_editing(NULL)
    }
    
    removeModal()
    rebuildPhenoStyles()
  })
  
  # ── Phenotype buttons list ──
  output$phenoButtonsUI <- renderUI({
    if (length(phenotypes$list) == 0) {
      return(div(
        style = "padding: 16px; text-align: center; color: #95a5a6; font-style: italic; font-size: 13px;",
        icon("info-circle"),
        " No phenotypes created yet"
      ))
    }
    
    lapply(names(phenotypes$list), function(nm) {
      ph <- phenotypes$list[[nm]]
      assigned_ids <- phenotypes$assign[[nm]] %||% character(0)
      count <- length(assigned_ids)
      safe_id <- gsub("[^a-zA-Z0-9]", "_", nm)
      
      if (identical(ph$type, "motif")) {
        # ── Motif mini preview ──
        mini_preview <- tags$div(
          class = "pheno-mini-preview",
          style = sprintf(
            "width:36px; height:36px; border:1px solid #ccc; border-radius:6px;
                         display:flex; align-items:center; justify-content:center;
                         font-size:20px; color:%s; background:#fafafa;",
            ph$motif_color
          ),
          ph$symbol
        )
      } else {
        mode <- ph$mode %||% ph$type %||% "fill"
        # ── Fill mini preview ──
        css_border <- switch(ph$lty %||% "solid",
                             "dashed" = "dashed",
                             "dotted" = "dotted",
                             "dotdash" = "dashed",
                             "solid"
        )
        
        bg_style <- if (identical(mode, "hatched")) {
          sprintf(
            "background-color:#ffffff;
               background-image: repeating-linear-gradient(
                 45deg, transparent, transparent 5px,
                 %s 5px, %s 7px);",
            ph$fill %||% "#475569",
            ph$fill %||% "#475569"
          )
        } else if (isTRUE(ph$hatched)) {
          sprintf(
            "background-color: %s;
               background-image: repeating-linear-gradient(
                 45deg, transparent, transparent 5px,
                 rgba(0,0,0,0.3) 5px, rgba(0,0,0,0.3) 7px);",
            ph$fill %||% "#ffffff"
          )
        } else {
          sprintf("background-color: %s;", ph$fill %||% "#ffffff")
        }
        
        mini_preview <- tags$div(
          class = "pheno-mini-preview",
          style = sprintf(
            "width: 36px; height: 36px; border: 2px %s %s; border-radius: 6px; %s",
            css_border,
            ph$col %||% "#cbd5e1",
            bg_style
          )
        )
      }
      
      div(
        class = "pheno-list-item",
        mini_preview,
        div(
          class = "pheno-info",
          tags$a(
            class = "pheno-name",
            nm,
            href = "#",
            title = "Click to apply/remove",
            onclick = sprintf(
              'Shiny.setInputValue("pheno_apply_action", {name: "%s", nonce: Math.random()}, {priority:"event"}); return false;',
              gsub('"', '\\\\"', nm)
            )
          ),
          div(
            class = "pheno-count",
            sprintf(
              "%d individual%s",
              count,
              if (count != 1) "s" else ""
            )
          )
        ),
        div(
          class = "pheno-actions",
          tags$button(
            type = "button",
            class = "btn btn-sm btn-default",
            title = "Edit",
            onclick = sprintf(
              'Shiny.setInputValue("pheno_edit_action", {name: "%s", nonce: Math.random()}, {priority:"event"});',
              gsub('"', '\\\\"', nm)
            ),
            icon("edit")
          ),
          tags$button(
            type = "button",
            class = "btn btn-sm btn-danger",
            title = "Delete",
            onclick = sprintf(
              'Shiny.setInputValue("pheno_delete_action", {name: "%s", nonce: Math.random()}, {priority:"event"});',
              gsub('"', '\\\\"', nm)
            ),
            icon("trash")
          )
        )
      )
    })
  })
  
  output$motifConfigLibraryUI <- renderUI({
    req(isTRUE(motif_config_mode()))
    choices <- motif_choice_labels()
    configs <- motif_symbol_config()
    
    tagList(
      div(
        class = "motif-library",
        p(
          class = "motif-library__hint",
          "Configure each motif independently. Changes are reflected live on the pedigree for every phenotype using that symbol."
        ),
        lapply(seq_along(choices), function(i) {
          label <- names(choices)[i]
          symbol <- unname(choices)[i]
          cfg <- normalize_motif_config(configs[[symbol]])
          
          div(
            class = "motif-library__item",
            div(class = "motif-library__preview", symbol),
            div(
              class = "motif-library__meta",
              div(class = "motif-library__name", label),
              div(
                class = "motif-library__code",
                sprintf(
                  "%s • size %.1f • x %.2f • y %.2f",
                  motif_unicode_code(symbol),
                  cfg$cex,
                  cfg$dx,
                  cfg$dy
                )
              )
            ),
            tags$button(
              type = "button",
              class = "btn btn-sm btn-default",
              onclick = sprintf(
                'Shiny.setInputValue("motif_config_action", {symbol: "%s", label: "%s", nonce: Math.random()}, {priority:"event"});',
                symbol,
                gsub('"', "\\\\\"", label, fixed = TRUE)
              ),
              icon("sliders"),
              " Configure"
            )
          )
        })
      )
    )
  })
  
  observeEvent(input$toggle_motif_config_mode, {
    motif_config_mode(!motif_config_mode())
  })
  
  observe({
    if (motif_config_mode()) {
      shinyjs::addClass("motif_config_toggle_wrapper", "active")
      shinyjs::runjs("$('#motif_config_status').text('ON');")
    } else {
      shinyjs::removeClass("motif_config_toggle_wrapper", "active")
      shinyjs::runjs("$('#motif_config_status').text('OFF');")
    }
  })
  
  observeEvent(input$motif_config_action, {
    req(input$motif_config_action$symbol)
    symbol <- input$motif_config_action$symbol
    label <- input$motif_config_action$label %||% "Motif"
    cfg <- normalize_motif_config(motif_symbol_config()[[symbol]])
    
    motif_config_symbol(symbol)
    motif_config_preview(list(
      symbol = symbol,
      cex = cfg$cex,
      dx = cfg$dx,
      dy = cfg$dy
    ))
    
    showModal(modalDialog(
      title = tags$div(
        style = "display:flex; align-items:center; gap:10px;",
        icon("sliders"),
        paste("Configure:", label)
      ),
      div(
        class = "preview-container",
        div(class = "preview-label", "Live Preview"),
        uiOutput("motifConfigPreview")
      ),
      sliderInput(
        "motif_cfg_size",
        "Size",
        min = 1.0,
        max = 3.2,
        value = cfg$cex,
        step = 0.1
      ),
      sliderInput(
        "motif_cfg_dx",
        "Horizontal Offset",
        min = -0.30,
        max = 0.30,
        value = cfg$dx,
        step = 0.01
      ),
      sliderInput(
        "motif_cfg_dy",
        "Vertical Offset",
        min = -0.30,
        max = 0.30,
        value = cfg$dy,
        step = 0.01
      ),
      footer = tagList(
        actionButton(
          "cancelMotifConfig",
          "Cancel",
          class = "btn btn-default"
        ),
        actionButton(
          "saveMotifConfig",
          HTML('<i class="fa fa-save"></i> Save'),
          class = "btn btn-primary"
        )
      ),
      size = "s",
      easyClose = FALSE
    ))
  })
  
  observe({
    req(motif_config_symbol())
    req(input$motif_cfg_size, input$motif_cfg_dx, input$motif_cfg_dy)
    motif_config_preview(list(
      symbol = motif_config_symbol(),
      cex = input$motif_cfg_size,
      dx = input$motif_cfg_dx,
      dy = input$motif_cfg_dy
    ))
  })
  
  output$motifConfigPreview <- renderUI({
    preview <- motif_config_preview()
    req(preview$symbol)
    create_motif_config_preview(
      symbol = preview$symbol,
      color = "#D32F2F",
      config = preview
    )
  })
  
  observeEvent(input$cancelMotifConfig, {
    motif_config_preview(NULL)
    motif_config_symbol(NULL)
    removeModal()
  })
  
  observeEvent(input$saveMotifConfig, {
    req(motif_config_symbol())
    preview <- motif_config_preview()
    cfgs <- motif_symbol_config()
    cfgs[[motif_config_symbol()]] <- list(
      cex = preview$cex,
      dx = preview$dx,
      dy = preview$dy
    )
    motif_symbol_config(cfgs)
    motif_config_preview(NULL)
    motif_config_symbol(NULL)
    removeModal()
    showNotification(
      "Motif configuration saved.",
      type = "message",
      duration = 2
    )
  })
  
  # ── Apply phenotype (top-level observer) ──
  observeEvent(
    input$pheno_apply_action,
    {
      nm <- input$pheno_apply_action$name
      req(ped())
      if (!(length(sel()) == 1 && nzchar(sel()[1]))) {
        showNotification(
          "Select one individual before applying a phenotype.",
          type = "warning",
          duration = 3
        )
        return()
      }
      req(nm %in% names(phenotypes$list))
      id <- sel()[1]
      cur <- phenotypes$assign[[nm]] %||% character(0)
      
      if (id %in% cur) {
        phenotypes$assign[[nm]] <- setdiff(cur, id)
        showNotification(
          sprintf("Removed '%s' from %s", nm, id),
          type = "message",
          duration = 2
        )
      } else {
        phenotypes$assign[[nm]] <- union(cur, id)
        showNotification(
          sprintf("Applied '%s' to %s", nm, id),
          type = "message",
          duration = 2
        )
      }
      rebuildPhenoStyles()
    },
    ignoreInit = TRUE
  )
  
  # ── Edit phenotype (top-level observer) ──
  observeEvent(
    input$pheno_edit_action,
    {
      nm <- input$pheno_edit_action$name
      req(nm %in% names(phenotypes$list))
      pheno_editing(nm)
      openPhenoModal(phenotypes$list[[nm]])
    },
    ignoreInit = TRUE
  )
  
  # ── Delete phenotype (top-level observer) ──
  pheno_to_delete <- reactiveVal(NULL)
  
  observeEvent(
    input$pheno_delete_action,
    {
      nm <- input$pheno_delete_action$name
      req(nm %in% names(phenotypes$list))
      pheno_to_delete(nm)
      showModal(modalDialog(
        title = tags$div(
          style = "color: #e74c3c; display: flex; align-items: center; gap: 10px;",
          icon("exclamation-triangle"),
          "Confirm Deletion"
        ),
        tags$p(sprintf("Delete phenotype '%s'?", nm)),
        tags$p(
          class = "text-muted",
          style = "font-size: 13px;",
          "All assignments will be removed. This cannot be undone."
        ),
        footer = tagList(
          modalButton("Cancel"),
          tags$button(
            type = "button",
            class = "btn btn-danger",
            onclick = 'Shiny.setInputValue("pheno_confirm_delete", Math.random(), {priority:"event"});',
            icon("trash"),
            " Delete"
          )
        ),
        size = "s",
        easyClose = TRUE
      ))
    },
    ignoreInit = TRUE
  )
  
  # ── Confirm delete (top-level observer) ──
  observeEvent(
    input$pheno_confirm_delete,
    {
      nm <- pheno_to_delete()
      req(nm, nm %in% names(phenotypes$list))
      phenotypes$list[[nm]] <- NULL
      phenotypes$assign[[nm]] <- NULL
      rebuildPhenoStyles()
      removeModal()
      pheno_to_delete(NULL)
      showNotification(
        sprintf("Deleted: %s", nm),
        type = "warning",
        duration = 3
      )
    },
    ignoreInit = TRUE
  )
  
  # ══════════════════════════════════════════════════════
  # GENDER BUTTONS
  # ══════════════════════════════════════════════════════
  
  observeEvent(input$btn_set_male, {
    req(ped(), length(sel()) == 1)
    id <- sel()[1]
    if (!(id %in% labels(ped()))) {
      showNotification(
        "Individual not found in pedigree.",
        type = "error"
      )
      return()
    }
    children_count <- length(pedtools::children(ped(), id))
    if (children_count > 0) {
      showNotification(
        "Cannot change sex: individual has children",
        type = "error",
        duration = 5
      )
      return()
    }
    current_sex <- pedtools::getSex(ped(), id)
    if (current_sex == 1) {
      showNotification(
        sprintf("%s is already male", id),
        type = "message"
      )
      return()
    }
    save_hist()
    tryCatch(
      {
        new_ped <- pedtools::setSex(ped(), ids = id, sex = 1)
        ped(new_ped)
        if (!is.null(values$pedData)) {
          i <- which(values$pedData$id == id)
          if (length(i) == 1) values$pedData$sex[i] <- 1
        }
        showNotification(
          sprintf("%s set to male", id),
          type = "message"
        )
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })
  
  observeEvent(input$btn_set_female, {
    req(ped(), length(sel()) == 1)
    id <- sel()[1]
    if (!(id %in% labels(ped()))) {
      showNotification(
        "Individual not found in pedigree.",
        type = "error"
      )
      return()
    }
    children_count <- length(pedtools::children(ped(), id))
    if (children_count > 0) {
      showNotification(
        "Cannot change sex: individual has children",
        type = "error",
        duration = 5
      )
      return()
    }
    current_sex <- pedtools::getSex(ped(), id)
    if (current_sex == 2) {
      showNotification(
        sprintf("%s is already female", id),
        type = "message"
      )
      return()
    }
    save_hist()
    tryCatch(
      {
        new_ped <- pedtools::setSex(ped(), ids = id, sex = 2)
        ped(new_ped)
        if (!is.null(values$pedData)) {
          i <- which(values$pedData$id == id)
          if (length(i) == 1) values$pedData$sex[i] <- 2
        }
        showNotification(
          sprintf("%s set to female", id),
          type = "message"
        )
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })
  
  observeEvent(input$btn_set_unknown, {
    req(ped(), length(sel()) == 1)
    id <- sel()[1]
    if (!(id %in% labels(ped()))) {
      showNotification(
        "Individual not found in pedigree.",
        type = "error"
      )
      return()
    }
    children_count <- length(pedtools::children(ped(), id))
    if (children_count > 0) {
      showNotification(
        "Cannot change sex: individual has children",
        type = "error",
        duration = 5
      )
      return()
    }
    current_sex <- pedtools::getSex(ped(), id)
    if (current_sex == 0) {
      showNotification(
        sprintf("%s sex is already unknown", id),
        type = "message"
      )
      return()
    }
    save_hist()
    tryCatch(
      {
        new_ped <- pedtools::setSex(ped(), ids = id, sex = 0)
        ped(new_ped)
        if (!is.null(values$pedData)) {
          i <- which(values$pedData$id == id)
          if (length(i) == 1) values$pedData$sex[i] <- 0
        }
        showNotification(
          sprintf("%s set to unknown sex", id),
          type = "message"
        )
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })
  
  # ══════════════════════════════════════════════════════
  # ASSIGNED AT BIRTH
  # ══════════════════════════════════════════════════════
  
  observeEvent(input$AFAB, {
    req(ped(), length(sel()) > 0)
    save_hist()
    id <- sel()[1]
    if (id %in% afab_ids()) {
      afab_ids(setdiff(afab_ids(), id))
      if (!is.null(values$pedData)) {
        i <- which(values$pedData$id == id)
        if (length(i) == 1) values$pedData$assigned_at_birth[i] <- ""
      }
    } else {
      amab_ids(setdiff(amab_ids(), id))
      umab_ids(setdiff(umab_ids(), id))
      afab_ids(c(afab_ids(), id))
      if (!is.null(values$pedData)) {
        i <- which(values$pedData$id == id)
        if (length(i) == 1) {
          values$pedData$assigned_at_birth[i] <- "AFAB"
        }
      }
    }
  })
  
  observeEvent(input$AMAB, {
    req(ped(), length(sel()) > 0)
    save_hist()
    id <- sel()[1]
    if (id %in% amab_ids()) {
      amab_ids(setdiff(amab_ids(), id))
      if (!is.null(values$pedData)) {
        i <- which(values$pedData$id == id)
        if (length(i) == 1) values$pedData$assigned_at_birth[i] <- ""
      }
    } else {
      afab_ids(setdiff(afab_ids(), id))
      umab_ids(setdiff(umab_ids(), id))
      amab_ids(c(amab_ids(), id))
      if (!is.null(values$pedData)) {
        i <- which(values$pedData$id == id)
        if (length(i) == 1) {
          values$pedData$assigned_at_birth[i] <- "AMAB"
        }
      }
    }
  })
  
  observeEvent(input$UMAB, {
    req(ped(), length(sel()) > 0)
    save_hist()
    id <- sel()[1]
    if (id %in% umab_ids()) {
      umab_ids(setdiff(umab_ids(), id))
      if (!is.null(values$pedData)) {
        i <- which(values$pedData$id == id)
        if (length(i) == 1) values$pedData$assigned_at_birth[i] <- ""
      }
    } else {
      afab_ids(setdiff(afab_ids(), id))
      amab_ids(setdiff(amab_ids(), id))
      umab_ids(c(umab_ids(), id))
      if (!is.null(values$pedData)) {
        i <- which(values$pedData$id == id)
        if (length(i) == 1) {
          values$pedData$assigned_at_birth[i] <- "UMAB"
        }
      }
    }
  })
  
  # ══════════════════════════════════════════════════════
  # STATUS MARKERS
  # ══════════════════════════════════════════════════════
  
  # ── Deceased ──
  observeEvent(input$btn_toggle_deceased, {
    req(ped(), length(sel()) == 1)
    save_hist()
    id <- sel()[1]
    if (!(id %in% labels(ped()))) {
      showNotification(
        "Individual not found in pedigree.",
        type = "error"
      )
      return()
    }
    if (id %in% deceased_ids()) {
      deceased_ids(setdiff(deceased_ids(), id))
      if (!is.null(values$pedData)) {
        i <- which(values$pedData$id == id)
        if (length(i) == 1) values$pedData$deceased[i] <- FALSE
      }
      showNotification(
        sprintf("%s unmarked as deceased", id),
        type = "message"
      )
    } else {
      deceased_ids(c(deceased_ids(), id))
      if (!is.null(values$pedData)) {
        i <- which(values$pedData$id == id)
        if (length(i) == 1) values$pedData$deceased[i] <- TRUE
      }
      showNotification(
        sprintf("%s marked as deceased", id),
        type = "message"
      )
    }
  })
  
  # ── Miscarriage ──
  observeEvent(input$set_miscarriage, {
    req(ped(), length(sel()) == 1)
    save_hist()
    id <- sel()[1]
    if (!(id %in% labels(ped()))) {
      showNotification(
        "Individual not found in pedigree.",
        type = "error"
      )
      return()
    }
    if (id %in% miscarriage()) {
      miscarriage(setdiff(miscarriage(), id))
      showNotification(
        sprintf("Miscarriage removed from %s", id),
        type = "message"
      )
    } else {
      fa <- tryCatch(pedtools::father(ped(), id), error = function(e) NA)
      mo <- tryCatch(pedtools::mother(ped(), id), error = function(e) NA)
      is_founder <- is.na(fa) || is.na(mo) || fa == "0" || mo == "0"
      if (is_founder) {
        showNotification(
          "Cannot mark a founder as miscarriage. A miscarriage must have parents.",
          type = "error",
          duration = 6
        )
        return()
      }
      if (length(pedtools::children(ped(), id)) > 0) {
        showNotification(
          "Cannot mark a parent as miscarriage",
          type = "error"
        )
        return()
      }
      miscarriage(c(miscarriage(), id))
      # Clear personal data for miscarriage individuals
      if (!is.null(values$pedData)) {
        i <- which(values$pedData$id == id)
        if (length(i) == 1) {
          values$pedData$last_name[i] <- ""
          values$pedData$first_name[i] <- ""
          values$pedData$date_of_birth[i] <- ""
          values$pedData$date_of_death[i] <- ""
          values$pedData$age[i] <- ""
          if ("age_unit" %in% names(values$pedData)) {
            values$pedData$age_unit[i] <- "years"
          }
        }
      }
      deceased_ids(setdiff(deceased_ids(), id))
      showNotification(
        sprintf("%s marked as miscarriage", id),
        type = "message"
      )
    }
  })
  
  # ── Adopted ──
  observeEvent(input$set_adopted, {
    req(ped(), length(sel()) == 1)
    save_hist()
    id <- sel()[1]
    if (!(id %in% labels(ped()))) {
      showNotification(
        "Individual not found in pedigree.",
        type = "error"
      )
      return()
    }
    if (id %in% adopted_ids()) {
      adopted_ids(setdiff(adopted_ids(), id))
      showNotification(
        sprintf("Adopted removed from %s", id),
        type = "message"
      )
    } else {
      adopted_ids(c(adopted_ids(), id))
      showNotification(
        sprintf("%s marked as adopted", id),
        type = "message"
      )
    }
  })
  
  # ── Proband ──
  observeEvent(input$set_proband, {
    req(ped(), length(sel()) == 1)
    save_hist()
    id <- sel()[1]
    if (!(id %in% labels(ped()))) {
      showNotification(
        "Individual not found in pedigree.",
        type = "error"
      )
      return()
    }
    current_proband <- proband_id()
    if (length(current_proband) > 0 && id == current_proband[1]) {
      proband_id(character(0))
      showNotification(
        sprintf("Proband removed from %s", id),
        type = "message"
      )
    } else {
      if (length(current_proband) > 0) {
        showNotification(
          sprintf(
            "Proband moved from %s to %s",
            current_proband[1],
            id
          ),
          type = "message",
          duration = 4
        )
      } else {
        showNotification(
          sprintf("%s marked as proband", id),
          type = "message"
        )
      }
      proband_id(id)
    }
  })
  
  # ── Starred ──
  observeEvent(input$starred, {
    req(ped(), length(sel()) == 1)
    save_hist()
    id <- sel()[1]
    if (!(id %in% labels(ped()))) {
      showNotification(
        "Individual not found in pedigree.",
        type = "error"
      )
      return()
    }
    if (id %in% starred_ids()) {
      starred_ids(setdiff(starred_ids(), id))
      showNotification(
        sprintf("Star removed from %s", id),
        type = "message"
      )
    } else {
      starred_ids(c(starred_ids(), id))
      showNotification(
        sprintf("%s marked as starred", id),
        type = "message"
      )
    }
  })
  
  # ── Infertility ──
  observeEvent(input$fertility, {
    req(ped(), length(sel()) == 1)
    save_hist()
    id <- sel()[1]
    if (!(id %in% labels(ped()))) {
      showNotification(
        "Individual not found in pedigree.",
        type = "error"
      )
      return()
    }
    if (id %in% infertility_ids()) {
      infertility_ids(setdiff(infertility_ids(), id))
      showNotification(
        sprintf("Infertility removed from %s", id),
        type = "message"
      )
    } else {
      infertility_ids(c(infertility_ids(), id))
      showNotification(
        sprintf("%s marked as infertile", id),
        type = "message"
      )
    }
  })
  
  # ── Info bar ──
  infoBar_text <- reactive({
    parts <- character(0)
    if (length(sel()) > 0) {
      id <- sel()[1]
      txt <- paste("Selected:", id)
      # Status markers
      markers <- character(0)
      if (id %in% deceased_ids()) {
        markers <- c(markers, "Deceased")
      }
      if (id %in% adopted_ids()) {
        markers <- c(markers, "Adopted")
      }
      if (id %in% miscarriage()) {
        markers <- c(markers, "Miscarriage")
      }
      if (id %in% starred_ids()) {
        markers <- c(markers, "Starred")
      }
      if (id %in% proband_id()) {
        markers <- c(markers, "Proband")
      }
      if (id %in% infertility_ids()) {
        markers <- c(markers, "Infertile")
      }
      if (id %in% afab_ids()) {
        markers <- c(markers, "AFAB")
      }
      if (id %in% amab_ids()) {
        markers <- c(markers, "AMAB")
      }
      if (id %in% umab_ids()) {
        markers <- c(markers, "UMAB")
      }
      # Find phenotypes assigned to this individual
      ph_names <- character(0)
      for (nm in names(phenotypes$assign)) {
        if (id %in% (phenotypes$assign[[nm]] %||% character(0))) {
          ph_names <- c(ph_names, nm)
        }
      }
      all_tags <- c(markers, ph_names)
      if (length(all_tags) > 0) {
        txt <- paste0(txt, " \u2014 ", paste(all_tags, collapse = ", "))
      }
      parts <- c(parts, txt)
    } else {
      parts <- c(parts, "Click on an individual to select")
    }
    n_ph <- length(phenotypes$list)
    if (n_ph > 0) {
      parts <- c(parts, paste0(n_ph, " phenotype(s)"))
    }
    paste(parts, collapse = "   |   ")
  })
  
  output$infoBar <- renderText({
    infoBar_text()
  })
  
  # ── Legend ──
  output$legendUI <- renderUI({
    ph <- phenotypes$list
    if (length(ph) == 0) {
      return(div(
        class = "box",
        tags$p(tags$em("Create phenotypes to get started."))
      ))
    }
    
    items <- lapply(names(ph), function(nm) {
      p <- ph[[nm]]
      
      if (identical(p$type, "motif")) {
        swatch <- tags$span(
          class = "swatch",
          style = sprintf(
            "color:%s; background:transparent; border:1px solid #ccc;
                         text-align:center; line-height:18px; font-size:13px;",
            p$motif_color
          ),
          p$symbol
        )
      } else {
        mode <- p$mode %||% p$type %||% "fill"
        css_border <- switch(p$lty %||% "solid",
                             "dashed" = "dashed",
                             "dotted" = "dotted",
                             "dotdash" = "dashed",
                             "solid"
        )
        
        bg_style <- if (identical(mode, "hatched")) {
          sprintf(
            "background:repeating-linear-gradient(45deg,transparent,transparent 3px,%s 3px,%s 6px); background-color:white;",
            p$fill %||% "#475569",
            p$fill %||% "#475569"
          )
        } else if (isTRUE(p$hatched)) {
          sprintf(
            "background:repeating-linear-gradient(45deg,%s,%s 3px,white 3px,white 6px);",
            p$fill %||% "#ffffff",
            p$fill %||% "#ffffff"
          )
        } else {
          sprintf("background:%s;", p$fill %||% "#ffffff")
        }
        
        swatch <- tags$span(
          class = "swatch",
          style = sprintf(
            "border: 2px %s %s; %s",
            css_border,
            p$col %||% "#cbd5e1",
            bg_style
          )
        )
      }
      
      tags$div(class = "legend-item", swatch, tags$span(nm))
    })
    
    div(class = "box", tags$h5("Legend"), tagList(items))
  })
  
  # ── Individuals table (DT) ──
  output$tableDT <- DT::renderDT(
    {
      req(ped())
      
      p_obj <- ped()
      ids <- setdiff(labels(p_obj), miscarriage())
      pd <- values$pedData
      ph <- phenotypes$list
      dead <- deceased_ids()
      
      # Compute inbreeding coefficients
      f_vals <- tryCatch(
        ribd::inbreeding(p_obj, ids = labels(p_obj), Xchrom = FALSE),
        error = function(e) NULL
      )
      if (!is.null(f_vals)) {
        names(f_vals) <- labels(p_obj)
      }
      
      # Determine if relationship columns should be shown
      stats_on <- isTRUE(display_stats_mode())
      selected <- sel()
      show_rel <- stats_on && length(selected) > 0 && nzchar(selected[1])
      
      # Pre-compute degree and kinship for relationship columns
      deg_df <- NULL
      phi_mat <- NULL
      if (show_rel) {
        all_ids <- labels(p_obj)
        deg_df <- tryCatch(
          ribd::coeffTable(
            x = p_obj,
            ids = all_ids,
            coeff = c("f", "phi", "deg"),
            self = FALSE,
            Xchrom = FALSE
          ),
          error = function(e) NULL
        )
        phi_mat <- tryCatch(
          ribd::kinship(p_obj, ids = all_ids, Xchrom = FALSE),
          error = function(e) NULL
        )
      }
      
      # Build table row by row
      tbl <- do.call(
        rbind,
        lapply(ids, function(id) {
          sex_icon <- if (id %in% males(p_obj)) "\u2642" else "\u2640"
          
          clean_rel_ids <- function(x) {
            x <- as.character(x %||% character(0))
            x <- x[!is.na(x) & nzchar(x) & x != "0"]
            intersect(x, labels(p_obj))
          }
          
          family_chips_html <- function(member_ids, inline = FALSE) {
            member_ids <- clean_rel_ids(member_ids)
            if (!length(member_ids)) {
              return('<span class="table-family-empty">—</span>')
            }
            
            chips <- vapply(member_ids, function(mid) {
              sex_val <- pedtools::getSex(p_obj, mid)
              icon <- switch(as.character(sex_val),
                             "1" = "\u2642",
                             "2" = "\u2640",
                             "?"
              )
              cls <- switch(as.character(sex_val),
                            "1" = "male",
                            "2" = "female",
                            "unknown"
              )
              ids_json <- gsub(
                "\"",
                "&quot;",
                as.character(jsonlite::toJSON(mid, auto_unbox = TRUE)),
                fixed = TRUE
              )
              highlight_json <- gsub(
                "\"",
                "&quot;",
                as.character(jsonlite::toJSON(list(mid), auto_unbox = TRUE)),
                fixed = TRUE
              )
              sprintf(
                paste0(
                  '<button type="button" class="ind-panel__family-chip %s" ',
                  'onclick="Shiny.setInputValue(&quot;family_chip_click&quot;, %s, {priority:&quot;event&quot;}); window.flashPedigreeHighlight(%s, this);" ',
                  'onmousedown="window.flashPedigreeHighlight(%s, this, 900);">',
                  '<span class="ind-panel__family-chip-icon">%s</span><span>%s</span></button>'
                ),
                cls,
                ids_json,
                highlight_json,
                highlight_json,
                htmltools::htmlEscape(icon),
                htmltools::htmlEscape(mid)
              )
            }, character(1))
            
            chip_class <- if (isTRUE(inline)) {
              "table-family-chips table-family-chips--inline"
            } else {
              "table-family-chips"
            }
            paste0('<div class="', chip_class, '">', paste(chips, collapse = ""), '</div>')
          }
          
          phenotypes_html <- function(individual_id) {
            if (!length(phenotypes$list) || !length(phenotypes$assign)) {
              return('<span class="table-family-empty">—</span>')
            }
            
            assigned_names <- names(phenotypes$assign)[vapply(
              names(phenotypes$assign),
              function(nm) individual_id %in% (phenotypes$assign[[nm]] %||% character(0)),
              logical(1)
            )]
            assigned_names <- assigned_names[assigned_names %in% names(phenotypes$list)]
            
            if (!length(assigned_names)) {
              return('<span class="table-family-empty">—</span>')
            }
            
            chips <- vapply(assigned_names, function(nm) {
              ph <- phenotypes$list[[nm]]
              ph_type <- ph$type %||% "fill"
              safe_name <- htmltools::htmlEscape(nm)
              
              if (identical(ph_type, "motif")) {
                swatch_style <- sprintf(
                  "border:1px solid #dbe3ee; color:%s; background:#fff;",
                  ph$motif_color %||% "#D32F2F"
                )
                swatch <- sprintf(
                  '<span class="table-pheno-swatch" style="%s">%s</span>',
                  swatch_style,
                  htmltools::htmlEscape(ph$symbol %||% "•")
                )
              } else {
                css_border <- switch(ph$lty %||% "solid",
                                     "dashed" = "dashed",
                                     "dotted" = "dotted",
                                     "dotdash" = "dashed",
                                     "solid"
                )
                mode <- ph$mode %||% ph_type
                bg_style <- if (identical(mode, "hatched") || isTRUE(ph$hatched)) {
                  sprintf(
                    "background-color:%s; background-image:repeating-linear-gradient(45deg, transparent, transparent 4px, rgba(15,23,42,0.34) 4px, rgba(15,23,42,0.34) 6px);",
                    ph$fill %||% "#ffffff"
                  )
                } else {
                  sprintf("background-color:%s;", ph$fill %||% "#ffffff")
                }
                swatch_style <- sprintf(
                  "border:2px %s %s; %s",
                  css_border,
                  ph$col %||% "#cbd5e1",
                  bg_style
                )
                swatch <- sprintf(
                  '<span class="table-pheno-swatch" style="%s"></span>',
                  swatch_style
                )
              }
              
              sprintf(
                '<span class="table-pheno-chip" title="%s">%s<span class="table-pheno-name">%s</span></span>',
                safe_name,
                swatch,
                safe_name
              )
            }, character(1))
            
            paste0('<div class="table-pheno-list">', paste(chips, collapse = ""), '</div>')
          }
          
          r <- if (!is.null(pd)) {
            pd[pd$id == id, , drop = FALSE]
          } else {
            data.frame()
          }
          ln <- if (nrow(r) == 1) (r$last_name[1] %||% "") else ""
          fn <- if (nrow(r) == 1) (r$first_name[1] %||% "") else ""
          dob <- if (nrow(r) == 1) {
            (r$date_of_birth[1] %||% "")
          } else {
            ""
          }
          dod <- if (nrow(r) == 1) {
            (r$date_of_death[1] %||% "")
          } else {
            ""
          }
          age_val <- if (nrow(r) == 1) (r$age[1] %||% "") else ""
          age_u <- if (nrow(r) == 1 && "age_unit" %in% names(r)) {
            (r$age_unit[1] %||% "years")
          } else {
            "years"
          }
          is_dead <- id %in% dead
          
          # Individual cell: ID, sex marker and optional name.
          ln_up <- if (nzchar(ln)) toupper(ln) else ""
          display_name <- paste(c(ln_up, fn)[nzchar(c(ln_up, fn))], collapse = " ")
          sex_class <- if (id %in% males(p_obj)) {
            "male"
          } else if (id %in% females(p_obj)) {
            "female"
          } else {
            "unknown"
          }
          individual_label <- sprintf(
            paste0(
              '<span class="pedigree-individual-cell">',
              '<span class="pedigree-individual-cell__sex %s">%s</span>',
              '<span class="pedigree-individual-cell__main">',
              '<span class="pedigree-individual-cell__id">%s</span>',
              '%s',
              '</span></span>'
            ),
            sex_class,
            htmltools::htmlEscape(sex_icon),
            htmltools::htmlEscape(id),
            if (nzchar(display_name)) {
              sprintf(
                '<span class="pedigree-individual-cell__name">%s</span>',
                htmltools::htmlEscape(display_name)
              )
            } else {
              ""
            }
          )
          
          # Inbreeding coefficient
          fv <- if (!is.null(f_vals)) f_vals[as.character(id)] else NA
          f_txt <- if (is.na(fv)) {
            "\u2014"
          } else if (fv == 0) {
            "0"
          } else {
            round3(fv)
          }
          
          # Status badge HTML
          status_html <- if (is_dead) {
            '<span class="badge-status badge-deceased">Deceased</span>'
          } else {
            '<span class="badge-status badge-alive">Alive</span>'
          }
          
          father_txt <- tryCatch(
            pedtools::father(p_obj, id, internal = FALSE),
            error = function(e) NA_character_
          )
          mother_txt <- tryCatch(
            pedtools::mother(p_obj, id, internal = FALSE),
            error = function(e) NA_character_
          )
          father_txt <- if (
            length(father_txt) == 0 ||
            is.na(father_txt) ||
            father_txt == "0"
          ) {
            "\u2014"
          } else {
            as.character(father_txt)
          }
          mother_txt <- if (
            length(mother_txt) == 0 ||
            is.na(mother_txt) ||
            mother_txt == "0"
          ) {
            "\u2014"
          } else {
            as.character(mother_txt)
          }
          
          sibling_ids <- clean_rel_ids(tryCatch(
            pedtools::siblings(p_obj, id, internal = FALSE),
            error = function(e) character(0)
          ))
          spouse_ids <- clean_rel_ids(tryCatch(
            pedtools::spouses(p_obj, id, internal = FALSE),
            error = function(e) character(0)
          ))
          child_ids <- clean_rel_ids(tryCatch(
            pedtools::children(p_obj, id, internal = FALSE),
            error = function(e) character(0)
          ))
          
          # Relationship columns (conditional on stats mode + selection)
          if (show_rel) {
            base <- selected[1]
            if (as.character(id) == base) {
              deg_txt <- "\u2014"
              r_txt <- "\u2014"
              rel_txt <- "Self"
            } else {
              # Degree
              deg_val <- NA
              if (!is.null(deg_df)) {
                pair <- deg_df[
                  (deg_df$id1 == base & deg_df$id2 == id) |
                    (deg_df$id1 == id & deg_df$id2 == base), ,
                  drop = FALSE
                ]
                if (nrow(pair) > 0 && "deg" %in% names(pair)) {
                  deg_val <- pair$deg[1]
                }
              }
              deg_txt <- if (is.na(deg_val)) {
                "\u2014"
              } else if (is.infinite(deg_val)) {
                "\u221E"
              } else {
                as.character(deg_val)
              }
              # %R from kinship
              r_pct <- NA
              if (!is.null(phi_mat)) {
                bid <- as.character(base)
                tid <- as.character(id)
                if (
                  bid %in%
                  rownames(phi_mat) &&
                  tid %in% colnames(phi_mat)
                ) {
                  phi_val <- phi_mat[bid, tid]
                  if (!is.na(phi_val) && phi_val > 0) {
                    r_pct <- min(max(2 * phi_val, 0), 1) *
                      100
                  }
                }
              }
              r_txt <- if (is.na(r_pct)) {
                "\u2014"
              } else {
                paste0(round(r_pct, 1), "%")
              }
              # Textual relationship description
              rel_txt <- tryCatch(
                describe_relationship(
                  p_obj,
                  base,
                  as.character(id)
                ),
                error = function(e) "\u2014"
              )
            }
          }
          
          if (show_rel) {
            data.frame(
              Individual = individual_label,
              Mother = family_chips_html(mother_txt),
              Father = family_chips_html(father_txt),
              f = f_txt,
              Status = status_html,
              Phenotypes = phenotypes_html(id),
              Siblings = family_chips_html(sibling_ids, inline = TRUE),
              Spouses = family_chips_html(spouse_ids, inline = TRUE),
              Children = family_chips_html(child_ids, inline = TRUE),
              deg = deg_txt,
              R = r_txt,
              Relation = rel_txt,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          } else {
            data.frame(
              Individual = individual_label,
              Mother = family_chips_html(mother_txt),
              Father = family_chips_html(father_txt),
              f = f_txt,
              Status = status_html,
              Phenotypes = phenotypes_html(id),
              Siblings = family_chips_html(sibling_ids, inline = TRUE),
              Spouses = family_chips_html(spouse_ids, inline = TRUE),
              Children = family_chips_html(child_ids, inline = TRUE),
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          }
        })
      )
      
      # Column definitions depend on whether relationship columns are present
      non_orderable <- which(names(tbl) %in% c(
        "Mother",
        "Father",
        "Status",
        "Phenotypes",
        "Siblings",
        "Spouses",
        "Children",
        "R",
        "Relation"
      )) - 1
      
      DT::datatable(
        tbl,
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        options = list(
          pageLength = 6,
          lengthChange = FALSE,
          searching = TRUE,
          ordering = TRUE,
          scrollX = FALSE,
          autoWidth = FALSE,
          info = TRUE,
          language = list(
            search = "Search",
            info = "Showing _START_ to _END_ of _TOTAL_ individuals",
            paginate = list(
              `previous` = "\u276E",
              `next` = "\u276F"
            )
          ),
          dom = "ftrip",
          columnDefs = list(
            list(orderable = FALSE, targets = non_orderable)
          )
        ),
        class = "pedigree-table-dt compact hover stripe row-border"
      )
    },
    server = FALSE
  )
  
  output$pedigreeTableViewControls <- renderUI({
    current <- pedigree_table_view()
    div(
      class = "pedigree-view-toolbar",
      tags$button(
        type = "button",
        class = paste("pedigree-view-btn", if (identical(current, "table")) "is-active" else ""),
        onclick = "Shiny.setInputValue('pedigree_table_view_select', 'table', {priority:'event'});",
        "Table"
      ),
      tags$button(
        type = "button",
        class = paste("pedigree-view-btn", if (identical(current, "cards")) "is-active" else ""),
        onclick = "Shiny.setInputValue('pedigree_table_view_select', 'cards', {priority:'event'});",
        "Cards"
      )
    )
  })
  
  output$pedigreeTableContent <- renderUI({
    if (identical(pedigree_table_view(), "cards")) {
      uiOutput("pedigreeCardView")
    } else {
      DT::DTOutput("tableDT")
    }
  })
  
  observeEvent(input$pedigree_table_view_select, {
    view <- input$pedigree_table_view_select
    if (view %in% c("table", "cards")) {
      pedigree_table_view(view)
    }
  }, ignoreInit = TRUE)
  
  output$pedigreeCardView <- renderUI({
    req(ped())
    p_obj <- ped()
    ids <- setdiff(labels(p_obj), miscarriage())
    pd <- values$pedData
    
    clean_rel_ids <- function(x) {
      x <- as.character(x %||% character(0))
      x <- x[!is.na(x) & nzchar(x) & x != "0"]
      intersect(x, labels(p_obj))
    }
    
    family_chips_ui <- function(member_ids) {
      member_ids <- clean_rel_ids(member_ids)
      if (!length(member_ids)) {
        return(tags$span(class = "pedigree-person-card__empty", "\u2014"))
      }
      div(
        class = "table-family-chips",
        lapply(member_ids, function(mid) {
          sex_val <- pedtools::getSex(p_obj, mid)
          icon <- switch(as.character(sex_val),
                         "1" = "\u2642",
                         "2" = "\u2640",
                         "?"
          )
          cls <- switch(as.character(sex_val),
                        "1" = "male",
                        "2" = "female",
                        "unknown"
          )
          ids_json <- as.character(jsonlite::toJSON(mid, auto_unbox = TRUE))
          highlight_json <- as.character(jsonlite::toJSON(list(mid), auto_unbox = TRUE))
          tags$button(
            type = "button",
            class = paste("ind-panel__family-chip", cls),
            onclick = sprintf(
              "Shiny.setInputValue('family_chip_click', %s, {priority:'event'}); window.flashPedigreeHighlight(%s, this);",
              ids_json,
              highlight_json
            ),
            onmousedown = sprintf(
              "window.flashPedigreeHighlight(%s, this, 900);",
              highlight_json
            ),
            tags$span(class = "ind-panel__family-chip-icon", icon),
            tags$span(mid)
          )
        })
      )
    }
    
    person_card <- function(id) {
      r <- if (!is.null(pd)) pd[pd$id == id, , drop = FALSE] else data.frame()
      ln <- if (nrow(r) == 1) (r$last_name[1] %||% "") else ""
      fn <- if (nrow(r) == 1) (r$first_name[1] %||% "") else ""
      ln_up <- if (nzchar(ln)) toupper(ln) else ""
      display_name <- paste(c(ln_up, fn)[nzchar(c(ln_up, fn))], collapse = " ")
      sex_val <- pedtools::getSex(p_obj, id)
      sex_icon <- switch(as.character(sex_val),
                         "1" = "\u2642",
                         "2" = "\u2640",
                         "?"
      )
      sex_class <- switch(as.character(sex_val),
                          "1" = "male",
                          "2" = "female",
                          "unknown"
      )
      father_id <- clean_rel_ids(tryCatch(pedtools::father(p_obj, id, internal = FALSE), error = function(e) character(0)))
      mother_id <- clean_rel_ids(tryCatch(pedtools::mother(p_obj, id, internal = FALSE), error = function(e) character(0)))
      sibling_ids <- clean_rel_ids(tryCatch(pedtools::siblings(p_obj, id, internal = FALSE), error = function(e) character(0)))
      spouse_ids <- clean_rel_ids(tryCatch(pedtools::spouses(p_obj, id, internal = FALSE), error = function(e) character(0)))
      child_ids <- clean_rel_ids(tryCatch(pedtools::children(p_obj, id, internal = FALSE), error = function(e) character(0)))
      
      div(
        class = "pedigree-person-card",
        div(class = "pedigree-person-card__kicker", "Individual"),
        div(
          class = paste("pedigree-person-card__title", sex_class),
          paste(c(id, sex_icon, display_name)[nzchar(c(id, sex_icon, display_name))], collapse = " ")
        ),
        div(
          class = "pedigree-person-card__section",
          div(class = "pedigree-person-card__label", "Parents"),
          family_chips_ui(c(father_id, mother_id))
        ),
        div(
          class = "pedigree-person-card__section",
          div(class = "pedigree-person-card__label", "Siblings"),
          family_chips_ui(sibling_ids)
        ),
        div(
          class = "pedigree-person-card__section",
          div(class = "pedigree-person-card__label", "Spouses"),
          family_chips_ui(spouse_ids)
        ),
        div(
          class = "pedigree-person-card__section",
          div(class = "pedigree-person-card__label", "Children"),
          family_chips_ui(child_ids)
        )
      )
    }
    
    div(
      class = "pedigree-card-carousel",
      tags$button(
        type = "button",
        class = "pedigree-card-nav",
        onclick = "window.pedigreeCardsMove && window.pedigreeCardsMove(-1);",
        HTML("&#8249;")
      ),
      div(
        id = "pedigreeCardTrack",
        class = "pedigree-card-track",
        lapply(ids, person_card)
      ),
      tags$button(
        type = "button",
        class = "pedigree-card-nav",
        onclick = "window.pedigreeCardsMove && window.pedigreeCardsMove(1);",
        HTML("&#8250;")
      )
    )
  })
  
  # ════════════════════════════════════════════════════════
  # SYSTEM 1: Quick Add - Server handlers
  # ════════════════════════════════════════════════════════
  
  observeEvent(input$relative_mode, {
    relative_mode(input$relative_mode)
  })
  observeEvent(input$sibling_type, {
    sibling_type(input$sibling_type)
    sibling_number(1)
  })
  observeEvent(input$sibling_sex, {
    sibling_sex(input$sibling_sex)
  })
  observeEvent(input$sibling_number_action, {
    current <- sibling_number()
    if (input$sibling_number_action == "increase" && current < 10) {
      sibling_number(current + 1)
    } else if (input$sibling_number_action == "decrease" && current > 1) {
      sibling_number(current - 1)
    }
  })
  observeEvent(input$shared_parent, {
    shared_parent_val(input$shared_parent)
  })
  observeEvent(input$twin_mode, {
    twin_mode(input$twin_mode)
  })
  observeEvent(input$twin_type, {
    twin_type_val(input$twin_type)
  })
  observeEvent(input$twin_sex, {
    twin_sex_val(input$twin_sex)
  })
  observeEvent(input$triplet_sex2, {
    triplet_sex2(as.integer(input$triplet_sex2))
  })
  observeEvent(input$triplet_sex3, {
    triplet_sex3(as.integer(input$triplet_sex3))
  })
  observeEvent(input$child_sex, {
    child_sex(input$child_sex)
    child_is_miscarriage(FALSE)
  })
  observeEvent(input$child_is_miscarriage, {
    child_is_miscarriage(as.logical(input$child_is_miscarriage))
  })
  observeEvent(input$child_number_action, {
    current <- child_number()
    if (input$child_number_action == "increase" && current < 10) {
      child_number(current + 1)
    } else if (input$child_number_action == "decrease" && current > 1) {
      child_number(current - 1)
    }
  })
  
  # ── Config panel renderUI ──
  output$relative_config_panel <- renderUI({
    mode <- relative_mode()
    if (is.null(mode)) {
      return(div(
        class = "config-empty",
        tags$i(class = "fa fa-arrow-up"),
        p("Select a relative type above")
      ))
    }
    
    if (mode == "parents") {
      div(
        class = "config-panel-content",
        div(
          class = "config-description",
          tags$i(class = "fa fa-user-plus config-desc-icon"),
          span(
            "Add both parents (mother and father) to the selected individual"
          )
        ),
        actionButton(
          "addParents",
          tagList(tags$i(class = "fa fa-plus"), "Add Parents"),
          class = "primary-action-btn"
        )
      )
    } else if (mode == "siblings") {
      div(
        class = "config-panel-content",
        div(
          class = "config-field",
          div(class = "config-field-label", "Type"),
          div(
            class = "segment-control",
            tags$button(
              id = "tab_full",
              class = paste(
                "segment-btn",
                if (sibling_type() == "full") "active" else ""
              ),
              "Full",
              onclick = "$(this).addClass('active').siblings().removeClass('active'); Shiny.setInputValue('sibling_type', 'full', {priority: 'event'});"
            ),
            tags$button(
              id = "tab_half",
              class = paste(
                "segment-btn",
                if (sibling_type() == "half") "active" else ""
              ),
              "Half",
              onclick = "$(this).addClass('active').siblings().removeClass('active'); Shiny.setInputValue('sibling_type', 'half', {priority: 'event'});"
            ),
            tags$button(
              id = "tab_twins",
              class = paste(
                "segment-btn",
                if (sibling_type() == "twins") "active" else ""
              ),
              "Twin/Triplet",
              onclick = "$(this).addClass('active').siblings().removeClass('active'); Shiny.setInputValue('sibling_type', 'twins', {priority: 'event'});"
            )
          )
        ),
        uiOutput("sibling_ui_content")
      )
    } else if (mode == "children") {
      n <- child_number()
      is_misc <- child_is_miscarriage()
      div(
        class = "config-panel-content",
        uiOutput("partner_selection_ui"),
        div(
          class = "config-field",
          div(class = "config-field-label", "Sex"),
          div(
            class = "segment-control",
            tags$button(
              id = "child_sex_male",
              class = paste(
                "segment-btn",
                if (!is_misc && child_sex() == 1) {
                  "active"
                } else {
                  ""
                }
              ),
              style = if (is_misc) {
                "opacity: 0.4; pointer-events: none;"
              } else {
                ""
              },
              tagList(tags$i(class = "fa fa-mars"), "Male"),
              onclick = "Shiny.setInputValue('child_sex', 1, {priority: 'event'}); Shiny.setInputValue('child_is_miscarriage', false, {priority: 'event'});"
            ),
            tags$button(
              id = "child_sex_female",
              class = paste(
                "segment-btn",
                if (!is_misc && child_sex() == 2) {
                  "active"
                } else {
                  ""
                }
              ),
              style = if (is_misc) {
                "opacity: 0.4; pointer-events: none;"
              } else {
                ""
              },
              tagList(tags$i(class = "fa fa-venus"), "Female"),
              onclick = "Shiny.setInputValue('child_sex', 2, {priority: 'event'}); Shiny.setInputValue('child_is_miscarriage', false, {priority: 'event'});"
            ),
            tags$button(
              id = "child_sex_unknown",
              class = paste(
                "segment-btn",
                if (!is_misc && child_sex() == 0) {
                  "active"
                } else {
                  ""
                }
              ),
              style = if (is_misc) {
                "opacity: 0.4; pointer-events: none;"
              } else {
                ""
              },
              "Other",
              onclick = "Shiny.setInputValue('child_sex', 0, {priority: 'event'}); Shiny.setInputValue('child_is_miscarriage', false, {priority: 'event'});"
            )
          )
        ),
        div(
          style = "margin-top: 6px;",
          tags$button(
            id = "child_misc_btn",
            class = if (is_misc) {
              "pill-toggle is-active"
            } else {
              "pill-toggle"
            },
            style = paste(
              "width: 100%;",
              if (is_misc) {
                "background-color: #64748b; color: white; border-color: #64748b;"
              } else {
                ""
              }
            ),
            tagList(
              tags$i(class = "fa fa-triangle-exclamation"),
              " Miscarriage"
            ),
            onclick = "Shiny.setInputValue('child_is_miscarriage', true, {priority: 'event'});"
          )
        ),
        div(
          class = "config-field",
          div(class = "config-field-label", "Count"),
          div(
            class = "stepper-control",
            tags$button(
              class = "stepper-btn",
              tags$i(class = "fa fa-minus"),
              onclick = "Shiny.setInputValue('child_number_action', 'decrease', {priority: 'event'});"
            ),
            span(class = "stepper-value", n),
            tags$button(
              class = "stepper-btn",
              tags$i(class = "fa fa-plus"),
              onclick = "Shiny.setInputValue('child_number_action', 'increase', {priority: 'event'});"
            )
          )
        ),
        actionButton(
          "btn_add_children",
          tagList(tags$i(class = "fa fa-plus"), "Add"),
          class = "primary-action-btn"
        )
      )
    }
  })
  
  # ── Sibling sub-panel ──
  output$sibling_ui_content <- renderUI({
    type <- sibling_type()
    
    if (type == "full") {
      tagList(
        div(
          class = "config-info",
          span(class = "config-dot"),
          span(
            class = "config-info-text",
            "Full siblings \u2014 Same mother and father"
          )
        ),
        div(class = "config-label", "SEX"),
        div(
          class = "sex-selector",
          tags$button(
            id = "sibling_sex_male",
            class = "sex-btn active",
            HTML(
              '<i class="fa fa-mars"></i><span class="sex-label">Male</span>'
            ),
            onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('sibling_sex', 1, {priority: 'event'});"
          ),
          tags$button(
            id = "sibling_sex_female",
            class = "sex-btn",
            HTML(
              '<i class="fa fa-venus"></i><span class="sex-label">Female</span>'
            ),
            onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('sibling_sex', 2, {priority: 'event'});"
          ),
          tags$button(
            id = "sibling_sex_unknown",
            class = "sex-btn",
            HTML(
              '<i class="fa fa-question"></i><span class="sex-label">Other</span>'
            ),
            onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('sibling_sex', 0, {priority: 'event'});"
          )
        ),
        div(class = "config-label", "NUMBER"),
        div(
          class = "number-control",
          div(
            class = "number-stepper",
            tags$button(
              class = "stepper-btn",
              "\u2212",
              onclick = "Shiny.setInputValue('sibling_number_action', 'decrease', {priority: 'event'});"
            ),
            div(
              class = "stepper-value",
              as.character(sibling_number())
            ),
            tags$button(
              class = "stepper-btn",
              "+",
              onclick = "Shiny.setInputValue('sibling_number_action', 'increase', {priority: 'event'});"
            )
          )
        ),
        actionButton(
          "btn_add_sibling",
          HTML(
            '<span class="primary-wide-plus">+</span> Add Sibling'
          ),
          class = "primary-wide-btn"
        )
      )
    } else if (type == "half") {
      tagList(
        div(
          class = "config-info",
          span(class = "config-dot"),
          span(
            class = "config-info-text",
            "Half-siblings \u2014 One shared parent"
          )
        ),
        div(class = "config-label", "SHARED PARENT"),
        div(
          class = "pill-toggle-group",
          tags$button(
            id = "shared_father",
            class = "pill-toggle is-active",
            "Father",
            onclick = "$(this).siblings().removeClass('is-active'); $(this).addClass('is-active'); Shiny.setInputValue('shared_parent', 'father', {priority: 'event'});"
          ),
          tags$button(
            id = "shared_mother",
            class = "pill-toggle",
            "Mother",
            onclick = "$(this).siblings().removeClass('is-active'); $(this).addClass('is-active'); Shiny.setInputValue('shared_parent', 'mother', {priority: 'event'});"
          )
        ),
        div(class = "config-label", "SEX"),
        div(
          class = "sex-selector",
          tags$button(
            id = "sibling_sex_male",
            class = "sex-btn active",
            HTML(
              '<i class="fa fa-mars"></i><span class="sex-label">Male</span>'
            ),
            onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('sibling_sex', 1, {priority: 'event'});"
          ),
          tags$button(
            id = "sibling_sex_female",
            class = "sex-btn",
            HTML(
              '<i class="fa fa-venus"></i><span class="sex-label">Female</span>'
            ),
            onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('sibling_sex', 2, {priority: 'event'});"
          ),
          tags$button(
            id = "sibling_sex_unknown",
            class = "sex-btn",
            HTML(
              '<i class="fa fa-question"></i><span class="sex-label">Other</span>'
            ),
            onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('sibling_sex', 0, {priority: 'event'});"
          )
        ),
        div(class = "config-label", "NUMBER"),
        div(
          class = "number-control",
          div(
            class = "number-stepper",
            tags$button(
              class = "stepper-btn",
              "\u2212",
              onclick = "Shiny.setInputValue('sibling_number_action', 'decrease', {priority: 'event'});"
            ),
            div(
              class = "stepper-value",
              as.character(sibling_number())
            ),
            tags$button(
              class = "stepper-btn",
              "+",
              onclick = "Shiny.setInputValue('sibling_number_action', 'increase', {priority: 'event'});"
            )
          )
        ),
        actionButton(
          "btn_add_sibling",
          HTML(
            '<span class="primary-wide-plus">+</span> Add Sibling'
          ),
          class = "primary-wide-btn"
        )
      )
    } else {
      # Twin / Triplet
      tm <- twin_mode()
      tagList(
        div(
          class = "pill-toggle-group",
          tags$button(
            id = "mode_twin",
            class = if (tm == "twin") {
              "pill-toggle is-active"
            } else {
              "pill-toggle"
            },
            "Twin",
            onclick = "$(this).siblings().removeClass('is-active'); $(this).addClass('is-active'); Shiny.setInputValue('twin_mode', 'twin', {priority: 'event'});"
          ),
          tags$button(
            id = "mode_triplet",
            class = if (tm == "triplet") {
              "pill-toggle is-active"
            } else {
              "pill-toggle"
            },
            "Triplet",
            onclick = "$(this).siblings().removeClass('is-active'); $(this).addClass('is-active'); Shiny.setInputValue('twin_mode', 'triplet', {priority: 'event'});"
          )
        ),
        if (tm == "twin") {
          tagList(
            div(
              class = "config-info",
              span(class = "config-dot"),
              span(
                class = "config-info-text",
                "Add one twin sibling"
              )
            ),
            div(class = "config-label", "ZYGOSITY"),
            div(
              class = "twin-type-selector",
              tags$button(
                id = "tt_mz",
                class = if (twin_type_val() == 1) {
                  "twin-type-btn active"
                } else {
                  "twin-type-btn"
                },
                "MZ",
                onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('twin_type', 1, {priority: 'event'});"
              ),
              tags$button(
                id = "tt_dz",
                class = if (twin_type_val() == 2) {
                  "twin-type-btn active"
                } else {
                  "twin-type-btn"
                },
                "DZ",
                onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('twin_type', 2, {priority: 'event'});"
              ),
              tags$button(
                id = "tt_uk",
                class = if (twin_type_val() == 3) {
                  "twin-type-btn active"
                } else {
                  "twin-type-btn"
                },
                "Unknown",
                onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('twin_type', 3, {priority: 'event'});"
              )
            ),
            if (twin_type_val() %in% c(2, 3)) {
              tagList(
                div(class = "config-label", "TWIN SEX"),
                div(
                  class = "sex-selector",
                  tags$button(
                    id = "twin_sex_male",
                    class = if (twin_sex_val() == 1) {
                      "sex-btn active"
                    } else {
                      "sex-btn"
                    },
                    HTML(
                      '<i class="fa fa-mars"></i><span class="sex-label">Male</span>'
                    ),
                    onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('twin_sex', 1, {priority: 'event'});"
                  ),
                  tags$button(
                    id = "twin_sex_female",
                    class = if (twin_sex_val() == 2) {
                      "sex-btn active"
                    } else {
                      "sex-btn"
                    },
                    HTML(
                      '<i class="fa fa-venus"></i><span class="sex-label">Female</span>'
                    ),
                    onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('twin_sex', 2, {priority: 'event'});"
                  ),
                  tags$button(
                    id = "twin_sex_unknown",
                    class = if (twin_sex_val() == 0) {
                      "sex-btn active"
                    } else {
                      "sex-btn"
                    },
                    HTML(
                      '<i class="fa fa-question"></i><span class="sex-label">Other</span>'
                    ),
                    onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('twin_sex', 0, {priority: 'event'});"
                  )
                )
              )
            } else {
              div(
                class = "config-info",
                span(class = "config-dot"),
                span(
                  class = "config-info-text",
                  "Monozygotic twins must have the same sex"
                )
              )
            },
            actionButton(
              "btn_add_twin",
              HTML(
                '<span class="primary-wide-plus">+</span> Add Twin'
              ),
              class = "primary-wide-btn"
            )
          )
        } else {
          tagList(
            div(
              class = "config-info",
              span(class = "config-dot"),
              span(
                class = "config-info-text",
                "Add two siblings to form a triplet group"
              )
            ),
            div(class = "config-label", "SIBLING 2 SEX"),
            div(
              class = "sex-selector",
              tags$button(
                id = "trip_sex2_male",
                class = if (triplet_sex2() == 1) {
                  "sex-btn active"
                } else {
                  "sex-btn"
                },
                HTML(
                  '<i class="fa fa-mars"></i><span class="sex-label">Male</span>'
                ),
                onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('triplet_sex2', 1, {priority: 'event'});"
              ),
              tags$button(
                id = "trip_sex2_female",
                class = if (triplet_sex2() == 2) {
                  "sex-btn active"
                } else {
                  "sex-btn"
                },
                HTML(
                  '<i class="fa fa-venus"></i><span class="sex-label">Female</span>'
                ),
                onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('triplet_sex2', 2, {priority: 'event'});"
              ),
              tags$button(
                id = "trip_sex2_unknown",
                class = if (triplet_sex2() == 0) {
                  "sex-btn active"
                } else {
                  "sex-btn"
                },
                HTML(
                  '<i class="fa fa-question"></i><span class="sex-label">Other</span>'
                ),
                onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('triplet_sex2', 0, {priority: 'event'});"
              )
            ),
            div(class = "config-label", "SIBLING 3 SEX"),
            div(
              class = "sex-selector",
              tags$button(
                id = "trip_sex3_male",
                class = if (triplet_sex3() == 1) {
                  "sex-btn active"
                } else {
                  "sex-btn"
                },
                HTML(
                  '<i class="fa fa-mars"></i><span class="sex-label">Male</span>'
                ),
                onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('triplet_sex3', 1, {priority: 'event'});"
              ),
              tags$button(
                id = "trip_sex3_female",
                class = if (triplet_sex3() == 2) {
                  "sex-btn active"
                } else {
                  "sex-btn"
                },
                HTML(
                  '<i class="fa fa-venus"></i><span class="sex-label">Female</span>'
                ),
                onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('triplet_sex3', 2, {priority: 'event'});"
              ),
              tags$button(
                id = "trip_sex3_unknown",
                class = if (triplet_sex3() == 0) {
                  "sex-btn active"
                } else {
                  "sex-btn"
                },
                HTML(
                  '<i class="fa fa-question"></i><span class="sex-label">Other</span>'
                ),
                onclick = "$(this).siblings().removeClass('active'); $(this).addClass('active'); Shiny.setInputValue('triplet_sex3', 0, {priority: 'event'});"
              )
            ),
            actionButton(
              "btn_add_triplet_quick",
              HTML(
                '<span class="primary-wide-plus">+</span> Add Triplets'
              ),
              class = "primary-wide-btn"
            )
          )
        }
      )
    }
  })
  
  # ── Partner selection UI ──
  output$partner_selection_ui <- renderUI({
    req(ped(), length(sel()) > 0)
    id <- sel()[1]
    sex_val <- pedtools::getSex(ped(), id)
    if (sex_val == 0) {
      return(NULL)
    }
    
    partners <- getPartners(ped(), id)
    if (length(partners) == 0) {
      return(div(
        class = "partner-box",
        tags$strong("No existing partner"),
        "New partner will be created"
      ))
    }
    if (length(partners) == 1) {
      return(div(
        class = "partner-box",
        tags$strong(sprintf("Existing partner: %s", partners[1])),
        div(
          class = "partner-checkbox",
          checkboxInput(
            "use_new_partner",
            "Create new partner",
            value = FALSE
          )
        )
      ))
    }
    div(
      class = "partner-box",
      tags$strong("Multiple partners"),
      radioButtons(
        "partner_choice",
        NULL,
        choices = c(
          setNames(partners, paste("Partner:", partners)),
          "new" = "Create new partner"
        ),
        selected = partners[1]
      )
    )
  })
  
  # ── System 1 action handlers ──
  observeEvent(input$addParents, {
    req(ped(), length(sel()) > 0)
    save_hist()
    tryCatch(
      {
        old_ped <- ped()
        new_ped <- addParentsToIndividual(old_ped, sel()[1])
        relabel_and_update(old_ped, new_ped)
        showNotification("Parents added", type = "message")
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })
  
  observeEvent(input$btn_add_sibling, {
    req(ped(), length(sel()) > 0)
    save_hist()
    tryCatch(
      {
        type <- sibling_type()
        n <- sibling_number()
        sex <- sibling_sex()
        old_ped <- ped()
        if (type == "full") {
          new_ped <- addFullSiblings(
            old_ped,
            sel()[1],
            n = n,
            sex = sex
          )
        } else {
          shared <- shared_parent_val()
          new_ped <- addHalfSiblings(
            old_ped,
            sel()[1],
            n = n,
            sex = sex,
            shared_parent = shared
          )
        }
        relabel_and_update(old_ped, new_ped)
        showNotification(
          sprintf("%d %s sibling(s) added", n, type),
          type = "message"
        )
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })
  
  observeEvent(input$btn_add_twin, {
    req(ped(), length(sel()) > 0)
    save_hist()
    tryCatch(
      {
        id <- sel()[1]
        zygosity <- twin_type_val()
        old_ped <- ped()
        twin_sex_param <- if (zygosity == 1) NULL else twin_sex_val()
        result_twin <- addTwin(
          old_ped,
          id,
          zygosity = zygosity,
          twin_sex = twin_sex_param
        )
        # Add twins relation BEFORE relabeling
        TW <- twins_df()
        twins_df(rbind(TW, result_twin$twins_relation))
        relabel_and_update(old_ped, result_twin$ped)
        # Select new twin
        updated_twins <- twins_df()
        last_twin <- tail(updated_twins, 1)
        if (nrow(last_twin) > 0) {
          sel(last_twin$id2[1])
        }
        showNotification("Twin added", type = "message", duration = 4)
      },
      error = function(e) {
        showNotification(
          paste("Error adding twin:", e$message),
          type = "error",
          duration = 8
        )
      }
    )
  })
  
  observeEvent(input$btn_add_triplet_quick, {
    req(ped(), length(sel()) == 1)
    save_hist()
    tryCatch(
      {
        id <- sel()[1]
        old_ped <- ped()
        sex2 <- triplet_sex2()
        sex3 <- triplet_sex3()
        result <- addTriplets(old_ped, id, sex2 = sex2, sex3 = sex3)
        # Add twin relations BEFORE relabeling
        TW <- twins_df()
        twins_df(rbind(TW, result$twins_relations))
        relabel_and_update(old_ped, result$ped)
        updated_twins <- twins_df()
        last_two <- tail(updated_twins, 2)
        if (nrow(last_two) > 0) {
          sel(last_two$id2[1])
        }
        showNotification(
          "Triplets added",
          type = "message",
          duration = 4
        )
      },
      error = function(e) {
        showNotification(
          paste("Error adding triplets:", e$message),
          type = "error",
          duration = 8
        )
      }
    )
  })
  
  observeEvent(input$btn_add_children, {
    req(ped(), length(sel()) > 0)
    id <- sel()[1]
    sex_val <- pedtools::getSex(ped(), id)
    if (sex_val == 0) {
      showNotification(
        "Individual must have defined sex to add children",
        type = "error",
        duration = 5
      )
      return()
    }
    save_hist()
    tryCatch(
      {
        n <- child_number()
        old_ped <- ped()
        is_misc <- child_is_miscarriage()
        # Resolve partner
        partners <- getPartners(old_ped, id)
        partner <- NULL
        if (length(partners) == 0) {
          partner <- NULL
        } else if (length(partners) == 1) {
          if (
            !is.null(input$use_new_partner) && input$use_new_partner
          ) {
            partner <- NULL
          } else {
            partner <- partners[1]
          }
        } else {
          if (!is.null(input$partner_choice)) {
            if (input$partner_choice == "new") {
              partner <- NULL
            } else {
              partner <- input$partner_choice
            }
          } else {
            partner <- partners[1]
          }
        }
        
        if (is_misc) {
          current_ped <- old_ped
          mc_ids <- character(0)
          for (i in seq_len(n)) {
            result <- addMiscarriageChild(
              current_ped,
              id,
              partner_id = partner
            )
            current_ped <- result$ped
            mc_ids <- c(mc_ids, result$child_id)
          }
          miscarriage(union(miscarriage(), mc_ids))
          relabel_and_update(old_ped, current_ped)
          showNotification(
            sprintf("%d miscarriage(s) added", n),
            type = "message"
          )
        } else {
          sex <- child_sex()
          new_ped <- addChildrenToIndividual(
            old_ped,
            id,
            partner_id = partner,
            n = n,
            sex = sex
          )
          relabel_and_update(old_ped, new_ped)
          showNotification(
            sprintf("%d child(ren) added", n),
            type = "message"
          )
        }
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })
  
  # ════════════════════════════════════════════════════════
  # SYSTEM 2: Family Grid - Server handlers
  # ════════════════════════════════════════════════════════
  
  button_config <- list(
    btn_add_parents = list(type = "parents", label = "Parents"),
    btn_add_sister = list(type = "sister", label = "Sister"),
    btn_add_brother = list(type = "brother", label = "Brother"),
    btn_add_sibling_unknown = list(
      type = "sibling_unknown",
      label = "Unknown Sibling"
    ),
    btn_add_twins = list(type = "twins", label = "Twins"),
    btn_add_daughter = list(type = "daughter", label = "Daughter"),
    btn_add_son = list(type = "son", label = "Son"),
    btn_add_child_unknown = list(
      type = "child_unknown",
      label = "Unknown Child"
    ),
    btn_add_miscarriage = list(type = "miscarriage", label = "Miscarriage")
  )
  
  reset_grid_selection <- function() {
    for (btn_id in names(button_config)) {
      removeClass(btn_id, "selected")
    }
    state$selected_type <- NULL
    state$click_count <- 0
    state$count <- 1
    state$sib_kinship <- "full"
    state$twins_mode <- "twin"
  }
  
  handle_button_click <- function(btn_id) {
    config <- button_config[[btn_id]]
    if (
      !is.null(state$selected_type) && state$selected_type == config$type
    ) {
      # Clicking the same button again deselects
      reset_grid_selection()
    } else {
      reset_grid_selection()
      state$selected_type <- config$type
      state$click_count <- 1
      addClass(btn_id, "selected")
    }
  }
  
  # 9 button observers
  observeEvent(input$btn_add_parents, handle_button_click("btn_add_parents"))
  observeEvent(input$btn_add_sister, handle_button_click("btn_add_sister"))
  observeEvent(input$btn_add_brother, handle_button_click("btn_add_brother"))
  observeEvent(
    input$btn_add_sibling_unknown,
    handle_button_click("btn_add_sibling_unknown")
  )
  observeEvent(input$btn_add_twins, handle_button_click("btn_add_twins"))
  observeEvent(
    input$btn_add_daughter,
    handle_button_click("btn_add_daughter")
  )
  observeEvent(input$btn_add_son, handle_button_click("btn_add_son"))
  observeEvent(
    input$btn_add_child_unknown,
    handle_button_click("btn_add_child_unknown")
  )
  observeEvent(
    input$btn_add_miscarriage,
    handle_button_click("btn_add_miscarriage")
  )
  
  # Grid confirm Add button
  observeEvent(input$grid_confirm_add, {
    if (!is.null(state$selected_type)) {
      execute_add_relative(state$selected_type)
      reset_grid_selection()
    }
  })
  
  # Grid option observers
  observeEvent(input$grid_zone_close, {
    reset_grid_selection()
  })
  observeEvent(input$grid_stepper_minus, {
    if (state$count > 1) state$count <- state$count - 1
  })
  observeEvent(input$grid_stepper_plus, {
    if (state$count < 10) state$count <- state$count + 1
  })
  observeEvent(input$grid_sib_kinship_full, {
    state$sib_kinship <- "full"
  })
  observeEvent(input$grid_sib_kinship_half, {
    state$sib_kinship <- "half"
  })
  observeEvent(input$grid_half_shared_father, {
    state$half_shared_parent <- "father"
  })
  observeEvent(input$grid_half_shared_mother, {
    state$half_shared_parent <- "mother"
  })
  observeEvent(input$grid_twins_mode_twin, {
    state$twins_mode <- "twin"
  })
  observeEvent(input$grid_twins_mode_triplet, {
    state$twins_mode <- "triplet"
  })
  observeEvent(input$grid_zygosity_monozygous, {
    state$twins_zygosity <- "monozygous"
    state$twins_gender <- "other"
  })
  observeEvent(input$grid_zygosity_dizygous, {
    state$twins_zygosity <- "dizygous"
  })
  observeEvent(input$grid_zygosity_unknown, {
    state$twins_zygosity <- "unknown"
  })
  observeEvent(input$grid_gender_male, {
    state$twins_gender <- "male"
  })
  observeEvent(input$grid_gender_female, {
    state$twins_gender <- "female"
  })
  observeEvent(input$grid_gender_other, {
    state$twins_gender <- "other"
  })
  observeEvent(input$grid_trip_sex2_male, {
    state$triplet_sex2 <- "male"
  })
  observeEvent(input$grid_trip_sex2_female, {
    state$triplet_sex2 <- "female"
  })
  observeEvent(input$grid_trip_sex2_unknown, {
    state$triplet_sex2 <- "unknown"
  })
  observeEvent(input$grid_trip_sex3_male, {
    state$triplet_sex3 <- "male"
  })
  observeEvent(input$grid_trip_sex3_female, {
    state$triplet_sex3 <- "female"
  })
  observeEvent(input$grid_trip_sex3_unknown, {
    state$triplet_sex3 <- "unknown"
  })
  observeEvent(input$grid_partner_select, {
    state$selected_partner <- input$grid_partner_select
  })
  observeEvent(
    sel(),
    {
      state$selected_partner <- NULL
    },
    ignoreInit = TRUE
  )
  
  # ── execute_add_relative: dispatcher for all grid actions ──
  execute_add_relative <- function(type) {
    if (is.null(ped()) || length(sel()) == 0) {
      showNotification(
        "Please select an individual first",
        type = "warning"
      )
      return()
    }
    id <- sel()[1]
    old_ped <- ped()
    save_hist()
    
    tryCatch(
      {
        switch(type,
               "parents" = {
                 new_ped <- addParentsToIndividual(old_ped, id)
                 relabel_and_update(old_ped, new_ped)
                 showNotification("Parents added", type = "message")
               },
               "sister" = {
                 if (state$sib_kinship == "half") {
                   new_ped <- addHalfSiblings(
                     old_ped,
                     id,
                     n = state$count,
                     sex = 2L,
                     shared_parent = state$half_shared_parent
                   )
                   relabel_and_update(old_ped, new_ped)
                   showNotification(
                     sprintf("%d half-sister(s) added", state$count),
                     type = "message"
                   )
                 } else {
                   new_ped <- addFullSiblings(
                     old_ped,
                     id,
                     n = state$count,
                     sex = 2
                   )
                   relabel_and_update(old_ped, new_ped)
                   showNotification(
                     sprintf("%d sister(s) added", state$count),
                     type = "message"
                   )
                 }
               },
               "brother" = {
                 if (state$sib_kinship == "half") {
                   new_ped <- addHalfSiblings(
                     old_ped,
                     id,
                     n = state$count,
                     sex = 1L,
                     shared_parent = state$half_shared_parent
                   )
                   relabel_and_update(old_ped, new_ped)
                   showNotification(
                     sprintf(
                       "%d half-brother(s) added",
                       state$count
                     ),
                     type = "message"
                   )
                 } else {
                   new_ped <- addFullSiblings(
                     old_ped,
                     id,
                     n = state$count,
                     sex = 1
                   )
                   relabel_and_update(old_ped, new_ped)
                   showNotification(
                     sprintf("%d brother(s) added", state$count),
                     type = "message"
                   )
                 }
               },
               "sibling_unknown" = {
                 if (state$sib_kinship == "half") {
                   new_ped <- addHalfSiblings(
                     old_ped,
                     id,
                     n = state$count,
                     sex = 0L,
                     shared_parent = state$half_shared_parent
                   )
                   relabel_and_update(old_ped, new_ped)
                   showNotification(
                     sprintf(
                       "%d half-sibling(s) added",
                       state$count
                     ),
                     type = "message"
                   )
                 } else {
                   new_ped <- addFullSiblings(
                     old_ped,
                     id,
                     n = state$count,
                     sex = 0
                   )
                   relabel_and_update(old_ped, new_ped)
                   showNotification(
                     sprintf(
                       "%d unknown sibling(s) added",
                       state$count
                     ),
                     type = "message"
                   )
                 }
               },
               "twins" = {
                 if (state$twins_mode == "triplet") {
                   sex2 <- switch(state$triplet_sex2,
                                  "male" = 1L,
                                  "female" = 2L,
                                  0L
                   )
                   sex3 <- switch(state$triplet_sex3,
                                  "male" = 1L,
                                  "female" = 2L,
                                  0L
                   )
                   result <- addTriplets(
                     old_ped,
                     id,
                     sex2 = sex2,
                     sex3 = sex3
                   )
                   TW <- twins_df()
                   twins_df(rbind(TW, result$twins_relations))
                   relabel_and_update(old_ped, result$ped)
                   updated <- twins_df()
                   last_two <- tail(updated, 2)
                   if (nrow(last_two) > 0) {
                     sel(last_two$id2[1])
                   }
                   showNotification("Triplets added", type = "message")
                 } else {
                   zygosity <- switch(state$twins_zygosity,
                                      "monozygous" = 1L,
                                      "dizygous" = 2L,
                                      3L
                   )
                   twin_sex_param <- if (zygosity == 1L) {
                     NULL
                   } else {
                     switch(state$twins_gender,
                            "male" = 1L,
                            "female" = 2L,
                            NULL
                     )
                   }
                   result <- addTwin(
                     old_ped,
                     id,
                     zygosity = zygosity,
                     twin_sex = twin_sex_param
                   )
                   TW <- twins_df()
                   twins_df(rbind(TW, result$twins_relation))
                   relabel_and_update(old_ped, result$ped)
                   updated <- twins_df()
                   last_twin <- tail(updated, 1)
                   if (nrow(last_twin) > 0) {
                     sel(last_twin$id2[1])
                   }
                   showNotification("Twin added", type = "message")
                 }
               },
               "daughter" = {
                 .grid_add_children(
                   old_ped,
                   id,
                   n = state$count,
                   sex = 2,
                   is_misc = FALSE
                 )
               },
               "son" = {
                 .grid_add_children(
                   old_ped,
                   id,
                   n = state$count,
                   sex = 1,
                   is_misc = FALSE
                 )
               },
               "child_unknown" = {
                 .grid_add_children(
                   old_ped,
                   id,
                   n = state$count,
                   sex = 0,
                   is_misc = FALSE
                 )
               },
               "miscarriage" = {
                 .grid_add_children(
                   old_ped,
                   id,
                   n = state$count,
                   sex = 0,
                   is_misc = TRUE
                 )
               }
        )
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  }
  
  # Helper for grid children actions
  .grid_add_children <- function(old_ped, id, n, sex, is_misc) {
    sex_val <- pedtools::getSex(old_ped, id)
    if (sex_val == 0) {
      showNotification(
        "Individual must have defined sex to add children",
        type = "error"
      )
      return()
    }
    # Resolve partner
    partners <- getPartners(old_ped, id)
    partner <- if (
      !is.null(state$selected_partner) && state$selected_partner != "new"
    ) {
      state$selected_partner
    } else if (length(partners) == 1) {
      partners[1]
    } else {
      NULL
    }
    
    if (is_misc) {
      current_ped <- old_ped
      mc_ids <- character(0)
      for (i in seq_len(n)) {
        result <- addMiscarriageChild(
          current_ped,
          id,
          partner_id = partner
        )
        current_ped <- result$ped
        mc_ids <- c(mc_ids, result$child_id)
      }
      miscarriage(union(miscarriage(), mc_ids))
      relabel_and_update(old_ped, current_ped)
      showNotification(
        sprintf("%d miscarriage(s) added", n),
        type = "message"
      )
    } else {
      new_ped <- addChildrenToIndividual(
        old_ped,
        id,
        partner_id = partner,
        n = n,
        sex = sex
      )
      relabel_and_update(old_ped, new_ped)
      showNotification(
        sprintf("%d child(ren) added", n),
        type = "message"
      )
    }
  }
  
  # ── Helper: wrap zone content ──
  .zone_wrap <- function(label, options_content) {
    tags$div(
      class = "family-grid__center state-selected",
      tags$button(
        class = "zone-close-btn",
        HTML("&times;"),
        onclick = "Shiny.setInputValue('grid_zone_close', Date.now(), {priority:'event'});"
      ),
      tags$div(
        class = "center-content",
        tags$span(class = "center-content__type", "Adding"),
        tags$span(class = "center-content__label", label),
        tags$div(class = "center-content__options", options_content),
        tags$button(
          class = "grid-add-btn",
          tags$span(
            class = "material-symbols-outlined",
            style = "font-size:16px;",
            "add"
          ),
          "Add",
          onclick = "Shiny.setInputValue('grid_confirm_add', Date.now(), {priority:'event'});"
        )
      )
    )
  }
  
  # ── Shared UI fragments (reactive) ──
  .stepper_ui <- function() {
    div(
      class = "stepper-control",
      tags$button(
        class = "stepper-btn",
        "\u2212",
        onclick = "Shiny.setInputValue('grid_stepper_minus', Date.now(), {priority:'event'});"
      ),
      span(class = "stepper-value", state$count),
      tags$button(
        class = "stepper-btn",
        "+",
        onclick = "Shiny.setInputValue('grid_stepper_plus', Date.now(), {priority:'event'});"
      )
    )
  }
  
  # ── Zone 1: Parents ──
  output$zone_parents <- renderUI({
    sel_type <- state$selected_type
    if (is.null(sel_type) || sel_type != "parents") {
      return(NULL)
    }
    .zone_wrap(
      "Parents",
      tags$span(
        class = "center-content__hint",
        "Automatically adds a father and a mother"
      )
    )
  })
  
  # ── Zone 2: Siblings (sister, brother, unknown, twins) ──
  output$zone_siblings <- renderUI({
    sel_type <- state$selected_type
    if (
      is.null(sel_type) ||
      !(sel_type %in%
        c("sister", "brother", "sibling_unknown", "twins"))
    ) {
      return(NULL)
    }
    
    label <- button_config[[paste0("btn_add_", sel_type)]]$label %||%
      sel_type
    if (
      sel_type %in%
      c("sister", "brother", "sibling_unknown") &&
      state$sib_kinship == "half"
    ) {
      label <- paste0("Half-", label)
    }
    if (sel_type == "twins" && state$twins_mode == "triplet") {
      label <- "Triplets"
    }
    
    options_content <- if (
      sel_type %in% c("sister", "brother", "sibling_unknown")
    ) {
      tagList(
        div(
          class = "segment-control",
          tags$button(
            class = paste(
              "segment-btn",
              if (state$sib_kinship == "full") "active" else ""
            ),
            "Full",
            onclick = "Shiny.setInputValue('grid_sib_kinship_full', Date.now(), {priority:'event'});"
          ),
          tags$button(
            class = paste(
              "segment-btn",
              if (state$sib_kinship == "half") "active" else ""
            ),
            "Half",
            onclick = "Shiny.setInputValue('grid_sib_kinship_half', Date.now(), {priority:'event'});"
          )
        ),
        if (state$sib_kinship == "half") {
          div(
            class = "pill-toggle-group",
            tags$button(
              class = paste(
                "pill-toggle",
                if (state$half_shared_parent == "father") {
                  "is-active"
                } else {
                  ""
                }
              ),
              "Father",
              onclick = "Shiny.setInputValue('grid_half_shared_father', Date.now(), {priority:'event'});"
            ),
            tags$button(
              class = paste(
                "pill-toggle",
                if (state$half_shared_parent == "mother") {
                  "is-active"
                } else {
                  ""
                }
              ),
              "Mother",
              onclick = "Shiny.setInputValue('grid_half_shared_mother', Date.now(), {priority:'event'});"
            )
          )
        } else {
          NULL
        },
        .stepper_ui()
      )
    } else {
      # twins
      tagList(
        div(
          class = "segment-control",
          tags$button(
            class = paste(
              "segment-btn",
              if (state$twins_mode == "twin") "active" else ""
            ),
            "Twin",
            onclick = "Shiny.setInputValue('grid_twins_mode_twin', Date.now(), {priority:'event'});"
          ),
          tags$button(
            class = paste(
              "segment-btn",
              if (state$twins_mode == "triplet") "active" else ""
            ),
            "Triplet",
            onclick = "Shiny.setInputValue('grid_twins_mode_triplet', Date.now(), {priority:'event'});"
          )
        ),
        if (state$twins_mode == "twin") {
          tagList(
            div(
              class = "segment-control",
              tags$button(
                class = paste(
                  "segment-btn",
                  if (state$twins_zygosity == "monozygous") {
                    "active"
                  } else {
                    ""
                  }
                ),
                "MZ",
                onclick = "Shiny.setInputValue('grid_zygosity_monozygous', Date.now(), {priority:'event'});"
              ),
              tags$button(
                class = paste(
                  "segment-btn",
                  if (state$twins_zygosity == "dizygous") {
                    "active"
                  } else {
                    ""
                  }
                ),
                "DZ",
                onclick = "Shiny.setInputValue('grid_zygosity_dizygous', Date.now(), {priority:'event'});"
              ),
              tags$button(
                class = paste(
                  "segment-btn",
                  if (state$twins_zygosity == "unknown") {
                    "active"
                  } else {
                    ""
                  }
                ),
                "Unknown",
                onclick = "Shiny.setInputValue('grid_zygosity_unknown', Date.now(), {priority:'event'});"
              )
            ),
            if (state$twins_zygosity != "monozygous") {
              div(
                class = "sex-selector",
                tags$button(
                  class = paste(
                    "sex-btn",
                    if (state$twins_gender == "female") {
                      "active"
                    } else {
                      ""
                    }
                  ),
                  HTML('<i class="fa fa-venus"></i>'),
                  onclick = "Shiny.setInputValue('grid_gender_female', Date.now(), {priority:'event'});"
                ),
                tags$button(
                  class = paste(
                    "sex-btn",
                    if (state$twins_gender == "male") {
                      "active"
                    } else {
                      ""
                    }
                  ),
                  HTML('<i class="fa fa-mars"></i>'),
                  onclick = "Shiny.setInputValue('grid_gender_male', Date.now(), {priority:'event'});"
                ),
                tags$button(
                  class = paste(
                    "sex-btn",
                    if (state$twins_gender == "other") {
                      "active"
                    } else {
                      ""
                    }
                  ),
                  HTML('<i class="fa fa-question"></i>'),
                  onclick = "Shiny.setInputValue('grid_gender_other', Date.now(), {priority:'event'});"
                )
              )
            } else {
              NULL
            }
          )
        } else {
          tagList(
            div(
              style = "font-size:11px; color:#64748b;",
              "Sibling 2:"
            ),
            div(
              class = "sex-selector",
              style = "margin-bottom:4px;",
              tags$button(
                class = paste(
                  "sex-btn",
                  if (state$triplet_sex2 == "female") {
                    "active"
                  } else {
                    ""
                  }
                ),
                HTML('<i class="fa fa-venus"></i>'),
                onclick = "Shiny.setInputValue('grid_trip_sex2_female', Date.now(), {priority:'event'});"
              ),
              tags$button(
                class = paste(
                  "sex-btn",
                  if (state$triplet_sex2 == "male") {
                    "active"
                  } else {
                    ""
                  }
                ),
                HTML('<i class="fa fa-mars"></i>'),
                onclick = "Shiny.setInputValue('grid_trip_sex2_male', Date.now(), {priority:'event'});"
              ),
              tags$button(
                class = paste(
                  "sex-btn",
                  if (state$triplet_sex2 == "unknown") {
                    "active"
                  } else {
                    ""
                  }
                ),
                HTML('<i class="fa fa-question"></i>'),
                onclick = "Shiny.setInputValue('grid_trip_sex2_unknown', Date.now(), {priority:'event'});"
              )
            ),
            div(
              style = "font-size:11px; color:#64748b;",
              "Sibling 3:"
            ),
            div(
              class = "sex-selector",
              tags$button(
                class = paste(
                  "sex-btn",
                  if (state$triplet_sex3 == "female") {
                    "active"
                  } else {
                    ""
                  }
                ),
                HTML('<i class="fa fa-venus"></i>'),
                onclick = "Shiny.setInputValue('grid_trip_sex3_female', Date.now(), {priority:'event'});"
              ),
              tags$button(
                class = paste(
                  "sex-btn",
                  if (state$triplet_sex3 == "male") {
                    "active"
                  } else {
                    ""
                  }
                ),
                HTML('<i class="fa fa-mars"></i>'),
                onclick = "Shiny.setInputValue('grid_trip_sex3_male', Date.now(), {priority:'event'});"
              ),
              tags$button(
                class = paste(
                  "sex-btn",
                  if (state$triplet_sex3 == "unknown") {
                    "active"
                  } else {
                    ""
                  }
                ),
                HTML('<i class="fa fa-question"></i>'),
                onclick = "Shiny.setInputValue('grid_trip_sex3_unknown', Date.now(), {priority:'event'});"
              )
            )
          )
        }
      )
    }
    
    .zone_wrap(label, options_content)
  })
  
  # ── Zone 3: Children (daughter, son, child_unknown, miscarriage) ──
  output$zone_children <- renderUI({
    sel_type <- state$selected_type
    if (
      is.null(sel_type) ||
      !(sel_type %in%
        c("daughter", "son", "child_unknown", "miscarriage"))
    ) {
      return(NULL)
    }
    
    label <- button_config[[paste0("btn_add_", sel_type)]]$label %||%
      sel_type
    partners <- if (!is.null(ped()) && length(sel()) > 0) {
      getPartners(ped(), sel()[1])
    } else {
      character(0)
    }
    
    options_content <- tagList(
      if (length(partners) > 0) {
        div(
          style = "margin-bottom:6px;",
          lapply(partners, function(p) {
            tags$button(
              class = paste(
                "pill-toggle",
                if (identical(state$selected_partner, p)) {
                  "is-active"
                } else {
                  ""
                }
              ),
              p,
              onclick = sprintf(
                "Shiny.setInputValue('grid_partner_select', '%s', {priority:'event'});",
                p
              )
            )
          }),
          tags$button(
            class = paste(
              "pill-toggle",
              if (identical(state$selected_partner, "new")) {
                "is-active"
              } else {
                ""
              }
            ),
            "New",
            onclick = "Shiny.setInputValue('grid_partner_select', 'new', {priority:'event'});"
          )
        )
      } else {
        NULL
      },
      .stepper_ui()
    )
    
    .zone_wrap(label, options_content)
  })
  
  privacy_mode <- reactiveVal(FALSE)
  stats_mode <- reactiveVal(FALSE)
  pedigree_choices_mode <- reactiveVal(FALSE)
  pedigree_data_mode <- reactiveVal(TRUE)
  pedigree_table_mode <- reactiveVal(TRUE)
  pedigree_table_view <- reactiveVal("table")
  floating_individual_id <- reactiveVal(NULL)
  relationship_window_open <- reactiveVal(FALSE)
  
  active_tab <- reactiveVal("structure")
  legend_panel <- reactiveVal("conventions")
  personal_panel <- reactiveVal("info")
  try_pedigree_choices <- c(
    "3rd cousins + child",
    "Nuclear: 2 children (mixed)",
    "Half-sibs: paternal",
    "Ancestral: 2 gen back",
    "Ancestral: 3 gen back",
    "1st cousins",
    "1st cousins + child"
  )
  
  switch_capsule_page <- function(page_id, btn_id) {
    shinyjs::hide("page_tools")
    shinyjs::hide("page_search")
    shinyjs::hide("page_examples")
    shinyjs::hide("page_informations")
    shinyjs::show(page_id)
    shinyjs::runjs(sprintf(
      "document.querySelectorAll('.capsule-actions .cap-btn').forEach(function(el){el.classList.remove('active');el.setAttribute('aria-pressed','false');});var b=document.getElementById('%s');if(b){b.classList.add('active');b.setAttribute('aria-pressed','true');}",
      btn_id
    ))
  }
  observeEvent(input$tools,
               {
                 switch_capsule_page("page_tools", "tools")
               },
               ignoreInit = TRUE
  )
  observeEvent(input$search,
               {
                 switch_capsule_page("page_search", "search")
               },
               ignoreInit = TRUE
  )
  observeEvent(input$examples,
               {
                 switch_capsule_page("page_examples", "examples")
               },
               ignoreInit = TRUE
  )
  observeEvent(input$informations,
               {
                 switch_capsule_page("page_informations", "informations")
               },
               ignoreInit = TRUE
  )
  
  pedigree_tpl_names <- names(families)
  pedigree_tpl_idx <- reactiveVal(
    max(1, which(pedigree_tpl_names == default_family_name)[1])
  )
  
  observeEvent(input$pedigree_tpl_prev, {
    n <- length(pedigree_tpl_names)
    pedigree_tpl_idx(((pedigree_tpl_idx() - 2) %% n) + 1)
  })
  observeEvent(input$pedigree_tpl_next, {
    n <- length(pedigree_tpl_names)
    pedigree_tpl_idx((pedigree_tpl_idx() %% n) + 1)
  })
  observeEvent(input$pedigree_tpl_confirm, {
    nm <- pedigree_tpl_names[pedigree_tpl_idx()]
    load_family_pedigree(nm)
  })
  
  observeEvent(input$plot_text_size,
               {
                 v <- suppressWarnings(as.numeric(input$plot_text_size))
                 if (is.finite(v) && v > 0) plot_settings$cex <- v
               },
               ignoreInit = TRUE
  )
  observeEvent(input$plot_symbol_size,
               {
                 v <- suppressWarnings(as.numeric(input$plot_symbol_size))
                 if (is.finite(v) && v > 0) plot_settings$symbolsize <- v
               },
               ignoreInit = TRUE
  )
  observeEvent(input$plot_margins,
               {
                 v <- suppressWarnings(as.numeric(input$plot_margins))
                 if (is.finite(v) && v >= 0) plot_settings$mar <- v
               },
               ignoreInit = TRUE
  )
  observeEvent(input$plot_settings_reset, {
    updateNumericInput(session, "plot_text_size", value = 1)
    updateNumericInput(session, "plot_symbol_size", value = 1)
    updateNumericInput(session, "plot_margins", value = 5)
    plot_settings$cex <- 1
    plot_settings$symbolsize <- 1
    plot_settings$mar <- 5
  })
  
  output$pedigree_tpl_label <- renderText({
    pedigree_tpl_names[pedigree_tpl_idx()]
  })
  output$pedigree_tpl_preview <- renderPlot({
    nm <- pedigree_tpl_names[pedigree_tpl_idx()]
    p <- tryCatch(families[[nm]](), error = function(e) NULL)
    if (is.null(p)) {
      plot.new()
      title(main = nm)
      return()
    }
    p <- tryCatch(relabel_gen(p), error = function(e) p)
    op <- par(mar = c(0, 0, 0, 0))
    on.exit(par(op))
    tryCatch(plot(p), error = function(e) {
      plot.new()
      title(main = nm)
    })
  })
  
  observeEvent(input$tab_structure, {
    active_tab("structure")
  })
  observeEvent(input$tab_legend, {
    active_tab("legend")
  })
  observeEvent(input$tab_family, {
    active_tab("family")
  })
  observeEvent(input$tab_personal, {
    active_tab("personal")
  })
  observeEvent(input$tab_link, {
    active_tab("link")
  })
  observeEvent(input$tab_relationship, {
    active_tab("relationship")
  })
  observeEvent(input$legend_nav_conventions, {
    legend_panel("conventions")
  })
  observeEvent(input$legend_nav_actions, {
    legend_panel("actions")
  })
  observeEvent(input$legend_nav_phenotypes, {
    legend_panel("phenotypes")
  })
  observeEvent(input$personal_nav_info, {
    personal_panel("info")
  })
  
  observeEvent(input$personal_nav_family, {
    personal_panel("family")
  })
  
  observeEvent(input$personal_nav_relationship, {
    personal_panel("relationship")
  })
  observeEvent(input$pedigree_choice_select, {
    req(input$pedigree_choice_select)
    load_family_pedigree(input$pedigree_choice_select)
  })
  
  observe({
    current_tab <- active_tab()
    shinyjs::runjs(
      "document.querySelectorAll('.sidebar-nav__item').forEach(el => el.classList.remove('active'));"
    )
    shinyjs::runjs(
      paste0(
        "var tab = document.getElementById('",
        session$ns(paste0("tab_", current_tab)),
        "'); if (tab) { tab.classList.add('active'); }"
      )
    )
  })
  
  
  output$pedigreeDataSummary <- renderUI({
    req(isTRUE(pedigree_data_mode()), ped())
    
    ids <- setdiff(labels(ped()), miscarriage())
    founder_ids <- tryCatch(
      pedtools::founders(ped()),
      error = function(e) character(0)
    )
    founder_ids <- intersect(ids, founder_ids)
    non_founder_ids <- setdiff(ids, founder_ids)
    female_ids <- intersect(ids, females(ped()))
    male_ids <- intersect(ids, males(ped()))
    untyped_ids <- setdiff(ids, union(female_ids, male_ids))
    generations_n <- tryCatch(
      pedtools::generations(ped(), what = "max"),
      error = function(e) NA_real_
    )
    if (!is.finite(generations_n)) {
      generations_n <- NA_real_
    }
    inbreeding_loop_count <- length(safe_inbreeding_loops(ped()))
    leaf_ids <- ids[vapply(
      ids,
      function(id) {
        length(tryCatch(
          pedtools::children(ped(), id, internal = FALSE),
          error = function(e) character(0)
        )) ==
          0
      },
      logical(1)
    )]
    
    stat_card <- function(
    label,
    value,
    category = NULL,
    icon_name = NULL,
    style_key = NULL,
    onclick = NULL
    ) {
      card_key <- category %||%
        style_key %||%
        tolower(gsub("[^a-zA-Z0-9]+", "", label))
      card_icon <- icon_name %||% switch(card_key,
                                         individuals = "groups",
                                         founders = "account_tree",
                                         nonfounders = "hub",
                                         leaves = "eco",
                                         generations = "timeline",
                                         loops = "all_inclusive",
                                         females = "female",
                                         males = "male",
                                         untyped = "radio_button_unchecked",
                                         "analytics"
      )
      card_contents <- tagList(
        p(class = "pedigree-data-card__label", label),
        p(class = "pedigree-data-card__value", as.character(value)),
        span(
          class = "pedigree-data-card__icon",
          span(class = "material-symbols-outlined", card_icon)
        )
      )
      
      if (!is.null(category)) {
        div(
          class = paste(
            "pedigree-data-card pedigree-data-card--btn",
            paste0("pedigree-data-card--", card_key)
          ),
          onmousedown = sprintf(
            "$(this).addClass('active'); Shiny.setInputValue('summary_highlight', '%s', {priority: 'event'});",
            category
          ),
          onmouseup = "$(this).removeClass('active'); Shiny.setInputValue('summary_highlight', null, {priority: 'event'});",
          onmouseleave = "$(this).removeClass('active'); Shiny.setInputValue('summary_highlight', null, {priority: 'event'});",
          ontouchstart = sprintf(
            "$(this).addClass('active'); Shiny.setInputValue('summary_highlight', '%s', {priority: 'event'});",
            category
          ),
          ontouchend = "$(this).removeClass('active'); Shiny.setInputValue('summary_highlight', null, {priority: 'event'});",
          card_contents
        )
      } else if (!is.null(onclick)) {
        div(
          class = paste(
            "pedigree-data-card pedigree-data-card--btn",
            paste0("pedigree-data-card--", card_key)
          ),
          onclick = onclick,
          card_contents
        )
      } else {
        div(
          class = paste(
            "pedigree-data-card",
            paste0("pedigree-data-card--", card_key)
          ),
          card_contents
        )
      }
    }
    
    cards <- tagList(
      stat_card("Individuals", length(ids), style_key = "individuals"),
      stat_card("Founders", length(founder_ids), category = "founders"),
      stat_card("Non-founders", length(non_founder_ids), category = "nonfounders"),
      stat_card("Leaves", length(leaf_ids), category = "leaves"),
      stat_card(
        "Generations",
        if (is.na(generations_n)) {
          "\u2014"
        } else {
          as.integer(generations_n)
        },
        style_key = "generations"
      ),
      stat_card(
        "Inbreeding Loops",
        inbreeding_loop_count,
        style_key = "loops",
        onclick = "Shiny.setInputValue('show_inbreeding_details', Date.now(), {priority: 'event'});"
      ),
      stat_card("Females", length(female_ids), category = "females"),
      stat_card("Males", length(male_ids), category = "males"),
      stat_card("Untyped Members", length(untyped_ids), category = "untyped")
    )
    
    tagList(
      div(
        class = "pedigree-data-carousel",
        tags$button(
          type = "button",
          class = "pedigree-data-carousel__nav",
          onclick = "window.pedigreeDataCarouselMove && window.pedigreeDataCarouselMove(-1);",
          span(class = "material-symbols-outlined", "chevron_left")
        ),
        div(
          class = "pedigree-data-carousel__viewport",
          div(
            id = "pedigreeDataCarouselTrack",
            class = "pedigree-data-grid",
            cards
          )
        ),
        tags$button(
          type = "button",
          class = "pedigree-data-carousel__nav",
          onclick = "window.pedigreeDataCarouselMove && window.pedigreeDataCarouselMove(1);",
          span(class = "material-symbols-outlined", "chevron_right")
        )
      ),
      p(
        class = "pedigree-data-hint",
        tags$i(class = "bi bi-hand-index", `aria-hidden` = "true"),
        "Press and hold a card to highlight individuals. Click Inbreeding Loops to inspect detected loops."
      ),
      uiOutput("consanguinityContent")
    )
  })
  
  output$pedigreeTablePanel <- renderUI({
    req(isTRUE(pedigree_table_mode()))
    
    div(
      class = "pedigree-table-wrap",
      div(
        class = "section-heading",
        div(
          class = "section-heading__icon",
          tags$i(class = "bi bi-table", `aria-hidden` = "true")
        ),
        div(
          h3(class = "section-heading__title", "Pedigree Table"),
          p(
            class = "section-heading__subtitle",
            "Detailed individuals, status markers and phenotype assignments for the current pedigree."
          )
        )
      ),
      hr(class = "section-divider", style = "margin: 0 0 12px 0;"),
      uiOutput("pedigreeTableViewControls"),
      uiOutput("pedigreeTableContent")
    )
  })
  
  plotAlignment <- reactive({
    req(ped())
    pedtools:::.pedAlignment(
      ped(),
      align = c(1.5, 2),
      twins = if (nrow(twins_df()) > 0) twins_df() else NULL,
      miscarriage = if (length(miscarriage()) > 0) miscarriage() else NULL
    )
  })
  
  summary_highlight_ids <- reactive({
    cat <- input$summary_highlight
    if (is.null(cat) || !isTRUE(nzchar(cat))) {
      return(character(0))
    }
    req(ped())
    all_ids <- setdiff(labels(ped()), miscarriage())
    switch(cat,
           founders = {
             f <- tryCatch(pedtools::founders(ped()), error = function(e) character(0))
             intersect(all_ids, f)
           },
           nonfounders = {
             f <- tryCatch(pedtools::founders(ped()), error = function(e) character(0))
             setdiff(all_ids, f)
           },
           leaves = {
             all_ids[vapply(all_ids, function(id) {
               length(tryCatch(pedtools::children(ped(), id, internal = FALSE),
                               error = function(e) character(0)
               )) == 0
             }, logical(1))]
           },
           females = intersect(all_ids, females(ped())),
           males = intersect(all_ids, males(ped())),
           untyped = {
             f_ids <- intersect(all_ids, females(ped()))
             m_ids <- intersect(all_ids, males(ped()))
             setdiff(all_ids, union(f_ids, m_ids))
           },
           character(0)
    )
  })
  
  plotAnnotation <- reactive({
    req(ped())
    ids <- labels(ped())
    labs <- setNames(ids, ids)
    
    fill_vec <- character(0)
    if (length(pheno_styles$fill) > 0) {
      fill_vec <- pheno_styles$fill
    }
    if (length(sel()) > 0) {
      for (sid in sel()) {
        fill_vec[sid] <- "#cce5ff"
      }
    }
    
    hl_ids <- summary_highlight_ids()
    if (length(hl_ids) > 0) {
      for (hid in hl_ids) {
        fill_vec[hid] <- "#EF4444"
      }
    }
    
    direct_ids <- input$summary_highlight_ids_direct
    if (!is.null(direct_ids) && length(direct_ids) > 0) {
      for (did in direct_ids) {
        if (did %in% ids) {
          fill_vec[did] <- "#3B82F6"
        }
      }
    }
    
    col_param <- if (length(pheno_styles$col) > 0) {
      col_list <- list()
      unique_colors <- unique(pheno_styles$col)
      for (color in unique_colors) {
        ids_with_color <- names(pheno_styles$col)[
          pheno_styles$col == color
        ]
        col_list[[color]] <- ids_with_color
      }
      if (length(sel()) > 0) {
        for (color in names(col_list)) {
          col_list[[color]] <- setdiff(col_list[[color]], sel())
        }
        col_list[["#0d6efd"]] <- union(col_list[["#0d6efd"]], sel())
      }
      col_list
    } else if (length(sel()) > 0) {
      list("#0d6efd" = sel())
    } else {
      list()
    }
    
    hatch_arg <- if (length(pheno_styles$hatched) > 0) {
      pheno_styles$hatched
    } else {
      NULL
    }
    
    lty_param <- if (length(pheno_styles$lty) > 0) {
      split(names(pheno_styles$lty), unname(pheno_styles$lty))
    } else {
      1
    }
    
    fill_arg <- if (length(fill_vec) > 0) fill_vec else NA
    
    textAnnot_list <- list()
    for (pos in c(
      "top",
      "bottom",
      "left",
      "right",
      "topleft",
      "topright",
      "bottomleft",
      "bottomright",
      "inside"
    )) {
      annotations <- text_annotations[[pos]]
      if (length(annotations) > 0) {
        textAnnot_list[[pos]] <- unlist(annotations)
      }
    }
    
    pedtools:::.pedAnnotation(
      ped(),
      labs = labs,
      fill = fill_arg,
      hatched = hatch_arg,
      col = col_param,
      lty = lty_param,
      carrier = if (length(carrier_ids()) > 0) carrier_ids() else NULL,
      deceased = if (length(deceased_ids()) > 0) deceased_ids() else NULL,
      starred = if (length(starred_ids()) > 0) starred_ids() else NULL,
      proband = if (length(proband_id()) > 0) proband_id() else NULL,
      textAnnot = if (length(textAnnot_list) > 0) {
        formatAnnot(
          textAnnot_list,
          cex = annot_style$cex,
          font = annot_style$font,
          col = annot_style$col
        )
      } else {
        NULL
      }
    )
  })
  
  legend_panel_tabs <- function() {
    current_panel <- legend_panel()
    
    div(
      class = "segment-control legend-segment",
      tags$button(
        type = "button",
        class = paste(
          "segment-btn",
          if (identical(current_panel, "conventions")) {
            "active"
          } else {
            ""
          }
        ),
        "Conventions",
        onclick = "Shiny.setInputValue('legend_nav_conventions', Date.now(), {priority:'event'});"
      ),
      tags$button(
        type = "button",
        class = paste(
          "segment-btn",
          if (identical(current_panel, "actions")) "active" else ""
        ),
        "Edit Individual",
        onclick = "Shiny.setInputValue('legend_nav_actions', Date.now(), {priority:'event'});"
      ),
      tags$button(
        type = "button",
        class = paste(
          "segment-btn",
          if (identical(current_panel, "phenotypes")) "active" else ""
        ),
        "Phenotypes",
        onclick = "Shiny.setInputValue('legend_nav_phenotypes', Date.now(), {priority:'event'});"
      )
    )
  }
  personal_panel_tabs <- function() {
    current_panel <- personal_panel()
    
    div(
      class = "segment-control legend-segment",
      tags$button(
        type = "button",
        class = paste(
          "segment-btn",
          if (identical(current_panel, "info")) "active" else ""
        ),
        "Personal Info",
        onclick = "Shiny.setInputValue('personal_nav_info', Date.now(), {priority:'event'});"
      ),
      tags$button(
        type = "button",
        class = paste(
          "segment-btn",
          if (identical(current_panel, "family")) "active" else ""
        ),
        "Family",
        onclick = "Shiny.setInputValue('personal_nav_family', Date.now(), {priority:'event'});"
      ),
      tags$button(
        type = "button",
        class = paste(
          "segment-btn",
          if (identical(current_panel, "relationship")) "active" else ""
        ),
        "Relationship",
        onclick = "Shiny.setInputValue('personal_nav_relationship', Date.now(), {priority:'event'});"
      )
    )
  }
  legend_conventions_pane <- function() {
    div(
      class = "legend-pane",
      div(
        class = "structure-card",
        p(class = "structure-card__label", "Standard Conventions"),
        p(
          class = "structure-card__text",
          "Pedigrees use shared symbols, labels and line rules so every diagram can be read consistently by different users."
        ),
        p(
          class = "structure-card__text",
          "The chart title identifies the family or case being shown."
        ),
        p(
          class = "structure-card__text",
          "Symbols, connectors and labels follow the same reading logic across examples."
        ),
        p(
          class = "structure-card__text",
          "This makes comparison across generations faster and more reliable."
        ),
        p(
          class = "structure-card__text",
          style = "margin-bottom: 0;",
          "For technical reasons, some standard pedigree symbols could not be reproduced exactly in this application."
        )
      ),
      div(
        class = "structure-card",
        div(
          class = "structure-card__label",
          "PHENOTYPE ANNOTATIONS"
        ),
        p(
          class = "structure-card__text",
          "To allow users to enrich the pedigree with their own observations, a phenotype annotation feature has been introduced.
    This functionality enables users to add custom phenotype labels or visual markers directly to individuals in the pedigree diagram.
    These annotations help document clinical traits, genetic characteristics, or any other relevant information, making the diagram
    more informative and adaptable to different research or analysis needs."
        )
      )
    )
  }
  
  legend_actions_pane <- function() {
    div(
      class = "legend-pane",
      div(
        class = "structure-card",
        p(class = "structure-card__label", "Try It Yourself"),
        p(
          class = "structure-card__text",
          style = "margin-bottom: 0;",
          "Select an individual by clicking on them in the pedigree, then modify their display or status by clicking the legend buttons below."
        )
      ),
      div(
        class = "box",
        div(class = "legend-label", "Gender"),
        div(
          class = "legend-group",
          actionButton(
            "btn_set_male",
            tagList(
              span(
                class = "material-symbols-outlined",
                "crop_square"
              ),
              "Male"
            ),
            class = "legend-btn"
          ),
          actionButton(
            "btn_set_female",
            tagList(
              span(
                class = "material-symbols-outlined",
                "circle"
              ),
              "Female"
            ),
            class = "legend-btn"
          ),
          actionButton(
            "btn_set_unknown",
            tagList(
              span(
                class = "material-symbols-outlined",
                "thermostat_carbon"
              ),
              "Undefined"
            ),
            class = "legend-btn"
          )
        ),
        br(),
        div(class = "legend-label", "Assigned at Birth"),
        p(
          class = "structure-card__text",
          style = "margin: 0 0 10px 0;",
          "Assigned at birth describes the sex recorded at birth. It can be useful in medical or genetic notation when it differs from the current gender identity shown in the pedigree."
        ),
        div(
          class = "legend-group",
          actionButton(
            "AFAB",
            tagList(
              span(class = "legend-badge", "AFAB"),
              "Assigned Female at Birth"
            ),
            class = "legend-btn"
          ),
          actionButton(
            "AMAB",
            tagList(
              span(class = "legend-badge", "AMAB"),
              "Assigned Male at Birth"
            ),
            class = "legend-btn"
          ),
          actionButton(
            "UMAB",
            tagList(
              span(class = "legend-badge", "UMAB"),
              "Undetermined at Birth"
            ),
            class = "legend-btn"
          )
        ),
        br(),
        div(class = "legend-label", "Reference Individual"),
        p(
          class = "structure-card__text",
          style = "margin: 0 0 10px 0;",
          "The proband is the reference person from whom the pedigree is described or investigated. They are often the first person who prompted the family study."
        ),
        div(
          class = "legend-group",
          actionButton(
            "set_proband",
            tagList(
              span(
                class = "material-symbols-outlined",
                "arrow_outward"
              ),
              "Proband"
            ),
            class = "legend-btn"
          )
        ),
        br(),
        div(class = "legend-label", "Life Events"),
        div(
          class = "legend-group",
          actionButton(
            "btn_toggle_deceased",
            tagList(
              span(
                class = "material-symbols-outlined",
                "person_off"
              ),
              "Deceased"
            ),
            class = "legend-btn"
          ),
          actionButton(
            "set_miscarriage",
            tagList(
              span(
                class = "material-symbols-outlined",
                "change_history"
              ),
              "Miscarriage"
            ),
            class = "legend-btn"
          )
        ),
        br(),
        div(class = "legend-label", "Family Situation"),
        div(
          class = "legend-group",
          actionButton(
            "fertility",
            tagList(
              span(
                class = "material-symbols-outlined",
                "align_flex_end"
              ),
              "Infertility"
            ),
            class = "legend-btn"
          ),
          actionButton(
            "set_adopted",
            tagList(
              span(
                class = "material-symbols-outlined",
                "data_array"
              ),
              "Adopted"
            ),
            class = "legend-btn"
          )
        )
      )
    )
  }
  
  legend_phenotypes_pane <- function() {
    div(
      class = "legend-pane legend-phenotype-pane",
      div(
        class = "structure-card",
        p(class = "structure-card__label", "Phenotypes"),
        p(
          class = "structure-card__text",
          style = "margin-bottom: 0;",
          "Create phenotype styles and assign them to selected individuals. Multiple phenotype elements can be superimposed on the same individual, allowing complex annotations to be displayed simultaneously."
        )
      ),
      div(
        class = "box",
        actionButton(
          "newPheno",
          tagList(
            span(
              class = "material-symbols-outlined",
              "add_circle"
            ),
            "Create Phenotype"
          ),
          class = "legend-btn"
        ),
        # div(
        #   class = "motif-config-toggle-row",
        #   div(
        #       id = "motif_config_toggle_wrapper",
        #       class = "icon-button_bis",
        #       tags$button(
        #           id = "toggle_motif_config_mode",
        #         class = "action-btn_bis",
        #          HTML(
        #             '<span class="material-symbols-outlined">tune</span>'
        #         )
        #     ),
        #     div(class = "icon-title_bis", "Motif Config")
        #  ),
        # span(
        #      id = "motif_config_status",
        #     class = "motif-config-status",
        #      "OFF"
        #   )
        # ),
        # uiOutput("motifConfigLibraryUI"),
        br(),
        uiOutput("phenoButtonsUI"),
        br(),
        uiOutput("legendUI")
      )
    )
  }
  
  output$pedPlot <- renderPlot(
    {
      req(ped())
      pd <- values$pedData
      al <- plotAlignment()
      an <- plotAnnotation()
      classic <- display_classic_mode()
      stats <- display_stats_mode()
      sc <- safe_ped_scaling(
        al,
        an,
        cex = plot_settings$cex,
        symbolsize = plot_settings$symbolsize,
        margins = rep(plot_settings$mar, 4),
        autoScale = TRUE
      )
      
      render_pedigree_to_device(
        al = al,
        an = an,
        sc = sc,
        ids = labels(ped()),
        title_text = input$pedigree_title_structure %||% "Family",
        adopted = adopted_ids(),
        afab = afab_ids(),
        amab = amab_ids(),
        umab = umab_ids(),
        infertility = infertility_ids(),
        ped_obj = ped(),
        ped_data = pd,
        deceased = deceased_ids(),
        classic_mode = classic,
        stats_mode = stats,
        selected_id = sel(),
        phenotypes_list = phenotypes$list,
        phenotypes_assign = phenotypes$assign,
        motif_configs = get_effective_motif_configs(),
        show_motifs = TRUE,
        show_stats = stats
      )
      
      ctrs(data.frame(
        x = al$xall + sc$boxw / 2,
        y = al$yall + sc$boxh / 2,
        id_plot = al$plotord
      ))
    },
    res = 96,
    bg = "transparent"
  )
  
  observe({
    req(ped())
    pd <- values$pedData
    al <- plotAlignment()
    an <- plotAnnotation()
    
    sc <- safe_ped_scaling(
      al,
      an,
      cex = plot_settings$cex,
      symbolsize = plot_settings$symbolsize,
      margins = rep(plot_settings$mar, 4),
      autoScale = TRUE
    )
    
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 900, height = 650, res = 150, bg = "white")
    tryCatch(
      {
        render_pedigree_to_device(
          al = al,
          an = an,
          sc = sc,
          ids = labels(ped()),
          title_text = input$pedigree_title_structure %||% "Family",
          adopted = adopted_ids(),
          afab = afab_ids(),
          amab = amab_ids(),
          umab = umab_ids(),
          infertility = infertility_ids(),
          ped_obj = ped(),
          ped_data = pd,
          deceased = deceased_ids(),
          classic_mode = display_classic_mode(),
          stats_mode = display_stats_mode(),
          selected_id = floating_individual_id() %||% character(0),
          phenotypes_list = phenotypes$list,
          phenotypes_assign = phenotypes$assign,
          motif_configs = get_effective_motif_configs(),
          show_motifs = TRUE,
          show_stats = display_stats_mode()
        )
        dev.off()
      },
      error = function(e) {
        try(dev.off(), silent = TRUE)
      }
    )
    
    if (file.exists(tmp) && file.info(tmp)$size > 0) {
      raw <- readBin(tmp, "raw", file.info(tmp)$size)
      unlink(tmp)
      b64 <- paste0("data:image/png;base64,", base64enc::base64encode(raw))
      
      session$sendCustomMessage(
        "syncPedigree",
        list(
          img = b64,
          title = input$pedigree_title_structure %||% "Family Pedigree",
          selected_id = floating_individual_id() %||% "",
          timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
        )
      )
    } else {
      unlink(tmp)
    }
  })
  
  # ── Genomic Tools (Search page) ──
  gene_data <- reactiveVal(NULL)
  gene_error <- reactiveVal(NULL)
  
  safe_api <- function(url, timeout_sec = 12) {
    tryCatch(
      {
        res <- httr::GET(url, httr::timeout(timeout_sec))
        if (httr::status_code(res) != 200) {
          return(NULL)
        }
        jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"),
                           simplifyVector = FALSE
        )
      },
      error = function(e) NULL
    )
  }
  safe_api_post <- function(url, body_json, timeout_sec = 12) {
    tryCatch(
      {
        res <- httr::POST(url,
                          body = body_json, encode = "raw",
                          httr::add_headers("Content-Type" = "application/json"),
                          httr::timeout(timeout_sec)
        )
        if (httr::status_code(res) != 200) {
          return(NULL)
        }
        jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"),
                           simplifyVector = FALSE
        )
      },
      error = function(e) NULL
    )
  }
  
  gene_card <- function(header_class, header_text, ...) {
    div(
      class = "structure-card gallery-card gene-card",
      tags$div(class = paste("gene-card__header", header_class), header_text),
      ...
    )
  }
  
  observeEvent(input$gene_search_btn, {
    req(nchar(trimws(input$gene_query %||% "")) > 0)
    gene_data(NULL)
    gene_error(NULL)
    query <- trimws(input$gene_query)
    tryCatch(
      {
        data <- safe_api(paste0(
          "https://mygene.info/v3/query?q=symbol:", URLencode(query),
          "&species=human&fields=symbol,name,summary,alias,type_of_gene,",
          "genomic_pos,entrezgene,ensembl.gene,uniprot.Swiss-Prot,MIM&size=1"
        ))
        if (is.null(data)) stop("MyGene.info unavailable")
        hit <- if (length(data$hits) > 0) {
          data$hits[[1]]
        } else {
          d2 <- safe_api(paste0(
            "https://mygene.info/v3/query?q=", URLencode(query),
            "&species=human&fields=symbol,name,summary,alias,type_of_gene,",
            "genomic_pos,entrezgene,ensembl.gene,uniprot.Swiss-Prot,MIM&size=1"
          ))
          if (!is.null(d2) && length(d2$hits) > 0) d2$hits[[1]] else NULL
        }
        if (is.null(hit)) {
          gene_error(paste0("Gene '", query, "' not found."))
          return()
        }
        sym <- hit$symbol %||% query
        up_id <- NULL
        if (!is.null(hit$uniprot)) {
          sp <- hit$uniprot[["Swiss-Prot"]]
          up_id <- if (is.list(sp)) sp[[1]] else sp
        }
        ens_id <- NULL
        if (!is.null(hit$ensembl)) {
          if (!is.null(hit$ensembl$gene)) {
            ens_id <- hit$ensembl$gene
          } else if (length(hit$ensembl) > 0) ens_id <- hit$ensembl[[1]]$gene
        }
        prot <- if (!is.null(up_id)) safe_api(paste0("https://rest.uniprot.org/uniprotkb/", up_id, ".json"))
        ot_diseases <- NULL
        if (!is.null(ens_id)) {
          ot_body <- paste0(
            '{"query":"{ target(ensemblId:\\"', ens_id,
            '\\") { associatedDiseases(page:{size:15,index:0}) { count rows { disease { name } score } } } }"}'
          )
          ot_diseases <- safe_api_post("https://api.platform.opentargets.org/api/v4/graphql", ot_body)
        }
        reactome <- safe_api(paste0(
          "https://reactome.org/ContentService/search/query?query=", URLencode(sym),
          "&species=Homo%20sapiens&types=Pathway&cluster=true"
        ))
        panelapp <- safe_api(paste0(
          "https://panelapp.genomicsengland.co.uk/api/v1/genes/", URLencode(sym), "/"
        ))
        pubs <- safe_api(paste0(
          "https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=", URLencode(sym),
          "&format=json&pageSize=6&sort=DATE_DESC"
        ))
        clinvar <- safe_api(paste0(
          "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=clinvar&term=",
          URLencode(sym), "[gene]&retmode=json&retmax=0"
        ))
        gene_data(list(
          hit = hit, uniprot_id = up_id, protein = prot,
          ensembl_id = ens_id, ot_diseases = ot_diseases, reactome = reactome,
          panelapp = panelapp, publications = pubs, clinvar = clinvar
        ))
      },
      error = function(e) gene_error(paste0("API error: ", e$message))
    )
  })
  
  observeEvent(input$gene_quick, {
    updateTextInput(session, "gene_query", value = input$gene_quick)
    session$sendCustomMessage("click_gene_search", "")
  })
  
  output$gene_status <- renderUI({
    if (!is.null(gene_error())) {
      div(
        style = "color:#cc3d4a; font-size:13px;",
        HTML('<i class="bi bi-exclamation-triangle"></i> '), gene_error()
      )
    } else if (!is.null(gene_data())) {
      g <- gene_data()
      n <- sum(!sapply(g[c("hit", "protein", "ot_diseases", "reactome", "panelapp", "publications", "clinvar")], is.null))
      div(
        style = "color:#16a06a; font-size:13px;",
        HTML('<i class="bi bi-check-circle"></i>'), paste0(" ", n, " APIs queried.")
      )
    }
  })
  
  output$gene_overview_ui <- renderUI({
    req(gene_data())
    g <- gene_data()
    hit <- g$hit
    sym <- hit$symbol %||% "\u2014"
    nm <- hit$name %||% "\u2014"
    smry <- hit$summary %||% "No summary available."
    chr <- ""
    if (!is.null(hit$genomic_pos)) {
      p <- hit$genomic_pos
      chr <- if (!is.null(p$chr)) {
        paste("Chr", p$chr)
      } else if (length(p) > 0) paste("Chr", p[[1]]$chr) else ""
    }
    gt <- hit$type_of_gene %||% ""
    als <- if (is.list(hit$alias)) {
      paste(head(unlist(hit$alias), 5), collapse = ", ")
    } else {
      hit$alias %||% "\u2014"
    }
    gene_card(
      "gc-h-green", paste0("Gene Overview \u2014 ", sym),
      div(
        style = "display:flex;gap:8px;margin-bottom:12px;",
        if (nchar(chr) > 0) tags$span(class = "badge gene-chip", chr),
        if (nchar(gt) > 0) tags$span(class = "badge gene-chip", gt)
      ),
      h4(nm),
      div(style = "border-left:3px solid #e0e0e0;padding-left:14px;margin:12px 0;color:#48484a;font-size:14px;", smry),
      div(
        style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:8px;font-size:13px;",
        if (!is.null(hit$entrezgene)) {
          div(
            div(style = "color:#86868b;font-size:11px;", "ENTREZ"),
            tags$a(href = paste0("https://ncbi.nlm.nih.gov/gene/", hit$entrezgene), target = "_blank", hit$entrezgene)
          )
        },
        if (!is.null(g$ensembl_id)) {
          div(
            div(style = "color:#86868b;font-size:11px;", "ENSEMBL"),
            tags$a(href = paste0("https://ensembl.org/Homo_sapiens/Gene/Summary?g=", g$ensembl_id), target = "_blank", g$ensembl_id)
          )
        },
        if (!is.null(g$uniprot_id)) {
          div(
            div(style = "color:#86868b;font-size:11px;", "UNIPROT"),
            tags$a(href = paste0("https://uniprot.org/uniprot/", g$uniprot_id), target = "_blank", g$uniprot_id)
          )
        },
        if (!is.null(hit$MIM)) {
          div(
            div(style = "color:#86868b;font-size:11px;", "OMIM"),
            tags$a(href = paste0("https://omim.org/entry/", hit$MIM), target = "_blank", hit$MIM)
          )
        },
        div(div(style = "color:#86868b;font-size:11px;", "ALIASES"), als)
      ),
      div(style = "font-size:11px;color:#86868b;margin-top:12px;", "From MyGene.info")
    )
  })
  
  output$gene_protein_ui <- renderUI({
    req(gene_data())
    g <- gene_data()
    if (is.null(g$protein)) {
      return(NULL)
    }
    prot <- g$protein
    desc <- tryCatch(prot$proteinDescription$recommendedName$fullName$value, error = function(e) "\u2014")
    slen <- tryCatch(prot$sequence$length, error = function(e) "\u2014")
    smass <- tryCatch(paste0(round(prot$sequence$molWeight / 1000, 1), " kDa"), error = function(e) "\u2014")
    locs <- tryCatch(
      {
        li <- Filter(function(x) x$commentType == "SUBCELLULAR LOCATION", prot$comments)
        if (length(li) > 0) paste(sapply(li[[1]]$subcellularLocations, function(x) x$location$value), collapse = ", ") else "\u2014"
      },
      error = function(e) "\u2014"
    )
    func <- tryCatch(
      {
        fi <- Filter(function(x) x$commentType == "FUNCTION", prot$comments)
        if (length(fi) > 0) paste(sapply(fi[[1]]$texts, function(x) x$value), collapse = " ") else "\u2014"
      },
      error = function(e) "\u2014"
    )
    gene_card(
      "gc-h-teal", "Protein Structure",
      div(
        style = "font-size:13px;",
        tags$b("UniProt: "), g$uniprot_id, tags$br(),
        tags$b("Length: "), paste(slen, "aa"), tags$br(),
        tags$b("Mass: "), smass, tags$br(),
        tags$b("Location: "), locs
      ),
      if (func != "\u2014") p(style = "margin-top:10px;font-size:13px;color:#48484a;", func),
      if (!is.null(g$uniprot_id)) {
        div(
          style = "margin-top:10px;",
          tags$a(
            href = paste0("https://alphafold.ebi.ac.uk/entry/", g$uniprot_id),
            target = "_blank", class = "btn btn-outline-primary btn-sm", "AlphaFold \u2197"
          )
        )
      },
      div(style = "font-size:11px;color:#86868b;margin-top:8px;", "From UniProt, AlphaFold")
    )
  })
  
  output$gene_string_ui <- renderUI({
    req(gene_data())
    sym <- gene_data()$hit$symbol
    if (is.null(sym)) {
      return(NULL)
    }
    img_url <- paste0(
      "https://string-db.org/api/image/network?identifiers=", URLencode(sym),
      "&species=9606&network_flavor=confidence&required_score=700"
    )
    gene_card(
      "gc-h-blue", "Interaction Network",
      p(style = "font-size:13px;color:#636366;", "Confidence-scored interactions."),
      tags$img(src = img_url, alt = "STRING", style = "width:100%;border-radius:8px;"),
      div(
        style = "margin-top:10px;",
        tags$a(
          href = paste0("https://string-db.org/cgi/network?identifiers=", URLencode(sym), "&species=9606"),
          target = "_blank", class = "btn btn-outline-primary btn-sm", "STRING \u2197"
        )
      ),
      div(style = "font-size:11px;color:#86868b;margin-top:8px;", "From STRING-db")
    )
  })
  
  output$gene_diseases_ui <- renderUI({
    req(gene_data())
    g <- gene_data()
    rows <- tryCatch(g$ot_diseases$data$target$associatedDiseases$rows, error = function(e) NULL)
    total <- tryCatch(g$ot_diseases$data$target$associatedDiseases$count, error = function(e) 0)
    if (is.null(rows) || length(rows) == 0) {
      return(NULL)
    }
    gene_card(
      "gc-h-orange", paste0("Disease Associations \u2014 ", total, " conditions"),
      tagList(lapply(rows, function(r) {
        nm <- tryCatch(r$disease$name, error = function(e) "Unknown")
        sc <- tryCatch(round(r$score * 100, 1), error = function(e) 0)
        cl <- if (sc > 50) "#cc3d4a" else if (sc > 20) "#c98a08" else "#86868b"
        div(
          style = "display:flex;justify-content:space-between;align-items:center;padding:6px 0;border-bottom:1px solid rgba(0,0,0,0.05);",
          span(style = "font-size:13px;", nm),
          span(style = paste0("font-family:monospace;font-size:13px;font-weight:600;color:", cl, ";"), paste0(sc, "%"))
        )
      })),
      div(style = "font-size:11px;color:#86868b;margin-top:8px;", "From OpenTargets")
    )
  })
  
  output$gene_panels_ui <- renderUI({
    req(gene_data())
    g <- gene_data()
    results <- tryCatch(g$panelapp$results, error = function(e) NULL)
    if (is.null(results) || length(results) == 0) {
      return(NULL)
    }
    gene_card(
      "gc-h-purple", paste0("Gene Panels (", length(results), ")"),
      tagList(lapply(results, function(r) {
        panel_name <- tryCatch(r$panel$name, error = function(e) "Unknown")
        conf <- tryCatch(r$confidence_level, error = function(e) "0")
        moi <- tryCatch(r$mode_of_inheritance, error = function(e) "")
        dot_col <- if (conf == "3") "#16a06a" else if (conf == "2") "#c98a08" else "#cc3d4a"
        div(
          style = "display:flex;justify-content:space-between;align-items:center;padding:6px 0;border-bottom:1px solid rgba(0,0,0,0.05);",
          div(
            span(style = paste0("display:inline-block;width:8px;height:8px;border-radius:50%;background:", dot_col, ";margin-right:8px;")),
            strong(style = "font-size:13px;", panel_name)
          ),
          span(style = "font-size:11px;color:#86868b;font-family:monospace;", moi)
        )
      })),
      p(style = "font-size:11px;color:#86868b;margin-top:4px;", "Green = high evidence, Amber = moderate."),
      div(style = "font-size:11px;color:#86868b;margin-top:8px;", "From Genomics England PanelApp")
    )
  })
  
  output$gene_pathways_ui <- renderUI({
    req(gene_data())
    g <- gene_data()
    entries <- tryCatch(
      {
        grp <- g$reactome$results
        if (length(grp) > 0) head(grp[[1]]$entries, 6) else NULL
      },
      error = function(e) NULL
    )
    if (is.null(entries) || length(entries) == 0) {
      return(NULL)
    }
    gene_card(
      "gc-h-green", paste0("Pathways (", length(entries), ")"),
      tagList(lapply(entries, function(en) {
        nm <- tryCatch(en$name, error = function(e) "Unknown")
        sid <- tryCatch(en$stId, error = function(e) "")
        div(
          style = "display:flex;justify-content:space-between;align-items:center;padding:6px 0;border-bottom:1px solid rgba(0,0,0,0.05);",
          span(style = "font-size:13px;", nm),
          if (nchar(sid) > 0) {
            tags$a(
              href = paste0("https://reactome.org/content/detail/", sid),
              target = "_blank", style = "font-size:11px;font-family:monospace;", sid
            )
          }
        )
      })),
      div(style = "font-size:11px;color:#86868b;margin-top:8px;", "From Reactome")
    )
  })
  
  output$gene_clinvar_ui <- renderUI({
    req(gene_data())
    g <- gene_data()
    sym <- g$hit$symbol
    total <- tryCatch(as.integer(g$clinvar$esearchresult$count), error = function(e) 0)
    if (is.na(total) || total == 0) {
      return(NULL)
    }
    gene_card(
      "gc-h-orange", paste0("Clinical Variants \u2014 ", format(total, big.mark = ","), " in ClinVar"),
      p(
        style = "font-size:13px;color:#48484a;",
        paste0(format(total, big.mark = ","), " variants registered in ClinVar for ", sym, ".")
      ),
      div(
        style = "margin-top:10px;",
        tags$a(
          href = paste0("https://www.ncbi.nlm.nih.gov/clinvar/?term=", sym, "[gene]"),
          target = "_blank", class = "btn btn-outline-primary btn-sm", "View in ClinVar \u2197"
        )
      ),
      div(style = "font-size:11px;color:#86868b;margin-top:8px;", "From NCBI ClinVar")
    )
  })
  
  output$gene_pubs_ui <- renderUI({
    req(gene_data())
    g <- gene_data()
    results <- tryCatch(g$publications$resultList$result, error = function(e) list())
    if (length(results) == 0) {
      return(NULL)
    }
    gene_card(
      "gc-h-gray", paste0("Recent Publications (", length(results), ")"),
      tagList(lapply(results, function(pub) {
        tt <- pub$title %||% "Untitled"
        jn <- pub$journalTitle %||% ""
        dt <- pub$firstPublicationDate %||% ""
        doi <- pub$doi
        ct <- pub$citedByCount %||% 0
        div(
          style = "padding:8px 0;border-bottom:1px solid rgba(0,0,0,0.05);",
          if (!is.null(doi)) {
            tags$a(
              href = paste0("https://doi.org/", doi), target = "_blank",
              style = "font-weight:600;font-size:13px;color:#1d1d1f;", tt
            )
          } else {
            strong(style = "font-size:13px;", tt)
          },
          div(
            style = "font-size:12px;color:#86868b;margin-top:2px;",
            paste0(jn, if (nchar(dt) > 0) paste0(" \u00b7 ", dt), if (ct > 0) paste0(" \u00b7 ", ct, " cit."))
          )
        )
      })),
      div(style = "font-size:11px;color:#86868b;margin-top:8px;", "From Europe PMC")
    )
  })
  
  output$gene_links_ui <- renderUI({
    req(gene_data())
    g <- gene_data()
    sym <- g$hit$symbol %||% ""
    up <- g$uniprot_id
    mk <- function(label, href) {
      tags$a(
        href = href, target = "_blank",
        class = "btn btn-outline-secondary btn-sm", label
      )
    }
    gene_card(
      "gc-h-gray", "External Resources",
      div(
        style = "display:flex;flex-wrap:wrap;gap:8px;",
        mk("NCBI Gene", paste0("https://www.ncbi.nlm.nih.gov/gene/?term=", sym)),
        if (!is.null(up)) mk("UniProt", paste0("https://www.uniprot.org/uniprot/", up)),
        mk("DECIPHER", paste0("https://www.deciphergenomics.org/search?q=", sym)),
        mk("gnomAD", paste0("https://gnomad.broadinstitute.org/gene/", sym, "?dataset=gnomad_r4")),
        mk("PubMed", paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", sym)),
        mk("Orphanet", paste0("https://www.orpha.net/en/disease/search?query=", sym)),
        mk("ClinVar", paste0("https://www.ncbi.nlm.nih.gov/clinvar/?term=", sym)),
        mk("STRING", paste0("https://string-db.org/cgi/network?identifiers=", sym, "&species=9606")),
        mk("Reactome", paste0("https://reactome.org/content/query?q=", sym)),
        mk("PanelApp", paste0("https://panelapp.genomicsengland.co.uk/panels/genes/", sym)),
        mk("GeneReviews", paste0("https://www.ncbi.nlm.nih.gov/books/NBK1116/?term=", sym))
      )
    )
  })
  
  # ── Gallery: example pedigrees ──
  scale_annotations_gallery <- function(annotations, factor = 0.65) {
    for (id in names(annotations)) {
      for (i in seq_along(annotations[[id]]$texts)) {
        annotations[[id]]$texts[[i]]$cex <- annotations[[id]]$texts[[i]]$cex * factor
      }
    }
    annotations
  }
  build_example_pedData <- function(ped_obj, info_list) {
    ids <- labels(ped_obj)
    pick <- function(id, key, default = "") {
      v <- info_list[[id]][[key]]
      if (is.null(v)) default else v
    }
    data.frame(
      id = ids,
      sex = pedtools::getSex(ped_obj, ids),
      first_name = vapply(ids, function(i) pick(i, "fn", ""), character(1)),
      last_name = vapply(ids, function(i) pick(i, "ln", ""), character(1)),
      date_of_birth = vapply(ids, function(i) pick(i, "dob", ""), character(1)),
      date_of_death = vapply(ids, function(i) pick(i, "dod", ""), character(1)),
      age = vapply(ids, function(i) pick(i, "age", ""), character(1)),
      age_unit = vapply(ids, function(i) pick(i, "age_u", "years"), character(1)),
      deceased = vapply(ids, function(i) pick(i, "dec", FALSE), logical(1)),
      comments = vapply(ids, function(i) pick(i, "com", ""), character(1)),
      assigned_at_birth = "",
      stringsAsFactors = FALSE
    )
  }
  
  example_colours <- list(
    ex_ad = c("Breast Cancer" = "#E74C3C", "Ovarian Cancer" = "#8E44AD", "Breast + Ovarian" = "#E67E22"),
    ex_ar = c("Cystic Fibrosis" = "#2980B9", "Carrier" = "#85C1E9"),
    ex_xlr = c("Affected Male (DMD)" = "#8E44AD", "Carrier Female" = "#D2B4DE")
  )
  example_descriptions <- list(
    ex_ad = tagList(p(
      style = "font-size:13px; color:#48484a; line-height:1.5;",
      strong("Autosomal Dominant"), " \u2014 Hereditary Breast & Ovarian Cancer (BRCA1).",
      " Affected individuals appear in every generation. Each child of an affected parent has a 50% chance of inheriting the pathogenic variant. Note the MZ twins in generation III and the miscarriage."
    )),
    ex_ar = tagList(p(
      style = "font-size:13px; color:#48484a; line-height:1.5;",
      strong("Autosomal Recessive"), " \u2014 Cystic Fibrosis (CFTR).",
      " Both parents are healthy carriers (heterozygous). Affected children receive two pathogenic copies of the variant (homozygous). Note the DZ twins and the miscarriage."
    )),
    ex_xlr = tagList(p(
      style = "font-size:13px; color:#48484a; line-height:1.5;",
      strong("X-Linked Recessive"), " \u2014 Duchenne Muscular Dystrophy (DMD).",
      " The pathogenic variant is on the X chromosome. Males (XY) with one copy are affected (hemizygous). Females (XX) with one copy are carriers. An affected father cannot transmit the condition to his sons."
    ))
  )
  
  output$example_legend <- renderUI({
    req(input$example_ped_choice)
    cols <- example_colours[[input$example_ped_choice]]
    tags$div(
      style = "display:flex; flex-direction:column; gap:6px;",
      lapply(seq_along(cols), function(i) {
        tags$div(
          style = "display:flex; align-items:center; gap:8px; font-size:12px;",
          tags$span(style = paste0(
            "display:inline-block; width:14px; height:14px; border-radius:3px;",
            "background:", cols[i], "; border:1px solid #ccc;"
          )),
          names(cols)[i]
        )
      }),
      tags$div(
        style = "display:flex; align-items:center; gap:8px; font-size:12px; margin-top:4px;",
        tags$span(
          style = "display:inline-block; width:14px; height:14px; background:white; border:1px solid #ccc; border-radius:3px; position:relative;",
          tags$span(style = "position:absolute; top:6px; left:0; width:14px; height:2px; background:#333; transform:rotate(-45deg);")
        ),
        "Deceased"
      ),
      tags$div(
        style = "display:flex; align-items:center; gap:8px; font-size:12px;",
        tags$span(style = "display:inline-block; width:0; height:0; border-left:7px solid transparent; border-right:7px solid transparent; border-bottom:14px solid #999;"),
        "Miscarriage"
      ),
      tags$div(
        style = "display:flex; align-items:center; gap:8px; font-size:12px;",
        tags$span(style = "display:inline-block; width:14px; text-align:center; font-size:14px; font-weight:bold;", "||"),
        "Twins"
      )
    )
  })
  output$example_description <- renderUI({
    req(input$example_ped_choice)
    example_descriptions[[input$example_ped_choice]]
  })
  
  output$example_ped_plot <- renderPlot(
    {
      req(input$example_ped_choice)
      choice <- input$example_ped_choice
      if (choice == "ex_ad") {
        x <- nuclearPed(3, sex = c(2, 1, 2))
        x <- addChildren(x, mother = 3, nch = 3, sex = c(2, 2, 1))
        x <- addChildren(x, father = 4, nch = 2, sex = c(1, 2))
        x <- relabel_gen(x)
        info <- list(
          "I-1"   = list(fn = "Robert", ln = "Doe", dob = "12-03-1938", dod = "05-11-2019", dec = TRUE, age = "81", age_u = "years"),
          "I-2"   = list(fn = "Mary", ln = "Doe", dob = "22-07-1941", com = "Breast Ca Dx 48y", age = "84", age_u = "years"),
          "II-1"  = list(fn = "Mark", ln = "Doe", dob = "01-09-1965"),
          "II-2"  = list(fn = "Catherine", ln = "Doe", dob = "14-02-1963", com = "Breast+Ovarian Dx 38y"),
          "II-3"  = list(fn = "Philip", ln = "Doe", dob = "30-06-1967"),
          "II-4"  = list(fn = "Nancy", ln = "Doe", dob = "18-04-1969"),
          "II-5"  = list(fn = "Isabel", ln = "Doe", dob = "25-12-1970", com = "Ovarian Ca Dx 51y"),
          "III-1" = list(fn = "Emma", ln = "Doe", dob = "11-05-1990", com = "Breast Ca Dx 29y"),
          "III-2" = list(fn = "Lily", ln = "Doe", dob = "11-05-1990"),
          "III-3" = list(fn = "Lucas", ln = "Doe", dob = "08-01-1993"),
          "III-4" = list(fn = "Thomas", ln = "Doe", dob = "20-09-1995"),
          "III-5" = list()
        )
        ped_data <- build_example_pedData(x, info)
        deceased_ids <- c("I-1")
        labs <- labels(x)
        fill_vec <- setNames(rep("white", length(labs)), labs)
        fill_vec[c("I-2", "II-2", "II-5", "III-1")] <- c("#E74C3C", "#E67E22", "#8E44AD", "#E74C3C")
        res <- plot(x,
                    fill = fill_vec, cex = 0.75, draw = FALSE,
                    twins = data.frame(id1 = "III-1", id2 = "III-2", code = 1),
                    miscarriage = "III-5", deceased = deceased_ids, proband = "III-1",
                    textAbove = c("III-1" = "BRCA1+", "III-2" = "BRCA1+", "II-2" = "BRCA1+", "I-2" = "BRCA1+", "II-5" = "BRCA1+"),
                    margins = c(3, 4, 8, 4),
                    title = "Autosomal Dominant \u2014 Hereditary Breast & Ovarian Cancer (BRCA1)"
        )
        pedtools::drawPed(res$alignment, annotation = res$annotation, scaling = res$scaling)
        draw_title_box("Autosomal Dominant \u2014 HBOC (BRCA1)")
        annots <- prepare_annotation_data(x, ped_data, deceased_ids_vec = deceased_ids)
        annots <- scale_annotations_gallery(annots, 0.65)
        draw_custom_annotations(res$alignment, annots, res$scaling, labs)
      } else if (choice == "ex_ar") {
        x <- nuclearPed(4, sex = c(1, 2, 1, 2))
        x <- addChildren(x, father = 3, nch = 2, sex = c(2, 1))
        x <- relabel_gen(x)
        info <- list(
          "I-1"   = list(fn = "Richard", ln = "Doe", dob = "08-04-1955", com = "Carrier CFTR"),
          "I-2"   = list(fn = "Susan", ln = "Doe", dob = "19-11-1958", com = "Carrier CFTR"),
          "II-1"  = list(fn = "Kevin", ln = "Doe", dob = "03-03-1982", com = "CF Dx at birth"),
          "II-2"  = list(fn = "Laura", ln = "Doe", dob = "22-01-1984"),
          "II-3"  = list(fn = "Rachel", ln = "Doe", dob = "17-06-1984", com = "Carrier"),
          "II-4"  = list(fn = "Brian", ln = "Doe", dob = "15-08-1985", dod = "12-04-2018", dec = TRUE, age = "32", age_u = "years", com = "CF Dx at birth"),
          "II-5"  = list(),
          "III-1" = list(fn = "Hannah", ln = "Doe", dob = "14-06-2010", com = "Carrier"),
          "III-2" = list(fn = "Ethan", ln = "Doe", dob = "27-02-2013")
        )
        ped_data <- build_example_pedData(x, info)
        deceased_ids <- c("II-4")
        labs <- labels(x)
        fill_vec <- setNames(rep("white", length(labs)), labs)
        fill_vec[c("II-1", "II-4", "I-1", "I-2", "II-3", "III-1")] <- c("#2980B9", "#2980B9", "#85C1E9", "#85C1E9", "#85C1E9", "#85C1E9")
        res <- plot(x,
                    fill = fill_vec, cex = 0.75, draw = FALSE,
                    twins = data.frame(id1 = "II-3", id2 = "II-4", code = 2),
                    miscarriage = "II-5", deceased = deceased_ids, proband = "II-1",
                    carrier = c("I-1", "I-2", "II-3", "III-1"),
                    textAbove = c("I-1" = "CFTR +/-", "I-2" = "CFTR +/-", "II-1" = "CFTR +/+", "II-4" = "CFTR +/+", "II-3" = "CFTR +/-", "III-1" = "CFTR +/-"),
                    margins = c(3, 4, 8, 4),
                    title = "Autosomal Recessive \u2014 Cystic Fibrosis (CFTR)"
        )
        pedtools::drawPed(res$alignment, annotation = res$annotation, scaling = res$scaling)
        draw_title_box("Autosomal Recessive \u2014 Cystic Fibrosis (CFTR)")
        annots <- prepare_annotation_data(x, ped_data, deceased_ids_vec = deceased_ids)
        annots <- scale_annotations_gallery(annots, 0.65)
        draw_custom_annotations(res$alignment, annots, res$scaling, labs)
      } else if (choice == "ex_xlr") {
        x <- nuclearPed(4, sex = c(1, 2, 1, 2))
        x <- addChildren(x, mother = 4, nch = 2, sex = c(1, 1))
        x <- relabel_gen(x)
        info <- list(
          "I-1"   = list(fn = "James", ln = "Doe", dob = "15-06-1950"),
          "I-2"   = list(fn = "Margaret", ln = "Doe", dob = "28-09-1953", com = "Carrier DMD"),
          "II-1"  = list(fn = "David", ln = "Doe", dob = "12-01-1978", dod = "10-03-2005", dec = TRUE, age = "27", age_u = "years", com = "DMD Dx 4y"),
          "II-2"  = list(fn = "John", ln = "Doe", dob = "11-08-1979"),
          "II-3"  = list(fn = "Sarah", ln = "Doe", dob = "07-07-1980", com = "Carrier"),
          "II-4"  = list(fn = "Andrew", ln = "Doe", dob = "20-11-1983", com = "DMD Dx 3y"),
          "II-5"  = list(fn = "Emily", ln = "Doe", dob = "04-05-1986", com = "Carrier"),
          "III-1" = list(fn = "Oliver", ln = "Doe", dob = "22-04-2012", com = "DMD Dx 5y"),
          "III-2" = list()
        )
        ped_data <- build_example_pedData(x, info)
        deceased_ids <- c("II-1")
        labs <- labels(x)
        fill_vec <- setNames(rep("white", length(labs)), labs)
        fill_vec[c("II-1", "II-4", "I-2", "II-3", "II-5", "III-1")] <- c("#8E44AD", "#8E44AD", "#D2B4DE", "#D2B4DE", "#D2B4DE", "#8E44AD")
        res <- plot(x,
                    fill = fill_vec, cex = 0.75, draw = FALSE,
                    miscarriage = "III-2", deceased = deceased_ids, proband = "II-1",
                    carrier = c("I-2", "II-3", "II-5"),
                    textAbove = c("I-2" = "Xd/X", "II-1" = "Xd/Y", "II-3" = "Xd/X", "II-4" = "Xd/Y", "II-5" = "Xd/X", "III-1" = "Xd/Y"),
                    margins = c(3, 4, 8, 4),
                    title = "X-Linked Recessive \u2014 Duchenne Muscular Dystrophy (DMD)"
        )
        pedtools::drawPed(res$alignment, annotation = res$annotation, scaling = res$scaling)
        draw_title_box("X-Linked Recessive \u2014 Duchenne (DMD)")
        annots <- prepare_annotation_data(x, ped_data, deceased_ids_vec = deceased_ids)
        annots <- scale_annotations_gallery(annots, 0.65)
        draw_custom_annotations(res$alignment, annots, res$scaling, labs)
      }
    },
    res = 96
  )
  
  observeEvent(input$pedClick, {
    req(ped(), input$pedClick, ctrs())
    idx <- nearest_click(ctrs(), input$pedClick, thresh = 25)
    if (is.na(idx)) {
      return()
    }
    id <- labels(ped())[idx]
    if (length(sel()) == 1 && identical(sel(), id)) {
      sel(character(0))
    } else {
      sel(id)
    }
  })
  
  observeEvent(input$pedDblClick, {
    req(ped(), input$pedDblClick, ctrs())
    idx <- nearest_click(ctrs(), input$pedDblClick, thresh = 25)
    if (is.na(idx)) {
      return()
    }
    id <- labels(ped())[idx]
    sel(id)
    floating_individual_id(id)
  })
  
  observeEvent(input$close_individual_float, {
    floating_individual_id(NULL)
  }, ignoreInit = TRUE)
  
  observeEvent(input$ctx_relationship, {
    req(ped())
    id <- ctx_id()
    if (!is.null(id) && id %in% labels(ped())) {
      sel(id)
    }
    relationship_window_open(TRUE)
    ctx_id(NULL)
  }, ignoreInit = TRUE)
  
  observeEvent(input$close_relationship_float, {
    relationship_window_open(FALSE)
  }, ignoreInit = TRUE)
  
  output$individualFloatingWindow <- renderUI({
    id <- floating_individual_id()
    req(id)
    
    div(
      id = "individualFloatingWindow",
      class = "individual-float individual-float--report",
      div(
        class = "individual-float__header",
        div(
          class = "individual-float__title",
          span(class = "material-symbols-outlined", "badge"),
          div(
            class = "individual-float__title-block",
            div(class = "individual-float__title-main", sprintf("Individual %s", id)),
            div(class = "individual-float__subtitle", "Report")
          )
        ),
        tags$button(
          type = "button",
          class = "individual-float__close",
          onclick = "Shiny.setInputValue('close_individual_float', Date.now(), {priority:'event'});",
          span(class = "material-symbols-outlined", "close")
        )
      ),
      div(
        class = "individual-float__body",
        tags$iframe(
          class = "individual-report-frame",
          src = "app_files/notes_app_general.html",
          title = sprintf("Report for individual %s", id)
        )
      )
    )
  })
  
  output$relationshipFloatingWindow <- renderUI({
    req(isTRUE(relationship_window_open()))
    
    div(
      id = "relationshipFloatingWindow",
      class = "relationship-float",
      div(
        class = "relationship-float__header",
        div(
          class = "relationship-float__title",
          span(
            class = "relationship-float__icon",
            span(class = "material-symbols-outlined", "swap_horiz")
          ),
          div(
            div(class = "relationship-float__title-text", "Relationship"),
            div(
              class = "relationship-float__subtitle",
              "Analyze genetic relationships and kinship between individuals."
            )
          )
        ),
        tags$button(
          type = "button",
          class = "relationship-float__close",
          onclick = "Shiny.setInputValue('close_relationship_float', Date.now(), {priority:'event'});",
          span(class = "material-symbols-outlined", "close")
        )
      ),
      div(
        class = "relationship-float__body",
        personal_relationship_pane()
      )
    )
  })
  
  output$editorContent <- renderUI({
    section_intro <- function(icon, title, subtitle) {
      tagList(
        div(
          class = "section-heading",
          div(
            class = "section-heading__icon",
            tags$i(
              class = paste("bi", icon),
              `aria-hidden` = "true"
            )
          ),
          div(
            h3(class = "section-heading__title", title),
            p(class = "section-heading__subtitle", subtitle)
          )
        ),
        hr(class = "section-divider")
      )
    }
    
    switch(active_tab(),
           structure = tagList(
             div(
               class = "section-page",
               section_intro(
                 "bi-bounding-box-circles",
                 "Structure",
                 "Choose and understand the overall family structure used to build the pedigree."
               ),
               div(
                 class = "structure-card",
                 p(
                   class = "structure-card__label",
                   "Identification Data"
                 ),
                 p(
                   class = "structure-card__text",
                   "The pedigree title identifies the family or case being shown, and each individual receives a generation-based ID."
                 ),
                 p(
                   class = "structure-card__text",
                   "Activate Pedigree Choices below to edit the title and switch between predefined pedigree structures."
                 ),
                 div(
                   class = "structure-generations",
                   span(class = "structure-dot", HTML("&bull;")),
                   span(
                     class = "structure-chip structure-chip--gen1",
                     "I-1"
                   ),
                   span(class = "structure-genlabel", "Generation I"),
                   span(class = "structure-dot", HTML("&bull;")),
                   span(
                     class = "structure-chip structure-chip--gen2",
                     "II-3"
                   ),
                   span(class = "structure-genlabel", "Generation II"),
                   span(class = "structure-dot", HTML("&bull;")),
                   span(
                     class = "structure-chip structure-chip--gen3",
                     "III-1"
                   ),
                   span(class = "structure-genlabel", "Generation III")
                 ),
                 p(
                   class = "structure-card__text",
                   style = "margin-bottom: 0;",
                   "The Roman numeral marks the generation, while the number identifies the person inside that generation."
                 )
               ),
               div(
                 class = "structure-card",
                 p(class = "structure-card__label", "Try It Yourself"),
                 p(
                   class = "structure-card__text",
                   "Try different predefined pedigrees from the carousel above and observe how each one's characteristics are reflected in the Pedigree Data Summary, Inbreeding, and Pedigree Table tabs."
                 ),
               ),
               div(
                 class = "structure-card structure-picker",
                 div(
                   class = "structure-picker__name",
                   tags$i(class = "bi bi-pencil", `aria-hidden` = "true"),
                   div(
                     class = "structure-picker__input-wrap",
                     textInput(
                       "pedigree_title_structure",
                       label = NULL,
                       value = "",
                       placeholder = "Family name (optional)",
                       width = "100%"
                     )
                   )
                 ),
                 div(
                   class = "structure-picker__carousel",
                   actionButton(
                     "pedigree_tpl_prev",
                     label = HTML('<i class="bi bi-arrow-left" aria-hidden="true"></i>'),
                     class = "structure-picker__arrow"
                   ),
                   div(
                     class = "structure-picker__preview",
                     plotOutput("pedigree_tpl_preview", height = "180px"),
                     div(
                       class = "structure-picker__caption",
                       textOutput("pedigree_tpl_label", inline = TRUE)
                     )
                   ),
                   actionButton(
                     "pedigree_tpl_next",
                     label = HTML('<i class="bi bi-arrow-right" aria-hidden="true"></i>'),
                     class = "structure-picker__arrow"
                   )
                 ),
                 div(
                   class = "structure-picker__actions",
                   actionButton(
                     "pedigree_tpl_confirm",
                     "Select this pedigree",
                     class = "btn btn-primary structure-picker__btn"
                   )
                 )
               ),
               div(
                 class = "structure-card display-settings display-settings--compact",
                 div(
                   class = "display-settings__header",
                   tags$i(class = "bi bi-sliders2 display-settings__icon", `aria-hidden` = "true"),
                   tags$span(class = "display-settings__title", "Display Settings"),
                   actionButton(
                     "plot_settings_reset",
                     HTML('<i class="bi bi-arrow-counterclockwise" aria-hidden="true"></i>'),
                     class = "btn display-settings__reset display-settings__reset--icon",
                     title = "Default Settings"
                   )
                 ),
                 div(
                   class = "display-settings__row",
                   div(
                     class = "display-settings__field",
                     tags$label(`for` = "plot_text_size", "Text (cex)"),
                     numericInput("plot_text_size", label = NULL, value = 1, min = 0, step = 0.1)
                   ),
                   div(
                     class = "display-settings__field",
                     tags$label(`for` = "plot_symbol_size", "Symbol"),
                     numericInput("plot_symbol_size", label = NULL, value = 1, min = 0, step = 0.1)
                   ),
                   div(
                     class = "display-settings__field",
                     tags$label(`for` = "plot_margins", "Margins"),
                     numericInput("plot_margins", label = NULL, value = 5, min = 0, step = 1)
                   )
                 )
               )
             )
           ),
           legend = tagList(
             div(
               class = "section-page",
               section_intro(
                 "bi-sliders",
                 "Legend / Options",
                 "Symbols, status markers and display conventions for the pedigree."
               ),
               legend_panel_tabs(),
               switch(legend_panel(),
                      "actions" = legend_actions_pane(),
                      "phenotypes" = legend_phenotypes_pane(),
                      legend_conventions_pane()
               )
             )
           ),
           personal = tagList(
             div(
               class = "section-page",
               section_intro(
                 "bi-person",
                 "Personal",
                 "Edit identity, annotations and relationship tools for the selected person."
               ),
               personal_info_pane()
             )
           ),
           family = tagList(
             div(
               class = "section-page",
               section_intro(
                 "bi-diagram-3",
                 "Family",
                 "View family connections and add relatives to the selected individual."
               ),
               personal_family_pane()
             )
           ),
           relationship = tagList(
             div(
               class = "section-page",
               section_intro(
                 "bi-arrow-left-right",
                 "Relationship",
                 "Analyze genetic relationships and kinship between individuals."
               ),
               personal_relationship_pane()
             )
           ),
           link = tagList(
             div(
               class = "section-page",
               section_intro(
                 "bi-link-45deg",
                 "Link",
                 "Compare two individuals and inspect their family relationship and genetic connection."
               ),
               selectInput(
                 "person_a",
                 "Individual A",
                 choices = c("II-2", "I-1", "I-2")
               ),
               selectInput(
                 "person_b",
                 "Individual B",
                 choices = c("II-2", "I-1", "I-2")
               ),
               actionButton(
                 "analyze",
                 "Analyze Link",
                 class = "btn btn-primary"
               ),
               textInput("relation", "Relation (text)"),
               textInput("ibd", "IBD (kappa) / Delta & Phi degree")
             )
           )
    )
  })
  
  shinyjs::onclick("btn_privacy_mode", {
    display_classic_mode(!display_classic_mode())
  })
  
  shinyjs::onclick("btn2", {
    display_stats_mode(!display_stats_mode())
  })
  observeEvent(input$btn_information, {
    information_menu_open(!isTRUE(information_menu_open()))
    plot_settings_menu_open(FALSE)
  }, ignoreInit = TRUE)
  observeEvent(input$btn_plot_settings, {
    plot_settings_menu_open(!isTRUE(plot_settings_menu_open()))
    information_menu_open(FALSE)
  }, ignoreInit = TRUE)
  observeEvent(input$close_information_menu, {
    information_menu_open(FALSE)
  }, ignoreInit = TRUE)
  observeEvent(input$close_plot_settings_menu, {
    plot_settings_menu_open(FALSE)
  }, ignoreInit = TRUE)
  shinyjs::onclick("toggle_pedigree_choices", {
    pedigree_choices_mode(!pedigree_choices_mode())
  })
  
  output$informationMenu <- renderUI({
    req(isTRUE(information_menu_open()))
    div(
      class = "top-popover",
      div(
        class = "top-popover__header",
        div(
          class = "top-popover__header-main",
          span(class = "material-symbols-outlined", "info"),
          span(class = "top-popover__title", "Application help")
        ),
        tags$button(
          type = "button",
          class = "top-popover__close",
          onclick = "Shiny.setInputValue('close_information_menu', Date.now(), {priority:'event'});",
          span(class = "material-symbols-outlined", "close")
        )
      ),
      div(
        class = "top-popover__body",
        tags$ul(
          class = "help-list",
          tags$li(
            span(class = "material-symbols-outlined", "touch_app"),
            div(tags$strong("Select"), "Click an individual to edit identity and status.")
          ),
          tags$li(
            span(class = "material-symbols-outlined", "open_in_new"),
            div(tags$strong("Double-click"), "Open a draggable individual workspace.")
          ),
          tags$li(
            span(class = "material-symbols-outlined", "mouse"),
            div(tags$strong("Right-click"), "Open actions: annotations, comments, reorder siblings, relationship calculation and delete.")
          ),
          tags$li(
            span(class = "material-symbols-outlined", "lock_person"),
            div(tags$strong("Privacy Mode"), "Hide personal information while keeping hover access.")
          ),
          tags$li(
            span(class = "material-symbols-outlined", "monitoring"),
            div(tags$strong("Genetic Stats"), "Show inbreeding, degree and relatedness metrics.")
          ),
          tags$li(
            span(class = "material-symbols-outlined", "palette"),
            div(tags$strong("Phenotypes"), "Create visual styles and apply them to selected individuals.")
          )
        )
      )
    )
  })
  
  output$plotSettingsMenu <- renderUI({
    req(isTRUE(plot_settings_menu_open()))
    div(
      class = "top-popover top-popover--settings",
      div(
        class = "top-popover__header",
        div(
          class = "top-popover__header-main",
          span(class = "material-symbols-outlined", "tune"),
          span(class = "top-popover__title", "Plot settings")
        ),
        tags$button(
          type = "button",
          class = "top-popover__close",
          onclick = "Shiny.setInputValue('close_plot_settings_menu', Date.now(), {priority:'event'});",
          span(class = "material-symbols-outlined", "close")
        )
      ),
      div(
        class = "top-popover__body",
        div(
          class = "settings-popover__grid",
          numericInput("plot_text_size", "Text size", value = plot_settings$cex, min = 0.3, max = 3, step = 0.1),
          numericInput("plot_symbol_size", "Symbol size", value = plot_settings$symbolsize, min = 0.3, max = 3, step = 0.1),
          numericInput("plot_margins", "Margins", value = plot_settings$mar, min = 0, max = 12, step = 1)
        ),
        div(
          class = "settings-popover__actions",
          actionButton(
            "plot_settings_reset",
            tagList(span(class = "material-symbols-outlined", "restart_alt"), "Reset"),
            class = "btn btn-default"
          )
        )
      )
    )
  })
  
  observe({
    if (display_classic_mode()) {
      shinyjs::removeClass("privacy_button_wrapper", "active")
      shinyjs::runjs("$('#privacy_toggle_status').text('OFF');")
    } else {
      shinyjs::addClass("privacy_button_wrapper", "active")
      shinyjs::runjs("$('#privacy_toggle_status').text('ON');")
    }
  })
  
  observe({
    if (display_stats_mode()) {
      shinyjs::addClass("btn2_wrapper", "active")
      shinyjs::runjs("$('#btn2_toggle_status').text('ON');")
    } else {
      shinyjs::removeClass("btn2_wrapper", "active")
      shinyjs::runjs("$('#btn2_toggle_status').text('OFF');")
    }
  })
  observe({
    if (pedigree_choices_mode()) {
      shinyjs::addClass("structure_choices_wrapper", "active")
      shinyjs::runjs("$('#structure_choices_status').text('ON');")
    } else {
      shinyjs::removeClass("structure_choices_wrapper", "active")
      shinyjs::runjs("$('#structure_choices_status').text('OFF');")
    }
  })
  output$individual_panel <- renderUI({
    s <- sel()
    p <- ped()
    pd <- values$pedData
    
    if (length(s) == 0 || is.null(p) || is.null(pd)) {
      return(div(
        class = "box ind-panel",
        tags$h5("Individual"),
        div(
          class = "ind-panel__empty",
          "Click on an individual to view details"
        )
      ))
    }
    
    # Privacy mode: hide individual details
    if (!display_classic_mode()) {
      return(div(
        class = "box ind-panel",
        tags$h5("Individual"),
        div(
          class = "ind-panel__empty",
          HTML(
            '<span class="material-symbols-outlined" style="font-size:20px; color:#a1a1aa;">lock_person</span>'
          ),
          tags$p(
            style = "margin:8px 0 0; font-size:11px; color:#a1a1aa;",
            "Privacy mode is active. Individual details are hidden."
          )
        )
      ))
    }
    
    id <- s[1]
    row <- pd[pd$id == id, , drop = FALSE]
    if (nrow(row) == 0) {
      return(NULL)
    }
    
    sex_val <- row$sex[1]
    sex_class <- switch(as.character(sex_val),
                        "1" = "male",
                        "2" = "female",
                        "unknown"
    )
    sex_label <- switch(as.character(sex_val),
                        "1" = "Male",
                        "2" = "Female",
                        "Unknown"
    )
    sex_icon <- switch(as.character(sex_val),
                       "1" = "\u2642",
                       "2" = "\u2640",
                       "?"
    )
    is_dead <- isTRUE(id %in% deceased_ids())
    is_misc <- id %in% miscarriage()
    
    # Delete button (shared by all individual types)
    delete_btn <- tags$button(
      class = "ind-panel__delete-btn",
      title = "Delete individual",
      onclick = sprintf(
        "if(confirm('Delete %s from the pedigree?'))Shiny.setInputValue('btn_delete_selected',{id:'%s',nonce:Math.random()},{priority:'event'});",
        id,
        id
      ),
      HTML(
        '<span class="material-symbols-outlined" style="font-size:18px;">delete_outline</span>'
      )
    )
    
    # Miscarriage: simplified panel (ID + status + delete)
    if (is_misc) {
      return(div(
        class = "box ind-panel",
        tags$h5("Individual"),
        div(
          class = "ind-panel__header",
          div(
            class = "ind-panel__avatar",
            style = "background:#E65100; color:white;",
            "\u25B3"
          ),
          div(
            span(class = "ind-panel__id", id)
          ),
          span(
            class = "ind-panel__status",
            style = "color:#E65100;",
            "Miscarriage"
          ),
          delete_btn
        )
      ))
    }
    
    first_name <- row$first_name[1] %||% ""
    last_name <- row$last_name[1] %||% ""
    dob <- row$date_of_birth[1] %||% ""
    dod <- row$date_of_death[1] %||% ""
    age <- row$age[1] %||% ""
    age_unit <- if ("age_unit" %in% names(row)) {
      (row$age_unit[1] %||% "years")
    } else {
      "years"
    }
    
    # JS helper for firing change events
    js_change <- function(field, val_expr) {
      sprintf(
        "Shiny.setInputValue('ind_field_change',{id:'%s',field:'%s',value:%s,nonce:Math.random()},{priority:'event'});",
        id,
        field,
        val_expr
      )
    }
    
    # ── Genetics section (conditional on stats mode) ──
    # ── Genetics section (conditional on stats mode) ──
    genetics_section <- NULL
    if (isTRUE(display_stats_mode())) {
      all_ids <- labels(p)
      
      f_vals <- tryCatch(
        ribd::inbreeding(p, ids = all_ids, Xchrom = FALSE),
        error = function(e) NULL
      )
      if (!is.null(f_vals)) {
        names(f_vals) <- all_ids
      }
      
      gs <- compute_selected_stats(
        ped = p,
        target_id = id,
        selected_id = sel(),
        f_vals = f_vals
      )
      
      genetics_rows <- list(
        div(
          class = "ind-panel__stat-row",
          span(class = "ind-panel__stat-dot", style = "background:#8E24AA;"),
          span(class = "ind-panel__stat-label", "f"),
          span(class = "ind-panel__stat-value", format_stat_value(gs$f))
        )
      )
      
      if (length(sel()) > 0 && nzchar(sel()[1])) {
        genetics_rows <- c(
          genetics_rows,
          list(
            div(
              class = "ind-panel__stat-row",
              span(class = "ind-panel__stat-dot", style = "background:#00897B;"),
              span(class = "ind-panel__stat-label", "deg"),
              span(class = "ind-panel__stat-value", if (is.na(gs$deg)) "\u2014" else as.character(gs$deg))
            ),
            div(
              class = "ind-panel__stat-row",
              span(class = "ind-panel__stat-dot", style = "background:#E65100;"),
              span(class = "ind-panel__stat-label", "%R"),
              span(class = "ind-panel__stat-value", format_stat_value(gs$r_pct))
            )
          )
        )
      }
      
      genetics_section <- div(
        class = "ind-panel__genetics",
        div(class = "ind-panel__genetics-title", "Genetics"),
        genetics_rows
      )
    }
    
    #   genetics_section <- div(
    #     class = "ind-panel__genetics",
    #     div(class = "ind-panel__genetics-title", "Genetics"),
    #     div(
    #       class = "ind-panel__stat-row",
    #       span(class = "ind-panel__stat-dot", style = "background:#8E24AA;"),
    #       span(class = "ind-panel__stat-label", "f"),
    #       span(class = "ind-panel__stat-value", f_display)
    #     )
    #   )
    # }
    
    tagList(
      div(
        class = "box ind-panel",
        tags$h5("Individual"),
        # Header: avatar + ID + sex + status + delete
        div(
          class = "ind-panel__header",
          div(class = paste("ind-panel__avatar", sex_class), sex_icon),
          div(
            span(class = "ind-panel__id", id),
            tags$br(),
            span(class = "ind-panel__sex", sex_label)
          ),
          span(
            class = paste(
              "ind-panel__status",
              if (is_dead) "deceased" else "alive"
            ),
            if (is_dead) "Deceased" else "Alive"
          ),
          delete_btn
        ),
        # Form fields
        div(
          class = "ind-panel__form",
          # Last name
          div(
            class = "ind-panel__row",
            tags$input(
              type = "text",
              class = "ind-panel__input",
              value = last_name,
              placeholder = "Last name",
              onchange = js_change("last_name", "this.value")
            )
          ),
          # First name
          div(
            class = "ind-panel__row",
            tags$input(
              type = "text",
              class = "ind-panel__input",
              value = first_name,
              placeholder = "First name",
              onchange = js_change("first_name", "this.value")
            )
          ),
          # Date of birth
          div(
            class = "ind-panel__row",
            tags$label("DOB"),
            tags$input(
              type = "date",
              class = "ind-panel__input",
              value = if (nzchar(dob)) {
                tryCatch(
                  format(as.Date(dob, "%d-%m-%Y"), "%Y-%m-%d"),
                  error = function(e) ""
                )
              } else {
                ""
              },
              onchange = js_change(
                "date_of_birth",
                "(function(v){if(!v)return '';var p=v.split('-');return p[2]+'-'+p[1]+'-'+p[0];})(this.value)"
              )
            )
          ),
          # Age (number + unit selector)
          div(
            class = "ind-panel__row",
            tags$label("Age"),
            div(
              class = "ind-panel__age-group",
              tags$input(
                type = "number",
                class = "ind-panel__age-num",
                value = if (nzchar(age)) age else "",
                placeholder = "0",
                min = "0",
                onchange = js_change("age", "this.value")
              ),
              tags$select(
                class = "ind-panel__unit-select",
                onchange = js_change("age_unit", "this.value"),
                tags$option(
                  value = "years",
                  selected = if (age_unit == "years") NA else NULL,
                  "Years"
                ),
                tags$option(
                  value = "months",
                  selected = if (age_unit == "months") NA else NULL,
                  "Months"
                ),
                tags$option(
                  value = "weeks",
                  selected = if (age_unit == "weeks") NA else NULL,
                  "Weeks"
                ),
                tags$option(
                  value = "days",
                  selected = if (age_unit == "days") NA else NULL,
                  "Days"
                )
              )
            )
          ),
          # Deceased toggle
          div(
            class = "ind-panel__toggle-row",
            tags$label(style = "font-size:11px; color:#64748b;", "Deceased"),
            tags$button(
              class = paste("ind-panel__toggle", if (is_dead) "active" else ""),
              onclick = js_change("deceased", if (is_dead) "false" else "true")
            )
          ),
          # Date of death (visible only if deceased)
          if (is_dead) {
            div(
              class = "ind-panel__row",
              tags$label("DOD"),
              tags$input(
                type = "date",
                class = "ind-panel__input",
                value = if (nzchar(dod)) {
                  tryCatch(
                    format(as.Date(dod, "%d-%m-%Y"), "%Y-%m-%d"),
                    error = function(e) ""
                  )
                } else {
                  ""
                },
                onchange = js_change(
                  "date_of_death",
                  "(function(v){if(!v)return '';var p=v.split('-');return p[2]+'-'+p[1]+'-'+p[0];})(this.value)"
                )
              )
            )
          } else {
            NULL
          }
        ),
        # Genetics section (only visible when stats mode is ON)
        genetics_section
      )
    ) # end tagList
  })
  
  # ── Family panel (parents, siblings, spouses, children) ──
  output$family_panel <- renderUI({
    return(NULL)
    
    s <- sel()
    p <- ped()
    if (length(s) == 0 || is.null(p)) {
      return(tags$details(
        class = "box ind-panel ind-panel--family-collapsible",
        open = NA,
        tags$summary(tags$h5("Family")),
        div(
          class = "ind-panel--family-body",
          div(
            class = "ind-panel__empty",
            "Select an individual to view family connections."
          )
        )
      ))
    }
    id <- s[1]
    all_ids <- labels(p)
    if (!(id %in% all_ids)) {
      return(NULL)
    }
    
    safe_get <- function(fn, ...) tryCatch(fn(p, ...), error = function(e) character(0))
    parent_ids <- safe_get(pedtools::parents, id)
    child_ids <- safe_get(pedtools::children, id)
    sibling_ids <- safe_get(pedtools::siblings, id)
    spouse_ids <- safe_get(pedtools::spouses, id)
    
    make_chip <- function(mid) {
      sex_val <- pedtools::getSex(p, mid)
      icon <- switch(as.character(sex_val),
                     "1" = "\u2642",
                     "2" = "\u2640",
                     "?"
      )
      cls <- switch(as.character(sex_val),
                    "1" = "male",
                    "2" = "female",
                    "unknown"
      )
      ids_json <- sprintf("['%s']", mid)
      tags$button(
        class = paste("ind-panel__family-chip", cls),
        onclick = sprintf(
          "window.flashPedigreeHighlight(%s, this);",
          ids_json
        ),
        onmousedown = sprintf(
          "window.flashPedigreeHighlight(%s, this, 900);",
          ids_json
        ),
        tags$span(class = "ind-panel__family-chip-icon", icon),
        tags$span(mid)
      )
    }
    
    make_section <- function(label, member_ids) {
      if (length(member_ids) == 0) {
        return(NULL)
      }
      ids_json <- paste0("['", paste(member_ids, collapse = "','"), "']")
      div(
        class = "ind-panel__family-section",
        tags$span(
          class = "ind-panel__family-label pedigree-data-card--btn",
          style = "cursor: pointer; display: inline-block; padding: 2px 6px; border-radius: 6px;",
          onclick = sprintf(
            "window.flashPedigreeHighlight(%s, this);",
            ids_json
          ),
          onmousedown = sprintf(
            "window.flashPedigreeHighlight(%s, this, 900);",
            ids_json
          ),
          label
        ),
        div(class = "ind-panel__family-chips", lapply(member_ids, make_chip))
      )
    }
    
    tags$details(
      class = "box ind-panel ind-panel--family-collapsible",
      open = NA,
      tags$summary(tags$h5("Family")),
      div(
        class = "ind-panel--family-body",
        div(
          class = "ind-panel__hint",
          tags$span(class = "material-symbols-outlined ind-panel__hint-icon", "account_tree"),
          tags$span(HTML("Click a family member to select them in the pedigree."))
        ),
        make_section("Parents", parent_ids),
        make_section("Siblings", sibling_ids),
        make_section("Spouses", spouse_ids),
        make_section("Children", child_ids),
        if (length(parent_ids) == 0 && length(sibling_ids) == 0 &&
            length(spouse_ids) == 0 && length(child_ids) == 0) {
          div(
            class = "ind-panel__empty",
            "No family connections found for this individual."
          )
        }
      )
    )
  })
  
  # ── Family chip click: select that individual ──
  observeEvent(input$family_chip_click, {
    req(input$family_chip_click, ped())
    clicked_id <- input$family_chip_click
    if (clicked_id %in% labels(ped())) {
      sel(clicked_id)
    }
  })
  
  # ── Observer: save individual field changes to pedData ──
  # Cross-calculation logic (age = number, age_unit = years/months/weeks/days):
  #   NOT deceased: DOB → age = today-DOB | age → DOB = today-age
  #   Deceased: DOB+DOD → age | DOB+age → DOD | DOD+age → DOB
  observeEvent(input$ind_field_change, {
    msg <- input$ind_field_change
    req(msg$id, msg$field, values$pedData)
    
    eid <- as.character(msg$id)
    field <- as.character(msg$field)
    value <- msg$value
    
    i <- which(values$pedData$id == eid)
    if (length(i) != 1) {
      return()
    }
    
    # Simple text fields
    if (field %in% c("last_name", "first_name")) {
      values$pedData[[field]][i] <- value %||% ""
      return()
    }
    
    is_dead <- isTRUE(eid %in% deceased_ids())
    unit <- values$pedData$age_unit[i] %||% "years"
    
    # ── Date of birth changed ──
    if (field == "date_of_birth") {
      values$pedData$date_of_birth[i] <- value %||% ""
      dob <- values$pedData$date_of_birth[i]
      dod <- values$pedData$date_of_death[i] %||% ""
      age_val <- values$pedData$age[i] %||% ""
      
      if (nzchar(dob)) {
        if (is_dead && nzchar(dod)) {
          res <- calculate_age(dob, dod, "years")
          values$pedData$age[i] <- res$value
          values$pedData$age_unit[i] <- res$unit
        } else if (is_dead && nzchar(age_val)) {
          values$pedData$date_of_death[i] <- date_from_age(
            dob,
            age_val,
            unit,
            "forward"
          )
        } else {
          res <- calculate_age(dob, "", "years")
          values$pedData$age[i] <- res$value
          values$pedData$age_unit[i] <- res$unit
        }
      }
      return()
    }
    
    # ── Date of death changed ──
    if (field == "date_of_death") {
      values$pedData$date_of_death[i] <- value %||% ""
      dob <- values$pedData$date_of_birth[i] %||% ""
      dod <- values$pedData$date_of_death[i]
      age_val <- values$pedData$age[i] %||% ""
      
      if (nzchar(dod)) {
        if (nzchar(dob)) {
          res <- calculate_age(dob, dod, "years")
          values$pedData$age[i] <- res$value
          values$pedData$age_unit[i] <- res$unit
        } else if (nzchar(age_val)) {
          values$pedData$date_of_birth[i] <- date_from_age(
            dod,
            age_val,
            unit,
            "backward"
          )
        }
      }
      return()
    }
    
    # ── Age value changed ──
    if (field == "age") {
      values$pedData$age[i] <- value %||% ""
      age_val <- values$pedData$age[i]
      dob <- values$pedData$date_of_birth[i] %||% ""
      dod <- values$pedData$date_of_death[i] %||% ""
      
      if (nzchar(age_val) && !is.na(parse_age_number(age_val))) {
        if (!is_dead) {
          values$pedData$date_of_birth[i] <- date_from_age(
            NULL,
            age_val,
            unit,
            "backward"
          )
        } else {
          if (nzchar(dob) && !nzchar(dod)) {
            values$pedData$date_of_death[i] <- date_from_age(
              dob,
              age_val,
              unit,
              "forward"
            )
          } else if (nzchar(dod) && !nzchar(dob)) {
            values$pedData$date_of_birth[i] <- date_from_age(
              dod,
              age_val,
              unit,
              "backward"
            )
          } else if (nzchar(dob) && nzchar(dod)) {
            values$pedData$date_of_death[i] <- date_from_age(
              dob,
              age_val,
              unit,
              "forward"
            )
          }
        }
      }
      return()
    }
    
    # ── Age unit changed → recalculate age from dates if available ──
    if (field == "age_unit") {
      new_unit <- value %||% "years"
      values$pedData$age_unit[i] <- new_unit
      dob <- values$pedData$date_of_birth[i] %||% ""
      dod <- values$pedData$date_of_death[i] %||% ""
      
      if (nzchar(dob)) {
        ref_dod <- if (is_dead && nzchar(dod)) dod else ""
        res <- calculate_age(dob, ref_dod, new_unit)
        values$pedData$age[i] <- res$value
        values$pedData$age_unit[i] <- res$unit
      }
      return()
    }
    
    # ── Deceased toggle ──
    if (field == "deceased") {
      dead <- identical(value, TRUE) || identical(value, "true")
      values$pedData$deceased[i] <- dead
      if (dead) {
        deceased_ids(union(deceased_ids(), eid))
      } else {
        deceased_ids(setdiff(deceased_ids(), eid))
        values$pedData$date_of_death[i] <- ""
      }
      dob <- values$pedData$date_of_birth[i] %||% ""
      dod <- if (dead) values$pedData$date_of_death[i] %||% "" else ""
      if (nzchar(dob)) {
        res <- calculate_age(dob, dod, "years")
        values$pedData$age[i] <- res$value
        values$pedData$age_unit[i] <- res$unit
      }
      return()
    }
  })
  # ── Hover tooltip ──
  hover_id <- reactive({
    req(input$pedHover, ped(), ctrs())
    idx <- nearest_click(ctrs(), input$pedHover, thresh = 25)
    if (is.na(idx)) {
      return(NULL)
    }
    labels(ped())[idx]
  })
  
  output$tooltip <- renderUI({
    h <- input$pedHover
    id <- hover_id()
    if (is.null(h) || is.null(id)) {
      return(NULL)
    }
    
    style <- sprintf(
      "left:%gpx; top:%gpx; transform:translate(12px,12px);",
      h$coords_css$x,
      h$coords_css$y
    )
    
    # Privacy mode: detailed tooltip with build_hover_html
    if (!display_classic_mode()) {
      html_content <- build_hover_html(
        id,
        values$pedData,
        adopted_ids_vec = adopted_ids(),
        deceased_ids_vec = deceased_ids()
      )
      return(tags$div(
        class = "hover-tooltip",
        style = style,
        HTML(html_content)
      ))
    }
    
    # Classic mode: simple tooltip
    sex_code <- pedtools::getSex(ped(), id)
    sex <- switch(as.character(sex_code),
                  "1" = "Male",
                  "2" = "Female",
                  "Unknown"
    )
    
    # Collect markers
    tags_list <- character(0)
    if (id %in% deceased_ids()) {
      tags_list <- c(tags_list, "Deceased")
    }
    if (id %in% adopted_ids()) {
      tags_list <- c(tags_list, "Adopted")
    }
    if (id %in% miscarriage()) {
      tags_list <- c(tags_list, "Miscarriage")
    }
    if (id %in% starred_ids()) {
      tags_list <- c(tags_list, "Starred")
    }
    if (id %in% proband_id()) {
      tags_list <- c(tags_list, "Proband")
    }
    if (id %in% infertility_ids()) {
      tags_list <- c(tags_list, "Infertile")
    }
    if (id %in% afab_ids()) {
      tags_list <- c(tags_list, "AFAB")
    }
    if (id %in% amab_ids()) {
      tags_list <- c(tags_list, "AMAB")
    }
    if (id %in% umab_ids()) {
      tags_list <- c(tags_list, "UMAB")
    }
    
    # Phenotypes
    for (nm in names(phenotypes$assign)) {
      if (id %in% (phenotypes$assign[[nm]] %||% character(0))) {
        tags_list <- c(tags_list, nm)
      }
    }
    
    extra <- if (length(tags_list) > 0) {
      paste0("<br>", paste(tags_list, collapse = ", "))
    } else {
      ""
    }
    
    tags$div(
      class = "ttip",
      style = style,
      HTML(paste0("<b>", id, "</b> (", sex, ")", extra))
    )
  })
  
  # ════════════════════════════════════════════════════════
  # Annotation modal
  # ════════════════════════════════════════════════════════
  
  showAnnotationModal <- function(id, position) {
    existing_text <- text_annotations[[position]][[id]] %||% ""
    position_labels <- list(
      top = "Top \u2191",
      bottom = "Bottom \u2193",
      left = "Left \u2190",
      right = "Right \u2192",
      topleft = "Top-Left \u2196",
      topright = "Top-Right \u2197",
      bottomleft = "Bottom-Left \u2199",
      bottomright = "Bottom-Right \u2198",
      inside = "Inside \u2299"
    )
    
    showModal(modalDialog(
      title = tags$div(
        style = "display: flex; align-items: center; gap: 12px; font-size: 18px; font-weight: 600;",
        icon("pen", style = "color: #3498db;"),
        sprintf("Add Annotation \u2014 %s", position_labels[[position]])
      ),
      tags$p(
        sprintf("Individual: %s", id),
        style = "color: #64748b; margin-bottom: 16px;"
      ),
      textAreaInput(
        "annot_text",
        label = "Annotation text:",
        value = existing_text,
        rows = 3,
        placeholder = "Enter text to display..."
      ),
      tags$hr(style = "margin: 12px 0;"),
      tags$label(
        "Style",
        style = "font-weight: 600; margin-bottom: 8px; display: block;"
      ),
      fluidRow(
        column(
          4,
          colourInput(
            "annot_col",
            "Color",
            value = annot_style$col,
            showColour = "background",
            closeOnClick = TRUE
          )
        ),
        column(
          4,
          numericInput(
            "annot_cex",
            "Size",
            value = annot_style$cex,
            min = 0.5,
            max = 3.0,
            step = 0.1
          )
        ),
        column(
          4,
          selectInput(
            "annot_font",
            "Font",
            choices = c(
              "Normal" = 1,
              "Bold" = 2,
              "Italic" = 3,
              "Bold Italic" = 4
            ),
            selected = annot_style$font
          )
        )
      ),
      tags$div(
        style = "font-size: 11px; color: #94a3b8; margin-top: 8px;",
        "Leave empty to remove the annotation"
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "save_annotation",
          HTML('<i class="fa fa-save"></i> Save'),
          class = "btn btn-primary",
          onclick = sprintf(
            'Shiny.setInputValue("annotation_data", {id: "%s", position: "%s"}, {priority: "event"})',
            id,
            position
          )
        )
      ),
      size = "m",
      easyClose = FALSE
    ))
  }
  
  # ════════════════════════════════════════════════════════
  # Context menu (right-click)
  # ════════════════════════════════════════════════════════
  
  observeEvent(input$ped_context, {
    target_id <- hover_id()
    if (is.null(target_id) && !is.null(input$ped_context) && !is.null(ctrs())) {
      ctx <- input$ped_context
      centers <- ctrs()
      if (is.data.frame(centers) && nrow(centers) > 0) {
        dx <- centers$x - ctx$x
        dy <- centers$y - ctx$y
        nearest <- which.min(dx^2 + dy^2)
        if (length(nearest) == 1 && is.finite(dx[nearest]) && is.finite(dy[nearest])) {
          pixel_dist <- sqrt(dx[nearest]^2 + dy[nearest]^2)
          if (pixel_dist <= 35) {
            target_id <- labels(ped())[centers$id_plot[nearest]]
            sel(target_id)
          }
        }
      }
    }
    if (is.null(target_id) && length(sel()) > 0 && nzchar(sel()[1])) {
      target_id <- sel()[1]
    }
    ctx_id(target_id)
  })
  
  observeEvent(input$close_context_menu, {
    ctx_id(NULL)
  })
  
  output$contextMenu <- renderUI({
    ctx <- input$ped_context
    id <- ctx_id()
    if (is.null(ctx) || is.null(id)) {
      return(NULL)
    }
    
    style <- sprintf("left:%gpx; top:%gpx;", ctx$x, ctx$y)
    
    comment_svg <- '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><line x1="12" y1="5" x2="12" y2="19"/><line x1="5" y1="12" x2="19" y2="12"/></svg>'
    
    tags$div(
      class = "context-menu",
      style = style,
      
      # Quick row: Proband, Carrier, Starred
      tags$div(
        class = "quick-row",
        tags$button(
          class = "quick-btn",
          onclick = "Shiny.setInputValue('ctx_proband', Math.random(), {priority:'event'})",
          tags$span(
            class = "material-symbols-outlined",
            style = "font-size:16px;",
            "arrow_outward"
          ),
          tags$span("Proband")
        ),
        tags$button(
          class = "quick-btn",
          onclick = "Shiny.setInputValue('ctx_carrier', Math.random(), {priority:'event'})",
          tags$span(
            class = "material-symbols-outlined",
            style = "font-size:16px;",
            "filter_tilt_shift"
          ),
          tags$span("Carrier")
        ),
        tags$button(
          class = "quick-btn",
          onclick = "Shiny.setInputValue('ctx_starred', Math.random(), {priority:'event'})",
          tags$span(
            class = "material-symbols-outlined",
            style = "font-size:16px;",
            "asterisk"
          ),
          tags$span("Starred")
        )
      ),
      tags$div(class = "menu-sep"),
      
      # Add Annotation (submenu with position grid)
      tags$div(
        class = "menu-item has-submenu",
        HTML(
          '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"/><path d="M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z"/></svg>'
        ),
        tags$span("Add Annotation"),
        tags$span(class = "arrow", HTML("\u203A")),
        tags$div(
          class = "submenu",
          tags$div(class = "submenu-title", "POSITION"),
          tags$div(
            class = "pos-grid",
            tags$button(
              class = "pos-btn",
              onclick = "Shiny.setInputValue('ctx_annot_topleft', Math.random(), {priority:'event'})",
              HTML("\u2196")
            ),
            tags$button(
              class = "pos-btn",
              onclick = "Shiny.setInputValue('ctx_annot_top', Math.random(), {priority:'event'})",
              HTML("\u2191")
            ),
            tags$button(
              class = "pos-btn",
              onclick = "Shiny.setInputValue('ctx_annot_topright', Math.random(), {priority:'event'})",
              HTML("\u2197")
            ),
            tags$button(
              class = "pos-btn",
              onclick = "Shiny.setInputValue('ctx_annot_left', Math.random(), {priority:'event'})",
              HTML("\u2190")
            ),
            tags$button(
              class = "pos-btn center",
              onclick = "Shiny.setInputValue('ctx_annot_inside', Math.random(), {priority:'event'})",
              HTML("\u25CB")
            ),
            tags$button(
              class = "pos-btn",
              onclick = "Shiny.setInputValue('ctx_annot_right', Math.random(), {priority:'event'})",
              HTML("\u2192")
            ),
            tags$button(
              class = "pos-btn",
              onclick = "Shiny.setInputValue('ctx_annot_bottomleft', Math.random(), {priority:'event'})",
              HTML("\u2199")
            ),
            tags$button(
              class = "pos-btn",
              onclick = "Shiny.setInputValue('ctx_annot_bottom', Math.random(), {priority:'event'})",
              HTML("\u2193")
            ),
            tags$button(
              class = "pos-btn",
              onclick = "Shiny.setInputValue('ctx_annot_bottomright', Math.random(), {priority:'event'})",
              HTML("\u2198")
            )
          )
        )
      ),
      tags$div(class = "menu-sep"),
      tags$div(class = "menu-section", "OPTIONS"),
      
      # Add Comment
      tags$div(
        class = "menu-item",
        onclick = "Shiny.setInputValue('ctx_comment', Math.random(), {priority:'event'})",
        HTML(comment_svg),
        tags$span("Add Comment")
      ),
      
      # Reorder Siblings
      tags$div(
        class = "menu-item",
        onclick = "Shiny.setInputValue('ctx_reorder', Math.random(), {priority:'event'})",
        HTML('<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="17 1 21 5 17 9"/><path d="M3 11V9a4 4 0 0 1 4-4h14"/><polyline points="7 23 3 19 7 15"/><path d="M21 13v2a4 4 0 0 1-4 4H3"/></svg>'),
        tags$span("Reorder Siblings")
      ),
      tags$div(
        class = "menu-item",
        onclick = "Shiny.setInputValue('ctx_relationship', Math.random(), {priority:'event'})",
        tags$span(
          class = "material-symbols-outlined",
          style = "font-size:15px;",
          "swap_horiz"
        ),
        tags$span("Relationship Calculation")
      ),
      tags$div(class = "thick-sep"),
      
      # Delete
      tags$div(
        class = "menu-item del",
        onclick = "Shiny.setInputValue('ctx_delete', Math.random(), {priority:'event'})",
        tags$span(
          class = "material-symbols-outlined",
          style = "font-size:14px; color:#e53935;",
          "person_off"
        ),
        tags$span("Delete"),
        tags$span(class = "shortcut", HTML("&#x232B;"))
      )
    )
  })
  
  # ── Context: Proband ──
  observeEvent(input$ctx_proband, {
    req(ctx_id(), ped())
    id <- ctx_id()
    if (!(id %in% labels(ped()))) {
      ctx_id(NULL)
      return()
    }
    save_hist()
    current <- proband_id()
    if (length(current) > 0 && id == current[1]) {
      proband_id(character(0))
      showNotification(sprintf("Proband removed from %s", id), type = "message")
    } else {
      if (length(current) > 0) {
        showNotification(
          sprintf("Proband moved from %s to %s", current[1], id),
          type = "message",
          duration = 4
        )
      } else {
        showNotification(sprintf("%s marked as proband", id), type = "message")
      }
      proband_id(id)
    }
    ctx_id(NULL)
  })
  
  # ── Context: Carrier ──
  observeEvent(input$ctx_carrier, {
    req(ctx_id(), ped())
    id <- ctx_id()
    if (!(id %in% labels(ped()))) {
      ctx_id(NULL)
      return()
    }
    save_hist()
    if (id %in% carrier_ids()) {
      carrier_ids(setdiff(carrier_ids(), id))
      showNotification(sprintf("Carrier removed from %s", id), type = "message")
    } else {
      carrier_ids(c(carrier_ids(), id))
      showNotification(sprintf("Carrier applied to %s", id), type = "message")
    }
    ctx_id(NULL)
  })
  
  # ── Context: Starred ──
  observeEvent(input$ctx_starred, {
    req(ctx_id(), ped())
    id <- ctx_id()
    if (!(id %in% labels(ped()))) {
      ctx_id(NULL)
      return()
    }
    save_hist()
    if (id %in% starred_ids()) {
      starred_ids(setdiff(starred_ids(), id))
      showNotification(sprintf("Star removed from %s", id), type = "message")
    } else {
      starred_ids(c(starred_ids(), id))
      showNotification(sprintf("%s marked as starred", id), type = "message")
    }
    ctx_id(NULL)
  })
  
  # ── Context: Add Comment ──
  observeEvent(input$ctx_comment, {
    req(ctx_id(), ped(), values$pedData)
    id <- ctx_id()
    if (!(id %in% labels(ped()))) {
      ctx_id(NULL)
      return()
    }
    row <- values$pedData[values$pedData$id == id, , drop = FALSE]
    existing_comment <- if (nrow(row) > 0) row$comments[1] %||% "" else ""
    
    showModal(modalDialog(
      title = tags$div(
        style = "display: flex; align-items: center; gap: 12px;",
        tags$i(
          class = "fa fa-comment",
          style = "color: #007aff; font-size: 18px;"
        ),
        sprintf("Add Comment \u2014 %s", id)
      ),
      size = "m",
      easyClose = TRUE,
      textAreaInput(
        "comment_text",
        label = "Comment:",
        value = existing_comment,
        rows = 4,
        placeholder = "Enter a comment for this individual..."
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "save_comment",
          tagList(tags$i(class = "fa fa-save"), " Save Comment"),
          class = "btn btn-primary",
          onclick = sprintf(
            'Shiny.setInputValue("comment_target_id", "%s", {priority: "event"})',
            id
          )
        )
      )
    ))
    ctx_id(NULL)
  })
  
  # ── Save Comment ──
  observeEvent(input$save_comment, {
    req(input$comment_target_id, values$pedData)
    id <- input$comment_target_id
    comment_text <- input$comment_text %||% ""
    idx <- which(values$pedData$id == id)
    if (length(idx) > 0) {
      values$pedData$comments[idx] <- trimws(comment_text)
      if (nzchar(trimws(comment_text))) {
        showNotification(
          sprintf("Comment added for %s", id),
          type = "message",
          duration = 3
        )
      } else {
        showNotification(
          sprintf("Comment removed for %s", id),
          type = "message",
          duration = 3
        )
      }
    }
    removeModal()
  })
  
  # ── Context: Annotation position observers ──
  observeEvent(input$ctx_annot_top, {
    req(ctx_id())
    showAnnotationModal(ctx_id(), "top")
    ctx_id(NULL)
  })
  observeEvent(input$ctx_annot_bottom, {
    req(ctx_id())
    showAnnotationModal(ctx_id(), "bottom")
    ctx_id(NULL)
  })
  observeEvent(input$ctx_annot_left, {
    req(ctx_id())
    showAnnotationModal(ctx_id(), "left")
    ctx_id(NULL)
  })
  observeEvent(input$ctx_annot_right, {
    req(ctx_id())
    showAnnotationModal(ctx_id(), "right")
    ctx_id(NULL)
  })
  observeEvent(input$ctx_annot_topleft, {
    req(ctx_id())
    showAnnotationModal(ctx_id(), "topleft")
    ctx_id(NULL)
  })
  observeEvent(input$ctx_annot_topright, {
    req(ctx_id())
    showAnnotationModal(ctx_id(), "topright")
    ctx_id(NULL)
  })
  observeEvent(input$ctx_annot_bottomleft, {
    req(ctx_id())
    showAnnotationModal(ctx_id(), "bottomleft")
    ctx_id(NULL)
  })
  observeEvent(input$ctx_annot_bottomright, {
    req(ctx_id())
    showAnnotationModal(ctx_id(), "bottomright")
    ctx_id(NULL)
  })
  observeEvent(input$ctx_annot_inside, {
    req(ctx_id())
    showAnnotationModal(ctx_id(), "inside")
    ctx_id(NULL)
  })
  
  # ── Save annotation ──
  observeEvent(input$save_annotation, {
    req(input$annotation_data)
    id <- input$annotation_data$id
    position <- input$annotation_data$position
    text <- input$annot_text %||% ""
    # Save style settings
    annot_style$col <- input$annot_col %||% annot_style$col
    annot_style$cex <- as.numeric(input$annot_cex %||% annot_style$cex)
    annot_style$font <- as.integer(input$annot_font %||% annot_style$font)
    if (nzchar(trimws(text))) {
      text_annotations[[position]][[id]] <- trimws(text)
      showNotification(
        sprintf("Annotation added to %s (%s)", id, position),
        type = "message",
        duration = 3
      )
    } else {
      text_annotations[[position]][[id]] <- NULL
      showNotification(
        sprintf("Annotation removed from %s (%s)", id, position),
        type = "message",
        duration = 3
      )
    }
    removeModal()
  })
  
  # ── Context: Reorder Siblings ──
  reorder_focus_id <- reactiveVal(NULL)
  reorder_working_order <- reactiveVal(character(0))
  reorder_selected_idx <- reactiveVal(1L)
  
  observeEvent(input$ctx_reorder, {
    req(ctx_id(), ped())
    p <- ped()
    focus_id <- ctx_id()
    if (pedtools::is.pedList(p)) {
      showNotification(
        "Reordering is only supported for a single connected pedigree.",
        type = "warning", duration = 5
      )
      ctx_id(NULL)
      return()
    }
    if (!(focus_id %in% labels(p))) {
      showNotification("Selected individual not found in pedigree.", type = "warning")
      ctx_id(NULL)
      return()
    }
    full_sibs <- tryCatch(
      pedtools::siblings(p, focus_id, half = FALSE),
      error = function(e) character(0)
    )
    sibship <- c(focus_id, full_sibs)
    if (length(sibship) < 2) {
      showNotification(
        sprintf("%s has no full siblings to reorder.", focus_id),
        type = "message", duration = 4
      )
      ctx_id(NULL)
      return()
    }
    all_labs <- labels(p)
    sibship_ordered <- all_labs[all_labs %in% sibship]
    reorder_focus_id(focus_id)
    reorder_working_order(sibship_ordered)
    reorder_selected_idx(match(focus_id, sibship_ordered))
    ctx_id(NULL)
    showModal(modalDialog(
      title = tags$div(
        style = "display:flex; align-items:center; gap:10px;",
        tags$span(class = "material-symbols-outlined", "swap_horiz"),
        sprintf("Reorder Sibship of %s", focus_id)
      ),
      tags$p(
        style = "font-size:12px; color:#555; margin-bottom:10px;",
        "Only the full sibship (individuals sharing both parents with the selected person) is listed. Use the arrows to change the left/right plot order within this sibship."
      ),
      uiOutput("reorderList"),
      div(
        style = "display:flex; gap:8px; margin-top:12px; justify-content:center;",
        actionButton("reorder_move_up",
                     HTML('<i class="fa fa-arrow-left"></i> Move Left'),
                     class = "btn btn-default btn-sm"
        ),
        actionButton("reorder_move_down",
                     HTML('Move Right <i class="fa fa-arrow-right"></i>'),
                     class = "btn btn-default btn-sm"
        )
      ),
      footer = tagList(
        actionButton("cancelReorder", "Cancel", class = "btn btn-default"),
        actionButton("resetReorder", "Reset", class = "btn btn-default"),
        actionButton("applyReorder",
                     HTML('<i class="fa fa-check"></i> Apply'),
                     class = "btn btn-primary"
        )
      ),
      easyClose = TRUE,
      size = "m"
    ))
  })
  
  output$reorderList <- renderUI({
    ids <- reorder_working_order()
    if (length(ids) == 0) {
      return(tags$p("No individuals to reorder."))
    }
    sel_idx <- reorder_selected_idx()
    if (is.null(sel_idx) || sel_idx < 1 || sel_idx > length(ids)) sel_idx <- 1L
    tags$div(
      class = "reorder-chips",
      style = "display:flex; flex-direction:row; flex-wrap:wrap; gap:10px; justify-content:center; align-items:center; padding:12px 8px; background:#f8fafc; border:1px solid #e5e7eb; border-radius:8px;",
      lapply(seq_along(ids), function(i) {
        is_sel <- (i == sel_idx)
        tags$div(
          class = "reorder-chip",
          `data-idx` = i,
          onclick = sprintf(
            "Shiny.setInputValue('reorder_chip_click', %d, {priority:'event'});", i
          ),
          style = paste0(
            "cursor:pointer; user-select:none;",
            "display:flex; flex-direction:column; align-items:center; gap:4px;",
            "min-width:64px; padding:10px 12px; border-radius:8px;",
            "border:2px solid ", if (is_sel) "#2563eb" else "#cbd5e1",
            "; background:", if (is_sel) "#dbeafe" else "#ffffff",
            "; box-shadow:", if (is_sel) "0 0 0 3px rgba(37,99,235,0.15)" else "0 1px 2px rgba(0,0,0,0.04)",
            "; transition:all 120ms ease;"
          ),
          tags$span(style = "font-size:10px; color:#64748b;", paste0("#", i)),
          tags$span(
            style = paste0(
              "font-weight:600; font-size:13px; color:",
              if (is_sel) "#1e3a8a" else "#0f172a", ";"
            ),
            ids[i]
          )
        )
      })
    )
  })
  
  observeEvent(input$reorder_chip_click, {
    v <- suppressWarnings(as.integer(input$reorder_chip_click))
    if (!is.na(v) && v >= 1 && v <= length(reorder_working_order())) {
      reorder_selected_idx(v)
    }
  })
  observeEvent(input$reorder_move_up, {
    cur <- reorder_working_order()
    idx <- reorder_selected_idx()
    if (is.null(idx) || idx <= 1 || idx > length(cur)) {
      return()
    }
    cur[c(idx - 1, idx)] <- cur[c(idx, idx - 1)]
    reorder_working_order(cur)
    reorder_selected_idx(idx - 1L)
  })
  observeEvent(input$reorder_move_down, {
    cur <- reorder_working_order()
    idx <- reorder_selected_idx()
    if (is.null(idx) || idx < 1 || idx >= length(cur)) {
      return()
    }
    cur[c(idx, idx + 1)] <- cur[c(idx + 1, idx)]
    reorder_working_order(cur)
    reorder_selected_idx(idx + 1L)
  })
  observeEvent(input$resetReorder, {
    p <- ped()
    focus_id <- reorder_focus_id()
    if (is.null(p) || pedtools::is.pedList(p) || is.null(focus_id)) {
      return()
    }
    if (!(focus_id %in% labels(p))) {
      return()
    }
    full_sibs <- tryCatch(
      pedtools::siblings(p, focus_id, half = FALSE),
      error = function(e) character(0)
    )
    sibship <- c(focus_id, full_sibs)
    all_labs <- labels(p)
    sibship_ordered <- all_labs[all_labs %in% sibship]
    reorder_working_order(sibship_ordered)
    reorder_selected_idx(match(focus_id, sibship_ordered))
  })
  observeEvent(input$cancelReorder, {
    removeModal()
  })
  observeEvent(input$applyReorder, {
    p <- ped()
    new_order <- reorder_working_order()
    if (is.null(p) || length(new_order) == 0) {
      removeModal()
      return()
    }
    all_labs <- labels(p)
    current_sibship_order <- all_labs[all_labs %in% new_order]
    if (identical(new_order, current_sibship_order)) {
      removeModal()
      showNotification("Order unchanged.", type = "message")
      return()
    }
    save_hist()
    old_ped <- p
    new_ped <- tryCatch(
      {
        reordered <- pedtools::reorderPed(old_ped, neworder = new_order, internal = FALSE)
        pedtools::parentsBeforeChildren(reordered)
      },
      error = function(e) {
        showNotification(sprintf("Cannot reorder: %s", e$message), type = "error", duration = 5)
        NULL
      }
    )
    if (is.null(new_ped)) {
      return()
    }
    removeModal()
    relabel_and_update(old_ped, new_ped)
    showNotification("Pedigree reordered successfully.", type = "message")
  })
  
  # ── Context: Delete ──
  observeEvent(input$ctx_delete, {
    req(ctx_id(), ped())
    tgt <- ctx_id()
    if (!(tgt %in% labels(ped()))) {
      showNotification("Individual not found in pedigree.", type = "error")
      ctx_id(NULL)
      return()
    }
    save_hist()
    old_ped <- ped()
    newped <- tryCatch(
      pedtools::removeIndividuals(old_ped, ids = tgt),
      error = function(e) {
        showNotification(
          sprintf("Cannot delete %s: %s", tgt, e$message),
          type = "error",
          duration = 5
        )
        NULL
      }
    )
    if (is.null(newped)) {
      ctx_id(NULL)
      return(invisible(NULL))
    }
    
    # Clean all status lists BEFORE relabel
    miscarriage(setdiff(miscarriage(), tgt))
    deceased_ids(setdiff(deceased_ids(), tgt))
    carrier_ids(setdiff(carrier_ids(), tgt))
    starred_ids(setdiff(starred_ids(), tgt))
    adopted_ids(setdiff(adopted_ids(), tgt))
    proband_id(setdiff(proband_id(), tgt))
    afab_ids(setdiff(afab_ids(), tgt))
    amab_ids(setdiff(amab_ids(), tgt))
    umab_ids(setdiff(umab_ids(), tgt))
    infertility_ids(setdiff(infertility_ids(), tgt))
    current_twins <- twins_df()
    if (nrow(current_twins) > 0) {
      twins_df(current_twins[
        current_twins$id1 != tgt & current_twins$id2 != tgt, ,
        drop = FALSE
      ])
    }
    relabel_and_update(old_ped, newped)
    
    if (length(sel()) > 0 && sel()[1] == tgt) {
      sel(character(0))
    }
    showNotification(sprintf("%s deleted from pedigree", tgt), type = "message")
    ctx_id(NULL)
  })
  # ════════════════════════════════════════════════════════
  # Delete Selected (individual panel button)
  # ════════════════════════════════════════════════════════
  observeEvent(input$btn_delete_selected, {
    req(sel(), ped())
    tgt <- sel()[1]
    if (!(tgt %in% labels(ped()))) {
      showNotification("Individual not found in pedigree.", type = "error")
      return()
    }
    save_hist()
    old_ped <- ped()
    newped <- tryCatch(
      pedtools::removeIndividuals(old_ped, ids = tgt),
      error = function(e) {
        showNotification(
          sprintf("Cannot delete %s: %s", tgt, e$message),
          type = "error",
          duration = 5
        )
        NULL
      }
    )
    if (is.null(newped)) {
      return(invisible(NULL))
    }
    
    # Clean all status lists BEFORE relabel
    miscarriage(setdiff(miscarriage(), tgt))
    deceased_ids(setdiff(deceased_ids(), tgt))
    carrier_ids(setdiff(carrier_ids(), tgt))
    starred_ids(setdiff(starred_ids(), tgt))
    adopted_ids(setdiff(adopted_ids(), tgt))
    proband_id(setdiff(proband_id(), tgt))
    afab_ids(setdiff(afab_ids(), tgt))
    amab_ids(setdiff(amab_ids(), tgt))
    umab_ids(setdiff(umab_ids(), tgt))
    infertility_ids(setdiff(infertility_ids(), tgt))
    current_twins <- twins_df()
    if (nrow(current_twins) > 0) {
      twins_df(current_twins[
        current_twins$id1 != tgt & current_twins$id2 != tgt, ,
        drop = FALSE
      ])
    }
    relabel_and_update(old_ped, newped)
    sel(character(0))
    showNotification(sprintf("%s deleted from pedigree", tgt), type = "message")
  })
  
  # Build the detailed UI panel that explains each loop as two inheritance paths
  # converging on the affected individual.
  describe_inbreeding_content_ui <- function(ped_obj) {
    if (is.null(ped_obj)) {
      return(NULL)
    }
    
    loops <- safe_inbreeding_loops(ped_obj)
    
    if (length(loops) == 0) {
      return(
        tags$div(
          class = "loop-empty",
          tags$h4("No inbreeding loops detected"),
          tags$p("This pedigree does not contain any identifiable inbreeding loops.")
        )
      )
    }
    
    tops_lbl <- unique(unlist(lapply(loops, function(L) id_to_label(ped_obj, L$top))))
    bottoms_lbl <- unique(unlist(lapply(loops, function(L) id_to_label(ped_obj, L$bottom))))
    
    summary_block <- tags$div(
      class = "loop-summary-card",
      tags$div(class = "loop-card-header", "Summary"),
      tags$p(sprintf("Total number of loops: %d", length(loops))),
      tags$p(sprintf("Common ancestors involved: %s", paste(tops_lbl, collapse = ", "))),
      tags$p(sprintf("Affected individual(s): %s", paste(bottoms_lbl, collapse = ", ")))
    )
    
    loop_blocks <- lapply(seq_along(loops), function(i) {
      L <- loops[[i]]
      
      top_lbl <- id_to_label(ped_obj, L$top)
      bottom_lbl <- id_to_label(ped_obj, L$bottom)
      
      pathA_full <- format_path_label(ped_obj, L$top, L$pathA, L$bottom)
      pathB_full <- format_path_label(ped_obj, L$top, L$pathB, L$bottom)
      
      pathA_branch <- paste(id_to_label(ped_obj, c(L$top, L$pathA)), collapse = " \u2192 ")
      pathB_branch <- paste(id_to_label(ped_obj, c(L$top, L$pathB)), collapse = " \u2192 ")
      
      interpretation <- sprintf(
        "Individual %s is inbred through the common ancestor %s. One inheritance path may follow branch %s, while another may follow branch %s. These two paths converge at %s.",
        bottom_lbl, top_lbl, pathA_branch, pathB_branch, bottom_lbl
      )
      
      tags$div(
        class = "loop-card",
        tags$div(class = "loop-card-header", paste("Loop", i)),
        tags$div(
          class = "loop-grid",
          tags$div(
            class = "loop-item",
            tags$span(class = "loop-label", "Common ancestor: "),
            tags$span(class = "loop-value", top_lbl)
          ),
          tags$div(
            class = "loop-item",
            tags$span(class = "loop-label", "Affected individual: "),
            tags$span(class = "loop-value", bottom_lbl)
          )
        ),
        tags$p(class = "loop-interpretation", interpretation),
        tags$div(
          class = "loop-path",
          tags$div(tags$b("Path 1: "), tags$code(pathA_full)),
          tags$div(tags$b("Path 2: "), tags$code(pathB_full))
        )
      )
    })
    
    tags$div(
      class = "loops-container",
      summary_block,
      loop_blocks
    )
  }
  # ---------------------------------------------------------------------------
  # Pairwise relationship and kinship analysis
  # ---------------------------------------------------------------------------
  relationship_person_a <- reactive({
    req(ped())
    ids <- labels(ped())
    selected_ids <- sel()
    if (length(selected_ids) >= 1 && selected_ids[1] %in% ids) {
      selected_ids[1]
    } else {
      character(0)
    }
  })
  
  output$relationshipPersonAReadonly <- renderUI({
    id <- relationship_person_a()
    if (!length(id) || !nzchar(id[1])) {
      return(div(class = "relationship-person-readonly is-empty", "No individual selected"))
    }
    div(class = "relationship-person-readonly", id[1])
  })
  
  observe({
    req(ped())
    
    ids <- labels(ped())
    if (!length(ids)) {
      return()
    }
    
    default_a <- relationship_person_a()
    
    selected_ids <- sel()
    b_choices <- setdiff(ids, default_a)
    default_b <- if (length(selected_ids) >= 2 && selected_ids[2] %in% b_choices) {
      selected_ids[2]
    } else {
      if (length(b_choices)) b_choices[1] else character(0)
    }
    
    updateSelectInput(session, "person_b", choices = b_choices, selected = default_b)
  })
  
  output$relationshipHints <- renderUI({
    req(ped())
    
    id1 <- relationship_person_a()
    
    if (!length(id1) || !nzchar(id1[1])) {
      return(div(class = "hint", "Select an individual first."))
    }
    
    if (is.null(input$person_b) || !nzchar(input$person_b)) {
      return(div(class = "hint", "Choose Individual B."))
    }
    
    if (identical(id1[1], input$person_b)) {
      return(div(class = "hint", "Please choose two different individuals."))
    }
    
    NULL
  })
  
  rel_info_df <- reactive({
    id1 <- relationship_person_a()
    req(ped(), length(id1), nzchar(id1[1]))
    create_info_table(ped(), id1[1])
  })
  
  rel_top_related_df <- reactive({
    id1 <- relationship_person_a()
    req(ped(), length(id1), nzchar(id1[1]))
    create_top_related_table(ped(), id1[1], n_top = 5)
  })
  
  rel_pairwise_df <- eventReactive(input$analyze_relationship,
                                   {
                                     id1 <- relationship_person_a()
                                     req(ped(), length(id1), input$person_b)
                                     
                                     shiny::validate(
                                       need(nzchar(id1[1]), "Select an individual first."),
                                       need(nzchar(input$person_b), "Choose Individual B."),
                                       need(!identical(id1[1], input$person_b), "Choose two different individuals.")
                                     )
                                     
                                     create_pairwise_table(ped(), c(id1[1], input$person_b))
                                   },
                                   ignoreInit = FALSE
  )
  
  output$relInfoTable <- renderDT({
    req(rel_info_df())
    make_rel_dt(rel_info_df())
  })
  
  output$relTopRelatedTable <- renderDT({
    req(rel_top_related_df())
    make_rel_dt(rel_top_related_df())
  })
  
  output$relPairwiseTable <- renderDT({
    req(rel_pairwise_df())
    make_rel_dt(rel_pairwise_df())
  })
  
  observeEvent(input$analyze_relationship, {
    req(ped())
    id1 <- relationship_person_a()
    id1 <- if (length(id1)) id1[1] else ""
    id2 <- input$person_b %||% ""
    
    if (!isTruthy(id1) || !isTruthy(id2)) {
      output$rel_text_personal <- renderText("Please select two individuals.")
      output$rel_kappa_personal <- renderTable(data.frame())
      output$rel_canonical_personal <- renderText("")
      return()
    }
    
    if (identical(id1, id2)) {
      output$rel_text_personal <- renderText("Please choose two different individuals.")
      output$rel_kappa_personal <- renderTable(data.frame())
      output$rel_canonical_personal <- renderText("")
      return()
    }
    
    rel_pedtools <- tryCatch(
      pedtools::relation(ped(), from = id1, to = id2),
      error = function(e) NA_character_
    )
    
    rel_verbal <- tryCatch(
      {
        txt <- format(verbalisr::verbalise(ped(), c(id1, id2)))
        gsub("([[:graph:]])  ([[:graph:]])", "\\1 \\2", txt)
      },
      error = function(e) NULL
    )
    
    output$rel_text_personal <- renderText({
      paste0(
        id1, " — ", id2, " : ",
        if (!is.na(rel_pedtools)) paste0(rel_pedtools, if (!is.null(rel_verbal)) " | " else "") else "",
        if (!is.null(rel_verbal)) rel_verbal else "(description unavailable)"
      )
    })
    
    inb <- tryCatch(ribd::inbreeding(ped(), c(id1, id2)), error = function(e) c(NA_real_, NA_real_))
    phi <- tryCatch(ribd::kinship(ped(), c(id1, id2)), error = function(e) NA_real_)
    deg <- tryCatch(ribd::kin2deg(phi, unrelated = NA), error = function(e) NA_real_)
    
    if (all(!is.na(inb)) && all(inb == 0)) {
      kap <- tryCatch(ribd::kappaIBD(ped(), c(id1, id2)), error = function(e) c(NA_real_, NA_real_, NA_real_))
      df <- data.frame(
        id1 = id1, id2 = id2,
        f1 = round(inb[1], 4), f2 = round(inb[2], 4),
        phi = round(phi, 4), deg = round(deg, 4),
        k0 = round(kap[1], 4), k1 = round(kap[2], 4), k2 = round(kap[3], 4),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    } else {
      delta <- tryCatch(ribd::condensedIdentity(ped(), c(id1, id2)), error = function(e) rep(NA_real_, 9))
      names(delta) <- paste0("Δ", 1:9)
      df <- data.frame(
        id1 = id1, id2 = id2,
        f1 = round(inb[1], 4), f2 = round(inb[2], 4),
        phi = round(phi, 4), deg = round(deg, 4),
        t(round(as.numeric(delta), 4)),
        check.names = FALSE, row.names = NULL
      )
      colnames(df)[7:15] <- paste0("Δ", 1:9)
    }
    
    output$rel_kappa_personal <- renderTable(df)
    
    output$rel_canonical_personal <- renderText({
      if (!isTRUE(input$showCanonicalPersonal)) {
        return("")
      }
      if (!(all(!is.na(inb)) && all(inb == 0))) {
        return("constructPedigree is not relevant for inbred individuals.")
      }
      kap <- tryCatch(ribd::kappaIBD(ped(), c(id1, id2)), error = function(e) c(NA, NA, NA))
      if (any(is.na(kap))) {
        return("Kappa not available.")
      }
      txt <- capture.output(ribd::constructPedigree(kappa = kap, describe = TRUE))
      paste(txt[txt != ""], collapse = "\n")
    })
  })
}


shinyApp(ui = ui, server = server)
