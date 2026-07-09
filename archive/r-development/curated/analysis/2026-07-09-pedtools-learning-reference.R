# Archived R development file
# Original path: apprentissage_test /fonction/aidefonctionpedtools.R
# Original created: 2026-07-09 03:35:11
# Original modified: 2026-07-09 03:35:11
# Archive rationale: Late learning/reference file documenting pedtools helper experimentation.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# Dependencies ----------------------------------------------------------------
library(shiny)
library(bslib)
library(pedtools)
library(ribd)

# Helpers ---------------------------------------------------------------------
relabel_gen <- function(ped_obj) {
  rf <- names(formals(pedtools::relabel))
  
  if ("new" %in% rf) {
    pedtools::relabel(ped_obj, new = "generations")
  } else {
    pedtools::relabel(ped_obj, "generations")
  }
}

make_random_ped <- function() {
  relabel_gen(
    randomPed(
      n = sample(5:10, 1),
      maxDirectGap = Inf,
      selfing = FALSE
    )
  )
}

safe_capture <- function(expr) {
  warnings <- character()
  
  out <- tryCatch(
    withCallingHandlers(
      capture.output(force(expr)),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      paste("Erreur :", conditionMessage(e))
    }
  )
  
  txt <- paste(out, collapse = "\n")
  
  if (length(warnings) > 0) {
    txt <- paste(
      paste0("Avertissement : ", warnings),
      txt,
      sep = "\n"
    )
  }
  
  if (!nzchar(txt)) {
    txt <- "(aucune sortie)"
  }
  
  txt
}

safe_plot <- function(x) {
  tryCatch(
    {
      graphics::par(mar = c(1, 1, 1, 1))
      plot(
        x,
        cex = 0.9,
        margins = c(2, 2, 4, 2)
      )
    },
    error = function(e) {
      graphics::plot.new()
      graphics::text(
        x = 0.5,
        y = 0.58,
        labels = "Impossible d'afficher ce pedigree",
        col = "red",
        cex = 1.2
      )
      graphics::text(
        x = 0.5,
        y = 0.45,
        labels = conditionMessage(e),
        col = "gray40",
        cex = 0.9
      )
    }
  )
}

get_family_code <- function(f) {
  body_txt <- paste(deparse(body(f)), collapse = "\n")
  
  paste0(
    "x <- relabel_gen(\n",
    "  ",
    gsub("\n", "\n  ", body_txt),
    "\n)"
  )
}

quote_id <- function(id) {
  paste0('"', id, '"')
}

ids_to_r_vector <- function(ids) {
  if (is.null(ids) || length(ids) == 0) {
    return("character(0)")
  }
  
  paste0(
    "c(",
    paste(paste0('"', ids, '"'), collapse = ", "),
    ")"
  )
}

require_id1 <- function(input) {
  if (is.null(input$id1) || input$id1 == "") {
    stop("Sélectionne d'abord un ID 1.", call. = FALSE)
  }
  
  input$id1
}

require_id2 <- function(input) {
  if (is.null(input$id2) || input$id2 == "") {
    stop("Sélectionne d'abord un ID 2.", call. = FALSE)
  }
  
  input$id2
}

require_multi_ids <- function(input, n = 2) {
  ids <- input$ids_multi
  
  if (is.null(ids) || length(ids) < n) {
    stop(
      paste0("Sélectionne au moins ", n, " individus dans IDs multiples."),
      call. = FALSE
    )
  }
  
  ids
}

# Families --------------------------------------------------------------------
# La structure est conservée : chaque entrée est directement une fonction.

families <- list(
  # ── Nuclear Families ──────────────────────────────────
  "Nuclear: Trio (1 child)" = function() nuclearPed(1),
  "Nuclear: 2 children (mixed)" = function() nuclearPed(2, sex = c(1, 2)),
  "Nuclear: 5 children" = function() nuclearPed(5, sex = c(1, 1, 2, 2, 2)),
  
  # ── Linear / Ancestral ───────────────────────────────
  "Ancestral: 2 gen back" = function() ancestralPed(2),
  "Ancestral: 3 gen back" = function() ancestralPed(3),
  
  # ── Half-siblings ────────────────────────────────────
  "Half-sibs: paternal" = function() halfSibPed(1, 1),
  "Half-sibs: maternal" = function() halfSibPed(1, 1, type = "maternal"),
  "Half-sibs: 2+2 maternal" = function() halfSibPed(2, 2, type = "maternal"),
  
  # ── Cousins (full) ───────────────────────────────────
  "1st cousins" = function() cousinPed(1, symmetric = TRUE),
  "1st cousins + child" = function() {
    cousinPed(1, symmetric = TRUE, child = TRUE)
  },
  "2nd cousins" = function() cousinPed(2, symmetric = TRUE),
  "2nd cousins + child" = function() {
    cousinPed(2, symmetric = TRUE, child = TRUE)
  },
  "3rd cousins" = function() cousinPed(3, symmetric = TRUE),
  "3rd cousins + child" = function() {
    cousinPed(3, symmetric = TRUE, child = TRUE)
  },
  
  # ── Complex / Inbred structures ─────────────────────
  "Half-sib stack (2)" = function() halfSibStack(2),
  "Half-sib stack (3)" = function() halfSibStack(3),
  "Half-sib triangle (3)" = function() halfSibTriangle(3),
  "Half-sib triangle (4)" = function() halfSibTriangle(4),
  
  # ── 3/4 siblings ────────────────────────────────────
  "3/4-siblings" = function() {
    nuclearPed(2) |>
      addSon(c(3, 5), verbose = FALSE) |>
      addSon(4:5, verbose = FALSE)
  },
  "3/4-siblings + child" = function() {
    nuclearPed(2) |>
      addSon(c(3, 5), verbose = FALSE) |>
      addDaughter(4:5, verbose = FALSE) |>
      addSon(6:7, verbose = FALSE)
  }
)

# Actions ---------------------------------------------------------------------
actions <- list(
  "print_x" = list(
    label = "print(x)",
    code = function(input) "print(x)",
    run = function(x, input) {
      print(x)
      invisible(NULL)
    }
  ),
  
  "coeff_table" = list(
    label = "coeffTable(x)",
    code = function(input) "coeffTable(x)",
    run = function(x, input) coeffTable(x)
  ),
  
  "coeff_table_selected" = list(
    label = "coeffTable selected",
    code = function(input) 'coeffTable(x, coeff = c("phi", "deg", "kappa"))',
    run = function(x, input) {
      coeffTable(
        x,
        coeff = c("phi", "deg", "kappa")
      )
    }
  ),
  
  "matrix_attrs" = list(
    label = "as.matrix(x)",
    code = function(input) {
      paste(
        "m <- as.matrix(x, include.attrs = TRUE)",
        "m",
        sep = "\n"
      )
    },
    run = function(x, input) {
      m <- as.matrix(x, include.attrs = TRUE)
      m
    }
  ),
  
  "loop_breakers" = list(
    label = "findLoopBreakers(x)",
    code = function(input) {
      paste(
        "findLoopBreakers(",
        "  x,",
        "  score = NULL,",
        "  errorIfFail = TRUE,",
        "  allowFounder = FALSE,",
        "  allowRepeated = FALSE",
        ")",
        sep = "\n"
      )
    },
    run = function(x, input) {
      findLoopBreakers(
        x,
        score = NULL,
        errorIfFail = TRUE,
        allowFounder = FALSE,
        allowRepeated = FALSE
      )
    }
  ),
  
  "pedsize" = list(
    label = "pedsize(x)",
    code = function(input) "pedsize(x)",
    run = function(x, input) pedsize(x)
  ),
  
  "generations" = list(
    label = "generations(x)",
    code = function(input) {
      'generations(x, what = c("max", "compMax", "indiv", "depth"))'
    },
    run = function(x, input) {
      generations(
        x,
        what = c("max", "compMax", "indiv", "depth")
      )
    }
  ),
  
  "n_children" = list(
    label = "nChildren(x)",
    code = function(input) "nChildren(x, ids = labels(x), named = FALSE)",
    run = function(x, input) {
      nChildren(
        x,
        ids = labels(x),
        named = FALSE
      )
    }
  ),
  
  "has_unbroken_loops" = list(
    label = "hasUnbrokenLoops(x)",
    code = function(input) "hasUnbrokenLoops(x)",
    run = function(x, input) hasUnbrokenLoops(x)
  ),
  
  "has_inbred_founders" = list(
    label = "hasInbredFounders(x)",
    code = function(input) 'hasInbredFounders(x, chromType = "autosomal")',
    run = function(x, input) {
      hasInbredFounders(
        x,
        chromType = "autosomal"
      )
    }
  ),
  
  "has_selfing" = list(
    label = "hasSelfing(x)",
    code = function(input) "hasSelfing(x)",
    run = function(x, input) hasSelfing(x)
  ),
  
  "has_common_ancestor" = list(
    label = "hasCommonAncestor(x)",
    code = function(input) "hasCommonAncestor(x)",
    run = function(x, input) hasCommonAncestor(x)
  ),
  
  "subnucs" = list(
    label = "subnucs(x)",
    code = function(input) "subnucs(x)",
    run = function(x, input) subnucs(x)
  ),
  
  "peeling_order" = list(
    label = "peelingOrder(x)",
    code = function(input) "peelingOrder(x)",
    run = function(x, input) peelingOrder(x)
  ),
  
  "founders" = list(
    label = "founders(x)",
    code = function(input) "founders(x, internal = FALSE)",
    run = function(x, input) {
      founders(x, internal = FALSE)
    }
  ),
  
  "nonfounders" = list(
    label = "nonfounders(x)",
    code = function(input) "nonfounders(x, internal = FALSE)",
    run = function(x, input) {
      nonfounders(x, internal = FALSE)
    }
  ),
  
  "leaves" = list(
    label = "leaves(x)",
    code = function(input) "leaves(x, internal = FALSE)",
    run = function(x, input) {
      leaves(x, internal = FALSE)
    }
  ),
  
  "males" = list(
    label = "males(x)",
    code = function(input) "males(x, internal = FALSE)",
    run = function(x, input) {
      males(x, internal = FALSE)
    }
  ),
  
  "females" = list(
    label = "females(x)",
    code = function(input) "females(x, internal = FALSE)",
    run = function(x, input) {
      females(x, internal = FALSE)
    }
  ),
  
  "typed_members" = list(
    label = "typedMembers(x)",
    code = function(input) "typedMembers(x, internal = FALSE)",
    run = function(x, input) {
      typedMembers(x, internal = FALSE)
    }
  ),
  
  "untyped_members" = list(
    label = "untypedMembers(x)",
    code = function(input) "untypedMembers(x, internal = FALSE)",
    run = function(x, input) {
      untypedMembers(x, internal = FALSE)
    }
  ),
  
  "father" = list(
    label = "father(x, id)",
    code = function(input) {
      paste0(
        "father(x, id = ",
        quote_id(input$id1),
        ", internal = FALSE)"
      )
    },
    run = function(x, input) {
      father(
        x,
        id = require_id1(input),
        internal = FALSE
      )
    }
  ),
  
  "mother" = list(
    label = "mother(x, id)",
    code = function(input) {
      paste0(
        "mother(x, id = ",
        quote_id(input$id1),
        ", internal = FALSE)"
      )
    },
    run = function(x, input) {
      mother(
        x,
        id = require_id1(input),
        internal = FALSE
      )
    }
  ),
  
  "children" = list(
    label = "children(x, id)",
    code = function(input) {
      paste0(
        "children(x, id = ",
        quote_id(input$id1),
        ", internal = FALSE, bySpouse = FALSE)"
      )
    },
    run = function(x, input) {
      children(
        x,
        id = require_id1(input),
        internal = FALSE,
        bySpouse = FALSE
      )
    }
  ),
  
  "children2" = list(
    label = "children2(x, id1, id2)",
    code = function(input) {
      paste0(
        "children2(x, id1 = ",
        quote_id(input$id1),
        ", id2 = ",
        quote_id(input$id2),
        ", internal = FALSE)"
      )
    },
    run = function(x, input) {
      children2(
        x,
        id1 = require_id1(input),
        id2 = require_id2(input),
        internal = FALSE
      )
    }
  ),
  
  "spouses" = list(
    label = "spouses(x, id)",
    code = function(input) {
      paste0(
        "spouses(x, id = ",
        quote_id(input$id1),
        ", internal = FALSE)"
      )
    },
    run = function(x, input) {
      spouses(
        x,
        id = require_id1(input),
        internal = FALSE
      )
    }
  ),
  
  "unrelated" = list(
    label = "unrelated(x, id)",
    code = function(input) {
      paste0(
        "unrelated(x, id = ",
        quote_id(input$id1),
        ", internal = FALSE)"
      )
    },
    run = function(x, input) {
      unrelated(
        x,
        id = require_id1(input),
        internal = FALSE
      )
    }
  ),
  
  "parents" = list(
    label = "parents(x, id)",
    code = function(input) {
      paste0(
        "parents(x, id = ",
        quote_id(input$id1),
        ", internal = FALSE)"
      )
    },
    run = function(x, input) {
      parents(
        x,
        id = require_id1(input),
        internal = FALSE
      )
    }
  ),
  
  "grandparents" = list(
    label = "grandparents(x, id)",
    code = function(input) {
      paste0(
        "grandparents(x, id = ",
        quote_id(input$id1),
        ", degree = 2, internal = FALSE)"
      )
    },
    run = function(x, input) {
      grandparents(
        x,
        id = require_id1(input),
        degree = 2,
        internal = FALSE
      )
    }
  ),
  
  "siblings" = list(
    label = "siblings(x, id)",
    code = function(input) {
      paste0(
        "siblings(x, id = ",
        quote_id(input$id1),
        ", half = NA, internal = FALSE)"
      )
    },
    run = function(x, input) {
      siblings(
        x,
        id = require_id1(input),
        half = NA,
        internal = FALSE
      )
    }
  ),
  
  "nephews_nieces" = list(
    label = "nephews_nieces(x, id)",
    code = function(input) {
      paste0(
        "nephews_nieces(x, id = ",
        quote_id(input$id1),
        ", removal = 1, half = NA, internal = FALSE)"
      )
    },
    run = function(x, input) {
      nephews_nieces(
        x,
        id = require_id1(input),
        removal = 1,
        half = NA,
        internal = FALSE
      )
    }
  ),
  
  "niblings" = list(
    label = "niblings(x, id)",
    code = function(input) {
      paste0(
        "niblings(x, id = ",
        quote_id(input$id1),
        ", half = NA, internal = FALSE)"
      )
    },
    run = function(x, input) {
      niblings(
        x,
        id = require_id1(input),
        half = NA,
        internal = FALSE
      )
    }
  ),
  
  "piblings" = list(
    label = "piblings(x, id)",
    code = function(input) {
      paste0(
        "piblings(x, id = ",
        quote_id(input$id1),
        ", half = NA, internal = FALSE)"
      )
    },
    run = function(x, input) {
      piblings(
        x,
        id = require_id1(input),
        half = NA,
        internal = FALSE
      )
    }
  ),
  
  "ancestors" = list(
    label = "ancestors(x, id)",
    code = function(input) {
      paste0(
        "ancestors(x, id = ",
        quote_id(input$id1),
        ", maxGen = Inf, inclusive = FALSE, internal = FALSE)"
      )
    },
    run = function(x, input) {
      ancestors(
        x,
        id = require_id1(input),
        maxGen = Inf,
        inclusive = FALSE,
        internal = FALSE
      )
    }
  ),
  
  "descendants" = list(
    label = "descendants(x, id)",
    code = function(input) {
      paste0(
        "descendants(x, id = ",
        quote_id(input$id1),
        ", maxGen = Inf, inclusive = FALSE, internal = FALSE)"
      )
    },
    run = function(x, input) {
      descendants(
        x,
        id = require_id1(input),
        maxGen = Inf,
        inclusive = FALSE,
        internal = FALSE
      )
    }
  ),
  
  "common_ancestors" = list(
    label = "commonAncestors(x, ids)",
    code = function(input) {
      paste0(
        "commonAncestors(x, ids = ",
        ids_to_r_vector(input$ids_multi),
        ", maxGen = Inf, inclusive = FALSE, internal = FALSE)"
      )
    },
    run = function(x, input) {
      commonAncestors(
        x,
        ids = require_multi_ids(input, 2),
        maxGen = Inf,
        inclusive = FALSE,
        internal = FALSE
      )
    }
  ),
  
  "common_descendants" = list(
    label = "commonDescendants(x, ids)",
    code = function(input) {
      paste0(
        "commonDescendants(x, ids = ",
        ids_to_r_vector(input$ids_multi),
        ", maxGen = Inf, inclusive = FALSE, internal = FALSE)"
      )
    },
    run = function(x, input) {
      commonDescendants(
        x,
        ids = require_multi_ids(input, 2),
        maxGen = Inf,
        inclusive = FALSE,
        internal = FALSE
      )
    }
  ),
  
  "descent_paths" = list(
    label = "descentPaths(x)",
    code = function(input) "descentPaths(x, ids = founders(x), internal = FALSE)",
    run = function(x, input) {
      descentPaths(
        x,
        ids = founders(x),
        internal = FALSE
      )
    }
  )
)

action_groups <- list(
  "Base" = c(
    "print_x",
    "coeff_table",
    "coeff_table_selected",
    "matrix_attrs",
    "loop_breakers"
  ),
  
  "Structure" = c(
    "pedsize",
    "generations",
    "n_children",
    "has_unbroken_loops",
    "has_inbred_founders",
    "has_selfing",
    "has_common_ancestor",
    "subnucs",
    "peeling_order"
  ),
  
  "Individus" = c(
    "founders",
    "nonfounders",
    "leaves",
    "males",
    "females",
    "typed_members",
    "untyped_members"
  ),
  
  "Relations ID 1" = c(
    "father",
    "mother",
    "children",
    "spouses",
    "unrelated",
    "parents",
    "grandparents",
    "siblings",
    "nephews_nieces",
    "niblings",
    "piblings",
    "ancestors",
    "descendants"
  ),
  
  "Relations multiples" = c(
    "children2",
    "common_ancestors",
    "common_descendants",
    "descent_paths"
  )
)

make_action_button <- function(id) {
  actionButton(
    inputId = id,
    label = actions[[id]]$label,
    class = "btn btn-outline-primary btn-sm action-btn"
  )
}

make_tab_panel <- function(group_name, ids) {
  tabPanel(
    title = group_name,
    tags$br(),
    tags$div(
      class = "button-grid",
      lapply(ids, make_action_button)
    )
  )
}

action_tabs <- do.call(
  tabsetPanel,
  c(
    list(id = "action_tabs", type = "tabs"),
    lapply(names(action_groups), function(group_name) {
      make_tab_panel(
        group_name = group_name,
        ids = action_groups[[group_name]]
      )
    })
  )
)

# UI --------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Pedigree demo",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  tags$head(
    tags$style(
      HTML(
        "
        .button-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
          gap: 8px;
          margin-bottom: 10px;
        }

        .action-btn {
          width: 100%;
          white-space: normal;
        }

        .small-help {
          color: #6c757d;
          font-size: 0.9rem;
          margin-top: 6px;
        }

        pre {
          background-color: #f8f9fa !important;
          border: 1px solid #dee2e6 !important;
          border-radius: 6px !important;
          padding: 12px !important;
          font-size: 0.9rem !important;
        }

        #ped_code {
          max-height: 260px;
          overflow-y: auto;
        }

        #console_output {
          max-height: 440px;
          overflow-y: auto;
        }

        .console-label {
          font-weight: 700;
          margin-top: 12px;
          margin-bottom: 4px;
        }

        .sidebar-title {
          font-weight: 700;
          letter-spacing: 0.04em;
        }
        "
      )
    )
  ),
  
  sidebar = sidebar(
    title = tags$span(class = "sidebar-title", "SELECTION"),
    
 
    card(
      card_header("Fonctions à tester"),
      action_tabs,
      
      tags$hr(),
      
      actionButton(
        inputId = "clear_console",
        label = "Clear console",
        class = "btn btn-outline-secondary"
      )
    ),
    tags$hr(),
    
    
      
      card(
        card_header("Individus à tester"),
        
        selectInput(
          inputId = "id1",
          label = "ID 1",
          choices = character(0)
        ),
        
        selectInput(
          inputId = "id2",
          label = "ID 2",
          choices = character(0)
        ),
        
        selectizeInput(
          inputId = "ids_multi",
          label = "IDs multiples",
          choices = character(0),
          multiple = TRUE
        )
        
     
      )
      
      
    
    
   
  ),
  div( style ="  display: flex;
  flex-direction: row; gap: 15px;" ,
       selectInput(
         inputId = "family_type",
         label = "Choose a pedigree",
         choices = c("Sélectionner un pedigree" = "", names(families)),
         selected = ""
       ),
    actionButton(
    inputId = "random",
    label = "Random pedigree"
  )
  
  
  

  ),
  tags$hr(),
  
  tags$strong("Code du pedigree courant"),
  verbatimTextOutput("ped_code"),
  card(
    full_screen = TRUE,
    
    card_header("Pedigree"),

    plotOutput(
      outputId = "ped_plot",
      height = "560px"
    ),
    

    

  ),
  tags$hr(),
  
  card(
    card_header("Console"),
    
    tags$div(
      class = "console-label",
      textOutput("console_title")
    ),
    
    tags$div(class = "console-label", "Code exécuté"),
    verbatimTextOutput("console_code"),
    
    tags$div(class = "console-label", "Résultat"),
    verbatimTextOutput("console_output")
  )
)

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {
  ped <- reactiveVal(NULL)
  ped_code <- reactiveVal("")
  
  console_title <- reactiveVal("")
  console_code <- reactiveVal("")
  console_output <- reactiveVal("")
  
  clear_console <- function() {
    console_title("")
    console_code("")
    console_output("")
  }
  
  set_current_ped <- function(x, code) {
    ped(x)
    ped_code(code)
    clear_console()
  }
  
  observeEvent(input$random, {
    updateSelectInput(
      session,
      "family_type",
      selected = ""
    )
    
    x <- tryCatch(
      make_random_ped(),
      error = function(e) {
        showNotification(
          paste("Erreur pendant la génération aléatoire :", conditionMessage(e)),
          type = "error"
        )
        NULL
      }
    )
    
    if (is.null(x)) {
      return()
    }
    
    code <- paste(
      "x <- relabel_gen(",
      "  randomPed(",
      "    n = sample(5:10, 1),",
      "    maxDirectGap = Inf,",
      "    selfing = FALSE",
      "  )",
      ")",
      sep = "\n"
    )
    
    set_current_ped(x, code)
  })
  
  observeEvent(input$family_type, {
    if (is.null(input$family_type) || input$family_type == "") {
      return()
    }
    
    f <- families[[input$family_type]]
    
    x <- tryCatch(
      relabel_gen(f()),
      error = function(e) {
        showNotification(
          paste("Impossible de créer ce pedigree :", conditionMessage(e)),
          type = "error"
        )
        NULL
      }
    )
    
    if (is.null(x)) {
      return()
    }
    
    set_current_ped(
      x = x,
      code = get_family_code(f)
    )
  }, ignoreInit = TRUE)
  
  observeEvent(ped(), {
    x <- ped()
    
    if (is.null(x)) {
      updateSelectInput(session, "id1", choices = character(0))
      updateSelectInput(session, "id2", choices = character(0))
      updateSelectizeInput(session, "ids_multi", choices = character(0), server = TRUE)
      return()
    }
    
    ids <- labels(x)
    
    selected_id1 <- if (length(ids) >= 1) ids[1] else character(0)
    selected_id2 <- if (length(ids) >= 2) ids[2] else selected_id1
    selected_multi <- head(ids, min(2, length(ids)))
    
    updateSelectInput(
      session,
      "id1",
      choices = ids,
      selected = selected_id1
    )
    
    updateSelectInput(
      session,
      "id2",
      choices = ids,
      selected = selected_id2
    )
    
    updateSelectizeInput(
      session,
      "ids_multi",
      choices = ids,
      selected = selected_multi,
      server = TRUE
    )
  }, ignoreInit = TRUE)
  
  output$ped_plot <- renderPlot({
    x <- ped()
    
    if (is.null(x)) {
      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()
      graphics::text(
        x = 0.5,
        y = 0.56,
        labels = "Aucun pedigree chargé",
        col = "gray30",
        cex = 1.3
      )
      graphics::text(
        x = 0.5,
        y = 0.44,
        labels = "Sélectionne un pedigree ou clique sur Random pedigree.",
        col = "gray50",
        cex = 1
      )
      return(invisible(NULL))
    }
    
    safe_plot(x)
  }, res = 96)
  
  output$ped_code <- renderText({
    ped_code()
  })
  
  run_action <- function(action_id) {
    x <- ped()
    
    if (is.null(x)) {
      console_title("Aucun pedigree chargé")
      console_code("")
      console_output("Sélectionne un pedigree ou clique sur Random pedigree avant d'exécuter une fonction.")
      return()
    }
    
    action <- actions[[action_id]]
    
    code_txt <- tryCatch(
      action$code(input),
      error = function(e) ""
    )
    
    result_txt <- safe_capture(
      action$run(x, input)
    )
    
    console_title(action$label)
    console_code(code_txt)
    console_output(result_txt)
  }
  
  for (action_id in names(actions)) {
    local({
      id <- action_id
      
      observeEvent(input[[id]], {
        run_action(id)
      }, ignoreInit = TRUE)
    })
  }
  
  observeEvent(input$clear_console, {
    clear_console()
  })
  
  output$console_title <- renderText({
    console_title()
  })
  
  output$console_code <- renderText({
    console_code()
  })
  
  output$console_output <- renderText({
    console_output()
  })
}

# Run app ---------------------------------------------------------------------
shinyApp(ui, server)