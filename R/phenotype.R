
# =============================================================================
# Phenotype Palette and Visual Motif Helpers
# =============================================================================

# ── Color palette (72 colors) ─────────────────────────────────────────────────
PHENO_COLOR_PALETTE <- c(
  # Grey (black to white)
  "#000000",
  "#424242",
  "#616161",
  "#757575",
  "#9E9E9E",
  "#BDBDBD",
  "#E0E0E0",
  "#F5F5F5",
  # Red
  "#B71C1C",
  "#C62828",
  "#D32F2F",
  "#E53935",
  "#EF5350",
  "#E57373",
  "#EF9A9A",
  "#FFCDD2",
  # Dark orange
  "#BF360C",
  "#D84315",
  "#E64A19",
  "#FF5722",
  "#FF7043",
  "#FF8A65",
  "#FFAB91",
  "#FFCCBC",
  # Light orange & yellow
  "#E65100",
  "#EF6C00",
  "#F57C00",
  "#FF9800",
  "#FFA726",
  "#FFB74D",
  "#FFCC80",
  "#FFE0B2",
  # Yellow
  "#F57F17",
  "#F9A825",
  "#FBC02D",
  "#FFEB3B",
  "#FFEE58",
  "#FFF176",
  "#FFF59D",
  "#FFF9C4",
  # Green
  "#33691E",
  "#558B2F",
  "#689F38",
  "#7CB342",
  "#8BC34A",
  "#9CCC65",
  "#AED581",
  "#C5E1A5",
  # Blue
  "#01579B",
  "#0277BD",
  "#0288D1",
  "#039BE5",
  "#03A9F4",
  "#29B6F6",
  "#4FC3F7",
  "#B3E5FC",
  # Purple/Indigo
  "#4A148C",
  "#6A1B9A",
  "#7B1FA2",
  "#8E24AA",
  "#9C27B0",
  "#AB47BC",
  "#BA68C8",
  "#E1BEE7",
  # Pink/Magenta
  "#880E4F",
  "#AD1457",
  "#C2185B",
  "#D81B60",
  "#E91E63",
  "#EC407A",
  "#F06292",
  "#F8BBD0"
)

motif_symbols_by_position <- list(
  topleft = c(
    "\u25E4 Black upper left triangle" = "\u25E4",
    "\u25DC Upper left quadrant" = "\u25DC",
    "\u231C Top left corner" = "\u231C",
    "\u25F0 Upper left triangle" = "\u25F0"
  ),
  topright = c(
    "\u25E5 Black upper right triangle" = "\u25E5",
    "\u25DD Upper right quadrant" = "\u25DD",
    "\u231D Top right corner" = "\u231D",
    "\u25F1 Upper right triangle" = "\u25F1"
  ),
  center = c(
    "\U00002B12 Black upper half square" = "\U00002B12",
    "\U00002B13 Black lower half square" = "\U00002B13",
    "\U00002B14 Black upper right triangle square" = "\U00002B14",
    "\U00002B15 Black lower left triangle square" = "\U00002B15",
    "\U00002B16 Lozenge with left half black" = "\U00002B16",
    "\U00002B17 Lozenge with right half black" = "\U00002B17",
    "\U00002B18 White diamond with horizontal bar" = "\U00002B18",
    "\U00002B19 Black diamond with horizontal bar" = "\U00002B19",
    "\U00002B22 Black hexagon" = "\U00002B22",
    "\U00002BCA Black upper half circle" = "\U00002BCA",
    "\U00002BCB Black lower half circle" = "\U00002BCB",
    "\u25A6 Grid" = "\u25A6",
    "\u25A3 White square with small square" = "\u25A3",
    "\u25A4 Horizontal fill" = "\u25A4",
    "\u25A5 Vertical fill" = "\u25A5",
    "\u25A7 Diagonal hatch" = "\u25A7",
    "\u25A8 Reverse hatch" = "\u25A8",
    "\u25A9 Cross hatch" = "\u25A9",
    "\u25AC Black rectangle" = "\u25AC",
    "\u25AD White rectangle" = "\u25AD",
    "\u25AE Black vertical rectangle" = "\u25AE",
    "\u25AF White vertical rectangle" = "\u25AF",
    "\u25B6 Black right triangle" = "\u25B6",
    "\u25B7 White right triangle" = "\u25B7",
    "\u25B8 Small black right triangle" = "\u25B8",
    "\u25B9 Small white right triangle" = "\u25B9",
    "\u25CB Circle" = "\u25CB",
    "\u25CD Circle with vertical fill" = "\u25CD",
    "\u25CE Bullseye" = "\u25CE",
    "\u25C6 Black diamond" = "\u25C6",
    "\u25C7 White diamond" = "\u25C7",
    "\u25C8 Diamond with fill" = "\u25C8",
    "\u25C9 Fisheye" = "\u25C9",
    "\u25D2 Circle with upper half black" = "\u25D2",
    "\u25D3 Circle with lower half black" = "\u25D3",
    "\u25D4 Circle with upper right quadrant black" = "\u25D4",
    "\u25D5 Circle with all but upper left quadrant black" = "\u25D5",
    "\u25D6 Left half black circle" = "\u25D6",
    "\u25D7 Right half black circle" = "\u25D7",
    "\u25D8 Inverse bullet" = "\u25D8",
    "\u25D9 Inverse white circle" = "\u25D9",
    "\u25E2 Black lower right triangle" = "\u25E2",
    "\u25E3 Black lower left triangle" = "\u25E3",
    "\u25E4 Black upper left triangle" = "\u25E4",
    "\u25E5 Black upper right triangle" = "\u25E5",
    "\u25E7 Square with left half black" = "\u25E7",
    "\u25E8 Square with right half black" = "\u25E8",
    "\u25E9 Square with upper left diagonal half black" = "\u25E9",
    "\u25EA Square with lower right diagonal half black" = "\u25EA",
    "\u25F7 White circle with upper left quadrant" = "\u25F7",
    "\u25F8 Upper left triangle" = "\u25F8",
    "\u25F9 Upper right triangle" = "\u25F9",
    "\u25FA Lower left triangle" = "\u25FA",
    "\u2573 Cross" = "\u2573",
    "\u2713 Check mark" = "\u2713",
    "\u2715 Multiplication X" = "\u2715",
    "\u2733 Eight spoke asterisk" = "\u2733",
    "\u2714 Check mark" = "\u2714",
    "\u2734 Eight pointed star" = "\u2734",
    "\u2735 Eight pointed pinwheel star" = "\u2735",
    "\u2736 Six pointed black star" = "\u2736",
    "\u2756 Black diamond minus white X" = "\u2756",
    "\u2691 Black flag" = "\u2691",
    "\u269B Atom symbol" = "\u269B",
    "\u26C6 Rain" = "\u26C6",
    "\u26CC Crossed swords" = "\u26CC",
    "\u2726 Four pointed star" = "\u2726",
    "\U00002BF5 Symbol for transpose" = "\U00002BF5"
  ),
  bottomleft = c(
    "\u25E3 Black lower left triangle" = "\u25E3",
    "\u25DF Lower left quadrant" = "\u25DF",
    "\u231E Bottom left corner" = "\u231E",
    "\u25F2 Lower left triangle" = "\u25F2"
  ),
  bottomright = c(
    "\u25E2 Black lower right triangle" = "\u25E2",
    "\u25DE Lower right quadrant" = "\u25DE",
    "\u231F Bottom right corner" = "\u231F",
    "\u25F3 Lower right triangle" = "\u25F3"
  )
)

motif_position_choices <- c(
  "Top-left" = "topleft",
  "Top-right" = "topright",
  "Center" = "center",
  "Bottom-left" = "bottomleft",
  "Bottom-right" = "bottomright"
)

motif_choices_for_position <- function(position = "center") {
  position <- position %||% "center"
  if (!position %in% names(motif_symbols_by_position)) {
    position <- "center"
  }
  motif_symbols_by_position[[position]]
}

motif_choice_labels <- function(position = "center") {
  choices <- motif_choices_for_position(position)
  setNames(unname(choices), names(choices))
}

motif_picker_ui <- function(selected = NULL, position = "center") {
  choices <- motif_choice_labels(position)
  if (is.null(selected) || !selected %in% unname(choices)) {
    selected <- unname(choices)[1]
  }
  
  items <- lapply(seq_along(choices), function(i) {
    label <- names(choices)[i]
    value <- unname(choices)[i]
    glyph <- value
    
    tags$button(
      type = "button",
      class = paste(
        "motif-picker__item",
        if (identical(value, selected)) "is-active" else ""
      ),
      onclick = sprintf(
        "Shiny.setInputValue('pheno_motif_symbol', '%s', {priority:'event'});",
        value
      ),
      title = label,
      tags$span(class = "motif-picker__glyph", glyph)
    )
  })
  
  div(
    class = "motif-picker",
    p(class = "motif-picker__label", "Symbol"),
    div(class = "motif-picker__list", tagList(items))
  )
}

motif_default_config <- function() {
  list(cex = 2.2, dx = 0, dy = 0)
}

normalize_motif_config <- function(config = NULL) {
  defaults <- motif_default_config()
  if (is.null(config)) {
    return(defaults)
  }
  modifyList(defaults, config)
}

motif_unicode_code <- function(symbol) {
  utf8_values <- utf8ToInt(enc2utf8(symbol))
  if (length(utf8_values) == 0) {
    return("U+0000")
  }
  paste(sprintf("U+%04X", utf8_values), collapse = " ")
}

create_motif_config_preview <- function(symbol, color, config = NULL) {
  cfg <- normalize_motif_config(config)
  box_size <- 74
  center <- box_size / 2
  
  tags$div(
    class = "motif-config-preview-shell",
    tags$div(
      class = "motif-config-preview-box",
      tags$div(
        style = sprintf(
          paste(
            "position:absolute;",
            "left:%spx;",
            "top:%spx;",
            "transform:translate(-50%%,-50%%);",
            "font-size:%spx;",
            "line-height:1;",
            "font-weight:700;",
            "color:%s;",
            "font-family:'Noto Sans Symbols 2','Segoe UI Symbol','Apple Symbols','Arial Unicode MS',sans-serif;"
          ),
          center + (box_size * cfg$dx),
          center + (box_size * cfg$dy),
          26 * cfg$cex / 2.2,
          color
        ),
        symbol
      )
    )
  )
}

# ── Draw motif overlay on plot ─────────────────────────
draw_motif_symbol <- function(
    cx,
    cy,
    boxw,
    boxh,
    symbol,
    color,
    position,
    config = NULL,
    cex = 2.2
) {
  cfg <- normalize_motif_config(config)
  offset <- switch(position,
                   "topleft" = list(dx = -boxw * 0.3, dy = boxh * -0.3),
                   "topright" = list(dx = boxw * 0.3, dy = boxh * -0.3),
                   "center" = list(dx = -boxw * 0.0, dy = boxh * 0.0),
                   "bottomleft" = list(dx = -boxw * 0.3, dy = boxh * 0.3),
                   "bottomright" = list(dx = boxw * 0.3, dy = boxh * 0.3),
                   list(dx = 0, dy = boxh * 0.5)
  )
  text(
    cx + offset$dx + (boxw * cfg$dx),
    cy + offset$dy + (boxh * cfg$dy),
    labels = symbol,
    col = color,
    cex = cfg$cex %||% cex,
    xpd = NA
  )
}

# ── Format text annotations with style ────────────────
formatAnnot <- function(textAnnot, cex = 1.0, font = 2, col = "#1976D2") {
  if (is.null(textAnnot)) {
    return(NULL)
  }
  lapply(textAnnot, function(b) list(b, cex = cex, font = font, col = col))
}

# ── Validate numeric input ────────────────────────────
checknum <- function(x, name, min = -Inf, max = Inf) {
  if (is.null(x) || is.na(x) || !is.numeric(x)) {
    showNotification(paste0(name, " must be numeric"), type = "error")
    return(NULL)
  }
  if (x < min || x > max) {
    showNotification(
      paste0(name, " must be between ", min, " and ", max),
      type = "error"
    )
    return(NULL)
  }
  x
}

# ── Preview shape helper ──────────────────────────────
createPreviewShape <- function(
    fill_color,
    border_color,
    border_style,
    is_hatched,
    hatch_color = NULL
) {
  css_border <- switch(border_style,
                       "dashed" = "dashed",
                       "dotted" = "dotted",
                       "dotdash" = "dashed",
                       "solid"
  )
  
  bg_style <- if (is_hatched) {
    if (!is.null(hatch_color)) {
      sprintf(
        "background-color: #ffffff;
       background-image: repeating-linear-gradient(
         45deg,
         transparent,
         transparent 7px,
         %s 7px,
         %s 9px
       );",
        hatch_color,
        hatch_color
      )
    } else {
      sprintf(
        "background-color: %s;
       background-image: repeating-linear-gradient(
         45deg,
         transparent,
         transparent 7px,
         rgba(0, 0, 0, 0.3) 7px,
         rgba(0, 0, 0, 0.3) 9px
       );",
        fill_color
      )
    }
  } else {
    sprintf("background-color: %s;", fill_color)
  }
  
  tags$div(
    style = sprintf(
      "width: 100px;
       height: 100px;
       border-radius: 12px;
       border: 4px %s %s;
       margin: 0 auto;
       box-shadow: 0 4px 6px rgba(0,0,0,0.1);
       %s",
      css_border,
      border_color,
      bg_style
    )
  )
}
createPreviewMotif <- function(symbol, color) {
  box_size <- 72
  center <- box_size / 2
  motif_size <- round(box_size * 0.30)
  
  tags$div(
    style = sprintf(
      "position: relative; width: %spx; height: %spx; margin: 0 auto;",
      box_size,
      box_size
    ),
    tags$div(
      style = "position: absolute; inset: 0; border-radius: 10px; border: 2px solid #475569; background: #ffffff;"
    ),
    tags$div(
      class = "motif-preview-symbol",
      style = sprintf(
        paste(
          "position: absolute;",
          "width: 0;",
          "height: 0;",
          "display: flex;",
          "align-items: center;",
          "justify-content: center;",
          "line-height: 1;",
          "color: %s;",
          "font-size: %spx;",
          "font-weight: 700;",
          "left: %spx;",
          "top: %spx;",
          "transform: translate(-50%%, -50%%);"
        ),
        color,
        motif_size,
        center,
        center
      ),
      symbol
    )
  )
}