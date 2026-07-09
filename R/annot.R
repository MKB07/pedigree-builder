
# =============================================================================
# Plot Annotation and Rendering Helpers
# =============================================================================

# ── build_hover_html: build detailed HTML tooltip for privacy mode ────────────
build_hover_html <- function(
    id,
    ped_data,
    adopted_ids_vec = character(0),
    deceased_ids_vec = character(0)
) {
  row <- ped_data[ped_data$id == id, , drop = FALSE]
  if (nrow(row) == 0) {
    return(paste0("<strong>ID: ", id, "</strong>"))
  }
  
  get_val <- function(r, col) {
    if (col %in% names(r)) as.character(r[[col]][1] %||% "") else ""
  }
  
  ln <- get_val(row, "last_name")
  fn <- get_val(row, "first_name")
  dob <- get_val(row, "date_of_birth")
  dod <- get_val(row, "date_of_death")
  age <- get_val(row, "age")
  com <- get_val(row, "comments")
  deceased <- id %in% deceased_ids_vec
  is_adopted <- id %in% adopted_ids_vec
  
  html_parts <- c()
  
  # ID line
  id_line <- paste0("<strong style='color:#93c5fd;'>ID: ", id, "</strong>")
  if (is_adopted) {
    id_line <- paste0(
      id_line,
      " <span style='background:#f59e0b;color:white;padding:1px 6px;",
      "border-radius:4px;font-size:10px;margin-left:6px;'>ADOPTED</span>"
    )
  }
  html_parts <- c(html_parts, id_line)
  
  # Name
  if (nzchar(ln) || nzchar(fn)) {
    name_parts <- c()
    if (nzchar(ln)) {
      name_parts <- c(name_parts, toupper(trimws(ln)))
    }
    if (nzchar(fn)) {
      name_parts <- c(name_parts, tools::toTitleCase(trimws(fn)))
    }
    html_parts <- c(
      html_parts,
      paste0(
        "<span style='color:#60a5fa;font-weight:300;'>",
        paste(name_parts, collapse = " "),
        "</span>"
      )
    )
  }
  
  # Dates
  date_parts <- c()
  if (nzchar(dob)) {
    dob_fmt <- format_date(dob)
    if (nzchar(dob_fmt)) {
      date_parts <- c(
        date_parts,
        paste0("\u00B0", gsub("/", "-", dob_fmt))
      )
    }
  }
  if (deceased && nzchar(dod)) {
    dod_fmt <- format_date(dod)
    if (nzchar(dod_fmt)) {
      date_parts <- c(
        date_parts,
        paste0("\u2020", gsub("/", "-", dod_fmt))
      )
    }
  }
  if (length(date_parts) > 0) {
    html_parts <- c(
      html_parts,
      paste0(
        "<span style='color:#f9a8d4;'>",
        paste(date_parts, collapse = " \u2014 "),
        "</span>"
      )
    )
  }
  
  # Status
  status_parts <- c()
  if (deceased && length(date_parts) == 0) {
    status_parts <- c(status_parts, "\u2020")
  }
  if (nzchar(age)) {
    age_num <- trimws(gsub("[^0-9]", "", age))
    if (nzchar(age_num)) {
      status_parts <- c(status_parts, paste(age_num, "years"))
    }
  }
  if (length(status_parts) > 0) {
    html_parts <- c(
      html_parts,
      paste0(
        "<span style='color:#f9a8d4;font-style:italic;'>",
        paste(status_parts, collapse = " \u2022 "),
        "</span>"
      )
    )
  }
  
  # Comments
  if (nzchar(com)) {
    com_clean <- trimws(com)
    if (nchar(com_clean) > 50) {
      com_clean <- paste0(substr(com_clean, 1, 47), "...")
    }
    html_parts <- c(
      html_parts,
      paste0(
        "<span style='color:#86efac;font-size:11px;'>",
        htmltools::htmlEscape(com_clean),
        "</span>"
      )
    )
  }
  
  paste(html_parts, collapse = "<br/>")
}

# ── draw_title_box: blue framed title on the pedigree plot ────────────────────
draw_title_box <- function(title_text) {
  if (is.null(title_text) || !nzchar(title_text)) {
    return(invisible(NULL))
  }
  usr <- par("usr")
  title_x <- mean(usr[1:2])
  title_y <- usr[4] + (usr[4] - usr[3]) * 0.08
  text_width <- strwidth(title_text, cex = 1.2, font = 2)
  text_height <- strheight(title_text, cex = 1.2, font = 2)
  pad_x <- text_width * 0.2
  pad_y <- text_height * 0.6
  rect(
    title_x - text_width / 2 - pad_x,
    title_y - text_height / 2 - pad_y,
    title_x + text_width / 2 + pad_x,
    title_y + text_height / 2 + pad_y,
    col = "#D6EAF8",
    border = "#2E86C1",
    lwd = 2,
    xpd = NA
  )
  text(
    title_x,
    title_y,
    title_text,
    cex = 1,
    col = "#1A5490",
    font = 2,
    xpd = NA
  )
  invisible(NULL)
}
safe_round3 <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NA_real_)
  }
  round(as.numeric(x), 3)
}

format_stat_value <- function(x, zero_as_int = TRUE) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("\u2014")
  }
  if (zero_as_int && isTRUE(all.equal(as.numeric(x), 0))) {
    return("0")
  }
  as.character(round(as.numeric(x), 3))
}

ped_neighbors <- function(ped, id) {
  out <- character(0)
  
  fa <- tryCatch(pedtools::father(ped, id, internal = FALSE), error = function(e) NA_character_)
  mo <- tryCatch(pedtools::mother(ped, id, internal = FALSE), error = function(e) NA_character_)
  ch <- tryCatch(pedtools::children(ped, id), error = function(e) character(0))
  
  if (length(fa) == 1 && !is.na(fa) && fa != "0" && nzchar(fa)) out <- c(out, as.character(fa))
  if (length(mo) == 1 && !is.na(mo) && mo != "0" && nzchar(mo)) out <- c(out, as.character(mo))
  if (length(ch) > 0) out <- c(out, as.character(ch))
  
  unique(out)
}

ped_degree <- function(ped, from_id, to_id) {
  from_id <- as.character(from_id)[1]
  to_id <- as.character(to_id)[1]
  
  if (!nzchar(from_id) || !nzchar(to_id)) {
    return(NA_integer_)
  }
  if (identical(from_id, to_id)) {
    return(0L)
  }
  
  ids_all <- as.character(labels(ped))
  if (!(from_id %in% ids_all) || !(to_id %in% ids_all)) {
    return(NA_integer_)
  }
  
  visited <- setNames(rep(FALSE, length(ids_all)), ids_all)
  dist <- setNames(rep(NA_integer_, length(ids_all)), ids_all)
  
  queue <- from_id
  visited[from_id] <- TRUE
  dist[from_id] <- 0L
  
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    
    neigh <- ped_neighbors(ped, current)
    if (length(neigh) == 0) next
    
    for (nb in neigh) {
      if (!isTRUE(visited[nb])) {
        visited[nb] <- TRUE
        dist[nb] <- dist[current] + 1L
        
        if (identical(nb, to_id)) {
          return(dist[nb])
        }
        queue <- c(queue, nb)
      }
    }
  }
  
  NA_integer_
}

compute_selected_stats <- function(ped, target_id, selected_id, f_vals = NULL) {
  target_id <- as.character(target_id)[1]
  
  f_val <- NA_real_
  if (!is.null(f_vals) && target_id %in% names(f_vals)) {
    f_val <- as.numeric(f_vals[[target_id]])
  }
  
  out <- list(
    f = f_val,
    deg = NA_integer_,
    r_pct = NA_real_
  )
  
  if (length(selected_id) == 0 || !nzchar(selected_id[1])) {
    return(out)
  }
  
  base_id <- as.character(selected_id[1])
  
  if (!(base_id %in% labels(ped)) || !(target_id %in% labels(ped))) {
    return(out)
  }
  
  if (identical(base_id, target_id)) {
    out$deg <- 0L
    out$r_pct <- 100
    return(out)
  }
  
  phi_val <- tryCatch(
    ribd::kinship(ped, ids = c(base_id, target_id), Xchrom = FALSE),
    error = function(e) NA_real_
  )
  
  out$r_pct <- if (is.na(phi_val)) NA_real_ else 100 * 2 * as.numeric(phi_val)
  out$deg <- ped_degree(ped, base_id, target_id)
  
  out
}
# ── prepare_annotation_data: build text annotations per individual ────────────
prepare_annotation_data <- function(
    ped,
    ped_data,
    deceased_ids_vec = character(0),
    infertility_ids_vec = character(0),
    stats_mode = FALSE,
    selected_id = character(0)
) {
  ids <- labels(ped)
  annotations <- list()
  
  # Pre-compute genetic stats if stats_mode is active
  f_vals <- NULL
  if (stats_mode) {
    f_vals <- tryCatch(
      ribd::inbreeding(ped, ids = ids, Xchrom = FALSE),
      error = function(e) {
        message("[Stats] inbreeding error: ", e$message)
        NULL
      }
    )
    if (!is.null(f_vals)) {
      names(f_vals) <- ids
    }
  }
  has_selected <- length(selected_id) > 0 && nzchar(selected_id[1])
  for (id in ids) {
    row <- ped_data[ped_data$id == id, , drop = FALSE]
    texts <- list()
    
    if (nrow(row) == 0) {
      # Even with no pedData row, add stats if active
      if (stats_mode) {
        gs <- compute_selected_stats(ped, id, selected_id, f_vals = f_vals)
        
        texts[[length(texts) + 1]] <- list(
          text = paste0("f=", format_stat_value(gs$f)),
          pos = 1, offset = 2.9, col = "#8E24AA", cex = 0.70, font = 1
        )
        
        if (has_selected) {
          texts[[length(texts) + 1]] <- list(
            text = paste0("deg=", if (is.na(gs$deg)) "\u2014" else gs$deg),
            pos = 4, offset = 0.9, col = "#00897B", cex = 0.70, font = 1, dy = 0.18
          )
          texts[[length(texts) + 1]] <- list(
            text = paste0("%R=", format_stat_value(gs$r_pct)),
            pos = 2, offset = 0.9, col = "#E65100", cex = 0.70, font = 1, dy = 0.18
          )
        }
      }
      annotations[[as.character(id)]] <- list(
        id = as.character(id),
        texts = texts
      )
      next
    }
    
    ln <- as.character(row$last_name[1] %||% "")
    fn <- as.character(row$first_name[1] %||% "")
    dob <- as.character(row$date_of_birth[1] %||% "")
    dod <- as.character(row$date_of_death[1] %||% "")
    age <- as.character(row$age[1] %||% "")
    age_unit <- if ("age_unit" %in% names(row)) {
      as.character(row$age_unit[1] %||% "years")
    } else {
      "years"
    }
    deceased <- id %in% deceased_ids_vec
    
    current_offset_below <- 2.9
    
    # Infertility (first, above name)
    if (id %in% infertility_ids_vec) {
      texts[[length(texts) + 1]] <- list(
        text = "\u23C8",
        pos = 1,
        offset = current_offset_below,
        col = "#4C4C4C",
        cex = 2,
        font = 1
      )
      current_offset_below <- current_offset_below + 0.2
    }
    
    # Name (below symbol)
    if (nzchar(ln) || nzchar(fn)) {
      name_parts <- c()
      if (nzchar(ln)) {
        name_parts <- c(name_parts, toupper(trimws(ln)))
      }
      if (nzchar(fn)) {
        name_parts <- c(name_parts, tools::toTitleCase(trimws(fn)))
      }
      texts[[length(texts) + 1]] <- list(
        text = paste(name_parts, collapse = " "),
        pos = 1,
        offset = current_offset_below,
        col = "#1976D2",
        cex = 1,
        font = 1
      )
      current_offset_below <- current_offset_below + 0.8
    }
    
    # Dates (below)
    if (nzchar(dob) || (deceased && nzchar(dod))) {
      date_parts <- c()
      if (nzchar(dob)) {
        dob_fmt <- format_date(dob)
        if (nzchar(dob_fmt)) {
          date_parts <- c(date_parts, gsub("/", "-", dob_fmt))
        }
      }
      if (deceased && nzchar(dod)) {
        dod_fmt <- format_date(dod)
        if (nzchar(dod_fmt)) {
          date_parts <- c(date_parts, gsub("/", "-", dod_fmt))
        }
      }
      if (length(date_parts) > 0) {
        texts[[length(texts) + 1]] <- list(
          text = paste(date_parts, collapse = " \u2014 "),
          pos = 1,
          offset = current_offset_below,
          col = "#C2185B",
          cex = 0.9,
          font = 1
        )
        current_offset_below <- current_offset_below + 0.8
      }
    }
    
    # Status: deceased symbol + age
    status_parts <- c()
    if (deceased) {
      status_parts <- c(status_parts, "\u2020")
    }
    if (nzchar(age)) {
      age_display <- format_age_text(age, age_unit)
      if (nzchar(age_display)) {
        status_parts <- c(status_parts, age_display)
      }
    }
    if (length(status_parts) > 0) {
      texts[[length(texts) + 1]] <- list(
        text = paste(status_parts, collapse = " \u2022 "),
        pos = 1,
        offset = current_offset_below,
        col = "#C2185B",
        cex = 0.9,
        font = 3
      )
      current_offset_below <- current_offset_below + 0.8
    }
    
    # Comments (below)
    comment <- if ("comments" %in% names(row)) {
      as.character(row$comments[1] %||% "")
    } else {
      ""
    }
    if (nzchar(trimws(comment))) {
      texts[[length(texts) + 1]] <- list(
        text = trimws(comment),
        pos = 1,
        offset = current_offset_below,
        col = "#546E7A",
        cex = 0.90,
        font = 3
      )
      current_offset_below <- current_offset_below + 0.8
    }
    
    # Genetic stats: show only f= on right side of each individual
    if (stats_mode) {
      gs <- compute_selected_stats(ped, id, selected_id, f_vals = f_vals)
      
      texts[[length(texts) + 1]] <- list(
        text = paste0("f=", format_stat_value(gs$f)),
        pos = 4,
        offset = 1.0,
        col = "#8E24AA",
        cex = 0.90,
        font = 1,
        dy = 0
      )
      
      if (has_selected) {
        texts[[length(texts) + 1]] <- list(
          text = paste0("deg=", if (is.na(gs$deg)) "\u2014" else gs$deg),
          pos = 4,
          offset = 1.0,
          col = "#00897B",
          cex = 0.78,
          font = 1,
          dy = 0.35
        )
        
        texts[[length(texts) + 1]] <- list(
          text = paste0("%R=", format_stat_value(gs$r_pct)),
          pos = 4,
          offset = 1.0,
          col = "#E65100",
          cex = 0.78,
          font = 1,
          dy = -0.35
        )
      }
    }
    
    annotations[[as.character(id)]] <- list(
      id = as.character(id),
      texts = texts
    )
  }
  return(annotations)
}

# ── draw_custom_annotations: render text on plot ──────────────────────────────
draw_custom_annotations <- function(
    alignment,
    annotations,
    scaling,
    ped_labels = NULL
) {
  xall <- alignment$xall
  yall <- alignment$yall
  plotord <- alignment$plotord
  boxh <- scaling$boxh
  boxw <- scaling$boxw
  if (is.null(ped_labels)) {
    ped_labels <- labels(alignment$ped)
  }
  
  for (i in seq_along(plotord)) {
    id_char <- as.character(ped_labels[plotord[i]])
    if (!id_char %in% names(annotations)) {
      next
    }
    annot <- annotations[[id_char]]
    if (length(annot$texts) == 0) {
      next
    }
    
    x_center <- xall[i]
    y_center <- yall[i] + boxh / 2
    
    for (txt_info in annot$texts) {
      dy <- if (!is.null(txt_info$dy)) txt_info$dy * boxh else 0
      text(
        x = x_center,
        y = y_center + dy,
        labels = txt_info$text,
        pos = txt_info$pos,
        offset = txt_info$offset,
        col = txt_info$col,
        cex = txt_info$cex,
        font = txt_info$font,
        xpd = NA
      )
    }
  }
}

# ── render_pedigree_to_device: reusable pedigree rendering function ───────────
#    Used by the Shiny plot output and export flows. It composes the pedtools
#    drawing, custom clinical annotations, status overlays and phenotype motifs
#    in a single deterministic order.
render_pedigree_to_device <- function(
    al,
    an,
    sc,
    ids,
    title_text = "",
    adopted = character(0),
    afab = character(0),
    amab = character(0),
    umab = character(0),
    infertility = character(0),
    ped_obj = NULL,
    ped_data = NULL,
    deceased = character(0),
    classic_mode = TRUE,
    stats_mode = FALSE,
    selected_id = character(0),
    phenotypes_list = list(),
    phenotypes_assign = list(),
    motif_configs = list(),
    show_motifs = TRUE,
    show_stats = FALSE
) {
  drawPed(al, annotation = an, scaling = sc)
  draw_title_box(title_text)
  
  for (i in seq_along(al$plotord)) {
    id_num <- al$plotord[i]
    id <- as.character(ids[id_num])
    x_center <- al$xall[i]
    y_center <- al$yall[i] + sc$boxh / 2
    
    if (id %in% adopted) {
      text(
        x_center,
        y_center,
        "[",
        pos = 2,
        offset = 1,
        col = "#4C4C4C",
        cex = 2.85,
        font = 5,
        xpd = NA
      )
      text(
        x_center,
        y_center,
        "]",
        pos = 4,
        offset = 1,
        col = "#4C4C4C",
        cex = 2.85,
        font = 5,
        xpd = NA
      )
    }
    
    aab <- ""
    if (id %in% afab) {
      aab <- "AFAB"
    } else if (id %in% amab) {
      aab <- "AMAB"
    } else if (id %in% umab) {
      aab <- "UMAB"
    }
    if (nzchar(aab)) {
      text(
        x_center,
        y_center,
        aab,
        pos = 3,
        offset = 1,
        col = "#D32F2F",
        cex = 0.85,
        font = 3,
        xpd = NA
      )
    }
  }
  
  if (classic_mode) {
    if (!is.null(ped_data) && nrow(ped_data) > 0) {
      pd_annotations <- prepare_annotation_data(
        ped_obj,
        ped_data,
        deceased_ids_vec = deceased,
        infertility_ids_vec = infertility,
        stats_mode = show_stats,
        selected_id = selected_id
      )
      draw_custom_annotations(al, pd_annotations, sc, ids)
    }
  } else if (show_stats) {
    if (!is.null(ped_data) && nrow(ped_data) > 0) {
      stats_annotations <- prepare_annotation_data(
        ped_obj,
        ped_data,
        deceased_ids_vec = deceased,
        infertility_ids_vec = infertility,
        stats_mode = TRUE,
        selected_id = selected_id
      )
      draw_custom_annotations(al, stats_annotations, sc, ids)
    }
  }
  
  if (show_motifs) {
    for (nm in names(phenotypes_list)) {
      ph <- phenotypes_list[[nm]]
      if (!identical(ph$type, "motif")) {
        next
      }
      assigned <- phenotypes_assign[[nm]] %||% character(0)
      if (length(assigned) == 0) {
        next
      }
      for (id in assigned) {
        idx_in_plotord <- match(match(id, ids), al$plotord)
        if (is.na(idx_in_plotord)) {
          next
        }
        cx <- al$xall[idx_in_plotord]
        cy <- al$yall[idx_in_plotord] + sc$boxh / 2
        draw_motif_symbol(
          cx,
          cy,
          sc$boxw,
          sc$boxh,
          ph$symbol,
          ph$motif_color,
          ph$position,
          config = motif_configs[[ph$symbol]]
        )
      }
    }
  }
  
  invisible(NULL)
}
