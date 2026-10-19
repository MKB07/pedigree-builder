# Archived R development file
# Original path: 🧩 Versions_Support/Support de test/Version beta TEST/R/fonctions.R
# Original created: 2025-11-17 15:13:33
# Original modified: 2025-11-17 19:01:48
# Archive rationale: Shared helper functions from the beta modular experiment.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

pedigree_list <- list(
  "Trio"                 = function() pedtools::nuclearPed(1),
  "Siblings (2)"         = function() pedtools::nuclearPed(2),
  "Sibship (3, M-F-M)"   = function() pedtools::nuclearPed(3, sex = c(1, 2, 1)),
  "Half-sibs, maternal"  = function() pedtools::halfSibPed(1, 1, type = "maternal"),
  "Half-sibs, paternal"  = function() pedtools::halfSibPed(1, 1),
  "Avuncular"            = function() pedtools::avuncularPed(),
  "Grandparent"          = function() pedtools::ancestralPed(2),
  "1st cousins (sym.)"   = function() pedtools::cousinPed(1, symmetric = TRUE)
)

COLOR_PALETTE <- list(
  row1 = c("#000000", "#212121", "#424242", "#616161", "#757575", "#9E9E9E", "#BDBDBD", "#E0E0E0"),
  row2 = c("#7F0000", "#B71C1C", "#C62828", "#D32F2F", "#E53935", "#EF5350", "#E57373", "#FFCDD2"),
  row3 = c("#BF360C", "#D84315", "#E64A19", "#F4511E", "#FF5722", "#FF7043", "#FF8A65", "#FFCCBC"),
  row4 = c("#FF6F00", "#FF8F00", "#FFA000", "#FFB300", "#FFC107", "#FFD54F", "#FFE082", "#FFECB3"),
  row5 = c("#1B5E20", "#2E7D32", "#388E3C", "#43A047", "#4CAF50", "#66BB6A", "#81C784", "#C8E6C9"),
  row6 = c("#0D47A1", "#1565C0", "#1976D2", "#1E88E5", "#2196F3", "#42A5F5", "#64B5F6", "#BBDEFB"),
  row7 = c("#4A148C", "#6A1B9A", "#7B1FA2", "#8E24AA", "#9C27B0", "#AB47BC", "#BA68C8", "#E1BEE7"),
  row8 = c("#880E4F", "#AD1457", "#C2185B", "#D81B60", "#E91E63", "#EC407A", "#F06292", "#F8BBD0")
)

ALL_COLORS <- unlist(COLOR_PALETTE, use.names = FALSE)

getPaletteColors <- function(type = "material", n = 8, palette_name = NULL) {
  tryCatch(
    {
      if (type == "material") {
        if (n <= length(ALL_COLORS)) {
          return(ALL_COLORS[1:n])
        } else {
          return(rep(ALL_COLORS, ceiling(n / length(ALL_COLORS)))[1:n])
        }
      } else if (type == "brewer") {
        palette_name <- palette_name %||% "Set3"
        available_palettes <- rownames(RColorBrewer::brewer.pal.info)

        if (!palette_name %in% available_palettes) {
          warning(sprintf("Palette '%s' not found, using 'Set3'", palette_name))
          palette_name <- "Set3"
        }

        max_n <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]

        if (n <= max_n) {
          return(RColorBrewer::brewer.pal(n, palette_name))
        } else {
          base_pal <- RColorBrewer::brewer.pal(max_n, palette_name)
          return(rep(base_pal, ceiling(n / max_n))[1:n])
        }
      } else if (type == "viridis") {
        option <- palette_name %||% "D"

        if (!option %in% c("A", "B", "C", "D", "E")) {
          warning(sprintf("Option '%s' not valid, using 'D'", option))
          option <- "D"
        }

        return(viridis(n, option = option))
      } else {
        warning(sprintf("Type '%s' not recognized, using 'material'", type))
        return(ALL_COLORS[1:n])
      }
    },
    error = function(e) {
      warning(sprintf("Error getting palette: %s", e$message))
      return(ALL_COLORS[1:min(n, length(ALL_COLORS))])
    }
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

stop2 <- function(...) {
  args <- lapply(list(...), toString)
  args <- append(args, list(call. = FALSE))
  do.call(stop, args)
}

sanitize_title <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }
  tryCatch(
    {
      x <- as.character(x)
      x <- gsub("[\r\n\t]+", " ", x)
      x <- gsub("\\s{2,}", " ", x)
      x <- trimws(x)
      if (nchar(x) > 200) x <- substr(x, 1, 200)
      return(x)
    },
    error = function(e) {
      return("")
    }
  )
}

validatePedigree <- function(ped) {
  if (is.null(ped)) {
    return(list(valid = FALSE, message = "Pedigree is NULL"))
  }

  tryCatch(
    {
      if (!inherits(ped, "ped")) {
        return(list(valid = FALSE, message = "Not a valid ped object"))
      }

      if (length(labels(ped)) == 0) {
        return(list(valid = FALSE, message = "Pedigree has no individuals"))
      }

      all_ids <- labels(ped)
      for (id in all_ids) {
        sex <- pedtools::getSex(ped, id)
        children_as_father <- which(ped$FIDX == match(id, all_ids))
        children_as_mother <- which(ped$MIDX == match(id, all_ids))

        if (length(children_as_father) > 0 && sex == 2) {
          return(list(valid = FALSE, message = sprintf("Individual %s is female but is a father", id)))
        }

        if (length(children_as_mother) > 0 && sex == 1) {
          return(list(valid = FALSE, message = sprintf("Individual %s is male but is a mother", id)))
        }
      }

      return(list(valid = TRUE, message = "Pedigree is valid"))
    },
    error = function(e) {
      return(list(valid = FALSE, message = paste("Validation error:", e$message)))
    }
  )
}

format_date <- function(x) {
  if (is.null(x) || is.na(x) || x == "" || x == "NA") {
    return("")
  }

  tryCatch(
    {
      if (inherits(x, "Date")) {
        return(format(x, "%d/%m/%Y"))
      }

      date_obj <- as.Date(x, format = "%d-%m-%Y")
      if (is.na(date_obj)) {
        return("")
      }

      return(format(date_obj, "%d/%m/%Y"))
    },
    error = function(e) {
      return("")
    }
  )
}

safe_format_date <- function(date_input) {
  if (is.null(date_input) || is.na(date_input)) {
    return("")
  }

  tryCatch(
    {
      if (inherits(date_input, "Date")) {
        return(format(date_input, "%d-%m-%Y"))
      }

      if (is.character(date_input) && nzchar(date_input)) {
        return(date_input)
      }

      return("")
    },
    error = function(e) {
      return("")
    }
  )
}

calculate_age_text <- function(dob, dod = NA) {
  fmt <- "%d-%m-%Y"

  if (!nzchar(dob)) {
    return("")
  }

  d0 <- tryCatch(as.Date(dob, fmt), error = function(e) NA)
  if (is.na(d0)) {
    return("")
  }

  d1 <- if (nzchar(dod)) {
    tryCatch(as.Date(dod, fmt), error = function(e) Sys.Date())
  } else {
    Sys.Date()
  }

  if (is.na(d1) || d1 < d0) {
    return("")
  }

  yrs <- as.integer(floor(as.numeric(difftime(d1, d0, units = "days")) / 365.25))

  if (yrs < 2) paste0(yrs, " year") else paste0(yrs, " years")
}

relabel_generations <- function(p) {
  rf <- names(formals(pedtools::relabel))
  if ("new" %in% rf) {
    pedtools::relabel(p, new = "generations")
  } else {
    pedtools::relabel(p, "generations")
  }
}

improve_layout <- function(ped) {
  ped <- pedtools::parentsBeforeChildren(ped)
  ped <- relabel_generations(ped)
  return(ped)
}

build_labs <- function(p) {
  ids <- labels(p)
  labs <- ids
  names(labs) <- ids
  return(labs)
}

makePedData <- function(ped) {
  if (is.null(ped)) {
    return(NULL)
  }

  df <- as.data.frame(ped, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))

  extra_cols <- c(
    "first_name", "last_name", "date_of_birth", "deceased",
    "date_of_death", "age", "comments", "assigned_at_birth"
  )

  for (col in extra_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- if (col == "deceased") FALSE else ""
    }
  }

  return(df)
}

getPartners <- function(ped, id) {
  if (is.null(ped)) {
    return(character(0))
  }

  all_children <- tryCatch(
    {
      id_idx <- match(id, labels(ped))
      if (is.na(id_idx)) {
        return(character(0))
      }

      children_as_father <- which(ped$FIDX == id_idx)
      children_as_mother <- which(ped$MIDX == id_idx)

      unique(c(children_as_father, children_as_mother))
    },
    error = function(e) {
      return(integer(0))
    }
  )

  if (length(all_children) == 0) {
    return(character(0))
  }

  partners <- character(0)

  for (child_idx in all_children) {
    fid_idx <- ped$FIDX[child_idx]
    mid_idx <- ped$MIDX[child_idx]

    if (fid_idx > 0 && labels(ped)[fid_idx] != id) {
      partners <- c(partners, labels(ped)[fid_idx])
    }
    if (mid_idx > 0 && labels(ped)[mid_idx] != id) {
      partners <- c(partners, labels(ped)[mid_idx])
    }
  }

  unique(partners)
}

getSiblings <- function(ped, id) {
  parents <- tryCatch(
    pedtools::parents(ped, id, internal = FALSE),
    error = function(e) c(NA, NA)
  )

  if (length(parents) != 2 || any(is.na(parents))) {
    return(character(0))
  }

  all_children <- tryCatch(
    pedtools::children(ped, parents[1]),
    error = function(e) character(0)
  )

  siblings <- setdiff(all_children, id)
  return(siblings)
}

parent_fun <- function(id, df) {
  if (is.na(id) || id == "" || id == "NA") {
    return("")
  }

  p <- df[df$id == id, , drop = FALSE]
  if (nrow(p) == 0) {
    return(as.character(id))
  }

  paste(
    id,
    if ("last_name" %in% names(p)) toupper(p$last_name) else "",
    if ("first_name" %in% names(p)) tools::toTitleCase(p$first_name) else ""
  )
}

canChangeSex <- function(ped, id, new_sex) {
  current_sex <- pedtools::getSex(ped, id)

  if (current_sex == new_sex) {
    return(list(can = TRUE, reason = NULL))
  }

  children_ids <- tryCatch(
    pedtools::children(ped, id),
    error = function(e) character(0)
  )

  if (length(children_ids) > 0) {
    if (current_sex %in% c(1, 2) && new_sex %in% c(1, 2) && current_sex != new_sex) {
      return(list(
        can = FALSE,
        reason = sprintf("Cannot change: individual is parent of %d child(ren)", length(children_ids))
      ))
    }
  }

  return(list(can = TRUE, reason = NULL))
}

safeSexChange <- function(ped, id, new_sex) {
  check <- canChangeSex(ped, id, new_sex)

  if (!check$can) {
    stop(check$reason)
  }

  new_ped <- tryCatch(
    {
      pedtools::setSex(ped, ids = id, sex = new_sex)
    },
    error = function(e) {
      stop(paste("Error changing sex:", e$message))
    }
  )

  return(new_ped)
}

addFullSiblings <- function(ped, id, n_siblings = 1, sex = 1, verbose = TRUE) {
  if (!pedtools::is.ped(ped)) stop("Input must be a ped object")
  if (length(id) != 1) stop("Please select exactly one individual")
  if (!id %in% labels(ped)) stop(sprintf("Individual '%s' not found", id))

  parents <- tryCatch(
    pedtools::parents(ped, id, internal = FALSE),
    error = function(e) c(NA, NA)
  )

  if (length(parents) != 2 || any(is.na(parents))) {
    if (verbose) message(sprintf("Individual '%s' has no parents. Creating parents...", id))

    ped <- tryCatch(
      {
        pedtools::addParents(ped, id, verbose = FALSE)
      },
      error = function(e) {
        stop(sprintf("Unable to add parents: %s", e$message))
      }
    )

    parents <- pedtools::parents(ped, id, internal = FALSE)
  }

  if (length(parents) != 2 || any(is.na(parents))) {
    stop("Unable to determine both parents")
  }

  father_id <- parents[1]
  mother_id <- parents[2]

  new_ped <- tryCatch(
    {
      pedtools::addChildren(
        ped,
        father = father_id,
        mother = mother_id,
        nch = n_siblings,
        sex = sex,
        verbose = FALSE
      )
    },
    error = function(e) {
      stop(sprintf("Error adding siblings: %s", e$message))
    }
  )

  return(new_ped)
}

addHalfSiblings <- function(ped, id, n_siblings = 1, sex = 1,
                            shared_parent = c("father", "mother"), verbose = TRUE) {
  if (!pedtools::is.ped(ped)) stop("Input must be a ped object")
  if (length(id) != 1) stop("Please select exactly one individual")
  if (!id %in% labels(ped)) stop(sprintf("Individual '%s' not found", id))

  shared_parent <- match.arg(shared_parent)

  parents <- tryCatch(
    pedtools::parents(ped, id, internal = FALSE),
    error = function(e) c(NA, NA)
  )

  if (length(parents) != 2 || any(is.na(parents))) {
    if (verbose) message(sprintf("Individual '%s' has no parents. Creating parents...", id))

    ped <- tryCatch(
      {
        pedtools::addParents(ped, id, verbose = FALSE)
      },
      error = function(e) {
        stop(sprintf("Unable to add parents: %s", e$message))
      }
    )

    parents <- pedtools::parents(ped, id, internal = FALSE)
  }

  if (length(parents) != 2 || any(is.na(parents))) {
    stop("Unable to determine both parents")
  }

  father_id <- parents[1]
  mother_id <- parents[2]

  new_ped <- tryCatch(
    {
      if (shared_parent == "father") {
        pedtools::addChildren(
          ped,
          father = father_id,
          mother = NULL,
          nch = n_siblings,
          sex = sex,
          verbose = FALSE
        )
      } else {
        pedtools::addChildren(
          ped,
          father = NULL,
          mother = mother_id,
          nch = n_siblings,
          sex = sex,
          verbose = FALSE
        )
      }
    },
    error = function(e) {
      stop(sprintf("Error adding half-siblings: %s", e$message))
    }
  )

  return(new_ped)
}

addTwin <- function(ped, id, sex = NULL, zygosity = 2) {
  if (length(id) != 1) stop("Select exactly one individual")

  if (zygosity == 1) {
    sex <- pedtools::getSex(ped, id)
  }

  if (is.null(sex)) {
    sex <- pedtools::getSex(ped, id)
  }

  labels_before <- labels(ped)

  new_ped <- addFullSiblings(ped, id, n_siblings = 1, sex = sex, verbose = FALSE)
  new_ped <- relabel_generations(new_ped)

  labels_after <- labels(new_ped)
  new_ids <- setdiff(labels_after, labels_before)

  if (length(new_ids) == 0) stop("No new twin was added")

  sibs <- getSiblings(new_ped, new_ids[1])
  original_id_after <- intersect(sibs, labels_before)

  if (length(original_id_after) == 0) {
    original_id_after <- id
  } else {
    original_id_after <- original_id_after[1]
  }

  twin_id <- new_ids[1]

  return(list(
    ped = new_ped,
    original_id = original_id_after,
    twin_id = twin_id,
    zygosity = zygosity
  ))
}

addTriplets <- function(ped, id) {
  if (length(id) != 1) stop("Select exactly one individual")

  sex_base <- pedtools::getSex(ped, id)
  labels_before <- labels(ped)

  new_ped <- addFullSiblings(ped, id, n_siblings = 2, sex = sex_base, verbose = FALSE)
  new_ped <- relabel_generations(new_ped)

  labels_after <- labels(new_ped)
  new_ids <- setdiff(labels_after, labels_before)

  if (length(new_ids) != 2) {
    stop(sprintf("Expected 2 new IDs but found %d", length(new_ids)))
  }

  sibs <- getSiblings(new_ped, new_ids[1])
  original_id_after <- intersect(sibs, labels_before)

  if (length(original_id_after) == 0) {
    original_id_after <- id
  } else {
    original_id_after <- original_id_after[1]
  }

  triplet_ids <- sort(c(original_id_after, new_ids))

  return(list(
    ped = new_ped,
    triplet_ids = triplet_ids
  ))
}

# ✅ NOUVELLE FONCTION : Détection robuste avec coordonnées normalisées
find_nearest_individual <- function(centers_df, norm_x, norm_y, max_distance = 0.15) {
  if (is.null(centers_df) || !nrow(centers_df)) {
    return(NA_integer_)
  }
  if (is.null(norm_x) || is.null(norm_y)) {
    return(NA_integer_)
  }

  x_range <- range(centers_df$x, na.rm = TRUE)
  y_range <- range(centers_df$y, na.rm = TRUE)

  if (diff(x_range) == 0 || diff(y_range) == 0) {
    return(NA_integer_)
  }

  norm_centers_x <- (centers_df$x - x_range[1]) / diff(x_range)
  norm_centers_y <- (centers_df$y - y_range[1]) / diff(y_range)

  distances <- sqrt((norm_centers_x - norm_x)^2 + (norm_centers_y - norm_y)^2)
  min_dist_idx <- which.min(distances)

  if (length(min_dist_idx) > 0 && distances[min_dist_idx] <= max_distance) {
    return(centers_df$id_plot[min_dist_idx])
  }

  return(NA_integer_)
}

nearest_by_centers <- function(centers_df, click, px_thresh = 25) {
  if (is.null(click$domain) || is.null(click$range)) {
    return(NA_integer_)
  }
  if (is.null(centers_df) || !nrow(centers_df)) {
    return(NA_integer_)
  }
  hit <- nearPoints(centers_df, click, xvar = "x", yvar = "y", threshold = px_thresh, maxpoints = 1)
  if (!nrow(hit)) {
    return(NA_integer_)
  }
  hit$id_plot[1]
}

prepare_annotation_data <- function(ped, ped_data) {
  ids <- labels(ped)
  annotations <- list()

  for (id in ids) {
    row <- ped_data[ped_data$id == id, , drop = FALSE]

    if (nrow(row) == 0) {
      annotations[[as.character(id)]] <- list(
        id = as.character(id),
        texts = list()
      )
      next
    }

    get_chr <- function(nm) {
      if (nm %in% names(row)) as.character(row[[nm]][1]) else ""
    }

    ln <- get_chr("last_name")
    fn <- get_chr("first_name")
    aab <- toupper(trimws(get_chr("assigned_at_birth")))
    dob <- get_chr("date_of_birth")
    dod <- get_chr("date_of_death")
    age <- get_chr("age")
    com <- get_chr("comments")
    deceased <- ("deceased" %in% names(row)) && isTRUE(row$deceased[1])

    texts <- list()

    if (nzchar(aab)) {
      texts[[length(texts) + 1]] <- list(
        text = aab,
        pos = 3,
        offset = 1,
        col = "#D32F2F",
        cex = 0.85,
        font = 3
      )
    }

    if (nzchar(ln) || nzchar(fn)) {
      name_parts <- c()
      if (nzchar(ln)) name_parts <- c(name_parts, toupper(trimws(ln)))
      if (nzchar(fn)) name_parts <- c(name_parts, tools::toTitleCase(trimws(fn)))

      full_name <- paste(name_parts, collapse = " ")

      texts[[length(texts) + 1]] <- list(
        text = full_name,
        pos = 1,
        offset = 2.9,
        col = "#1976D2",
        cex = 0.9,
        font = 2
      )
    }

    if (nzchar(dob) || (deceased && nzchar(dod))) {
      date_parts <- c()

      if (nzchar(dob)) {
        dob_formatted <- format_date(dob)
        if (nzchar(dob_formatted)) {
          dob_str <- gsub("/", "-", dob_formatted)
          date_parts <- c(date_parts, dob_str)
        }
      }

      if (deceased && nzchar(dod)) {
        dod_formatted <- format_date(dod)
        if (nzchar(dod_formatted)) {
          dod_str <- gsub("/", "-", dod_formatted)
          date_parts <- c(date_parts, dod_str)
        }
      }

      if (length(date_parts) > 0) {
        dates_text <- paste(date_parts, collapse = " — ")

        texts[[length(texts) + 1]] <- list(
          text = dates_text,
          pos = 1,
          offset = 3.7,
          col = "#C2185B",
          cex = 0.8,
          font = 1
        )
      }
    }

    status_parts <- c()

    if (deceased) {
      status_parts <- c(status_parts, "†")
    }

    if (nzchar(age)) {
      age_num <- trimws(gsub("[^0-9]", "", age))
      if (nzchar(age_num)) {
        age_text <- paste(age_num, "years")
        status_parts <- c(status_parts, age_text)
      }
    }

    if (length(status_parts) > 0) {
      status_text <- paste(status_parts, collapse = " • ")

      texts[[length(texts) + 1]] <- list(
        text = status_text,
        pos = 1,
        offset = 4.5,
        col = "#C2185B",
        cex = 0.75,
        font = 3
      )
    }

    if (nzchar(com)) {
      com_clean <- trimws(com)

      if (nchar(com_clean) > 40) {
        com_clean <- paste0(substr(com_clean, 1, 37), "...")
      }

      texts[[length(texts) + 1]] <- list(
        text = com_clean,
        pos = 1,
        offset = 5.5,
        col = "#388E3C",
        cex = 0.7,
        font = 3
      )
    }

    annotations[[as.character(id)]] <- list(
      id = as.character(id),
      texts = texts
    )
  }

  return(annotations)
}

draw_custom_annotations <- function(alignment, annotations, scaling) {
  xall <- alignment$xall
  yall <- alignment$yall
  plotord <- alignment$plotord
  boxh <- scaling$boxh

  for (i in seq_along(plotord)) {
    id <- plotord[i]
    id_char <- as.character(labels(alignment$ped)[id])

    if (!id_char %in% names(annotations)) next

    annot <- annotations[[id_char]]
    if (length(annot$texts) == 0) next

    x_center <- xall[i]
    y_center <- yall[i] + boxh / 2

    for (txt_info in annot$texts) {
      text(
        x = x_center,
        y = y_center,
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

ADOPTION_BRACKET_CONFIG <- list(
  width_ratio = 0.15,
  height_ratio = 1.2,
  left_offset = 4.9,
  right_offset = -1.5,
  line_width = 2.5,
  color = "#494B59"
)

.draw_adoption_brackets <- function(shape, x, y, w, h, lwd = NULL, col = NULL) {
  cfg <- ADOPTION_BRACKET_CONFIG
  if (is.null(lwd)) lwd <- cfg$line_width
  if (is.null(col)) col <- cfg$color

  bracket_width <- w * cfg$width_ratio
  bracket_height <- h * cfg$height_ratio

  left_x <- x - w / 2 - bracket_width * cfg$left_offset
  right_x <- x + w / 2 + bracket_width * cfg$right_offset

  segments(
    x0 = left_x, y0 = y - bracket_height / 2,
    x1 = left_x, y1 = y + bracket_height / 2,
    col = col, lwd = lwd, xpd = NA
  )
  segments(
    x0 = left_x, y0 = y - bracket_height / 2,
    x1 = left_x + bracket_width, y1 = y - bracket_height / 2,
    col = col, lwd = lwd, xpd = NA
  )
  segments(
    x0 = left_x, y0 = y + bracket_height / 2,
    x1 = left_x + bracket_width, y1 = y + bracket_height / 2,
    col = col, lwd = lwd, xpd = NA
  )

  segments(
    x0 = right_x, y0 = y - bracket_height / 2,
    x1 = right_x, y1 = y + bracket_height / 2,
    col = col, lwd = lwd, xpd = NA
  )
  segments(
    x0 = right_x, y0 = y - bracket_height / 2,
    x1 = right_x - bracket_width, y1 = y - bracket_height / 2,
    col = col, lwd = lwd, xpd = NA
  )
  segments(
    x0 = right_x, y0 = y + bracket_height / 2,
    x1 = right_x - bracket_width, y1 = y + bracket_height / 2,
    col = col, lwd = lwd, xpd = NA
  )
}



cleanOrphanPhenotypes <- function(ped, phenotypes) {
  if (is.null(ped)) {
    return(phenotypes)
  }

  current_ids <- labels(ped)

  for (pheno_name in names(phenotypes$assign)) {
    assigned_ids <- phenotypes$assign[[pheno_name]]
    valid_ids <- intersect(assigned_ids, current_ids)

    if (length(valid_ids) != length(assigned_ids)) {
      phenotypes$assign[[pheno_name]] <- valid_ids
    }
  }

  return(phenotypes)
}

saveToHistory <- function(history, pedigree, values, styles, phenotypes) {
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

      return(history)
    },
    error = function(e) {
      warning(paste("Error saving to history:", e$message))
      return(history)
    }
  )
}

colorPickerUI <- function(inputId, label = NULL, selected = "#FFFFFF") {
  tagList(
    if (!is.null(label)) tags$div(class = "color-picker-label", label),
    tags$div(
      class = "color-picker-container",
      tags$input(type = "hidden", id = inputId, value = selected),
      tags$div(
        class = "color-picker-display",
        id = paste0(inputId, "_display"),
        style = sprintf("background-color: %s;", selected)
      ),
      tags$div(
        class = "color-picker-dropdown",
        id = paste0(inputId, "_dropdown"),
        style = "display: none;",
        tags$div(
          class = "color-picker-grid",
          lapply(1:length(COLOR_PALETTE), function(row_idx) {
            tags$div(
              class = "color-picker-row",
              lapply(COLOR_PALETTE[[row_idx]], function(color) {
                tags$div(
                  class = if (color == selected) "color-circle selected" else "color-circle",
                  style = sprintf("background-color: %s;", color),
                  `data-color` = color,
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'}); selectColor('%s', '%s');",
                    inputId, color, inputId, color
                  )
                )
              })
            )
          })
        )
      )
    )
  )
}
