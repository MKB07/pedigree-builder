# 1. Formatting helpers --------------------------------------------------------
# These helpers only prepare values for UI tables. They do not perform pedigree
# calculations.
format_result <- function(x, empty = "None") {
  if (length(x) == 0) {
    return(empty)
  }
  paste(x, collapse = ", ")
}

format_num <- function(x, digits = 4, empty = "NA") {
  if (length(x) == 0) {
    return(empty)
  }
  x <- x[[1]]
  if (is.na(x)) {
    return(empty)
  }
  formatC(x, digits = digits, format = "f")
}
relabel_gen <- function(p) {
  rf <- names(formals(pedtools::relabel))
  if ("new" %in% rf) {
    pedtools::relabel(p, new = "generations")
  } else {
    pedtools::relabel(p, "generations")
  }
}

nearest_click <- function(centers, click, thresh = 25) {
  if (is.null(click$domain) || is.null(click$range)) {
    return(NA_integer_)
  }
  if (is.null(centers) || !nrow(centers)) {
    return(NA_integer_)
  }
  hit <- nearPoints(
    centers,
    click,
    xvar = "x",
    yvar = "y",
    threshold = thresh,
    maxpoints = 1
  )
  if (!nrow(hit)) {
    return(NA_integer_)
  }
  hit$id_plot[1]
}

# =============================================================================
# Pedigree Mutation Helpers
# =============================================================================

# ── updateLabelsData: relabel pedigree and remap all attached metadata ───────
updateLabelsData <- function(
    pedigree,
    styles,
    textAnnot,
    new = "generations",
    .alignment = NULL
) {
  newdat <- list()
  ped <- pedigree$ped
  
  if (is.null(ped)) {
    return(list(
      ped = NULL,
      twins = NULL,
      miscarriage = NULL,
      styles = styles,
      textAnnot = textAnnot,
      idMap = NULL
    ))
  }
  
  old_labels <- labels(ped)
  
  if (identical(new, "asPlot") || identical(new, "generations")) {
    idMap <- pedtools::relabel(
      ped,
      new = new,
      .alignment = .alignment,
      returnLabs = TRUE
    )
    newped <- pedtools::relabel(ped, new = idMap, reorder = TRUE)
  } else {
    newped <- pedtools::relabel(ped, new = new, reorder = FALSE)
    idMap <- setNames(labels(newped), old_labels)
  }
  
  newdat$ped <- newped
  newdat$idMap <- idMap
  
  # Remap twins
  if (!is.null(twins <- pedigree$twins)) {
    twins$id1 <- as.character(idMap[as.character(twins$id1)])
    twins$id2 <- as.character(idMap[as.character(twins$id2)])
    newdat$twins <- twins
  } else {
    newdat$twins <- NULL
  }
  
  # Remap miscarriage
  if (!is.null(misc <- pedigree$miscarriage)) {
    newdat$miscarriage <- as.character(idMap[as.character(misc)])
  } else {
    newdat$miscarriage <- NULL
  }
  
  # Remap styles
  newdat$styles <- list()
  
  if (!is.null(fill <- styles$fill)) {
    if (!is.null(names(fill))) {
      names(fill) <- as.character(idMap[names(fill)])
      newdat$styles$fill <- fill
    } else {
      newdat$styles$fill <- NULL
    }
  } else {
    newdat$styles$fill <- NULL
  }
  
  for (sty in c(
    "hatched",
    "carrier",
    "aff",
    "dashed",
    "deceased",
    "proband",
    "starred",
    "adopted",
    "infertility",
    "afab",
    "amab",
    "umab"
  )) {
    if (!is.null(styles[[sty]]) && length(styles[[sty]]) > 0) {
      mapped_ids <- as.character(idMap[as.character(styles[[sty]])])
      mapped_ids <- mapped_ids[!is.na(mapped_ids)]
      newdat$styles[[sty]] <- if (length(mapped_ids) > 0) {
        mapped_ids
      } else {
        NULL
      }
    } else {
      newdat$styles[[sty]] <- NULL
    }
  }
  
  # Remap text annotations
  if (!is.null(textAnnot)) {
    newdat$textAnnot <- lapply(textAnnot, function(v) {
      if (!is.null(names(v))) {
        setNames(v, as.character(idMap[names(v)]))
      } else {
        v
      }
    })
  } else {
    newdat$textAnnot <- NULL
  }
  
  return(newdat)
}


# =============================================================================
# Age, Date and Label Helpers
# =============================================================================

# ── calculate_age: compute numeric age + unit from dob (+ dod or today) ───────
#    Returns list(value, unit) e.g. list("42", "years") or list("6", "months")
calculate_age <- function(dob, dod = "", preferred_unit = "years") {
  fmt <- "%d-%m-%Y"
  empty <- list(value = "", unit = preferred_unit)
  if (!nzchar(dob)) {
    return(empty)
  }
  d0 <- tryCatch(as.Date(dob, fmt), error = function(e) NA)
  if (is.na(d0)) {
    return(empty)
  }
  d1 <- if (nzchar(dod)) {
    tryCatch(as.Date(dod, fmt), error = function(e) NA)
  } else {
    Sys.Date()
  }
  if (is.na(d1) || d1 < d0) {
    return(empty)
  }
  total_days <- as.numeric(difftime(d1, d0, units = "days"))
  
  # Cascade to smaller unit when value <= 0: years -> months -> weeks -> days
  unit_chain <- c("years", "months", "weeks", "days")
  start <- match(preferred_unit, unit_chain)
  if (is.na(start)) {
    start <- 1L
  }
  
  for (u in unit_chain[start:length(unit_chain)]) {
    val <- switch(u,
                  "days" = as.integer(floor(total_days)),
                  "weeks" = as.integer(floor(total_days / 7)),
                  "months" = as.integer(floor(total_days / 30.4375)),
                  as.integer(floor(total_days / 365.25))
    )
    if (val > 0 || u == "days") {
      return(list(value = as.character(val), unit = u))
    }
  }
  list(value = as.character(as.integer(floor(total_days))), unit = "days")
}

# ── format_age_text: build display string from value + unit ──
format_age_text <- function(age_val, age_unit) {
  if (is.null(age_val) || !nzchar(age_val)) {
    return("")
  }
  n <- suppressWarnings(as.integer(age_val))
  if (is.na(n)) {
    return("")
  }
  unit_label <- switch(age_unit %||% "years",
                       "days" = if (n == 1) "day" else "days",
                       "weeks" = if (n == 1) "week" else "weeks",
                       "months" = if (n == 1) "month" else "months",
                       if (n == 1) "year" else "years"
  )
  paste(n, unit_label)
}

# ── parse_age_number: extract integer from age value ──
parse_age_number <- function(age_val) {
  if (is.null(age_val) || !nzchar(age_val)) {
    return(NA_integer_)
  }
  nums <- gsub("[^0-9]", "", trimws(age_val))
  if (!nzchar(nums)) {
    return(NA_integer_)
  }
  as.integer(nums)
}

# ── age_to_days: convert age value + unit to days ──
age_to_days <- function(age_val, age_unit) {
  n <- parse_age_number(age_val)
  if (is.na(n)) {
    return(NA_real_)
  }
  switch(age_unit %||% "years",
         "days" = n,
         "weeks" = n * 7,
         "months" = as.integer(round(n * 30.4375)),
         as.integer(round(n * 365.25))
  )
}

# ── date_from_age: compute a date given a reference date, age value, unit, and direction ──
#    direction = "backward" → ref_date - age  |  "forward" → ref_date + age
date_from_age <- function(
    ref_date_str,
    age_val,
    age_unit = "years",
    direction = "backward"
) {
  fmt <- "%d-%m-%Y"
  ref <- if (is.null(ref_date_str) || !nzchar(ref_date_str)) {
    Sys.Date()
  } else {
    tryCatch(as.Date(ref_date_str, fmt), error = function(e) NA)
  }
  ndays <- age_to_days(age_val, age_unit)
  if (is.na(ref) || is.na(ndays) || ndays < 0) {
    return("")
  }
  result <- if (direction == "forward") ref + ndays else ref - ndays
  format(result, fmt)
}

# ── format_date: dd-mm-yyyy → dd/mm/yyyy ──
format_date <- function(x) {
  if (is.null(x) || is.na(x) || x == "" || x == "NA") {
    return("")
  }
  tryCatch(format(as.Date(x, "%d-%m-%Y"), "%d/%m/%Y"), error = function(e) "")
}

# ── round3: format a number to 3 decimal places ──
round3 <- function(x) {
  ifelse(is.na(x), NA, formatC(x, digits = 3, format = "f"))
}