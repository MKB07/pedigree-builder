# 2. Safe pedtools wrappers ----------------------------------------------------
# These functions are the only place in this section where the heavier pedtools
# calculations are called. If pedtools cannot compute a result, the UI receives a
# predictable empty/NA value instead of an error.
safe_inbreeding_vector <- function(ped_obj) {
  if (is.null(ped_obj)) {
    return(setNames(numeric(0), character(0)))
  }
  
  labs <- labels(ped_obj)
  out <- tryCatch(
    pedtools::inbreeding(ped_obj),
    error = function(e) rep(NA_real_, length(labs))
  )
  
  out <- as.numeric(out)
  if (length(out) != length(labs)) out <- rep(NA_real_, length(labs))
  names(out) <- labs
  out
}

safe_kinship_matrix <- function(ped_obj) {
  if (is.null(ped_obj)) {
    return(matrix(numeric(0), nrow = 0, ncol = 0))
  }
  
  labs <- labels(ped_obj)
  n <- length(labs)
  
  K <- tryCatch(
    as.matrix(pedtools::kinship(ped_obj)),
    error = function(e) matrix(NA_real_, nrow = n, ncol = n)
  )
  
  if (!all(dim(K) == c(n, n))) {
    K <- matrix(NA_real_, nrow = n, ncol = n)
  }
  
  rownames(K) <- labs
  colnames(K) <- labs
  K
}

safe_inbreeding_loops <- function(ped_obj) {
  if (is.null(ped_obj)) {
    return(list())
  }
  tryCatch(pedtools::inbreedingLoops(ped_obj), error = function(e) list())
}

# 3. Relationship helpers ------------------------------------------------------
loop_count_by_individual <- function(ped_obj) {
  labs <- labels(ped_obj)
  loops <- safe_inbreeding_loops(ped_obj)
  counts <- setNames(integer(length(labs)), labs)
  
  if (!length(loops)) {
    return(counts)
  }
  
  for (L in loops) {
    bottom <- as.character(L$bottom)
    if (length(bottom) && bottom %in% names(counts)) {
      counts[bottom] <- counts[bottom] + 1L
    }
  }
  counts
}

shared_members <- function(ped_obj, a, b, member_function) {
  if (is.null(ped_obj) || length(a) != 1 || length(b) != 1) {
    return(character(0))
  }
  intersect(member_function(ped_obj, a), member_function(ped_obj, b))
}

shared_children <- function(ped_obj, a, b) {
  shared_members(ped_obj, a, b, pedtools::children)
}

common_ancestors <- function(ped_obj, a, b) {
  shared_members(ped_obj, a, b, pedtools::ancestors)
}

yes_no <- function(condition) {
  ifelse(condition, "Yes", "No")
}

relationship_label <- function(ped_obj, a, b) {
  if (length(a) != 1 || length(b) != 1) {
    return("NA")
  }
  
  if (a == b) {
    return("Same individual")
  }
  if (b %in% pedtools::parents(ped_obj, a)) {
    return(sprintf("%s is a parent of %s", b, a))
  }
  if (a %in% pedtools::parents(ped_obj, b)) {
    return(sprintf("%s is a parent of %s", a, b))
  }
  if (b %in% pedtools::children(ped_obj, a)) {
    return(sprintf("%s is a child of %s", b, a))
  }
  if (a %in% pedtools::children(ped_obj, b)) {
    return(sprintf("%s is a child of %s", a, b))
  }
  if (b %in% pedtools::spouses(ped_obj, a)) {
    return("Spouses")
  }
  if (b %in% pedtools::siblings(ped_obj, a, half = FALSE)) {
    return("Full siblings")
  }
  if (b %in% pedtools::siblings(ped_obj, a, half = TRUE)) {
    return("Half siblings")
  }
  if (b %in% pedtools::siblings(ped_obj, a, half = NA)) {
    return("Siblings")
  }
  if (b %in% pedtools::grandparents(ped_obj, a)) {
    return(sprintf("%s is a grandparent of %s", b, a))
  }
  if (a %in% pedtools::grandparents(ped_obj, b)) {
    return(sprintf("%s is a grandparent of %s", a, b))
  }
  if (b %in% pedtools::ancestors(ped_obj, a)) {
    return(sprintf("%s is an ancestor of %s", b, a))
  }
  if (a %in% pedtools::ancestors(ped_obj, b)) {
    return(sprintf("%s is an ancestor of %s", a, b))
  }
  if (b %in% pedtools::descendants(ped_obj, a)) {
    return(sprintf("%s is a descendant of %s", b, a))
  }
  if (a %in% pedtools::descendants(ped_obj, b)) {
    return(sprintf("%s is a descendant of %s", a, b))
  }
  if (b %in% pedtools::piblings(ped_obj, a)) {
    return(sprintf("%s is a pibling of %s", b, a))
  }
  if (a %in% pedtools::piblings(ped_obj, b)) {
    return(sprintf("%s is a pibling of %s", a, b))
  }
  if (b %in% pedtools::niblings(ped_obj, a)) {
    return(sprintf("%s is a nibling of %s", b, a))
  }
  if (a %in% pedtools::niblings(ped_obj, b)) {
    return(sprintf("%s is a nibling of %s", a, b))
  }
  if (b %in% pedtools::unrelated(ped_obj, a)) {
    return("Unrelated")
  }
  
  "Related"
}

# 4. Table builders ------------------------------------------------------------
create_info_table <- function(ped_obj, selected_id) {
  if (is.null(ped_obj) || length(selected_id) == 0) {
    return(NULL)
  }
  
  F <- safe_inbreeding_vector(ped_obj)
  loop_counts <- loop_count_by_individual(ped_obj)
  
  ancestors_selected <- pedtools::ancestors(ped_obj, selected_id)
  descendants_selected <- pedtools::descendants(ped_obj, selected_id)
  
  info_list <- list(
    "Displayed ID" = selected_id,
    "Is Male" = selected_id %in% pedtools::males(ped_obj),
    "Is Female" = selected_id %in% pedtools::females(ped_obj),
    "Is Founder" = selected_id %in% pedtools::founders(ped_obj),
    "Is Nonfounder" = selected_id %in% pedtools::nonfounders(ped_obj),
    "Is Leaf" = selected_id %in% pedtools::leaves(ped_obj),
    "Is Typed" = selected_id %in% pedtools::typedMembers(ped_obj),
    "Inbreeding coefficient (F)" = format_num(F[selected_id]),
    "Number of inbreeding loops" = loop_counts[selected_id],
    "Father" = format_result(pedtools::father(ped_obj, selected_id)),
    "Mother" = format_result(pedtools::mother(ped_obj, selected_id)),
    "Parents" = format_result(pedtools::parents(ped_obj, selected_id)),
    "Children" = format_result(pedtools::children(ped_obj, selected_id)),
    "Spouses" = format_result(pedtools::spouses(ped_obj, selected_id)),
    "Siblings (all)" = format_result(pedtools::siblings(ped_obj, selected_id, half = NA)),
    "Full Siblings" = format_result(pedtools::siblings(ped_obj, selected_id, half = FALSE)),
    "Half Siblings" = format_result(pedtools::siblings(ped_obj, selected_id, half = TRUE)),
    "Grandparents" = format_result(pedtools::grandparents(ped_obj, selected_id)),
    "Niblings" = format_result(pedtools::niblings(ped_obj, selected_id)),
    "Piblings" = format_result(pedtools::piblings(ped_obj, selected_id)),
    "Ancestors" = format_result(ancestors_selected),
    "Number of ancestors" = length(ancestors_selected),
    "Descendants" = format_result(descendants_selected),
    "Number of descendants" = length(descendants_selected),
    "Unrelated" = format_result(pedtools::unrelated(ped_obj, selected_id))
  )
  
  data.frame(
    Relationship = names(info_list),
    Value = unlist(info_list),
    stringsAsFactors = FALSE
  )
}

create_top_related_table <- function(ped_obj, selected_id, n_top = 5) {
  if (is.null(ped_obj) || length(selected_id) != 1) {
    return(NULL)
  }
  
  K <- safe_kinship_matrix(ped_obj)
  if (!nrow(K) || !selected_id %in% rownames(K)) {
    return(NULL)
  }
  
  related_values <- K[selected_id, ]
  related_values <- related_values[names(related_values) != selected_id]
  related_values <- related_values[!is.na(related_values)]
  related_values <- related_values[related_values > 0]
  related_values <- sort(related_values, decreasing = TRUE)
  
  if (!length(related_values)) {
    return(data.frame(
      Individual = "None",
      Kinship = "0",
      stringsAsFactors = FALSE
    ))
  }
  
  related_values <- head(related_values, n_top)
  
  data.frame(
    Individual = names(related_values),
    Kinship = vapply(related_values, format_num, character(1)),
    stringsAsFactors = FALSE
  )
}

create_pairwise_table <- function(ped_obj, selected_ids) {
  if (is.null(ped_obj) || length(selected_ids) != 2) {
    return(NULL)
  }
  
  id1 <- selected_ids[1]
  id2 <- selected_ids[2]
  
  K <- safe_kinship_matrix(ped_obj)
  if (!nrow(K) || !all(c(id1, id2) %in% rownames(K))) {
    return(NULL)
  }
  
  kin_val <- K[id1, id2]
  F <- safe_inbreeding_vector(ped_obj)
  
  shared_parents <- shared_members(ped_obj, id1, id2, pedtools::parents)
  shared_spouses <- shared_members(ped_obj, id1, id2, pedtools::spouses)
  
  data.frame(
    Statistic = c(
      "Individual 1",
      "Individual 2",
      "Relationship",
      "Kinship coefficient",
      "Inbreeding of individual 1",
      "Inbreeding of individual 2",
      "Shared parents",
      "Shared spouses",
      "Shared children",
      "Common ancestors",
      "Individual 1 is ancestor of individual 2",
      "Individual 2 is ancestor of individual 1",
      "Unrelated"
    ),
    Value = c(
      id1,
      id2,
      relationship_label(ped_obj, id1, id2),
      format_num(kin_val),
      format_num(F[id1]),
      format_num(F[id2]),
      format_result(shared_parents),
      format_result(shared_spouses),
      format_result(shared_children(ped_obj, id1, id2)),
      format_result(common_ancestors(ped_obj, id1, id2)),
      yes_no(id1 %in% pedtools::ancestors(ped_obj, id2)),
      yes_no(id2 %in% pedtools::ancestors(ped_obj, id1)),
      yes_no(id2 %in% pedtools::unrelated(ped_obj, id1))
    ),
    stringsAsFactors = FALSE
  )
}

make_rel_dt <- function(df) {
  DT::datatable(
    df,
    rownames = FALSE,
    options = list(
      paging = FALSE,
      searching = FALSE,
      info = FALSE,
      lengthChange = FALSE,
      dom = "t",
      scrollX = TRUE
    )
  )
}


# ── describe_relationship: textual kinship description between two individuals ─
describe_relationship <- function(ped, from_id, to_id) {
  if (from_id == to_id) {
    return("Self")
  }
  from_c <- as.character(from_id)
  to_c <- as.character(to_id)
  sex_to <- getSex(ped, to_c)
  
  # Parent
  if (to_c %in% parents(ped, from_c)) {
    return(
      if (sex_to == 1) {
        "Father"
      } else if (sex_to == 2) {
        "Mother"
      } else {
        "Parent"
      }
    )
  }
  # Child
  if (to_c %in% children(ped, from_c)) {
    return(
      if (sex_to == 1) {
        "Son"
      } else if (sex_to == 2) {
        "Daughter"
      } else {
        "Child"
      }
    )
  }
  # Spouse
  if (to_c %in% spouses(ped, from_c)) {
    return("Spouse")
  }
  # Full sibling
  full_sibs <- siblings(ped, from_c, half = FALSE)
  if (to_c %in% full_sibs) {
    return(
      if (sex_to == 1) {
        "Brother"
      } else if (sex_to == 2) {
        "Sister"
      } else {
        "Sibling"
      }
    )
  }
  # Half sibling
  half_sibs <- siblings(ped, from_c, half = TRUE)
  if (to_c %in% half_sibs) {
    return(
      if (sex_to == 1) {
        "Half-Brother"
      } else if (sex_to == 2) {
        "Half-Sister"
      } else {
        "Half-Sibling"
      }
    )
  }
  # Grandparent
  gp <- grandparents(ped, from_c, degree = 2)
  if (to_c %in% gp) {
    return(
      if (sex_to == 1) {
        "Grandfather"
      } else if (sex_to == 2) {
        "Grandmother"
      } else {
        "Grandparent"
      }
    )
  }
  # Grandchild
  gp_to <- grandparents(ped, to_c, degree = 2)
  if (from_c %in% gp_to) {
    return(
      if (sex_to == 1) {
        "Grandson"
      } else if (sex_to == 2) {
        "Granddaughter"
      } else {
        "Grandchild"
      }
    )
  }
  # Great-Grandparent
  ggp <- grandparents(ped, from_c, degree = 3)
  if (to_c %in% ggp) {
    return(
      if (sex_to == 1) {
        "Great-Grandfather"
      } else if (sex_to == 2) {
        "Great-Grandmother"
      } else {
        "Great-Grandparent"
      }
    )
  }
  # Great-Grandchild
  ggp_to <- grandparents(ped, to_c, degree = 3)
  if (from_c %in% ggp_to) {
    return(
      if (sex_to == 1) {
        "Great-Grandson"
      } else if (sex_to == 2) {
        "Great-Granddaughter"
      } else {
        "Great-Grandchild"
      }
    )
  }
  # Uncle/Aunt (full)
  pib_full <- piblings(ped, from_c, half = FALSE)
  if (to_c %in% pib_full) {
    return(
      if (sex_to == 1) {
        "Uncle"
      } else if (sex_to == 2) {
        "Aunt"
      } else {
        "Uncle/Aunt"
      }
    )
  }
  # Half-Uncle/Aunt
  pib_half <- piblings(ped, from_c, half = TRUE)
  if (to_c %in% pib_half) {
    return(
      if (sex_to == 1) {
        "Half-Uncle"
      } else if (sex_to == 2) {
        "Half-Aunt"
      } else {
        "Half-Uncle/Aunt"
      }
    )
  }
  # Nephew/Niece (full)
  nib_full <- niblings(ped, from_c, half = FALSE)
  if (to_c %in% nib_full) {
    return(
      if (sex_to == 1) {
        "Nephew"
      } else if (sex_to == 2) {
        "Niece"
      } else {
        "Nephew/Niece"
      }
    )
  }
  # Half-Nephew/Niece
  nib_half <- niblings(ped, from_c, half = TRUE)
  if (to_c %in% nib_half) {
    return(
      if (sex_to == 1) {
        "Half-Nephew"
      } else if (sex_to == 2) {
        "Half-Niece"
      } else {
        "Half-Nephew/Niece"
      }
    )
  }
  # First Cousin (children of full piblings)
  cousin_ids <- character(0)
  for (pib_id in pib_full) {
    cousin_ids <- c(cousin_ids, children(ped, pib_id))
  }
  if (to_c %in% cousin_ids) {
    return("First Cousin")
  }
  # Half-First Cousin (children of half piblings)
  half_cousin_ids <- character(0)
  for (pib_id in pib_half) {
    half_cousin_ids <- c(half_cousin_ids, children(ped, pib_id))
  }
  if (to_c %in% half_cousin_ids) {
    return("Half-First Cousin")
  }
  # Great-Uncle/Aunt (grandparents' siblings)
  for (g in gp) {
    if (to_c %in% siblings(ped, g, half = FALSE)) {
      return(
        if (sex_to == 1) {
          "Great-Uncle"
        } else if (sex_to == 2) {
          "Great-Aunt"
        } else {
          "Great-Uncle/Aunt"
        }
      )
    }
  }
  # Grand-Nephew/Niece (niblings' children)
  for (nib_id in nib_full) {
    if (to_c %in% children(ped, nib_id)) {
      return(
        if (sex_to == 1) {
          "Grand-Nephew"
        } else if (sex_to == 2) {
          "Grand-Niece"
        } else {
          "Grand-Nephew/Niece"
        }
      )
    }
  }
  # Fallback: check shared ancestry
  anc_from <- ancestors(ped, from_c)
  anc_to <- ancestors(ped, to_c)
  if (
    to_c %in%
    anc_from ||
    from_c %in% anc_to ||
    length(intersect(anc_from, anc_to)) > 0
  ) {
    return("Related")
  }
  return("\u2014")
}
