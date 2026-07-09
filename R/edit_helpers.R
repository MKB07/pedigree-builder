

# ── getPartners: find partners connected through shared children ──────────────
getPartners <- function(ped, id) {
  if (is.null(ped) || !id %in% labels(ped)) {
    return(character(0))
  }
  tryCatch(
    {
      children <- pedtools::children(ped, id, internal = FALSE)
      if (length(children) == 0) {
        return(character(0))
      }
      partners <- c()
      for (child in children) {
        parents <- pedtools::parents(ped, child, internal = FALSE)
        if (length(parents) == 2) {
          partner <- setdiff(parents, id)
          if (length(partner) > 0) partners <- c(partners, partner)
        }
      }
      return(unique(partners))
    },
    error = function(e) character(0)
  )
}

# ── addParentsToIndividual: create missing parents for one selected person ────
addParentsToIndividual <- function(ped, id) {
  tryCatch(
    {
      if (!id %in% labels(ped)) {
        stop(sprintf(
          "Individual '%s' does not exist in the pedigree",
          id
        ))
      }
      existing_parents <- tryCatch(
        pedtools::parents(ped, id, internal = FALSE),
        error = function(e) c(NA, NA)
      )
      if (
        length(existing_parents) == 2 && !any(is.na(existing_parents))
      ) {
        stop(sprintf(
          "Individual '%s' already has parents: %s and %s",
          id,
          existing_parents[1],
          existing_parents[2]
        ))
      }
      pedtools::addParents(ped, id, verbose = FALSE)
    },
    error = function(e) stop(paste("Error adding parents:", e$message))
  )
}

# ── addFullSiblings: add siblings sharing both parents with selected person ───
addFullSiblings <- function(ped, id, n = 1, sex = 1) {
  parents <- pedtools::parents(ped, id, internal = FALSE)
  if (length(parents) != 2 || any(is.na(parents))) {
    ped <- addParentsToIndividual(ped, id)
    parents <- pedtools::parents(ped, id, internal = FALSE)
  }
  father <- parents[pedtools::getSex(ped, parents) == 1]
  mother <- parents[pedtools::getSex(ped, parents) == 2]
  pedtools::addChildren(
    ped,
    father = father,
    mother = mother,
    nch = n,
    sex = sex,
    verbose = FALSE
  )
}

# =============================================================================
# Extended Family Structure Helpers
# =============================================================================

# ── addHalfSiblings: add siblings sharing one selected parent ─────────────────
addHalfSiblings <- function(ped, id, n = 1, sex = 1, shared_parent = "father") {
  parents <- pedtools::parents(ped, id, internal = FALSE)
  if (length(parents) != 2 || any(is.na(parents))) {
    ped <- addParentsToIndividual(ped, id)
    parents <- pedtools::parents(ped, id, internal = FALSE)
  }
  father <- parents[pedtools::getSex(ped, parents) == 1]
  mother <- parents[pedtools::getSex(ped, parents) == 2]
  if (shared_parent == "father") {
    pedtools::addChildren(
      ped,
      father = father,
      mother = NULL,
      nch = n,
      sex = sex,
      verbose = FALSE
    )
  } else {
    pedtools::addChildren(
      ped,
      father = NULL,
      mother = mother,
      nch = n,
      sex = sex,
      verbose = FALSE
    )
  }
}

# ── addTwin: add one twin and return metadata; does not relabel ───────────────
addTwin <- function(ped, id, zygosity = 1, twin_sex = NULL) {
  if (length(id) != 1) {
    stop("Select exactly one individual")
  }
  if (!id %in% labels(ped)) {
    stop(sprintf("Individual '%s' not found", id))
  }
  if (!zygosity %in% c(1, 2, 3)) {
    stop("zygosity must be 1 (MZ), 2 (DZ), or 3 (UZ)")
  }
  
  sex_original <- pedtools::getSex(ped, id)
  original_id <- id
  twin_sex_final <- if (zygosity == 1) {
    sex_original
  } else if (!is.null(twin_sex)) {
    twin_sex
  } else {
    sex_original
  }
  
  parents <- tryCatch(
    pedtools::parents(ped, original_id, internal = FALSE),
    error = function(e) c(NA, NA)
  )
  if (length(parents) != 2 || any(is.na(parents))) {
    ped <- pedtools::addParents(ped, original_id, verbose = FALSE)
    parents <- pedtools::parents(ped, original_id, internal = FALSE)
  }
  if (length(parents) != 2 || any(is.na(parents))) {
    stop("Unable to determine both parents")
  }
  
  father_id <- parents[pedtools::getSex(ped, parents) == 1]
  mother_id <- parents[pedtools::getSex(ped, parents) == 2]
  if (length(father_id) != 1 || length(mother_id) != 1) {
    stop("Unable to identify father and mother correctly")
  }
  
  existing_ids <- labels(ped)
  twin_id <- "twin2"
  counter <- 2
  while (twin_id %in% existing_ids) {
    counter <- counter + 1
    twin_id <- paste0("twin", counter)
  }
  
  new_ped <- pedtools::addChildren(
    ped,
    father = father_id,
    mother = mother_id,
    nch = 1,
    sex = twin_sex_final,
    ids = twin_id,
    verbose = FALSE
  )
  
  twins_relation <- data.frame(
    id1 = original_id,
    id2 = twin_id,
    code = zygosity,
    stringsAsFactors = FALSE
  )
  list(
    ped = new_ped,
    original_id = original_id,
    twin_id = twin_id,
    zygosity = zygosity,
    twins_relation = twins_relation
  )
}

# ── addTriplets: add two additional siblings in a triplet group ───────────────
addTriplets <- function(ped, id, sex2 = 1, sex3 = 1) {
  if (length(id) != 1) {
    stop("Select exactly one individual")
  }
  if (!id %in% labels(ped)) {
    stop(sprintf("Individual '%s' not found", id))
  }
  
  parents <- tryCatch(
    pedtools::parents(ped, id, internal = FALSE),
    error = function(e) c(NA, NA)
  )
  if (length(parents) != 2 || any(is.na(parents))) {
    ped <- pedtools::addParents(ped, id, verbose = FALSE)
    parents <- pedtools::parents(ped, id, internal = FALSE)
  }
  father_id <- parents[pedtools::getSex(ped, parents) == 1]
  mother_id <- parents[pedtools::getSex(ped, parents) == 2]
  if (length(father_id) != 1 || length(mother_id) != 1) {
    stop("Unable to identify father and mother correctly")
  }
  
  existing_ids <- labels(ped)
  counter <- 2
  trip_id1 <- paste0("trip", counter)
  while (trip_id1 %in% existing_ids) {
    counter <- counter + 1
    trip_id1 <- paste0("trip", counter)
  }
  existing_ids <- c(existing_ids, trip_id1)
  counter <- counter + 1
  trip_id2 <- paste0("trip", counter)
  while (trip_id2 %in% existing_ids) {
    counter <- counter + 1
    trip_id2 <- paste0("trip", counter)
  }
  
  ped <- pedtools::addChildren(
    ped,
    father = father_id,
    mother = mother_id,
    nch = 1,
    sex = sex2,
    ids = trip_id1,
    verbose = FALSE
  )
  ped <- pedtools::addChildren(
    ped,
    father = father_id,
    mother = mother_id,
    nch = 1,
    sex = sex3,
    ids = trip_id2,
    verbose = FALSE
  )
  
  twins_relations <- data.frame(
    id1 = c(id, trip_id1),
    id2 = c(trip_id1, trip_id2),
    code = c(3L, 3L),
    stringsAsFactors = FALSE
  )
  list(
    ped = ped,
    original_id = id,
    trip_id1 = trip_id1,
    trip_id2 = trip_id2,
    twins_relations = twins_relations
  )
}

# ── addChildrenToIndividual: add child/children with optional partner ─────────
addChildrenToIndividual <- function(
    ped,
    id,
    partner_id = NULL,
    n = 1,
    sex = 1
) {
  id_sex <- pedtools::getSex(ped, id)
  if (is.null(partner_id)) {
    if (id_sex == 1) {
      ped <- pedtools::addChildren(
        ped,
        father = id,
        mother = NULL,
        nch = n,
        sex = sex,
        verbose = FALSE
      )
    } else if (id_sex == 2) {
      ped <- pedtools::addChildren(
        ped,
        father = NULL,
        mother = id,
        nch = n,
        sex = sex,
        verbose = FALSE
      )
    } else {
      stop(
        "Cannot add children to individual with unknown sex without specifying partner"
      )
    }
  } else {
    partner_sex <- pedtools::getSex(ped, partner_id)
    if (id_sex == 1 && partner_sex == 2) {
      ped <- pedtools::addChildren(
        ped,
        father = id,
        mother = partner_id,
        nch = n,
        sex = sex,
        verbose = FALSE
      )
    } else if (id_sex == 2 && partner_sex == 1) {
      ped <- pedtools::addChildren(
        ped,
        father = partner_id,
        mother = id,
        nch = n,
        sex = sex,
        verbose = FALSE
      )
    } else {
      stop("Invalid parent combination")
    }
  }
  return(ped)
}

# ── addMiscarriageChild: add pregnancy loss symbol; does not relabel ──────────
addMiscarriageChild <- function(ped, parent_id, partner_id = NULL) {
  id_sex <- pedtools::getSex(ped, parent_id)
  existing_ids <- labels(ped)
  counter <- 1
  mc_id <- paste0("mc", counter)
  while (mc_id %in% existing_ids) {
    counter <- counter + 1
    mc_id <- paste0("mc", counter)
  }
  
  if (is.null(partner_id)) {
    if (id_sex == 1) {
      ped <- pedtools::addChildren(
        ped,
        father = parent_id,
        mother = NULL,
        nch = 1,
        sex = 0,
        ids = mc_id,
        verbose = FALSE
      )
    } else if (id_sex == 2) {
      ped <- pedtools::addChildren(
        ped,
        father = NULL,
        mother = parent_id,
        nch = 1,
        sex = 0,
        ids = mc_id,
        verbose = FALSE
      )
    } else {
      stop(
        "Cannot add miscarriage child: parent must have defined sex (male or female)"
      )
    }
  } else {
    partner_sex <- pedtools::getSex(ped, partner_id)
    if (id_sex == 1 && partner_sex == 2) {
      ped <- pedtools::addChildren(
        ped,
        father = parent_id,
        mother = partner_id,
        nch = 1,
        sex = 0,
        ids = mc_id,
        verbose = FALSE
      )
    } else if (id_sex == 2 && partner_sex == 1) {
      ped <- pedtools::addChildren(
        ped,
        father = partner_id,
        mother = parent_id,
        nch = 1,
        sex = 0,
        ids = mc_id,
        verbose = FALSE
      )
    } else {
      stop("Invalid parent combination for miscarriage child")
    }
  }
  list(ped = ped, child_id = mc_id)
}
