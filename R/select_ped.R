
# =============================================================================
# Built-in Example Pedigrees
# =============================================================================

# ── Predefined families ───────────────────────────────────────────────────────
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

# Pre-generate base64 preview images for each pedigree template
pedigree_previews <- lapply(names(families), function(nm) {
  tryCatch(
    {
      p <- families[[nm]]()
      p <- relabel_gen(p)
      tmp <- tempfile(fileext = ".png")
      png(tmp, width = 320, height = 240, res = 96, bg = "#e2e8f0")
      par(mar = c(1, 1, 1, 1))
      plot(p, margins = c(1, 1, 1, 1), cex = 0.9)
      dev.off()
      raw <- readBin(tmp, "raw", file.info(tmp)$size)
      unlink(tmp)
      paste0("data:image/png;base64,", base64enc::base64encode(raw))
    },
    error = function(e) NULL
  )
})
names(pedigree_previews) <- names(families)