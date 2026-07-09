
# 5. Plot scaling helper -------------------------------------------------------
# pedtools:::.pedScaling() can fail in Shiny when the plot panel is too small for
# the requested labels, symbols or margins. This wrapper tries smaller values
# before letting the original error surface.
safe_ped_scaling <- function(
    alignment,
    annotation,
    cex = 1,
    symbolsize = 1,
    margins = rep(1, 4),
    autoScale = TRUE,
    minsize = 0.15
) {
  scale_once <- function(auto_scale, min_size, cex_value, symbol_value, margin_value) {
    pedtools:::.pedScaling(
      alignment,
      annotation,
      cex = cex_value,
      symbolsize = symbol_value,
      margins = margin_value,
      autoScale = auto_scale,
      minsize = min_size
    )
  }
  
  base_margins <- rep(margins, length.out = 4)
  attempts <- expand.grid(
    autoScale = c(autoScale, FALSE),
    minsize = c(minsize, 0.10, 0.05, 0.02),
    shrink = c(1, 0.85, 0.70, 0.55),
    stringsAsFactors = FALSE
  )
  
  last_error <- NULL
  for (i in seq_len(nrow(attempts))) {
    attempt <- attempts[i, ]
    cex_value <- max(0.25, cex * attempt$shrink)
    symbol_value <- max(0.25, symbolsize * attempt$shrink)
    margin_value <- pmax(0.05, base_margins * attempt$shrink)
    
    result <- tryCatch(
      scale_once(
        attempt$autoScale,
        attempt$minsize,
        cex_value,
        symbol_value,
        margin_value
      ),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    if (!is.null(result)) {
      return(result)
    }
  }
  
  stop(last_error)
}