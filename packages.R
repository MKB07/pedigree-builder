required_packages <- c(
  "shiny",
  "shinyjs",
  "htmltools",
  "pedtools",
  "DT",
  "colourpicker",
  "ribd",
  "verbalisr",
  "httr",
  "httr2",
  "jsonlite",
  "base64enc",
  "xml2",
  "visNetwork"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(required_packages)
