# Technical Notes

## Application Entry Point

`app.R` is the Shiny entry point. It loads helper files from `R/`, frontend resources from `www/`, and support modules from the repository root.

## Helper Files

- `R/Formatting_helpers.R`: formatting, relabeling and metadata remapping.
- `R/Relationship_helpers.R`: kinship, inbreeding and relationship summaries.
- `R/scaling_helper.R`: defensive wrapper around pedigree plot scaling.
- `R/edit_helpers.R`: pedigree mutation operations.
- `R/annot.R`: label and annotation helpers.
- `R/phenotype.R`: phenotype display and palette helpers.
- `R/select_ped.R`: pedigree image/card selection helpers.
- `R/modales.R`: modal UI helpers.

## Plot Scaling

`safe_ped_scaling()` protects the Shiny app against common `pedtools` rendering errors when labels or symbols cannot fit into the current plot area. It progressively reduces label size, symbol size, margins and minimum plot size before falling back to non-auto-scaled plotting.

## External Modules

`app_gene_modul_API.R` and `app_genereviewExplorer.R` are sourced into isolated environments so their helper names do not overwrite the main app helpers.

## Future Improvements

- Add automated tests with `testthat`.
- Consider `renv` for reproducible package versions.
- Split the largest server sections into Shiny modules.
- Add continuous integration once the GitHub repository is connected.
