# Development Journey

This document summarizes the local development trajectory reconstructed from archived R files. It complements the cleaned GitHub repository history by showing how the project evolved before regular commits were available.

## Reconstructed timeline

### 2024: Early pedigree interaction prototypes

The earliest preserved files focus on basic pedigree creation, person selection, and relationship display. These experiments show the initial exploration of `pedtools`, `kinship2`, Shiny inputs, and interactive pedigree editing.

Representative archive files:

- `archive/r-development/curated/core-evolution/2024-08-17-select-person-prototype.R`
- `archive/r-development/curated/core-evolution/2024-08-17-kinship-legend-prototype.R`
- `archive/r-development/curated/core-evolution/2024-08-18-basic-pedigree-prototype.R`

### 2025: Core application growth

The project expanded into phenotype display, family editing operations, custom annotation helpers, and larger Shiny application snapshots. Several files show the transition from small experiments to integrated app workflows.

Representative archive files:

- `archive/r-development/curated/core-evolution/2025-03-22-phenotype-stack-prototype.R`
- `archive/r-development/curated/core-evolution/2025-06-03-stack-pedigree-prototype.R`
- `archive/r-development/curated/core-evolution/2025-07-17-pedigree-table-selection-prototype.R`
- `archive/r-development/curated/full-snapshots/2025-09-17-application-snapshot.R`

### 2025: Interface and module experiments

A separate stream of files explored visual components, family-member cards, legend layouts, individual detail windows, and standalone modules. Some of these ideas later informed the cleaned application structure, while others remain as historical design experiments.

Representative archive files:

- `archive/r-development/curated/ui-experiments/2025-06-28-individual-window-design.R`
- `archive/r-development/curated/ui-experiments/2025-07-15-family-card-design.R`
- `archive/r-development/curated/ui-experiments/2025-10-18-legend-template-design.R`
- `archive/r-development/curated/modules/2025-07-08-family-add-module.R`

### 2025: Analysis and research support

Dedicated prototypes explored kinship coefficients, sibling lists, pedigree helper behavior, and external educational or genetics support tools. Not all of these were integrated into the main application, but they document the broader research and learning path.

Representative archive files:

- `archive/r-development/curated/analysis/2025-09-17-kinship-coefficients-prototype.R`
- `archive/r-development/curated/analysis/2025-06-28-sibling-list-prototype.R`
- `archive/r-development/curated/external-tools/2025-03-29-punnett-square-tool.R`
- `archive/r-development/curated/external-tools/2025-10-10-dlcn-score-tool.R`

### Late 2025: Modular beta architecture

The `Version beta TEST` files show an attempt to split the Shiny application into clearer modules for family operations, annotations, phenotypes, individual editing, context menus, and relationship analysis. These files are not used directly by the current app, but they are useful references for future refactoring.

Representative archive files:

- `archive/r-development/curated/modular-beta/2025-11-17-beta-app.R`
- `archive/r-development/curated/modular-beta/2025-11-17-beta-family-add-module.R`
- `archive/r-development/curated/modular-beta/2025-11-18-beta-link-analysis-module.R`

## How to use this archive

The archived files should be treated as historical source material. They can support future refactoring decisions, but they should not be sourced directly by the production application without review, testing, and cleanup.

Use `archive/r-development/SELECTED_FILES.md` for the curated subset and `archive/r-development/SOURCE_INVENTORY.md` for the complete source-file inventory.
