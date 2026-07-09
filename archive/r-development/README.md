# R Development Archive

This directory preserves selected R files from the local development history of Pedigree Builder.

The active Shiny application does **not** source these files. They are kept to document the project trajectory before the GitHub repository was cleaned and organized.

## Why this archive exists

The project was developed locally before GitHub was used as a structured version-control workflow. These archived files make that work visible without mixing old prototypes into the production app.

Each copied file starts with an archive header containing:

- original local path;
- original creation date when available from the filesystem;
- original modification date;
- archive rationale;
- active app status.

## Directory map

- `curated/core-evolution/`: early and intermediate pedigree-building prototypes.
- `curated/ui-experiments/`: visual and interface experiments.
- `curated/modules/`: standalone module prototypes.
- `curated/analysis/`: kinship, relationship, and pedtools experiments.
- `curated/external-tools/`: related tools that were not integrated into the main application.
- `curated/full-snapshots/`: larger full-application snapshots.
- `curated/modular-beta/`: beta attempt at a more modular Shiny architecture.

## Files

- `SELECTED_FILES.md`: curated files copied into this archive and why.
- `SOURCE_INVENTORY.md`: complete inventory of all `.R` files found in the source folder, including files not copied into the curated archive.
