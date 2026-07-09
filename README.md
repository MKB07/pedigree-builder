# Pedigree Builder

Pedigree Builder is an interactive R Shiny application for building, annotating, and analysing genetic family trees. It combines classical pedigree drawing with relationship analysis, biomedical reference databases, and educational tools in a single interface.

The project is intended for students, educators, genetic counsellors, and researchers who need a visual way to construct and study family pedigrees. It supports learning the basics of Mendelian inheritance, exploring complex family histories, and connecting pedigree observations with genomic and clinical reference information.

## Project Objective

The main objective of Pedigree Builder is to make pedigree construction and interpretation easier, more visual, and more connected to modern bioinformatics resources.

The application helps users:

- create pedigrees from scratch or from predefined examples;
- add individuals and define family relationships such as parents, siblings, partners, and offspring;
- record clinical phenotypes and special statuses such as deceased, proband, twins, or miscarriages;
- visualise hereditary patterns across generations;
- analyse kinship, inbreeding, consanguinity loops, and relatedness between individuals;
- retrieve gene, disease, pathway, variant, publication, and protein information from external databases;
- use integrated educational tools to explore inheritance patterns and genetic concepts.

## Core Modules

### Genetic Family Tree

The pedigree builder provides a visual workspace for representing family relationships and the inheritance of genetic traits across generations. Users can construct pedigrees, annotate individuals, and better understand familial genetic structures.

### Kinship and Relationship Analysis

The analysis module helps users explore genetic relationships within a pedigree. It supports calculations and visual interpretation of relatedness, inbreeding coefficients, and consanguinity patterns, which are important concepts in clinical genetics and inheritance analysis.

### Informatics and Reference Databases

The integrated genomic tools provide access to biomedical reference databases through public APIs. These resources support research and molecular genetics education by connecting a gene symbol or clinical question with external information sources.

Referenced resources include MyGene, UniProt, OpenTargets, Reactome, PanelApp, ClinVar, Europe PMC, STRING, and GeneReviews-oriented data.

### Learning and Research Tools

The application includes additional learning and research features, such as:

- a Gene Explorer for technical gene information;
- a Review Explorer for clinically relevant reference information;
- curated resources for genetics practice and education;
- an interactive Punnett square tool for inheritance pattern simulation;
- individual report generation from selected pedigree members.

## Important Disclaimer

Pedigree Builder is provided strictly for educational and research purposes. It is not a validated medical device and must not be used for clinical diagnosis, genetic counselling decisions, patient management, or any form of patient care.

The data and analyses shown in the application are indicative only and have not been validated for clinical use. Any medical decision should rely on certified diagnostic tools and qualified healthcare professionals.

## Technology

Pedigree Builder is built with:

- R and Shiny;
- `pedtools` for pedigree-related structures and calculations;
- REST APIs for external biomedical resources;
- Bootstrap, CSS, and JavaScript for the user interface.

## Repository Structure

```text
.
|-- app.R
|-- packages.R
|-- run_app.R
|-- application_features.pdf
|-- archive/
|   `-- r-development/
|-- data/
|   |-- app_gene_modul_API.R
|   |-- app_genereviewExplorer.R
|   |-- GRshortname_NBKid_genesymbol_dzname.txt
|   `-- notes_app_general.html
|-- R/
|   |-- Formatting_helpers.R
|   |-- Relationship_helpers.R
|   |-- scaling_helper.R
|   |-- edit_helpers.R
|   |-- annot.R
|   |-- phenotype.R
|   |-- select_ped.R
|   `-- modales.R
|-- www/
|   |-- styles.css
|   `-- script.js
`-- doc/
    |-- DEVELOPMENT_JOURNEY.md
    |-- LIMITATIONS.md
    |-- PROJECT_HISTORY.md
    `-- TECHNICAL_NOTES.md
```

## Installation

Install R and, optionally, RStudio. From the repository root, install the required packages with:

```r
source("packages.R")
```

## Running the Application

From the repository root, start the Shiny app with:

```r
shiny::runApp(".")
```

You can also use the helper script:

```r
source("run_app.R")
```

## Maintenance Notes

- `app.R` is the main Shiny application entry point.
- Reusable R helpers are stored in `R/`.
- Frontend assets are stored in `www/`.
- Gene and GeneReviews support files are stored in `data/`.
- Historical R prototypes and local development snapshots are stored in `archive/r-development/`.
- Network calls to external databases may fail if internet access is restricted or if upstream APIs change.
- Documentation in `doc/` records project history, technical notes, and known limitations.

## License

This project is distributed under the GNU General Public License v3.0. See `LICENSE` for the full license text.
