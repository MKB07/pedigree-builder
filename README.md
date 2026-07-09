# Pedigree Builder

Application Shiny pour construire, annoter et explorer des pedigrees dans un contexte d'apprentissage, de recherche et de prototypage autour de la genetique humaine.

## Objectif

Pedigree Builder centralise plusieurs besoins pratiques :

- creation interactive de pedigrees ;
- annotation des individus, phenotypes et informations familiales ;
- exploration des relations familiales et de coefficients apparentes ;
- consultation de ressources genes / GeneReviews depuis l'interface ;
- export et rechargement de donnees de travail.

L'application a ete developpee comme projet de TFE par une utilisatrice issue du domaine biomedical, avec une progression iterative sur plus d'un an. Ce depot correspond a une version nettoyee et documentee, preparee pour lecture, maintenance et partage.

## Statut clinique

Cette application n'est pas un dispositif medical valide. Elle ne doit pas etre utilisee pour poser un diagnostic, guider une decision clinique, remplacer un avis medical ou traiter des donnees patient reelles sans cadre institutionnel, validation, securite et consentement adaptes.

## Structure du depot

```text
.
|-- app.R
|-- app_gene_modul_API.R
|-- app_genereviewExplorer.R
|-- GRshortname_NBKid_genesymbol_dzname.txt
|-- notes_app_general.html
|-- packages.R
|-- run_app.R
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
`-- docs/
    |-- LIMITATIONS.md
    |-- PROJECT_HISTORY.md
    `-- TECHNICAL_NOTES.md
```

## Installation

Installer R et RStudio, puis installer les dependances :

```r
source("packages.R")
```

## Lancement local

Depuis le dossier du depot :

```r
shiny::runApp(".")
```

ou :

```r
source("run_app.R")
```

## Dependances principales

- `shiny`, `shinyjs`, `htmltools`
- `pedtools`, `ribd`, `verbalisr`
- `DT`, `colourpicker`
- `httr`, `httr2`, `jsonlite`, `xml2`, `base64enc`
- `visNetwork`

## Notes de maintenance

- Le fichier principal est `app.R`.
- Les fonctions reutilisables sont deplacees dans `R/`.
- Les ressources frontend sont dans `www/`.
- Les fichiers de support GeneReviews et notes doivent rester a la racine du depot.
- Les appels reseau vers des bases externes peuvent echouer si l'acces internet est restreint ou si les API changent.

## Licence

Aucune licence open source n'a encore ete choisie. Sauf ajout explicite d'une licence, tous droits reserves.
