# =============================================================================
# UI Helper Functions and Modal Content
# =============================================================================

# UI --------------------------------------------------------------------------
hero_description <- paste(
  "A genetic family tree (or pedigree chart) is a diagram used to represent",
  "family relationships and the inheritance of genetic traits across generations.",
  "This tool helps visualize hereditary patterns, identify possible transmission",
  "of genetic conditions, and better understand familial genetic structures."
)

action_card <- function(id, icon, title, subtitle, onclick) {
  tags$div(
    class = "pedigree-actions__item",
    tags$button(
      id = id,
      class = "pedigree-card",
      onclick = onclick,
      tags$span(class = "material-symbols-outlined pedigree-card__icon", icon),
      tags$span(class = "pedigree-card__title", title),
      tags$span(class = "pedigree-card__subtitle", subtitle)
    )
  )
}

research_item <- function(input_id, icon, title, subtitle) {
  tags$div(
    class = "icon-button",
    actionButton(
      input_id,
      label = HTML(sprintf('<span class=\"material-symbols-outlined\">%s</span>', icon)),
      class = "action-btn"
    ),
    tags$div(class = "icon-title", title),
    tags$div(class = "icon-desc", subtitle)
  )
}

modal_observer <- function(input, input_id, title, body) {
  observeEvent(input[[input_id]], {
    showModal(
      modalDialog(
        title = title,
        body,
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
}

show_app_info_modal <- function(content, size = "l") {
  showModal(
    modalDialog(
      title = NULL,
      size = size,
      easyClose = TRUE,
      class = "app-info-modal",
      content,
      footer = modalButton("Close")
    )
  )
}

about_modal_ui <- function() {
  tags$div(
    class = "app-info-shell",
    tags$p(class = "app-info-kicker", "About"),
    tags$h2(class = "app-info-title", "Genetic Pedigree"),
    tags$p(
      class = "app-info-desc",
      "An interactive application for building, annotating and analysing genetic family trees. Built with the ",
      tags$strong("R"), " programming language and the ",
      tags$a(
        href = "https://cran.r-project.org/package=pedtools",
        target = "_blank",
        rel = "noopener noreferrer",
        style = "color:#2E86C1; text-decoration:underline; font-weight:600;",
        "pedtools"
      ),
      " package by Magnus Dehli Vigeland."
    ),
    tags$div(
      class = "about-grid",
      tags$div(
        class = "about-card",
        tags$div(class = "about-card__accent about-card__accent--green"),
        tags$div(class = "about-card__icon", tags$i(class = "fa-solid fa-diagram-project", style = "color:#16a06a;")),
        tags$h3(class = "about-card__title", "Genetic Family Tree"),
        tags$p(class = "about-card__text", "A genetic family tree (or pedigree chart) is a diagram that represents family relationships and the inheritance of genetic traits across generations. This tool helps visualise hereditary patterns, identify the possible transmission of genetic conditions, and better understand familial genetic structures."),
        tags$p(class = "about-card__text", "The application allows you to create pedigrees from scratch, add individuals, define relationships (parents, siblings, partners), record clinical phenotypes, and annotate special statuses such as deceased, proband, twins or miscarriages.")
      ),
      tags$div(
        class = "about-card",
        tags$div(class = "about-card__accent about-card__accent--purple"),
        tags$div(class = "about-card__icon", tags$i(class = "fa-solid fa-people-arrows", style = "color:#8E44AD;")),
        tags$h3(class = "about-card__title", "Kinship & Relationship Analysis"),
        tags$p(class = "about-card__text", "This module allows users to explore the genetic relationships within a pedigree. It makes it possible to compute inbreeding coefficients, detect consanguinity loops, and analyse the degree of relatedness between any two individuals in the family."),
        tags$p(class = "about-card__text", "Understanding kinship is essential in clinical genetics for evaluating the risk of recessive disorders, interpreting segregation patterns, and planning genetic counselling strategies.")
      ),
      tags$div(
        class = "about-card",
        tags$div(class = "about-card__accent about-card__accent--blue"),
        tags$div(class = "about-card__icon", tags$i(class = "fa-solid fa-database", style = "color:#2E86C1;")),
        tags$h3(class = "about-card__title", "Informatics & Reference Databases"),
        tags$p(class = "about-card__text", "The integrated Genomic Tools section provides real-time access to major biomedical databases via their public APIs: MyGene, UniProt, OpenTargets, Reactome, PanelApp, ClinVar, Europe PMC, and STRING."),
        tags$p(class = "about-card__text", "Search any gene symbol to instantly retrieve functional annotations, protein data, disease associations, clinical variants, pathway memberships, interaction networks, and relevant publications - all without leaving the application.")
      ),
      tags$div(
        class = "about-card",
        tags$div(class = "about-card__accent about-card__accent--orange"),
        tags$div(class = "about-card__icon", tags$i(class = "fa-regular fa-lightbulb", style = "color:#e67e22;")),
        tags$h3(class = "about-card__title", "What is it for?"),
        tags$p(class = "about-card__text", "This application is designed for students, educators, genetic counsellors, and researchers who need a visual, interactive way to construct and study family pedigrees. It bridges classical pedigree drawing with modern bioinformatics."),
        tags$p(class = "about-card__text", "Whether you are learning the basics of Mendelian inheritance, investigating a complex family history, or exploring gene-disease relationships, this tool brings together pedigree construction, kinship analysis, and genomic data retrieval in a single interface.")
      )
    ),
    tags$div(
      class = "about-disclaimer",
      tags$div(class = "about-disclaimer__icon", tags$i(class = "fa-solid fa-triangle-exclamation")),
      tags$div(
        tags$h4(class = "about-disclaimer__title", "Important Disclaimer"),
        tags$p(
          class = "about-disclaimer__text",
          "This application is provided strictly for ", tags$strong("educational and research purposes."),
          " It is ", tags$strong("not"), " a medical device and must ", tags$strong("not"),
          " be used for clinical diagnosis, genetic counselling decisions, or any form of patient care."
        ),
        tags$p(class = "about-disclaimer__text", "The data and analyses presented here are indicative only and have not been validated for clinical use. Any medical decision should rely on certified diagnostic tools and the expertise of qualified healthcare professionals.")
      )
    ),
    tags$div(
      class = "about-tech",
      tags$div(class = "about-tech__badge", tags$i(class = "fa-solid fa-code"), tags$span("R + Shiny")),
      tags$div(class = "about-tech__badge", tags$i(class = "fa-solid fa-cube"), tags$span("pedtools")),
      tags$div(class = "about-tech__badge", tags$i(class = "fa-solid fa-cloud-arrow-down"), tags$span("REST APIs")),
      tags$div(class = "about-tech__badge", tags$i(class = "fa-brands fa-bootstrap"), tags$span("Bootstrap + CSS"))
    )
  )
}

references_modal_ui <- function() {
  tags$div(
    class = "app-info-shell",
    tags$p(class = "app-info-kicker", "References"),
    tags$h2(class = "app-info-title", "Reference Library"),
    tags$p(
      class = "app-info-desc",
      "Core packages, databases and public resources used by Pedigree Builder."
    ),
    tags$div(
      class = "reference-list",
      tags$div(
        class = "reference-item",
        tags$strong("Pedigree analysis"),
        tags$span("Vigeland, M. D. pedtools: Creating and Working with Pedigree and Marker Objects. "),
        tags$a(href = "https://cran.r-project.org/package=pedtools", target = "_blank", rel = "noopener noreferrer", "CRAN pedtools")
      ),
      tags$div(
        class = "reference-item",
        tags$strong("Identity-by-descent and kinship coefficients"),
        tags$span("Vigeland, M. D. ribd: Computation of IBD coefficients and relatedness measures. "),
        tags$a(href = "https://cran.r-project.org/package=ribd", target = "_blank", rel = "noopener noreferrer", "CRAN ribd")
      ),
      tags$div(
        class = "reference-item",
        tags$strong("Relationship descriptions"),
        tags$span("verbalisr package for verbalising pedigree relationships. "),
        tags$a(href = "https://cran.r-project.org/package=verbalisr", target = "_blank", rel = "noopener noreferrer", "CRAN verbalisr")
      ),
      tags$div(
        class = "reference-item",
        tags$strong("Gene annotation"),
        tags$span("MyGene.info, UniProt, Ensembl and NCBI Gene are used for gene identifiers, summaries and functional annotations.")
      ),
      tags$div(
        class = "reference-item",
        tags$strong("Clinical and disease resources"),
        tags$span("Open Targets, ClinVar, Genomics England PanelApp, Orphanet, DECIPHER and gnomAD provide gene-disease, variant and panel information.")
      ),
      tags$div(
        class = "reference-item",
        tags$strong("Pathways, interactions and literature"),
        tags$span("Reactome, STRING, Europe PMC, PubMed and GeneReviews are used for pathways, protein networks, publications and curated reviews.")
      )
    )
  )
}

resource_card <- function(name, desc, href, icon, icon_class, badge, badge_class) {
  tags$a(
    class = "resource-card",
    href = href,
    target = "_blank",
    rel = "noopener noreferrer",
    tags$div(
      class = "resource-card__top",
      tags$div(class = paste("resource-card__icon", icon_class), HTML(icon)),
      tags$span(class = paste("resource-card__badge", badge_class), badge)
    ),
    tags$span(class = "resource-card__name", name),
    tags$span(class = "resource-card__desc", desc),
    tags$span(class = "resource-card__link", "Learn more", HTML("&rarr;"))
  )
}

resources_modal_ui <- function() {
  tags$div(
    class = "resources-modal",
    tags$div(
      class = "resources-modal__header",
      tags$div(
        class = "resources-modal__header-left",
        tags$span(class = "resources-modal__title", "Resources"),
        tags$span(
          class = "resources-modal__subtitle",
          "External databases and tools for genomics research"
        )
      ),
      tags$button(
        class = "resources-modal__close",
        onclick = "Shiny.setInputValue('closeResourcesModal', Math.random(), {priority: 'event'});",
        HTML("&times;")
      )
    ),
    tags$div(
      class = "resources-modal__grid",
      resource_card(
        "DECIPHER",
        "Interactive database for the interpretation of genomic variants. Maps chromosomal imbalances and links them to phenotypic data.",
        "https://www.deciphergenomics.org",
        "&#x1F9EC;",
        "resource-card__icon--blue",
        "Genomics",
        "resource-card__badge--genomics"
      ),
      resource_card(
        "gnomAD",
        "Genome Aggregation Database. Aggregates and harmonizes exome and genome sequencing data to provide allele frequencies across populations.",
        "https://gnomad.broadinstitute.org",
        "&#x1F4CA;",
        "resource-card__icon--purple",
        "Genomics",
        "resource-card__badge--genomics"
      ),
      resource_card(
        "PubMed",
        "Comprehensive biomedical literature database from the National Library of Medicine. Access millions of citations and abstracts.",
        "https://pubmed.ncbi.nlm.nih.gov",
        "&#x1F4D6;",
        "resource-card__icon--green",
        "Literature",
        "resource-card__badge--literature"
      ),
      resource_card(
        "MedlinePlus",
        "Trusted health information from the National Library of Medicine. Provides patient-friendly descriptions of conditions, genes, and treatments.",
        "https://medlineplus.gov",
        "&#x2695;",
        "resource-card__icon--cyan",
        "Clinical",
        "resource-card__badge--clinical"
      ),
      resource_card(
        "Orphanet",
        "Portal for rare diseases and orphan drugs. Provides expert-reviewed information on rare disease nomenclature, prevalence, and care guidelines.",
        "https://www.orpha.net",
        "&#x1F3E5;",
        "resource-card__icon--rose",
        "Clinical",
        "resource-card__badge--clinical"
      ),
      resource_card(
        "CanRisk",
        "Cancer risk prediction tool based on validated models. Calculates breast and ovarian cancer risk using genetic and family history data.",
        "https://www.canrisk.org",
        "&#x1F4C8;",
        "resource-card__icon--amber",
        "Risk",
        "resource-card__badge--risk"
      ),
      resource_card(
        "QuickPed",
        "Interactive pedigree drawing tool. Quickly create and export pedigree diagrams with an intuitive drag-and-drop interface.",
        "https://magnusdv.shinyapps.io/quickped/",
        "&#x1F333;",
        "resource-card__icon--emerald",
        "Tools",
        "resource-card__badge--tools"
      ),
      resource_card(
        "UCSC Genome Browser",
        "Genome visualization platform. Browse and annotate genomic data with tracks for genes, variants, conservation, and regulatory elements.",
        "https://genome.ucsc.edu",
        "&#x1F50D;",
        "resource-card__icon--indigo",
        "Genomics",
        "resource-card__badge--genomics"
      ),
      resource_card(
        "NHGRI Educational Resources",
        "Educational materials from the National Human Genome Research Institute. Fact sheets, glossaries, and multimedia resources about genomics.",
        "https://www.genome.gov/About-Genomics/Educational-Resources",
        "&#x1F393;",
        "resource-card__icon--orange",
        "Education",
        "resource-card__badge--education"
      )
    )
  )
}
