# Archived R development file
# Original path: module autre/cardiogenetic.R
# Original created: 2025-10-10 18:49:14
# Original modified: 2025-10-10 18:49:15
# Archive rationale: Standalone cardiogenetics support tool kept as non-integrated module.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# app.R — Pertinence des tests génétiques en cardiogénétique
# Auteur : Assistant
# Description : Application Shiny pour estimer la pertinence d'un test génétique
# dans plusieurs pathologies héréditaires cardiovasculaires, via des critères
# cliniques publiés (versions simplifiées à visée d'aide à la décision et de triage).
# ⚠️ AVERTISSEMENT IMPORTANT : Cet outil ne remplace pas un avis spécialisé ni les
# recommandations en vigueur. Il implémente des versions opérationnelles et
# simplifiées de critères publiés (Ghent 2010 pour Marfan, Schwartz pour QT long,
# Shanghai pour Brugada, critères 2017 pour vEDS, heuristiques HCM/DCM pour le rendement
# du test). Toujours confronter au contexte clinique, à l'imagerie et aux lignes directrices locales.

# --- Packages ---
# install.packages(c("shiny", "bslib"))
library(shiny)
library(bslib)

# --- Utilitaires généraux ---
badge <- function(text, class = "primary") {
  tags$span(class = paste0("badge text-bg-", class), text)
}

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Cardiogénétique – Pertinence du test génétique",
  layout_columns(
    col_widths = c(5, 7),

    # Panneau gauche : sélection et actions
    card(
      card_header("Sélection du cadre clinique"),
      selectInput("patho", "Choisir la pathologie :",
        choices = c(
          "Aortopathies – Marfan (Ghent 2010)" = "marfan",
          "Aortopathies – Loeys–Dietz (critères cliniques)" = "lds",
          "Ehlers–Danlos vasculaire vEDS (2017)" = "veds",
          "Canalopathies – Long QT (Schwartz)" = "lqts",
          "Canalopathies – Brugada (Shanghai, simplifié)" = "brugada",
          "Cardiomyopathie hypertrophique – Rendement test" = "hcm",
          "Cardiomyopathie dilatée – Rendement test" = "dcm"
        ), selected = "marfan"
      ),
      hr(),
      actionButton("btn_reset", "Réinitialiser", class = "btn-outline-secondary"),
      actionButton("btn_demo", "Exemple", class = "btn-outline-primary"),
      downloadButton("dl_report", "Exporter rapport (HTML)")
    ),

    # Panneau droit : critères dynamiques + résultat
    card(
      card_header(uiOutput("title_box")),
      uiOutput("criteria_ui"),
      hr(),
      uiOutput("result_box"),
      hr(),
      div(
        class = "text-muted", style = "font-size:90%",
        HTML("<b>Note :</b> implémentation simplifiée pour l'aide à la prescription de tests. \
               Toujours considérer l'imagerie, les antécédents familiaux, et les recommandations locales.")
      )
    )
  ),
  tags$footer(
    class = "text-center text-muted p-3",
    HTML("© ", format(Sys.Date(), "%Y"), " – Cardiogénétique : outil d'aide à la décision (pro).")
  )
)

server <- function(input, output, session) {
  observeEvent(input$btn_reset, {
    updateSelectInput(session, "patho", selected = "marfan")
    # On force la reconstruction de l'UI en changeant de patho
  })

  observeEvent(input$btn_demo, {
    switch(input$patho,
      marfan = {
        updateSelectInput(session, "patho", selected = "marfan")
        updateNumericInput(session, "mf_z", value = 2.3)
        updateCheckboxInput(session, "mf_lens", TRUE)
        updateCheckboxInput(session, "mf_wr_thumb_both", TRUE)
        updateCheckboxInput(session, "mf_pectus_car", TRUE)
        updateCheckboxInput(session, "mf_face", TRUE)
        updateCheckboxInput(session, "mf_striae", TRUE)
      },
      lds = {
        updateSelectInput(session, "patho", selected = "lds")
        updateCheckboxGroupInput(session, "lds_major",
          selected = c("aneurysm", "tortuosity", "cranio", "uvula", "hypertelorism")
        )
      },
      veds = {
        updateSelectInput(session, "patho", selected = "veds")
        updateCheckboxGroupInput(session, "veds_major",
          selected = c("arterial", "intestinal", "uterine", "family")
        )
        updateCheckboxGroupInput(session, "veds_minor",
          selected = c("skin", "bruise", "face")
        )
      },
      lqts = {
        updateSelectInput(session, "patho", selected = "lqts")
        updateNumericInput(session, "qt_qtc", value = 482)
        updateCheckboxInput(session, "qt_tdp", TRUE)
        updateSelectInput(session, "qt_syncope", selected = "stress")
        updateCheckboxInput(session, "qt_fh_lqts", TRUE)
      },
      brugada = {
        updateSelectInput(session, "patho", selected = "brugada")
        updateSelectInput(session, "br_ecg", selected = "type1_spont")
        updateCheckboxInput(session, "br_syncope", TRUE)
        updateCheckboxInput(session, "br_fever", TRUE)
        updateCheckboxInput(session, "br_fh_scd", TRUE)
      },
      hcm = {
        updateSelectInput(session, "patho", selected = "hcm")
        updateNumericInput(session, "hcm_age", value = 35)
        updateCheckboxInput(session, "hcm_fh", TRUE)
        updateNumericInput(session, "hcm_lvh", value = 20)
        updateCheckboxInput(session, "hcm_redflags", TRUE)
      },
      dcm = {
        updateSelectInput(session, "patho", selected = "dcm")
        updateNumericInput(session, "dcm_age", value = 42)
        updateCheckboxInput(session, "dcm_fh", TRUE)
        updateCheckboxInput(session, "dcm_nonischemic", TRUE)
        updateCheckboxInput(session, "dcm_conduction", TRUE)
      }
    )
  })

  output$title_box <- renderUI({
    titles <- list(
      marfan = "Aortopathies – Marfan (Ghent 2010)",
      lds = "Aortopathies – Loeys–Dietz (critères cliniques)",
      veds = "Ehlers–Danlos vasculaire (2017)",
      lqts = "Canalopathies – Long QT (Score de Schwartz)",
      brugada = "Canalopathies – Brugada (Score de Shanghai, simplifié)",
      hcm = "Cardiomyopathie hypertrophique – Estimation du rendement du test",
      dcm = "Cardiomyopathie dilatée – Estimation du rendement du test"
    )
    tags$h4(titles[[input$patho]])
  })

  # --- UI dynamique pour chaque pathologie ---
  output$criteria_ui <- renderUI({
    switch(input$patho,
      marfan = tagList(
        p("Critères de Ghent 2010 – saisissez les éléments ci‑dessous. Le score systémique est calculé automatiquement (≥ 7 suggère Marfan)."),
        numericInput("mf_z", "Z-score racine aortique (≥ 2 anormal)", value = NA, step = 0.1),
        checkboxInput("mf_lens", "Ectopie lentis (subluxation du cristallin)", FALSE),
        h5("Score systémique (Ghent 2010)"),
        fluidRow(
          column(
            6,
            checkboxInput("mf_wr_thumb_both", "Signe du poignet ET du pouce (3 pts)", FALSE),
            checkboxInput("mf_wr_or_thumb", "Signe du poignet OU du pouce (1 pt)", FALSE),
            checkboxInput("mf_pectus_car", "Pectus carinatum (2 pts)", FALSE),
            checkboxInput("mf_pectus_exc", "Pectus excavatum/chest asymétrique (1 pt)", FALSE),
            checkboxInput("mf_hindfoot", "Déformation de l'arrière‑pied (2 pts)", FALSE),
            checkboxInput("mf_pes_planus", "Pied plat (1 pt)", FALSE),
            checkboxInput("mf_pneumo", "Pneumothorax (2 pts)", FALSE),
            checkboxInput("mf_dural", "Ectasie durale (2 pts)", FALSE)
          ),
          column(
            6,
            checkboxInput("mf_protrusio", "Protrusio acetabuli (2 pts)", FALSE),
            checkboxInput("mf_usls_armspan", "Ratio US/LS réduit ET envergure/taille augmentée (1 pt)", FALSE),
            checkboxInput("mf_scolio", "Scoliose/Hypercyphose (1 pt)", FALSE),
            checkboxInput("mf_elbow", "Extension du coude < 170° (1 pt)", FALSE),
            h5("Phénotype facial – cocher les sous-items (≥3/5 donne 1 point)"),
            checkboxInput("mf_face_dolicho", "Dolichocéphalie", FALSE),
            checkboxInput("mf_face_enoph", "Énophtalmie", FALSE),
            checkboxInput("mf_face_ptosis", "Ptose palpébrale / fentes en bas‑dehors", FALSE),
            checkboxInput("mf_face_pala", "Palais ogival / voûte palatine haute", FALSE),
            checkboxInput("mf_face_retro", "Rétrognathisme", FALSE),
            checkboxInput("mf_striae", "Vergetures cutanées (1 pt)", FALSE),
            checkboxInput("mf_myopia", "Myopie > 3 dioptries (1 pt)", FALSE),
            checkboxInput("mf_mvp", "Prolapsus mitral (1 pt)", FALSE)
          )
        ),
        uiOutput("mf_sys_calc")
      ),
      lds = tagList(
        p("Critères cliniques majeurs (approche de triage). Cocher si présent :"),
        checkboxGroupInput("lds_major", NULL,
          inline = FALSE,
          choices = list(
            "Anévrysme/dissection aortique précoce" = "aneurysm",
            "Tortuosité artérielle" = "tortuosity",
            "Hypertélorisme" = "hypertelorism",
            "Uvule bifide / fente palatine" = "uvula",
            "Phénotype craniofacial typique" = "cranio",
            "Antécédent familial LDS" = "family"
          )
        )
      ),
      veds = tagList(
        p("vEDS (COL3A1) – critères 2017. Indication forte si ≥2 majeurs ou 1 majeur + ≥2 mineurs."),
        checkboxGroupInput("veds_major", "Majeurs :",
          inline = FALSE,
          choices = list(
            "Rupture artérielle spontanée / dissection" = "arterial",
            "Perforation intestinale spontanée" = "intestinal",
            "Rupture utérine non proportionnelle" = "uterine",
            "Antécédent familial vEDS" = "family"
          )
        ),
        checkboxGroupInput("veds_minor", "Mineurs :",
          inline = FALSE,
          choices = list(
            "Peau fine/translucide" = "skin",
            "Ecchymoses faciles" = "bruise",
            "Faciès/veines visibles" = "face"
          )
        )
      ),
      lqts = tagList(
        p("Score de Schwartz – version complète (2011)."),
        numericInput("qt_qtc", "QTc au repos (ms)", value = NA, min = 300, max = 700),
        numericInput("qt_recov", "QTc à la 4ᵗ minute de récupération après EFX (ms)", value = NA, min = 300, max = 700),
        checkboxInput("qt_tdp", "Torsades de pointes documentées", FALSE),
        checkboxInput("qt_twal", "T-wave alternans", FALSE),
        checkboxInput("qt_notches", "Onde T à encoches dans ≥3 dérivations", FALSE),
        checkboxInput("qt_brady", "Bradycardie relative pour l'âge", FALSE),
        selectInput("qt_syncope", "Syncope :", c("aucune" = "none", "avec stress" = "stress", "sans stress" = "nostress"), selected = "none"),
        checkboxInput("qt_deaf", "Surdité congénitale", FALSE),
        checkboxInput("qt_fh_lqts", "Apparenté avec LQTS défini (clinique ou génétique)", FALSE),
        checkboxInput("qt_fh_scd", "DCI inexpliquée <30 ans chez apparenté au 1er degré", FALSE)
      ),
      brugada = tagList(
        p("Score de Shanghai (complet) — diagnostic Brugada basé sur ECG, clinique, ATCD familiaux et génétique."),
        selectInput("br_ecg", "ECG :", c(
          "Type 1 spontané (nominal ou hautes dérivations)" = "type1_spont",
          "Type 1 induit par fièvre" = "type1_fever",
          "Type 1 induit par test aux bloqueurs sodiques" = "type1_drug",
          "Type 2/3 converti en type 1 après test" = "type23_conv",
          "Aucun des ci-dessus" = "none"
        )),
        h5("Historique clinique / symptômes"),
        checkboxInput("br_vfvt", "Arrêt cardiaque, VF/PMVT documentée (3 pts)", FALSE),
        checkboxInput("br_agonal", "Respiration agonale nocturne (2 pts)", FALSE),
        checkboxInput("br_syncope_arr", "Syncope suspecte arythmique (2 pts)", FALSE),
        checkboxInput("br_syncope_unclear", "Syncope de cause indéterminée (1 pt)", FALSE),
        checkboxInput("br_aflt30", "FA/Flutter < 30 ans sans cause (0,5 pt)", FALSE),
        h5("ATCD familiaux"),
        checkboxInput("br_fh_def", "Diagnostic Brugada défini chez apparenté 1er/2e degré (2 pts)", FALSE),
        checkboxInput("br_fh_scd", "Mort subite suspecte <45 ans chez apparenté (1 pt)", FALSE),
        h5("Génétique"),
        checkboxInput("br_gene_lp", "Variant (likely pathogenic/pathogenic) compatible (0,5 pt)", FALSE),
        helpText("Interprétation : score ≥ 3,5 probabilité élevée → diagnostic probable/défini; 2–3 possible; <2 non diagnostique. Nécessite contexte clinique.")
      ),
      hcm = tagList(
        p("Heuristique de rendement du test (gènes sarcomériques)."),
        numericInput("hcm_age", "Âge au diagnostic (années)", value = NA, min = 0, max = 100),
        checkboxInput("hcm_fh", "Antécédent familial CMH / mort subite", FALSE),
        numericInput("hcm_lvh", "Épaisseur pariétale max (mm)", value = NA, min = 10, max = 40),
        checkboxInput("hcm_redflags", "Phénotype sévère/événement majeur (arrêt cardiaque, syncope inexpliquée, multiples atteintes)", FALSE)
      ),
      dcm = tagList(
        p("Heuristique de rendement du test (CMD non ischémique)."),
        numericInput("dcm_age", "Âge au diagnostic (années)", value = NA, min = 0, max = 100),
        checkboxInput("dcm_fh", "Antécédent familial CMD / MCD / mort subite", FALSE),
        checkboxInput("dcm_nonischemic", "Étiologie non ischémique (exclure coronaropathie)", FALSE),
        checkboxInput("dcm_conduction", "Troubles de conduction/arythmies (BAV, TV, FA précoce)", FALSE)
      )
    )
  })

  # --- Calculs par pathologie ---
  # 1) Marfan (logique de recommandation de test)
  mf_sys_points <- reactive({
    pts <- 0
    if (isTRUE(input$mf_wr_thumb_both)) pts <- pts + 3 else if (isTRUE(input$mf_wr_or_thumb)) pts <- pts + 1
    if (isTRUE(input$mf_pectus_car)) pts <- pts + 2
    if (isTRUE(input$mf_pectus_exc)) pts <- pts + 1
    if (isTRUE(input$mf_hindfoot)) pts <- pts + 2
    if (isTRUE(input$mf_pes_planus)) pts <- pts + 1
    if (isTRUE(input$mf_pneumo)) pts <- pts + 2
    if (isTRUE(input$mf_dural)) pts <- pts + 2
    if (isTRUE(input$mf_protrusio)) pts <- pts + 2
    if (isTRUE(input$mf_usls_armspan)) pts <- pts + 1
    if (isTRUE(input$mf_scolio)) pts <- pts + 1
    if (isTRUE(input$mf_elbow)) pts <- pts + 1
    # Phénotype facial : 1 point si ≥3/5 sous‑items
    face_subs <- sum(c(isTRUE(input$mf_face_dolicho), isTRUE(input$mf_face_enoph), isTRUE(input$mf_face_ptosis), isTRUE(input$mf_face_pala), isTRUE(input$mf_face_retro)))
    if (face_subs >= 3) pts <- pts + 1
    if (isTRUE(input$mf_striae)) pts <- pts + 1
    if (isTRUE(input$mf_myopia)) pts <- pts + 1
    if (isTRUE(input$mf_mvp)) pts <- pts + 1
    pts
  })

  output$mf_sys_calc <- renderUI({
    pts <- mf_sys_points()
    tagList(
      p(HTML(paste0("Score systémique calculé : ", badge(pts, if (pts >= 7) "danger" else if (pts >= 5) "warning" else "secondary"), " (seuil Ghent = 7)")))
    )
  })

  rec_marfan <- reactive({
    z <- input$mf_z
    lens <- isTRUE(input$mf_lens)
    sys <- mf_sys_points()
    fh <- isTRUE(input$mf_fh)
    # Règle Ghent opératoire pour recommandation de test (pré-test) :
    # forte si : (Z>=2 ET (lens OU sys>=7)) OU (fh ET (sys>=7 OU Z>=2))
    strong <- (!is.na(z) && z >= 2 && (lens || sys >= 7)) || (fh && (sys >= 7 || (!is.na(z) && z >= 2)))
    moderate <- (!is.na(z) && z >= 2) || lens || sys >= 5 || fh
    list(
      level = if (strong) "forte" else if (moderate) "modérée" else "faible",
      detail = list(z = z, lens = lens, sys = sys, fh = fh)
    )
  })

  # 2) Loeys–Dietz : nombre de critères majeurs
  rec_lds <- reactive({
    nmajor <- length(input$lds_major %||% character(0))
    level <- if (nmajor >= 2) "forte" else if (nmajor == 1) "modérée" else "faible"
    list(level = level, nmajor = nmajor)
  })

  # 3) vEDS : règle simple
  rec_veds <- reactive({
    nM <- length(input$veds_major %||% character(0))
    n_m <- length(input$veds_minor %||% character(0))
    strong <- nM >= 2 || (nM >= 1 && n_m >= 2)
    moderate <- nM == 1 || n_m >= 2
    list(level = if (strong) "forte" else if (moderate) "modérée" else "faible", nM = nM, n_m = n_m)
  })

  # 4) LQTS – Score de Schwartz (complet)
  lqts_points <- reactive({
    pts <- 0
    qtc <- input$qt_qtc
    rec <- input$qt_recov
    if (!is.na(qtc)) {
      if (qtc >= 480) {
        pts <- pts + 3
      } else if (qtc >= 460) {
        pts <- pts + 2
      } else if (qtc >= 450) pts <- pts + 1 # (mâles)
    }
    if (!is.na(rec) && rec >= 480) pts <- pts + 1 # 4e minute de récup EFX
    if (isTRUE(input$qt_tdp)) pts <- pts + 2
    if (isTRUE(input$qt_twal)) pts <- pts + 1
    if (isTRUE(input$qt_notches)) pts <- pts + 1
    if (isTRUE(input$qt_brady)) pts <- pts + 0.5
    if (input$qt_syncope == "stress") pts <- pts + 2
    if (input$qt_syncope == "nostress") pts <- pts + 1
    if (isTRUE(input$qt_deaf)) pts <- pts + 0.5
    if (isTRUE(input$qt_fh_lqts)) pts <- pts + 1
    if (isTRUE(input$qt_fh_scd)) pts <- pts + 0.5
    pts
  })
  rec_lqts <- reactive({
    s <- lqts_points()
    level <- if (s >= 3.5) "forte" else if (s >= 1.5) "modérée" else "faible"
    list(level = level, score = s)
  })

  # 5) Brugada – Shanghai complet (opérationnel)
  shanghai_points <- reactive({
    pts <- 0
    # ECG
    ecg <- input$br_ecg
    pts <- pts + switch(ecg,
      type1_spont = 3.5, # Type 1 spontanné
      type1_fever = 3.0, # Type 1 induit par fièvre
      type1_drug = 2.0, # Type 1 induit par bloqueur Na+
      type23_conv = 2.0, # Type 2/3 converti en type 1 après test
      none = 0
    )
    # Historique clinique / symptômes
    if (isTRUE(input$br_vfvt)) pts <- pts + 3 # AC, VF/PMVT documentée
    if (isTRUE(input$br_agonal)) pts <- pts + 2 # respiration agonale nocturne
    if (isTRUE(input$br_syncope_arr)) pts <- pts + 2 # syncope suspecte arythmique
    if (isTRUE(input$br_syncope_unclear)) pts <- pts + 1 # syncope de cause indéterminée
    if (isTRUE(input$br_aflt30)) pts <- pts + 0.5 # FA/Flutter <30 ans sans cause
    if (isTRUE(input$br_fever)) pts <- pts + 0.5 # type 1 déclenché par fièvre (supplémentaire)
    # ATCD familiaux
    if (isTRUE(input$br_fh_def)) pts <- pts + 2 # Brugada défini (≥3.5) chez apparenté 1er/2e degré
    if (isTRUE(input$br_fh_scd)) pts <- pts + 1 # SCD suspecte <45 ans
    # Génétique
    if (isTRUE(input$br_gene_lp)) pts <- pts + 0.5 # Variant (LP/P) compatible
    pts
  })
  rec_brugada <- reactive({
    s <- shanghai_points()
    # Seuils Shanghai : ≥3.5 probable/définitif; 2.0–3.0 possible; <2 non diagnostique
    level <- if (s >= 3.5) "forte" else if (s >= 2) "modérée" else "faible"
    list(level = level, score = s)
  }) # Shanghai simplifié
  shanghai_points <- reactive({
    pts <- 0
    ecg <- input$br_ecg
    pts <- pts + switch(ecg,
      type1_spont = 3.5,
      type1_drug = 2,
      type23_conv = 2,
      none = 0
    )
    if (isTRUE(input$br_syncope)) pts <- pts + 2
    if (isTRUE(input$br_fever)) pts <- pts + 0.5
    if (isTRUE(input$br_fh_brugada)) pts <- pts + 0.5
    if (isTRUE(input$br_fh_scd)) pts <- pts + 0.5
    pts
  })
  rec_brugada <- reactive({
    s <- shanghai_points()
    # Seuils usuels : ≥4 probabilité élevée; 3–3.5 intermédiaire; <3 faible (simplifié)
    level <- if (s >= 4) "forte" else if (s >= 3) "modérée" else "faible"
    list(level = level, score = s)
  })

  # 6) HCM – heuristique de rendement
  rec_hcm <- reactive({
    age <- input$hcm_age
    fh <- isTRUE(input$hcm_fh)
    lvh <- input$hcm_lvh
    red <- isTRUE(input$hcm_redflags)
    score <- 0
    if (!is.na(age) && age < 45) score <- score + 2 else if (!is.na(age) && age <= 60) score <- score + 1
    if (fh) score <- score + 2
    if (!is.na(lvh) && lvh >= 20) score <- score + 2 else if (!is.na(lvh) && lvh >= 16) score <- score + 1
    if (red) score <- score + 2
    level <- if (score >= 5) "forte" else if (score >= 3) "modérée" else "faible"
    list(level = level, score = score)
  })

  # 7) DCM – heuristique de rendement
  rec_dcm <- reactive({
    age <- input$dcm_age
    fh <- isTRUE(input$dcm_fh)
    nonisch <- isTRUE(input$dcm_nonischemic)
    cond <- isTRUE(input$dcm_conduction)
    score <- 0
    if (!is.na(age) && age < 50) score <- score + 2
    if (fh) score <- score + 2
    if (nonisch) score <- score + 1
    if (cond) score <- score + 1
    level <- if (score >= 4) "forte" else if (score >= 2) "modérée" else "faible"
    list(level = level, score = score)
  })

  # --- Boîte de résultat ---
  output$result_box <- renderUI({
    res <- switch(input$patho,
      marfan = rec_marfan(),
      lds = rec_lds(),
      veds = rec_veds(),
      lqts = rec_lqts(),
      brugada = rec_brugada(),
      hcm = rec_hcm(),
      dcm = rec_dcm()
    )
    interp <- switch(res$level,
      forte = badge("Pertinence du test : ÉLEVÉE", "danger"),
      modérée = badge("Pertinence du test : INTERMÉDIAIRE", "warning"),
      badge("Pertinence du test : FAIBLE", "success")
    )
    details <- switch(input$patho,
      marfan = paste0("Z=", res$detail$z, ", lens=", res$detail$lens, ", score systémique=", res$detail$sys, ", AF=", res$detail$fh),
      lds = paste0("Nombre de critères majeurs : ", res$nmajor),
      veds = paste0("Majeurs=", res$nM, ", Mineurs=", res$n_m),
      lqts = paste0("Score de Schwartz = ", sprintf("%.1f", res$score)),
      brugada = paste0("Score de Shanghai (simplifié) = ", sprintf("%.1f", res$score)),
      hcm = paste0("Score heuristique HCM = ", res$score),
      dcm = paste0("Score heuristique DCM = ", res$score)
    )
    tagList(
      h5("Interprétation"),
      p(interp),
      p(em("Détails : "), details)
    )
  })

  # --- Rapport HTML ---
  output$dl_report <- downloadHandler(
    filename = function() {
      paste0("rapport_cardio_gene_", input$patho, "_", format(Sys.time(), "%Y%m%d-%H%M"), ".html")
    },
    content = function(file) {
      title <- switch(input$patho,
        marfan = "Aortopathies – Marfan (Ghent 2010)",
        lds = "Aortopathies – Loeys–Dietz",
        veds = "Ehlers–Danlos vasculaire (2017)",
        lqts = "Long QT (Schwartz)",
        brugada = "Brugada (Shanghai)",
        hcm = "CMH – rendement test",
        dcm = "CMD – rendement test"
      )
      res_ui <- capture.output(print(output$result_box()))
      html <- paste0(
        "<html><head><meta charset='utf-8'><title>", title, "</title>",
        "<style>body{font-family:Arial,Helvetica,sans-serif;margin:2rem;}h1{margin-top:0;} table{border-collapse:collapse;margin:1rem 0;}td,th{border:1px solid #ddd;padding:6px 10px;}</style>",
        "</head><body>",
        "<h1>", title, "</h1>",
        "<p>Date : ", format(Sys.time(), "%Y-%m-%d %H:%M"), "</p>",
        "<h2>Résultat</h2>",
        as.character(htmltools::doRenderTags(output$result_box())),
        "<hr><p style='font-size:90%;color:#666'>Outil d'aide à la décision – versions simplifiées des critères publiés. ",
        "La décision finale de test génétique dépend du contexte clinique, de l'imagerie et des recommandations locales.</p>",
        "</body></html>"
      )
      writeLines(html, file, useBytes = TRUE)
    }
  )
}

shinyApp(ui, server)
