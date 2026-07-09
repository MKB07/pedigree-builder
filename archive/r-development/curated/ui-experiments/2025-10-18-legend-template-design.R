# Archived R development file
# Original path: apprentissage_test /design/templatelegend.R
# Original created: 2025-10-18 03:25:19
# Original modified: 2025-10-18 03:25:20
# Archive rationale: Advanced legend and UI template design experiment.
# Active app status: not sourced by the current application; kept for project history and technical review.
# -----------------------------------------------------------------------------

# 🔘 LIBRAIRIES
library(shiny)
library(shinyWidgets)
library(pedtools)
library(shinyjs)
library(shinyBS)
library(lubridate)
library(shinyjqui)
library(htmltools)

# ====== STYLES GLOBAUX ======
styles_css <- "
@import url('https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined');
@import url('https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css');
@import url('https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css');

*, *::before, *::after{ box-sizing: border-box; }
html, body{
  min-height:100vh; margin:0; padding:0;
  font-family:'Helvetica Neue', Helvetica, Arial, sans-serif;
  background: linear-gradient(135deg,
      rgba(255,255,255,0.95) 0%,
      rgba(240,240,240,0.92) 40%,
      rgba(225,225,225,0.88) 70%,
      rgba(210,210,210,0.82) 100%);
  color:#2e2e2e;
}
h1, h2, h3{ color:#3a3a3a; letter-spacing:.02em; }
h1{ font-weight:500; line-height:1.25; }

/* Topbar collante */
.mega-nav{ position:fixed; top:0; left:0; right:0; z-index:1030; overflow:hidden;
  background:#0a0a0a; border-bottom:1px solid #1a1a1a;
  box-shadow: rgba(9,30,66,.25) 0 1px 1px, rgba(9,30,66,.13) 0 0 1px 1px; }
.topbar{ height:48px; display:flex; align-items:center; justify-content:center; background:#000; border-bottom:1px solid #161616; }
.topbar .toggle{ right:12px; top:50%; transform:translateY(-50%); width:38px; height:38px; border-radius:50%; position:absolute;
  background:rgba(255,255,255,.06); border:1px solid rgba(255,255,255,.12); color:#e6e6e6; display:flex; align-items:center; justify-content:center; }

/* Hero */
.expanded{ padding:42px 20px 24px; text-align:center; }
.expanded .kicker{ letter-spacing:.35em; color:#9b9b9b; font-size:12px; margin-bottom:12px; }
.expanded p{ color:#bdbdbd; font-size:16px; margin:0 auto; max-width:900px; }

.hr-slot{ display:flex; align-items:center; gap:12px; width:80%; margin:8px auto; justify-content:center; color:#9b9b9b; font-size:12px; }
.hr-slot::before,.hr-slot::after{ content:''; height:1px; flex:1; background-image:linear-gradient(to right, transparent, rgba(61,61,61,.9), transparent); }

#gasses{ display:flex; flex-wrap:wrap; justify-content:center; gap:32px; padding:28px 0; }
.btn.gas{ background:transparent !important; padding:0 !important; border-radius:8px !important; border:3px double currentColor !important; }
.gas{
  --blur:1.75rem; --box-blur:calc(0.5 * var(--blur)); --glow:var(--color); --size:12rem;
  width:var(--size); height:var(--size); display:inline-flex; flex-direction:column; justify-content:space-around; align-items:center;
  border-radius:12px; color:var(--color,#c4c4c6); padding:1rem; letter-spacing:.12em; position:relative; overflow:hidden; cursor:pointer;
  box-shadow: inset 0 0 0 1px rgba(255,255,255,.12), inset 0 0 0 2px rgba(0,0,0,.22),
              0 0 0 2px rgba(0,0,0,.35), 0 0 0 6px rgba(255,255,255,.06),
              inset 0 0 var(--box-blur) var(--glow), 0 0 var(--box-blur) var(--glow);
  transition: filter .18s ease, transform .18s ease, box-shadow .18s ease;
}
.gas:hover{ filter:brightness(120%) drop-shadow(0 0 10px var(--glow)); transform:translateY(-3px); }
.gas .number{ font-weight:600; font-size:.9rem; letter-spacing:.1em; opacity:.85; }
.gas .symbol{ font-size:4rem; line-height:1; text-shadow:0 0 var(--blur) var(--glow); }
.gas .name{ margin:0; font-size:.95rem; letter-spacing:.18em; text-transform:uppercase; opacity:.95; }
.gas.silver{ --color:#c4c4c6; }

.mega-nav .expanded{ transition:max-height .28s ease, opacity .20s ease, padding .20s ease; }
.mega-nav.collapsed .expanded{ max-height:0; opacity:0; padding:0; pointer-events:none; }
#center_col{ display:flex; flex-direction:column; align-items:center; text-align:center; padding-top:44px; }

/* ====== MODÈLE 1 (iOS-like groupé) ====== */
.legend-wrap{
  max-width: calc(100vw - 32px); margin: 24px auto; background: #FFFFFF; border-radius: 16px;
  border: 1px solid rgba(0,0,0,0.06);
  box-shadow: 0 1px 0 rgba(0,0,0,0.04), 0 20px 40px rgba(0,0,0,0.06);
  overflow: hidden;
}
.legend-header{ display:flex; align-items:center; justify-content:space-between; padding:12px 14px; border-bottom:1px solid rgba(0,0,0,0.06); }
.legend-title{ font-size:18px; font-weight:600; color:#1C1C1E; }
.legend-tools{ display:flex; gap:8px; }
.icon-btn{ all:unset; width:36px; height:32px; display:grid; place-items:center; border-radius:10px; border:1px solid rgba(0,0,0,0.06); color:#6B7280; cursor:pointer; }
.icon-btn:hover{ background:#FAFAFC; }
.icon-btn.danger{ color:#B00020; border-color: rgba(176,0,32,.18); }
.legend-body{ padding:12px; background:#FAFAFB; }
.section-title{ margin:12px 2px 8px; font-size:11px; font-weight:600; letter-spacing:.4px; text-transform:uppercase; color:#6E6E73; }

/* Groupe (liste) */
.list{ background:#FFFFFF; border:1px solid rgba(0,0,0,0.06); border-radius:14px; overflow:hidden; }
.legend-item-btn{ all:unset; width:100%; display:flex; align-items:center; gap:12px; padding:12px 14px; cursor:pointer; font-size:16px; color:#666A70; line-height:1.2; }
.legend-item-btn + .legend-item-btn{ border-top:1px solid rgba(0,0,0,0.06); }
.legend-item-btn:hover{ background:#F8F8FA; }
.legend-item-text{ flex:1; color:#555B61; }
.mark{ width:24px; height:24px; display:flex; align-items:center; justify-content:center; font-size:18px; color:#8A8F98; }
.legend-footer{ display:flex; align-items:center; gap:12px; padding:12px; background:#fff; border-top:1px solid rgba(0,0,0,0.06); }
.footer-btn{ all:unset; flex:1; display:flex; align-items:center; justify-content:space-between; gap:12px; padding:12px 14px; border-radius:14px; color:#fff;
  border: 1px solid #FFF;
  background: radial-gradient(161% 160% at 5.7% -44%, rgba(26,26,26,.5) 0%, rgba(26,26,26,.27) 100%); }
.plus-btn{ width:32px; height:32px; border-radius:10px; display:flex; align-items:center; justify-content:center; background:#e2e2e2; color:#8A8F98; }
"

# ====== JS sticky/collapse ======

script_js <- "
(function(){
  var nav, btn, last=0, delta=10;
  function calcNavHeight(){
    var tb = nav.querySelector('.topbar');
    var ext = nav.querySelector('.expanded');
    var hTop = tb ? tb.offsetHeight : 0;
    var isCollapsed = nav.classList.contains('collapsed');
    var hExt = (!isCollapsed && ext) ? ext.offsetHeight : 0;
    return hTop + hExt;
  }
  function afterLayout(fn){ requestAnimationFrame(()=>requestAnimationFrame(fn)); }
  function applyBodyPadding(){ document.body.style.paddingTop = calcNavHeight() + 'px'; }
  function expandNav(){ nav.classList.remove('collapsed'); if (btn) btn.setAttribute('aria-expanded','true'); afterLayout(applyBodyPadding); }
  function collapseNav(){ if (!nav.classList.contains('collapsed')){ nav.classList.add('collapsed'); if (btn) btn.setAttribute('aria-expanded','false'); afterLayout(applyBodyPadding);} }
  function init(){
    nav = document.getElementById('megaNav'); btn = document.getElementById('collapseBtn'); if(!nav) return;
    expandNav();
    if (btn){
      btn.setAttribute('aria-controls','megaNav'); btn.setAttribute('aria-expanded','true');
      document.addEventListener('click', function(e){ var t = e.target.closest('#collapseBtn'); if(!t) return; if (nav.classList.contains('collapsed')) expandNav(); else collapseNav(); });
    }
    window.addEventListener('wheel', function(){ if (!nav.classList.contains('collapsed')) collapseNav(); }, {passive:true});
    window.addEventListener('scroll', function(){ var st = window.pageYOffset || document.documentElement.scrollTop;
      if (Math.abs(st - last) <= delta){ last = st <= 0 ? 0 : st; return; }
      if (st > last && !nav.classList.contains('collapsed')) collapseNav(); last = st <= 0 ? 0 : st; }, {passive:true});
    let rid; window.addEventListener('resize', function(){ cancelAnimationFrame(rid); rid = requestAnimationFrame(applyBodyPadding); });
    afterLayout(applyBodyPadding);
  }
  document.addEventListener('DOMContentLoaded', init);
})();
"

# ========================= UI =========================
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(styles_css)),
    tags$script(HTML(script_js)),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@24,100,0,0"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=DM+Mono:wght@500&display=swap"
    )
  ),

  # Header / Hero
  div(
    id = "megaNav", class = "mega-nav",
    div(
      class = "topbar",
      div(class = "smallcaps hr-slot", "TESTING VERSION"),
      tags$button(
        id = "collapseBtn", class = "toggle", title = "Collapse/Expand",
        tags$i(class = "bi bi-chevron-up")
      )
    ),
    div(
      class = "expanded",
      div(class = "kicker", "GET STARTED"),
      h1("Genetic Pedigree"),
      p("Visualize, create and analyze family relationships with powerful pedigree tools."),
      div(
        id = "gasses",
        actionButton("create",
          class = "gas silver",
          label = HTML('<span class="number">01</span><div class="symbol">CR</div><p class="name">Create</p>')
        ),
        actionButton("select_list",
          class = "gas silver",
          label = HTML('<span class="number">02</span><div class="symbol">SL</div><p class="name">Select</p>')
        ),
        actionButton("randomPed",
          class = "gas silver",
          label = HTML('<span class="number">03</span><div class="symbol">RP</div><p class="name">Random</p>')
        )
      )
    )
  ),
  br(),
  div(
    id = "center_col",
    h1(style = "font-size:35px; letter-spacing:.3px; margin:0 0 4px 0;", "PEDIGREE BUILDER"),
    h3(uiOutput("pedTitle")),
    p(
      style = "color: transparent; background:#666; -webkit-background-clip:text; background-clip:text; text-shadow:0 3px 3px rgba(255,255,255,.5); font-size:15px;",
      "Draw Pedigree."
    )
  ),

  # ===== LÉGENDES =====
  fluidRow(
    column(
      width = 4,
      # ------- Modèle 1 -------
      div(
        class = "legend-wrap",
        div(
          class = "legend-header",
          div(class = "legend-title", "LEGEND – Model 1"),
          div(
            class = "legend-tools",
            actionButton("undo", tags$span(class = "material-symbols-outlined", "undo"), class = "icon-btn"),
            actionButton("remove", tags$span(class = "material-symbols-outlined", "person_off"), class = "icon-btn danger")
          )
        ),
        div(
          class = "legend-body",
          div(span(class = "section-title", "Gender")),
          div(
            class = "list",
            actionButton("Male", tagList(div(class = "mark", tags$span(class = "material-symbols-outlined", "check_box_outline_blank")), span(class = "legend-item-text", "Male")), class = "legend-item-btn"),
            actionButton("Female", tagList(div(class = "mark", tags$span(class = "material-symbols-outlined", "circle")), span(class = "legend-item-text", "Female")), class = "legend-item-btn"),
            actionButton("Unknown", tagList(div(class = "mark", tags$span(class = "material-symbols-outlined", "thermostat_carbon")), span(class = "legend-item-text", "Unknown")), class = "legend-item-btn")
          ),
          div(class = "section-title", "Status"),
          div(
            class = "list",
            actionButton("Miscarriage", tagList(div(class = "mark", tags$span(class = "material-symbols-outlined", "change_history")), span(class = "legend-item-text", "Miscarriage")), class = "legend-item-btn"),
            actionButton("Adopted", tagList(div(class = "mark", tags$span(class = "material-symbols-outlined", "data_array")), span(class = "legend-item-text", "Adopted")), class = "legend-item-btn"),
            actionButton("Deceased", tagList(div(class = "mark", tags$span(class = "material-symbols-outlined", "frame_person_off")), span(class = "legend-item-text", "Deceased")), class = "legend-item-btn")
          ),
          div(class = "section-title", "Informations"),
          div(
            class = "list",
            actionButton("Proband", tagList(div(class = "mark", tags$span(class = "material-symbols-outlined", "right_click")), span(class = "legend-item-text", "Proband")), class = "legend-item-btn"),
            actionButton("Carrier", tagList(div(class = "mark", tags$span(class = "material-symbols-outlined", "control_camera")), span(class = "legend-item-text", "Carrier")), class = "legend-item-btn"),
            actionButton("starred", tagList(div(class = "mark", tags$span(class = "material-symbols-outlined", "grade")), span(class = "legend-item-text", "Starred")), class = "legend-item-btn")
          )
        ),
        div(
          class = "legend-footer",
          actionButton("newPheno", tagList(span("Add Phenotype"), div(class = "plus-btn", tags$span(class = "material-symbols-outlined", "add"))), class = "footer-btn"),
          div(uiOutput("phenoButtonsUI"))
        )
      )
    ),

    # ------- Modèle 4 : Neumorphism Dark -------
    column(
      width = 4,
      tags$style(HTML("
/* ===== Neumorphism Dark — atténué ===== */
.dneu-wrap{
  --bg:#171a20; --panel:#1b1f26; --edge:#20242c;
  margin:24px auto; padding:14px; border-radius:20px;
  background:var(--bg);
  box-shadow: 6px 6px 14px rgba(0,0,0,.28), -6px -6px 14px rgba(255,255,255,.03);
  border:1px solid rgba(255,255,255,.035);
  color:#cfd5df;
}
.dneu-inner{
  background:var(--panel); border-radius:16px; overflow:hidden; padding:10px;
  box-shadow: inset 2px 2px 4px rgba(0,0,0,.20), inset -2px -2px 4px rgba(255,255,255,.03);
  color:#d8dee7;
}
.dneu-header{ display:flex; align-items:center; justify-content:space-between;
  padding:10px 12px; border-radius:12px;
  background:linear-gradient(180deg,#1e222a 0%, #191d24 100%);
  border:1px solid rgba(255,255,255,.035);
  box-shadow: 0 1px 0 rgba(255,255,255,.02) inset, 0 8px 16px rgba(0,0,0,.18);
}
.dneu-title{ font-weight:700; font-size:15px; letter-spacing:.18px; color:#eef2f8; }
.dneu-tools{ display:flex; gap:8px; }
.dneu-btn{
  all:unset; width:38px; height:34px; border-radius:10px; display:grid; place-items:center; cursor:pointer;
  color:#aab2bf;
  background:linear-gradient(180deg,#232833,#191d24);
  border:1px solid rgba(255,255,255,.035);
  box-shadow: 1px 1px 2px rgba(0,0,0,.25), -1px -1px 2px rgba(255,255,255,.03);
  transition: filter .12s ease, transform .06s ease;
}
.dneu-btn:hover{ filter:brightness(1.04); }
.dneu-btn:active{ transform:translateY(1px); }
.dneu-btn.danger{ color:#ffb6bf; }
.dneu-section{ font-size:10px; color:#8f97a6; letter-spacing:.35em; margin:14px 8px 8px; text-transform:uppercase; }
.dneu-list{ background:#1a1e25; border-radius:14px; overflow:hidden; border:1px solid rgba(255,255,255,.03);
  box-shadow: inset 1px 1px 2px rgba(0,0,0,.20), inset -1px -1px 2px rgba(255,255,255,.02);
}
.dneu-item{ all:unset; display:flex; align-items:center; gap:12px; width:100%; cursor:pointer; padding:12px 14px; color:#e3e8f1; }
.dneu-item + .dneu-item{ border-top:1px solid rgba(255,255,255,.035); }
.dneu-item:hover{ background:rgba(255,255,255,.03); }
.dneu-mark{ width:26px; height:26px; border-radius:10px; display:flex; align-items:center; justify-content:center; flex:0 0 26px;
  color:#b7c0ce; background:linear-gradient(180deg,#212632,#191d24); border:1px solid rgba(255,255,255,.04);
  box-shadow: 1px 1px 2px rgba(0,0,0,.25), -1px -1px 2px rgba(255,255,255,.02);
}
.dneu-text{ flex:1; color:#d3d9e4; letter-spacing:.12px; }
.dneu-footer{ display:flex; gap:10px; padding:12px 2px; }
.dneu-cta{ all:unset; flex:1; display:flex; align-items:center; justify-content:space-between; gap:10px; cursor:pointer; padding:12px 14px;
  color:#f5f8ff; font-weight:700; border-radius:12px;
  background:linear-gradient(180deg,#202533,#181c24);
  border:1px solid rgba(255,255,255,.04);
  box-shadow: 0 1px 0 rgba(255,255,255,.02) inset, 0 8px 16px rgba(0,0,0,.20);
}
.dneu-plus{ width:32px; height:32px; border-radius:10px; display:flex; align-items:center; justify-content:center;
  background:#232836; color:#dfe5ef; border:1px solid rgba(255,255,255,.04);
  box-shadow: 1px 1px 2px rgba(0,0,0,.25), -1px -1px 2px rgba(255,255,255,.02);
}
")),
      div(
        class = "dneu-wrap",
        div(
          class = "dneu-inner",
          div(
            class = "dneu-header",
            div(class = "dneu-title", "LEGEND – Neumorphism Dark"),
            div(
              class = "dneu-tools",
              actionButton("undo4", tags$span(class = "material-symbols-outlined", "undo"), class = "dneu-btn"),
              actionButton("remove4", tags$span(class = "material-symbols-outlined", "person_off"), class = "dneu-btn danger")
            )
          ),
          div(class = "dneu-section", "Gender"),
          div(
            class = "dneu-list",
            actionButton("Male4", tagList(div(class = "dneu-mark", tags$span(class = "material-symbols-outlined", "check_box_outline_blank")), span(class = "dneu-text", "Male")), class = "dneu-item"),
            actionButton("Female4", tagList(div(class = "dneu-mark", tags$span(class = "material-symbols-outlined", "circle")), span(class = "dneu-text", "Female")), class = "dneu-item"),
            actionButton("Unknown4", tagList(div(class = "dneu-mark", tags$span(class = "material-symbols-outlined", "thermostat_carbon")), span(class = "dneu-text", "Unknown")), class = "dneu-item")
          ),
          div(class = "dneu-section", "Status"),
          div(
            class = "dneu-list",
            actionButton("Miscarriage4", tagList(div(class = "dneu-mark", tags$span(class = "material-symbols-outlined", "change_history")), span(class = "dneu-text", "Miscarriage")), class = "dneu-item"),
            actionButton("Adopted4", tagList(div(class = "dneu-mark", tags$span(class = "material-symbols-outlined", "data_array")), span(class = "dneu-text", "Adopted")), class = "dneu-item"),
            actionButton("Deceased4", tagList(div(class = "dneu-mark", tags$span(class = "material-symbols-outlined", "frame_person_off")), span(class = "dneu-text", "Deceased")), class = "dneu-item")
          ),
          div(class = "dneu-section", "Informations"),
          div(
            class = "dneu-list",
            actionButton("Proband4", tagList(div(class = "dneu-mark", tags$span(class = "material-symbols-outlined", "right_click")), span(class = "dneu-text", "Proband")), class = "dneu-item"),
            actionButton("Carrier4", tagList(div(class = "dneu-mark", tags$span(class = "material-symbols-outlined", "control_camera")), span(class = "dneu-text", "Carrier")), class = "dneu-item"),
            actionButton("Starred4", tagList(div(class = "dneu-mark", tags$span(class = "material-symbols-outlined", "grade")), span(class = "dneu-text", "Starred")), class = "dneu-item")
          ),
          div(
            class = "dneu-footer",
            actionButton("newPheno4", tagList(span("Add Phenotype"), div(class = "dneu-plus", tags$span(class = "material-symbols-outlined", "add"))), class = "dneu-cta")
          )
        )
      )
    ),

    # ------- Modèle 5 : Dark Neon -------
    column(
      width = 4,
      tags$style(HTML("
/* ===== Neumorphism Light (clair, doux) ===== */
.nl-wrap{
  --bg:#f5f7fb; --panel:#ffffff; --edge:#e9edf5; --text:#2e3340;
  margin:24px auto; padding:14px; border-radius:18px;
  background:var(--bg); color:var(--text);
  border:1px solid #eef1f6;
  box-shadow:
    12px 12px 24px rgba(0,0,0,.05),
    -12px -12px 24px rgba(255,255,255,.75);
}
.nl-inner{
  background:var(--panel); border-radius:16px; overflow:hidden; padding:10px;
  border:1px solid #eef1f6;
  box-shadow:
    inset 2px 2px 6px rgba(0,0,0,.05),
    inset -2px -2px 6px rgba(255,255,255,.55);
}

/* Header légèrement bombé */
.nl-header{
  display:flex; align-items:center; justify-content:space-between;
  padding:10px 12px; border-radius:12px;
  background:linear-gradient(180deg,#ffffff 0%, #f4f7fb 100%);
  border:1px solid #e9edf5;
  box-shadow:
    inset 1px 1px 2px rgba(255,255,255,.8),
    inset -1px -1px 2px rgba(0,0,0,.02),
    6px 6px 16px rgba(0,0,0,.06),
    -6px -6px 16px rgba(255,255,255,.8);
}
.nl-title{ font-weight:700; font-size:14px; letter-spacing:.18em; color:#2e3340; }
.nl-tools{ display:flex; gap:8px; }
.nl-btn{
  all:unset; width:38px; height:34px; border-radius:10px; display:grid; place-items:center; cursor:pointer;
  color:#3a4150;
  background:linear-gradient(180deg,#ffffff,#eef2f8);
  border:1px solid #e6ebf3;
  box-shadow:
    2px 2px 6px rgba(0,0,0,.06),
    -2px -2px 6px rgba(255,255,255,.8),
    inset 1px 1px 2px rgba(255,255,255,.7),
    inset -1px -1px 2px rgba(0,0,0,.02);
  transition: filter .12s ease, transform .06s ease;
}
.nl-btn:hover{ filter:brightness(1.03); }
.nl-btn:active{ transform:translateY(1px); }
.nl-btn.danger{ color:#b00020; }

/* Sections groupées */
.nl-section{ font-size:10px; color:#6b7383; letter-spacing:.35em; margin:14px 8px 8px; text-transform:uppercase; }
.nl-list{
  background:#ffffff; border-radius:14px; overflow:hidden;
  border:1px solid #e8ecf3;
  box-shadow:
    inset 1px 1px 2px rgba(0,0,0,.03),
    inset -1px -1px 2px rgba(255,255,255,.7);
}
.nl-item{
  all:unset; width:100%; display:flex; align-items:center; gap:12px;
  padding:12px 14px; cursor:pointer; color:#2e3340;
  background:linear-gradient(180deg,#ffffff 0%, #f6f8fc 100%);
}
.nl-item + .nl-item{ border-top:1px solid #edf1f6; }
.nl-item:hover{
  background:linear-gradient(180deg,#ffffff 0%, #eff3f9 100%);
  filter:brightness(1.01);
}

.nl-mark{
  width:26px; height:26px; border-radius:10px; display:flex; align-items:center; justify-content:center; flex:0 0 26px;
  color:#495366;
  background:linear-gradient(180deg,#ffffff,#eef2f8);
  border:1px solid #e6ebf3;
  box-shadow:
    1px 1px 3px rgba(0,0,0,.06),
    -1px -1px 3px rgba(255,255,255,.8),
    inset 1px 1px 2px rgba(255,255,255,.7),
    inset -1px -1px 2px rgba(0,0,0,.02);
}
.nl-text{ flex:1; color:#2f3442; letter-spacing:.12px; }

/* CTA */
.nl-cta{
  all:unset; width:100%; display:flex; align-items:center; justify-content:space-between; gap:12px; cursor:pointer; padding:14px 16px; margin-top:12px;
  border-radius:12px; color:#2e3340; font-weight:800; letter-spacing:.12em;
  background:linear-gradient(180deg,#ffffff 0%, #eef2f8 100%);
  border:1px solid #e6ebf3;
  box-shadow:
    2px 2px 10px rgba(0,0,0,.06),
    -2px -2px 10px rgba(255,255,255,.8),
    inset 1px 1px 2px rgba(255,255,255,.7),
    inset -1px -1px 2px rgba(0,0,0,.02);
}
.nl-plus{
  width:34px; height:34px; border-radius:10px; display:flex; align-items:center; justify-content:center;
  background:linear-gradient(180deg,#ffffff,#eef2f8); color:#2e3340; border:1px solid #e6ebf3;
  box-shadow:
    1px 1px 3px rgba(0,0,0,.06),
    -1px -1px 3px rgba(255,255,255,.8),
    inset 1px 1px 2px rgba(255,255,255,.7),
    inset -1px -1px 2px rgba(0,0,0,.02);
}
      ")),
      div(
        class = "nl-wrap",
        div(
          class = "nl-inner",
          div(
            class = "nl-header",
            div(class = "nl-title", "LEGEND – Neumorphism Light"),
            div(
              class = "nl-tools",
              actionButton("undo5", tags$span(class = "material-symbols-outlined", "undo"), class = "nl-btn"),
              actionButton("remove5", tags$span(class = "material-symbols-outlined", "person_off"), class = "nl-btn danger")
            )
          ),
          div(class = "nl-section", "Gender"),
          div(
            class = "nl-list",
            actionButton("Male5", tagList(div(class = "nl-mark", tags$span(class = "material-symbols-outlined", "check_box_outline_blank")), span(class = "nl-text", "Male")), class = "nl-item"),
            actionButton("Female5", tagList(div(class = "nl-mark", tags$span(class = "material-symbols-outlined", "circle")), span(class = "nl-text", "Female")), class = "nl-item"),
            actionButton("Unknown5", tagList(div(class = "nl-mark", tags$span(class = "material-symbols-outlined", "thermostat_carbon")), span(class = "nl-text", "Unknown")), class = "nl-item")
          ),
          div(class = "nl-section", "Status"),
          div(
            class = "nl-list",
            actionButton("Miscarriage5", tagList(div(class = "nl-mark", tags$span(class = "material-symbols-outlined", "change_history")), span(class = "nl-text", "Miscarriage")), class = "nl-item"),
            actionButton("Adopted5", tagList(div(class = "nl-mark", tags$span(class = "material-symbols-outlined", "data_array")), span(class = "nl-text", "Adopted")), class = "nl-item"),
            actionButton("Deceased5", tagList(div(class = "nl-mark", tags$span(class = "material-symbols-outlined", "frame_person_off")), span(class = "nl-text", "Deceased")), class = "nl-item")
          ),
          div(class = "nl-section", "Informations"),
          div(
            class = "nl-list",
            actionButton("Proband5", tagList(div(class = "nl-mark", tags$span(class = "material-symbols-outlined", "right_click")), span(class = "nl-text", "Proband")), class = "nl-item"),
            actionButton("Carrier5", tagList(div(class = "nl-mark", tags$span(class = "material-symbols-outlined", "control_camera")), span(class = "nl-text", "Carrier")), class = "nl-item"),
            actionButton("Starred5", tagList(div(class = "nl-mark", tags$span(class = "material-symbols-outlined", "grade")), span(class = "nl-text", "Starred")), class = "nl-item")
          ),
          actionButton("newPheno5", tagList(span("Add Phenotype"), div(class = "nl-plus", tags$span(class = "material-symbols-outlined", "add"))), class = "nl-cta")
        )
      )
    )
  ),
  fluidRow(
    # ------- Modèle 6 : Mixed (fond sombre + boutons clairs groupés) -------
    column(
      width = 6,
      tags$style(HTML("
.mixdark-wrap{
  --g1:#1b1e23; --g2:#0f1115; --txt:#bfc5ce;
  margin:24px auto; padding:14px; border-radius:18px;
  background: linear-gradient(180deg,var(--g1),var(--g2)); color:var(--txt);
  border:1px solid rgba(255,255,255,.06); box-shadow: 0 22px 50px rgba(0,0,0,.35);
}
.mixdark-header{ display:flex; align-items:center; justify-content:space-between; padding:12px 14px; }
.mixdark-title{ font-weight:600; letter-spacing:.18em; color:#e6eaef; }
.mixdark-tools{ display:flex; gap:8px; }
.mixlight-btn{ all:unset; width:42px; height:36px; border-radius:12px; display:grid; place-items:center; cursor:pointer;
  background: radial-gradient(100% 100% at 50% 10%, #fff 0%, #eff1f5 70%, #e3e6ea 100%);
  color:#1c1f24; border:1px solid rgba(0,0,0,.08);
  box-shadow: 0 1px 0 #fff inset, 0 10px 24px rgba(0,0,0,.15); }
.mixlight-btn:hover{ filter:brightness(1.03); }

.mixdark-section{ font-size:11px; color:#aab0ba; text-transform:uppercase; letter-spacing:.3em; margin:12px 8px 8px; }

/* 👉 Groupe sans gap : même logique que Model 1 */
.mixdark-list{
  background:#ffffff; border:1px solid rgba(0,0,0,.06);
  border-radius:14px; overflow:hidden;
}
.mixdark-item{
  all:unset; display:flex; align-items:center; gap:12px; padding:12px 14px; width:100%; cursor:pointer;
  background: radial-gradient(100% 100% at 50% 0%, #fff 0%, #f4f6f9 70%, #e8ecf2 100%);
  color:#222; line-height:1.2;
}
.mixdark-item + .mixdark-item{ border-top:1px solid rgba(0,0,0,.06); }
.mixdark-item:hover{ filter:brightness(1.02); }
.mixdark-mark{ width:26px; height:26px; display:flex; align-items:center; justify-content:center; border-radius:8px; background:#f0f2f6; color:#3a3f46; flex:0 0 26px; }
.mixdark-text{ flex:1; color:#333; letter-spacing:.15px; }

.mixdark-cta{ all:unset; width:100%; display:flex; align-items:center; justify-content:space-between; gap:12px; cursor:pointer; padding:14px 16px; margin-top:12px;
  border-radius:12px; color:#1b1e23; font-weight:700;
  background: radial-gradient(100% 100% at 50% 0%, #fff 0%, #eef1f6 70%, #e2e6ec 100%);
  border:1px solid rgba(0,0,0,.08); box-shadow: 0 1px 0 #fff inset, 0 10px 24px rgba(0,0,0,.12); }
.mixdark-plus{ width:34px; height:34px; border-radius:10px; display:flex; align-items:center; justify-content:center; background:#e9edf3; color:#2b2f36; }
      ")),
      div(
        class = "mixdark-wrap",
        div(
          class = "mixdark-header",
          div(class = "mixdark-title", "LEGEND – Mixed: dark gradient + light glossy buttons"),
          div(
            class = "mixdark-tools",
            actionButton("undo6", tags$span(class = "material-symbols-outlined", "undo"), class = "mixlight-btn"),
            actionButton("remove6", tags$span(class = "material-symbols-outlined", "person_off"), class = "mixlight-btn")
          )
        ),
        div(class = "mixdark-section", "Gender"),
        div(
          class = "mixdark-list",
          actionButton("Male6", tagList(div(class = "mixdark-mark", tags$span(class = "material-symbols-outlined", "check_box_outline_blank")), span(class = "mixdark-text", "Male")), class = "mixdark-item"),
          actionButton("Female6", tagList(div(class = "mixdark-mark", tags$span(class = "material-symbols-outlined", "circle")), span(class = "mixdark-text", "Female")), class = "mixdark-item"),
          actionButton("Unknown6", tagList(div(class = "mixdark-mark", tags$span(class = "material-symbols-outlined", "thermostat_carbon")), span(class = "mixdark-text", "Unknown")), class = "mixdark-item")
        ),
        div(class = "mixdark-section", "Status"),
        div(
          class = "mixdark-list",
          actionButton("Miscarriage6", tagList(div(class = "mixdark-mark", tags$span(class = "material-symbols-outlined", "change_history")), span(class = "mixdark-text", "Miscarriage")), class = "mixdark-item"),
          actionButton("Adopted6", tagList(div(class = "mixdark-mark", tags$span(class = "material-symbols-outlined", "data_array")), span(class = "mixdark-text", "Adopted")), class = "mixdark-item"),
          actionButton("Deceased6", tagList(div(class = "mixdark-mark", tags$span(class = "material-symbols-outlined", "frame_person_off")), span(class = "mixdark-text", "Deceased")), class = "mixdark-item")
        ),
        div(class = "mixdark-section", "Informations"),
        div(
          class = "mixdark-list",
          actionButton("Proband6", tagList(div(class = "mixdark-mark", tags$span(class = "material-symbols-outlined", "right_click")), span(class = "mixdark-text", "Proband")), class = "mixdark-item"),
          actionButton("Carrier6", tagList(div(class = "mixdark-mark", tags$span(class = "material-symbols-outlined", "control_camera")), span(class = "mixdark-text", "Carrier")), class = "mixdark-item"),
          actionButton("Starred6", tagList(div(class = "mixdark-mark", tags$span(class = "material-symbols-outlined", "grade")), span(class = "mixdark-text", "Starred")), class = "mixdark-item")
        ),
        actionButton("newPheno6", tagList(span("Add Phenotype"), div(class = "mixdark-plus", tags$span(class = "material-symbols-outlined", "add"))), class = "mixdark-cta")
      )
    ),

    # ------- Modèle 7 : Mixed Light (fond clair + boutons foncés groupés) -------
    column(
      width = 6,
      tags$style(HTML("
.mixlight2-wrap{
  --bg:#f6f7fb; --panel:#ffffff; --btn:#1b1f2b; --btn2:#0e1118; --txt:#343a45;
  margin:24px auto; padding:14px; border-radius:18px; background:var(--bg);
  border:1px solid rgba(0,0,0,.06); box-shadow: 0 10px 28px rgba(0,0,0,.10);
}
.mixlight2-header{ display:flex; align-items:center; justify-content:space-between; padding:12px 14px; }
.mixlight2-title{ font-weight:700; color:#2a2f38; letter-spacing:.15em; }
.mixlight2-tools{ display:flex; gap:10px; }
.darkpill{ all:unset; width:44px; height:36px; border-radius:12px; display:grid; place-items:center; cursor:pointer; color:#f3f6fb;
  background: linear-gradient(180deg,#232a3a,#0f1320); border:1px solid rgba(0,0,0,.35);
  box-shadow: 0 3px 10px rgba(0,0,0,.25); }
.darkpill:hover{ filter:brightness(1.05); }

.mixlight2-section{ font-size:11px; color:#606774; letter-spacing:.25em; text-transform:uppercase; margin:12px 8px 8px; }

/* 👉 Groupe sans gap, comme Model 1 */
.ml2-list{
  background:#ffffff; border:1px solid rgba(0,0,0,.06);
  border-radius:14px; overflow:hidden;
}
.ml2-item{
  all:unset; display:flex; align-items:center; gap:12px; padding:12px 14px; width:100%; cursor:pointer;
  background:#fff; color:#2d333d; line-height:1.2;
  box-shadow: 0 1px 0 #fff inset;
}
.ml2-item + .ml2-item{ border-top:1px solid rgba(0,0,0,.06); }
.ml2-item:hover{ background:#f7f9fc; }
.ml2-mark{ width:26px; height:26px; display:flex; align-items:center; justify-content:center; border-radius:8px; background:#e9edf5; color:#2b2f38; flex:0 0 26px; }
.ml2-text{ flex:1; color:#2f3440; letter-spacing:.12px; }

.ml2-cta{ all:unset; width:100%; display:flex; align-items:center; justify-content:space-between; gap:12px; cursor:pointer; padding:14px 16px; margin-top:12px;
  border-radius:12px; color:#f4f6fb; font-weight:700;
  background: linear-gradient(180deg,#232a3a,#0f1320); border:1px solid rgba(0,0,0,.35);
  box-shadow: 0 3px 12px rgba(0,0,0,.25); }
.ml2-plus{ width:34px; height:34px; border-radius:10px; display:flex; align-items:center; justify-content:center; background:#0b0f19; color:#e9eef7; }
      ")),
      div(
        class = "mixlight2-wrap",
        div(
          class = "mixlight2-header",
          div(class = "mixlight2-title", "LEGEND – Mixed: light background + dark buttons"),
          div(
            class = "mixlight2-tools",
            actionButton("undo7", tags$span(class = "material-symbols-outlined", "undo"), class = "darkpill"),
            actionButton("remove7", tags$span(class = "material-symbols-outlined", "person_off"), class = "darkpill")
          )
        ),
        div(class = "mixlight2-section", "Gender"),
        div(
          class = "ml2-list",
          actionButton("Male7", tagList(div(class = "ml2-mark", tags$span(class = "material-symbols-outlined", "check_box_outline_blank")), span(class = "ml2-text", "Male")), class = "ml2-item"),
          actionButton("Female7", tagList(div(class = "ml2-mark", tags$span(class = "material-symbols-outlined", "circle")), span(class = "ml2-text", "Female")), class = "ml2-item"),
          actionButton("Unknown7", tagList(div(class = "ml2-mark", tags$span(class = "material-symbols-outlined", "thermostat_carbon")), span(class = "ml2-text", "Unknown")), class = "ml2-item")
        ),
        div(class = "mixlight2-section", "Status"),
        div(
          class = "ml2-list",
          actionButton("Miscarriage7", tagList(div(class = "ml2-mark", tags$span(class = "material-symbols-outlined", "change_history")), span(class = "ml2-text", "Miscarriage")), class = "ml2-item"),
          actionButton("Adopted7", tagList(div(class = "ml2-mark", tags$span(class = "material-symbols-outlined", "data_array")), span(class = "ml2-text", "Adopted")), class = "ml2-item"),
          actionButton("Deceased7", tagList(div(class = "ml2-mark", tags$span(class = "material-symbols-outlined", "frame_person_off")), span(class = "ml2-text", "Deceased")), class = "ml2-item")
        ),
        div(class = "mixlight2-section", "Informations"),
        div(
          class = "ml2-list",
          actionButton("Proband7", tagList(div(class = "ml2-mark", tags$span(class = "material-symbols-outlined", "right_click")), span(class = "ml2-text", "Proband")), class = "ml2-item"),
          actionButton("Carrier7", tagList(div(class = "ml2-mark", tags$span(class = "material-symbols-outlined", "control_camera")), span(class = "ml2-text", "Carrier")), class = "ml2-item"),
          actionButton("Starred7", tagList(div(class = "ml2-mark", tags$span(class = "material-symbols-outlined", "grade")), span(class = "ml2-text", "Starred")), class = "ml2-item")
        ),
        actionButton("newPheno7", tagList(span("Add Phenotype"), div(class = "ml2-plus", tags$span(class = "material-symbols-outlined", "add"))), class = "ml2-cta")
      )
    )
  )
)

server <- function(input, output, session) {
  output$pedTitle <- renderText("")
}

shinyApp(ui, server)
