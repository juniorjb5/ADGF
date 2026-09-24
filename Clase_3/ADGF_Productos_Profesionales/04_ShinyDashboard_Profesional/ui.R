library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

# -----------------------------------------------------------------------------
# UI | Dashboard financiero profesional
# La lógica de cálculo está separada en server.R.
# -----------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      tags$b("ADGF"),
      tags$span(" | Financial Lab", style = "font-weight:300;")
    ),
    titleWidth = 275,
    dropdownMenu(
      type = "notifications",
      badgeStatus = "warning",
      notificationItem(
        text = "Datos sintéticos para fines pedagógicos",
        icon = icon("info-circle"),
        status = "info"
      )
    )
  ),

  dashboardSidebar(
    width = 275,

    tags$div(
      class = "sidebar-brand-block",
      tags$div(class = "brand-kicker", "PORTAFOLIO ANDINA"),
      tags$div(class = "brand-sub", "Análisis de desempeño y riesgo")
    ),

    tags$div(
      class = "filter-panel",
      selectInput(
        "activo",
        "Activo / portafolio",
        choices = NULL
      ),
      dateRangeInput(
        "rango",
        "Rango de análisis",
        start = as.Date("2018-01-01"),
        end = as.Date("2025-12-01"),
        min = as.Date("2018-01-01"),
        max = as.Date("2025-12-01"),
        format = "yyyy-mm-dd"
      ),
      actionButton(
        "reset_filtros",
        "Restablecer filtros",
        icon = icon("refresh"),
        class = "btn-reset"
      ),
      tags$div(style = "height:8px;"),
      downloadButton(
        "descargar_csv",
        "Descargar datos filtrados",
        class = "btn-download"
      )
    ),

    sidebarMenu(
      id = "tabs",
      menuItem("Resumen ejecutivo", tabName = "resumen", icon = icon("dashboard")),
      menuItem("Desempeño", tabName = "desempeno", icon = icon("line-chart")),
      menuItem("Riesgo", tabName = "riesgo", icon = icon("shield")),
      menuItem("Análisis corporativo", tabName = "corporativo", icon = icon("building")),
      menuItem("Explorador de datos", tabName = "datos", icon = icon("table")),
      menuItem("Metodología", tabName = "metodologia", icon = icon("info-circle"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        :root {
          --fin-navy: #102d44;
          --fin-blue: #1f5c8b;
          --fin-teal: #13706f;
          --fin-gold: #b9822b;
          --fin-bg: #f3f6f9;
          --fin-card: #ffffff;
          --fin-line: #dce5eb;
          --fin-text: #22313f;
          --fin-muted: #6b7c8c;
        }

        body, .content-wrapper, .right-side {
          background: var(--fin-bg) !important;
          color: var(--fin-text);
        }

        .main-header .logo {
          background: #0d2639 !important;
          color: #fff !important;
          font-size: 17px;
          letter-spacing: .2px;
        }

        .main-header .navbar {
          background: linear-gradient(90deg, #102d44 0%, #1f5c8b 70%, #13706f 100%) !important;
        }

        .main-sidebar {
          background: #102d44 !important;
        }

        .sidebar-menu > li > a {
          color: #d7e3eb !important;
          border-left: 3px solid transparent !important;
        }

        .sidebar-menu > li.active > a,
        .sidebar-menu > li:hover > a {
          background: #173b56 !important;
          color: #fff !important;
          border-left-color: #2db0a9 !important;
        }

        .sidebar-brand-block {
          padding: 20px 17px 13px 17px;
          border-bottom: 1px solid rgba(255,255,255,.10);
        }

        .brand-kicker {
          color: #ffffff;
          font-weight: 700;
          letter-spacing: .08em;
          font-size: 12px;
        }

        .brand-sub {
          color: #a9bdca;
          font-size: 12px;
          margin-top: 4px;
        }

        .filter-panel {
          padding: 15px 15px 8px 15px;
          color: #d7e3eb;
        }

        .filter-panel label {
          color: #d7e3eb;
          font-size: 12px;
          font-weight: 600;
        }

        .btn-reset, .btn-download {
          width: 100%;
          border-radius: 6px;
          border: 0;
        }

        .btn-reset {
          background: #254b66;
          color: white;
        }

        .btn-download {
          background: #13706f;
          color: white;
        }

        .content-header > h1 {
          font-size: 25px;
          color: var(--fin-navy);
          font-weight: 700;
        }

        .content-header > h1 > small {
          color: var(--fin-muted);
          font-weight: 400;
        }

        .box {
          border-radius: 10px;
          border-top: 0;
          box-shadow: 0 4px 18px rgba(16,45,68,.08);
          overflow: hidden;
        }

        .box-header {
          padding: 14px 16px;
          border-bottom: 1px solid var(--fin-line);
        }

        .box-header .box-title {
          font-size: 15px;
          font-weight: 700;
          color: var(--fin-navy);
        }

        .small-box {
          border-radius: 10px;
          box-shadow: 0 4px 18px rgba(16,45,68,.09);
        }

        .small-box h3 {
          font-size: 28px;
        }

        .small-box p {
          font-size: 13px;
        }

        .bg-aqua { background: #1f5c8b !important; }
        .bg-green { background: #13706f !important; }
        .bg-yellow { background: #b9822b !important; }
        .bg-red { background: #8f3c3c !important; }

        .executive-note {
          background: white;
          border-left: 5px solid var(--fin-teal);
          border-radius: 8px;
          padding: 14px 18px;
          margin-bottom: 18px;
          box-shadow: 0 3px 12px rgba(16,45,68,.06);
        }

        .executive-note strong { color: var(--fin-navy); }
        .text-muted-fin { color: var(--fin-muted); }

        .method-card {
          background: white;
          border: 1px solid var(--fin-line);
          border-radius: 10px;
          padding: 20px 22px;
          margin-bottom: 16px;
          box-shadow: 0 3px 12px rgba(16,45,68,.05);
        }

        .method-card h4 {
          margin-top: 0;
          color: var(--fin-navy);
          font-weight: 700;
        }

        .dataTables_wrapper {
          font-size: 12px;
        }

        @media (max-width: 767px) {
          .main-header .logo { width: 100% !important; }
        }
      "))
    ),

    tabItems(
      tabItem(
        tabName = "resumen",
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "executive-note",
              tags$strong("Resumen ejecutivo. "),
              tags$span("Los indicadores se recalculan con el activo y el rango seleccionados. Las figuras son interactivas y permiten explorar el detalle sin abandonar el tablero.")
            )
          )
        ),
        fluidRow(
          valueBoxOutput("vb_retorno", width = 3),
          valueBoxOutput("vb_vol", width = 3),
          valueBoxOutput("vb_sharpe", width = 3),
          valueBoxOutput("vb_dd", width = 3)
        ),
        fluidRow(
          box(
            title = "Evolución acumulada",
            width = 8,
            status = "primary",
            solidHeader = FALSE,
            plotlyOutput("plot_indice", height = 365)
          ),
          box(
            title = "Distribución de retornos",
            width = 4,
            status = "primary",
            solidHeader = FALSE,
            plotlyOutput("plot_hist", height = 365)
          )
        ),
        fluidRow(
          box(
            title = "Mapa riesgo-retorno",
            width = 6,
            plotlyOutput("plot_riesgo_retorno", height = 320)
          ),
          box(
            title = "Lectura del periodo seleccionado",
            width = 6,
            uiOutput("lectura_ejecutiva"),
            hr(),
            fluidRow(
              column(6, tags$b("VaR histórico 95%"), tags$div(textOutput("txt_var"), class = "text-muted-fin")),
              column(6, tags$b("CVaR histórico 95%"), tags$div(textOutput("txt_cvar"), class = "text-muted-fin"))
            )
          )
        )
      ),

      tabItem(
        tabName = "desempeno",
        fluidRow(
          box(
            title = "Activo seleccionado vs. portafolio equiponderado",
            width = 12,
            plotlyOutput("plot_comparacion", height = 410)
          )
        ),
        fluidRow(
          box(
            title = "Retornos mensuales",
            width = 6,
            plotlyOutput("plot_retornos", height = 330)
          ),
          box(
            title = "Volatilidad móvil de 12 meses",
            width = 6,
            plotlyOutput("plot_vol_movil", height = 330)
          )
        )
      ),

      tabItem(
        tabName = "riesgo",
        fluidRow(
          box(
            title = "Drawdown",
            width = 7,
            plotlyOutput("plot_drawdown", height = 360)
          ),
          box(
            title = "VaR y cola izquierda",
            width = 5,
            plotlyOutput("plot_var", height = 360)
          )
        ),
        fluidRow(
          box(
            title = "Correlación entre activos",
            width = 7,
            plotlyOutput("plot_cor", height = 390)
          ),
          box(
            title = "Métricas de riesgo por activo",
            width = 5,
            DTOutput("tabla_riesgo")
          )
        )
      ),

      tabItem(
        tabName = "corporativo",
        fluidRow(
          valueBoxOutput("vb_margen", width = 3),
          valueBoxOutput("vb_roa", width = 3),
          valueBoxOutput("vb_deuda", width = 3),
          valueBoxOutput("vb_liquidez", width = 3)
        ),
        fluidRow(
          box(
            title = "Rentabilidad, apalancamiento y tamaño",
            width = 7,
            plotlyOutput("plot_corporativo", height = 390)
          ),
          box(
            title = "Indicadores corporativos",
            width = 5,
            DTOutput("tabla_corporativa")
          )
        )
      ),

      tabItem(
        tabName = "datos",
        fluidRow(
          box(
            title = "Observaciones del activo seleccionado",
            width = 12,
            DTOutput("tabla_datos")
          )
        )
      ),

      tabItem(
        tabName = "metodologia",
        fluidRow(
          column(
            width = 6,
            tags$div(
              class = "method-card",
              tags$h4("Arquitectura del producto"),
              tags$p("Datos → transformación → métricas → visualización → interacción."),
              tags$p("El tablero no contiene resultados pegados manualmente: todos los componentes dependen del mismo flujo de datos y reaccionan a los filtros del usuario.")
            ),
            tags$div(
              class = "method-card",
              tags$h4("Frecuencia y anualización"),
              tags$p(HTML("Los retornos tienen frecuencia <b>mensual</b>. La volatilidad anual se obtiene como <i>SD mensual × √12</i>.")),
              tags$p("Cambiar la frecuencia de los datos requiere revisar la regla de anualización y la interpretación de cada métrica.")
            )
          ),
          column(
            width = 6,
            tags$div(
              class = "method-card",
              tags$h4("Métricas"),
              tags$ul(
                tags$li(tags$b("Retorno anualizado:"), " crecimiento compuesto equivalente."),
                tags$li(tags$b("Volatilidad:"), " dispersión anual equivalente."),
                tags$li(tags$b("Sharpe:"), " retorno por unidad de volatilidad; aquí se usa rf = 0."),
                tags$li(tags$b("Drawdown:"), " caída desde un máximo previo."),
                tags$li(tags$b("VaR / CVaR:"), " umbral histórico y pérdida promedio en la cola.")
              )
            ),
            tags$div(
              class = "method-card",
              tags$h4("Nota pedagógica"),
              tags$p(HTML("Los datos son <b>sintéticos</b>. El tablero está diseñado para mostrar estructura, reproducibilidad e interacción; no constituye una recomendación de inversión."))
            )
          )
        )
      )
    )
  )
)

ui
