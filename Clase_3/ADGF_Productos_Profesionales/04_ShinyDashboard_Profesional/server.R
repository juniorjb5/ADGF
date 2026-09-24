library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales)

# -----------------------------------------------------------------------------
# SERVER | Dashboard financiero profesional
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  # ---------- 1. Carga y preparación de datos ----------
  ruta_retornos <- if (file.exists("../data/retornos_activos.csv")) {
    "../data/retornos_activos.csv"
  } else {
    "data/retornos_activos.csv"
  }

  ruta_corporativo <- if (file.exists("../data/indicadores_empresas.csv")) {
    "../data/indicadores_empresas.csv"
  } else {
    "data/indicadores_empresas.csv"
  }

  retornos_wide <- readr::read_csv(ruta_retornos, show_col_types = FALSE)
  corporativo <- readr::read_csv(ruta_corporativo, show_col_types = FALSE)

  retornos_long <- retornos_wide |>
    mutate(fecha = as.Date(fecha)) |>
    pivot_longer(-fecha, names_to = "activo", values_to = "retorno")

  portafolio_base <- retornos_long |>
    group_by(fecha) |>
    summarise(retorno = mean(retorno), .groups = "drop") |>
    mutate(activo = "PORTAFOLIO_EW")

  panel_base <- bind_rows(retornos_long, portafolio_base)

  activos_disponibles <- c(
    "PORTAFOLIO_EW",
    sort(setdiff(unique(panel_base$activo), "PORTAFOLIO_EW"))
  )

  updateSelectInput(
    session,
    "activo",
    choices = activos_disponibles,
    selected = "PORTAFOLIO_EW"
  )

  # ---------- 2. Filtros ----------
  observeEvent(input$reset_filtros, {
    updateSelectInput(session, "activo", selected = "PORTAFOLIO_EW")
    updateDateRangeInput(
      session,
      "rango",
      start = min(retornos_long$fecha),
      end = max(retornos_long$fecha)
    )
  })

  datos_filtrados <- reactive({
    req(input$activo, input$rango)

    d <- panel_base |>
      filter(
        activo == input$activo,
        fecha >= input$rango[1],
        fecha <= input$rango[2]
      ) |>
      arrange(fecha)

    validate(
      need(nrow(d) >= 12, "Seleccione un rango con al menos 12 observaciones mensuales.")
    )

    d |>
      mutate(
        indice = 100 * cumprod(1 + retorno),
        max_previo = cummax(indice),
        drawdown = indice / max_previo - 1,
        vol_12m = slider::slide_dbl(
          retorno,
          sd,
          .before = 11,
          .complete = TRUE
        ) * sqrt(12)
      )
  })

  metricas_sel <- reactive({
    d <- datos_filtrados()
    n <- nrow(d)
    q05 <- quantile(d$retorno, .05, na.rm = TRUE)

    retorno_anual <- prod(1 + d$retorno)^(12 / n) - 1
    vol_anual <- sd(d$retorno, na.rm = TRUE) * sqrt(12)

    tibble(
      retorno_anual = retorno_anual,
      volatilidad_anual = vol_anual,
      sharpe = ifelse(vol_anual == 0, NA_real_, retorno_anual / vol_anual),
      max_drawdown = min(d$drawdown, na.rm = TRUE),
      var95 = -q05,
      cvar95 = -mean(d$retorno[d$retorno <= q05], na.rm = TRUE)
    )
  })

  metricas_todos <- reactive({
    req(input$rango)

    panel_base |>
      filter(fecha >= input$rango[1], fecha <= input$rango[2]) |>
      group_by(activo) |>
      arrange(fecha, .by_group = TRUE) |>
      mutate(
        indice = 100 * cumprod(1 + retorno),
        max_previo = cummax(indice),
        drawdown = indice / max_previo - 1
      ) |>
      summarise(
        n = n(),
        retorno_anual = prod(1 + retorno)^(12 / n) - 1,
        volatilidad_anual = sd(retorno) * sqrt(12),
        sharpe = retorno_anual / volatilidad_anual,
        max_drawdown = min(drawdown),
        var95 = -quantile(retorno, .05),
        .groups = "drop"
      )
  })

  # ---------- 3. Value boxes de mercado ----------
  output$vb_retorno <- renderValueBox({
    k <- metricas_sel()
    valueBox(
      percent(k$retorno_anual, accuracy = .1),
      "Retorno anualizado",
      icon = icon("line-chart"),
      color = "aqua"
    )
  })

  output$vb_vol <- renderValueBox({
    k <- metricas_sel()
    valueBox(
      percent(k$volatilidad_anual, accuracy = .1),
      "Volatilidad anual",
      icon = icon("area-chart"),
      color = "yellow"
    )
  })

  output$vb_sharpe <- renderValueBox({
    k <- metricas_sel()
    valueBox(
      ifelse(is.na(k$sharpe), "N/A", round(k$sharpe, 2)),
      "Sharpe aproximado",
      icon = icon("balance-scale"),
      color = "green"
    )
  })

  output$vb_dd <- renderValueBox({
    k <- metricas_sel()
    valueBox(
      percent(k$max_drawdown, accuracy = .1),
      "Maximum drawdown",
      icon = icon("arrow-down"),
      color = "red"
    )
  })

  output$txt_var <- renderText({
    percent(metricas_sel()$var95, accuracy = .01)
  })

  output$txt_cvar <- renderText({
    percent(metricas_sel()$cvar95, accuracy = .01)
  })

  output$lectura_ejecutiva <- renderUI({
    k <- metricas_sel()
    nombre <- str_replace_all(input$activo, "_", " ")

    tags$div(
      tags$h4(style = "color:#102d44;font-weight:700;", nombre),
      tags$p(
        sprintf(
          "En el rango seleccionado, el retorno anualizado es %s y la volatilidad anual equivalente es %s.",
          percent(k$retorno_anual, accuracy = .1),
          percent(k$volatilidad_anual, accuracy = .1)
        )
      ),
      tags$p(
        sprintf(
          "La peor caída desde un máximo previo fue %s. El Sharpe aproximado es %s.",
          percent(k$max_drawdown, accuracy = .1),
          ifelse(is.na(k$sharpe), "no disponible", round(k$sharpe, 2))
        )
      ),
      tags$p(
        class = "text-muted-fin",
        "Estas métricas deben interpretarse conjuntamente; ninguna resume por sí sola todas las dimensiones del riesgo."
      )
    )
  })

  # ---------- 4. Gráficos de resumen ----------
  output$plot_indice <- renderPlotly({
    d <- datos_filtrados()

    p <- ggplot(d, aes(fecha, indice)) +
      geom_line(linewidth = .9) +
      labs(
        x = NULL,
        y = "Índice base 100",
        subtitle = str_replace_all(input$activo, "_", " ")
      ) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p) |>
      layout(hovermode = "x unified") |>
      config(displaylogo = FALSE)
  })

  output$plot_hist <- renderPlotly({
    d <- datos_filtrados()
    q05 <- quantile(d$retorno, .05)

    p <- ggplot(d, aes(retorno)) +
      geom_histogram(bins = 24, alpha = .85) +
      geom_vline(xintercept = q05, linetype = 2, linewidth = .8) +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "Retorno mensual", y = "Frecuencia") +
      theme_minimal(base_size = 12)

    ggplotly(p) |>
      config(displaylogo = FALSE)
  })

  output$plot_riesgo_retorno <- renderPlotly({
    m <- metricas_todos() |>
      filter(activo != "PORTAFOLIO_EW") |>
      mutate(activo_label = str_replace_all(activo, "ANDINA_", ""))

    p <- ggplot(m, aes(volatilidad_anual, retorno_anual, label = activo_label)) +
      geom_point(size = 4, alpha = .85) +
      geom_text(nudge_y = .012, size = 3.2) +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "Volatilidad anual", y = "Retorno anualizado") +
      theme_minimal(base_size = 12)

    ggplotly(p) |>
      config(displaylogo = FALSE)
  })

  # ---------- 5. Desempeño ----------
  output$plot_comparacion <- renderPlotly({
    req(input$activo, input$rango)

    comparacion <- panel_base |>
      filter(
        activo %in% unique(c(input$activo, "PORTAFOLIO_EW")),
        fecha >= input$rango[1],
        fecha <= input$rango[2]
      ) |>
      group_by(activo) |>
      arrange(fecha, .by_group = TRUE) |>
      mutate(indice = 100 * cumprod(1 + retorno)) |>
      ungroup() |>
      mutate(activo = str_replace_all(activo, "_", " "))

    p <- ggplot(comparacion, aes(fecha, indice, color = activo)) +
      geom_line(linewidth = .95) +
      labs(x = NULL, y = "Índice base 100", color = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())

    ggplotly(p) |>
      layout(hovermode = "x unified", legend = list(orientation = "h", y = -0.2)) |>
      config(displaylogo = FALSE)
  })

  output$plot_retornos <- renderPlotly({
    d <- datos_filtrados()

    p <- ggplot(d, aes(fecha, retorno)) +
      geom_col(width = 24) +
      geom_hline(yintercept = 0, linewidth = .5) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = NULL, y = "Retorno mensual") +
      theme_minimal(base_size = 12)

    ggplotly(p) |>
      config(displaylogo = FALSE)
  })

  output$plot_vol_movil <- renderPlotly({
    d <- datos_filtrados()

    p <- ggplot(d, aes(fecha, vol_12m)) +
      geom_line(linewidth = .95, na.rm = TRUE) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = NULL, y = "Volatilidad anualizada") +
      theme_minimal(base_size = 12)

    ggplotly(p) |>
      layout(hovermode = "x unified") |>
      config(displaylogo = FALSE)
  })

  # ---------- 6. Riesgo ----------
  output$plot_drawdown <- renderPlotly({
    d <- datos_filtrados()

    plot_ly(
      data = d,
      x = ~fecha,
      y = ~drawdown,
      type = "scatter",
      mode = "lines",
      fill = "tozeroy",
      hovertemplate = "Fecha: %{x}<br>Drawdown: %{y:.2%}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Caída desde máximo", tickformat = ".0%"),
        hovermode = "x unified"
      ) |>
      config(displaylogo = FALSE)
  })

  output$plot_var <- renderPlotly({
    d <- datos_filtrados()
    q05 <- quantile(d$retorno, .05)

    p <- ggplot(d, aes(retorno)) +
      geom_density(fill = "grey75", alpha = .65) +
      geom_vline(xintercept = q05, linetype = 2, linewidth = .9) +
      annotate(
        "text",
        x = q05,
        y = Inf,
        label = paste0("VaR 95% = ", percent(-q05, accuracy = .1)),
        vjust = 1.6,
        hjust = 1.05,
        size = 3.5
      ) +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "Retorno mensual", y = "Densidad") +
      theme_minimal(base_size = 12)

    ggplotly(p) |>
      config(displaylogo = FALSE)
  })

  output$plot_cor <- renderPlotly({
    req(input$rango)

    d <- retornos_wide |>
      mutate(fecha = as.Date(fecha)) |>
      filter(fecha >= input$rango[1], fecha <= input$rango[2]) |>
      select(-fecha)

    m <- cor(d, use = "pairwise.complete.obs")

    plot_ly(
      x = colnames(m),
      y = rownames(m),
      z = m,
      type = "heatmap",
      zmin = -1,
      zmax = 1,
      colorscale = list(
        c(0, "#9b3131"),
        c(.5, "#f4f5f6"),
        c(1, "#235d88")
      ),
      hovertemplate = "%{y} vs %{x}<br>Correlación: %{z:.2f}<extra></extra>"
    ) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "")) |>
      config(displaylogo = FALSE)
  })

  output$tabla_riesgo <- renderDT({
    metricas_todos() |>
      filter(activo != "PORTAFOLIO_EW") |>
      transmute(
        Activo = str_replace_all(activo, "ANDINA_", ""),
        Volatilidad = percent(volatilidad_anual, accuracy = .1),
        `Max. DD` = percent(max_drawdown, accuracy = .1),
        `VaR 95%` = percent(var95, accuracy = .1),
        Sharpe = round(sharpe, 2)
      ) |>
      datatable(
        rownames = FALSE,
        options = list(dom = "t", pageLength = 4, autoWidth = TRUE)
      )
  })

  # ---------- 7. Análisis corporativo ----------
  output$vb_margen <- renderValueBox({
    x <- corporativo |> slice_max(margen_neto, n = 1, with_ties = FALSE)
    valueBox(
      percent(x$margen_neto, accuracy = .1),
      paste("Mayor margen |", x$empresa),
      icon = icon("percent"),
      color = "green"
    )
  })

  output$vb_roa <- renderValueBox({
    x <- corporativo |> slice_max(roa, n = 1, with_ties = FALSE)
    valueBox(
      percent(x$roa, accuracy = .1),
      paste("Mayor ROA |", x$empresa),
      icon = icon("line-chart"),
      color = "aqua"
    )
  })

  output$vb_deuda <- renderValueBox({
    x <- corporativo |> slice_min(endeudamiento, n = 1, with_ties = FALSE)
    valueBox(
      percent(x$endeudamiento, accuracy = .1),
      paste("Menor deuda |", x$empresa),
      icon = icon("bank"),
      color = "yellow"
    )
  })

  output$vb_liquidez <- renderValueBox({
    x <- corporativo |> slice_max(razon_corriente, n = 1, with_ties = FALSE)
    valueBox(
      round(x$razon_corriente, 2),
      paste("Mayor liquidez |", x$empresa),
      icon = icon("tint"),
      color = "red"
    )
  })

  output$plot_corporativo <- renderPlotly({
    plot_ly(
      data = corporativo,
      x = ~endeudamiento,
      y = ~roa,
      size = ~ingresos,
      text = ~paste0(
        "<b>", empresa, "</b>",
        "<br>Ingresos: ", ingresos,
        "<br>Margen neto: ", percent(margen_neto, accuracy = .1),
        "<br>ROA: ", percent(roa, accuracy = .1),
        "<br>Endeudamiento: ", percent(endeudamiento, accuracy = .1)
      ),
      hoverinfo = "text",
      type = "scatter",
      mode = "markers"
    ) |>
      layout(
        xaxis = list(title = "Endeudamiento", tickformat = ".0%"),
        yaxis = list(title = "ROA", tickformat = ".1%")
      ) |>
      config(displaylogo = FALSE)
  })

  output$tabla_corporativa <- renderDT({
    corporativo |>
      transmute(
        Empresa = empresa,
        Ingresos = ingresos,
        Utilidad = utilidad_neta,
        `Margen neto` = percent(margen_neto, accuracy = .1),
        ROA = percent(roa, accuracy = .1),
        Endeudamiento = percent(endeudamiento, accuracy = .1),
        Liquidez = round(razon_corriente, 2)
      ) |>
      datatable(
        rownames = FALSE,
        options = list(dom = "t", pageLength = 4, autoWidth = TRUE)
      )
  })

  # ---------- 8. Explorador y descarga ----------
  output$tabla_datos <- renderDT({
    d <- datos_filtrados() |>
      transmute(
        Fecha = fecha,
        Activo = activo,
        Retorno = percent(retorno, accuracy = .01),
        `Índice base 100` = round(indice, 2),
        Drawdown = percent(drawdown, accuracy = .01),
        `Volatilidad 12m` = ifelse(
          is.na(vol_12m),
          NA_character_,
          percent(vol_12m, accuracy = .1)
        )
      )

    datatable(
      d,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })

  output$descargar_csv <- downloadHandler(
    filename = function() {
      paste0("datos_", input$activo, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(datos_filtrados(), file)
    }
  )
}

server
