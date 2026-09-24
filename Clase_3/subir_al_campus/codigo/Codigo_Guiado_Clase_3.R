# ============================================================
# CLASE 3 — ANALÍTICA DE DATOS PARA LA GESTIÓN FINANCIERA
# Análisis estadístico, visualización y tableros
# ============================================================
#
# Este archivo acompaña la demostración de la clase.
# La idea no es memorizar funciones: siga la secuencia del análisis.
#
# Carpeta de trabajo esperada: raíz de Clase_3_ADGF_Replanteada/
# ============================================================

library(tidyverse)
library(scales)

# ------------------------------------------------------------
# 1. IMPORTAR Y ORGANIZAR LOS DATOS
# ------------------------------------------------------------

retornos_wide <- read_csv(
  "datos/Clase_3/retornos_activos.csv",
  show_col_types = FALSE
) |>
  mutate(fecha = as.Date(fecha))

# pivot_longer(): pasa varias columnas de activos a dos columnas:
#                 activo y retorno.
retornos <- retornos_wide |>
  pivot_longer(
    cols = -fecha,
    names_to = "activo",
    values_to = "retorno"
  )

# Revisar estructura.
glimpse(retornos)
head(retornos)

# ------------------------------------------------------------
# 2. ESTADÍSTICA DESCRIPTIVA
# ------------------------------------------------------------

resumen <- retornos |>
  group_by(activo) |>
  summarise(
    n = n(),                         # número de observaciones
    media = mean(retorno),           # promedio
    mediana = median(retorno),       # valor central
    volatilidad = sd(retorno),       # desviación estándar
    p05 = quantile(retorno, 0.05),   # percentil 5%
    p25 = quantile(retorno, 0.25),
    p75 = quantile(retorno, 0.75),
    minimo = min(retorno),
    maximo = max(retorno),
    .groups = "drop"
  )

resumen

# Una tabla más fácil de leer en porcentaje.
resumen |>
  mutate(
    across(
      c(media, mediana, volatilidad, p05, p25, p75, minimo, maximo),
      ~ percent(.x, accuracy = 0.1)
    )
  )

# ------------------------------------------------------------
# 3. DISTRIBUCIONES
# ------------------------------------------------------------

# Histograma: muestra cómo se reparten los retornos.
ggplot(retornos, aes(x = retorno)) +
  geom_histogram(bins = 25) +
  facet_wrap(~ activo, scales = "free_y") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Distribución de retornos mensuales",
    x = "Retorno",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Boxplot: facilita comparar centro, dispersión y extremos.
ggplot(retornos, aes(x = activo, y = retorno)) +
  geom_boxplot() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Comparación de distribuciones",
    x = NULL,
    y = "Retorno mensual"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------
# 4. CORRELACIÓN
# ------------------------------------------------------------

# cor(): calcula la matriz de correlaciones entre columnas numéricas.
matriz_cor <- retornos_wide |>
  select(-fecha) |>
  cor(use = "pairwise.complete.obs")

round(matriz_cor, 2)

# Gráfico de dispersión para dos activos.
ggplot(
  retornos_wide,
  aes(x = ANDINA_BANCO, y = ANDINA_ENERGIA)
) +
  geom_point(alpha = 0.65) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Banco vs. Energía",
    x = "Retorno Banco",
    y = "Retorno Energía"
  ) +
  theme_minimal()

# Pearson: asociación lineal.
cor(
  retornos_wide$ANDINA_BANCO,
  retornos_wide$ANDINA_ENERGIA,
  method = "pearson"
)

# Spearman: correlación basada en rangos.
cor(
  retornos_wide$ANDINA_BANCO,
  retornos_wide$ANDINA_ENERGIA,
  method = "spearman"
)

# ------------------------------------------------------------
# 5. INTERVALO DE CONFIANZA Y PRUEBA DE HIPÓTESIS
# ------------------------------------------------------------

ret_banco <- retornos |>
  filter(activo == "ANDINA_BANCO") |>
  pull(retorno)

# t.test() contrasta aquí H0: media = 0.
# También entrega un intervalo de confianza para la media.
prueba_banco <- t.test(
  ret_banco,
  mu = 0,
  conf.level = 0.95
)

prueba_banco
prueba_banco$conf.int
prueba_banco$p.value

# Lectura introductoria:
# - p-value < 0.05: se rechaza H0 al 5%.
# - p-value >= 0.05: no se rechaza H0 al 5%.
# Ojo: la decisión estadística NO reemplaza la interpretación financiera.

# ------------------------------------------------------------
# 6. BOOTSTRAP
# ------------------------------------------------------------

set.seed(2026)

# sample(..., replace = TRUE): remuestrea con reemplazo.
# replicate(2000, ...): repite el cálculo 2.000 veces.
boot_medias <- replicate(
  2000,
  mean(
    sample(
      ret_banco,
      size = length(ret_banco),
      replace = TRUE
    )
  )
)

# Intervalo percentil bootstrap del 95%.
ic_boot <- quantile(boot_medias, c(0.025, 0.975))
ic_boot

# Distribución de medias bootstrap.
tibble(media_boot = boot_medias) |>
  ggplot(aes(x = media_boot)) +
  geom_histogram(bins = 35) +
  geom_vline(xintercept = ic_boot, linetype = 2) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Distribución bootstrap de la media",
    x = "Media remuestreada",
    y = "Frecuencia"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 7. MÉTRICAS FINANCIERAS
# ------------------------------------------------------------

# Construimos un índice acumulado base 100 y drawdown.
retornos_metricas <- retornos |>
  group_by(activo) |>
  arrange(fecha, .by_group = TRUE) |>
  mutate(
    indice = 100 * cumprod(1 + retorno),
    pico = cummax(indice),
    drawdown = indice / pico - 1
  ) |>
  ungroup()

# Métricas con frecuencia mensual.
metricas <- retornos_metricas |>
  group_by(activo) |>
  summarise(
    n = n(),
    # Retorno anualizado usando el retorno compuesto del periodo.
    retorno_anual = prod(1 + retorno)^(12 / n) - 1,

    # Volatilidad mensual llevada a escala anual.
    volatilidad_anual = sd(retorno) * sqrt(12),

    # Sharpe aproximado con rf = 0 solo para fines didácticos.
    sharpe_aprox = retorno_anual / volatilidad_anual,

    # Peor caída desde un máximo previo.
    max_drawdown = min(drawdown),

    # VaR histórico 95%: negativo del percentil 5%.
    var95_hist = -quantile(retorno, 0.05),
    .groups = "drop"
  )

metricas

metricas |>
  transmute(
    activo,
    retorno_anual = percent(retorno_anual, accuracy = 0.1),
    volatilidad_anual = percent(volatilidad_anual, accuracy = 0.1),
    sharpe_aprox = round(sharpe_aprox, 2),
    max_drawdown = percent(max_drawdown, accuracy = 0.1),
    var95_hist = percent(var95_hist, accuracy = 0.1)
  )

# Evolución del índice acumulado.
ggplot(
  retornos_metricas,
  aes(x = fecha, y = indice, group = activo, linetype = activo)
) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Índice acumulado por activo",
    subtitle = "Base 100 al inicio del periodo",
    x = NULL,
    y = "Índice",
    linetype = NULL
  ) +
  theme_minimal()

# Drawdown.
ggplot(
  retornos_metricas,
  aes(x = fecha, y = drawdown, group = activo, linetype = activo)
) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Drawdown por activo",
    x = NULL,
    y = "Drawdown",
    linetype = NULL
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 8. MÉTRICAS CORPORATIVAS
# ------------------------------------------------------------

empresas <- read_csv(
  "datos/Clase_3/indicadores_empresas.csv",
  show_col_types = FALSE
)

empresas |>
  select(
    empresa,
    margen_neto,
    roa,
    endeudamiento,
    razon_corriente
  )

# ------------------------------------------------------------
# 9. SIGUIENTE PASO: PRODUCTO ANALÍTICO
# ------------------------------------------------------------
#
# Con los mismos objetos podemos construir:
# - tablas en un reporte R Markdown;
# - gráficos reproducibles;
# - KPIs en Flexdashboard;
# - filtros interactivos con Shiny.
#
# Revise ahora:
# dashboard/01_Flexdashboard_Estatico.Rmd
# dashboard/02_Flexdashboard_Shiny.Rmd
# ============================================================
