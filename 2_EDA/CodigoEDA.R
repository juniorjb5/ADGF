
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(zoo)
library(ggridges)
library(ggdist)
library(patchwork)
library(scales)
library(GGally)
library(moments)

set.seed(123)

# 1) Cargar datos -----------------------------------------------------------
# Traigo el dataset EDHEC (retornos mensuales) y lo dejo como xts:
# fecha = índice, estrategias = columnas. Esto es la base para todo el EDA.
data(edhec, package = "PerformanceAnalytics")
R_xts <- edhec


# 2.1) Wide limpio (data.frame) + nombres correctos -------------------------
# Paso de xts a data.frame “clásico”: creo la columna date y pego los retornos
# como columnas. Con check.names=FALSE evito que R modifique nombres de estrategias.
R_wide <- data.frame(
  date = as.Date(index(R_xts)),
  coredata(R_xts),
  check.names = FALSE
)

# 2.2) Long limpio (ret numérico) -------------------------------------------
# Transformo a formato largo: una fila por (fecha, estrategia) con ret numérico.
# Esto facilita summarise/ggplot y evita problemas con valores no finitos.
R_tbl <- R_wide %>%
  pivot_longer(-date, names_to = "strategy", values_to = "ret") %>%
  mutate(
    strategy = as.character(strategy),
    ret = as.numeric(ret)
  ) %>%
  filter(is.finite(ret))

# 2.3) Top N estrategias por volatilidad (sd) -------------------------------
# Calculo sd por estrategia y selecciono las topN más volátiles.
# Así reduzco ruido visual y me enfoco en las que “más se mueven”.
topN <- 8
top_strat <- R_tbl %>%
  group_by(strategy) %>%
  summarise(sd_ret = sd(ret), .groups = "drop") %>%
  arrange(desc(sd_ret)) %>%
  slice_head(n = topN) %>%
  pull(strategy)

R_top <- R_tbl %>% filter(strategy %in% top_strat)


# 3) KPIs básicos + colas ---------------------------------------------------
# Tabla resumen por estrategia: tamaño muestral, tendencia central, volatilidad,
# asimetría/curtosis y percentiles clave (colas). Ordeno por p05 (downside).
kpi_tbl <- R_tbl %>%
  group_by(strategy) %>%
  summarise(
    n = n(),
    mean = mean(ret, na.rm = TRUE),
    med  = median(ret, na.rm = TRUE),
    sd   = sd(ret, na.rm = TRUE),
    skew = moments::skewness(ret, na.rm = TRUE),
    kurt = moments::kurtosis(ret, na.rm = TRUE),
    p01  = quantile(ret, 0.01, na.rm = TRUE),
    p05  = quantile(ret, 0.05, na.rm = TRUE),
    p50  = quantile(ret, 0.50, na.rm = TRUE),
    p95  = quantile(ret, 0.95, na.rm = TRUE),
    p99  = quantile(ret, 0.99, na.rm = TRUE),
    min  = min(ret, na.rm = TRUE),
    max  = max(ret, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(p05)

kpi_tbl


# 4) Distribución / colas / no-normalidad -----------------------------------
# Bloque para “ver” forma de retornos: dispersión, colas, outliers y qué tan
# lejos está la data de una normalidad ideal (muy típico en finanzas).

# 4.1 Ridgelines (densidades apiladas) --------------------------------------
# Ordeno estrategias por volatilidad y comparo densidades apiladas.
# Útil para ver asimetría y colas (especialmente a la izquierda).
ord <- R_top %>%
  group_by(strategy) %>%
  summarise(sd_ret = sd(ret), .groups = "drop") %>%
  arrange(desc(sd_ret)) %>%
  pull(strategy)

R_top2 <- R_top %>%
  mutate(strategy_ord = factor(strategy, levels = ord))

p_ridge <- ggplot(R_top2, aes(x = ret, y = strategy_ord, fill = strategy_ord)) +
  geom_density_ridges(alpha = 0.65, scale = 1.2, rel_min_height = 0.01, color = "white") +
  theme_minimal(base_size = 12) +
  guides(fill = "none") +
  labs(
    title = "Distribución de retornos (ridgelines)",
    subtitle = "Comparación visual de forma, asimetría y colas entre estrategias",
    x = "Retorno mensual", y = NULL
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1))

p_ridge


# 4.2 Raincloud (halfeye + box + puntos) ------------------------------------
# “Todo en uno”: forma (halfeye), resumen robusto (boxplot) y nube de puntos.
# Muy útil para comparar mediana, dispersión y outliers sin asumir normalidad.
p_rain <- R_top %>%
  ggplot(aes(x = fct_reorder(strategy, ret, .fun = median), y = ret)) +
  ggdist::stat_halfeye(adjust = 0.7, width = 0.6, .width = c(0.5, 0.8, 0.95)) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.35) +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.1), size = 0.8) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Raincloud plot: distribución + mediana + incertidumbre",
    subtitle = "Útil para ver colas, dispersión y outliers por estrategia",
    x = NULL, y = "Retorno mensual"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1))


# 4.3 ECDF (cola izquierda enfocada) ----------------------------------------
# CDF empírica para leer probabilidades directas: P(Retorno ≤ x).
# Muy buena para comparar downside risk entre estrategias.
p_ecdf <- R_top %>%
  ggplot(aes(x = ret, color = strategy)) +
  stat_ecdf(size = 0.9, alpha = 0.9) +
  theme_minimal(base_size = 12) +
  labs(
    title = "ECDF: enfoque en cola izquierda (riesgo de pérdidas)",
    subtitle = "Lectura directa: P(Retorno ≤ x). Comparación de downside risk",
    x = "Retorno mensual", y = "Probabilidad acumulada"
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  guides(color = guide_legend(ncol = 2))


# 4.4 Q-Q plot vs Normal -----------------------------------------------------
# Comparo cuantiles empíricos vs cuantiles normales (en z-scores).
# Si se despega en colas, queda evidente la no-normalidad (colas gordas/asimetría).
qq_data <- R_top %>%
  group_by(strategy) %>%
  mutate(
    z = (ret - mean(ret)) / sd(ret),
    q_theo = qnorm(ppoints(n())),
    q_emp  = sort(z),
    q_theo_sorted = sort(q_theo)
  ) %>%
  summarise(q_theo = q_theo_sorted, q_emp = q_emp, .groups = "drop_last") %>%
  ungroup()

p_qq <- qq_data %>%
  ggplot(aes(x = q_theo, y = q_emp)) +
  geom_point(alpha = 0.6, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_wrap(~strategy, scales = "free", ncol = 4) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Q–Q plot vs Normal (z-scores)",
    subtitle = "Desviaciones en colas => no-normalidad (muy típico en retornos)",
    x = "Cuantiles teóricos (Normal)", y = "Cuantiles empíricos (retornos estandarizados)"
  )

# Mostrar bloque de distribuciones
print(p_ridge)
print(p_rain)
print(p_ecdf)
print(p_qq)



# 5) Outliers y eventos extremos --------------------------------------------
# Aquí me enfoco en extremos: qué tan frecuentes son los eventos raros
# y qué tan malos han sido (tail risk / event risk).

# 5.1 Outliers robustos por estrategia usando MAD ---------------------------
# MAD = Median Absolute Deviation (Desviación Absoluta Mediana).
# Se calcula como: MAD = mediana(|ret - mediana(ret)|). Es una medida de dispersión
# ROBUSTA: no se deja sesgar por outliers como sí pasa con la desviación estándar.
# Aquí estimo un “z-score robusto”: robust_z = (ret - mediana)/MAD, y marco como
# outlier los puntos con |robust_z| > 5. Luego resumo cuántos outliers hay, su tasa,
# y dos referencias de cola: el peor retorno (min) y el percentil 5% (worst_5).
out_tbl <- R_tbl %>%
  group_by(strategy) %>%
  mutate(
    med = median(ret),
    mad = mad(ret, constant = 1),  # constante=1: MAD “cruda”
    robust_z = (ret - med) / (mad + 1e-12),
    outlier_mad = abs(robust_z) > 5
  ) %>%
  summarise(
    outliers = sum(outlier_mad),
    out_rate = mean(outlier_mad),
    worst_1 = min(ret),
    worst_5 = quantile(ret, 0.05),
    .groups = "drop"
  ) %>%
  arrange(desc(out_rate))

print(out_tbl)

# 5.2 Peores 10 retornos por estrategia -------------------------------------
# Saco los 10 meses más malos por estrategia para visualizar event risk.
worst10 <- R_tbl %>%
  group_by(strategy) %>%
  slice_min(ret, n = 10, with_ties = FALSE) %>%
  ungroup()

p_worst <- worst10 %>%
  ggplot(aes(x = ret, y = fct_reorder(strategy, ret, .fun = min))) +
  geom_point(alpha = 0.7) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Peores retornos (top 10) por estrategia",
    subtitle = "Vista rápida del tail risk (event risk)",
    x = "Retorno mensual", y = NULL
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1))

print(p_worst)



# 6) Dinámica temporal: vol rolling, drawdowns ------------------------------
# Ahora sí miro el tiempo: volatilidad en ventanas móviles, crisis vs calma,
# y drawdowns (caídas máximas) como lectura clásica financiera.

# 6.1 Volatilidad rolling (12 meses) ----------------------------------------
# Selecciono 3 estrategias (fijas) para que el gráfico sea reproducible.
# Calculo SD móvil de 12 meses para ver cambios de régimen y clustering.
R_pick <- R_xts[, c("Convertible Arbitrage", "Equity Market Neutral", "Merger Arbitrage")]
R_pick <- R_pick[complete.cases(R_pick), ]

roll_sd <- zoo::rollapply(R_pick, width = 12, FUN = sd, align = "right", fill = NA)

roll_sd_wide <- data.frame(
  date = as.Date(index(roll_sd)),
  coredata(roll_sd),
  check.names = FALSE
)
names(roll_sd_wide) <- c("date", colnames(roll_sd))

roll_sd_tbl <- roll_sd_wide %>%
  pivot_longer(-date, names_to = "strategy", values_to = "roll_sd") %>%
  mutate(roll_sd = as.numeric(roll_sd)) %>%
  filter(is.finite(roll_sd))

p_roll_sd <- ggplot(roll_sd_tbl, aes(x = date, y = roll_sd, color = strategy)) +
  geom_line(linewidth = 0.9) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Volatilidad rolling (12 meses)",
    subtitle = "EDA temporal: cambios de régimen y clustering de volatilidad",
    x = NULL, y = "SD rolling (retorno mensual)"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

p_roll_sd


# 6.2 Drawdowns (EDA financiero clásico) ------------------------------------
# Panel rápido: rendimiento acumulado, drawdowns y riesgo (muy estándar en finanzas).
par(mfrow = c(1,1))
charts.PerformanceSummary(R_pick, main = "Performance Summary (retorno, drawdown, riesgo)")



# 7) Dependencia: correlación ------------------------------------------------
# Co-movimiento entre estrategias. Primera lectura de diversificación:
# quién se mueve con quién (linealmente).

# 7.1 Matriz de correlación (Pearson) ---------------------------------------
cor_p <- cor(R_xts, use = "pairwise.complete.obs", method = "pearson")

# Paso a formato tidy para dibujar heatmap en ggplot
cor_to_tbl <- function(M, name = "pearson"){
  as.data.frame(M) %>%
    rownames_to_column("A") %>%
    pivot_longer(-A, names_to = "B", values_to = "rho") %>%
    mutate(type = name)
}

cor_tbl <- bind_rows(
  cor_to_tbl(cor_p, "Pearson")
)

p_cor <- cor_tbl %>%
  filter(A != B) %>%
  ggplot(aes(x = A, y = B, fill = rho)) +
  geom_tile() +
  facet_wrap(~type, ncol = 2) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Correlación: Pearson",
    subtitle = "Pearson captura dependencia lineal",
    x = NULL, y = NULL
  )

print(p_cor)



# 8) Bootstrap para KPIs + incertidumbre ------------------------------------
# No me quedo solo con el punto estimado: saco IC bootstrap para KPIs.
# Ejemplos: media (retorno) y p05 (downside) por estrategia.

boot_ci <- function(x, stat = mean, B = 3000, alpha = 0.05){
  x <- x[is.finite(x)]
  boots <- replicate(B, stat(sample(x, replace = TRUE)))
  quantile(boots, c(alpha/2, 1 - alpha/2))
}

ci_kpi <- R_tbl %>%
  group_by(strategy) %>%
  summarise(
    mean = mean(ret),
    lo_mean = boot_ci(ret, mean)[1],
    hi_mean = boot_ci(ret, mean)[2],
    p05 = quantile(ret, 0.05),
    lo_p05 = boot_ci(ret, function(z) quantile(z, 0.05))[1],
    hi_p05 = boot_ci(ret, function(z) quantile(z, 0.05))[2],
    .groups = "drop"
  ) %>%
  arrange(mean)

# 8.1 Error bars: media con IC ----------------------------------------------
# Comparo retorno promedio con incertidumbre (IC 95%).
p_ci_mean <- ci_kpi %>%
  ggplot(aes(x = reorder(strategy, mean), y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lo_mean, ymax = hi_mean), width = 0.2) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "KPI con incertidumbre: retorno promedio (IC bootstrap 95%)",
    subtitle = "No solo el punto: también importa la precisión del KPI",
    x = NULL, y = "Retorno promedio"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

# 8.2 Error bars: p05 con IC -------------------------------------------------
# Mismo enfoque, pero ahora para riesgo de cola (percentil 5%).
p_ci_p05 <- ci_kpi %>%
  ggplot(aes(x = reorder(strategy, p05), y = p05)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lo_p05, ymax = hi_p05), width = 0.2) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Riesgo de cola con incertidumbre: percentil 5% (IC bootstrap 95%)",
    subtitle = "Downside robusto para comparar estrategias",
    x = NULL, y = "p5 (retorno)"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

print(p_ci_mean)
print(p_ci_p05)



# 9) Panel final (composición pro) ------------------------------------------
# Armo un “dashboard” estático con 4 gráficos clave para llevar a slides
# o para una lectura rápida sin mil ventanas abiertas.
library(cowplot)

cowplot::plot_grid(
  p_ridge + theme(legend.position="none"),
  p_rain  + theme(legend.position="none"),
  p_roll_sd + theme(legend.position="none"),
  p_ci_p05  + theme(legend.position="none"),
  ncol = 2
)








