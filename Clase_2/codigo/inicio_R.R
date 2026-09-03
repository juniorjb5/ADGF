# Clase 1 - Caso de salud financiera
# Trabaje con el directorio raíz de la carpeta como working directory.

library(tidyverse)

datos <- read_csv("datos/caso_salud_financiera.csv", show_col_types = FALSE)

# 1. Estructura general
glimpse(datos)

# 2. Cobertura
datos |>
  summarise(
    filas = n(),
    unidades = n_distinct(unidad),
    periodos = n_distinct(periodo)
  )

# 3. Llave propuesta
datos |>
  count(unidad, periodo) |>
  filter(n > 1)

# 4. Métricas básicas. No corrija todavía los problemas del dataset:
#    primero identifique qué decisiones metodológicas son necesarias.
metricas <- datos |>
  mutate(
    margen_ebitda = ebitda_mill / ingresos_mill,
    roa = utilidad_neta_mill / activos_mill,
    deuda_activos = deuda_mill / activos_mill,
    razon_corriente = activo_corriente_mill / pasivo_corriente_mill
  )

metricas |>
  select(unidad, periodo, moneda, margen_ebitda, roa, deuda_activos, razon_corriente) |>
  print(n = 20)
