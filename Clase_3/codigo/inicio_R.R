# Clase 3 — archivo mínimo para iniciar la demostración
# Ejecute desde la carpeta raíz de la clase.

library(tidyverse)
library(scales)

retornos <- read_csv(
  "datos/Clase_3/retornos_activos.csv",
  show_col_types = FALSE
) |>
  mutate(fecha = as.Date(fecha)) |>
  pivot_longer(
    -fecha,
    names_to = "activo",
    values_to = "retorno"
  )

glimpse(retornos)

# Para el desarrollo completo:
# source("codigo/Codigo_Guiado_Clase_3.R")
