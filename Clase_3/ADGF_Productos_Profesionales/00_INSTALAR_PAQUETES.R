# Paquetes utilizados en los cuatro productos de la Clase 3
paquetes <- c(
  "tidyverse",
  "plotly",
  "DT",
  "scales",
  "knitr",
  "rmarkdown",
  "flexdashboard",
  "shiny",
  "shinydashboard",
  "slider",
  "quarto"
)

faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]

if (length(faltantes) > 0) {
  install.packages(faltantes)
} else {
  message("Todos los paquetes de R ya están instalados.")
}

message("\nNota: para renderizar el archivo .qmd también se requiere Quarto instalado en el equipo.")
message("Las versiones recientes de RStudio suelen incluir soporte integrado para Quarto.")
