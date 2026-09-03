# Clase 1 — Analítica de Datos para la Gestión Financiera

Carpeta preparada para ejecutar la presentación en **R Markdown + xaringan** conservando la plantilla utilizada en las clases anteriores.

## Archivo principal

- `Clase_1_Fundamentos_ADGF.Rmd`

## Paquetes de R requeridos

```r
install.packages(c(
  "xaringan", "xaringanExtra", "rmarkdown", "knitr",
  "tidyverse", "scales"
))
```

## Estructura

- `img/`: imágenes de la plantilla original.
- `datos/`: caso sintético para la práctica de clase.
- `codigo/`: archivos de inicio equivalentes en R y Python.
- `taller/`: plantilla de entrega para los grupos.
- `fonts_mtheme.css`, `cols.css`, `cols_macro.js`: estilo heredado de la plantilla previa.

## Ejecución

1. Abra la carpeta como directorio de trabajo en RStudio.
2. Abra `Clase_1_Fundamentos_ADGF.Rmd`.
3. Ejecute **Knit**.
4. El archivo HTML se generará en esta misma carpeta.

La presentación no ejecuta Python: los fragmentos de Python se muestran de forma paralela como código. Por lo tanto, Python no es necesario para compilar el `.Rmd`.

## Nota del caso

`caso_salud_financiera.csv` es un conjunto de datos **sintético**, creado únicamente con fines docentes. Contiene deliberadamente algunas situaciones que los estudiantes deben identificar durante la práctica.
