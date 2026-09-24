# Clase 3 — Analítica de Datos para la Gestión Financiera

## Tema

**Análisis estadístico, visualización y tableros para la gestión financiera**

La carpeta conserva la plantilla xaringan de las clases anteriores y utiliza una misma base sintética durante toda la sesión.

## Archivos principales

- `Clase_3.Rmd`: presentación completa de la clase.
- `codigo/Codigo_Guiado_Clase_3.R`: demostración en R, comentada paso a paso.
- `codigo/inicio_R.R`: carga mínima para comenzar la demostración.
- `dashboard/01_Flexdashboard_Estatico.Rmd`: tablero sin Shiny.
- `dashboard/02_Flexdashboard_Shiny.Rmd`: tablero interactivo con Shiny.
- `prompts/Prompt_IA_Dashboard.txt`: prompt base para ampliar el tablero con IA.
- `NOTAS_DOCENTE_Clase_3.md`: tiempos y ruta sugerida de la sesión.
- `datos/Clase_3/retornos_activos.csv`: retornos mensuales sintéticos.
- `datos/Clase_3/indicadores_empresas.csv`: indicadores corporativos sintéticos.

## Paquetes requeridos

Para la presentación:

```r
install.packages(c(
  "xaringan", "xaringanExtra", "rmarkdown", "knitr",
  "tidyverse", "scales", "lubridate"
))
```

Para los dashboards:

```r
install.packages(c(
  "flexdashboard", "shiny", "DT"
))
```

## Ejecución de la presentación

1. Abra la carpeta completa como proyecto/directorio de trabajo en RStudio.
2. Abra `Clase_3.Rmd`.
3. Ejecute **Knit**.
4. El HTML se generará en la carpeta principal.
5. En la presentación, la tecla **O** activa la vista de miniaturas mediante `tile_view`.

## Ejecución de los dashboards

Abra cada `.Rmd` dentro de `dashboard/` y presione **Run Document** o **Knit**, según corresponda.

La versión Shiny requiere una sesión de R activa mientras se visualiza.

## Datos

Los datos incluidos son sintéticos y fueron construidos únicamente con fines docentes. Los retornos son mensuales, por lo que las anualizaciones del ejemplo utilizan 12 periodos por año.

## Nota

La sesión no incluye actividad final. La prioridad es desarrollar de forma guiada el flujo completo y dejar claros los conceptos antes de pasar al siguiente taller del curso.
