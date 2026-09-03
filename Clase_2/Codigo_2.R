# =============================================================================
# ANALITICA DE DATOS PARA LA GESTION FINANCIERA
# CLASE 2 - EJEMPLO GUIADO PARA DESARROLLAR EN CLASE
# Caso sencillo: ventas de cafe + TRM
# =============================================================================
#
# Proposito:
#   Ejecutar el codigo paso a paso con los estudiantes antes de la Actividad 6.
#
# Pregunta:
#   ¿Podemos construir una base de ventas en pesos, sin perder operaciones y
#   dejando visibles los problemas de calidad?
#
# Ruta del ejercicio:
#   1. Crear y comprender dos fuentes pequeñas.
#   2. Inspeccionar su estructura.
#   3. Revisar llaves, faltantes y duplicados.
#   4. Estandarizar textos y fechas.
#   5. Integrar ventas y TRM.
#   6. Validar el resultado.
#   7. Construir un reporte sencillo de calidad.
#
# Recomendacion docente:
#   No ejecute todo de una vez. Avance bloque por bloque con Ctrl + Enter.
# =============================================================================


# 0. PAQUETES -----------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Lectura rapida de simbolos que apareceran:
# <-   guarda un resultado dentro de un objeto.
# |>   toma el resultado anterior y lo envia al siguiente paso.
# $    permite consultar una columna de una tabla.
# ==   compara si dos valores son iguales.
# &    exige que dos condiciones se cumplan al mismo tiempo.
# |    exige que se cumpla al menos una condicion.
# !    niega una condicion: convierte TRUE en FALSE y viceversa.


# 1. CREAR DOS FUENTES PEQUENAS ------------------------------------------------
# En la Actividad 6 los datos se leen desde archivos. Aqui los construimos
# directamente para concentrarnos en la logica del procedimiento.

# tibble(): crea una tabla organizada en filas y columnas.
# c(): combina varios valores para formar una columna.
ventas_raw <- tibble(
  factura_id = c("F001", "F002", "F003", "F003", "F004", "F005"),
  fecha = c(
    "24/08/2026", "25/08/2026", "26/08/2026",
    "26/08/2026", "27/08/2026", "28/08/2026"
  ),
  cliente = c("Café Norte", "Café Sur", "Café Centro", "Café Centro", "Café Norte", "Café Pacífico"),
  moneda = c("USD", "usd ", "COP", "COP", "US$", "USD"),
  valor = c(12000, 18500, 42000000, 42000000, -9000, 15750)
)

trm_raw <- tibble(
  fecha = c("24/08/2026", "25/08/2026", "26/08/2026", "27/08/2026"),
  trm = c(4050, 4075, 4060, 4090)
)

# Miramos las dos tablas antes de transformarlas.
ventas_raw
trm_raw


# 2. COMPRENDER LAS FUENTES ----------------------------------------------------
# Unidad de ventas_raw: una fila deberia representar una factura.
# Llave esperada: factura_id.
# Unidad de trm_raw: una fila representa una fecha.
# Llave esperada: fecha.

dim(ventas_raw)      # dim(): muestra numero de filas y columnas.
names(ventas_raw)    # names(): muestra los nombres de las variables.
glimpse(ventas_raw)  # glimpse(): resume estructura, tipos y primeros valores.

dim(trm_raw)
names(trm_raw)
glimpse(trm_raw)


# 3. AUDITORIA INICIAL ---------------------------------------------------------

# 3.1 Valores faltantes por variable.
ventas_raw |>
  # summarise(): produce una tabla resumen.
  # across(): aplica la misma operacion a varias columnas.
  # everything(): selecciona todas las columnas.
  # is.na(): pregunta si cada valor esta faltante.
  # En ~ sum(is.na(.x)), .x representa cada columna que se esta revisando.
  summarise(across(everything(), ~ sum(is.na(.x))))

# 3.2 Facturas repetidas. F003 aparece dos veces.
duplicados_factura <- ventas_raw |>
  count(factura_id) |>  # count(): cuenta filas para cada factura.
  filter(n > 1)         # filter(): conserva filas que cumplen la condicion.

duplicados_factura

# 3.3 Filas completamente idénticas.
duplicados_exactos <- ventas_raw |>
  group_by(across(everything())) |> # group_by(): forma grupos de filas iguales.
  filter(n() > 1) |>                # n(): cuenta filas dentro de cada grupo.
  ungroup()                         # ungroup(): elimina la agrupacion temporal.

duplicados_exactos

# 3.4 Categorias de moneda tal como llegaron.
ventas_raw |>
  count(moneda)  # Muestra cuantas veces aparece cada forma de escribir moneda.


# 4. ESTANDARIZACION -----------------------------------------------------------

ventas_limpias <- ventas_raw |>
  # mutate(): crea variables nuevas o modifica variables existentes.
  mutate(
    # Convertimos texto a fecha usando el orden dia/mes/año.
    fecha = dmy(fecha),  # dmy(): interpreta una fecha como dia-mes-año.

    # Quitamos espacios y pasamos las etiquetas a mayuscula.
    # str_trim(): quita espacios al inicio y al final.
    # str_to_upper(): convierte el texto a mayusculas.
    moneda = str_to_upper(str_trim(moneda)),

    # USD y US$ deben representar una sola categoria.
    # case_when(): asigna un resultado segun varias condiciones.
    moneda = case_when(
      # %in% pregunta si un valor pertenece a una lista.
      moneda %in% c("USD", "US$") ~ "USD",
      moneda == "COP" ~ "COP",
      # NA_character_ representa un texto faltante.
      TRUE ~ NA_character_
    )
  ) |>
  # En este ejemplo F003 es un duplicado exacto de carga.
  distinct()  # distinct(): conserva una sola copia de filas identicas.

trm_limpia <- trm_raw |>
  mutate(fecha = dmy(fecha)) |>
  distinct()

ventas_limpias
trm_limpia


# 5. VALIDAR LAS LLAVES ANTES DEL JOIN ----------------------------------------

ventas_limpias |>
  count(factura_id) |>
  filter(n > 1)

trm_limpia |>
  count(fecha) |>
  filter(n > 1)

# Si estas dos tablas quedan vacias, las llaves son unicas despues del ajuste.


# 6. REVISAR COINCIDENCIAS ANTES DEL JOIN -------------------------------------

# anti_join() muestra las ventas cuya fecha no existe en la tabla de TRM.
# La factura F005 quedara visible porque no tenemos TRM para el 28 de agosto.
ventas_sin_trm <- ventas_limpias |>
  anti_join(trm_limpia, by = "fecha") # by indica la llave del cruce.

ventas_sin_trm


# 7. INTEGRAR CON LEFT_JOIN ----------------------------------------------------

filas_antes <- nrow(ventas_limpias) # nrow(): cuenta las filas de una tabla.

base_integrada <- ventas_limpias |>
  # left_join(): conserva todas las ventas y agrega la TRM si encuentra fecha.
  left_join(trm_limpia, by = "fecha")

filas_despues <- nrow(base_integrada)

base_integrada

# left_join() conserva todas las ventas. F005 permanece, pero su TRM es NA.
filas_antes
filas_despues


# 8. CONSTRUIR EL VALOR HOMOLOGADO A COP --------------------------------------

base_analitica <- base_integrada |>
  mutate(
    # case_when() aplica una regla distinta para COP, USD u otros casos.
    valor_cop = case_when(
      moneda == "COP" ~ valor,
      moneda == "USD" & !is.na(trm) ~ valor * trm,
      # NA_real_ representa un numero faltante.
      TRUE ~ NA_real_
    ),

    # Creamos banderas. No borramos automaticamente las observaciones.
    error_valor = is.na(valor) | valor <= 0,
    error_trm = moneda == "USD" & is.na(trm),
    registro_revisar = error_valor | error_trm
  )

base_analitica


# 9. REPORTE SENCILLO DE CALIDAD ----------------------------------------------

reporte_calidad <- base_analitica |>
  summarise(
    filas_finales = n(),                       # n(): numero de filas.
    facturas_unicas = n_distinct(factura_id), # n_distinct(): valores diferentes.
    facturas_sin_trm = sum(error_trm),         # sum(): suma los TRUE como 1.
    valores_no_positivos = sum(error_valor),
    registros_para_revisar = sum(registro_revisar)
  )

reporte_calidad


