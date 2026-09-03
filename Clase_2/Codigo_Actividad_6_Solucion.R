# =============================================================================
# ANALITICA DE DATOS PARA LA GESTION FINANCIERA
# CLASE 2 - SOLUCION DOCENTE DE LA ACTIVIDAD 6
# Caso: Grupo Andino
# =============================================================================
#
# Objetivo:
#   Construir una base empresa-trimestre a partir de tres fuentes, documentar
#   sus problemas de calidad, integrar sin perder observaciones y producir:
#
#   - Resultados/base_analitica_grupo_andino.csv
#   - Resultados/reporte_calidad_grupo_andino.csv
#   - Resultados/registros_para_revision.csv
#
# Este codigo es una solucion de referencia para el docente. Algunas decisiones
# podrian resolverse de otra forma si el grupo las justifica correctamente.
# =============================================================================


# 0. PAQUETES Y RUTAS ----------------------------------------------------------

library(tidyverse)

ruta_datos <- "Datos/Clase_2"
ruta_resultados <- "Resultados/Clase_2"

dir.create(ruta_resultados, recursive = TRUE, showWarnings = FALSE)


# 1. IMPORTAR LAS TRES FUENTES -------------------------------------------------

maestro_raw <- read_csv(
  file.path(ruta_datos, "maestro_empresas.csv"),
  show_col_types = FALSE
)

estados_raw <- read_csv(
  file.path(ruta_datos, "estados_financieros_raw.csv"),
  show_col_types = FALSE
)

trm_raw <- read_csv(
  file.path(ruta_datos, "trm_trimestral.csv"),
  show_col_types = FALSE
)


# 2. COMPRENDER LAS FUENTES ----------------------------------------------------
#
# maestro_raw:
#   Una fila debe representar una empresa.
#   Llave esperada: id_empresa.
#
# estados_raw:
#   Una fila debe representar una empresa en un trimestre.
#   Llave esperada: id_empresa + periodo.
#
# trm_raw:
#   Una fila debe representar un trimestre.
#   Llave esperada: periodo.

dim(maestro_raw)
names(maestro_raw)
glimpse(maestro_raw)

dim(estados_raw)
names(estados_raw)
glimpse(estados_raw)

dim(trm_raw)
names(trm_raw)
glimpse(trm_raw)


# 3. AUDITORIA INICIAL ---------------------------------------------------------

# 3.1 Resumen de faltantes de cada fuente.
faltantes_maestro <- maestro_raw |>
  summarise(across(everything(), ~ sum(is.na(.x))))

faltantes_estados <- estados_raw |>
  summarise(across(everything(), ~ sum(is.na(.x))))

faltantes_trm <- trm_raw |>
  summarise(across(everything(), ~ sum(is.na(.x))))

faltantes_maestro
faltantes_estados
faltantes_trm

# 3.2 Duplicados de las llaves antes de limpiar.
duplicados_maestro_inicial <- maestro_raw |>
  count(id_empresa) |>
  filter(n > 1)

duplicados_estados_inicial <- estados_raw |>
  count(id_empresa, periodo) |>
  filter(n > 1)

duplicados_trm_inicial <- trm_raw |>
  count(periodo) |>
  filter(n > 1)

duplicados_maestro_inicial
duplicados_estados_inicial
duplicados_trm_inicial

# 3.3 Categorias y formatos tal como llegaron.
maestro_raw |> count(moneda_reporte)
estados_raw |> count(moneda)
estados_raw |> count(periodo)


# 4. ESTANDARIZAR EL MAESTRO ---------------------------------------------------

maestro <- maestro_raw |>
  mutate(
    id_empresa = str_to_upper(str_trim(id_empresa)),
    empresa = str_squish(empresa),
    sector = str_squish(sector),
    moneda_reporte = str_to_upper(str_trim(moneda_reporte)),
    moneda_reporte = case_when(
      moneda_reporte %in% c("USD", "US$") ~ "USD",
      moneda_reporte == "COP" ~ "COP",
      TRUE ~ NA_character_
    )
  )

# Validamos que el identificador sea unico despues de estandarizar.
duplicados_maestro <- maestro |>
  count(id_empresa) |>
  filter(n > 1)

duplicados_maestro


# 5. ESTANDARIZAR LOS ESTADOS FINANCIEROS -------------------------------------

filas_estados_recibidas <- nrow(estados_raw)

estados <- estados_raw |>
  mutate(
    id_empresa = str_to_upper(str_trim(id_empresa)),
    moneda = str_to_upper(str_trim(moneda)),

    # Convertimos 2026T3 en 2026-Q3 y conservamos el mismo patron para todos.
    periodo = str_to_upper(str_trim(periodo)),
    periodo = str_replace(periodo, "^(\\d{4})T([1-4])$", "\\1-Q\\2")
  )

# Detectamos filas totalmente iguales antes de eliminarlas.
duplicados_exactos <- estados |>
  group_by(across(everything())) |>
  filter(n() > 1) |>
  ungroup()

duplicados_exactos

# Decision documentada:
# El registro repetido es identico en todas las variables, por lo que se trata
# como una duplicacion de carga y se conserva una sola copia.
estados <- estados |>
  distinct()

filas_estados_sin_duplicados <- nrow(estados)
duplicados_eliminados <- filas_estados_recibidas - filas_estados_sin_duplicados

# Comprobamos la llave empresa-trimestre despues de la limpieza.
duplicados_llave_final <- estados |>
  count(id_empresa, periodo) |>
  filter(n > 1)

duplicados_llave_final


# 6. ESTANDARIZAR LA TRM -------------------------------------------------------

trm <- trm_raw |>
  mutate(
    periodo = str_to_upper(str_trim(periodo)),
    unidad = str_squish(unidad)
  ) |>
  distinct()

duplicados_trm <- trm |>
  count(periodo) |>
  filter(n > 1)

duplicados_trm


# 7. AUDITAR COINCIDENCIAS ANTES DE INTEGRAR ----------------------------------

# Estados sin empresa en el maestro.
estados_sin_maestro <- estados |>
  anti_join(maestro, by = "id_empresa")

# Estados cuyo periodo no encuentra una TRM.
estados_sin_trm <- estados |>
  anti_join(trm, by = "periodo")

estados_sin_maestro
estados_sin_trm


# 8. INTEGRAR LAS TRES FUENTES -------------------------------------------------

filas_antes_join <- nrow(estados)

# estados es la tabla principal porque define la poblacion empresa-trimestre.
base_integrada <- estados |>
  left_join(
    maestro,
    by = "id_empresa",
    relationship = "many-to-one"
  ) |>
  left_join(
    trm,
    by = "periodo",
    relationship = "many-to-one"
  )

filas_despues_join <- nrow(base_integrada)

# El numero de filas no deberia aumentar.
filas_antes_join
filas_despues_join


# 9. VALIDACIONES POSTERIORES AL JOIN -----------------------------------------

base_validada <- base_integrada |>
  mutate(
    # La moneda observada debe coincidir con la moneda del maestro.
    error_moneda = is.na(moneda_reporte) | moneda != moneda_reporte,

    # Tolerancia relativa de 1% para la identidad contable.
    error_balance_rel = abs(activos - pasivos - patrimonio) / activos,
    error_balance = is.na(error_balance_rel) | error_balance_rel >= 0.01,

    # Los activos corrientes no deberian superar los activos totales.
    error_activos_corrientes = is.na(activos_corrientes) |
      activos_corrientes > activos,

    # Variables necesarias para conversion y analisis posterior.
    error_trm = moneda == "USD" &
      (is.na(trm_promedio) | is.na(trm_cierre)),
    utilidad_faltante = is.na(utilidad_neta),
    sector_faltante = is.na(sector),

    # Una sola bandera resume si el registro requiere revision.
    registro_revisar = error_moneda |
      error_balance |
      error_activos_corrientes |
      error_trm |
      utilidad_faltante |
      sector_faltante
  )


# 10. HOMOLOGAR VALORES A COP --------------------------------------------------
#
# Decision financiera usada en esta solucion:
#   - Stocks: activos, pasivos y patrimonio usan TRM de cierre.
#   - Flujos: ingresos y utilidad usan TRM promedio del trimestre.
#   - Valores que ya estan en COP no se modifican.

base_analitica <- base_validada |>
  mutate(
    factor_stock = case_when(
      moneda == "COP" ~ 1,
      moneda == "USD" ~ trm_cierre,
      TRUE ~ NA_real_
    ),
    factor_flujo = case_when(
      moneda == "COP" ~ 1,
      moneda == "USD" ~ trm_promedio,
      TRUE ~ NA_real_
    ),

    activos_cop = activos * factor_stock,
    pasivos_cop = pasivos * factor_stock,
    patrimonio_cop = patrimonio * factor_stock,
    activos_corrientes_cop = activos_corrientes * factor_stock,
    pasivos_corrientes_cop = pasivos_corrientes * factor_stock,
    ingresos_cop = ingresos * factor_flujo,
    utilidad_neta_cop = utilidad_neta * factor_flujo
  )


# 11. REPORTE DE CALIDAD -------------------------------------------------------

reporte_calidad <- tibble(
  indicador = c(
    "Filas recibidas en estados financieros",
    "Duplicados exactos eliminados",
    "Filas de la base analitica",
    "Llaves duplicadas despues de limpiar",
    "Estados sin correspondencia en maestro",
    "Estados sin correspondencia en TRM",
    "Registros con inconsistencia de moneda",
    "Registros que incumplen identidad contable",
    "Registros con activos corrientes invalidos",
    "Registros con utilidad faltante",
    "Registros con sector faltante",
    "Registros que requieren revision manual"
  ),
  valor = c(
    filas_estados_recibidas,
    duplicados_eliminados,
    nrow(base_analitica),
    nrow(duplicados_llave_final),
    nrow(estados_sin_maestro),
    nrow(estados_sin_trm),
    sum(base_analitica$error_moneda, na.rm = TRUE),
    sum(base_analitica$error_balance, na.rm = TRUE),
    sum(base_analitica$error_activos_corrientes, na.rm = TRUE),
    sum(base_analitica$utilidad_faltante, na.rm = TRUE),
    sum(base_analitica$sector_faltante, na.rm = TRUE),
    sum(base_analitica$registro_revisar, na.rm = TRUE)
  )
)

reporte_calidad


# 12. PORCENTAJE DE FALTANTES EN VARIABLES PRINCIPALES ------------------------

porcentaje_faltantes <- base_analitica |>
  summarise(
    across(
      c(
        activos, pasivos, patrimonio, ingresos, utilidad_neta,
        trm_promedio, trm_cierre, sector
      ),
      ~ mean(is.na(.x)) * 100
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "porcentaje_faltante"
  )

porcentaje_faltantes


# 13. REGISTROS PARA REVISION MANUAL ------------------------------------------

registros_revision <- base_analitica |>
  filter(registro_revisar) |>
  select(
    id_empresa, empresa, periodo, moneda, sector,
    error_moneda, error_balance, error_activos_corrientes,
    error_trm, utilidad_faltante, sector_faltante
  )

registros_revision


# 14. EXPORTAR PRODUCTOS -------------------------------------------------------

write_csv(
  base_analitica,
  file.path(ruta_resultados, "base_analitica_grupo_andino.csv"),
  na = ""
)

write_csv(
  reporte_calidad,
  file.path(ruta_resultados, "reporte_calidad_grupo_andino.csv"),
  na = ""
)

write_csv(
  registros_revision,
  file.path(ruta_resultados, "registros_para_revision.csv"),
  na = ""
)

write_csv(
  porcentaje_faltantes,
  file.path(ruta_resultados, "porcentaje_faltantes.csv"),
  na = ""
)


# 15. CONCLUSION DOCENTE -------------------------------------------------------
#
# La base queda estructuralmente integrada, con 32 combinaciones esperadas de
# empresa-trimestre y sin llaves duplicadas. Sin embargo, no debe considerarse
# completamente depurada porque conserva problemas que requieren revision:
#
#   - una identidad contable inconsistente;
#   - un registro con activos corrientes mayores que los activos totales;
#   - una utilidad neta faltante;
#   - el sector faltante de una empresa.
#
# Esta es una conclusion valida para la actividad: la base puede continuar a
# una etapa exploratoria siempre que los registros marcados se investiguen y
# que sus limitaciones se comuniquen. No se deben corregir inventando valores.

