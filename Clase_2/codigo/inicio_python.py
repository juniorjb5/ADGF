# Clase 1 - Caso de salud financiera
# Ejecute desde el directorio raíz de la carpeta.

import pandas as pd

datos = pd.read_csv("datos/caso_salud_financiera.csv")

# 1. Estructura general
datos.info()

# 2. Cobertura
print({
    "filas": len(datos),
    "unidades": datos["unidad"].nunique(),
    "periodos": datos["periodo"].nunique(),
})

# 3. Llave propuesta
duplicados = (
    datos.groupby(["unidad", "periodo"])
         .size()
         .reset_index(name="n")
         .query("n > 1")
)
print(duplicados)

# 4. Métricas básicas. No corrija todavía los problemas del dataset:
#    primero identifique qué decisiones metodológicas son necesarias.
metricas = datos.assign(
    margen_ebitda=datos["ebitda_mill"] / datos["ingresos_mill"],
    roa=datos["utilidad_neta_mill"] / datos["activos_mill"],
    deuda_activos=datos["deuda_mill"] / datos["activos_mill"],
    razon_corriente=datos["activo_corriente_mill"] / datos["pasivo_corriente_mill"],
)

print(metricas[[
    "unidad", "periodo", "moneda", "margen_ebitda", "roa",
    "deuda_activos", "razon_corriente"
]].head(20))
