# ============================================================================
# ANALITICA DE DATOS PARA LA GESTION FINANCIERA
# Introduccion practica a R
# ============================================================================
#
# Objetivo de esta guia:
#   1. Reconocer la consola y el script.
#   2. Crear y consultar objetos.
#   3. Trabajar con vectores y tablas.
#   4. Identificar datos faltantes.
#   5. Aplicar operaciones basicas con dplyr.
#   6. Construir una primera visualizacion.
#
# Recomendacion para la clase:
# Ejecutar el codigo por bloques. No es necesario correr todo al mismo tiempo.
# En RStudio/Positron se ejecuta una linea o seleccion con Ctrl + Enter.
# ============================================================================


# 1. CONSOLA Y OPERACIONES BASICAS --------------------------------------------

# La consola permite darle una instruccion a R y observar inmediatamente
# el resultado. Por ejemplo, R puede utilizarse como una calculadora.
3 + 4
10 / 2
2^3

# Ctrl + L limpia visualmente la consola.
# Esta accion NO borra los objetos creados en la sesion.


# 2. OBJETOS Y OPERADOR DE ASIGNACION -----------------------------------------

# Un objeto permite guardar un resultado para usarlo posteriormente.
# En R se recomienda utilizar <- para asignar valores.
precio_usd <- 25
tasa_cambio <- 4100

# Para consultar el contenido, escribimos el nombre del objeto.
precio_usd
tasa_cambio

# Los objetos pueden utilizarse en nuevos calculos.
precio_cop <- precio_usd * tasa_cambio
precio_cop

# R diferencia mayusculas y minusculas.
# precio_usd y Precio_USD serian objetos diferentes.


# 3. FUNCIONES ----------------------------------------------------------------

# Una funcion es una instruccion preparada para realizar una tarea.
# Su estructura general es nombre_funcion(argumentos).
sqrt(25)                 # raiz cuadrada
log(100)                 # logaritmo natural
round(10.6789, digits=2) # redondear a dos decimales

# Para consultar la ayuda de una funcion se utiliza ?
# ?round

# Tambien podemos construir nuestras propias funciones con function().
# Una funcion recibe unos datos de entrada, aplica un procedimiento y devuelve
# un resultado. El siguiente ejemplo calcula la cuota fija mensual de un
# credito mediante el sistema de amortizacion frances.

calcular_cuota <- function(capital, tasa_ea, plazo_meses) {

  # Validaciones sencillas para evitar entradas sin sentido financiero.
  if (capital <= 0) {
    stop("El capital debe ser mayor que cero.")
  }

  if (tasa_ea < 0) {
    stop("La tasa efectiva anual no puede ser negativa.")
  }

  if (plazo_meses <= 0) {
    stop("El plazo debe ser mayor que cero.")
  }

  # La tasa debe ingresarse como proporción: 12% se escribe 0.12.
  # Primero convertimos la tasa efectiva anual en tasa efectiva mensual.
  tasa_mensual <- (1 + tasa_ea)^(1 / 12) - 1

  # Cuando la tasa es cero, la cuota es simplemente capital / plazo.
  if (tasa_mensual == 0) {
    cuota <- capital / plazo_meses
  } else {
    cuota <- capital * tasa_mensual /
      (1 - (1 + tasa_mensual)^(-plazo_meses))
  }

  # return() indica el resultado que entrega la funcion.
  return(cuota)
}

# Ejemplo: credito de $100 millones, tasa de 12% EA y plazo de 120 meses.
cuota_credito <- calcular_cuota(
  capital = 100000000,
  tasa_ea = 0.12,
  plazo_meses = 120
)

cuota_credito


# Podemos reutilizar la misma funcion cambiando los argumentos.
calcular_cuota(
  capital = 150000000,
  tasa_ea = 0.08,
  plazo_meses = 240
)


# 4. VECTORES -----------------------------------------------------------------

# Un vector almacena varios valores del mismo tipo.
# La funcion c() combina los elementos.
retornos <- c(0.012, -0.008, 0.004, 0.015, -0.003)
retornos

# Algunas funciones resumen rapidamente un vector numerico.
length(retornos)  # numero de observaciones
mean(retornos)    # promedio
median(retornos)  # mediana
min(retornos)     # minimo
max(retornos)     # maximo
sd(retornos)      # desviacion estandar
summary(retornos) # resumen general


# 5. INDEXACION: SELECCIONAR ELEMENTOS ----------------------------------------

# Los corchetes [] permiten seleccionar posiciones.
# En R, la primera posicion es 1.
retornos[1]       # primer elemento
retornos[2]       # segundo elemento
retornos[1:3]     # elementos del primero al tercero
retornos[c(1, 5)] # elementos primero y quinto

# Tambien podemos seleccionar utilizando una condicion logica.
retornos[retornos > 0]  # solamente retornos positivos
retornos[retornos < 0]  # solamente retornos negativos


# 6. DATOS FALTANTES -----------------------------------------------------------

# NA representa un dato faltante o no disponible.
retornos_na <- c(0.012, -0.008, NA, 0.015, -0.003)
retornos_na

# is.na() identifica las posiciones que contienen NA.
is.na(retornos_na)
sum(is.na(retornos_na))

# Muchas funciones devuelven NA cuando existen faltantes.
mean(retornos_na)

# na.rm = TRUE indica que los NA se excluyen del calculo.
# Esto no los elimina del objeto original.
mean(retornos_na, na.rm = TRUE)


# 7. DATA FRAMES: DATOS ORGANIZADOS EN FILAS Y COLUMNAS -----------------------

# Construimos una tabla pequena y completamente reproducible.
# Cada fila representa un activo en una fecha determinada.
mercado <- data.frame(
  fecha = as.Date(c(
    "2026-08-24", "2026-08-24",
    "2026-08-25", "2026-08-25",
    "2026-08-26", "2026-08-26",
    "2026-08-27", "2026-08-27",
    "2026-08-28", "2026-08-28"
  )),
  ticker = rep(c("ABC", "XYZ"), times = 5),
  precio = c(100, 50, 102, 49, 101, NA, 104, 51, 106, 52),
  volumen = c(1200, 800, 1500, 950, 1300, 1000, 1700, 1100, 1600, 1250)
)

mercado
View(mercado)  # abre la tabla en el visor; se usa de forma interactiva

# Inspeccion inicial de una tabla.
dim(mercado)       # numero de filas y columnas
nrow(mercado)      # numero de filas
ncol(mercado)      # numero de columnas
names(mercado)     # nombres de las variables
str(mercado)       # estructura y tipos de datos
summary(mercado)   # resumen de las variables
head(mercado)      # primeras filas


# 8. FORMAS DE SELECCIONAR INFORMACION ----------------------------------------

# Una celda: [fila, columna]
mercado[1, 3]

# Una fila completa
mercado[1, ]

# Una columna por nombre
mercado[, "precio"]
mercado$precio

# Varias columnas
mercado[, c("ticker", "precio")]

# Filas que cumplen una condicion
mercado[mercado$ticker == "ABC", ]


# 9. PAQUETES -----------------------------------------------------------------

# Un paquete amplía las funciones disponibles en R.
# install.packages() se ejecuta una sola vez por computador.
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("plotly")

# library() se ejecuta cada vez que iniciamos una nueva sesion.
library(tidyverse)


# 10. MANIPULACION CON DPLYR ---------------------------------------------------

# El operador %>%  o el operador |> conecta pasos de izquierda a derecha.
# Puede leerse como "luego" o "despues".

# select(): conserva las columnas indicadas.
mercado_reducido <- mercado |>
  select(fecha, ticker, precio)

mercado_reducido

# filter(): conserva las filas que cumplen una condicion.
mercado_abc <- mercado |>
  filter(ticker == "ABC")

mercado_abc

# mutate(): crea o transforma variables.
mercado_transformado <- mercado |>
  mutate(
    precio_miles = precio / 1000,
    volumen_alto = volumen >= 1200
  )

mercado_transformado

# arrange(): ordena las observaciones.
mercado_ordenado <- mercado |>
  arrange(ticker, fecha)

mercado_ordenado

# group_by() + summarise(): calcula resultados para cada grupo.
resumen_activos <- mercado |>
  group_by(ticker) |>
  summarise(
    observaciones   = n(),
    precio_promedio = mean(precio, na.rm = TRUE),
    precio_minimo   = min(precio, na.rm = TRUE),
    precio_maximo   = max(precio, na.rm = TRUE),
    .groups = "drop"
  )

resumen_activos


# 11. IMPORTACION DE UN ARCHIVO EXCEL -----------------------------------------

library(readxl)

# Es importante verificar primero la carpeta de trabajo.
getwd()

# Opcion recomendada: trabajar con un proyecto y utilizar una ruta relativa.
ruta_salarios <- "Datos/Salaries.xlsx"

# file.exists() permite comprobar si el archivo se encuentra en esa ruta.
file.exists(ruta_salarios)


  #salarios <- read_excel(ruta_salarios)
  salarios <- read_excel("Datos/Salaries.xlsx")

  # Primera revision despues de importar.
  glimpse(salarios)
  names(salarios)
  summary(salarios)

  # Ejemplo de transformacion. La tasa se define como objeto para que quede
  # documentado el supuesto utilizado en la conversion.
  tasa_usdcop <- 4100

  resumen_salarios <- salarios |>
    select(rank, sex, yrs.service, salary) |>
    filter(!is.na(salary)) |>
    mutate(salario_cop = salary * tasa_usdcop) |>
    group_by(rank, sex) |>
    summarise(
      personas        = n(),
      salario_prom_usd = mean(salary),
      salario_med_usd  = median(salary),
      salario_prom_cop = mean(salario_cop),
      .groups = "drop"
    )

  print(resumen_salarios)
  resumen_salarios


# 12. PRIMER GRAFICO FINANCIERO CON GGPLOT2 -----------------------------------


grafico_precios <- ggplot(
  data = mercado,
  mapping = aes(
    x = fecha,
    y = precio,
    color = ticker,
    group = ticker
  )
) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 2.5, na.rm = TRUE) +
  labs(
    title = "Evolución del precio por activo",
    subtitle = "Ejemplo financiero construido durante la clase",
    x = "Fecha",
    y = "Precio",
    color = "Activo"
  ) +
  theme_minimal(base_size = 12)

grafico_precios

# Para convertirlo en un grafico interactivo:
# install.packages("plotly")  # solamente la primera vez

  plotly::ggplotly(grafico_precios)


# 13. ACTIVIDAD CORTA ----------------------------------------------------------

  
  #
  # Retomen el flujo CRISP-DM propuesto en la actividad inicial para la
  # empresa exportadora de café.
  #
  # Con la base Datos_exportadora_cafe.xlsx:
  #
  # 1. PREGUNTA
  # Revisen la pregunta financiera que formularon inicialmente.
  # Si es necesario, ajústenla a las variables disponibles.
  #
  # 2. DATOS
  # Identifiquen qué variables necesitan para responderla.
  #
  # 3. PREPARACIÓN
  # Usen únicamente las operaciones vistas en clase.
  #
  # 4. ANÁLISIS
  # Calculen entre 2 y 4 indicadores descriptivos que aporten evidencia
  # para responder la pregunta.
  # De igual manera es viable el calculo de indicadores financieros utilizando
  # la función mutate()
  #
  # 5. VISUALIZACIÓN
  # Elaboren uno o dos gráficos básicos que permitan comunicar el hallazgo.
  #
  # 6. INTERPRETACIÓN
  # Expliquen qué encontraron, qué decisión podría tomar la empresa
  # y qué información adicional necesitarían para mejorar el análisis.
  #
  # ENTREGA
  # Presenten brevemente:
  # - la pregunta analítica;
  # - las variables y operaciones utilizadas;
  # - los indicadores y gráficos;
  # - una conclusión financiera.
  #
  # Importante:
  # No se evalúa el uso de funciones avanzadas. Se evalúa la coherencia
  # entre la pregunta, los datos, el análisis y la decisión propuesta.