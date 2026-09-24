# Notas del docente — Clase 3

## Duración total: 4 horas

La clase está pensada como una secuencia continua sobre la misma base: estadística → inferencia → métricas → visualización → reporte → dashboard → Shiny.

### Distribución sugerida del tiempo

| Bloque | Tiempo |
|---|---:|
| Estadística descriptiva y distribuciones | 40 min |
| Dependencia e inferencia estadística | 45 min |
| Métricas financieras básicas | 25 min |
| Visualización y reportes reproducibles | 30 min |
| Pausa | 10 min |
| Flexdashboard | 45 min |
| Shiny + apoyo de IA | 35 min |
| Síntesis y preguntas | 10 min |
| **Total** | **240 min** |

## Enfoque pedagógico

- No profundizar en demostraciones matemáticas; priorizar interpretación.
- En hipótesis, concentrarse en H0, H1, p-value y lectura del resultado.
- En bootstrap, explicar el procedimiento y mostrar la distribución resultante.
- En métricas, insistir en frecuencia, periodo, unidades y benchmark.
- En Flexdashboard, sí dedicar tiempo a las definiciones de layout y a los tres escenarios de uso: ejecutivo, comparativo y seguimiento.
- Shiny se presenta como extensión de Flexdashboard. El objetivo es comprender input → reactive → output, no dominar toda la sintaxis.
- La IA se presenta como apoyo para escalar código ya comprendido. Validar siempre fórmulas, variables, filtros y frecuencia.

## Demostración sugerida

1. Ejecutar `codigo/Codigo_Guiado_Clase_3.R` por bloques.
2. Abrir `dashboard/01_Flexdashboard_Estatico.Rmd` y mostrar cómo los mismos objetos se reorganizan como producto analítico.
3. Abrir `dashboard/02_Flexdashboard_Shiny.Rmd` y mostrar primero el YAML, luego los inputs, el reactive principal y finalmente los outputs.
4. Utilizar `prompts/Prompt_IA_Dashboard.txt` para mostrar cómo pedir a una IA una ampliación concreta del tablero.

No se incluye actividad final en esta sesión.
