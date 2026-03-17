


library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)
library(plotly)




# xts
portfolio_sd_xts_builtin <-
  StdDev(asset_returns_xts, weights = w)
portfolio_sd_xts_builtin_percent <-
  round(portfolio_sd_xts_builtin * 100, 2)
portfolio_sd_xts_builtin_percent[1,1]




#tidyverse
portfolio_sd_tidy_builtin_percent <-
  portfolio_returns_dplyr_byhand %>%
  summarise(
    sd = sd(returns),
    sd_byhand =
      sqrt(sum((returns - mean(returns))^2)/(nrow(.)-1))) %>%
  mutate(dplyr = round(sd, 4) * 100,
         dplyr_byhand = round(sd_byhand, 4) * 100)
portfolio_sd_tidy_builtin_percent %>%
  select(dplyr, dplyr_byhand)




# tidyquant
portfolio_sd_tidyquant_builtin_percent <-
  portfolio_returns_tq_rebalanced_monthly %>%
  tq_performance(Ra = returns,
                 Rb = NULL,
                 performance_fun = table.Stats) %>%
  select(Stdev) %>%
  mutate(tq_sd = round(Stdev, 4) * 100)








sd_plot <- sd(portfolio_returns_tq_rebalanced_monthly$returns)
mean_plot <- mean(portfolio_returns_tq_rebalanced_monthly$returns)

portfolio_returns_tq_rebalanced_monthly %>%
  mutate(hist_col_red =
           if_else(returns < (mean_plot - sd_plot),
                   returns, as.numeric(NA)),
         hist_col_green =
           if_else(returns > (mean_plot + sd_plot),
                   returns, as.numeric(NA)),
         hist_col_blue =
           if_else(returns > (mean_plot - sd_plot) &
                     returns < (mean_plot + sd_plot),
                   returns, as.numeric(NA))) %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = hist_col_red),
             color = "red") +
  geom_point(aes(y = hist_col_green),
             color = "green") +
  geom_point(aes(y = hist_col_blue),
             color = "blue") +
  labs(title = "Colored Scatter", y = "monthly returns") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))






portfolio_returns_tq_rebalanced_monthly %>%
  mutate(hist_col_red =
           if_else(returns < (mean_plot - sd_plot),
                   returns, as.numeric(NA)),
         hist_col_green =
           if_else(returns > (mean_plot + sd_plot),
                   returns, as.numeric(NA)),
         hist_col_blue =
           if_else(returns > (mean_plot - sd_plot) &
                     returns < (mean_plot + sd_plot),
                   returns, as.numeric(NA))) %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = hist_col_red),
             color = "red") +
  geom_point(aes(y = hist_col_green),
             color = "green") +
  geom_point(aes(y = hist_col_blue),
             color = "blue") +
  geom_hline(yintercept = (mean_plot + sd_plot),
             color = "purple",
             linetype = "dotted") +
  geom_hline(yintercept = (mean_plot-sd_plot),
             color = "purple",
             linetype = "dotted") +
  labs(title = "Colored Scatter with Line", y = "monthly returns") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))







asset_returns_long %>%
  group_by(asset) %>%
  summarise(expected_return = mean(returns),
            stand_dev = sd(returns)) %>%
  add_row(asset = "Portfolio",
          stand_dev =
            sd(portfolio_returns_tq_rebalanced_monthly$returns),
          expected_return =
            mean(portfolio_returns_tq_rebalanced_monthly$returns)) %>%
  ggplot(aes(x = stand_dev,
             y = expected_return,
             color = asset)) +
  geom_point(size = 2) +
  geom_text(
    aes(x =
          sd(portfolio_returns_tq_rebalanced_monthly$returns) * 1.11,
        y =
          mean(portfolio_returns_tq_rebalanced_monthly$returns),
        label = "Portfolio")) +
  ylab("expected return") +
  xlab("standard deviation") +
  ggtitle("Expected Monthly Returns versus Risk") +
  scale_y_continuous(labels = function(x){ paste0(x, "%")}) +
  theme_update(plot.title = element_text(hjust = 0.5))






window <- 24
port_rolling_sd_xts <-
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = sd,
            width = window) %>%
  na.omit() %>%
  `colnames<-`("rolling_sd")
tail(port_rolling_sd_xts, 6)






port_rolling_sd_xts_hc <-
  round(port_rolling_sd_xts, 4) * 100
highchart(type = "stock") %>%
  hc_title(text = "24-Month Rolling Volatility") %>%
  hc_add_series(port_rolling_sd_xts_hc,
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(
    labels = list(format = "{value}%"),
    opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled= TRUE) %>%
  hc_legend(enabled = TRUE)





port_rolling_sd_tidy_does_not_work <-
  portfolio_returns_dplyr_byhand %>%
  mutate(rolling_sd = rollapply(returns,
                                FUN = sd,
                                width = window,
                                fill = NA)) %>%
  select(date, rolling_sd) %>%
  na.omit()
tail(port_rolling_sd_tidy_does_not_work, 6)







port_rolling_sd_tq <-
  portfolio_returns_tq_rebalanced_monthly %>%
  tq_mutate(mutate_fun = rollapply,
            width = window,
            FUN = sd,
            col_rename = "rolling_sd") %>%
  select(date, rolling_sd) %>%
  na.omit()  
port_rolling_sd_tq %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = rolling_sd), color = "cornflowerblue") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  labs(title = "Rolling Standard Deviation", y = "") +
  theme(plot.title = element_text(hjust = 0.5))






# Asimetria



skew_xts <-
  skewness(portfolio_returns_xts_rebalanced_monthly$returns)
skew_xts





skew_tidy <-
  portfolio_returns_tq_rebalanced_monthly %>%
  summarise(skew_builtin = skewness(returns))
skew_tidy




portfolio_density_plot <-
  portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  stat_density(geom = "line",
               alpha = 1,
               colour = "cornflowerblue")
shaded_area_data <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x <
           mean(portfolio_returns_tq_rebalanced_monthly$returns))
portfolio_density_plot_shaded <-
  portfolio_density_plot +
  geom_area(data = shaded_area_data,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5)
portfolio_density_plot_shaded
















portfolio_density_plot_shaded <-
  portfolio_density_plot +
  geom_area(data = shaded_area_data,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5)
median <-
  median(portfolio_returns_tq_rebalanced_monthly$returns)
mean <-
  mean(portfolio_returns_tq_rebalanced_monthly$returns)
median_line_data <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>%
  filter(x <= median)
portfolio_density_plot_shaded +
  geom_segment(data = shaded_area_data,
               aes(x = mean, y = 0, xend = mean, yend = density), 
               color = "red", linetype = "dotted") +
  annotate(geom = "text",
           x = mean, y = 5, label = "mean",  color = "red",
           fontface = "plain", angle = 90, alpha = .8, vjust = -1.75) +
  geom_segment(data = median_line_data,
               aes(x = median, y = 0, xend = median, yend = density),
               color = "black",  linetype = "dotted") +
  annotate(geom = "text",
           x = median, y = 5,   label = "median",
           fontface = "plain", angle = 90, alpha = .8,   vjust = 1.75) +
  ggtitle("Density Plot Illustrating Skewness")





# Kurtosis


kurt_xts <-
  kurtosis(portfolio_returns_xts_rebalanced_monthly$returns)
kurt_xts




kurt_tidy <-
  portfolio_returns_tq_rebalanced_monthly %>%
  summarise(
    kurt_builtin = kurtosis(returns))














# VaR



library(quantmod)
library(PerformanceAnalytics)
#########Fecha inicial de descarga de datos
maxDate = "2000-01-01"
#Serie a descargar
tick<-"AMZN"
#Obtener la serie de precios desde Yahoo Finance
prices <- Ad(getSymbols(tick, auto.assign = FALSE, from=maxDate))
plot(prices)
View(prices)
#Calcular retornos
rets <- dailyReturn(prices)
plot(rets)
hist(rets)
library(tseries)
jarque.bera.test(rets)
#Calcular VaR y CVaR
VaR(rets,p=0.95,method = "historical")
quantile(rets,0.05)
VaR(rets,p=0.99,method = "gaussian")
VaR(rets, p = 0.99, method = "historical")
ES(rets,p=0.99,method = "gaussian")
ES(rets, p = 0.99, method = "historical")
#Series que conforman el portafolio
tickers<- c("MSFT", "AAPL", "AMZN")
#Definir pesos del portafolio
weights<-c(0.5,0.1,0.4)
#Obtener series de precios del portafolio
getSymbols(tickers, from=maxDate)
View(AAPL)
#Crear el portafolio
Port.prices <- na.omit(merge(Ad(MSFT),Ad(AAPL), Ad(AMZN)))
colnames(Port.prices)<-tickers
View(Port.prices)
plot(Port.prices)
#Retornos del portafolio
Port.returns <- ROC(Port.prices,type="discrete")[-1]
colnames(Port.returns)<-tickers
View(Port.returns)
plot(Port.returns)
VaR(Port.returns,p=0.95, weights = weights, portfolio_method = "component", method = "historical")
ES(Port.returns,p=0.95, weights = weights, portfolio_method = "component", method = "historical")
#Calcular VaR Individual
VaR.Hist<- VaR(Port.returns,p=0.95, weights = NULL, portfolio_method = "single", method = "historical")
VaR.Gaus<- VaR(Port.returns,p=0.95, weights = NULL, portfolio_method = "single", method = "gaussian")
VaR.Mod<- VaR(Port.returns,p=0.95, weights = NULL, portfolio_method = "single", method = "modified")
#Guardar datos
All.VaR<-data.frame(rbind(VaR.Hist,VaR.Gaus,VaR.Mod))
rownames(All.VaR)<-c("Hist","Gaus","Mod")
All.VaR
#Calcular VaR Portfolio
Port.VaR.Hist<- VaR(Port.returns,p=0.95, weights = weights, portfolio_method = "component", method = "historical")$hVaR
Port.VaR.Gaus<- VaR(Port.returns,p=0.95, weights = weights, portfolio_method = "component", method = "gaussian")$VaR
Port.VaR.Mod<- VaR(Port.returns,p=0.95, weights = weights, portfolio_method = "component", method = "modified")$MVaR
All.VaR$Portafolio<-c(Port.VaR.Hist,Port.VaR.Gaus,Port.VaR.Mod)
All.VaR<-abs(All.VaR)
All.VaR$Type<-c("Hist","Gaus","Mod")
All.VaR
#Base para gráfico
library(reshape2)
library(ggplot2)
plotVaR<-melt(All.VaR,variable.name = "Ticker", value.name = "VaR")
g1<-ggplot(plotVaR,aes(x=Type,y=VaR, fill=Ticker))+
  geom_bar(stat="identity", position = "dodge") 
g1
############################################################################
#Simulación Monte Carlo para el VaR de una acción
#Instalar librerias
library(quantmod)
#library(tidyquant)
library(xts)
#library(rvest)
library(tidyverse)
#library(stringr)
#library(forcats)
#library(lubridate)
#library(plotly)
library(dplyr)
#library(PerformanceAnalytics)
#Simulamos un año bursatil
periodos <- 252
periodos <- periodos - 1
#descargamos los stocks
getSymbols("TSLA", from = '2015-01-01', to = "2022-01-06", warnings = FALSE, auto.assign = TRUE)
TSLA_adj <- TSLA$TSLA.Adjusted
TSLA_log_returns <- dailyReturn(Ad(TSLA), type="log")
# Creamos la función
gbm_sim <- function(periodos, close_prices, vector_returns) {
  # estimadores de mu y sigma
  mu <- mean(vector_returns)
  sig <- sd(vector_returns)
  fin <- length(vector_returns)
  ini <- fin - periodos
  # simulacion
  sim_actual_values <- TSLA$TSLA.Close[ini]
  S <-  vector(mode="numeric", length=periodos) # aqui vamos guardando los valores
  S[1] <- sim_actual_values
  for (t in c(2:periodos) ) {
    new_S <- S[t-1] * exp( (mu-(0.5*sig^2)) + sig * rnorm(1)   )
    S[t] <- new_S
  } 
  return(S)
}
#Ahora simulamos 1000 realidades distintas
# AHora podemos simular varias veces y plotear:
n_sims = 1000
n_periods = 252
sims <- matrix(NA, ncol=n_sims+1, nrow=n_periods)
for (i in 1:n_sims) {
  values <-  gbm_sim(periodos = 252, close_prices =  TSLA$TSLA.Adjusted, vector_returns = TSLA_log_returns)
  sims[, i] <- values
}
sims[, n_sims+1] <- 1:n_periods
# Seteo del plot
plot(sims[, n_sims+1], sims[,1], type="l",
     xlab="tiempo", ylab="W(t)", ylim=c(0, 2000))
colors = rainbow(n_sims)
# add lines
for ( i in 1:n_sims) {
  lines(sims[, n_sims+1], sims[,i], type="l", col=colors[i])
}
# VALUE AT RISK 
# En sims, tenemos los PRECIOS simulados. Necesitamos las rentabilidades.
# Tenemos que convertir, cada columna de sims, en Rentabilidades.
returns_gbm <- apply( sims[,c(1:n_sims)], 2, Delt)
# Histograma de todas esas rentabilidades
hist(returns_gbm, 40)
# Porcentil 5 -- VAR al  99% y 95%
quantile(returns_gbm, c(0.05, 0.01), na.rm=TRUE) 
# Histograma de todas esas rentabilidades
hist(returns_gbm, 40)
abline(v=quantile(returns_gbm, c(0.05), na.rm=TRUE) )










# Markowitz


# =========================================================
# LIBRERÍAS
# =========================================================
# quantmod: descarga y manejo de datos financieros
# timeSeries: convierte los datos al formato requerido por fPortfolio
# fPortfolio: construye la frontera eficiente y portafolios óptimos
# caTools: utilidades varias (aunque en este script no se usa directamente)
# dplyr: manipulación de datos
# PerformanceAnalytics: cálculo de retornos y métricas financieras
# ggplot2: visualización de resultados

library("quantmod")
library("timeSeries")
library("fPortfolio")
library("caTools")
library("dplyr")
library("PerformanceAnalytics")
library("ggplot2")


###########################################################
###################### PASOS PREVIOS ######################
###########################################################

# Vector con los tickers de las acciones seleccionadas.
# Cada ticker representa un activo financiero que hará parte del portafolio.
tickers <- c("TSLA", "NEM", "NFLX", "BAC", "AMZN", "JNJ")


# ---------------------------------------------------------
# DESCARGA DE PRECIOS
# ---------------------------------------------------------
# Se crea un objeto vacío donde se almacenarán los precios
# de cierre de cada una de las acciones.
PrecPort <- NULL

# Recorremos el vector de tickers y descargamos los precios
# desde Yahoo Finance a partir del 1 de enero de 2019.
# El argumento auto.assign = FALSE evita que cada serie se
# guarde automáticamente en el entorno de trabajo.
# [,4] selecciona la cuarta columna del objeto descargado,
# que corresponde al precio de cierre.
for (Ticker in tickers)
  PrecPort <- cbind(PrecPort, getSymbols(Ticker, from = '2019-01-01',
                                         auto.assign = FALSE)[,4])

# Visualización de la matriz de precios
PrecPort


# ---------------------------------------------------------
# ORGANIZACIÓN DE LA BASE
# ---------------------------------------------------------
# Se renombran las columnas de la matriz de precios con los
# tickers de las acciones seleccionadas.
colnames(PrecPort) <- tickers


# ---------------------------------------------------------
# CÁLCULO DE RETORNOS
# ---------------------------------------------------------
# Se calculan los retornos discretos de cada activo.
# ROC mide la variación porcentual entre un periodo y el siguiente.
# na.omit elimina el primer valor faltante generado por el cálculo.
RetPort <- na.omit(ROC(PrecPort, type = "discrete"))

# Visualización de los retornos
RetPort


# ---------------------------------------------------------
# CONVERSIÓN A timeSeries
# ---------------------------------------------------------
# La librería fPortfolio trabaja mejor con objetos de clase
# timeSeries, por eso convertimos la matriz de retornos.
RetPort <- as.timeSeries(RetPort)

# Visualización final de los retornos en formato timeSeries
RetPort


###############################################################################
###################### FRONTERA Y PORTAFOLIOS EFICIENTES ######################
###############################################################################

# ---------------------------------------------------------
# CÁLCULO DE LA FRONTERA EFICIENTE
# ---------------------------------------------------------
# Se estima la frontera eficiente usando los retornos históricos.
# constraints = "LongOnly" impone la restricción de no venta en corto,
# es decir, todos los pesos del portafolio deben ser no negativos.
fronteraEff <- portfolioFrontier(RetPort, constraints = "LongOnly")


# ---------------------------------------------------------
# GRÁFICOS DE LA FRONTERA
# ---------------------------------------------------------
# En el gráfico pueden incorporarse varios elementos:
# 1: Frontera eficiente
# 2: Portafolio de mínima varianza global
# 3: Línea tangente al portafolio
# 4: Riesgo y retorno de cada activo
# 5: Portafolio con pesos iguales
# 6: Fronteras de dos activos
# 7: Portafolios simulados por Monte Carlo
# 8: Ratio de Sharpe

# Gráfico con algunos elementos básicos de interés
plot(fronteraEff, c(1,2,3))

# Gráfico general por defecto
plot(fronteraEff)


# ---------------------------------------------------------
# PUNTOS DE RIESGO Y RETORNO DE LA FRONTERA
# ---------------------------------------------------------
# Se extraen los pares de riesgo-retorno que conforman la
# frontera eficiente. Esto puede ser útil para análisis posteriores.
Riesgo_Retorno <- frontierPoints(fronteraEff)



# ---------------------------------------------------------
# MATRICES DE CORRELACIÓN Y COVARIANZA
# ---------------------------------------------------------
# La matriz de correlación permite observar qué tan relacionados
# están los movimientos de los activos.
MatrizCorr <- cor(RetPort)

# La matriz de covarianza es clave en Markowitz, pues recoge
# no solo la volatilidad individual de cada activo sino también
# cómo se mueven conjuntamente.
MatrizCov <- cov(RetPort)

# Mostrar ambas matrices
MatrizCov
MatrizCorr


# ---------------------------------------------------------
# PESOS DE LOS PORTAFOLIOS SOBRE LA FRONTERA EFICIENTE
# ---------------------------------------------------------
# Se extraen los pesos de los activos para cada portafolio
# perteneciente a la frontera eficiente.
fronteraPesos <- getWeights(fronteraEff)

# Renombramos columnas con los tickers
colnames(fronteraPesos) <- tickers

# Se construye un gráfico de barras apiladas para visualizar
# cómo cambian los pesos de las acciones a lo largo de la frontera.
barplot(t(fronteraPesos),
        main = "Pesos de los activos en la Frontera Eficiente",
        col = cm.colors(ncol(fronteraPesos) + 2),
        legend = colnames(fronteraPesos))


###########################################################
#################### PUNTOS INTERESANTES ##################
###########################################################

# ---------------------------------------------------------
# PORTAFOLIO DE VARIANZA MÍNIMA GLOBAL
# ---------------------------------------------------------
# Este portafolio corresponde al punto de la frontera
# que tiene el menor nivel de riesgo posible.
VMG <- minvariancePortfolio(RetPort,
                            spec = portfolioSpec(),
                            constraints = "LongOnly")

# Resumen del portafolio de varianza mínima global
VMG


#Covariance Risk Budgets: aporte de cada activo al riesgo total del portafolio, medido desde la estructura de covarianza.
#Target Returns and Risks: resumen de desempeño del portafolio

# ---------------------------------------------------------
# PESOS DEL PORTAFOLIO DE VARIANZA MÍNIMA GLOBAL
# ---------------------------------------------------------
# Extraemos los pesos asignados a cada activo dentro del VMG.
VMG_Pesos <- getWeights(VMG)

# Convertimos los pesos en data.frame para facilitar el gráfico.
DF_VMG_Pesos <- data.frame(VMG_Pesos)

# Guardamos los nombres de las acciones
acciones <- colnames(fronteraPesos)

# Gráfico de barras con los pesos del portafolio de mínima varianza
ggplot(data = DF_VMG_Pesos, aes(x = acciones, y = VMG_Pesos, fill = acciones)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  geom_text(aes(label = sprintf("%.02f %%", VMG_Pesos * 100)),
            position = position_dodge(width = 0.9),
            vjust = -0.25,
            check_overlap = TRUE) +
  ggtitle("Pesos de las acciones del portafolio de varianza mínima global") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Acciones", y = "Pesos (%)")


# ---------------------------------------------------------
# PORTAFOLIO TANGENTE
# ---------------------------------------------------------
# Este portafolio es el que maximiza la relación retorno/riesgo,
# es decir, el mayor Ratio de Sharpe dentro del conjunto eficiente.
# En teoría, corresponde al punto donde la Línea de Mercado de Capitales
# es tangente a la frontera eficiente.
LMC <- tangencyPortfolio(RetPort,
                         spec = portfolioSpec(),
                         constraints = "LongOnly")

# Resumen del portafolio tangente
LMC

# El portafolio tangente busca el mejor balance entre ganar más
# y asumir menos riesgo. En otras palabras, identifica el portafolio
# que ofrece la mayor recompensa por cada unidad de riesgo tomada.


# ---------------------------------------------------------
# PESOS DEL PORTAFOLIO TANGENTE
# ---------------------------------------------------------
# Extraemos los pesos del portafolio tangente.
LMC_Pesos <- getWeights(LMC)

# Convertimos a data.frame para graficar
DF_LMC_Pesos <- data.frame(LMC_Pesos)

# Nombres de las acciones
acciones <- colnames(fronteraPesos)

# Gráfico de barras con la composición del portafolio tangente
ggplot(data = DF_LMC_Pesos, aes(x = acciones, y = LMC_Pesos, fill = acciones)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  geom_text(aes(label = sprintf("%.02f %%", LMC_Pesos * 100)),
            position = position_dodge(width = 0.9),
            vjust = -0.25,
            check_overlap = TRUE) +
  ggtitle("Pesos de las acciones del portafolio tangente") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Acciones", y = "Pesos (%)")
