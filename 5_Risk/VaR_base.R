
install.packages("PerformanceAnalytics")

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


#Simulación Monte Carlo



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










