# INTRODUCCIÓN A R PARA FINANZAS CUANTITATIVAS
# TEMA 2: Asset Pricing Models
# Autor: Jhonatan Ever Medina Muriel


##########################################
########### PASOS PREVIOS ################
##########################################

library("quantmod")
library("readr")

#Extrayendo la data

# Acciones y S&P 500
#Vector de tickers
tickers<-c("^GSPC","IBM")
Acciones<- NULL
for (Ticker in tickers)
  Acciones<-cbind(Acciones, getSymbols(Ticker, from= '2018-01-01',to="2021-07-01", auto.assign=FALSE)[,4])
Acciones
RetAcciones <- na.omit(ROC(Acciones, type="discrete"))
RetAcciones
RetAccionesM<-apply.monthly(RetAcciones, mean)

#US3MTBR (Tasa libre de riesgo)

library(readr)
Estados_Unidos_3_meses_Datos_Históricos_Rendimiento_de_Bonos <- read_csv("Estados Unidos 3 meses Datos Históricos Rendimiento de Bonos.csv")
View(Estados_Unidos_3_meses_Datos_Históricos_Rendimiento_de_Bonos)

str(Estados_Unidos_3_meses_Datos_Históricos_Rendimiento_de_Bonos)
US3MTBR<-Estados_Unidos_3_meses_Datos_Históricos_Rendimiento_de_Bonos[,2]
Fecha<-as.Date(Estados_Unidos_3_meses_Datos_Históricos_Rendimiento_de_Bonos$Fecha,"%m/%d/%Y")
US3MTBR<-xts(US3MTBR, order.by=Fecha)
US3MTBRM<-apply.monthly(US3MTBR, mean)


#Uniendo la data y pasando a datos mensuales
Data<-na.omit(cbind(RetAccionesM,US3MTBRM))
colnames(Data)<-c("S&P 500","IBM","US3MTBR")
#OJO: Data contiene RETORNOS

########################################
################ CAPM ##################
########################################

# 1) Excedentes de Retornos

ExRetSP500<-Data$`S&P 500`-Data$US3MTBR
ExRetIBM<-Data$IBM-Data$US3MTBR

# 2) Modelo de regresión CAPM
D.CAPM.IBM<-cbind(ExRetSP500,ExRetIBM)
D.CAPM.IBM.DF = as.data.frame(D.CAPM.IBM)
CAPM_IBM = lm(ExRetIBM~0+ExRetSP500,data=D.CAPM.IBM.DF)
summary(CAPM_IBM)

# 3) Línea de Mercado de Valores // Security Market Line

plot(D.CAPM.IBM.DF$S.P.500,D.CAPM.IBM.DF$IBM,
     main="Regresión CAPM para IBM",
     ylab="Excedente de retornos en IBM",
     xlab="Excedente de retornos en el MERCADO")
abline(CAPM_IBM,col="red")     # Línea de la regresión
abline(h=0,v=0)                 # Se solicita graficar los ejes

# 4) Excedente de Retorno Esperado de IBM
ExRetEsp<-colMeans(D.CAPM.IBM.DF)
ExRetEsp
ExRetEsp.DF<-data.frame(as.list(ExRetEsp))
  # Retornos esperados del MERCADO
ExRetEspMerc<-ExRetEsp.DF[1,1]
  # Beta
Beta<-data.frame(as.list(CAPM_IBM$coefficients))
Beta<-Beta[1,1]

ExRetEspIBM<-Beta*ExRetEspMerc
ExRetEspIBM

# 5) Varianza
#Varianza del Mercado
VarCAPM<-data.frame(sapply(D.CAPM.IBM.DF, var))
VarMerc<-VarCAPM[1,1]

#Varianza de los residuos
VarRes<-sigma(CAPM_IBM)^2
VarRes
#Varianza
VarIBM<-Beta*VarMerc+VarRes
VarIBM

# Extra: Beta usando el concepto de covarianza
BetaCov<-cov(D.CAPM.IBM.DF$IBM,D.CAPM.IBM.DF$S.P.500)/var(D.CAPM.IBM.DF$S.P.500) 
BetaCov

#############################################
############ Single Index Model #############
#############################################

# 1) Modelo
D.CAPM.IBM<-cbind(ExRetSP500,ExRetIBM)
D.CAPM.IBM.DF = as.data.frame(D.CAPM.IBM)
SIM_IBM = lm(ExRetIBM~ExRetSP500,data=D.CAPM.IBM.DF)
summary(SIM_IBM)

# 2) Gráfico
plot(D.CAPM.IBM.DF$S.P.500,D.CAPM.IBM.DF$IBM,
     main="Single Index Model para IBM",
     ylab="Excedente de retornos en IBM",
     xlab="Excedente de retornos en el MERCADO")
abline(SIM_IBM,col="blue")     # Línea de la regresión
abline(h=0,v=0)                 # Se solicita graficar los ejes

# Usando PerformanceAnalytics
library(PerformanceAnalytics)
# Alfa
CAPM.alpha(Data$IBM,Data$`S&P 500`,Data$US3MTBR)
# Beta
CAPM.beta(Data$IBM,Data$`S&P 500`,Data$US3MTBR)


############################################
####### CAPM vs. Single Index Model ########
############################################

# Gráfico
plot(D.CAPM.IBM.DF$S.P.500,D.CAPM.IBM.DF$IBM,
     main="Capital Asset Pricing Model vs. Single Index Model para IBM",
     ylab="Excedente de retornos en IBM",
     xlab="Excedente de retornos en el MERCADO")
abline(CAPM_IBM,col="red")     # Línea de CAPM
abline(SIM_IBM,col="blue")     # Línea de SIM
abline(h=0,v=0)                 # Se solicita graficar los ejes
legend("topleft", legend=c("CAPM", "SIM"),
       col=c("red", "blue"), lty=1:1, cex=0.8,
       title="Modelos", text.font=4, bg='lightblue')