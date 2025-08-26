# Valor de la acción de Google  -------------------------------------------

## Librerías necesarias para series de tiempo ------------------------------
install.packages("xts")
install.packages("astsa")
install.packages("forecast")
install.packages("foreign")
install.packages("timsac")
install.packages("vars")
install.packages("mFiltyr")
install.packages("dynlm")
install.packages("nlme")
install.packages("zoo")
install.packages("quantmod")
install.packages("lmtest")
install.packages("lubridate")
tsdiag(modelo_arima1)

library(xts)
library(tidyverse)
library(lubridate)
library(tseries)
library(astsa)
library(forecast)
library(foreign)
library(timsac)
library(vars)
library(mFiltyr)
library(dynlm)
library(nlme)
library(zoo)
library(ggplot2)
library(lmtest)



## Importación y gráficación de datos  ---------------------------------------------------
# Importar datos de csv

library(readr)
#/GOOGL_NUM.csv
googl_data <- read_csv("Documents/CD/4_FOURTH/Estadística/Ads_YouTube/datasets/GOOGL_NUM.csv", 
                       +col_types = cols(Date = col_date(format = "%Y-%m-%d")))
attach(googl_data)


# Gráficar los datos
googldata_plot <- ggplot(googl_data, aes(x = Date, y = Close)) +
  geom_line(color = "skyblue", size = 1) +
  labs(title = "Evolución del Precio Ajustado de Google",
       x = "Fecha",
       y = "Precio (USD)") +
  theme_minimal()
googldata_plot


## Conversión de data frame a time series ---------------------------------

#Hacer los datos una vabirable zoo dado que es inconsistente
data_ts <- zoo(data$Close, order.by = as.Date(data$Date))
plot(data_ts, type="l", main="Evolución del Cierre de Google", 
     xlab = "Fecha", ylab = "Cierre")


#Función para pasar de zoo a ts
zoo_ts <- function(serie_zoo, frecuencia = 365) {
  # Completar días faltantes
  serie_completa <- merge(serie_zoo, zoo(, seq(start(serie_zoo), end(serie_zoo), by = "day")), all = TRUE)
  
  # Imputar NAs (puedes usar na.locf o na.approx)
  serie_imputada <- na.approx(serie_completa)  # interpola
  
  # Convertir a ts
  ts(coredata(serie_imputada), start = c(2020, 2, 3), frequency = frecuencia)
}

#Convertir los datos de zoo a time series
data_ts = zoo_ts(data_ts, 365)


# Estadística descriptiva -------------------------------------------------

# Descomposición de las variables
decom <- stl(data_ts, s.window = "periodic")
plot(decom, main="Propiedades de la serie de tiempo")

# Gráficar la función de autocorrelación simple y la parcial
par(mfrow = c(2,1), mar=c(4,4,4,1)+.1)
acf(data_ts, main="Autocorrelación simple")
pacf(data_ts, main="Autocorrelación parcial")

acf(data_dif, main="Autocorrelación simple")
pacf(data_dif, main="Autocorrelación parcial")

## Prueba de estacionareidad ----------------------------------------------

#Dickey-Fuller
df_test <- adf.test(data_ts, alternative="stationary")
df_test

## Diferenciación ----------------------------------------------------------

#Obtener en número de diferencias que se deben hacer 
num_dif <-ndiffs(data_ts)
#Hacer la primera diferencia
data_dif <- diff(data_ts)

#Gráficar la serie diferenciada
m <- mean(data_dif, na.rm = TRUE) #obtener la media
s <- sd(data_dif, na.rm = TRUE) # obtener la desviación estándar
plot(data_dif, main="Serie Diferenciada", 
     xlab="Año",ylab = "Valores diferenciados")
abline(h = m, col = "red", lwd = 2)         # Línea de la media
abline(h = m + s, col = "blue", lty = 2)       # Línea de +1 desviación
abline(h = m - s, col = "blue", lty = 2)       # Línea de -1 desviación

df_testdif <- adf.test(data_dif, alternative="stationary")
df_testdif

# Gráficar la función de autocorrelación simple y la parcial
par(mfrow = c(2,1), mar=c(4,4,4,1)+.1)
acf(data_dif, main="Autocorrelación simple")
pacf(data_dif, main="Autocorrelación parcial")


## Creación del modelo ARIMA -----------------------------------------------
modelo_arima1<-arima(data_ts, c(0,1,1), method="ML")
modelo_arima1
#Gráficación del modelo
plot(modelo_arima1)
## Evaluación del modelo
AIC(modelo_arima1) #AIC
BIC(modelo_arima1) #BIC
#Significancia de los coeficientes 
coeftest(modelo_arima1)
#Diagnóstico del modelo (Prueba de Ljung-Box)
tsdiag(modelo_arima1)
#Prueba Box para ruido blanco en residuos
Box.test(residuals(modelo_arima1), type="Ljung-Box")


#ARIMA generado de manera automática
modelo_arima2 = auto.arima(data_ts) #(1,1,0)
modelo_arima2
coeftest(modelo_arima2)
tsdiag(modelo_arima2)
Box.test(residuals(modelo_arima2), type="Ljung-Box")

#Modelo ARIMA 3 con (1,1,1)
modelo_arima3<-arima(data_ts, c(1,1,1), method="ML")
modelo_arima3
## Evaluación del modelo
AIC(modelo_arima3)
BIC(modelo_arima3)
#Significancia de los coeficientes 
coeftest(modelo_arima3)
#Diagnóstico del modelo (Prueba de Ljung-Box)
tsdiag(modelo_arima3)
#Prueba Box para ruido blanco en residuos
Box.test(residuals(modelo_arima3), type="Ljung-Box")



## Validación del modelo  --------------------------------------------------

plot.ts(modelo_arima2$residuals, main="", sub="", xlab="Tiempo",ylab="Residuos")
acf(modelo_arima2$residuals,main="", sub="", xlab="Tiempo",ylab="Autocorrelación")
pacf(modelo_arima2$residuals,main="", sub="", xlab="Tiempo",ylab="Autocorrelación")

qqnorm(modelo_arima2$residuals,main="", sub=""); qqline(modelo_arima2$residuals)
shapiro.test(modelo_arima2$residuals)

## Predicción  -------------------------------------------------------------

modelo_arima2 <- auto.arima(data_ts)
pronostico <- forecast(modelo_arima2, h = 730)
plot(pronostico, main="", sub = "Figura 10: Valores pronosticados")


