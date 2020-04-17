# LUIS BALDERAS RUIZ. 77145416N
# luisbalderas@correo.ugr.es
# Ejercicio guiado. Curso 2019-2020

# setwd("~/Documentos/Master/TimeSeries-DataStreamMining/TimeSeries/TrabajoAutonomo")

library(tseries)

NPred = 7 # Valores a predecir
NTest = 7 # Valores para test

serie = scan('./data/Estacion2870_diaria.txt')

serie.ts = ts(serie,frequency = 60)
plot(decompose(serie.ts))

# Como se puede ver, no hay una tendencia clara pero sí una estacionalidad, que se mantiene en un rango fijo
# por lo que no es necesaria la aplicación de ninguna transformación, como podría ser logaritmo.

# Dividimos ahora el conjunto de datos para entrenamiento y test. Necesitamos hacer una predicción sobre los valores diarios del
# mes de marzo de 2018. Tomo los últimos 365 datos, es decir, los datos del último año, como test para evaluar el modelo que 

serieTr = serie.ts[1:(length(serie.ts)-NTest)]
tiempoTr = 1:length(serieTr)
serieTs = serie.ts[(length(serie.ts)-NTest+1):length(serie)]
tiempoTs = (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

plot.ts(serieTr, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs,col='red')

# A continuación, tratamos de estudiar la estacionaridad. Para ello, debemos eliminar previamente la tendencia y la estacionalidad.
# En este caso, aplicando la metodología Box-Jenkins vemos que no parece haber tendencia.

#############################################################################################
# ELIMINACIÓN DE LA ESTACIONALIDAD

# Es vital conocer el periodo para tratar la estacionalidad. Como poseemos un dato por día, establecemos el periodo en 
# 364 (para solucionar el desajuste por año bisiesto, falta de días en algunos años,etc), es decir, el mismo día del año anterior.
acf(serieTr)
k = 60
estacionalidad.H1 = decompose(serie.ts)$seasonal[(k+1):(2*k)]

# Tenemos 3 periodos de estacionalidad (3 años)
aux = rep(estacionalidad.H1, 3)
aux[1093] = mean(aux)
# Restamos esos valores de estacionalidad al conjunto de entrenamiento
serieTr.SinEst.H1 = serieTr - aux
# Restamos la estacionalidad calculada con decompose 
serieTs.SinEst.H1 = serieTs - estacionalidad.H1
plot.ts(serieTr.SinEst.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs.SinEst.H1, col='red')

# Obteniendo la serie sin estacionalidad

#############################################################################################
# ESTACIONARIDAD

# Visualizando ACF y encontrando que tiende a 0 muy rápidamente, podríamos pensar que es estacionaria
acf(serieTr.SinEst.H1) # Nos aseguramos con el test de Dickey-Fuller

# Aplicamos el test aumentado de Dickey-Fuller
adftest.H1 = adf.test(serieTr.SinEst.H1)
# p-valor == 0.01 < 0.05 --> la serie temporal es estacionaria

serieTr.SinTendEstDiff.H1 = diff(serieTr.SinEst.H1)
serieTs.SinTendEstDiff.H1 = diff(serieTs.SinEst.H1)
acf(serieTr.SinTendEstDiff.H1)
pacf(serieTr.SinTendEstDiff.H1)
adftest.H2 = adf.test(serieTr.SinTendEstDiff.H1)

# Tanto la gráfica de autocorrelación como la de autocorrelación parcial tienden a cero rápidamente
acf(serieTr.SinEst.H1)
pacf(serieTr.SinEst.H1)


#####################################################################
# MODELOS

# Con los resultados obtenidos, podemos proponer dos modelos distintos: autorregresivos AR o medias móviles MA. En ambos
# casos es necesario definir el parámetro p. Empezando por el AR, dado que la gráfica ACF tiende a cero muy rápidamente
# nos fijamos en la gráfica PACF y vemos que la posición del último valor 
# distinto de cero es 23, aunque también está el 15,12,8,7,6,5 o 4 como posibles valores. Además, como hemos diferenciado una 
# vez, deberíamos incluirlo. Para el caso del MA, procedemos al contrario, encontrando que el 
# último valor distinto de cero es el 23. Sin embargo, también podemos probar con 15, 5, 4 o 3, que son valores más claros por
# sobresalir más del margen.

# ARIMA(23,1,0)

modelo_arima.H1 = arima(serieTr.SinEst.H1, order=c(5,1,0))
valoresAjustados1.H1 = serieTr.SinEst.H1 + modelo_arima.H1$residuals

# Predicciones
Predicciones1.H1 = predict(modelo_arima.H1,n.ahead=NPred)
valoresPredichos1.H1 = Predicciones1.H1$pred

# Error cuadrático acumulado del ajuste en entrenamiento y test
errorTr1.H1 = sum((modelo_arima.H1$residuals)^2)
errorTs1.H1 = sum((valoresPredichos1.H1- serieTs.SinEst.H1)^2)

plot.ts(serieTr.SinTendEstDiff.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados1.H1,col='blue')
lines(tiempoTs,serieTs.SinEst.H1,col='red')
lines(tiempoTs, valoresPredichos1.H1,col='blue')
  