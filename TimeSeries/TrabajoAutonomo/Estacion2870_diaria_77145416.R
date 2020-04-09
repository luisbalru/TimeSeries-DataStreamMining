# LUIS BALDERAS RUIZ. 77145416N
# luisbalderas@correo.ugr.es
# Ejercicio guiado. Curso 2019-2020

# setwd("~/Documentos/Master/TimeSeries-DataStreamMining/TimeSeries/TrabajoAutonomo")

library(tseries)

NPred = 31 # Valores a predecir
NTest = 364 # Valores para test

serie = scan('./data/Estacion2870_diaria.txt')

serie.ts = ts(serie,frequency = 364)
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
# En este caso, aplicando la metodología Box-Jenkins vemos que no parece haber tendencia, luego nos centramos en la estacionalidad.

#############################################################################################
# ELIMINACIÓN DE LA ESTACIONALIDAD

# Es vital conocer el periodo para tratar la estacionalidad. Como poseemos un dato por día, establecemos el periodo en 
# 364 (para solucionar el desajuste por año bisiesto, falta de días en algunos años,etc), es decir, el mismo día del año anterior.
acf(serieTr)
k = 364
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


