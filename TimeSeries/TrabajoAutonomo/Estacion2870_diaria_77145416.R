# LUIS BALDERAS RUIZ. 77145416N
# luisbalderas@correo.ugr.es
# Ejercicio guiado. Curso 2019-2020

# setwd("~/Universidad/Master/TimeSeries-DataStreamMining/TimeSeries/TrabajoAutonomo")

library(tseries)

NPred = 7 # Valores a predecir
NTest = 7 # Valores para test

serie = scan('./data/Estacion2870_diaria.txt')

serie.ts = ts(serie,frequency = 60)
plot(decompose(serie.ts))

# La tendencia encontrada es parecida a la mensual, que fue asociada a ruido. Respecto a la
# estacionalidad, se encuentran ciertos patrones, aunque es difuso, ya que, como son datos de
# febrero y marzo de cada año, no hay una estacionalidad clara salvo que en general va subiendo 
# la temperatura de febrero a marzo con fuertes picos.

# Debemos hacer una predicción de los 7 primeros días de marzo de 2018, por lo que separo en
# training y test con 7 elementos en este último.
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
# 60, es decir, el mismo día del año anterior.
acf(serieTr)
k = 60
estacionalidad.H1 = decompose(serie.ts)$seasonal[1:k]

# Tenemos 5 periodos
aux = rep(estacionalidad.H1, 5)
# Restamos esos valores de estacionalidad al conjunto de entrenamiento tomando los primeros
# 257 días, que corresponde al conjunto de training
serieTr.SinEst.H1 = serieTr - aux[1:257]
# Restamos la estacionalidad calculada con decompose, tomando los últimos siete elementos de febrero 
serieTs.SinEst.H1 = serieTs - estacionalidad.H1[22:28]
plot.ts(serieTr.SinEst.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs.SinEst.H1, col='red')

# Obteniendo la serie sin estacionalidad

#############################################################################################
# ESTACIONARIEDAD

# Visualizando ACF y encontrando que tiende a 0 muy rápidamente, podríamos pensar que es estacionaria
acf(serieTr.SinEst.H1) # Nos aseguramos con el test de Dickey-Fuller

# Aplicamos el test aumentado de Dickey-Fuller
adftest.H1 = adf.test(serieTr.SinEst.H1)
# p-valor == 0.01 < 0.05 --> la serie temporal es estacionaria

# A pesar de que el test de Dickey-Fuller nos indica que es estacionaria, la gráfica de autocorrelación
# no tiende rápidamente a cero, haciendo más difícil elegir los parámetros del modelo ARIMA, por lo
# que diferencio la serie
serieTr.SinTendEstDiff.H1 = diff(serieTr.SinEst.H1)
serieTs.SinTendEstDiff.H1 = diff(serieTs.SinEst.H1)
acf(serieTr.SinTendEstDiff.H1)
pacf(serieTr.SinTendEstDiff.H1)
adftest.H2 = adf.test(serieTr.SinTendEstDiff.H1)
# En efecto, además de conseguir el mismo p-valor, diferenciando se obtienen unas gráficas de
# autocorrelación y autocorrelación parcial más propias de una serie estacionaria susceptible de
# modelarse con un método ARIMA.



#####################################################################
# MODELOS

# Con los resultados obtenidos, podemos proponer dos modelos distintos: autorregresivos AR o medias móviles MA. En ambos
# casos es necesario definir el parámetro p. Empezando por el AR, dado que la gráfica ACF tiende a cero muy rápidamente
# alternando valores positivos y negativos, nos fijamos en la gráfica PACF y vemos que la posición del último valor 
# distinto de cero es 7. Para el caso del MA, procedemos al contrario, encontrando que el último valor distinto de cero
# es el 7.

# AR(7). Como hemos diferenciado una vez para conseguir estacionaridad, utilizamos el modelo ARIMA(3,1,0). Asimismo, calculamos
# el error de entrenamiento y test para comparar con otros modelos.


# También se nos presenta la siguiente opción: utilizar la serie diferenciada o sin diferenciar. En el primer caso, tendríamos
# que nuestro modelo AR sería ARIMA(7,0,1) y luego deshacer la diferenciación integrando. Por comodidad, optamos por el enfoque
# planteado arriba (ARIMA(7,1,0)) para que sea el modelo quien haga la integración de forma automática.

# Para que el modelo ARIMA funcione bien, es necesario que la serie esté centrada en el cero
# por lo que le resto la media
serieTr.SinTendEstC.H1 = serieTr.SinTendEstDiff.H1 - mean(serieTr.SinTendEstDiff.H1)
serieTs.SinTendEstC.H1 = serieTs.SinTendEstDiff.H1 - mean(serieTs.SinTendEstDiff.H1)


modelo_arima.H1 = arima(serieTr.SinTendEstC.H1, order=c(7,1,0))
valoresAjustados1.H1 = serieTr.SinTendEstC.H1 + modelo_arima.H1$residuals

# Predicciones
Predicciones1.H1 = predict(modelo_arima.H1,n.ahead=NPred)
valoresPredichos1.H1 = Predicciones1.H1$pred

# Error cuadrático acumulado del ajuste en entrenamiento y test
errorTr1.H1 = sum((modelo_arima.H1$residuals)^2)
errorTs1.H1 = sum((valoresPredichos1.H1[1:6] - serieTs.SinTendEstC.H1)^2)

plot.ts(serieTr.SinTendEstC.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados1.H1,col='blue')
lines(tiempoTs[1:6],serieTs.SinTendEstC.H1,col='red')
lines(tiempoTs[1:6], valoresPredichos1.H1[1:6],col='blue')

# AR(2)

modelo_arima_ar2.H1 = arima(serieTr.SinTendEstC.H1, order=c(2,1,0))
valoresAjustados4.H1 = serieTr.SinTendEstC.H1 + modelo_arima_ar2.H1$residuals

# Predicciones
Predicciones4.H1 = predict(modelo_arima_ar2.H1,n.ahead=NPred)
valoresPredichos4.H1 = Predicciones4.H1$pred

# Error cuadrático acumulado del ajuste en entrenamiento y test
errorTr4.H1 = sum((modelo_arima_ar2.H1$residuals)^2)
errorTs4.H1 = sum((valoresPredichos4.H1[1:6] - serieTs.SinTendEstC.H1)^2)

plot.ts(serieTr.SinTendEstC.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados4.H1,col='blue')
lines(tiempoTs[1:6],serieTs.SinTendEstC.H1,col='red')
lines(tiempoTs[1:6], valoresPredichos4.H1[1:6],col='blue')

# MODELO MA

# MA(7)  

modelo_ma.H1 = arima(serieTr.SinTendEstC.H1, order=c(0,1,7))
valoresAjustados2.H1 = serieTr.SinTendEstC.H1 + modelo_ma.H1$residuals
# Predicciones
Predicciones2.H1 = predict(modelo_ma.H1,n.ahead=NPred)
valoresPredichos2.H1 = Predicciones2.H1$pred

# Error cuadrático acumulado del ajuste en entrenamiento y test
errorTr2.H1 = sum((modelo_ma.H1$residuals)^2)
errorTs2.H1 = sum((valoresPredichos2.H1[1:6]- serieTs.SinTendEstC.H1)^2)

plot.ts(serieTr.SinTendEstC.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados2.H1,col='blue')
lines(tiempoTs[1:6],serieTs.SinTendEstC.H1,col='red')
lines(tiempoTs, valoresPredichos2.H1,col='blue')

# MA(2)  

modelo_ma2.H1 = arima(serieTr.SinTendEstC.H1, order=c(0,1,2))
valoresAjustados3.H1 = serieTr.SinTendEstC.H1 + modelo_ma2.H1$residuals
# Predicciones
Predicciones3.H1 = predict(modelo_ma2.H1,n.ahead=NPred)
valoresPredichos3.H1 = Predicciones3.H1$pred

# Error cuadrático acumulado del ajuste en entrenamiento y test
errorTr3.H1 = sum((modelo_ma2.H1$residuals)^2)
errorTs3.H1 = sum((valoresPredichos3.H1[1:6]- serieTs.SinTendEstC.H1)^2)

plot.ts(serieTr.SinTendEstC.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados3.H1,col='blue')
lines(tiempoTs[1:6],serieTs.SinTendEstC.H1,col='red')
lines(tiempoTs, valoresPredichos3.H1,col='blue')


##############################################################################
# VALIDACIÓN DE MODELOS

# BONDAD DEL AJUSTE
# Necesitamos evaluar la aleatoriedad de los residuos (Test de Box-Pierce) para descartar que el modelo tenga
# sesgo, la normalidad de los residuos, para garantizar que la serie temporal ha sido modelada correctamente
# (Test de Jarque Bera y Shapiro-Wilk) y realizar una confirmación gráfica

# MODELO AR(7)
boxtestM1 = Box.test(modelo_arima.H1$residuals) # pvalue 0.3367 --> aleatoriedad
JB.H1 = jarque.bera.test(modelo_arima.H1$residuals) # pvalue 0.05231 > 0.05 --> normal (por poco)
SW.H1 = shapiro.test(modelo_arima.H1$residuals) # pvalue 0.07556--> normal

# Gráficamente vemos que los residuos son correctos porque siguen una normal
hist(modelo_arima.H1$residuals, col='blue', prob=T, ylim=c(0,0.3), xlim=c(-10,10))
lines(density(modelo_arima.H1$residuals))

# MODELO AR(2) --> cumple los tests
boxtestM4 = Box.test(modelo_arima_ar2.H1$residuals) # pvalue 0.1096 --> aleatoriedad
JB.H4 = jarque.bera.test(modelo_arima_ar2.H1$residuals) # pvalue 0.4949 > 0.05 --> normal
SW.H4 = shapiro.test(modelo_arima_ar2.H1$residuals) # pvalue 0.7159--> normal

# Gráficamente vemos que los residuos son correctos porque siguen una normal
hist(modelo_arima_ar2.H1$residuals, col='blue', prob=T, ylim=c(0,0.3), xlim=c(-10,10))
lines(density(modelo_arima_ar2.H1$residuals))

# MODELO MA(7) --> no cumple los tests estadísticos
boxtestM2 = Box.test(modelo_ma.H1$residuals) # pvalue 0.6301 --> aleatoriedad
JB2.H1 = jarque.bera.test(modelo_ma.H1$residuals) # pvalue 0.01484 --> rechazamos la hipótesis de normalidad
SW2.H1 = shapiro.test(modelo_ma.H1$residuals) # pvalue 0.009127--> rechazamos la hipótesis de normalidad

# Gráficamente vemos que los residuos son correctos porque siguen una normal
hist(modelo_ma.H1$residuals, col='blue', prob=T, ylim=c(0,0.3), xlim=c(-10,10))
lines(density(modelo_ma.H1$residuals))

# MODELO MA(2) --> NO cumple los tests estadísticos
boxtestM3 = Box.test(modelo_ma2.H1$residuals) # pvalue 0.6228 --> aleatoriedad
JB3.H1 = jarque.bera.test(modelo_ma2.H1$residuals) # pvalue 0.02459 --> rechazamos la hipótesis de normalidad
SW3.H1 = shapiro.test(modelo_ma2.H1$residuals) # pvalue 0.0089977--> rechazamos la hipótesis de normalidad


# Los dos modelos basados en medias móviles NO cumplen los tests de normalidad sobre los residuos,
# pero sí su aleatoriedad, por lo que debemos abandonarlos. Comparamos por tanto los modelos AR para
# elegir cuál es el mejor

#############################################################################
# SELECCIÓN DEL MEJOR MODELO

# MSE
library(scorer)
# MODELO AR
mean_squared_error(serieTr.SinTendEstC.H1, valoresAjustados1.H1)
mean_squared_error(serieTs.SinTendEstC.H1, valoresPredichos1.H1)

# El MSE en entrenamiento es 8.862888 y en test 55.84348

# MODELO MA

mean_squared_error(serieTr.SinTendEstC.H1, valoresAjustados4.H1)
mean_squared_error(serieTs.SinTendEstC.H1, valoresPredichos4.H1)
# El MSE en entrenamiento es 11.62977 y en test 60.46596


# A continuación, con el criterio de información de Akaike confirmamos estos resultados y escogemos finalmente un modelo para 
# predecir.


# AIC = 2k +nLog(RSS/n)
# k = grados de libertad; n = numero de datos; RSS = mua de los errores al cuadrado

AIC(modelo_arima.H1,modelo_arima_ar2.H1)

# Vemos que el AR(7) tiene un menor coeficiente AIC, a pesar de que la complejidad del modelo es
# superior. También el MSE es menor, por lo que elijo AR(7) sobre AR(2).

#############################################################################
# PREDICCIÓN CON EL MODELO MÁS FAVORABLE SEGÚN AIC

# Una vez validado el modelo, generamos la predicción para los 7 primeros días de marzo
# de 2018. Para ello, debemos deshacer todos los cambios sobre el conjunto de datos 
# para así recuperar la serie temporal original. Es decir, deshacer la diferenciación, 
# devolver la estacionalidad y la media.

# Partimos de la serie original y le restamos la estacionalidad
serieEntera = serie.ts
tiempo = 1:length(serieEntera)

aux = ts(serieEntera, frequency=60)
aux = decompose(aux)$seasonal
estacionalidad = as.numeric(aux[1:60])
aux = rep(estacionalidad, 5)
serieSinEst = serieEntera - aux[1:264]

# Ajustamos un modelo ARIMA AR. Vimos que para asegurar la estacionaridad 
# necesitábamos diferenciar una vez, por lo que indicamos d=1. Además, según el PACF,
# el último valor distinto de 0 es el 7

# Centramos la serie previamente en el 0
serieSinEstC = serieSinEst - mean(serieSinEst)

modelo = arima(serieSinEstC,order=c(7,1,0))
# Generamos los valores ajustados (en entrenamiento)
valoresAjustados = serieSinEstC + modelo$residuals
# Y las predicciones sobre los dos meses
Predicciones = predict(modelo, n.ahead=7)
valoresPredichos = Predicciones$pred

# Devolvemos la media eliminada antes del modelo para así recuperar la referencia real
valoresAjustados = valoresAjustados + mean(serieSinEst)
valoresPredichos = valoresPredichos + mean(serieSinEst)

# A continuación, devolvemos la estacionalidad tanto a los valores ajustados como predichos
valoresAjustados = valoresAjustados + aux[1:264] 
valoresPredichos = valoresPredichos + estacionalidad[29:35]

# Alargamos el tiempo para incluir la predicción
tiempoPred = (tiempo[length(tiempo)]+(1:7))
valoresAjustados = as.numeric(valoresAjustados)
valoresPredichos = as.numeric(valoresPredichos)
# Representamos los valores ajustados y predichos de la serie original
plot.ts(serie,xlim=c(1,max(tiempoPred)),ylim=c(1,30))
lines(valoresAjustados, col='blue')
lines(265:271,valoresPredichos, col='red')

# Hasta aquí llegaría nuestro estudio si no tuviéramos los datos reales del año 1960. En este caso sí los tenemos, por lo que 
# podemos comprobar cuán bueno ha sido nuestro ajuste. 

# Cargamos los valores reales de predicción para comparar con lo predicho
predReales = scan('./data/real-diario.txt')
lines(265:271,predReales,col='green')
# Sobre la gráfica anterior, en verde, representamos los verdaderos valores y a simple vista parecen ajustarse bastante bien.

# Evaluémos el error cuadrático medio cometido entre los valores reales y los predichos.
mse_prediccion = mean_squared_error(predReales,valoresPredichos)
# 37.5630800473263 de mse