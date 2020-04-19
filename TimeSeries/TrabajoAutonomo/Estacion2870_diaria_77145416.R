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
# ESTACIONARIDAD

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

# Se podrían intentar más modelos, pero la autocorrelación para valores de q = 2,4,5, a pesar de 
# ser mayores que 0, apenas sobrepasan -0.1, por lo que son prácticamente ruido. De la misma manera
# para p = 2, que casi llega a -0.2.

##############################################################################
# VALIDACIÓN DE MODELOS

# BONDAD DEL AJUSTE
# Necesitamos evaluar la aleatoriedad de los residuos (Test de Box-Pierce) para descartar que el modelo tenga
# sesgo, la normalidad de los residuos, para garantizar que la serie temporal ha sido modelada correctamente
# (Test de Jarque Bera y Shapiro-Wilk) y realizar una confirmación gráfica

# MODELO AR
boxtestM1 = Box.test(modelo_arima.H1$residuals) # pvalue 0.3367 --> aleatoriedad
JB.H1 = jarque.bera.test(modelo_arima.H1$residuals) # pvalue 0.05231 > 0.05 --> normal (por poco)
SW.H1 = shapiro.test(modelo_arima.H1$residuals) # pvalue 0.07556--> normal

# Gráficamente vemos que los residuos son correctos porque siguen una normal
hist(modelo_arima.H1$residuals, col='blue', prob=T, ylim=c(0,0.3), xlim=c(-10,10))
lines(density(modelo_arima.H1$residuals))

# MODELO MA --> no cumple los tests estadísticos
boxtestM2 = Box.test(modelo_ma.H1$residuals) # pvalue 0.6301 --> aleatoriedad
JB2.H1 = jarque.bera.test(modelo_ma.H1$residuals) # pvalue 0.01484 --> rechazamos la hipótesis de normalidad
SW2.H1 = shapiro.test(modelo_ma.H1$residuals) # pvalue 0.009127--> rechazamos la hipótesis de normalidad

# Gráficamente vemos que los residuos son correctos porque siguen una normal
hist(modelo_ma.H1$residuals, col='blue', prob=T, ylim=c(0,0.3), xlim=c(-10,10))
lines(density(modelo_ma.H1$residuals))

# MODELO MA --> NO cumple los tests estadísticos
boxtestM3 = Box.test(modelo_ma2.H1$residuals) # pvalue 0.6228 --> aleatoriedad
JB3.H1 = jarque.bera.test(modelo_ma2.H1$residuals) # pvalue 0.02459 --> rechazamos la hipótesis de normalidad
SW3.H1 = shapiro.test(modelo_ma2.H1$residuals) # pvalue 0.0089977--> rechazamos la hipótesis de normalidad
