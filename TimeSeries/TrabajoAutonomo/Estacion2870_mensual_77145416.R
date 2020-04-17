# LUIS BALDERAS RUIZ. 77145416N
# luisbalderas@correo.ugr.es
# Ejercicio Autónomo. Mensual. Curso 2019-2020

# setwd("~/Documentos/Master/TimeSeries-DataStreamMining/TimeSeries/TrabajoAutonomo")

NPred = 12 # Valores a predecir
NTest = 12 # Valores para test

library(tseries)

# Lectura de datos
serie = scan('./data/Estacion2870_mensual.txt')

# Generación de la serie. Frecuencia de 12 por ser anual
serie.ts = ts(serie,frequency = 12)
# Vemos clara estacionalidad y dudosa tendencia. La asocio a ruido
plot(decompose(serie.ts))

# Separación en entrenamiento y test
serieTr = serie.ts[1:(length(serie.ts)-NTest)]
tiempoTr = 1:length(serieTr)
serieTs = serie.ts[(length(serie.ts)-NTest+1):length(serie)]
tiempoTs = (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

# Dibujando training y test
plot.ts(serieTr, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs,col='red')

# Como los rangos de la serie temporal son estables (no hay crecimientos en la estacionalidad)
# no es necesario aplicar ninguna transformación más allá del preprocesamiento ya efectuado.


# Como la tendencia es dudosa, ya que el incremento es de apenas un grado en toda
# la serie y además con altibajos, es complicado definir una aproximación funcional
# que modele la tendencia, por lo que la asocio a ruido y, para conseguir que sea
# estacionaria, me centro en eliminar la estacionalidad.

#############################################################################################
# ELIMINACIÓN DE LA ESTACIONALIDAD

# A simple vista parece que la función tiene estacionalidad. Para eliminarla, es vital conocer el periodo. Utilizamos la función
# de autocorrelación, y, sabiendo el periodo, calculamos la media para cada mes. Este cálculo lo realiza la función decompose
# en su atributo seasonal. Tomando los 12 valores (correspondientes al periodo), podemos restarlos a la serie temporal (a los meses
# homónimos de cada año) para sí eliminar la estacionalidad.

# Partiendo de la suposición de estacionalidad anual, utilizamos la función decompose y su parámetro
# de estacionalidad en los 12 meses del test (los últimos) para eliminarla. 
# Periodo de estacionalidad
k=12
estacionalidad.H1 = decompose(serie.ts)$seasonal[1:k]

# Se repiten los valores de la estacionalidad (en test) tantas veces como ciclos, es decir, 10 veces, dando lugar a 120 valores
aux = rep(estacionalidad.H1, 5)
# Me quedo con la estacionalidad de los meses existentes (mayo 2013-feb 2018)
# Restamos esos valores de estacionalidad al conjunto de entrenamiento
serieTr.SinTendEst.H1 = serieTr - aux[1:46]
# Restamos la estacionalidad calculada con decompose 
serieTs.SinTendEst.H1 = serieTs - aux[47:58]
plot.ts(serieTr.SinTendEst.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs.SinTendEst.H1, col='red')
# quedando la serie sin estacionalidad.

#############################################################################################
# ESTACIONARIDAD

# Visualizando ACF y encontrando que tiende a 0 muy rápidamente, podríamos pensar que es estacionaria
acf(serieTr.SinTendEst.H1) # La tendencia a 0 es rápido y sobrepasa 0. Salimos de dudas con el test aumentado de Dickey-Fuller

# Aplicamos el test aumentado de Dickey-Fuller --> p-value 0.2707, no es estacionaria
adftest.H1 = adf.test(serieTr.SinTendEst.H1)

# Diferenciamos
serieTr.SinTendEstDiff.H1 = diff(serieTr.SinTendEst.H1)
serieTs.SinTendEstDiff.H1 = diff(serieTs.SinTendEst.H1)

# Volvemos a aplicar el mismo test
adftest.H1 = adf.test(serieTr.SinTendEstDiff.H1) # Valor 0.01 < 0.05, por lo que es estacionaria con un nivel de confianza
# superior al 95 %

# A continuación, lo vemos con las gráficas de autocorrelación y autocorrelación parcial, ya que tienden a cero muy rápidamente
acf(serieTr.SinTendEstDiff.H1)
pacf(serieTr.SinTendEstDiff.H1)

#####################################################################
# MODELOS

# Con los resultados obtenidos, podemos proponer dos modelos distintos: autorregresivos AR o medias móviles MA. En ambos
# casos es necesario definir el parámetro p. Empezando por el AR, dado que la gráfica ACF tiende a cero muy rápidamente
# alternando valores positivos y negativos, nos fijamos en la gráfica PACF y vemos que la posición del último valor 
# distinto de cero es 3. Para el caso del MA, procedemos al contrario, encontrando que el último valor distinto de cero
# es el 3.

# AR(3). Como hemos diferenciado una vez para conseguir estacionaridad, utilizamos el modelo ARIMA(3,1,0). Asimismo, calculamos
# el error de entrenamiento y test para comparar con otros modelos.


# También se nos presenta la siguiente opción: utilizar la serie diferenciada o sin diferenciar. En el primer caso, tendríamos
# que nuestro modelo AR sería ARIMA(3,0,1) y luego deshacer la diferenciación integrando. Por comodidad, optamos por el enfoque
# planteado arriba (ARIMA(3,1,0)) para que sea el modelo quien haga la integración de forma automática.

# Para que el modelo ARIMA funcione bien, es necesario que la serie esté centrada en el cer
# por lo que le resto la media
serieTr.SinTendEstC.H1 = serieTr.SinTendEst.H1 - mean(serieTr.SinTendEst.H1)
serieTs.SinTendEstC.H1 = serieTs.SinTendEst.H1 - mean(serieTs.SinTendEst.H1)

modelo_arima.H1 = arima(serieTr.SinTendEstC.H1, order=c(3,1,0))
valoresAjustados1.H1 = serieTr.SinTendEstC.H1 + modelo_arima.H1$residuals

# Predicciones
Predicciones1.H1 = predict(modelo_arima.H1,n.ahead=NPred)
valoresPredichos1.H1 = Predicciones1.H1$pred

# Error cuadrático acumulado del ajuste en entrenamiento y test
errorTr1.H1 = sum((modelo_arima.H1$residuals)^2)
errorTs1.H1 = sum((valoresPredichos1.H1- serieTs.SinTendEst.H1)^2)

plot.ts(serieTr.SinTendEstC.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados1.H1,col='blue')
lines(tiempoTs,serieTs.SinTendEstC.H1,col='red')
lines(tiempoTs, valoresPredichos1.H1,col='blue')

# MODELO MA

# MA(3)
modelo_ma.H1 = arima(serieTr.SinTendEstC.H1, order=c(0,1,3))
valoresAjustados2.H1 = serieTr.SinTendEstC.H1 + modelo_ma.H1$residuals
# Predicciones
Predicciones2.H1 = predict(modelo_ma.H1,n.ahead=NPred)
valoresPredichos2.H1 = Predicciones2.H1$pred

# Error cuadrático acumulado del ajuste en entrenamiento y test
errorTr2.H1 = sum((modelo_ma.H1$residuals)^2)
errorTs2.H1 = sum((valoresPredichos2.H1- serieTs.SinTendEstC.H1)^2)

plot.ts(serieTr.SinTendEstC.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados2.H1,col='blue')
lines(tiempoTs,serieTs.SinTendEstC.H1,col='red')
lines(tiempoTs, valoresPredichos2.H1,col='blue')

##############################################################################
# VALIDACIÓN DE MODELOS

# BONDAD DEL AJUSTE
# Necesitamos evaluar la aleatoriedad de los residuos (Test de Box-Pierce) para descartar que el modelo tenga
# sesgo, la normalidad de los residuos, para garantizar que la serie temporal ha sido modelada correctamente
# (Test de Jarque Bera y Shapiro-Wilk) y realizar una confirmación gráfica

# MODELO AR
boxtestM1 = Box.test(modelo_arima.H1$residuals) # pvalue 0.5577 --> aleatoriedad
JB.H1 = jarque.bera.test(modelo_arima.H1$residuals) # pvalue 0.5472 --> normal
SW.H1 = shapiro.test(modelo_arima.H1$residuals) # pvalue 0.57--> normal

# Gráficamente vemos que los residuos son correctos porque siguen una normal
hist(modelo_arima.H1$residuals, col='blue', prob=T, ylim=c(0,1), xlim=c(-2,2))
lines(density(modelo_arima.H1$residuals))

# MODELO MA --> cumple los tests estadísticos
boxtestM2 = Box.test(modelo_ma.H1$residuals) # pvalue 0.9391 --> aleatoriedad
JB2.H1 = jarque.bera.test(modelo_ma.H1$residuals) # pvalue 0.6982 --> normal
SW2.H1 = shapiro.test(modelo_ma.H1$residuals) # pvalue 0.772--> normal

# Gráficamente vemos que los residuos son correctos porque siguen una normal
hist(modelo_ma.H1$residuals, col='blue', prob=T, ylim=c(0,1), xlim=c(-2,2))
lines(density(modelo_ma.H1$residuals))


#############################################################################
# SELECCIÓN DEL MEJOR MODELO

# MSE
library(scorer)
# MODELO AR
mean_squared_error(serieTr.SinTendEstC.H1, valoresAjustados1.H1)
mean_squared_error(serieTs.SinTendEstC.H1, valoresPredichos1.H1)

# El MSE en entrenamiento es 0.8920383 y en test 5.480675

# MODELO MA

mean_squared_error(serieTr.SinTendEstC.H1, valoresAjustados2.H1)
mean_squared_error(serieTs.SinTendEstC.H1, valoresPredichos2.H1)
# El MSE en entrenamiento es 0.862258 y en test 2.286113


# A continuación, con el criterio de información de Akaike confirmamos estos resultados y escogemos finalmente un modelo para 
# predecir.


# AIC = 2k +nLog(RSS/n)
# k = grados de libertad; n = numero de datos; RSS = mua de los errores al cuadrado

AIC(modelo_arima.H1,modelo_ma.H1)

# Hemos visto que el modelo basado en medias móviles tiene mejor MSE tanto en train como 
# en test. Ahora, el criterio de información de Akaike muestra que para el mismo número 
# de parámetros, el modelo MA tiene menor AIC, luego es más conveniente.

#############################################################################
# PREDICCIÓN CON EL MODELO MÁS FAVORABLE SEGÚN AIC

# Una vez validado el modelo, generamos la predicción para los datos de Marzo y Abril
# de 2018. Para ello, debemos deshacer todos los cambios sobre el conjunto de datos 
# para así recuperar la serie temporal original. Es decir, deshacer la diferenciación, 
# devolver la estacionalidad y la media.

# Partimos de la serie original y le restamos la estacionalidad
serieEntera = serie.ts
tiempo = 1:length(serieEntera)

aux = ts(serieEntera, frequency=12)
aux = decompose(aux)$seasonal
estacionalidad = as.numeric(aux[1:12])
aux = rep(estacionalidad, round(length(serieEntera)/length(estacionalidad)))
serieSinEst = serieEntera - aux[1:58]

# Ajustamos un modelo ARIMA medias móviles Vimos que para asegurar la estacionaridad 
# necesitábamos diferenciar una vez, por lo que indicamos d=1. Además, según el ACF,
# el último valor distinto de 0 es el 3

# Centramos la serie previamente en el 0
serieSinEstC = serieSinEst - mean(serieSinEst)

modelo = arima(serieSinEstC,order=c(0,1,3))
# Generamos los valores ajustados (en entrenamiento)
valoresAjustados = serieSinEstC + modelo$residuals
# Y las predicciones sobre los dos meses
Predicciones = predict(modelo, n.ahead=2)
valoresPredichos = Predicciones$pred

# Devolvemos la media eliminada antes del modelo para así recuperar la referencia real
valoresAjustados = valoresAjustados + mean(serieSinEst)
valoresPredichos = valoresPredichos + mean(serieSinEst)

# A continuación, devolvemos la estacionalidad tanto a los valores ajustados como predichos
valoresAjustados = valoresAjustados + aux[1:58] 
valoresPredichos = valoresPredichos + estacionalidad[10:11]

# Alargamos el tiempo para incluir la predicción
tiempoPred = (tiempo[length(tiempo)]+(1:2))
valoresAjustados = as.numeric(valoresAjustados)
valoresPredichos = as.numeric(valoresPredichos)
# Representamos los valores ajustados y predichos de la serie original
plot.ts(serie,xlim=c(1,max(tiempoPred)),ylim=c(1,40))
lines(valoresAjustados, col='blue')
lines(59:60,valoresPredichos, col='red')

# Hasta aquí llegaría nuestro estudio si no tuviéramos los datos reales del año 1960. En este caso sí los tenemos, por lo que 
# podemos comprobar cuán bueno ha sido nuestro ajuste. 

# Cargamos los valores reales de predicción para comparar con lo predicho
predReales = scan('./data/real-mensual.txt')
lines(59:60,predReales,col='green')
# Sobre la gráfica anterior, en verde, representamos los verdaderos valores y a simple vista parecen ajustarse bastante bien.

# Evaluémos el error cuadrático medio cometido entre los valores reales y los predichos.
mse_prediccion = mean_squared_error(predReales,valoresPredichos)
# 0.907640594619933 de mse