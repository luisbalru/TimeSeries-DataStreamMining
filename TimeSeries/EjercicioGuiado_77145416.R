# LUIS BALDERAS RUIZ. 77145416N
# luisbalderas@correo.ugr.es
# Ejercicio guiado. Curso 2019-2020

# setwd('/home/luisbalru/Documentos/Master/TimeSeries-DataStreamMining/TimeSeries/data')

# OBJETIVO: Entrenar y aplicar un modelo paramétrico sobre la serie para predecir el número
# de pasajeros de avión en 1960. Para ello, la serie debe ser estacionaria. 


# Cargamos la librería de series temporales
#install.packages('tseries')
#install.packages('forecast',dependecies=T)
library(tseries)
library(forecast)

NPred = 12 # Valores a predecir
NTest = 12 # Valores para test

serie = scan('pasajeros_1949_1959.dat')
# Aplicamos la metodología Box-Jenkins para dividir la serie en su parte de tendencia, estacionalidad 
# y la componente irregular o aleatoria. Como estamos trabajando con unos datos mensuales durante años,
# la serie presenta una estacionalidad anual. 
serie.ts = ts(serie,frequency = 12)
plot(decompose(serie.ts))
# Encontramos a simple vista tendencia positiva, posiblemente lineal, y una estacionalidad clara, repitiéndose el patrón
# a la misma distancia entre los puntos 'homólogos' desde el punto de vista del periodo. 

# Como se puede ver, la varianza se mueve en valores entre -40 y 40, por lo que es demasiado grande. Necesitamos un preprocesamiento
# de los datos. Aplicamos un logaritmo, normalizando así el tratamiento de la estacionalidad, para acotar esa variabilidad.
serie.ts = log(serie.ts)
serie.log = log(serie)
plot(decompose(serie.ts))
# Como resultado de la transformación logarítmica, vemos que el rango de la varianza se ha reducido muchísimo
# (-0.2,-0.05), por lo que ya podemos estudiar la tendencia y la estacionalidad de la serie

# Dividimos ahora el conjunto de datos para entrenamiento y test. Como necesitamos predecir los valores de la  
# serie para todos los meses de 1960, cogemos los últimos 12 datos (el último año) para el test
serieTr = serie.log[1:(length(serie.log)-NTest)]
tiempoTr = 1:length(serieTr)
serieTs = serie.log[(length(serie.log)-NTest+1):length(serie)]
tiempoTs = (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

plot.ts(serieTr, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs,col='red')

# A continuación, tratamos de estudiar la estacionaridad. Para ello, debemos eliminar previamente la tendencia y la estacionalidad.
# Lo hacemos precisamente en ese orden porque para eliminar la estacionalidad aplicando la función de autocorrelación, es necesario que
# la serie en ese momento ya no tenga tendencia, mostrándose así los patrones (anuales en este caso) sin distorsión.

#############################################################################################
# MODELADO Y ELIMINACIÓN DE TENDENCIA

# La serie parece tener una tendencia lineal, así que asumimos que así es y luego lo comprobamos con test estadísticos
# El enfoque elegido es el de estimación funcional, ya que somos capaces de identificar la posible función, en este caso
# una recta, que modela la tendencia. Otra posibilidad sería utilizar un filtro, pero aplicar modelo lineal desde el principio
# es más simple y rápido
parametros.H1 = lm(serieTr ~ tiempoTr) 

# Estimación de la tendencia
TendEstimadaTr.H1 = parametros.H1$coefficients[1] + tiempoTr*parametros.H1$coefficients[2]
TendEstimadaTs.H1 = parametros.H1$coefficients[1] + tiempoTs*parametros.H1$coefficients[2]

# Sobre la figura anterior, mostramos el modelo de la tendencia que hemos hecho
plot.ts(serieTr, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTr, TendEstimadaTr.H1, col='blue')
lines(tiempoTs, serieTs, col='red')
lines(tiempoTs, TendEstimadaTs.H1, col = 'green')

# Gráficamente parece ser una tendencia acertada. No obstante, es necesario comprobarlo con un test estadístico.
# Aplicamos t-Test, no sin antes comprobar que los datos son  normales, para comparar los residuos del ajuste con
# los errores del modelo en test

# Test de normalidad de Jarque Bera
JB = jarque.bera.test(parametros.H1$residuals) # p-value 0.4158223
JB = jarque.bera.test((TendEstimadaTs.H1-serieTs)) # p-value 0.4158223

# p-values > 0.05, por lo que se asume normalidad


# Test de Student
TT = t.test(c(parametros.H1$residuals,TendEstimadaTs.H1-serieTs)) # p-value 0.5414746 > 0.05 --> No existen diferencias
# significativas en los datos, por lo que la hipótesis de tendencia lineal es acertada.

# Como ya conocemos la tendencia de la serie, es momento de eliminarla. Para ello, a los valores de training y test de la
# serie les sustraemos su tendencia.
serieTr.SinTend.H1 = serieTr - TendEstimadaTr.H1
serieTs.SinTend.H1 = serieTs - TendEstimadaTs.H1
plot.ts(serieTr.SinTend.H1, xlim=c(1, tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs.SinTend.H1, col='red')
# Obtenemos la serie sin tendencia en el plot

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
aux = rep(estacionalidad.H1, length(serieTr)/length(estacionalidad.H1))
# Restamos esos valores de estacionalidad al conjunto de entrenamiento
serieTr.SinTendEst.H1 = serieTr.SinTend.H1 - aux
# Restamos la estacionalidad calculada con decompose 
serieTs.SinTendEst.H1 = serieTs.SinTend.H1 - estacionalidad.H1
plot.ts(serieTr.SinTendEst.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs.SinTendEst.H1, col='red')
# quedando la serie sin estacionalidad.

#############################################################################################
# ESTACIONARIDAD

# Visualizando ACF y encontrando que tiende a 0 muy rápidamente, podríamos pensar que es estacionaria
acf(serieTr.SinTendEst.H1) # La tendencia a 0 es lenta y sobrepasa 0. Salimos de dudas con el test aumentado de Dickey-Fuller

# Aplicamos el test aumentado de Dickey-Fuller
adftest.H1 = adf.test(serieTr.SinTendEst.H1)

# Obtenemos que el p-valor es 0.6427417 > 0.05, por lo que no es estacionaria. Para conseguirlo, diferenciamos.
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
# distinto de cero es 4. Para el caso del MA, procedemos al contrario, encontrando que el último valor distinto de cero
# es el 16. Sin embargo, al estar tan cerca del margen definido, es posible que no obtengamos resultados muy significativo,
# por lo que también probare con p = 1

# AR(4). Como hemos diferenciado una vez para conseguir estacionaridad, utilizamos el modelo ARIMA(4,1,0). Asimismo, calculamos
# el error de entrenamiento y test para comparar con otros modelos.


# También se nos presenta la siguiente opción: utilizar la serie diferenciada o sin diferenciar. En el primer caso, tendríamos
# que nuestro modelo AR sería ARIMA(4,0,1) y luego deshacer la diferenciación integrando. Por comodidad, optamos por el enfoque
# planteado arriba (ARIMA(4,1,0)) para que sea el modelo quien haga la integración de forma automática.


modelo_arima.H1 = arima(serieTr.SinTendEst.H1, order=c(4,1,0))
valoresAjustados1.H1 = serieTr.SinTendEst.H1 + modelo_arima.H1$residuals

# Predicciones
Predicciones1.H1 = predict(modelo_arima.H1,n.ahead=NPred)
valoresPredichos1.H1 = Predicciones1.H1$pred

# Error cuadrático acumulado del ajuste en entrenamiento y test
errorTr1.H1 = sum((modelo_arima.H1$residuals)^2)
errorTs1.H1 = sum((valoresPredichos1.H1- serieTs.SinTendEst.H1)^2)

plot.ts(serieTr.SinTendEstDiff.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados1.H1,col='blue')
lines(tiempoTs,serieTs.SinTendEst.H1,col='red')
lines(tiempoTs, valoresPredichos1.H1,col='blue')

# MODELO MA

# MA(1)
modelo_ma.H1 = arima(serieTr.SinTendEst.H1, order=c(0,1,1))
valoresAjustados2.H1 = serieTr.SinTendEst.H1 + modelo_ma.H1$residuals
# Predicciones
Predicciones2.H1 = predict(modelo_ma.H1,n.ahead=NPred)
valoresPredichos2.H1 = Predicciones2.H1$pred

# Error cuadrático acumulado del ajuste en entrenamiento y test
errorTr2.H1 = sum((modelo_ma.H1$residuals)^2)
errorTs2.H1 = sum((valoresPredichos2.H1- serieTs.SinTendEst.H1)^2)

plot.ts(serieTr.SinTendEstDiff.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados2.H1,col='blue')
lines(tiempoTs,serieTs.SinTendEst.H1,col='red')
lines(tiempoTs, valoresPredichos2.H1,col='blue')

# MA(16)
modelo_ma2.H1 = arima(serieTr.SinTendEst.H1, order=c(0,1,16))
valoresAjustados3.H1 = serieTr.SinTendEst.H1 + modelo_ma2.H1$residuals
# Predicciones
Predicciones3.H1 = predict(modelo_ma2.H1,n.ahead=NPred)
valoresPredichos3.H1 = Predicciones2.H1$pred

# Error cuadrático acumulado del ajuste en entrenamiento y test
errorTr3.H1 = sum((modelo_ma2.H1$residuals)^2)
errorTs3.H1 = sum((valoresPredichos3.H1- serieTs.SinTendEst.H1)^2)

plot.ts(serieTr.SinTendEstDiff.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados3.H1,col='blue')
lines(tiempoTs,serieTs.SinTendEst.H1,col='red')
lines(tiempoTs, valoresPredichos3.H1,col='blue')

##############################################################################
# VALIDACIÓN DE MODELOS

# BONDAD DEL AJUSTE
# Necesitamos evaluar la aleatoriedad de los residuos (Test de Box-Pierce) para descartar que el modelo tenga
# sesgo, la normalidad de los residuos, para garantizar que la serie temporal ha sido modelada correctamente
# (Test de Jarque Bera y Shapiro-Wilk) y realizar una confirmación gráfica

# MODELO AR(4)
boxtestM1 = Box.test(modelo_arima.H1$residuals) # pvalue 0.9416972 --> aleatoriedad
JB.H1 = jarque.bera.test(modelo_arima.H1$residuals) # pvalue 0.8187785 --> normal
SW.H1 = shapiro.test(modelo_arima.H1$residuals) # pvalue 0.2903847--> normal

# Gráficamente vemos que los residuos son correctos porque siguen una normal
hist(modelo_arima.H1$residuals, col='blue', prob=T, ylim=c(0,20), xlim=c(-0.2,0.2))
lines(density(modelo_arima.H1$residuals))

# MODELO MA(1) --> cumple los tests estadísticos
boxtestM2 = Box.test(modelo_ma.H1$residuals) # pvalue 0.976203 --> aleatoriedad
JB2.H1 = jarque.bera.test(modelo_ma.H1$residuals) # pvalue 0.299881 --> normal
SW2.H1 = shapiro.test(modelo_ma.H1$residuals) # pvalue 0.1979714--> normal

# Gráficamente vemos que los residuos son correctos porque siguen una normal
hist(modelo_ma.H1$residuals, col='blue', prob=T, ylim=c(0,20), xlim=c(-0.2,0.2))
lines(density(modelo_ma.H1$residuals))

# MODELO MA(16) --> cumple los tests estadísticos
boxtestM3 = Box.test(modelo_ma2.H1$residuals) # pvalue 0.9539837--> aleatoriedad
JB3.H1 = jarque.bera.test(modelo_ma2.H1$residuals) # pvalue 0.8983741 --> normal
SW3.H1 = shapiro.test(modelo_ma2.H1$residuals) # pvalue 0.1761939-> normal

# Gráficamente vemos que los residuos son correctos porque siguen una normal
hist(modelo_ma2.H1$residuals, col='blue', prob=T, ylim=c(0,20), xlim=c(-0.2,0.2))
lines(density(modelo_ma2.H1$residuals))

#############################################################################
# SELECCIÓN DEL MEJOR MODELO

# MSE

# MODELO AR
# El MSE en entrenamiento es 0.1344656 y en test 0.01965443

# MODELO MA(1)
# El MSE en entrenamiento es 0.1466546 y en test 0.03969083

# MODELO MA(16)
# El MSE entrenamiento es 0.1112006 y en test 0.03969083

# Como se puede apreciar, el modelo AR es el que tiene menos MSE tanto en ajuste como en test, por lo que sería el más conveniente
# a priori para predecir. Si comparamos MA(1) y MA(16), MA(16) obtiene menos errores aunque probablemente la complejidad del
# modelo, evaluada en el número de parámetros, sea mucho mayor.

# A continuación, con el criterio de información de Akaike confirmamos estos resultados y escogemos finalmente un modelo para 
# predecir.


# AIC = 2k +nLog(RSS/n)
# k = grados de libertad; n = numero de datos; RSS = mua de los errores al cuadrado

AIC(modelo_arima.H1,modelo_ma.H1,modelo_ma2.H1)

# Como podemos ver, el modelo con menor valor de AIC es el AR (ARIMA(4,1,0)) CON -459.3873 y 5 parámetros.
# Si comparamos el model MA(1) con el MA(16), vemos que el MA(16) tiene un gran número de parámetros, por lo que la complejidad
# aumenta mucho. Además, el valor de AIC es mayor que el de MA(1). En definitiva, vemos que, a pesar de que la regla
# heurística nos inclinaría a elegir MA(16), por ser el último valor en el ACF mayor que 0, el criterio de información de Akaike
# termina por descartarlo. Por tanto, a elegir entre AR y MA, es preferible en este caso AR. De entre los dos MAs que tenemos,
# mejor que MA(1)




#############################################################################
# PREDICCIÓN CON EL MODELO MÁS FAVORABLE SEGÚN AIC

# Una vez validado el modelo, generamos la predicción para los datos de 1960. Para ello, debemos deshacer todos los cambios
# sobre el conjunto de datos para así recuperar la serie temporal original. Es decir, deshacer la diferenciación, devolver la
# estacionalidad, la tendencia y la transformación logarítmica.

# Partimos de la serie con la transformación logarítmica
serieEntera = serie.log
tiempo = 1:length(serieEntera)
# Generamos con el modelo lineal la tendencia estimada (aproximación funcional)
parametros = lm(serieEntera ~ tiempo)
TendEstimada = parametros$coefficients[1] + tiempo*parametros$coefficients[2]
# Sobre la serie transformada, eliminamos la tendencia
serieSinTend = serieEntera - TendEstimada
# Calculamos la estacionalidad con la función de autocorrelación y las medias
aux = ts(serieEntera, frequency=12)
aux = decompose(aux)$seasonal
estacionalidad = as.numeric(aux[1:12])
aux = rep(estacionalidad, length(serieSinTend)/length(estacionalidad))
# Calculada la estacionalidad, se la restamos a la serie temporal sin tendencia
serieSinTendEst =serieSinTend - aux
# Ajustamos un modelo ARIMA autoregresivo. Vimos que para asegurar la estacionaridad necesitábamos diferenciar una vez,
# por lo que indicamos d=1. Además, según el PACF, el último valor distinto de 0 es el 4
modelo = arima(serieSinTend, order=c(4,1,0))
# Generamos los valores ajustados (en entrenamiento)
valoresAjustados = serieSinTendEst + modelo$residuals
# y 12 predicciones (en test)
Predicciones = predict(modelo, n.ahead=NPred)
valoresPredichos = Predicciones$pred

# A continuación, devolvemos la estacionalidad tanto a los valores ajustados como predichos
valoresAjustados = valoresAjustados +aux 
valoresPredichos = valoresPredichos+estacionalidad

# Asimismo, devolvemos la tendencia
valoresAjustados = valoresAjustados+TendEstimada
tiempoPred = (tiempo[length(tiempo)]+(1:NPred))
TendEstimadaPred = parametros$coefficients[1]+tiempoPred*parametros$coefficients[2]
valoresPredichos = valoresPredichos + TendEstimadaPred

# Aplicamos una transformación exponencial a la serie con tendencia y estacionalidad para deshacer la transformación logarítmica
# que se realizó al principio para paliar los problemas en la varianza de la estacionalidad
valoresAjustados = exp(valoresAjustados) 
valoresPredichos = exp(valoresPredichos)

# Representamos los valores ajustados y predichos de la serie original
plot.ts(serie,xlim=c(1,max(tiempoPred)),ylim=c(100,650))
lines(valoresAjustados, col='blue')
lines(valoresPredichos, col='red')

# Hasta aquí llegaría nuestro estudio si no tuviéramos los datos reales del año 1960. En este caso sí los tenemos, por lo que 
# podemos comprobar cuán bueno ha sido nuestro ajuste. 

# Cargamos los valores reales de predicción para comparar con lo predicho
predReales = scan('pasajeros_1960.predict')
lines(tiempoPred,predReales,col='green')
# Sobre la gráfica anterior, en verde, representamos los verdaderos valores y a simple vista parecen ajustarse bastante bien.
# Evaluémos el error medio cometido entre los valores reales y los predichos.

ErrorMedio = sum(abs(predReales-valoresPredichos))
