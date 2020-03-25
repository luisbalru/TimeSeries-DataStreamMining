# LUIS BALDERAS RUIZ. 77145416N
# luisbalderas@correo.ugr.es
# Ejercicio guiado. Curso 2019-2020

# setwd('/home/luisbalru/Documentos/Master/TimeSeries-DataStreamMining/TimeSeries/data')

# OBJETIVO: Entrenar y aplicar un modelo paramétrico sobre la serie para predecir el número
# de pasajeros de avión en 1960. Para ello, la serie debe ser estacionaria. 


# Cargamos la librería de series temporales
install.packages('tseries')
library(tseries)

NPred = 12 # Valores a predecir
NTest = 12 # Valores para test

serie = scan('pasajeros_1949_1959.dat')
# Aplicamos la metodología Box-Jenkins para dividir la serie en su parte de tendencia, estacionalidad 
# y la componente irregular o aleatoria. Como estamos trabajando con unos datos mensuales durante años,
# la serie presenta una estacionalidad anual. 
serie.ts = ts(serie,frequency = 12)
plot(decompose(serie.ts))

# Como se puede ver, la varianza se mueve en valores entre -40 y 40, por lo que es demasiado grande.
# Aplicamos un logaritmo para acotar esa variabilidad
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

#############################################################################################
# MODELADO Y ELIMINACIÓN DE TENDENCIA

# La serie parece tener una tendencia lineal, así que asumimos que así es y luego lo comprobamos con test estadísticos
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

# Partiendo de la suposición de estacionalidad anual, utilizamos la función decompose y su parámetro
# de estacionalidad en los 12 meses del test (los últimos) para eliminarla.
# Periodo de estacionalidad
k=12
estacionalidad.H1 = decompose(serie.ts)$seasonal[1:k]

aux = rep(estacionalidad.H1, length(serieTr)/length(estacionalidad.H1))
serieTr.SinTend.H1 = serieTr.SinTend.H1 - aux
serieTs.SinTend.H1 = serieTs.SinTend.H1 - estacionalidad.H1
plot.ts(serieTr.SinTend.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs.SinTend.H1, col='red')
# quedando la serie sin estacionalidad.

#############################################################################################
# ESTACIONARIDAD

# Visualizando ACF y encontrando que tiende a 0 muy rápidamente, podríamos pensar que es estacionaria
acf(serieTr.SinTend.H1) # La tendencia a 0 es lenta y sobrepasa 0. Salimos de dudas con el test aumentado de Dickey-Fuller

# Aplicamos el test aumentado de Dickey-Fuller
adftest.H1 = adf.test(serieTr.SinTend.H1)

# Obtenemos que el p-valor es 0.6427417 > 0.05, por lo que no es estacionaria. Para conseguirlo, diferenciamos.
serieTr.SinTendEstDiff.H1 = diff(serieTr.SinTend.H1)
serieTs.SinTendEstDiff.H1 = diff(serieTs.SinTend.H1)

# Volvemos a aplicar el mismo test
adftest.H1 = adf.test(serieTr.SinTendEstDiff.H1) # Valor 0.01 < 0.05, por lo que es estacionaria con un nivel de confianza
# superior al 95 %

# A continuación, lo vemos con las gráficas de autocorrelación y autocorrelación parcial, ya que tienden a cero muy rápidamente
acf(serieTr.SinTendEstDiff.H1)
pacf(serieTr.SinTendEstDiff.H1)
