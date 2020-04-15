# LUIS BALDERAS RUIZ. 77145416N
# luisbalderas@correo.ugr.es
# Ejercicio guiado. Curso 2019-2020

# setwd("~/Documentos/Master/TimeSeries-DataStreamMining/TimeSeries/TrabajoAutonomo")

NPred = 2 # Valores a predecir
NTest = 8 # Valores para test

library(tseries)

serie = scan('./data/Estacion2870_mensual.txt')

serie.ts = ts(serie,frequency = 12)
plot(decompose(serie.ts))

serieTr = serie.ts[1:(length(serie.ts)-NTest)]
tiempoTr = 1:length(serieTr)
serieTs = serie.ts[(length(serie.ts)-NTest+1):length(serie)]
tiempoTs = (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

plot.ts(serieTr, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs,col='red')
