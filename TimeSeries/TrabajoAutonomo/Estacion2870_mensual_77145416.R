# LUIS BALDERAS RUIZ. 77145416N
# luisbalderas@correo.ugr.es
# Ejercicio guiado. Curso 2019-2020

# setwd("~/Documentos/Master/TimeSeries-DataStreamMining/TimeSeries/TrabajoAutonomo")


library(tseries)

serie = scan('./data/Estacion2870_mensual.txt')

serie.ts = ts(serie,frequency = 12)
plot(decompose(serie.ts))