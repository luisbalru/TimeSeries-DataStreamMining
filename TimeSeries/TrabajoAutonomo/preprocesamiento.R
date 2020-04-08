# LUIS BALDERAS RUIZ. 77145416N
# luisbalderas@correo.ugr.es
# Ejercicio de trabajo autónomo. Series temporales. Curso 2019-2020



######################################################################
#                        PREPROCESAMIENTO                            #
######################################################################
# setwd("~/Documentos/Master/TimeSeries-DataStreamMining/TimeSeries/TrabajoAutonomo")
# Lectura de datos
data = read.csv('./data/2870.csv', header=T, sep=';')
data$Fecha = as.Date(data$Fecha)
data$Year = substring(data$Fecha, 1,4)
data$Month = substring(data$Fecha,6,7)
data$TMax = data$Tmax
data$Tmax = NULL
data$Tmax = data$TMax
data$TMax = NULL
# Resumen estadístico de las variables
summary(data)

# Visualización de las variables respecto de Tmax
temp = data
plotY = function(x,y){
  plot(temp[,y]~temp[,x],xlab=paste(names(temp)[x]), ylab=names(temp)[y])
}
par(mfrow=c(3,4))
x = sapply(1:(dim(temp)[2]-1), plotY,dim(temp)[2])
par(mfrow=c(1,1))

# Vemos que hay una alta correlación con Tmed y Tmin (menos). Lo confirmamos
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
library(dplyr)
my_data = data %>% select(Tmin,Tmed,Racha,Vmax,TPrec,Prec1,Prec2,Prec3,Prec4,Tmax)
chart.Correlation(my_data, histogram=TRUE, pch=19)

######################################################################
# Outliers

library(outliers)
library(EnvStats)

# No encontramos ningún outlier univariante en la columna Tmax
q3 = quantile(data2$Tmax,0.75)
q1 = quantile(data2$Tmax,0.25)
iqr = q3 -q1

extremo.s.normal = q3 + 1.5*iqr
extremo.l.normal =  q1- 1.5*iqr
extremo.s.extremo = q3 + 3*iqr
extremo.l.extremo =  q1- 3*iqr

vector_normal = data2$Tmax > extremo.s.normal | data2$Tmax < extremo.l.normal
vector_extremo= data2$Tmax > extremo.s.extremo | data2$Tmax < extremo.l.extremo

######################################################################
# Transformación a serie temporal DIARIA
library(tseries)
serie.ts = ts(data$Tmax,frequency = 30)

######################################################################
# Valores perdidos. 

install.packages('imputeTS')

library(imputeTS)

plotNA.distribution(serie.ts)
plotNA.distributionBar(serie.ts)
plotNA.gapsize(serie.ts)
statsNA(serie.ts)

# Probamos con Kalman
imp = na_kalman(serie.ts)
plotNA.imputations(serie.ts,imp)
plotNA.distribution(imp)
plot(decompose(imp))
# Probamos con Seadec
imp2 = na_seasplit(serie.ts,algorithm='ma')
plotNA.imputations(serie.ts,imp2)
plotNA.distribution(imp2)
plot(decompose(imp2))

# Los plots me hacen pensar que la imputación más realista es la de Seadec, ya que en la franja donde hay más valores perdidos 
# la hace con cierta irregularidad y no como una línea recta (Kalman)

write.table(imp2,file='Estacion2870_diaria.txt', sep='\t')

########################################################################
# Transformación a serie temporal mensual
# Recupero la imputación de datos sobre el dataset original
data$Tmax = imp2
serie_mes = c(1:57)

meses1 = c('05','06','07','08','09','10','11','12')
mesesn = c('01','02')
meses = c('01','02','03','04','05','06','07','08','09','10','11','12')
anios = c(2015,2016,2017)


cont = 1
for(i in meses1){
  serie_mes[cont] = mean((data %>% filter(Month==i,Year==2013))$Tmax)
  cont = cont+1
}
aux = 1
for(i in meses){
  if(aux<5){
    serie_mes[cont] = mean((data %>% filter(Month==i,Year==2014))$Tmax)
  }
  else{
    serie_mes[cont] = 0.8*mean((data %>% filter(Month==i,Year==2014))$Tmax)+0.2*serie_mes[cont-12]
  }
  cont = cont+1
}

for(i in anios){
  for(j in meses){
    serie_mes[cont] =  0.8*mean((data %>% filter(Month==j,Year==i))$Tmax)+0.2*serie_mes[cont-12]
    cont = cont+1
  }
}

for(i in mesesn){
  serie_mes[cont] = 0.8*mean((data %>% filter(Month==i,Year==2018))$Tmax)+0.2*serie_mes[cont-12]
  cont = cont+1
}

serie.mes = ts(serie_mes,frequency=12)
plot(decompose(serie.mes))
write.table(serie.mes,file='Estacion2870_mensual.txt', sep='\t')