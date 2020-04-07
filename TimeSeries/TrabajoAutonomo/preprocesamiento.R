# LUIS BALDERAS RUIZ. 77145416N
# luisbalderas@correo.ugr.es
# Ejercicio de trabajo autónomo. Series temporales. Curso 2019-2020



######################################################################
#                        PREPROCESAMIENTO                            #
######################################################################

# Lectura de datos
data = read.csv('./data/2870.csv', header=T, sep=';')
data$TMax = data$Tmax
data$Tmax = NULL
data$Tmax = data$TMax
data$TMax = NULL
data$Fecha = as.Date(data$Fecha)
data$Year = substring(data$Fecha, 1,4)
data$Month = substring(data$Fecha,6,7)
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

# Vemos que hay una alta correlación con Tmed y Tmin (menos). Luego lo confirmaremos

######################################################################
# Valores perdidos. 
# Decido obviar las tuplas con valores perdidos porque, para la característica
# que queremos modelar, siempre que aparece un NA en ella, también aparecen en las demás relacionadas
# con temperatura así que, la imputación de cualquier medida no tendría más efecto que el de introducir
# ruido. Además, como son pocos instancias (apenas 200, todas alejadas de las últimas fechas), no parece
# que pueda tener un impacto grande en la predicción.

data2 = na.omit(data)

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