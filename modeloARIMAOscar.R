# Leer datos de un PDF
# install.packages("UnitrootTests")
library(tabulizer)
library(dplyr)
library(stringr)
library(UnitrootTests)

pages<-extract_tables("C01-Importación-de-combustibles-VOLUMEN-2020-03.pdf")#datos2020
datosImp <- do.call(rbind, pages)
nombresVar<-datosImp[1,]
datosImp<-as.data.frame(datosImp[2:nrow(datosImp),])
nombresVar[c(1,4,5,6,8,10,11,15,16,21,23,24)]<-c("Anio","GasAviacion","GasSuperior","GasRegular","rTurboJet","DieselLS","DieselULS","AceitesLub","GrasasLub","PetroleoReconst","Orimulsion","MezclasOleosas")
names(datosImp)<-nombresVar

dataSet = read.csv("datos.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ",", encoding="UTF-8" )

dataset = dataSet[c(1,2,5,6, 9,10)]
dataset = dataset[-c(46, 96, 146, 196),]
dataset$Diesel[dataset$Diesel =="-"] <- 0
dataset$DieselLS[dataset$DieselLS =="-"] <- 0
dataset2 = dataset
options(digits=9)
dataset2$Anio = as.numeric(dataset2$Anio)
dataset2$Mes = as.numeric(dataset2$Mes)
dataset2$GasSuperior = as.numeric(gsub(",", "", dataset2$GasSuperior))
dataset2$GasRegular = as.numeric(gsub(",", "", dataset2$GasRegular))
dataset2$Diesel = as.numeric(gsub(",", "", dataset2$Diesel))
dataset2$DieselLS = as.numeric(gsub(",", "", dataset2$DieselLS))
#Unir Diesel
dataset2$Diesel = dataset2$Diesel + dataset2$DieselLS
dataset2 = dataset2[-6]
dataSet = dataset2
View(dataSet)


diesel <- ts(dataSet$Diesel, start=c(2001, 1), end=c(2020, 3), frequency=12)

# Establecemos datos a usar
train <- head(diesel, round(length(diesel) * 0.7))
h <- length(diesel) - length(train)
test <- tail(diesel, h)
start(diesel)
end(diesel)

## INICIA LA CONSTRUCCION DEL MODELO

# Saber la frecuencia de la serie
frequency(diesel)

# Ver el gráfico de la serie
plot(diesel)
abline(reg=lm(diesel~time(diesel)), col=c("red"))

# Es una serie con frecuencia anual desde enero 2001 hasta marzo 2020. No se denota un comportamiento específico. 


### Descomposición de la serie
dec.diesel<-decompose(diesel)
plot(dec.diesel)

# Podemos observar una serie con tendencia a aumentar, que no es estacionaria en varianza, y además tiene estacionalidad.

### Esimar Parámetros del Modelo
# Cómo no es estacionaria en varianza le haremos una transformación logaritmica para hacerla constante en varianza.Lo haremos con la serie de entrenamiento que es la que nos ayudará a predecir
logTrain <- log(diesel)

# Intento de estacionalizar la serie con una transformación logarítmica
plot(decompose(logTrain))

plot(logTrain)
abline(reg=lm(logTrain~time(logTrain)), col=c("red"))
# Al parecer se logró hacer constante la serie en varianza. Debemos verificar si es estacionaria en media. Si tiene raices unitarias podemos decir que no es estacionaria en media y hay que aplicar procesos de diferenciación.
# Se puede apreciar menos oscilación con respecto a la varianza

adfTest(train)


