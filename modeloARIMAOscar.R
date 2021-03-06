# Leer datos de un PDF
# install.packages("fUnitRoots")
# install.packages("forecast")
library(tabulizer)
library(dplyr)
library(stringr)
library(fUnitRoots)
library(forecast)
library(lmtest)

pages<-extract_tables("C01-Importaci�n-de-combustibles-VOLUMEN-2020-03.pdf")#datos2020
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


## Creaci�n del Modelo de GasSuperior

# Creamos la serie de tiempo
superior <- ts(dataSet$GasSuperior, start=c(2001, 1), end=c(2020, 3), frequency=12)
View(superior)

# Establecemos datos a usar

# Datos de entrenamiento
train <- head(superior, round(length(superior) * 0.7))
h <- length(superior) - length(train)
test <- tail(superior, h)


### Iniciamos la construcci�n del modelo

# Primero, analizaremos el gr�fico de la serie
plot(superior)
abline(reg=lm(superior~time(superior)), col=c("red"))

# Es una serie con frecuencia anual desde enero 2001 hasta marzo 2020. La l�nea de la media nos indica que la serie tiene una tendencia a crecer.
# Cabe destacar que, durante los a�os 2011 y 2015 el precio se mantuvo estable, es hasta despu�s del 2015 que el precio creci� de nuevo.


### Descomposici�n de la serie
dec.superior<-decompose(superior)
plot(dec.superior)

# La tendencia no es estacionaria en varianza, pues los rangos var�an bastante con el tiempo. La serie tiene estacionalidad.

### Estimar par�metros del modelo
# Dado que no es estacionaria en varianza le aplicaremos una transformaci�n logaritmica para hacerla constante en varianza.
# Lo haremos con la serie de entrenamiento que es la que nos ayudar� a predecir
logTrain <- log(superior)

# Intento de estacionalizar la serie con una transformaci�n logar�tmica
plot(decompose(logTrain))

plot(logTrain)
abline(reg=lm(logTrain~time(logTrain)), col=c("red"))
# Al parecer se logra mejorar que la varianza sea un poco m�s constante Debemos verificar si es estacionaria en media. Si tiene raices unitarias podemos decir que no es estacionaria en media y hay que aplicar procesos de diferenciaci�n.
# Uno de los factores que nos llevan a pensar que en efecto la varianza es un poco m�s constante es que la gr�fica posee menos oscilaci�n.

# Usando la prueba de Dickey-Fuller
adfTest(train)
# El valor p es mayor a 0.05, por lo que no se puede rechazar la hip�tesis nula de las raices unitarias.

# Entonces, hacemos la segunda prueba.
unitrootTest(train)
# El valor p tampoco es mayor a 0.05. Esto significa que la serie no es estacionaria en media.

### Aplicando diferenciaci�n
# Se aplicar� una diferenciaci�n con el fin de hacer la serie estacionario en media.
adfTest(diff(train))
unitrootTest(diff(train))
# En ambas pruebas el valor p es menor a 0.05. Con una sola diferenciaci�n se puede rechazar la hip�tesis nula de las ra�ces unitarias. 

# - d=1 

### Identificando par�metros p y q

#### Funci�n de autocorrelaci�n:
acf(logTrain,80)
# Con respecto al presente gr�fico, podemos ver que se anula el valor j se anula luego del tercer retardo.
# - Se propone un valor q = 3

#### Funci�n de correlaci�n parcial
pacf(logTrain,80)
# Se anula luego del segundo retardo, por ello:
# - Se propone un valor p = 2

#### Resumen de par�metros:
# - d = 1
# - p = 2
# - q = 3
# ARIMA(2,1,3)

### �Estacionalidad en la serie?

# crear

decTrain <- decompose(superior)
plot(decTrain$seasonal)
# A simple vista, parece que la serie s� tiene estacionalidad. 

### Uso de la funci�n de autocorrelaci�n
# Se usar� la serie estacionarizada.
Acf(diff(logTrain),84)

# Al parecer s� existe estacionalidad en la serie. Para tener una idea de los par�metros estacionales veremos las funciones de autocorrelaci�n y autocorrelaci�n parcial con 36 resagos para ver en que momentos son los picos estacionales. Se usar� la serie estacionarizada.


Pacf(diff(logTrain),80)

fitArima <- arima(logTrain,order=c(2,1,3),seasonal = c(2,1,0))
fitAutoArima <- auto.arima(train)


coeftest(fitArima)
# Son significativos

coeftest(fitAutoArima)
# Tambi�n son significativos los coeficientes.

### An�lisis de residuales
qqnorm(fitArima$residuals)
qqline(fitArima$residuals)

checkresiduals(fitArima)


### Analizando los residuos del modelo generado de forma autom�tica por R
qqnorm(fitAutoArima$residuals)
qqline(fitAutoArima$residuals)
checkresiduals(fitAutoArima)


### Predicci�n con el modelo generado
fitArima %>%
  forecast(h) %>%
  autoplot() + autolayer(log(test))

### Predicci�n con el modelo automatico
fitAutoArima %>%
  forecast(h) %>%
  autoplot() + autolayer(test)



