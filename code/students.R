#setwd("D:/pruebas/R")
getwd()
datos <- read.csv("Students.csv")
#Mantener copias de los registros que serán eliminados para su posterior estimación de notas de matemáticas
datos2 <- read.csv("Students.csv") 

nrow(datos)
summary(datos)
head(datos[,1:8])
datos

#TIPOS DE VARIABLES

install.packages("printr")
install.packages("knitr")
library(printr)
library(knitr)

#Los enteros, los pasamos a numericos

tipos <- sapply(datos, class)
tipos
kable(data.frame(variables=names(tipos),clase=as.vector(tipos)))
datos[6:8] <- lapply(datos[6:8], as.numeric)

#Eliminación de datos irrelevantes
datos <- datos[,-(3:5)]
datos


#CEROS Y ELEMENTOS VACIOS
sapply(datos, function(x) sum(is.na(x)))

#no los hay, de ser así, estos registros serían eliminados 
#ya que los valores de los atributos seleccionados son necesarios para el estudio 

#OUTLIERS
boxplot.stats(datos$math.score)$out
boxplot.stats(datos$reading.score)$out
boxplot.stats(datos$writing.score)$out
boxplot.stats(datos$race.ethnicity)$out

#Borrar registros con resultado = 0 en alguna prueba
nrow(datos)
datos <- datos[!datos$math.score==0,]
nrow (datos)

#Guardar datos una vez preprocesados
write.csv(datos, file = "ResultadoNotas.csv" )

#ANÁLISIS DE DATOS
##################


#BLOXPLOTS
NombreColumnas <- c("","","","","","Matemáticas","Lectura","Escritura")
par(mfrow=c(2,2))
for(i in 1:ncol(datos)) {
  if (is.numeric(datos[,i])){
    boxplot(datos[,i], main = NombreColumnas[i], width = 100)
  }
}


#selección de grupos de datos que se quiere analizar
Mates    <- datos$math.score
Lectura  <- datos$reading.score
Escritura<- datos$writing.score
Genero   <- datos$gender
Raza     <- datos$race.ethnicity
Padres   <- datos$parental.level.of.education
Comida   <- datos$lunch
TestPrep <- datos$test.preparation.course

#Comprobación de la normalidad
install.packages("nortest")
library(nortest)

#Test de Saphiro Wilk
NormalidadSp <-matrix(c(shapiro.test(Mates)$p.value,
                        shapiro.test(Lectura)$p.value,
                        shapiro.test(Escritura)$p.value), ncol = 3, byrow = T)
colnames(NormalidadSp) <- NombreColumnas
NormalidadSp

#Mates
hist(Mates, main = paste("Histograma de matemáticas"))
qqnorm(Mates,main = paste("Normal Q-Q Plot para matemáticas"))
qqline(Mates,col="red")
shapiro.test(Mates)

#Lectura
hist(Lectura, main = paste("Histograma de Lectura"))
qqnorm(Lectura,main = paste("Normal Q-Q Plot para Lectura"))
qqline(Lectura,col="red")

#Escritura
hist(Escritura, main = paste("Histograma de Escritura"))
qqnorm(Escritura,main = paste("Normal Q-Q Plot para Escritura"))
qqline(Escritura,col="red")


shapiro.test(Mates)
shapiro.test(Lectura)
shapiro.test(Escritura)
tapply(Mates,Genero,shapiro.test)
tapply(Lectura,Genero,shapiro.test)
tapply(Escritura,Genero,shapiro.test)



#test de Anderson-Darling

Normalidad <-matrix(c(ad.test(Mates)$p.value,
                      ad.test(Lectura)$p.value,
                      ad.test(Escritura)$p.value), ncol = 3, byrow = T)

Normalidad <-matrix(c(ad.test(datos$math.score)$p.value,
                      ad.test(datos$reading.score)$p.value,
                      ad.test(datos$writing.score)$p.value), ncol = 3, byrow = T)
colnames(Normalidad) <- NombreColumnas
Normalidad



#Test de Lillie

lillie.test(Mates)
lillie.test(Lectura)
lillie.test(Escritura)

NormalidadLi <-matrix(c(lillie.test(Mates)$p.value,
                        lillie.test(Lectura)$p.value,
                        lillie.test(Escritura)$p.value), ncol = 3, byrow = T)
colnames(NormalidadLi) <- NombreColumnas
NormalidadLi

#Homogeneidad de la varianza de la prueba de matemáticas por genero
fligner.test(Mates ~ Genero, data = datos)

#Lectura
fligner.test(Lectura ~ Genero, data = datos)

#Escritura
fligner.test(Escritura ~ Genero, data = datos)

#Correlación de variables
corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("Estimación", "p-value")
for (i in 4:5) {
  spearman_test = cor.test(datos[,i],
                           datos[,"math.score"],
                           method = "spearman")
  corr_coef = spearman_test$estimate
  p_val = spearman_test$p.value
  # incluiomos el valor en la matriz de correlaciones
  pair = matrix(ncol = 2, nrow = 1)
  pair[1][1] = corr_coef
  pair[2][1] = p_val
  corr_matrix <- rbind(corr_matrix, pair)
  rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(datos)[i]
}
print(corr_matrix)

MatesHombre <- datos[datos$gender=="male",]$math.score
MatesMujer  <- datos[datos$gender=="female",]$math.score

##¿Lasmujeres obtienen mejores resultados en la prueba de matemáticas?
#tomaremos alpha = 0,05.
t.test(MatesHombre, MatesMujer, alternative = "greater") 

##Para el caso de la prueba de lectura
LectuHombre <- datos[datos$gender=="male",]$reading.score
LectuMujer  <- datos[datos$gender=="female",]$reading.score
t.test(LectuHombre, LectuMujer, alternative = "greater") 
summary(LectuHombre)
summary(LectuMujer)

##Para el caso de la prueba de escritura
EscriHombre <- datos[datos$gender=="male",]$writing.score
EscriMujer  <- datos[datos$gender=="female",]$writing.score
t.test(EscriHombre, EscriMujer, alternative = "greater") 


ggplot() + aes(x=Mates, fill =Genero) + geom_bar(position='dodge' )
ggplot() + aes(x=Lectura, fill =Genero) + geom_bar(position='dodge' )
ggplot() + aes(x=Escritura, fill=Genero) + geom_bar(position='dodge')



#Predecir la nota de matemáticas en función de la nota de las otras dos pruebas
modeloLectura   <- lm(Mates ~ Lectura, data = datos)
modeloEscritura <- lm(Mates ~ Escritura, data = datos)
modeloAmbas     <- lm(Mates ~ Lectura + Escritura, data = datos)
modeloLER       <- lm(Mates ~ Lectura + Escritura + Raza, data = datos)
modeloLEG       <- lm(Mates ~ Lectura + Escritura + Genero, data = datos)
modeloLEGR      <- lm(Mates ~ Lectura + Escritura + Genero + Raza, data = datos)
modeloLEGRP     <- lm(Mates ~ Lectura + Escritura + Genero + Raza + Padres, data = datos)
modeloLEGRC     <- lm(Mates ~ Lectura + Escritura + Genero + Raza + Comida, data = datos)
modeloLEGRT     <- lm(Mates ~ Lectura + Escritura + Genero + Raza + TestPrep, data = datos)
modeloLEGRPCT   <- lm(Mates ~ Lectura + Escritura + Genero + Raza + Padres + Comida + TestPrep, data = datos)

tablaCoef  <- matrix(c(1 , summary(modeloLectura)$r.squared,
                       2 , summary(modeloEscritura)$r.squared,
                       3 , summary(modeloAmbas)$r.squared,
                       4 , summary(modeloLER)$r.squared,
                       5 , summary(modeloLEG)$r.squared,
                       6 , summary(modeloLEGR)$r.squared,
                       7 , summary(modeloLEGRP)$r.squared,
                       8 , summary(modeloLEGRC)$r.squared,
                       9 , summary(modeloLEGRT)$r.squared,
                       10, summary(modeloLEGRPCT)$r.squared
), ncol = 2, byrow = TRUE)

colnames(tablaCoef) <- c("Modelo", "R^2")
tablaCoef

#Predecir nota de mates del registro eliminado
datos2[datos2$math.score==0,]

registro <- data.frame(
  Lectura   = 17,
  Escritura = 10,
  Genero    = "female",
  Raza      = "group C",
  Padres    = "some high school",
  Comida    = "free/reduced",
  TestPrep  = "none"
)
registro

predict(modeloLEGRPCT, registro)


#Predicción mediante particion de datos Training/Test
particion <- sample(2,nrow(datos),replace=TRUE, prob = c(0.75,0.25))
TrainData <- datos[particion==1,]
TestData  <- datos[particion==2,]

nrow(TrainData) #740
nrow(TestData) #259

TMates    <- TrainData$math.score
TLectura  <- TrainData$reading.score
TEscritura<- TrainData$writing.score
TGenero   <- TrainData$gender
TRaza     <- TrainData$race.ethnicity
TPadres   <- TrainData$parental.level.of.education
TComida   <- TrainData$lunch
TTestPrep <- TrainData$test.preparation.course

modeloTrain <- lm(formula = math.score ~ reading.score + writing.score + gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course, data=TrainData)

summary(modeloTrain)
predicTestData <- predict(modeloTrain, TestData, type = "response")

TestResult<-data.frame(
  real      = TestData$math.score,
  predicted = predicTestData,
  porc      = predicTestData*100/TestData$math.score,
  dif       = 100 - predicTestData*100/TestData$math.score,
  difPorcPos= abs(100 - predicTestData*100/TestData$math.score),
  difPos    = abs(predicTestData-TestData$math.score)
)
colnames(TestResult)<-c("Real","Predecido", "Porcentaje", "Dif%", "Dif%Pos", "DifPos")
kable(TestResult)


#Guardar datos estimados
write.csv(TestResult, file = "NotasEstimadas.csv" )

mean(TestResult$`Dif%Pos`)
mean(TestResult$DifPos)

registroBorrado <- data.frame(
  reading.score   = 17,
  writing.score   = 10,
  gender          = "female",
  race.ethnicity  = "group C",
  parental.level.of.education = "some high school",
  lunch                       = "free/reduced",
  test.preparation.course     = "none"
)
predict(modeloTrain, registroBorrado)

