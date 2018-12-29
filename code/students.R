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

#selección de grupos de datos que se quiere analizar
Mates    <- datos$math.score
Lectura  <- datos$reading.score
Escritura<- datos$writing.score
Genero   <- datos$gender
Raza     <- datos$race.ethnicity

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


#Predecir la nota de matemáticas en función de la nota de las otras dos pruebas
modeloLectura   <- lm(Mates ~ Lectura, data = datos)
modeloEscritura <- lm(Mates ~ Escritura, data = datos)
modeloAmbas     <- lm(Mates ~ Lectura + Escritura, data = datos)
modeloLER       <- lm(Mates ~ Lectura + Escritura + Raza, data = datos)
modeloLEG       <- lm(Mates ~ Lectura + Escritura + Genero, data = datos)
modeloLEGR      <- lm(Mates ~ Lectura + Escritura + Genero + Raza, data = datos)

tablaCoef  <- matrix(c(1, summary(modeloLectura)$r.squared,
                       2, summary(modeloEscritura)$r.squared,
                       3, summary(modeloAmbas)$r.squared,
                       4, summary(modeloLER)$r.squared,
                       5, summary(modeloLEG)$r.squared,
                       6, summary(modeloLEGR)$r.squared
                       ), ncol = 2, byrow = TRUE)

colnames(tablaCoef) <- c("Modelo", "R^2")
tablaCoef

#Predecir nota de mates del registro eliminado
datos2[datos2$math.score==0,]

registro <- data.frame(
  Lectura = 17,
  Escritura = 10,
  Genero = "female",
  Raza = "group C"
)

predict(modeloAmbas, registro)
