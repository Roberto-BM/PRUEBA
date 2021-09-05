###PREGUNTA 1
#Para la funcion tangente hiperbolica

x1 <- seq(-10,10,by=0.5)
x1

y1 <- tanh(x)
y1

#Para la derivada de la y

yprima <- cosh(x1)^-2
yprima

#Para ver las graficas

plot(x,y)
plot(x,yprima)

###PREGUNTA 2

#De la ecuacion Y=AX^B, sean los valores X e Y
# LogY = LogA + B*LogX; donde Y' = LongY, X' = LongX , A = LongA
# n = cantidad de valores

x <- c(2.5,3,4,5,5.5,6,7)
y <- c(12.5,10,7,4.5,4,3,3.5)

n <- length(x=y)
n

Yprima <- log10(y)
Yprima

Xprima <- log10(x)
Xprima

B <- (n*(sum(Xprima*Yprima))-(sum(Xprima))*(sum(Yprima)))/((n*(sum(Xprima^2)))-(sum(Xprima))^2)
B

#Para A':  A' = media(Y') - B*media(X'); entonces

Aprima <- mean(Yprima) - B*mean(Xprima)
Aprima

# Pero A' = longA; resolviendo tenemos

A <- 10^Aprima
A

#La ecuacion quedaria Y = 46.9656*X^-1421563

### PREGUNTA 3

#AREA DE TRIANGULO

#lados de trianguo
a <- 2
b <- 3
c <- 4
#angulo entre lados consecutivos(en radianes)

M <- 50*pi/180
sin(M)

Area <- function(a,b,c) {
  S <- (a*b)*(sin(M))/2
  return(S)
}
Area(a,b,c)

#PERIMETRO DE TRIANGULO

perimetro <- function(a, b, c) {
  P <- a+b+c
  return(P)
}
perimetro(a,b,c)

### PREGUNTA 4: Ecuacion de retardo de SCS

#CALCULO DE Tc
#L == longitud hidraulica en km
#CN == numero de curva SCS
#S == pendiente promedio de la cuenca en m/m

Tc <- function(L, CN, S) {
  Ecuacion <- (0.0136*(L^0.8)*((1000/CN)-9)^0.7)/S^0.5
  return(Ecuacion)
}
Tc(5.5,50,20)


### PREGUNTA 5
#1° Ingresamos la matriz del Sistema

A1 <- matrix(c(2,1,3,
                    5,4,1,
                     1,-1,-4), nrow = 3)
A1

# 2 Matriz de terminos independientes
B1 <- matrix(c(7,-19,4), nrow = 3)

# Del algrebra lineal de sabe A*X = B, entendemos que A es la matriz del sistema,
# B la matriz de los terminos independientes y X la matrix de las incognitas; entonces

Solucion_xyz <- solve(t(A1)%*%A1)%*%t(A1)%*%B1
Solucion_xyz

### PROBREMA 6
#Leer el archivo
library(rgdal)
library(sp)
library(tidyverse)

getwd()
setwd("D:/UNMSM/Progra/Codigos de clase/data_parcial")

shape_datapp <- readOGR(dsn = ".", layer = "uh_datos")
shape_datapp
head(shape_datapp)

# Agregamos el campo de pp promedio por AAA

datos1 <- shape_datapp@data
datos1

 datos_2 <- datos1%>%
  group_by(AAA)%>%
  summarise(mean(pcp))
 datos_2
 
par(mfrow=c(1, 2))
 
boxplot(datos1$pcp ~ datos1$AAA,
        main="Comportamiento de los Precipitacion por AAA",
        xlab = "Autoridad Administrativa del agua",
        ylab= "Precipitacion Total Multianual en (mm)",
        border="red")

boxplot(datos1$pcp ~ datos1$AAA,
        main="Comportamiento de los Evapotranspiración 
        potencial total multianual por AAA",
        xlab = "Autoridad Administrativa del agua",
        ylab=  "Evapotranspiración Potencia Total Multianual en mm",
        border="blue")

# Comportamiento de ambas variables en comparacion a los niveles de precipitacion

boxplot(datos1$pcp,datos1$pet, names = c("PCP","PET"),
        main="Diagrama de Cajas",ylim=c(0,3000),
        xlab="Valores Climatologicos para las AAA",
        ylab="Valores de Precipitacion en mm",
        col=c("red","blue"))

# Para el calculo de indice de aridez. Agregamos a datos1 el ia

datos_1 <- mutate(datos1, ia=(pcp/pet))
datos_1

# Ponemos las condiciones

clasificacion_ia<-c()
for(x in ia){
  if(1<=x){
    m<- print("Húmedo")
  }else if(0.7<=x){
    m<- print("Subhúmedo húmedo")
  }else if(0.5<=x){
    m<- print("Subhúmedo seco")
  }else if(0.2<=x){
    m<- print("semiarido")
  }else if(10.05<=x){
    m<- print("arido")
  }else if(0<=x){
    m<- print("Hiperarido")
  }
  clasificacion_ia<-c(m,clasificacion_ia)
}
length(clasificacion_ia)
clas<-tibble(iaz,clasificacion_ia)
view(clas)

# Seleccionamos el AAA, en este caso sera la AAA = "V"
#facet para remarcar el AAA seleccionado

AAA_interes <- datos1%>%
  group_by(AAA="V")
AAA_interes

 plot_1 <- ggplot(AAA_interes, aes(x = pcp, y =pet)) + geom_point()+
  geom_smooth() + 
  ggtitle("Cuadro de dispersion de Valores de PCP VS PET") + 
  theme_linedraw() + facet_wrap(~AAA)
 
plot_1 + xlab("Precipitacion Total Multianual(mm)") + 
  ylab ("Evapotranspiracion Potencial Multianual(mm)")
#
library(sf)
install.packages("sf")
install.packages("mapview")
library(mapview)
# Puntos

P1 <- st_point(c(272841.7,8666459.9))
P2 <- st_point(c(272893.6,8666456.9))
P3 <- st_point(c(272892.5,8666446.1))
P4 <- st_point(c(272913.8,8666441.5))
P5 <- st_point(c(272911.2,8666399.9))
P6 <- st_point(c(272837.5,8666407.9))

Coordenadas <- cbind(c(272841.7,272893.6,272892.5,272913.8,272911.2,272837.5),
              c(8666459.9,8666456.9,8666446.1,8666441.5,8666399.9,8666407.9))
Poligono <- Polygon(Coordenadas)

# Colocamos sistema de referencia a los puntos

SR <- st_sfc(P1,P2,P3,P4,P5,P6, crs = st_crs(32718))
SR

mapview(SR)
Poligono <- Polygons(list(Poligono), 1)

SPlgs <- SpatialPolygons(list(Poligono))
df <- data.frame(ID = 1)

Spacial_Poligono <- SpatialPolygonsDataFrame(SPlgs, df)

Plot <- plot(Spacial_Poligono, axes=T, col = 'Black',
     main="Escuela Profesional Ingenieria Geografica")
mapview(Spacial_Poligono)

