#install.packages("sandwich")
library(foreign)
library(dplyr)
library(ggplot2)
library(WDI)
library(ggthemes)
library(gridExtra)
library(ggstance)
library(ggrepel)
library(rworldmap)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(rgdal)
library(pacman)
library(stargazer)
library(car) 
library(lmtest)
library(sandwich)
library(quantmod)
library(tidyquant)


#Pregunta 2

#(a)

#Descargamos las bases y las guardamos en objetos

microsoft<-tq_get("MSFT", get = "stock.prices",from = "2000-01-01", to = "2018-08-01", periodicity = "monthly")
apple<-tq_get("AAPL", get = "stock.prices",from = "2000-01-01", to = "2018-08-01", periodicity = "monthly")

df1<-data.frame(apple$date, microsoft$adjusted, apple$adjusted)

#(b)

funcion1<-function(x, nacc=1, retlog=TRUE, grafret=FALSE, showtest=FALSE){
  if(retlog==TRUE){
    base<-x[,2:ncol(x)]
    n<-nrow(base)
    fecha<-x[,1]
    fecha1<-fecha[2:n]
    Z<-sapply(base, function(i){log(i[2:n]/i[1:(n-1)])})
    work<-data.frame(fecha1,Z)
    nacc1<-1+nacc
    a<-ggplot(work, aes(x=fecha1,y=cumprod(1+work[,nacc1])-1)) + geom_line() + geom_point(col="Red")+ggtitle("Retornos Mensuales Acumulados")
    b<-ggplot(work, aes(x=fecha1,y=work[,nacc1])) + geom_line() + geom_point(col="Red")+ggtitle("Retornos Mensuales")
    desvia=(work[,nacc1]-mean(work[,nacc1]))
    sumaske3=sum(desvia^3)/n
    sumaske2=(sum(desvia^2)/n)^(1.5)
    sumakur4=sum(desvia^4)/n
    sumakur2=(sum(desvia^2)/n)^2
    skew=sumaske3/sumaske2
    kurt=sumakur4/sumakur2
    jb=n*((skew^2/6)+((kurt-3)^2/24))
    pval<-1-pchisq(jb,df=2)
  }
  else{
    base<-x[,2:ncol(x)]
    n<-nrow(base)
    fecha<-x[,1]
    fecha1<-fecha[2:n]
    nacc1<-1+nacc
    Z<-sapply(base, function(i){(i[2:n]-i[1:(n-1)])/i[1:(n-1)]})
    work<-data.frame(fecha1,Z)
    a<-ggplot(work, aes(x=fecha1,y=cumprod(1+work[,nacc1])-1)) + geom_line() + geom_point(col="Red")+ggtitle("Retornos mensuales Acumulados")
    b<-ggplot(work, aes(x=fecha1,y=work[,nacc1])) + geom_line() + geom_point(col="Red")+ggtitle("Retornos mensuales")
    desvia=(work[,nacc1]-mean(work[,nacc1]))
    sumaske3=sum(desvia^3)/n
    sumaske2=(sum(desvia^2)/n)^(1.5)
    sumakur4=sum(desvia^4)/n
    sumakur2=(sum(desvia^2)/n)^2
    skew=sumaske3/sumaske2
    kurt=sumakur4/sumakur2
    jb=n*((skew^2/6)+((kurt-3)^2/24))
    pval<-1-pchisq(jb,df=2)
  }
  if(grafret==FALSE & showtest==FALSE){
    return(list(work,b))
  }
  else if(grafret==FALSE & showtest==TRUE){
    return(list(work,b, paste0("El estadístico es ",jb," y su valor p es: ", pval)))
  }
  else if(grafret==TRUE & showtest==FALSE){
    return(list(work,a))
  }
  else{
    return(list(work,a, paste0("El estadístico es ",jb," y su valor p es: ", pval))) 
  }
}

#Probando que sirve
funcion1(df1, nacc=1, showtest=TRUE)


#Pregunta 3

#(a)

set.seed(1234567)
R=10000
n=c(50,100,500,1000) 
betas_0=matrix(NA,nrow = R, ncol=4) 
betas_1=matrix(NA,nrow = R, ncol=4) 
beta0=2
beta1=2.5
beta2=1.0
beta3=0.8

for (j in 1:length(n)) {
  
  x_1=rnorm(n[j],20,1) 
  for (i in 1:R) {
    e=rnorm(n[j],0,1)
    x_2=beta3*x_1+e
    u=rnorm(n[j],0,1)
    v=beta2*x_2+u
    y=beta0+beta1*x_1+v
    modelo=lm(y~x_1) 
    betas_0[i,j]=modelo$coef[1]
    betas_1[i,j]=modelo$coef[2]
  }
}

### para beta_0

#n=50
E1_beta0 = mean(betas_0[,1])
V1_beta0 = var(betas_0[,1])
sesgo_01=abs(mean(betas_0[,1])-beta0)
#definimos el sesgo como la diferencia en valor absoluto del valor del parámetro y la media del experimento
sesgo_01


#n=100
E2_beta0 = mean(betas_0[,2])
V2_beta0 = var(betas_0[,2])
sesgo_02=abs(mean(betas_0[,2])-beta0)
sesgo_02

#n=500
E3_beta0 = mean(betas_0[,3])
V3_beta0 = var(betas_0[,3])
sesgo_03=abs(mean(betas_0[,3])-beta0)
sesgo_03

#n=1000
E4_beta0 = mean(betas_0[,4])
V4_beta0 = var(betas_0[,4])
sesgo_04=abs(mean(betas_0[,4])-beta0)
sesgo_04


### para beta_1

#n=50
E1_beta1 = mean(betas_1[,1])
V1_beta1 = var(betas_1[,1])
sesgo_11=abs(mean(betas_1[,1])-beta1)
#definimos el sesgo como la diferencia en valor absoluto del valor b1 y la media del experimento
sesgo_11


#n=100
E2_beta1 = mean(betas_1[,2])
V2_beta1 = var(betas_1[,2])
sesgo_12=abs(mean(betas_1[,2])-beta1)
sesgo_12

#n=500
E3_beta1 = mean(betas_1[,3])
V3_beta1 = var(betas_1[,3])
sesgo_13=abs(mean(betas_1[,3])-beta1)
sesgo_13

#n=1000
E4_beta1 = mean(betas_1[,4])
V4_beta1 = var(betas_1[,4])
sesgo_14=abs(beta1-mean(betas_1[,4]))
sesgo_14


#Compilamos los resultados en un dataframe
a <- data.frame("Parámetro"= c("Beta 0", "Beta 0", "Beta 0", "Beta 0",
                               "Beta 1", "Beta 1", "Beta 1", "Beta 1"),
                "n" = c(n, n) , 
                "Valor pob. beta" = c(beta0, beta0, beta0, beta0,
                                      beta1, beta1, beta1, beta1),
                "Esperanza" = c(E1_beta0, E2_beta0, E3_beta0, E4_beta0,
                          E1_beta1, E2_beta1, E3_beta1, E4_beta1),
                "Varianza" = c(V1_beta0, V2_beta0, V3_beta0, V4_beta0,
                            V1_beta1, V2_beta1, V3_beta1, V4_beta1),
                "Sesgo" = c(sesgo_01, sesgo_02, sesgo_03, sesgo_04,
                            sesgo_11, sesgo_12, sesgo_13, sesgo_14)
                )
print(a)



#(b)

c1 <- seq(min(betas_1[,1]), max(betas_1[,1]), length=R)
densidad_normal1 <- data.frame(c1=c1, f1=dnorm(c1, mean(betas_1[,1]), sd(betas_1[,1])))

c2 <- seq(min(betas_1[,2]), max(betas_1[,2]), length=R)
densidad_normal2 <- data.frame(c2=c2, f2=dnorm(c2, mean(betas_1[,2]), sd(betas_1[,2])))

c3 <- seq(min(betas_1[,3]), max(betas_1[,3]), length=R)
densidad_normal3 <- data.frame(c3=c3, f3=dnorm(c3, mean(betas_1[,3]), sd(betas_1[,3])))


c4 <- seq(min(betas_1[,4]), max(betas_1[,4]), length=R)
densidad_normal4 <- data.frame(c4=c4, f4=dnorm(c4, mean(betas_1[,4]), sd(betas_1[,4])))


#n=50
graph_beta11 <- data.frame(B11=betas_1[,1]) %>% ggplot(aes(betas_1[,1], ..density..))+
  geom_histogram(color = "black", bins = 30)+
  geom_line(data = densidad_normal1, mapping = aes(c1,f1), color = "red")+
  ggtitle("n=50")+xlab(expression(hat(beta)[1]))+
  theme_economist()

graph_beta11

#n=100
graph_beta12 <- data.frame(B12=betas_1[,2]) %>% ggplot(aes(betas_1[,2], ..density..))+
  geom_histogram(color = "black", bins = 30)+
  geom_line(data = densidad_normal2, mapping = aes(c2,f2), color = "red")+
  ggtitle("n=100")+xlab(expression(hat(beta)[1]))+
  theme_economist()

graph_beta12

#n=500
graph_beta13 <- data.frame(B11=betas_1[,3]) %>% ggplot(aes(betas_1[,3], ..density..))+
  geom_histogram(color = "black", bins = 30)+
  geom_line(data = densidad_normal3, mapping = aes(c3,f3), color = "red")+
  ggtitle("n=500")+xlab(expression(hat(beta)[1]))+
  theme_economist()

graph_beta13

#n=1000
graph_beta14 <- data.frame(B14=betas_1[,4]) %>% ggplot(aes(betas_1[,4], ..density..))+
  geom_histogram(color = "black", bins = 30)+
  geom_line(data = densidad_normal4, mapping = aes(c4,f4), color = "red")+
  ggtitle("n=1000")+xlab(expression(hat(beta)[1]))+
  theme_economist()

graph_beta14

grid.arrange(graph_beta11,graph_beta12,graph_beta13,graph_beta14)

#Parte (c)

#(c1) #con x2 proveniente de la distribucion uniforme

set.seed(1234567)
R=10000
n=c(50,100,500,1000) 
betas_u0=matrix(NA,nrow = R, ncol=4)
betas_u1=matrix(NA,nrow = R, ncol=4) 
beta0=2
beta1=2.5
beta2=1.0

for (j in 1:length(n)) {
  
  x_1=rnorm(n[j],20,1) 
  for (i in 1:R) {
    x_2=runif(n[j],0,1)
    u=rnorm(n[j],0,1)
    v=beta2*x_2+u
    y=beta0+beta1*x_1+v
    modelo=lm(y~x_1) 
    betas_u0[i,j]=modelo$coef[1]
    betas_u1[i,j]=modelo$coef[2]
  }
}

#Para el caso de B0

#n=50
E1u_beta0 = mean(betas_u0[,1])
V1u_beta0 = var(betas_u0[,1])
sesgo_u01=abs(mean(betas_u0[,1])-beta0)
#definimos el sesgo como la diferencia en valor absoluto de b0 y la media del experimento
sesgo_u01

#n=100
E2u_beta0 = mean(betas_u0[,2])
V2u_beta0 = var(betas_u0[,2])
sesgo_u02=abs(mean(betas_u0[,2])-beta0)
sesgo_u02

#n=500
E3u_beta0 = mean(betas_u0[,3])
V3u_beta0 = var(betas_u0[,3])
sesgo_u03=abs(mean(betas_u0[,3])-beta0)
sesgo_u03

#n=1000
E4u_beta0 = mean(betas_u0[,4])
V4u_beta0 = var(betas_u0[,4])
sesgo_u04=abs(mean(betas_u0[,4])-beta0)
sesgo_u04

#Para el caso de b1

#n=50
E1u_beta1= mean(betas_u1[,1])
V1u_beta1= var(betas_u1[,1])
sesgo_u11=abs(mean(betas_u1[,1])-beta1)
#definimos el sesgo como la diferencia en valor absoluto del valor b1 y la media del experimento
sesgo_u11

#n=100
E2u_beta1 = mean(betas_u1[,2])
V2u_beta1 = var(betas_u1[,2])
sesgo_u12=abs(mean(betas_u1[,2])-beta1)
sesgo_u12

#n=500
E3u_beta1 = mean(betas_u1[,3])
V3u_beta1 = var(betas_u1[,3])
sesgo_u13=abs(mean(betas_u1[,3])-beta1)
sesgo_u13

#n=1000
E4u_beta1 = mean(betas_u1[,4])
V4u_beta1 = var(betas_u1[,4])
sesgo_u14=abs(mean(betas_u1[,4])-beta1)
sesgo_u14


#RESPUESTA:
#En la variable a almacenamos un valor logico: TRUE si el sesgo es menor con n=1000 y FALSE en caso contrario
#"c1" arroja TRUE, por lo tanto el sesgo es menor con n=1000, ergo va desapareciendo a medida que aumenta n

tab2 <- data.frame("Parámetro"= c("Beta 0", "Beta 0", "Beta 0", "Beta 0",
                               "Beta 1", "Beta 1", "Beta 1", "Beta 1"),
                "n" = c(n, n) , 
                "Valor pob. beta" = c(beta0, beta0, beta0, beta0,
                                      beta1, beta1, beta1, beta1),
                "E()" = c(E1u_beta0, E2u_beta0, E3u_beta0, E4u_beta0,
                          E1u_beta1, E2u_beta1, E3u_beta1, E4u_beta1),
                "Var()" = c(V1u_beta0, V2u_beta0, V3u_beta0, V4u_beta0,
                            V1u_beta1, V2u_beta1, V3u_beta1, V4u_beta1),
                "Sesgo" = c(sesgo_u01, sesgo_u02, sesgo_u03, sesgo_u04,
                            sesgo_u11, sesgo_u12, sesgo_u13, sesgo_u14)
                )
print(tab2)




#(c2)

cu1 <- seq(min(betas_u1[,1]), max(betas_u1[,1]), length=R)
densidad_normalu1 <- data.frame(cu1=cu1, fu1=dnorm(cu1, mean(betas_u1[,1]), sd(betas_u1[,1])))

cu2 <- seq(min(betas_u1[,2]), max(betas_u1[,2]), length=R)
densidad_normalu2 <- data.frame(cu2=cu2, fu2=dnorm(cu2, mean(betas_u1[,2]), sd(betas_u1[,2])))

cu3 <- seq(min(betas_u1[,3]), max(betas_u1[,3]), length=R)
densidad_normalu3 <- data.frame(cu3=cu3, fu3=dnorm(cu3, mean(betas_u1[,3]), sd(betas_u1[,3])))

cu4 <- seq(min(betas_u1[,4]), max(betas_u1[,4]), length=R)
densidad_normalu4 <- data.frame(cu4=cu4, fu4=dnorm(cu4, mean(betas_u1[,4]), sd(betas_u1[,4])))


#n=50
graph_betau1 <- data.frame(Bu1=betas_u1[,1]) %>% ggplot(aes(betas_u1[,1], ..density..))+
  geom_histogram(color = "black", bins = 30)+
  geom_line(data = densidad_normalu1, mapping = aes(cu1,fu1), color = "red")+
  ggtitle("n=50")+xlab(expression(hat(beta)[1]))+
  theme_economist()

graph_betau1

#n=100
graph_betau2 <- data.frame(Bu2=betas_u1[,2]) %>% ggplot(aes(betas_u1[,2], ..density..))+
  geom_histogram(color = "black", bins = 30)+
  geom_line(data = densidad_normalu2, mapping = aes(cu2,fu2), color = "red")+
  ggtitle("n=100")+xlab(expression(hat(beta)[1]))+
  theme_economist()

graph_betau2

#n=500
graph_betau3 <- data.frame(Bu3=betas_u1[,3]) %>% ggplot(aes(betas_u1[,3], ..density..))+
  geom_histogram(color = "black", bins = 30)+
  geom_line(data = densidad_normalu3, mapping = aes(cu3,fu3), color = "red")+
  ggtitle("n=500")+xlab(expression(hat(beta)[1]))+
  theme_economist()

graph_betau3

#n=1000
graph_betau4 <- data.frame(Bu4=betas_u1[,4]) %>% ggplot(aes(betas_u1[,4], ..density..))+
  geom_histogram(color = "black", bins = 30)+
  geom_line(data = densidad_normalu4, mapping = aes(cu4,fu4), color = "red")+
  ggtitle("n=1000")+xlab(expression(hat(beta)[1]))+
  theme_economist()

graph_betau4

grid.arrange(graph_betau1,graph_betau2,graph_betau3,graph_betau4)

#RESPUESTA: Vemos que ambas distribuciones se asemejan bastante a una distribución normal
#(en línea roja la normal). Sin embargo, con n=1000 los límites (de la cota inferior y
#superior) de la distribución son menores que con n=100.



