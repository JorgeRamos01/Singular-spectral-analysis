#SSA Univariado RSSA vs VSSA
rm(list=ls())

path="/home/adrianrdzv/Escritorio/CIMAT/Series de Tiempo/proyecto_final/book_SSA/"
source(paste0(path,"functions_SSA.R"))

datos_bmv<-read.csv(paste0(path,"Bolsa Mexicana de Valores.csv"))
datos_bmv
colnames(datos_bmv)[2]="indiceBMV"
#plot(datos_bmv,type="l")
datos_bmv_ts<-ts(frequency = 12,start = c(1990,04),end = c(2018,08),datos_bmv[,"indiceBMV"])

#datos_bmv_ts[length(datos_bmv_ts)]=100.917

Y<-datos_bmv_ts
ts.plot(Y,ylab="indice",main="BMV")

## Dado que VSSA se supone como mas robusto a outliers y cambios estructurales se hara
## Una comparativa

## Obtener los mejores parametros usando valdiacion cruzada
Opt.choices(Y=Y,L.vec=2:24,mse.size=10,h=6,type = "R")

Opt.choices(Y=Y,L.vec=2:24,mse.size=10,h=6,type = "V")

## Realizar foreccasting de serie con RSSA
RSSA.Forecasting(L=14,1:4,h=6, Y)


VSSA.Forecasting(L=24,1:6,h=6, Y)

## La funcion de perdida es menor en VSSA como esperabamos dado la robustez

## Realizar analisis de sensitividad

##### Analisis de Senstividad para RSSA #####
par(mfrow=c(1,1))
Sensitivity(Y=Y,L=11:16,mse.size=10,h=6,max.r=5,theta=80,phi=15,type="R",title="Sensitividad BMV (1) RSSA")
Sensitivity(Y=Y,L=11:16,mse.size=10,h=6,max.r=5,theta=120,phi=25,type="R",title="Sensitividad BMV (2) RSSA")

#### Analisis de Senstividad para RSSA ####
par(mfrow=c(1,1))
Sensitivity(Y=Y,L=20:26,mse.size=10,h=2,max.r=7,theta=80,phi=15,type="V",title="Sensitividad BMV (1) VSSA")
Sensitivity(Y=Y,L=20:26,mse.size=10,h=2,max.r=7,theta=120,phi=25,type="V",title="Sensitividad BMV (2) VSSA")



#### Intervalos de confianza Bootstrap para RSSA ####

intervalo_RSSA<-Pred.int(24,1:6,6,Y,1000,.05)
prediccion_RSSA<-RSSA.Forecasting(L=24,1:6,h=6, Y)

#dev.new()
par(mfrow=c(1,2))

plot(c(Y,prediccion_RSSA),type="l",ylab = "índice bmv")
lines(c(rep(NA,length(c(Y))), prediccion_RSSA),type="l",col=1)
lines( c(rep(NA,length(c(Y))), intervalo_RSSA$U.pi),type="l",col=2)
lines( c(rep(NA,length(c(Y))), intervalo_RSSA$L.pi),type="l",col=3)

### Grafico Zoom (obs numero de observaciones finales en zoom)
obs=20
plot(c(Y[(length(Y)-obs):length(Y)],prediccion_RSSA),type="l",ylab = "índice bmv")
lines(c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), prediccion_RSSA),type="l",col=1)
lines( c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo_RSSA$U.pi),type="l",col=2)
lines( c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo_RSSA$L.pi),type="l",col=3)
mtext("Intervalo bootstrap y zoom BMV - RSSA", side = 3, line = -3, outer = TRUE,cex = 2)

#### Intervalos de confianza Bootstrap para VSSA ####

intervalo_VSSA<-Pred.int(14,1:4,6,Y,1000,.05,type = "V")
prediccion_VSSA<-VSSA.Forecasting(L=14,1:4,h=6, Y)

#dev.new()
par(mfrow=c(1,2))

plot(c(Y,prediccion_VSSA),type="l",ylab = "índice bmv")
lines(c(rep(NA,length(c(Y))), prediccion_VSSA),type="l",col=1)
lines( c(rep(NA,length(c(Y))), intervalo_VSSA$U.pi),type="l",col=2)
lines( c(rep(NA,length(c(Y))), intervalo_VSSA$L.pi),type="l",col=3)

### Grafico Zoom (obs numero de observaciones finales en zoom)
obs=20
plot(c(Y[(length(Y)-obs):length(Y)],prediccion_VSSA),type="l",ylab = "índice bmv")
lines(c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), prediccion_VSSA),type="l",col=1)
lines( c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo_VSSA$U.pi),type="l",col=2)
lines( c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo_VSSA$L.pi),type="l",col=3)
mtext("Intervalo bootstrap y zoom BMV - VSSA", side = 3, line = -3, outer = TRUE,cex = 2)
