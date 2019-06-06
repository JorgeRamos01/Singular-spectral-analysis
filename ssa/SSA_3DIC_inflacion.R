# Inflacion INPC RSSA Forecasting
#SSA Univariado
rm(list=ls())

# cargar librerias
path="/home/adrianrdzv/Escritorio/CIMAT/Series de Tiempo/proyecto_final/book_SSA/"
source(paste0(path,"functions_SSA.R"))


datos_inflacion<-read.csv(paste0(path,"datos2.csv"))
datos_inflacion_ts<-ts(frequency = 12,start = c(2005,01),end = c(2018,09),datos_inflacion[,"INPC"])

par(mfrow=c(1,1))
#plot(datos_inflacion$INPC,datos_inflacion$Fecha,main="Inflacion INPC",type="l")


datos_inflacion_ts[length(datos_inflacion_ts)]=100.917

ts.plot(datos_inflacion_ts,main="Inflacion INPC",type="l")
Y<-datos_inflacion_ts

## Obtener los mejores parametros usando valdiacion cruzada
Opt.choices(Y=Y,L.vec=2:24,mse.size=10,h=12)

## Realizar foreccasting de serie
RSSA.Forecasting(L=20,1:9,h=1, Y)
(abs(101.4601-96.69827)/96.69827)*100


## Realizar analisis de sensitividad
par(mfrow=c(1,1))
Sensitivity(Y=Y,L=17:20,mse.size=10,h=2,max.r=9,theta=80,phi=15,title="Sensitividad INPC (1)")
Sensitivity(Y=Y,L=17:20,mse.size=10,h=2,max.r=9,theta=120,phi=25,title="Sensitividad INPC (2)")
#?persp


intervalo<-Pred.int(20,1:10,6,Y,1000,.05)
prediccion<-RSSA.Forecasting(L=20,1:9,h=6, Y)
## Realizar plot de intervalor para un horizonte de 6
# ts.plot(cbind(intervalo$L.pi,prediccion,intervalo$U.pi),col=c(2,1,3),main="Intervalo bootstrap")



#dev.new()
par(mfrow=c(1,2))

plot(c(Y,prediccion),type="l",ylab = "INPC")
lines(c(rep(NA,length(c(Y))), prediccion),type="l",col=1)
lines( c(rep(NA,length(c(Y))), intervalo$U.pi),type="l",col=2)
lines( c(rep(NA,length(c(Y))), intervalo$L.pi),type="l",col=3)

### Grafico Zoom (obs numero de observaciones finales en zoom)
obs=20
plot(c(Y[(length(Y)-obs):length(Y)],prediccion),type="l",ylab = "INPC")
lines(c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), prediccion),type="l",col=1)
lines( c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo$U.pi),type="l",col=2)
lines( c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo$L.pi),type="l",col=3)
mtext("Intervalo bootstrap y zoom INPC", side = 3, line = -3, outer = TRUE,cex = 2)
