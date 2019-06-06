# PIB RSSA Forecasting
#SSA Univariado
rm(list=ls())

num_observaciones_predecir = 3

path="/home/adrianrdzv/Escritorio/CIMAT/Series de Tiempo/proyecto_final/book_SSA/"
source(paste0(path,"functions_SSA.R"))

datos_PIB<-read.csv(paste0(path,"PIB.csv"))
datos_PIB_ts<-ts(frequency = 4,start = c(1993,01),end = c(2018,02),datos_PIB[,"Valores.absolutos"])

length(datos_PIB_ts)
length(datos_PIB_ts[0:(length(datos_PIB_ts)-3)])


Y<-datos_PIB_ts[0:(length(datos_PIB_ts)-3)]
ts.plot(datos_PIB_ts,ylab="PIB",main="PIB")

## Obtener los mejores parametros usando valdiacion cruzada
Opt.choices(Y=Y,L.vec=2:24,mse.size=10,h=3)

## Realizar foreccasting de serie
prediccion <- RSSA.Forecasting(L=12,1:4,h=3, Y)
reales<-datos_PIB$Valores.absolutos[(length(datos_PIB$Valores.absolutos)-2):length(datos_PIB$Valores.absolutos)]

for (i in 1:num_observaciones_predecir){
  print(1- abs(prediccion[i]-reales[i])/reales[i])
}

## Realizar analisis de sensitividad
par(mfrow=c(1,1))
Sensitivity(Y=Y,L=10:14,mse.size=10,h=3,max.r=4,theta=80,phi=15,title="Sensitividad PIB (1)")
Sensitivity(Y=Y,L=10:14,mse.size=10,h=3,max.r=4,theta=120,phi=25,title="Sensitividad PIB (2)")


intervalo<-Pred.int(12,1:4,3,Y,1000,.05)
prediccion<-RSSA.Forecasting(L=12,1:4,h=3, Y)

## Realizar plot de intervalor para un horizonte de 6

#dev.new()
par(mfrow=c(1,2))

plot(c(Y,prediccion),type="l",ylab = "PIB")
lines(c(rep(NA,length(c(Y))), prediccion),type="l",col=1)
lines( c(rep(NA,length(c(Y))), intervalo$U.pi),type="l",col=2)
lines( c(rep(NA,length(c(Y))), intervalo$L.pi),type="l",col=3)

obs=20
plot(c(Y[(length(Y)-obs):length(Y)],prediccion),type="l",ylim = c(13500000,18500000),ylab = "PIB")
#lines(c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), prediccion),type="l",col=1)
lines( c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo$U.pi),type="l",col=2)
lines( c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo$L.pi),type="l",col=3)
mtext("Intervalo bootstrap y zoom PIB", side = 3, line = -3, outer = TRUE,cex = 2)

####-------------------------------------------#####


# intervalo<-Pred.int(11,1:6,90,Y,1000,.05)
# prediccion<-RSSA.Forecasting(L=11,1:6,h=90, Y)
## Realizar plot de intervalor para un horizonte de 6
# ts.plot(cbind(intervalo$L.pi,prediccion,intervalo$U.pi),col=c(2,1,3),main="Intervalo bootstrap")


# 
# dev.new()
# par(mfrow=c(1,2))
# 
# plot(c(Y,prediccion),type="l")
# lines(c(rep(NA,length(c(Y))), prediccion),type="l",col=1)
# lines( c(rep(NA,length(c(Y))), intervalo$U.pi),type="l",col=2)
# lines( c(rep(NA,length(c(Y))), intervalo$L.pi),type="l",col=3)
# 
# obs=60
# plot(c(Y[(length(Y)-obs):length(Y)],prediccion),type="l",ylim = c(13500000,18500000))
# #lines(c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), prediccion),type="l",col=1)
# lines( c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo$U.pi),type="l",col=2)
# lines( c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo$L.pi),type="l",col=3)
