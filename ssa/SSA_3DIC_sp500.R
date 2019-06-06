#SSA Univariado RSSA Forecasting
rm(list=ls())

### cargar funciones SSA RSSA VSSA Intervalos de confianza etc
path="/home/adrianrdzv/Escritorio/CIMAT/Series de Tiempo/proyecto_final/book_SSA/"
source(paste0(path,"functions_SSA.R"))

#### Prediccion SP 500 indicador global ###

library(quantmod)
#obtner de yahoo financial (usando la libreria quantmod el historico desde el 2008)
indicador_global= getSymbols("^GSPC",auto.assign = FALSE, from = "2008-01-07",to="2018-11-20",periodicity="weekly")

Y<-indicador_global$GSPC.Open
length(Y)
Y<-as.numeric(Y)
Y<-Y[-length(Y)]

ts.plot(Y,main="^GSPC open",ylab="GSPC.open")
plot(indicador_global$GSPC.Open,main="^GSPC open",ylab="GSPC.open")

## Obtener los mejores parametros usando valdiacion cruzada
Opt.choices(Y=Y,L.vec=2:24,mse.size=10,h=3)


## Realizar foreccasting de serie
RSSA.Forecasting(L=14,1:5,h=3, Y)

#(abs(101.4601-96.69827)/96.69827)*100

#### Analisis de senistividad ####
# all.rmse<-function(Y,L,mse.size,h,max.r){
#   rmse<-array(0,dim=max.r)
#   for(r in 1:max.r)
#     rmse[r]<-RMSE(Y,L,r,mse.size,h)
#   rmse
# }
# Sensitivity<-function(Y,L.vec,h,mse.size,max.r,theta,phi){
#   wid<-length(L.vec)
#   r.val<-1:max.r
#   rmse<-array(0,dim=c(wid,max.r))
#   for(i in 1:wid){
#     rmse[i,]<-all.rmse(Y,L.vec[i],mse.size,h,max.r)
#   }
#   persp(y=r.val,x=L.vec,z=rmse,theta=theta,phi=phi,shade=.10)
# }

## Realizar analisis de sensitividad
par(mfrow=c(1,1))
Sensitivity(Y=Y,L=13:16,mse.size=10,h=3,max.r=5,theta=80,phi=15,title="Sensitividad ^GSPC open (1)")
Sensitivity(Y=Y,L=13:16,mse.size=10,h=3,max.r=5,theta=120,phi=15,title="Sensitividad ^GSPC open (2)")

#### Bootstrap intervalo de prediccion #####

# Pred.int<-function(L,groups,h,Y,p,alpha){
#   N<-length(Y)
#   Recons<-SSA.Rec(Y,L,groups)
#   S.hat<-Recons$Approximation
#   E.hat<-Recons$Residual
#   L.pi<-U.pi<-0
#   new<-array(dim=c(h,p))
#   for(i in 1:p){
#     Y.hat<-S.hat+sample(E.hat,N,replace=TRUE)
#     new[,i]<-RSSA.Forecasting(L,groups,h,Y.hat)
#   }
#   L.pi<-apply(new,1,quantile,probs=alpha)
#   U.pi<-apply(new,1,quantile,probs=1-alpha)
#   data.frame(L.pi,U.pi)
# }

intervalo<-Pred.int(14,1:5,6,Y,1000,.05)
prediccion<-RSSA.Forecasting(L=14,1:5,h=6, Y)
## Realizar plot de intervalor para un horizonte de 6
par(mfrow=c(1,1))
ts.plot(cbind(intervalo$L.pi,prediccion,intervalo$U.pi),col=c(2,1,3),main="Intervalo bootstrap ^GSPC open")


#dev.new()
par(mfrow=c(1,2))

plot(c(Y,prediccion),type="l",ylab = "GSPC.Open")
lines(c(rep(NA,length(c(Y))), prediccion),type="l",col=1)
lines( c(rep(NA,length(c(Y))), intervalo$U.pi),type="l",col=2)
lines( c(rep(NA,length(c(Y))), intervalo$L.pi),type="l",col=3)

obs=50
plot(c(Y[(length(Y)-obs):length(Y)],prediccion),type="l",ylab = "GSPC.Open")
lines(c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), prediccion),type="l",col=1)
lines( c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo$U.pi),type="l",col=2)
lines( c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo$L.pi),type="l",col=3)
mtext("Intervalo bootstrap y zoom ^GSPC open", side = 3, line = -3, outer = TRUE,cex = 2)
# length(c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo$L.pi))
# length(c(rep(NA,length(c(Y[(length(Y)-obs):length(Y)]))), intervalo$L.pi))
# length(c(Y[(length(Y)-obs):length(Y)],prediccion))

