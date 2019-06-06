#Change point detection SSA

rm(list=ls())
# UniHankel<-function(Y,L){
#   k<-length(Y)-L+1
#   outer((1:L),(1:k),function(x,y) Y[(x+y-1)])
# }


setwd("/home/adrianrdzv/Escritorio/CIMAT/Series de Tiempo/proyecto_final/book_SSA")
source("functions_SSA.R")
library(tseries)


par(mfrow=c(1,1))
sim<-c(1:200,.5*c(399:300),2*c(75:100))
plot(ts(sim),xlab="Obs. No", ylab="Artificial data")
length(sim)

D<-Possible.D(L=30,r=14,Y=sim,B=70,f=40,T=50)
plot(ts(D,start=71),xlab="Obs. No", ylab=expression(D[t]))
senial_trick<-rnorm(length(sim),mean = 3,sd = 10) +sim

plot(senial_trick,type = "l")
D<-Possible.D(L=30,r=14,Y=senial_trick,B=70,f=40,T=50)
plot(ts(D,start=71),xlab="Obs. No", ylab=expression(D[t]))

signal_change<-read.csv("signal.csv",header = F)

plot(1:128,signal_change$V1,type="l")
plot(1:127,diff(signal_change$V1),type="l")

D<-Possible.D(L=15,r=8,Y=diff(signal_change$V1),B=35,f=15,T=20)
plot(ts(D,start=36),xlab="Obs. No", ylab=expression(D[t]))


# #### PIB change poiit ######
# 
# pib_ts<-read.csv("PIB.csv")
# bmv<-read.csv("Bolsa Mexicana de Valores.csv")
# bmv_ts<-ts(bmv$Indice.de.precios.y.cotizaciones.de.la.Bolsa.Mexicana.de.Valores,start = c(1990,04),frequency = 12)
# 
# #ts.plot(pib_ts$Valores.absolutos)
# ts.plot(bmv_ts)
# D<-Possible.D(L=45,r=8,Y=as.vector(bmv_ts),B=105,f=45,T=60)
# plot(ts(D,start=46),xlab="Obs. No", ylab=expression(D[t]))
# plot(ts(D,start=c(1994,01),frequency = 12),xlab="Obs. No", ylab=expression(D[t]))
# length(ts(D,start=46))
# length(bmv_ts)
# 
# ts(D,start=c(1994,1),frequency = 12)
# 
# ##############################################################3
# 
# # DiagAver<-function(X){
# #   L<-nrow(X);k<-ncol(X);N<-k+L-1
# #   D<-NULL
# #   for(j in 1:N){
# #     s1<-max(1,(j-N+L))
# #     s2<-min(L,j)
# #     place<-(s1:s2)+L*(((j+1-s1):(j+1-s2))-1)
# #     D[j]<-mean(X[place])
# #   }
# #   D
# # }
# # 
# # Group<-function(Y,L,groups){
# #   I<-groups;p<-length(I)
# #   SVD<-SVD(Y,L)
# #   LambdaI<-matrix(diag(SVD$d)[I,I],p,p)
# #   SVD$u[,I]%*%LambdaI%*%t(SVD$v[,I])
# # }
# # 
# # SVD<-function(Y,L){
# #   X<-UniHankel(Y,L)
# #   svd(X)
# # }
# # 
# # SSA.Rec<-function(Y,L,groups){
# #   N<-length(Y)
# #   I<-groups;p<-length(I)
# #   XI<-Group(Y,L,groups)
# #   Approx<-DiagAver(XI)
# #   Resid<-Y-Approx
# #   list(Approximation=Approx,Residual=Resid)
# # }
# # 
# # 
# # Approx<-SSA.Rec(UKYield,72,c(1))$Approximation
# # Data<-cbind(UKYield,Approx)
# # Series<-ts(Data,frequency=12,start=c(1985,1))
# # plot.ts(Series[,1],type="l",xlab= "Time", ylab= "UK Yield")
# # legend("bottomleft",horiz=FALSE,lwd=c(1,3),c("Initial","EF 1"))
# # lines(Series[,2],lwd=2)
# # 
# # 
# # 
# # 
# # 
# # 
