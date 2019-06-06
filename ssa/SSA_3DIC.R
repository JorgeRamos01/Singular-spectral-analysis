#SSA Univariado
rm(list=ls())

DiagAver<-function(X){
  L<-nrow(X);k<-ncol(X);N<-k+L-1
  D<-NULL
  for(j in 1:N){
    s1<-max(1,(j-N+L))
    s2<-min(L,j)
    place<-(s1:s2)+L*(((j+1-s1):(j+1-s2))-1)
    D[j]<-mean(X[place])
  }
  D
}

Group<-function(Y,L,groups){
  I<-groups;p<-length(I)
  SVD<-SVD(Y,L)
  LambdaI<-matrix(diag(SVD$d)[I,I],p,p)
  SVD$u[,I]%*%LambdaI%*%t(SVD$v[,I])
}

SVD<-function(Y,L){
  X<-UniHankel(Y,L)
  svd(X)
}

SSA.Rec<-function(Y,L,groups){
  N<-length(Y)
  I<-groups;p<-length(I)
  XI<-Group(Y,L,groups)
  Approx<-DiagAver(XI)
  Resid<-Y-Approx
  list(Approximation=Approx,Residual=Resid)
}

UniHankel<-function(Y,L){
  k<-length(Y)-L+1
  outer((1:L),(1:k),function(x,y) Y[(x+y-1)])
}

RSSA.Forecasting<-function(L,groups,h,Y){
  N<-length(Y)
  L<-min(L,(N-L+1))
  X<-UniHankel(Y,L)
  U<-matrix(svd(X)$u,L,L)
  pi<-array(U[L,groups],dim=length(groups))
  V2<-sum(pi^2)
  m<-length(groups)
  Udelta<-array(U[1:(L-1),groups],dim=c((L-1),m))
  A<-pi%*%t(Udelta)/(1-V2)
  yhat<-array(0,dim=(N+h))
  yhat[1:N]<-SSA.Rec(Y,L,groups)$Approximation
  for(l in (N+1):(N+h))
    yhat[l]<-A%*%yhat[(l-L+1):(l-1)]
  yhat[(N+1):(N+h)]
}

## Funcion RMSE funcion para validar (loss  function)
RMSE<-function(Y,L,r,mse.size,h){
  N<-length(Y)
  forecasts<-NULL
  train.size<-N-mse.size-h+1
  test<-Y[(train.size+h):(train.size+mse.size+h-1)]
  for(i in 1:mse.size){
    train<-Y[1:(train.size+i-1)]
    forecasts<-c(forecasts,RSSA.Forecasting(L,1:r,h,train)[h])
  }
  sqrt(mean((test-forecasts)^2))
}

## Obtener el r que da  minimo de perdida 
Opt.r<-function(Y,L,mse.size,h){
  opt.rmse<-array(0,dim=(L-1))
  for(r in 1:(L-1))
    opt.rmse[r]<-RMSE(Y,L,r,mse.size,h)
  which.min(opt.rmse)
}

# Obtener el tamaño de ventana optimo
Opt.choices<-function(Y,L.vec,h,mse.size){
  wid<-length(L.vec)
  rmse<-array(0,dim=wid)
  for(i in 1:wid){
    r0<-Opt.r(Y,L.vec[i],mse.size,h)
    rmse[i]<-RMSE(Y,L.vec[i],r0,mse.size,h)
  }
  window<-which.min(rmse)
  r.opt<-Opt.r(Y,L.vec[window],mse.size,h)
  L.opt<-L.vec[window]
  list(optimal.r=r.opt,optimal.L=L.opt,root.mean.square.error=min(rmse))
}

datos_inflacion<-read.csv("datos2.csv")
datos_inflacion_ts<-ts(frequency = 12,start = c(2005,01),end = c(2018,09),datos_inflacion[,"INPC"])

datos_inflacion_ts[length(datos_inflacion_ts)]=100.917

Y<-datos_inflacion_ts

## Obtener los mejores parametros usando valdiacion cruzada
Opt.choices(Y=Y,L.vec=2:24,mse.size=10,h=12)

## Realizar foreccasting de serie
RSSA.Forecasting(L=20,1:9,h=1, Y)
(abs(101.4601-96.69827)/96.69827)*100

#### Analisis de senistividad ####
all.rmse<-function(Y,L,mse.size,h,max.r){
  rmse<-array(0,dim=max.r)
  for(r in 1:max.r)
    rmse[r]<-RMSE(Y,L,r,mse.size,h)
  rmse
}
Sensitivity<-function(Y,L.vec,h,mse.size,max.r,theta,phi){
  wid<-length(L.vec)
  r.val<-1:max.r
  rmse<-array(0,dim=c(wid,max.r))
  for(i in 1:wid){
    rmse[i,]<-all.rmse(Y,L.vec[i],mse.size,h,max.r)
  }
  persp(y=r.val,x=L.vec,z=rmse,theta=theta,phi=phi,shade=.10)
}

## Realizar analisis de sensitividad
Sensitivity(Y=Y,L=17:20,mse.size=10,h=2,max.r=9,theta=80,phi=15)


#### Bootstrap intervalo de prediccion #####

Pred.int<-function(L,groups,h,Y,p,alpha){
  N<-length(Y)
  Recons<-SSA.Rec(Y,L,groups)
  S.hat<-Recons$Approximation
  E.hat<-Recons$Residual
  L.pi<-U.pi<-0
  new<-array(dim=c(h,p))
  for(i in 1:p){
    Y.hat<-S.hat+sample(E.hat,N,replace=TRUE)
    new[,i]<-RSSA.Forecasting(L,groups,h,Y.hat)
  }
  L.pi<-apply(new,1,quantile,probs=alpha)
  U.pi<-apply(new,1,quantile,probs=1-alpha)
  data.frame(L.pi,U.pi)
}

intervalo<-Pred.int(20,1:10,6,Y,1000,.05)
prediccion<-RSSA.Forecasting(L=20,1:9,h=6, Y)
## Realizar plot de intervalor para un horizonte de 6
ts.plot(cbind(intervalo$L.pi,prediccion,intervalo$U.pi),col=c(2,1,3),main="Intervalo bootstrap")
