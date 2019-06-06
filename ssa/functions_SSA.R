#SSA Univariado RSSA VSSSA


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


VSSA.Forecasting<-function(L,groups,h,Y){
  N<-length(Y)
  L<-min(L,(N-L+1))
  k<-N-L+1
  X<-UniHankel(Y,L)
  Lambda<-diag(svd(X)$d)
  U<-matrix(svd(X)$u,L,L)
  V<-matrix(svd(X)$v,k,L)
  yhat<-array(U[1:L,groups],dim=c(L,length(groups)))%*%Lambda[groups,groups]%*%array(t(V[1:k,groups]),dim=c(length(groups),k))
  pi<-array(U[L,groups],dim=length(groups))
  V2<-sum(pi^2)
  Udelta<-array(U[1:(L-1),groups],dim=c((L-1),
                                        length(groups)))
  A<-pi%*%t(Udelta)/(1-V2)
  pai<-Udelta%*%t(Udelta)+(1-V2)*t(A)%*%A
  Pv<-rbind(pai,A)
  for(l in (k+1):(N+h))
    yhat<-cbind(yhat,(Pv%*%yhat[2:L,(l-1)]))
  DiagAver(yhat)[(N+1):(N+h)]
}


## Funcion RMSE funcion para validar (loss  function)
RMSE<-function(Y,L,r,mse.size,h,type="R"){
  ## Tipo R para recurrent forecasting , Tipo - V para V Forecasting
  N<-length(Y)
  forecasts<-NULL
  train.size<-N-mse.size-h+1
  test<-Y[(train.size+h):(train.size+mse.size+h-1)]
  for(i in 1:mse.size){
    train<-Y[1:(train.size+i-1)]
    if(type=="R"){
      forecasts<-c(forecasts,RSSA.Forecasting(L,1:r,h,train)[h])
    }
    else{
      forecasts<-c(forecasts,VSSA.Forecasting(L,1:r,h,train)[h])
    }
    
  }
  sqrt(mean((test-forecasts)^2))
}


## Obtener el r que da  minimo de perdida 
Opt.r<-function(Y,L,mse.size,h,type="R"){
  ## Tipo R para recurrent forecasting , Tipo - V para V Forecasting
  opt.rmse<-array(0,dim=(L-1))
  for(r in 1:(L-1))
    opt.rmse[r]<-RMSE(Y,L,r,mse.size,h,type=type)## se manda el tipo de SSA
  which.min(opt.rmse)
}

# Obtener el tamaño de ventana optimo
Opt.choices<-function(Y,L.vec,h,mse.size,type="R"){
  wid<-length(L.vec)
  rmse<-array(0,dim=wid)
  for(i in 1:wid){
    r0<-Opt.r(Y,L.vec[i],mse.size,h)
    rmse[i]<-RMSE(Y,L.vec[i],r0,mse.size,h,type=type)## Se manda el tipo de SSA
  }
  window<-which.min(rmse)
  r.opt<-Opt.r(Y,L.vec[window],mse.size,h)
  L.opt<-L.vec[window]
  list(optimal.r=r.opt,optimal.L=L.opt,root.mean.square.error=min(rmse))
}

##### SENSITIVIDAD #####

all.rmse<-function(Y,L,mse.size,h,max.r,type="R"){
  rmse<-array(0,dim=max.r)
  for(r in 1:max.r)
    rmse[r]<-RMSE(Y,L,r,mse.size,h,type=type)## Se manda el tipo de SSA
  rmse
}
Sensitivity<-function(Y,L.vec,h,mse.size,max.r,theta,phi,type="R",title=""){
  wid<-length(L.vec)
  r.val<-1:max.r
  rmse<-array(0,dim=c(wid,max.r))
  for(i in 1:wid){
    rmse[i,]<-all.rmse(Y,L.vec[i],mse.size,h,max.r,type=type)
  }
  #persp(y=r.val,x=L.vec,z=rmse,theta=theta,phi=phi,shade=.10,ticktype = "detailed")
  persp(y=r.val,x=L.vec,z=rmse,theta=theta,phi=phi,shade=.10,main=title)
}

#### Intervalos de confianza Bootstrap #####

Pred.int<-function(L,groups,h,Y,p,alpha,type="R"){
  N<-length(Y)
  Recons<-SSA.Rec(Y,L,groups)
  S.hat<-Recons$Approximation
  E.hat<-Recons$Residual
  L.pi<-U.pi<-0
  new<-array(dim=c(h,p))
  for(i in 1:p){
    Y.hat<-S.hat+sample(E.hat,N,replace=TRUE)
    if(type=="R"){
      new[,i]<-RSSA.Forecasting(L,groups,h,Y.hat)
    }
    else{
      new[,i]<-VSSA.Forecasting(L,groups,h,Y.hat)
    }
    
  }
  L.pi<-apply(new,1,quantile,probs=alpha)
  U.pi<-apply(new,1,quantile,probs=1-alpha)
  data.frame(L.pi,U.pi)
}

## Funciones para deteccion de punto de cambio estructural

D.Comp<-function(L,r,Y,B,f,T,t){
  N<-length(Y)
  X.Base<-UniHankel(Y[(t+1):(t+B)],L)
  X.Test<-UniHankel(Y[(t+f+1):(t+f+T)],L)
  K<-T-L+1
  D0<-D1<-0
  U<-matrix(svd(X.Base)$u,L,L)
  U.r<-U[,1:r]
  for(j in 1:K){
    D0<-D0+t(X.Test[,j])%*%X.Test[,j]
    D1<-D1+t(X.Test[,j])%*%U.r%*%t(U.r)%*%X.Test[,j]
  }
  1-D1/D0
}

Possible.D<-function(L,r,Y,B,f,T){
  D<-0
  N<-length(Y)
  for(t in 0:(N-T-f))
    D<-c(D,D.Comp(L,r,Y,B,f,T,t))
  D
}