## pagina 71

#paquete con el dataset de motors
#install.packages("Mcomp")

HMSSA.Hankel<-function(Y,L){
  X<-NULL
  if(is.list(Y)){
    M<-length(Y)
    for(i in 1:M){
      ki<-length(Y[[i]])-L+1
      Xi<-outer((1:L),(1:ki),function(x,y) Y[[i]][(x+y-1)])
      X<-cbind(X,Xi)}
  }
  else{
    Y<-as.matrix(Y)
    M<-ncol(Y)
    for(i in 1:M){
      ki<-length(Y[,i])-L+1
      Xi<-outer((1:L),(1:ki),function(x,y) Y[(x+y-1),i])
      X<-cbind(X,Xi)}
  }
  X
}

H.SVD<-function(Y,L){
  X<-HMSSA.Hankel(Y,L)
  svd(X)
}

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

HDiagAver<-function(X,series.lengths){
  M<-length(series.lengths)
  L<-nrow(X);k<-series.lengths-L+1
  cols<-cumsum(c(0,k))
  D<-NULL
  for(j in 1:(M)){
    D[[j]]<-DiagAver(X[,((cols[j]+1):cols[(j+1)])])}
  D
}

HMSSA.Rec<-function(Y,L,groups){
  N<-NULL
  if(is.list(Y)){
    M<-length(Y)
    for(i in 1:M)
      N[i]<-length(Y[[i]])}
  else{
    Y<-as.matrix(Y)
    M<-ncol(Y)
    N<-rep(nrow(Y),M)}
  X<-HMSSA.Hankel(Y,L)
  XI<-H.Group(X,L,groups)
  Approx<-HDiagAver(XI,N)
  if(is.list(Y)){
    Resid<-vector("list",M)
    for(i in 1:M)
      Resid[[i]]<-Y[[i]]-Approx[[i]]}
  else{
    Approx<-matrix(unlist(Approx),ncol=M)
    Resid<-Y-Approx}
  list(Approximation=Approx,Residual=Resid)
}

H.Group<-function(Y,L,groups){
  I<-groups;p<-length(I)
  SVD<-H.SVD(Y,L)
  LambdaI<-matrix(diag(SVD$d)[I,I],p,p)
  SVD$u[,I]%*%LambdaI%*%t(SVD$v[,I])
}

HMSSA.R<-function(L,Group,h,Y){
  r<-length(Group)
  Dec<-H.SVD(Y,L)
  sigma<-Dec$d
  d<-length(sigma[sigma>0])
  U<-matrix(Dec$u,L,d)
  V<-matrix(Dec$v,ncol=d)
  hx<-HMSSA.Rec(Y,L,Group)$Approximation
  v<-array(U[L,Group],dim=c(1,r))
  Ud<-array(U[-L,Group],dim=c((L-1),r))
  R<-1/(1-sum(v^2))*v%*%t(Ud)
  if(is.list(Y)){
    M<-length(Y)
    forecasts<-vector("list",M)
    for(i in 1:M){}
    matrix(unlist(forecasts),ncol=M)
  }
  else{
    N<-nrow(Y);M<-ncol(Y)
    for(j in 1:h){
      Z<-array(hx[(N-L+1+j):(N+j-1),],dim=c((L-1),M))
      hx<-rbind(hx,R%*%Z)}
    hx[(N+1):(N+h),]
  }
}


require(Mcomp)
FR<-M1$YAI1$x
GR<-M1$YAI12$x
UK<-M1$YAI17$x
motor<-cbind(UK,FR,GR)
motor

par(bg="darkgray")## Background gray
ts.plot(motor,col=c(1,2,3))


round(HMSSA.R(9,c(1:3),6,motor),1)
motor


FR.new<-M1$YAI1$xx
GR.new<-M1$YAI12$xx
UK.new<-M1$YAI17$xx
motor.new<-cbind(UK.new,FR.new,GR.new)
motor.new
#Diferencia prediccion y real
motor.new - round(HMSSA.R(9,c(1:3),6,motor),1)


#### Ejemplo datos reales pagina 82 US Air Dataset
#https://www.transtats.bts.gov/Data_Elements.aspx?Data=3



Y<-1:15
L<-7 #
K<-length(Y)-L+1
outer((1:L),(1:K))
X<-outer((1:L),(1:K),function(x,y) Y[(x+y-1)])

expand.grid(c("a","b","c"),c("d","e"))
expand.grid((1:L),(1:K))
#apply(expand.grid((1:L),(1:K)),1,function(x) paste(x,collapse=""))


L=4.
Y1=round(runif(7,0,1),1)
Y1<-c(0.8,0.5,0.9,0.4,0.7,0.1,0.6)
L<-4
K<-length(Y1)-L+1
X<-outer((1:L),(1:K),function(x,y) Y1[(x+y-1)])
X

Y<-1:15
L<-7 #
K<-length(Y)-L+1
X<-outer((1:L),(1:K),function(x,y) Y[(x+y-1)])

SVD<-svd(X)
lambda<-SVD$d
Lambda<-diag(lambda)
U<-SVD$u
V<-SVD$v
X1<-lambda[1]*U[,1]%*%t(V[,1])
round(X1,2)

## Reconstruccion
I1<-c(2,3)
p<-length(I1)
XI1<-U[,I1]%*%matrix(Lambda[I1,I1],p,p)%*%t(V[,I1])
XI1


## Example 1.5

I1<-c(1)
p<-length(I1)
XI<-U[,I1]%*%matrix(Lambda[I1,I1],p,p)%*%t(V[,I1])
XI



D<-NULL
N<-length(Y)
for(t in 1:N){
  s1<-max(1,(t-N+L))
  s2<-min(L,t)
  place<-(s1:s2)+L*(((t+1-s1):(t+1-s2))-1)
  D[t]<-mean(XI[place])}

#
mean(c(14.5367,14.6088))


#### PRUEBA MULTIVARIADA ####


datos_inflacion<-read.csv("Escritorio/CIMAT/Series de Tiempo/proyecto_final/book_SSA/datos2.csv")
datos_inflacion_ts<-ts(frequency = 12,start = c(2005,01),datos_inflacion[,-1])


round(HMSSA.R(9,c(1:3),6,motor),1)
motor

round(HMSSA.R(20,c(1:5),2,datos_inflacion_ts),2)

(abs(101.8-96.094)/96.094)*100

HMSSA.R()
dim(datos_inflacion)

FR.new<-M1$YAI1$xx
GR.new<-M1$YAI12$xx
UK.new<-M1$YAI17$xx
motor.new<-cbind(UK.new,FR.new,GR.new)
motor.new
#Diferencia prediccion y real
motor.new - round(HMSSA.R(9,c(1:3),6,motor),1)



RMSE<-function(Y,L,r,mse.size,h){
  N<-nrow(Y);M<-ncol(Y)
  forecasts<-NULL
  train.size<-N-mse.size-h+1
  test<-Y[(train.size+h):(train.size+mse.size+h-1),]
  for(i in 1:mse.size){
    train<-Y[1:(train.size+i-1),]
    forec<-matrix(HMSSA.R(L,1:r,h,train),ncol=M)[h,]
    forecasts<-rbind(forecasts,forec)
  }
  sqrt(sum(colMeans(as.matrix((test-forecasts)^2))))
}


Opt.r<-function(Y,L,mse.size,h){
  opt.rmse<-array(0,dim=(L-1))
  for(r in 1:(L-1))
    opt.rmse[r]<-RMSE(Y,L,r,mse.size,h)
  which.min(opt.rmse)
}



Opt.choices<-function(Y,L.vec,mse.size,h){
  wid<-length(L.vec)
  rmse<-array(0,dim=wid)
  for(i in 1:wid){
    r0<-Opt.r(Y,L.vec[i],mse.size,h)
    rmse[i]<-RMSE(Y,L.vec[i],r0,mse.size,h)
  }
  window<-which.min(rmse)
  r.opt<-Opt.r(Y,L.vec[window],mse.size,h)
  L.opt<-L.vec[window]
  list(optimal.r=r.opt,optimal.L=L.opt,root.mean.square.error=rmse)
}

Opt.choices(datos_inflacion_ts,c(5:15),2,1)

Xround(HMSSA.R(20,c(1:5),2,datos_inflacion_ts),2)

round(HMSSA.R(23,c(1:19),2,datos_inflacion_ts),2)
(abs(101.53-96.094)/96.094)*100

round(HMSSA.R(12,c(1:3),2,datos_inflacion_ts),2)
(abs(103.95-96.094)/96.094)*100

#### Prueba univariada ####