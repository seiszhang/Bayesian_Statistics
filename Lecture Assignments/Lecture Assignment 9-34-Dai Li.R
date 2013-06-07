
###
rwish<-function(n,nu0,S0)
{
  sS0 <- chol(S0)
  S<-array( dim=c( dim(S0),n ) )
  for(i in 1:n)
  {
    Z <- matrix(rnorm(nu0 * dim(S0)[1]), nu0, dim(S0)[1]) %*% sS0
    S[,,i]<- t(Z)%*%Z
  }
  S[,,1:n]
}
###

library(MASS)
library(mvtnorm)
mu0<-c(0,0)
S0<-matrix( c(1,0.8,0.8,1),nrow=2,ncol=2)
y <- mvrnorm(n = 100, mu0, S0)

# MLE
Mmu = matrix(c(mean(y[,1]), mean(y[,2])),nrow=1,ncol=2)
Ms = cov(y)

#Gibbs Sampler
mu0<-c(0,0)
L0<-matrix( c(0.25,0.125,0.125,0.25),nrow=2,ncol=2)

nu0<-4
S0<-matrix( c(1,0.5,0.5,1),nrow=2,ncol=2)

n<-dim(y)[1] ; ybar<-apply(y,2,mean)
Sigma<-cov(y) ; THETA<-SIGMA<-NULL
yG<-NULL
S<-10000
for(m in 1:S)
{
  ###update theta
  Ln<-solve( solve(L0) + n*solve(Sigma) )
  mun<-Ln%*%( solve(L0)%*%mu0 + n*solve(Sigma)%*%ybar )
  theta<-rmvnorm(1,mun,Ln)
  ###update Sigma
  Sn<- S0 + ( t(y)-c(theta) )%*%t( t(y)-c(theta) )
  #Sigma<-rinvwish(1,nu0+n,solve(Sn))
  Sigma<-solve( rwish(1, nu0+n, solve(Sn)) )
  ### save results
  yG<-rbind(yG,rmvnorm(1,theta,Sigma))
  THETA<-rbind(THETA,theta)
  SIGMA<-rbind(SIGMA,c(Sigma))
}

#Plotting
x1 = seq(-4, 4, 0.25)
x2 = seq(-4, 4, 0.25)
z1 = matrix(0,nrow=length(x1),ncol=length(x2))
z2 = matrix(0,nrow=length(x1),ncol=length(x2))
z3 = matrix(0,nrow=length(x1),ncol=length(x2))
for (i in 1:length(x1))
{
  for (j in 1:length(x2))
  {
    z1[i,j] <- dmvnorm(c(x1[i],x2[j]),mu0,S0)
    z2[i,j] <- dmvnorm(c(x1[i],x2[j]),Mmu,Ms)
    z3[i,j] <- dmvnorm(c(x1[i],x2[j]),colMeans(THETA),matrix(colMeans(SIGMA),nrow=2,ncol=2))
  }
}
contour(x1,x2,z1,col='1',lty=1)
par(new=TRUE)
contour(x1,x2,z2,col='2',lty=2)
par(new=TRUE)
contour(x1,x2,z3,col='3',lty=3)
legend("bottomright", legend=c("True","MLE","Gibbs,Mean"),lty=c(1,2,3),col=c(1,2,3))

Theta.plot.1 <- hist(THETA[100:S,1],plot="false")
plot(Theta.plot.1$mids,Theta.plot.1$density,type="l",col='4',main="Comparasion-Mu1")
abline(v=colMeans(THETA)[1],col='3')
abline(v=0,col='1')
abline(v=Mmu[1],col='2')
legend("topright", legend=c("Gibbs","True","MLE"),lty=c(1,1,1),col=c(3,1,2))



Theta.plot.2 <- hist(THETA[100:S,2],plot="false")
plot(Theta.plot.1$mids,Theta.plot.1$density,type="l",col='4',main="Comparasion-Mu2")
abline(v=colMeans(THETA)[2],col='3')
abline(v=0,col='1')
abline(v=Mmu[2],col='2')
legend("topright", legend=c("Gibbs","True","MLE"),lty=c(1,1,1),col=c(3,1,2))