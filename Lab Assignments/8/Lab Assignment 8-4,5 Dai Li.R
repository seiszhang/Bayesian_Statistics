### Code from Hoff's website
### sample from the Wishart distribution
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
library(mvtnorm)
age<-read.table(header = TRUE,"http://www.stat.washington.edu/hoff/Book/Data/hwdata/agehw.dat")

mu0<-c(0,0)
L0<-matrix(c(10^5,0,0,10^5),2,2)

nu0<-3
S0<-matrix(c(1000,0,0,1000),2,2)

n<-dim(age)[1]
ybar<-apply(age,2,mean)
Sigma<-cov(age)
THETA<-SIGMA<-NULL

for (s in 1:10000)
{
  Ln<-solve(solve(L0)+n*solve(Sigma))
  mun<-Ln%*%(solve(L0)%*%mu0+n*solve(Sigma)%*%ybar)
  theta<-rmvnorm(1,mun,Ln)
  
  Sn<-S0+(t(age)-c(theta))%*%t(t(age)-c(theta))
  Sigma<-solve(rwish(1,nu0+n,solve(Sn)))
  
  THETA<-rbind(THETA,theta)
  SIGMA<-rbind(SIGMA,c(Sigma))
}
plot(density(THETA[,1]),col="blue",main="MCMC-DENSITY",xlim=c(35,50))
lines(density(THETA[,2]),col="red")
legend("topright", legend=c("Husband","Wife"), col=c(1,2), lty=1)
quantile(THETA[,1],c(0.025,0.975))
quantile(THETA[,2],c(0.025,0.975))

####Correlation Coffecient
Coff=SIGMA[,2]/sqrt(SIGMA[,1]*SIGMA[,4])
plot(density(Coff),main="MCMC-Correlation Coff Density")
quantile(Coff,c(0.025,0.975))

###########
mean(THETA[,1]>THETA[,2])

