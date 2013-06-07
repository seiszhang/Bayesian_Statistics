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

mu0<-c(45,40)
L0<-matrix(c(625,312.5,312.5,625),2,2)

nu0<-4
S0<-matrix(c(625,312.5,312.5,625),2,2)

prior.predictive<-matrix(nrow=100,ncol=2)
for (i in 1:100) {
theta.star<-rmvnorm(100,mu0,L0)
prior.predictive[i,]<-rmvnorm(1,colMeans(theta.star),cov(theta.star))
}

#plot(density(prior.predictive[,1]),col="blue",main="Prior Predictive Distribution",ylim=c(0,0.02))
#lines(density(prior.predictive[,2]),col="red")
#legend("topright", legend=c("Husband","Wife"), col=c(1,2), lty=1)

plot(prior.predictive[, 1], prior.predictive[, 2]) 
cov(prior.predictive)
abline(v=colMeans(prior.predictive)[1],col='blue')
abline(h=colMeans(prior.predictive)[2],col='red')

legend("topright", legend=c("Husband Avg","Wife Avg"), col=c(1,2), lty=1)

cof=cov(prior.predictive)[1,2]/(sqrt(var(prior.predictive[,1]))*sqrt(var(prior.predictive[,2])))
print(cof)
text(80,20, sprintf("<Correlation Coffecient=\"%f\">",cof))

###################################

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

mean(THETA[,2]>THETA[,1])

