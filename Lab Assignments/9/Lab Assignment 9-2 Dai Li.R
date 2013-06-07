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
Y <-read.table(header = TRUE,"http://www.stat.washington.edu/hoff/Book/Data/hwdata/interexp.dat")
n<???dim(Y)[1] ; p<???dim(Y)[2]
mu0 <- c(25,25)
Sigma0 <- matrix(1,0.5,0.5,1)

THETA<-SIGMA<-NULL
Sigma<???S0
Y.full <???Y
O <???1*(!is.na(Y))
for(j in 1:p)
{
  Y.full[is.na(Y.full[,j]),j]<???mean(Y.full[,j],na.rm=TRUE)
}

for (t in 1:10000)
{
  y.bar <- colMeans(Y.full,1)
  S <- (t(Y.full)-c(y.bar))%*%t(t(Y.full)-c(y.bar))
  Sigma <- rwish(1,n,solve(S))
  Theta <- rmvnorm(1,y.bar,1/n*Sigma)
  
  THETA<-rbind(THETA,Theta)
  SIGMA<-rbind(SIGMA,c(Sigma))
}

result <- THETA[,1]-THETA[,2]
print(mean(result))
quantile(result,c(0.025,0.975))