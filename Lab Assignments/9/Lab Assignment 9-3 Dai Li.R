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
Y <- read.table(header = TRUE,"http://www.stat.washington.edu/hoff/Book/Data/hwdata/interexp.dat")

### pri r parameters
n<???dim(Y)[1] ; p<???dim(Y) [2]
mu0<???c(25,25)
sd0<???(mu0/2)
L0<???matrix(.1,p,p) ; diag(L0)<???1 ; L0<??? L0*outer(sd0,sd0)
nu0<???p+2 ; S0<???L0

###
### s t a r t i n g va lue s
Sigma<???S0
Y.full <???Y
O <???1*(!is.na(Y))
for(j in 1:p)
{
  Y.full[is.na(Y.full[,j]),j]<???mean(Y.full[,j],na.rm=TRUE)
}
###

### Gibbs sampler
THETA<???SIGMA<???Y.MISS<???NULL
set.seed( 1 )
for (s in 1:10000)
{
  ###update Sigma
  ybar<???apply (Y.full,2,mean)
  S <- (t(Y.full)-c(ybar))%*%t(t(Y.full)-c(ybar))
  Sigma <- rwish(1,n,solve(S))
  ###
  ###update the theta
  
  theta <- rmvnorm(1,ybar,1/n*Sigma)
  
  
  ###update missing data
  for (i in 27:n)
  {
    b <??? which( O[ i ,]==0 )
    a <??? which( O[ i ,]==1 )
    iSa<??? solve(Sigma[a,a])
    beta.j <??? Sigma[b,a]%*%iSa
    Sigma.j <??? Sigma[b,b]???Sigma[b,a]%*%iSa%*%Sigma[a,b]
    theta.j<??? theta[b] + beta.j%*%(t(Y.full[i,a])???theta[a] )
    Y.full[i,b] <??? rmvnorm( 1,theta.j,Sigma.j)
  }
  
  ### save r e s u l t s
  THETA<???rbind(THETA, theta) ; SIGMA<???rbind(SIGMA, c(Sigma))
  Y.MISS<???rbind(Y.MISS,Y.full[O==0])
  ###
}
plot(THETA[100:10000],type='l')
plot(SIGMA[100:10000],type='l')
result <- THETA[,1]-THETA[,2]
print(mean(result))
quantile(result,c(0.025,0.975))
