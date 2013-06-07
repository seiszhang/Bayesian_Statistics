data <- read.csv("C:\\Users\\Dai\\Desktop\\Bayesian Statistics\\Lab Assignments\\12\\data.csv",header=T)
library(mvtnorm)
X <- cbind(matrix(1,nrow=35,ncol=1),unlist(data[,1]),unlist(data[,2]),unlist(data[,3]))
Y <- data[,4]
s1=rnorm(dim(X)[1],0,1)
s2=rnorm(dim(X)[1],0,1.5)
s3=rnorm(dim(X)[1],0,2)
X <- cbind(X,matrix(s1,nrow=dim(X)[1],ncol=1),matrix(s2,nrow=dim(X)[1],ncol=1),matrix(s3,nrow=dim(X)[1],ncol=1))

#Prior
b0 <- matrix(0,nrow=7,ncol=1)
S0 <- matrix(0,nrow=7,ncol=7)
diag(S0) <- 10

#Initials
beta <- b0
Sigma <- S0
Z <- X%*%beta+rnorm(35,0,1)

#Gibbs
T=5000
Beta <- t(beta)
set.seed(1)
count <- matrix(0,nrow=7)
reject <- matrix(nrow=T,ncol=7)
a=b=matrix(2,nrow=5)

for (t in 1:T)
{
  #latent variable z
  u <- runif(1,0,1)
  for (i in 1:dim(X)[1])
  {
    if (Y[i]==1)
    {
      f.a <- pnorm(0,X[i,]%*%beta,1)
      f.b <- 1
      u.star <- (f.b-f.a)*u+f.a
      Z[i] <- qnorm(u.star,0,1)
    }
    if (Y[i]==0)
    {
      f.a <- 0
      f.b <- pnorm(0,X[i,]%*%beta,1)      
      u.star <- (f.b-f.a)*u+f.a
      Z[i] <- qnorm(u.star,0,1)
    }  
  }
a=count+2
b=matrix(t-1,nrow=7)-count+2
  
p0<- p.hat <- matrix(nrow=7)
  
for ( j in 1:7)
{

  p0[j]<-rbeta(1,a[j],b[j])
  V <- solve(c^(-2)+t(X[,j])%*%X[,j])
  E <- V*(t(X[,j])%*%(Z-X[,-j]%*%matrix(beta[-j],nrow=6,ncol=1)))
  p.hat[j]<-p0[j]/(p0[j]+(1-p0[j])*rnorm(1,0,c^2)/rnorm(1,E,V))
  if (p0[j] < p.hat[j])
  {
    beta[j]=0
    count[j]=count[j]+1
    reject[t,j]=1
  }
  else
  {
    beta[j]<-rnorm(1,E,V)
    reject[t,j]=0
  }
}
  Beta <- rbind(Beta,t(beta))
}

##### Problem 2
Input=t(matrix(c(1,17,1,0,1,18,1,1),nrow=4,ncol=2))

Output=pnorm(Input%*%t(Beta[,1:4]),0,1)
Probability=sum(as.numeric(Output[1,]<Output[2,]))/dim(Output)[2]
print(Probability)
