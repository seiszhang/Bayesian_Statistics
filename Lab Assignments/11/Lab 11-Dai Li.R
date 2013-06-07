library(mvtnorm)
######## 1 ########33
#Informative Prior

beta0=matrix(c(23,0.001),nrow=2,ncol=1)
Sigma0=matrix(c(0.2,0,0,0.0001),nrow=2,ncol=2)
gamma0=2
sigma20=0.01

#Prior Predictive
S=5000
prior.predictive <- error <- sigma2 <- matrix(0,nrow=S,ncol=6)
set.seed(1)
for (s in 1:S)
{
    for (t in 1:6)
  {
    beta<- rmvnorm(1,beta0,Sigma0)
    sigma2[s,t] <- 1/rgamma(1,gamma0/2,gamma0*sigma20/2)
    error[s,t] <- rnorm(1,0,sigma2[s,t])
    prior.predictive[s,t] <- beta[1]+(t*2-1)*beta[2]+error[s,t]
  }
}

week=cbind(matrix(1,nrow=S,ncol=1),matrix(2,nrow=S,ncol=1),matrix(3,nrow=S,ncol=1),
      matrix(4,nrow=S,ncol=1),matrix(5,nrow=S,ncol=1),matrix(6,nrow=S,ncol=1))
plot(week,prior.predictive,ylim=c(20,26))
colMeans(prior.predictive)
quantile(prior.predictive[,1],c(0.025,0.975))
quantile(prior.predictive[,2],c(0.025,0.975))
quantile(prior.predictive[,3],c(0.025,0.975))
quantile(prior.predictive[,4],c(0.025,0.975))
quantile(prior.predictive[,5],c(0.025,0.975))
quantile(prior.predictive[,6],c(0.025,0.975))

############ 2 ############

Y <- read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/swim.dat")

#set initials
sigma2 <- 1/rgamma(1,gamma0/2,gamma0*sigma20/2)
Y.pred <- matrix(nrow=S,ncol=4)

#Posterior Predictive#
for ( s in 1:S)
{
  for (j in 1:4)
  {
    X <- matrix(c(1,1,1,1,1,1,1,3,5,7,9,11),nrow=6,ncol=2)
    Sigma.star <- solve(solve(Sigma0)+(t(X)%*%X)/sigma2)
    y <- unlist(matrix(Y[j,],nrow=6,ncol=1))
    mu.star <- Sigma.star%*%(solve(Sigma0)%*%beta0+t(X)%*%y/sigma2)
    beta <- t(rmvnorm(1,mu.star,Sigma.star))

    SSR <- t(y-X%*%beta)%*%(y-X%*%beta)
    sigma2 <- 1/rgamma(1,(gamma0+n)/2,(gamma0*sigma20+SSR)/2)
    
    Y.pred[s,j]<- c(1,13)%*%beta+rnorm(1,0,sigma2.t)
  }
}

count=matrix(0,nrow=4,ncol=1)
for (s in 1:S)
{
  for (j in 1:4)
  {
    count[j]=count[j]+as.numeric(Y.pred[s,j]==min(Y.pred[s,]))
  }
}

colMeans(Y.pred)
quantile(Y.pred[,1],c(0.025,0.975))
quantile(Y.pred[,2],c(0.025,0.975))
quantile(Y.pred[,3],c(0.025,0.975))
quantile(Y.pred[,4],c(0.025,0.975))



