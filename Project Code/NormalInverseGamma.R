library(mvtnorm)

#Load Data
Data <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Training.csv",header=T)
X <- Data[,3:14]
y <- Data[,15]
n <- dim(X)[1]
p <- dim(X)[2]
X <- matrix(unlist(X),nrow=n,ncol=p)
y <- matrix(unlist(y),nrow=n,1)
X <- matrix(scale(X,center=TRUE),nrow=n,ncol=p)
y <- matrix(scale(y),nrow=n,ncol=1)
Y <- matrix(nrow=n,ncol=1)
Y[y<=-1.5] <- 1
Y[y<=-0.5 & y>-1.5] <- 2
Y[y<=0.5 & y>-0.5] <- 3
Y[y<=1.5 & y>0.5] <- 4
Y[y>1.5] <- 5

#FREQUENTIST MLE ESTIMATE
fit.mle <- lm(y ~ 0+X)
Beta.mle <- fit.mle$coefficients
sigma.z.mle <- sqrt(var(y-X%*%Beta.mle))

#prior
beta0=Beta.mle
Sigma0=matrix(0,nrow=p,ncol=p)
diag(Sigma0)=0.2
nu0=1
sigma20=sigma.z.mle

#set initials
T=15000;
sigma2<-rep(NA,T)
Beta<- matrix(NA,nrow=T,ncol=p)
Beta[1,] <- c(beta0)
sigma2[1]<-sigma20
Sigma.z <- matrix(nrow=T,ncol=1)
Sigma.z[1] <- sqrt(var(y-X%*%Beta[1,]))

#Gibbs Sampling
set.seed(1)
for ( t in 2:T)
{
  Sigma.star <- solve(solve(Sigma0)+(t(X)%*%X)/sigma2[t-1])
  mu.star <- Sigma.star%*%(solve(Sigma0)%*%beta0+t(X)%*%y/sigma2[t-1])
  beta <- t(rmvnorm(1,mu.star,Sigma.star))
    
  SSR <- t(y-X%*%beta)%*%(y-X%*%beta)
  sigma2[t] <- 1/rgamma(1,(nu0+n)/2,(nu0*sigma20+SSR)/2) 
  Beta[t,] <- t(beta)
  
  Sigma.z[t] <- sqrt(var(X%*%beta-y))
}

write.csv(Beta, "C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Betas.csv", row.names=F)
write.csv(Sigma.z, "C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\SigmaZ.csv", row.names=F)
acf(Beta[,3],main="Beta3,Normal Inv-Gamma")
plot(Beta[5000:15000,3],type="l",main="Beta3,Normal Inv-Gamma")