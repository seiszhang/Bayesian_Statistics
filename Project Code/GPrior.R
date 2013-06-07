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
diag(Sigma0)=0.2
gamma0=2
sigma20=sigma.z.mle

S <- 15000
#set initials
beta0 <- Beta.mle
sigma2 <- 1/rgamma(1,gamma0/2,gamma0*sigma20/2)
Y.pred <- matrix(nrow=S,ncol=200)

#Monte Carlo Sampling
set.seed(1)
Beta <- matrix(nrow=S,ncol=3)
g<-n;nu0<-1;s20<-1

Hg<- (g/(g+1))*X%*%solve(t(X)%*%X)%*%t(X)
SSRg<- t(y)%*%(diag(1,nrow=n)-Hg)%*%y

s2<-1/rgamma(S,(nu0+n)/2,(nu0*s20+SSRg)/2 )

Vb<- g*solve(t(X)%*%X)/(g+1)
Eb<- Vb%*%t(X)%*%y

E<-matrix(rnorm(S*p,0,sqrt( s2 )),S,p)
Beta<-t(t(E%*%chol(Vb)) +c(Eb))

Sigma.z <- matrix(nrow=S,ncol=1)
for (s in 1:S)
{
  Sigma.z[s] <- sqrt(var(X%*%(Beta[s,])-y))
}

write.csv(Beta, "C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Betas.csv", row.names=F)
write.csv(Sigma.z, "C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\SigmaZ.csv", row.names=F)
acf(Beta[,3],main="Beta3,G Prior")
plot(Beta[5000:15000,3],type="l",main="Beta3,G Prior")
