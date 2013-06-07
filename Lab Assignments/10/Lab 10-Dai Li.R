library(mvtnorm)
#data
m=8
Y<-matrix(ncol=m)
Y[1] <- read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/school1.dat")
Y[2] <- read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/school2.dat")
Y[3] <- read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/school3.dat")
Y[4] <- read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/school4.dat")
Y[5] <- read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/school5.dat")
Y[6] <- read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/school6.dat")
Y[7] <- read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/school7.dat")
Y[8] <- read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/school8.dat")

#as.matrix(as.data.frame())
#X<-matrix(nrow=25,ncol=m)

##weakly informative priors
nu0<-2 ; s20<-15
eta0<-2 ; t20<-10
mu0<-7 ; g20<-5

### initials

n<-sv<-ybar<-rep(NA,m)
for ( j in 1:m)
{
  y<-unlist(Y[j])
  ybar[j] <- mean(y)
  sv[j] <- var(y)
  n[j] <- sum(y)
}
theta <- ybar ; sigma2 <- mean(sv)
mu <- mean(theta) ; tau2 <-var(theta)  

#########GIBBS#######
set.seed(1)
S<-5000
THETA<-matrix(nrow=S , ncol=m)
SMT<-matrix(nrow=S , ncol=3)

###
for ( s in 1:S)
{
  #sample a new value of mu
  vmu<- 1/(m/tau2+1/g20 )
  emu<- vmu*(m*mean(theta)/tau2 + mu0/g20 )
  mu<-rnorm( 1,emu,sqrt(vmu))
  
  
  #sample a new value of tau2
  etam<- eta0+m
  ss<- eta0*t20 + sum( (theta-mu)^2 )
  tau2<- 1/rgamma ( 1 , etam/2 , ss/2)
  
  #sample new values of the thetas
  for (j in 1:m)
  {
    vtheta <- 1/(n[j]/sigma2+1/tau2 )
    etheta <- vtheta*( ybar[j] * n[j]/sigma2+mu/tau2 )
    theta[j] <- rnorm( 1 , etheta , sqrt(vtheta))
  }
  
  #sample new value of sigma2
  nun<- nu0+sum(n)
  ss<- nu0*s20
  for ( j in 1 :m)
  { 
    y=unlist(Y[j])
    ss<-ss+sum((y - theta[j])^2)
  }
  sigma2 <- 1/rgamma ( 1,nun/2,ss/2)
  
  #store results
  THETA[s,]<-theta
  SMT[s,]<-c(sigma2,mu,tau2)
}
plot(SMT[,2],type="l",main='Trace Plot of mu')

##########sigma
mean(SMT[,1])
quantile(SMT[,1],c(0.025,0.975))
result<-hist(SMT[,1],breaks=20,plot="false")
plot(result$mids,result$density,type="l",main="Density of Sigma^2")
abline(v=sigma2,col=1)
abline(v=mean(SMT[,1]),col=2)
legend("topright", legend=c("Prior Believe","Posterior Mean"),lty=c(1,1),col=c(1,2))

###### mu
mean(SMT[,2])
quantile(SMT[,2],c(0.025,0.975))
result<-hist(SMT[,2],breaks=20,plot="false")
plot(result$mids,result$density,type="l",main="Density of Mu")
abline(v=mu,col=1)
abline(v=mean(SMT[,2]),col=2)
legend("topright", legend=c("Prior Believe","Posterior Mean"),lty=c(1,1),col=c(1,2))

###### tau2
mean(SMT[,3])
quantile(SMT[,3],c(0.025,0.975))
result<-hist(SMT[,3],breaks=20,plot="false")
plot(result$mids,result$density,type="l",main="Density of Tau^2")
abline(v=tau2,col=1)
abline(v=mean(SMT[,3]),col=2)
legend("topright", legend=c("Prior Believe","Posterior Mean"),lty=c(1,1),col=c(1,2))
  
# Question 3
#posterior
R=SMT[,3]/(SMT[,1]+SMT[,3])
mean(R)
quantile(R,c(0.025,0.975))
result.posterior<-hist(R,plot="false")
#prior
tau2.prior<-1/rgamma(5000,eta0/2,eta0*t20/2)
sigma2.prior<-1/rgamma(5000,nu0/2,nu0*s20/2)
R.prior=tau2.prior/(sigma2.prior+tau2.prior)
mean(R.prior)
quantile(R.prior,c(0.025,0.975))
result.prior<-hist(R.prior,plot="false")

plot(result.posterior$mids,result.posterior$density,type="l",main="Density of R",xlim=c(0,1),ylim=c(0,5))
lines(result.prior$mids,result.prior$density,type="l",col=2)
legend("topleft", legend=c("Prior R","Posterior R"),lty=c(1,1),col=c(1,2))


######### Problem 4
####4.1
count=0
for ( s in 1:S)
{
  if (THETA[s,7]<THETA[s,6]) count=count+1
}
print(count/S)

##4.2
s=1
count=0
for ( s in 1:S)
{
  count2=0
  for (j in 1:m)
  {
    if (THETA[s,7]<THETA[s,j]) count2=count2+1
  }
  if (count2==7) count=count+1
}
print(count/S)

#####Problem 5
colMeans(THETA)
ybar
mean(mu)
mean(THETA)