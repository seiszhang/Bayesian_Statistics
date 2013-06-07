library(mvtnorm)
data <- read.csv("C:\\Users\\Dai\\Desktop\\Bayesian Statistics\\Lab Assignments\\12\\data.csv",header=T)
library(mvtnorm)
X <- cbind(matrix(1,nrow=35,ncol=1),unlist(data[,1]),unlist(data[,2]),unlist(data[,3]))
s1=rnorm(dim(X)[1],0,1)
s2=rnorm(dim(X)[1],0,1.5)
s3=rnorm(dim(X)[1],0,2)
X <- cbind(X,matrix(s1,nrow=dim(X)[1],ncol=1),matrix(s2,nrow=dim(X)[1],ncol=1),matrix(s3,nrow=dim(X)[1],ncol=1))
y <- data[,4]

# Prior
p<- ncol(X)
p0<- rep(0.5,p)       
b0<- rep(0,p)         
s0<- rep(2,p) 


# SSVS
#use mle estimate for beta's starting value
mle<- glm(y ~ -1+X, family=binomial("probit"))
beta.mle<- mle$coef
beta<- beta.mle

n<- nrow(X)
z<- rep(0,n) 
T<- 5000 

Beta <- matrix(nrow=T,ncol=p)
count <- matrix(0,ncol=p)

for(t in 1:T){
  #latent variable
  eta<- X%*%beta # linear predictor
  z[y==0]<- qnorm(runif(sum(1-y),0,pnorm(0,eta[y==0],1)),eta[y==0],1)
  z[y==1]<- qnorm(runif(sum(y),pnorm(0,eta[y==1],1),1),eta[y==1],1)
  
  for(j in 1:p){

    V<- 1/(s0[j]^{-2} + sum(X[,j]^2))
    E<- V*sum(X[,j]*(z-X[,-j]%*%beta[-j]))
    p.hat<- 1/(1 + p0[j]/(1-p0[j]) * dnorm(0,E,sqrt(V))/dnorm(0,b0[j],s0[j]) )          
    m<- rbinom(1,1,p.hat)
    if (m==0) {count[j]=count[j]+1}
    rbinom(1,1,p.hat)
    beta[j]<- m*rnorm(1,E,sqrt(V))
  }
  
  # output results to a file for later processing
  Beta[t,] <- matrix(beta,ncol=7)
}

plot(Beta[,1],main="trace plot of beta-constant",ylim=c(-2,4))
plot(Beta[,2],main="trace plot of beta-age",,ylim=c(-0.5,0.3))
plot(Beta[,3],main="trace plot of beta-course",,ylim=c(-3,2))
plot(Beta[,4],main="trace plot of beta-statistics",,ylim=c(-2,3))
plot(Beta[,5],main="trace plot of beta-s1",,ylim=c(-1,1))
plot(Beta[,6],main="trace plot of beta-s2",,ylim=c(-1,1))
plot(Beta[,7],main="trace plot of beta-s3",,ylim=c(-0.5,0.5))

#output
table<- matrix(0,7,6)
for(i in 1:7){
  table[i,]<- c(mean(Beta[,i]),median(Beta[,i]),
                sqrt(var(Beta[,i])),quantile(Beta[,i],c(0.025,0.975)),count[i]/T)
}
table <- round(table*100)/100
write(t(table),file="problem3.txt",ncol=6)


