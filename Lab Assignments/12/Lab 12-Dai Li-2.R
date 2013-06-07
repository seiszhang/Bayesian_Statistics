data <- read.csv("C:\\Users\\Dai\\Desktop\\Bayesian Statistics\\Lab Assignments\\12\\data.csv",header=T)
#data$age<- (data$age - mean(data$age))/sqrt(var(data$age)) 
library(mvtnorm)
X <- cbind(matrix(1,nrow=35,ncol=1),unlist(data[,1]),unlist(data[,2]),unlist(data[,3]))
y <- data[,4]

##Prior
b0 <- matrix(c(0,0,0,0),nrow=4,ncol=1)
S0 <- matrix(0,nrow=4,ncol=4)
diag(S0) <- 5

#Initials
beta <- b0
Sigma <- S0
n <- nrow(X)
z <- rep(0,n)

#Gibbs
T=5000
Beta <- matrix(nrow=T,ncol=4)
for (t in 1:T)
{
  #latent variable z
  eta<- X%*%beta
  z[y==0]<- qnorm(runif(sum(1-y),0,pnorm(0,eta[y==0],1)),eta[y==0],1)
  z[y==1]<- qnorm(runif(sum(y),pnorm(0,eta[y==1],1),1),eta[y==1],1)
  
  #conditional distribution
  Sigma.hat <- solve(solve(Sigma)+t(X)%*%X)
  beta.hat <- Sigma.hat%*%(solve(Sigma)%*%b0+t(X)%*%z)
  beta <- c(rmvnorm(1,beta.hat,Sigma.hat))
  Beta[t,] <- beta
}

#output
table<- matrix(0,4,5)
for(i in 1:4){
  table[i,]<- c(mean(Beta[,i]),median(Beta[,i]),sqrt(var(Beta[,i])),quantile(Beta[,i],c(0.025,0.975)))
}
table <- round(table*100)/100
write(t(table),file="betas.txt",ncol=5)

#plot
plot(Beta[,1],main="trace plot of beta-constant",type='l',ylim=c(-5,8))
plot(Beta[,2],main="trace plot of beta-age",type='l',ylim=c(-0.5,0.3))
plot(Beta[,3],main="trace plot of beta-course",type='l',ylim=c(-3,2))
plot(Beta[,4],main="trace plot of beta-statistics",type='l',ylim=c(-2,3))

##### Problem 2  #######
Input=t(matrix(c(1,17,1,0,1,18,1,1),nrow=4,ncol=2))
Output=pnorm(Input%*%t(Beta))
prob=rowMeans(Output)
print(prob)

