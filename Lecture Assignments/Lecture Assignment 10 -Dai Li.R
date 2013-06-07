###Wishart
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

###GENERATE DATA####
library(MASS)
library(mvtnorm)
mu0<-c(0,0)
S0<-matrix( c(1,0.8,0.8,1),nrow=2,ncol=2)
y_2 <- mvrnorm(n = 100, mu0, S0)
y_1 <- mvrnorm(n = 50, mu0, S0)

#####MLE
Mmu = matrix(c(mean(y_2[,1]), mean(y_2[,2])),nrow=1,ncol=2)
Ms = cov(y_2)
y_1_estimate_mle <- matrix(nrow=50,ncol=2)
for (i in 1:50) {y_1_estimate_mle[i,]=mvrnorm(n=1,Mmu+1*(y_2[i,]-Mmu),Ms-1*Ms)}
mspe_mle= (1/50)*sum((y_1-y_1_estimate_mle)^2)
print(colMeans(y_1_estimate_mle))
print(quantile(y_1_estimate_mle[,1],c(0.025,0.975)))
print(quantile(y_1_estimate_mle[,2],c(0.025,0.975)))

#######Gibbs
mu0<-c(0,0)
L0<-matrix( c(0.25,0.125,0.125,0.25),nrow=2,ncol=2)

nu0<-4
S0<-matrix( c(1,0.5,0.5,1),nrow=2,ncol=2)

n<-dim(y_2)[1] ; ybar<-apply(y_2,2,mean)
Sigma<-cov(y_2) ; THETA<-SIGMA<-NULL

S<-1000
y_1_estimate_gibbs <- array(0, dim = c(50, 2, S))

for(m in 1:S)
{
  ###update theta
  Ln<-solve( solve(L0) + n*solve(Sigma) )
  mun<-Ln%*%( solve(L0)%*%mu0 + n*solve(Sigma)%*%ybar )
  theta<-rmvnorm(1,mun,Ln)
  ###update Sigma
  Sn<- S0 + ( t(y_2)-c(theta) )%*%t( t(y_2)-c(theta) )
  Sigma<-solve( rwish(1, nu0+n, solve(Sn)) )
  
  #Generate y_1 from Conditional Normal Distribution
  y_1_estimate_gibbs[,,m]=mvrnorm(n=50,theta,Sigma)

}

print(rowMeans(colMeans(y_1_estimate_gibbs)))
print(quantile(y_1_estimate_gibbs[,1,],c(0.025,0.975)))
print(quantile(y_1_estimate_gibbs[,2,],c(0.025,0.975)))

mspe_gibbs= (1/50)*sum((y_1-rowMeans(y_1_estimate_gibbs))^2)

############# LINEAR REGRESSION ############
reg.results <- lm(y_1 ~ y_2[1:50,])
predict.lm(reg.results)
mspe_linear_regression= (1/50)*sum((y_1-predict.lm)^2)


###### Oracle###########
mspe_oracle= (1/50)*sum((y_1-mvrnorm(n = 50, mu0, S0))^2)


################# Interval #############
interval <- matrix(0,nrow=50,ncol=2)
##########test subject 1##########
count=50
for (i in 1:50)
{
  interval[i,]=quantile(y_1_estimate_gibbs[i,1,],c(0.025,0.975))
  if ((interval[i,1]>y_1[i,1]) || (interval[i,2]<y_1[i,1])) count=count-1  
}
count.1=count
##########test subject 2##########
count=50
for (i in 1:50)
{
  interval[i,]=quantile(y_1_estimate_gibbs[i,2,],c(0.025,0.975))
  if ((interval[i,1]>y_1[i,2]) || (interval[i,2]<y_1[i,2])) count=count-1  
}
print(rbind(count.1/50,count/50))

