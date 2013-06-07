T=1000;v=0.5;lambda=5;n=100;alpha=10;beta=2

phi=matrix(nrow=100,ncol=T)
y=matrix(nrow=100,ncol=T)


for (i in 1:n) {
phi[i,1] <- rgamma(1,v/2,v/2)
y[i,1]=rpois(1,phi[i,1]*lambda[1])
if(y[i,1]>170) { y[i,1]=0 }
}
posterior[1] <- lambda[1]*prod(phi[,1]*y[,1])

for (j in 2:T) {
lambda[j] <- rgamma(1,alpha+sum(y[,j-1]),beta+sum(phi[,j-1]))
#lambda[j] <- 5
for (i in 1:n) {
y[i,j] <- rpois(1,phi[i,j-1]*lambda[j-1])
phi[i,j] <- rgamma(1,y[i,j]+v/2,lambda[j-1]+v/2)
if(y[i,1]>170) { y[i,1]=0 }
}
}


posterior=matrix(nrow=T,ncol=1)
posterior.1 <- matrix(nrow=T,ncol=1)
posterior.2 <- matrix(nrow=T,ncol=1)
posterior.3 <- matrix(nrow=T,ncol=1)
posterior.4 <- matrix(nrow=T,ncol=1)
temp1 <- matrix(nrow=100,ncol=T)
temp2 <- matrix(nrow=100,ncol=T)


for (j in 1:T) {
for (i in 1:n) {
temp1[i,j] <- (v/2)^(v/2)/factorial(v/2-1)*phi[i,j]^(v/2-1)*exp(-1*v/2*phi[i,j])
temp2[i,j] <- (phi[i,j]*lambda[j])^y[i,j]/factorial(y[i,j])*exp(-1*phi[i,j]*lambda[j])
}
posterior.2[j]=prod(temp1[,j])
posterior.3[j]=prod(temp2[,j])
posterior.1[j]=5
#posterior.1[j] <- beta^alpha/factorial(alpha-1)*lambda[j]^(alpha-1)*exp(-1*beta*lambda[j])
}
posterior=posterior.1*posterior.2*posterior.3


plot(seq(1,T,length=T),lambda,type="l",xlab="iterations",main="Trace Plot of Lambda")
quantile(posterior,0.025)
quantile(posterior,0.975)
mean(posterior)