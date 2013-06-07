theta=seq(0,100,length=1000);alpha=50;beta=1/0.1;
y <- c(2, 1, 9, 4, 3, 3, 7, 7, 5, 7);n=length(y)
a=4;b=6
prior1 <- dgamma(theta,alpha,beta)
prior2 <- function(theta,alpha,beta,a,b){
z=dgamma(theta,alpha,beta)*as.numeric(theta > a & theta < b)/(pgamma(b,alpha,beta)-pgamma(a,alpha,beta))
return(z)}

#plot(theta,prior1,type="l",xlab="theta",ylab="Prior density",ylim=c(0,0.8))
#lines(theta,prior2(theta,alpha,beta,a,b),col="red",lty=2)
#title("Prior Density Distribution")


posterior1 <- dgamma(theta,alpha+sum(y),beta+n);

likelihood2 <- theta^(sum(y))*exp(-1*length(y)*theta)/prod(factorial(y))
marginal.probability2 <- prior2(theta,alpha,beta,a,b)%*% likelihood2
posterior2 <- prior2(theta,alpha,beta,a,b) * likelihood2/marginal.probability2

u <- y;
u.star <- (pgamma(6,alpha,beta)-pgamma(4,alpha,beta))*u+pgamma(4,alpha,beta)

truncated.inverse.cdf <- function(p,theta,alpha,beta){
F.a=pgamma(4,alpha,beta)
F.b=pgamma(6,alpha,beta)
z=qgamma((F.b-F.a)*p+F.a,alpha,beta)
return(z)
}
result <- mapply(truncated.inverse.cdf,u.star,theta,alpha,beta)

lower1 <- qgamma(.025,alpha+sum(y),beta+n)
upper1 <- qgamma(.975,alpha+sum(y),beta+n)
length1 <- mean(upper1-lower1)
lower2 <- truncated.inverse.cdf(.025,theta,alpha,beta)
upper2 <- truncated.inverse.cdf(.975,theta,alpha,beta)
length2 <- length1 <- mean(upper2-lower2)


plot(theta,posterior1,type="l",xlab="theta",ylab="Posterior density",ylim=c(0,0.8),xlim=c(0,10))
lines(theta,posterior2,col="red",lty=2)
title("Posterior Density Distribution")
