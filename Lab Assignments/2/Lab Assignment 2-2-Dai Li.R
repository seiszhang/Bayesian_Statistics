theta=seq(0,100,length=1000);alpha=50;beta=1/0.1;
y <- c(2, 1, 9, 4, 3, 3, 7, 7, 5, 7);
y.new <- c(2,1,0,4,1,1,0,1,1,4)
n=length(y);n.new=length(y.new)
a=4;b=6
prior1 <- dgamma(theta,alpha,beta)
prior2 <- function(theta,alpha,beta,a,b){
z=dgamma(theta,alpha,beta)*as.numeric(theta > a & theta < b)/(pgamma(b,alpha,beta)-pgamma(a,alpha,beta))
return(z)}

#plot(theta,prior1,type="l",xlab="theta",ylab="Prior density",ylim=c(0,0.8))
#lines(theta,prior2(theta,alpha,beta,a,b),col="red",lty=2)
#title("Prior Density Distribution")


posterior1 <- dgamma(theta,alpha+sum(y),beta+n);
posterior2 <- dgamma(theta,alpha+sum(y),beta+n)*as.numeric(theta > a & theta < b)/(pgamma(b,alpha+sum(y),beta+n)-pgamma(a,alpha+sum(y),beta+n))
posterior3 <- dgamma(theta,alpha+sum(y.new),beta+n.new)*as.numeric(theta > a & theta < b)/(pgamma(b,alpha+sum(y.new),beta+n.new)-pgamma(a,alpha+sum(y.new),beta+n.new))


truncated.inverse.cdf <- function(p,theta,alpha,beta,yyyy){
F.a=pgamma(4,alpha+sum(yyyy),beta+length(yyyy))
F.b=pgamma(6,alpha+sum(yyyy),beta+length(yyyy))
z=qgamma((F.b-F.a)*p+F.a,alpha+sum(yyyy),beta+length(yyyy))
return(z)
}

lower1 <- qgamma(.025,alpha+sum(y),beta+n)
upper1 <- qgamma(.975,alpha+sum(y),beta+n)
length1 <- upper1-lower1
lower2 <- truncated.inverse.cdf(.025,theta,alpha,beta,y)
upper2 <- truncated.inverse.cdf(.975,theta,alpha,beta,y)
length2 <- upper2-lower2
lower3 <- truncated.inverse.cdf(.025,theta,alpha,beta,y.new)
upper3 <- truncated.inverse.cdf(.975,theta,alpha,beta,y.new)
length3 <- upper3-lower3



plot(theta,posterior1,,type="l",xlab="theta",ylab="Posterior density",ylim=c(0,1),xlim=c(0,10))
lines(theta,posterior2,col="red",lty=2)
legend(7.5,0.8,c("Untruncated","Truncated"),lty=c(1,1),col=c("black","red"))
title("Posterior Density Distribution")
print(length1)
print(length2)
