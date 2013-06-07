######problem 3###########
theta=seq(0,10,length=11);alpha=9;beta=2;S=1000000;
posterior <- dgamma(theta,alpha,beta)

expectation <- function(x){
z=dgamma(x,9,2)*as.numeric(x>=6)
return (z)
}
theoretical=integrate(expectation,lower=0,upper=Inf)

expectation <- sum(as.numeric(rgamma(S,alpha,beta)>=6))/S
print(expectation)

#######Problem 4############
threshold <- ceiling(100^(1/sqrt(2))/(2*pi))
temp <- rgamma(S,alpha,beta)
expectation2 <- sum(as.numeric(temp>=threshold))/S
print(expectation2)
hist((2*pi*temp)^(sqrt(2)),breaks=10,xlab="Bonus",main="Histogram of Bonus in Week 2")
