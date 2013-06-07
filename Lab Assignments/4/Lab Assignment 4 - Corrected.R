######problem 3###########
theta=seq(0,10,length=11);alpha=9;beta=2;S=10000;
posterior <- dgamma(theta,alpha,beta)
temp2 <- c(1,S);

for (i in 1:S) {
temp2[i] <- as.numeric(rpois(1,rgamma(1,alpha,beta))>=6)
}
expectation <- sum(temp2)/S

print(expectation)

#######Problem 4############
threshold <- ceiling(100^(1/sqrt(2))/(2*pi))
temp <- rpois(S,rgamma(S,alpha,beta))
expectation2 <- sum(as.numeric(temp>=threshold))/S
print(expectation2)
hist((2*pi*temp)^(sqrt(2)),breaks=10,xlab="Bonus",main="Histogram of Bonus in Week 2")
