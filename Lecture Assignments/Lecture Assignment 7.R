S=100000
Monte.Carlo=rpois(S,rgamma(S,2+66,1+44))
hist(Monte.Carlo,xlab="y",main="Histogram of y")
mean.Monte.Carlo=mean(Monte.Carlo)
variance.Monte.Carlo=var(Monte.Carlo)
credible.lower.Monte.Carlo=quantile(Monte.Carlo,0.025)
credible.upper.Monte.Carlo=quantile(Monte.Carlo,0.975)
print(mean.Monte.Carlo)
print(variance.Monte.Carlo)
print(credible.lower.Monte.Carlo)
print(credible.upper.Monte.Carlo)

theta=seq(0,1,length=1001);
posterior.predictive=dnbinom(x,2+66,(1+44)/(1+44+1))
plot(posterior.predictive,type="l",xlab="y",ylab="probability",main="Posterior Predictive of Y")
n=2+66
p=1/(1+44+1)
mean.posterior.predictive=n*p/(1-p)
variance.posterior.predictive=n*p/(1-p)^2
credible.lower.p=qnbinom(0.025,2+66,(1+44)/(1+44+1))
credible.upper.p=qnbinom(0.975,2+66,(1+44)/(1+44+1))
print(mean.posterior.predictive)
print(variance.posterior.predictive)
print(credible.lower.p)
print(credible.upper.p)