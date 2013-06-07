T=10001;sigma.cand=8
theta=matrix(nrow=1,ncol=T)
theta.star=matrix(nrow=1,ncol=T)
r=matrix(nrow=1,ncol=T)
theta[1]=3
acceptance.count=0

prior <- function(theta) {
  z <- 2/(3*sqrt(2*pi))*(exp(-1*0.5*theta^2)+0.5*exp(-1*0.5*(theta-3)^2))
  return (z)
}

for (i in 2:T) {
  theta.star[i]<-rnorm(1,theta[i-1],sigma.cand)
  r[i] <- prior(theta.star[i])/prior(theta[i-1])
  u <- runif(1,0,1)
  if (u< min(1,r[i])) {theta[i] <- theta.star[i]
  acceptance.count=acceptance.count+1}
  else {theta[i] <- theta[i-1]}
}

Acceptance.Rate <- acceptance.count/(T-1)

y <- hist(theta,plot="false")

plot(seq(1,T,length=T),theta,type="l")
plot(y$mids,prior(y$mids),type="l",col="green",ylim=c(0,0.4))
lines(y$mids,y$density,type="l",col="red")

