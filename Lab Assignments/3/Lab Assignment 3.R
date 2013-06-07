theta=seq(0,100,length=10001);nu=1
tau.square <- dgamma(theta,(nu/2),(nu/2))
x=seq(-10,10,length=10001)
x.tau.square <- dnorm(0,1/tau.square)
marginal.p.unconditional <- dt(x,nu)
marginal.p.conditional <- dt(x,nu)*as.numeric(x > min(x.tau.square) & x < max(x.tau.square))/(pt(max(x.tau.square),nu)-pt(min(x.tau.square),nu))


#draw 10000 samples
theta.sample <- runif(10000,0,1)
tau.square.sample <- dgamma(theta.sample,(nu/2),(nu/2))
x.tau.square.sample <- dnorm(0,1/tau.square.sample)
marginal.p.sample <- dt(x.tau.square.sample,nu)


#Plotting
par(mfrow = c(2,2))
plot(x,marginal.p.unconditional,xlim=c(-10,10),type="l",xlab="x",ylab="Unconditional Density P(x)")
plot(x,marginal.p.conditional,xlim=c(-1.0,1),type="l",xlab="x",ylab="Conditional Density x|x.tau.square")
hist(marginal.p.unconditional,breaks=10,plot=TRUE)
hist(marginal.p.sample,breaks=10,plot=TRUE)


#Calculating Credible Interval
truncated.inverse.cdf <- function(x,x.tau.square,nu){
F.a=pt(min(x.tau.square),nu)
F.b=pt(max(x.tau.square),nu)
z=qt((F.b-F.a)*x+F.a,nu)
return(z)
}
lower.theoretical <- qt(0.025,nu)
upper.theoretical <- qt(0.975,nu)
lower.actual <- truncated.inverse.cdf(.025,x.tau.square.sample,nu)
upper.actual <- truncated.inverse.cdf(.975,x.tau.square.sample,nu)

ks.test(marginal.p.sample,"pt",1)