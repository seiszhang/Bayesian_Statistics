a<-0.00001
b<-0.00001
n<-100
x<-seq(from=0,to=n,by=1)
xx <- seq(from=0,to=1,by=0.01)

BF <- gamma(a+b+n)*gamma(a)*gamma(b)/(2^n*gamma(a+x)*gamma(b+n-x)*gamma(a+b))
plot(BF,type="l")
plot(xx,dbeta(xx,a,b),type="l")
var(dbeta(xx,a,b))