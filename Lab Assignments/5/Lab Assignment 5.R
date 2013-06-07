data1<-read.table("http://www.stat.washington.edu/~hoff/courses/564-2010/Data/HwData/school1.dat")
data2<-read.table("http://www.stat.washington.edu/~hoff/courses/564-2010/Data/HwData/school2.dat")
data3<-read.table("http://www.stat.washington.edu/~hoff/courses/564-2010/Data/HwData/school3.dat")

school1 <- data3
mu.0=5;sigma.0.square=4;k.0=1;v.0=2;
n.1=length(t(school1))
k.1=k.0+n.1
mu.1=(sum(school1)+k.0*mu.0)/k.1
v.1=v.0+n.1
sigma.1.square=1/v.1*(v.0*sigma.0.square+var(school1)*(n.1-1)+(n.1*k.0/k.1*(mean(t(school1))-mu.0)^2))

mean.1=rnorm(S,mu.1,sqrt(sigma.1.square/k.1))
sd.1=sqrt(1/rgamma(S,v.1/2,v.1*sigma.1.square/2))
posterior.1<- rnorm(S,mean.1,sd.1)

mean.credible.lower=quantile(mean.1,0.025)
mean.credible.higher=quantile(mean.1,0.975)
sd.credible.lower=quantile(sd.1,0.025)
sd.credible.higher=quantile(sd.1,0.975)
mean.posterior.1=1/S*sum(posterior.1)

print(mean.credible.lower)
print(mean.credible.higher)
print(sd.credible.lower)
print(sd.credible.higher)
print(mean.posterior.1)


