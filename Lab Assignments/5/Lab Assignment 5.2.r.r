S=100000;
data1<-read.table("http://www.stat.washington.edu/~hoff/courses/564-2010/Data/HwData/school1.dat")
data2<-read.table("http://www.stat.washington.edu/~hoff/courses/564-2010/Data/HwData/school2.dat")
data3<-read.table("http://www.stat.washington.edu/~hoff/courses/564-2010/Data/HwData/school3.dat")

school1 <- data1
mu.0=5;sigma.0.square=4;k.0=1;v.0=2;
n.1=length(t(school1))
k.1=k.0+n.1
mu.1=(sum(school1)+k.0*mu.0)/k.1
v.1=v.0+n.1
sigma.1.square=1/v.1*(v.0*sigma.0.square+var(school1)*(n.1-1)+(n.1*k.0/k.1*(mean(t(school1))-mu.0)^2))

mean.1=rnorm(S,mu.1,sqrt(sigma.1.square/k.1))
sd.1=sqrt(1/rgamma(S,v.1/2,v.1*sigma.1.square/2))
posterior.1<- rnorm(S,mean.1,sd.1)

school1 <- data2
mu.0=5;sigma.0.square=4;k.0=1;v.0=2;
n.1=length(t(school1))
k.1=k.0+n.1
mu.1=(sum(school1)+k.0*mu.0)/k.1
v.1=v.0+n.1
sigma.1.square=1/v.1*(v.0*sigma.0.square+var(school1)*(n.1-1)+(n.1*k.0/k.1*(mean(t(school1))-mu.0)^2))

mean.2=rnorm(S,mu.1,sqrt(sigma.1.square/k.1))
sd.2=sqrt(1/rgamma(S,v.1/2,v.1*sigma.1.square/2))
posterior.2<- rnorm(S,mean.2,sd.2)

school1 <- data3
mu.0=5;sigma.0.square=4;k.0=1;v.0=2;
n.1=length(t(school1))
k.1=k.0+n.1
mu.1=(sum(school1)+k.0*mu.0)/k.1
v.1=v.0+n.1
sigma.1.square=1/v.1*(v.0*sigma.0.square+var(school1)*(n.1-1)+(n.1*k.0/k.1*(mean(t(school1))-mu.0)^2))

mean.3=rnorm(S,mu.1,sqrt(sigma.1.square/k.1))
sd.3=sqrt(1/rgamma(S,v.1/2,v.1*sigma.1.square/2))
posterior.3<- rnorm(S,mean.3,sd.3)

p.1=1/S*sum(as.numeric((mean.1<mean.2)&(mean.2<mean.3)))
p.2=1/S*sum(as.numeric((mean.1<mean.3)&(mean.3<mean.2)))
p.3=1/S*sum(as.numeric((mean.2<mean.1)&(mean.1<mean.3)))
p.4=1/S*sum(as.numeric((mean.2<mean.3)&(mean.3<mean.1)))
p.5=1/S*sum(as.numeric((mean.3<mean.1)&(mean.1<mean.2)))
p.6=1/S*sum(as.numeric((mean.3<mean.2)&(mean.3<mean.1)))


p.1=1/S*sum(as.numeric((posterior.1<posterior.2)&(posterior.2<posterior.3)))
p.2=1/S*sum(as.numeric((posterior.1<posterior.3)&(posterior.3<posterior.2)))
p.3=1/S*sum(as.numeric((posterior.2<posterior.1)&(posterior.1<posterior.3)))
p.4=1/S*sum(as.numeric((posterior.2<posterior.3)&(posterior.3<posterior.1)))
p.5=1/S*sum(as.numeric((posterior.3<posterior.1)&(posterior.1<posterior.2)))
p.6=1/S*sum(as.numeric((posterior.3<posterior.2)&(posterior.2<posterior.1)))
print(p.1)
print(p.2)
print(p.3)
print(p.4)
print(p.5)
print(p.6)

p.1=1/S*sum(as.numeric((mean.1>mean.2)&(mean.1>mean.3)))
p.2=1/S*sum(as.numeric((posterior.1>posterior.2)&(posterior.1>posterior.3)))

