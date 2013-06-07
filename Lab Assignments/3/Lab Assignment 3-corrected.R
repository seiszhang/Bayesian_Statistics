tau2<-rgamma(10000,shape=.5,rate=.5)
x.giventau2<-rnorm(10000,0,1/sqrt(tau2))
hist( x.giventau2)
qt(c(0.025,0.975),1)
quantile(x.giventau2,probs=0.0250)
quantile(x.giventau2,probs=0.0251)
(quantile(x.giventau2,probs=0.975)+quantile(x.giventau2,probs=
0.9751))/2
quantile(x.giventau2,probs=0.9750)
quantile(x.giventau2,probs=0.9751)
(quantile(x.giventau2,probs=0.0250)+quantile(x.giventau2,probs
=0.0251))/2
#3#
ks.test(c(x.giventau2),"pt",1)
