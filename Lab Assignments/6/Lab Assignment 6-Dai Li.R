T=10100;y=20;b=0.05;N=50;lamda=20;

for (i in 2:T) {
b[i] <- rbeta(1,21,N[i-1]-19)
N[i] <- rpois(1,lamda*(1-b[i]))+20
}

posterior <- 1/factorial(y)/factorial(N-y)*b^y*(1-b)^(N-y)*lamda^N*exp(-1*lamda)

#plot(seq(1,T,length=T),N,type="l",xlab="iterations",main="Trace Plot of N",ylim=c(15,60))
#plot(seq(1,T,length=T),b,type="l",xlab="iterations",main="Trace Plot of beta")
#plot(seq(1,10,length=10),posterior[1:10],type="b",xlab="iterations",main="First 10draws in Trace Plot of Gibbs Sampler")


lower <- quantile(b,0.05)
upper <- quantile(b,0.95)
print(lower)
print(upper)

r <- hist(N[100:10100])
print(r$density[1])