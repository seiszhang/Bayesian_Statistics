library(mvtnorm)

#Load Data
Data <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Training.csv",header=T)
X <- Data[,3:14]
y <- Data[,15]
n <- dim(X)[1]
p <- dim(X)[2]
X <- matrix(unlist(X),nrow=n,ncol=p)
y <- matrix(unlist(y),nrow=n,1)
X <- matrix(scale(X),nrow=n,ncol=p)
y <- matrix(scale(y),nrow=n,ncol=1)
Y <- matrix(nrow=n,ncol=1)
Y[y<=-1.5] <- 1
Y[y<=-0.5 & y>-1.5] <- 2
Y[y<=0.5 & y>-0.5] <- 3
Y[y<=1.5 & y>0.5] <- 4
Y[y>1.5] <- 5

#FREQUENTIST MLE ESTIMATE
fit.mle <- lm(y ~ 0+X)
Beta.mle <- fit.mle$coefficients
sigma.z.mle <- sqrt(var(y-X%*%Beta.mle))

#set initials
nu0=1
sigma20=sigma.z.mle
beta <- matrix(0,nrow=p,ncol=1)
sigma2 <- 1/rgamma(1,nu0/2,nu0*sigma20/2)
z <- matrix(nrow=n,ncol=1)

beta<- Beta.mle   #initial values
beta0<-rep(0,ncol(X))	 #prior mean of beta
Pbeta0<-diag(1,ncol(X)) #prior precision for beta (shrinkage prior)

T<-50000     #number of samples of Gibbs sampler
p0<- rep(0.5,p)        # prior probability of excluding a predictor
b0<- rep(0,p)          # prior mean for normal component if predictor included
cj<- rep(2,p)          # standard deviation for normal component

#Gibbs Sampling
Beta <- matrix(nrow=T,ncol=p)
Sigma.z <- matrix(nrow=T,ncol=1)
for ( t in 1:T)
{
  eta<-X%*%beta  
  #z[Y==1]<-qnorm(runif(sum(Y==1),0,pnorm(-1.5,eta[Y==1],1)),eta[Y==1],1)
  #z[Y==2]<-qnorm(runif(sum(Y==2),pnorm(-1.5,eta[Y==2],1),pnorm(-0.5,eta[Y==2],1)),eta[Y==2],1)
  #z[Y==3]<-qnorm(runif(sum(Y==3),pnorm(-0.5,eta[Y==3],1),pnorm(0.5,eta[Y==3],1)),eta[Y==3],1)
  #z[Y==4]<-qnorm(runif(sum(Y==4),pnorm(0.5,eta[Y==4],1),pnorm(1.5,eta[Y==4],1)),eta[Y==4],1)
  #z[Y==5]<-qnorm(runif(sum(Y==5),pnorm(1.5,eta[Y==5],1),1),eta[Y==5],1)
  
  sigma2[t] <- rgamma(1,(n+nu0)/2,(t(y-eta)%*%(y-eta)+nu0*sigma20)/2)
  phi<-1/sigma2[t]
    
  for(j in 1:p){
    # conditional posterior variance of beta_j under normal prior
    
    Vj<- 1/(t(X[,j])%*%X[,j]*phi+1/cj[j]^2)
    Ej<- Vj*phi*X[,j]%*%(y-X[,-j]%*%beta[-j])
    
    # conditional probability of including jth predictor
    phat<- 1-p0[j]/(p0[j]+ (1-p0[j]) * dnorm(0,b0[j],cj[j])/dnorm(0,Ej,sqrt(Vj)) )
    #phat<- 1/(1 + p0[j]/(1-p0[j]) * dnorm(0,Ej,sqrt(Vj))/dnorm(0,b0[j],cj[j]) )  
    
    m<- rbinom(1,1,phat)
    beta[j]<- m*rnorm(1,Ej,sqrt(Vj))
  }
  
  Beta[t,] <- t(beta)
  Sigma.z[t] <- sqrt(var(X%*%(Beta[t,])-y))

}

print("Betas Computation completed")
write.csv(Beta, "C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Betas.csv", row.names=F)
write.csv(Sigma.z, "C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\SigmaZ.csv", row.names=F)
plot(Beta[5000:50000,3],type="l",main="Beta3,SSVS")
acf(Beta[5000:50000,3],main="Beta3,SSVS")

################################
#Predicting Y using MLE
#Beta <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Betas.csv",header=T)

Y.pred.mle <- X%*%Beta.mle
sigma.z.mle <- sqrt(var(Y.pred.mle-Y))

#######################

# Plot Gibbs iterations 
#par(mfrow=c(3,2))
ylb=c("1","2","3","4","5","6","7","8","9","10","11","12")
for(j in 1:6){
  print(j)
  plot(Beta[,j],xlab="iteration",ylab=ylb[j])
}

# calculate posterior summaries of regression coefficients - 
# columns include posterior mean, median, standard deviation, 
# 95% credible interval, and Pr(beta_j=0|data)
table1<- matrix(0,p,6)
for(i in 1:p){
  table1[i,]<- c(mean(Beta[,i]),median(Beta[,i]),sqrt(var(Beta[,i])),
                 quantile(Beta[,i],probs=c(0.025,0.975)),
                 1-length(Beta[Beta[,i]==0,i])/nrow(Beta))
}
table1<- round(table1*1000)/1000
write(t(table1),file="beta_post2.txt",ncol=6)

# Calculate posterior probabilities for the best models among those
# visited
Mout<- Beta 
Mout<- matrix(as.numeric(I(Mout==0)),nrow(Mout),ncol(Mout)) 
Mindex<- Mout[1,] # different models sampled starting with first
# returns 1 if m1 and m2 are the same model
ind<- function(m1,m2){
  as.numeric(all(I(m1==m2)))
}
Im<- apply(Mout,1,ind,m2=Mout[1,]) # indicators of samples from 1st model
Nm<- sum(Im)                       # number of samples from 1st model
Mout<- Mout[Im==0,]
repeat{
  if(length(Mout)==12) Mout<- matrix(Mout,1,12)
  Mindex<- rbind(Mindex,Mout[1,])
  Im<- apply(Mout,1,ind,m2=Mout[1,])
  Nm<- c(Nm,sum(Im))
  print(sum(Nm))
  if(sum(Nm)==nrow(Beta)){ 
    break
  } else Mout<- Mout[Im==0,]
}


# Sort models visited by decreasing posterior probability
Pm<- Nm/sum(Nm)
ord<- order(Pm)
Pm<- Pm[rev(ord)]
Mindex<- Mindex[rev(ord),]
table2<- cbind(Pm,Mindex)
write(t(table2),file="Pmodel.txt",ncol=8)

table3=cbind(Pm,1-Mindex)

#Predicting Using Mixed Betas
Y.pred.ssvs<-matrix(nrow=200,ncol=1)
Mout<- Beta 
Mout<- matrix(as.numeric(I(Mout==0)),nrow(Mout),ncol(Mout)) 
s=1
for (s in 1:nrow(Beta)) {  
  if (identical(c(Mout[s,]),c(Mindex[1,])))
  Y.pred.ssvs <- cbind(Y.pred.ssvs,X%*%Beta[s,])
  #if (identical(c(Mout[s,]),c(Mindex[2,])))
  #Y.pred.ssvs <- cbind(Y.pred.ssvs,X%*%Beta[s,]) 
  #if (identical(c(Mout[s,]),c(Mindex[3,])))
  #Y.pred.ssvs <- cbind(Y.pred.ssvs,X%*%Beta[s,])
}
acf(Beta[5000:50000,3],main="Beta3,SSVS")