library(mvtnorm)

#Load Data
Data.training <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Training.csv",header=T)
Data.testing <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Testing.csv",header=T)
Data.validation <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Validation.csv",header=T)
Data <- Data.training

X <- Data[,3:14]
#X <- cbind(X[,3],X[,6],X[,9],X[,12])
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
Y.pred <- X%*%Beta.mle
sigma.z <- sqrt(var(Y.pred-y))

#Estimating Using MLE Result
Data <- Data.training
#Data <- Data.testing
#Data <- Data.validation

X <- Data[,3:14]
#X <- cbind(X[,3],X[,6],X[,9],X[,12])
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

Y.pred <- X%*%Beta.mle

###Estimation Bins
r.edge <- matrix(c(-100,-1.5,-0.5,0.5,1.5,100),nrow=6)
r.training <- matrix(c(0.0300,0.3100,0.4050,0.1650,0.0900),nrow=5)
r.testing <- matrix(c(0.0400,0.3050,0.3750,0.2100,0.0700),nrow=5)
r.validation <- matrix(c(0.0350,0.3100,0.3650,0.2150,0.0750),nrow=5)
r <- r.training
#r <- r.testing
#r <- r.validation

entropy <- sum(-1*r*log2(r))
b <- matrix(nrow=200,ncol=5)
b.edge <- b.cul <- matrix(nrow=200,ncol=4)

##Calculation Score for each 
score <- correct <- matrix(ncol=n)
for (pp in 1:n)
{
  b.edge[pp,] <- (r.edge[2:5]-Y.pred[pp])/sigma.z
  b.cul[pp,] <- pnorm(b.edge[pp,])
  b[pp,] <- c(b.cul[pp,1],b.cul[pp,2]-b.cul[pp,1],b.cul[pp,3]-b.cul[pp,2],
             b.cul[pp,4]-b.cul[pp,3],1-b.cul[pp,4])
  score[pp] <- log2(b[pp,Y[pp]]/r[Y[pp]])
  correct[pp]<- as.numeric( Y[pp] == which(b[pp,] == max(b[pp,])))
}
info.gain<- mean(score)/entropy

print(info.gain)
print(mean(correct))
Data[c(11,27,1,103,6),2]
Y[c(11,27,1,103,6),1]
barplot(height=t(b[c(11,27,1,103,6),]),
        beside=F,col=c("red","darkred","yellow","lightgreen","green"),
        names.arg=paste(c("Detriot","Cleveland","New York","Durham","Houston")),las=2,
        main="Predicted vs. Actual Probability of Population Trend for Several MSAs,Linear Regression",ylab="Probability")
text(x=0.45,y=0.5,c("Actual: 
                    Decrease Rapidly"),cex=0.9,font=2,col="blue")
text(x=1.6,y=0.5,c("Actual:
                   Decrease Slightly"),cex=0.9,font=2,col="blue")
text(x=2.8,y=0.5,c("Actual:
                   Remain the Same"),cex=0.9,font=2,col="blue")
text(x=4.0,y=0.5,c("Actual:
                   Increase Slightly"),cex=0.9,font=2,col="blue")
text(x=5.2,y=0.5,c("Actual:
                   Increase Rapidly"),cex=0.9,font=2,col="blue")

