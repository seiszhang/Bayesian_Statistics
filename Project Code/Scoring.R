library(mvtnorm)

#Load Data
Data.training <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Training.csv",header=T)
Data.testing <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Testing.csv",header=T)
Data.validation <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Validation.csv",header=T)

Data <- Data.training
#Data <- Data.testing
#Data <- Data.validation

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


#Load Saved Beta
Beta <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Betas.csv",header=T)
sigma.z <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\SigmaZ.csv",header=T)
#Y.predicted <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\YPredicted.csv",header=T)


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
Beta <-Beta[5001:nrow(Beta),]
sigma.z <- sigma.z[5001:nrow(sigma.z),]
#Y.predicted <- Y.predicted[1001:nrow(Y.predicted),]

S <- nrow(Beta)

info.gain <- correct.rate <- matrix(nrow=S)

for ( s in 1:S)
{
  Y.pred<- X%*%t(Beta[s,])
  #Y.pred<-t(Y.predicted[s,])
  correct <- score <- matrix(ncol=n)
  
  for (pp in 1:200)
  {
    b.edge[pp,] <- (r.edge[2:5]-Y.pred[pp])/sigma.z[s]
    b.cul[pp,] <- pnorm(b.edge[pp,])
    b[pp,] <- c(b.cul[pp,1],b.cul[pp,2]-b.cul[pp,1],b.cul[pp,3]-b.cul[pp,2],
               b.cul[pp,4]-b.cul[pp,3],1-b.cul[pp,4])
    score[pp] <- log2(b[pp,Y[pp]]/r[Y[pp]])
    correct[pp]<- as.numeric( Y[pp] == which(b[pp,] == max(b[pp,])))
  }
  info.gain[s] <- mean(score)/entropy
  correct.rate[s] <- mean(correct)
  if (s%%500==0) {print(s)}
}
plot(Beta[,3],type="l",ylim=c(0.2,1.2))
print(mean(info.gain))
print(mean(correct.rate))
print("Make Sure Which Data Set!As well as corresponding r bins!")