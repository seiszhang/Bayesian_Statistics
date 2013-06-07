library(nnet)

#Load Data
Data <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Training.csv",header=T)
X <- scale(Data[,3:14])
y <- scale(Data[,15])
n <- dim(X)[1]
p <- dim(X)[2]
Y <- matrix(nrow=n,ncol=1)
Y[y<=-1.5] <- 1
Y[y<=-0.5 & y>-1.5] <- 2
Y[y<=0.5 & y>-0.5] <- 3
Y[y<=1.5 & y>0.5] <- 4
Y[y>1.5] <- 5

colnames(Y)<-c("result")
mydata<-data.frame(cbind(Y,X))
attach(mydata)
m <- multinom(result ~ X,mydata)
                #POP + CPOPLASTLASTYEAR+CPOPLASTYEAR
                #+GDP+CGDPLASTLASTYEAR+CGDPLASTYEAR
              #+EMPLOYMENT+CEMPLOYMENTLASTLASTYEAR+CEMPLOYMENTLASTYEAR
               # +INCOME+CINCOMELASTLATSYEAR+CINCOMELASTYEAR
                #,mydata)

#New Inputs
Data.training <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Training.csv",header=T)
Data.testing <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Testing.csv",header=T)
Data.validation <- read.csv("C:\\Users\\Dai\\Desktop\\STA 601 Course Project\\Code\\Validation.csv",header=T)

Data <- Data.training
#Data <- Data.testing
#Data <- Data.validation

X <- scale(Data[,3:14])
y <- scale(Data[,15])
n <- dim(X)[1]
p <- dim(X)[2]
Y <- matrix(nrow=n,ncol=1)
Y[y<=-1.5] <- 1
Y[y<=-0.5 & y>-1.5] <- 2
Y[y<=0.5 & y>-0.5] <- 3
Y[y<=1.5 & y>0.5] <- 4
Y[y>1.5] <- 5

b<-predict(m,X,"probs")

###Estimation Bins
r.edge <- matrix(c(-100,-1.5,-0.5,0.5,1.5,100),nrow=6)
r.training <- matrix(c(0.0300,0.3100,0.4050,0.1650,0.0900),nrow=5)
r.testing <- matrix(c(0.0400,0.3050,0.3750,0.2100,0.0700),nrow=5)
r.validation <- matrix(c(0.0350,0.3100,0.3650,0.2150,0.0750),nrow=5)

r <- r.training
#r <- r.testing
#r <- r.validation

entropy <- sum(-1*r*log2(r))

#scoring
correct <- score <- matrix(ncol=n)
for (pp in 1:200)
{
  score[pp] <- log2(b[pp,Y[pp]]/r[Y[pp]])
  correct[pp]<- as.numeric( Y[pp] == which(b[pp,] == max(b[pp,])))
}

info.gain <- mean(score)/entropy
correct.rate <- mean(correct)

print(info.gain)
print(correct.rate)
plot(X[1:50,3],b[1:50,3],col="red",pch="3")
points(X[1:50,3],b[1:50,4],col="green",pch="4")
points(X[1:50,3],b[1:50,1],col="yellow",pch="1")
points(X[1:50,3],b[1:50,2],col="blue",pch="2")
points(X[1:50,3],b[1:50,5],col="black",pch="5")

x.sorted<-sort(X[,3],index.return=T)
barplot(height=t(b[x.sorted$ix[c(2,25,50,80,100,118,155,174,196)],]),
        beside=F,col=c("red","darkred","yellow","darkgreen","green"),
        names.arg=c(round(X[x.sorted$ix[c(2,25,50,80,100,118,155,174,196)],3],1)),
        main="Predicted Probability of Population Trend for Several MSAs,MultiLogit",
        xlab="Population Change in Preceding Year",ylab="Probability")

Data[c(11,27,1,103,6),2]
Y[c(11,27,1,103,6),1]
barplot(height=t(b[c(11,27,1,103,6),]),
        beside=F,col=c("red","darkred","yellow","lightgreen","green"),
        names.arg=paste(c("Detriot","Cleveland","New York","Durham","Houston")),las=2,
        main="Predicted vs. Actual Probability of Population Trend for Several MSAs,MultiLogit",ylab="Probability")
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


