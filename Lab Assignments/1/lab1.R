# Jan 15

###--- Help ---###

help.start() 
help(sqrt)  # help(function name)
sqrt(9) # compute square root            

###--- Scalar ---###

x <- 1  # assign 1 to x
x
y = 2 # assign 2 to x
y

# + addition
# - subtraction
# * multiplication
# / division
# ^ power

x + y
x - 2
x * y
x / 2
x ^ 2
(x - 2*y)^2

x <- 3
x <- x + 5  # assign a new value to x using the old one
x

###--- Vector ---###

# c(a,b,c,d) creates a vector (a,b,c,d)
z <- c(1,2,3,4,5) # assign a vector (1,2,3,4,5) to z
z

# z[i] returns i-th element in vector z
z[2]

z[2] <- 10 # assign 10 to 2nd element in z
z

# seq(a,b,length=c) creates a c-dim vector with equal steps between numbers
# a is the first and c is the last element
seq(1,10,length=20)

# a:b creates a vector (a,a+1,...,b-1,b)
1:10  

# rep(a,b) creates a b-dim vector with element a
rep(0,20)


###--- Matrix ---###

# matrix(vector,nrow=a,ncol=b) creates a x b matrix from the vector
matrix(1:6,nrow=2,ncol=3)
matrix(1:6,nrow=2,ncol=3,byrow=T) # check the difference

# x[i,j] returns (i,j) element in x
x[1,2]
# x[i,] returns i-th row in x
x[2,]
# x[,j] returns j-th column in x
x[,3]


###--- If statement ---###

# if(condition){lines}
# if condition is true, R excute the lines in brackets

#-- logical operator --#
# a == b means a is equal to b
# a != b means a is not equal to b
# a <=(>=) b means a is smaller (larger) than or equal to b 
# a <(>) b means a is smaller (larger) than b 
# A && B means A and B 
# A || B means A or B 

x <- -1
y <- 1
z = 0
if((x>0)&&(y>0)){z <- 1}
z
if((x>0)||(y>0)){z <- 1}
z

# if(condition){line 1}else{line 2}
# if condition is true, R excute the line 1, else the line 2

x = 0
z = 0
if(x>0)){
  z <- 1
}else{
  z <- 2
}
z

###--- For loop ---###

# for(var in vec){lines}
# counter var runs through the vector vec and does lines each run

x <- 0
for(i in 1:10){
  x <- x + 1
}
x


###--- Plot ---###

# plot(x): plot x (y-axis) versus index number (x-axis)
# plot(x,y): plot y (y-axis) versus x (x-axis)
# lines(x,y): add lines
# abline(h=y): draw horizontal line at y
# abline(v=x): draw vertical line at x


#-- Plotting parameters --#

# type: "l" = lines, "p" = points, etc.
# lty: line type, 1=solic, 2=dased, etc.
# col: color, "blue", "red" etc.
# xlab: x-axis labels
# ylab: y-axis labels
# main: title

# R code to generate stuff for lecture 1
p = seq(0,1,length=100)
pd1 = dbeta(p,1,1)
pd2 = dbeta(p,1,10)
plot(p,pd2,type="l",xlab="theta",ylab="prior density")
lines(p,pd1,lty=2)
abline(v=0.5)
title("Examples of prior densities for a probability")


###--- Example ---###

# Generate 1000 samples from uniform dist (x) and compute mean and percentage of x < 0.3

# Initial setting
count <- 0  # count: x < 0.3
store <- rep(0,1000) # store x at each iteration

for(i in 1:1000){
  x <- runif(1) # generate x from uni
  store[i] <- x # store x
  if(x < 0.3){count = count + 1} # check if x < 0.3
}

mean(store)
count/1000