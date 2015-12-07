
#------------------
# R basics
#------------------

library(astsa) # package contains the data sets and scripts that are used in the text
library(TSA) # package contains R functions and datasets detailed in the book by Cryer and Chan

?rnorm # description of command 'rnorm'

# Package ‘astsa’

data(package='astsa') # display all of the data files used in the text
require(astsa) # load package
data(varve) # use data set, 'varve', used in the text

# Commands

2+2 # expression
5*5+2; 5/5-1
2^3
pi; 1+pi/2

a=6 # assignment
y<-x<-1

ls()
rm(list=ls())

# Vectors and matrices

mydata=c(10,3,9,1,6,2)
mydata[3]; mydata[3:5]; mydata[-1]; mydata[-(1:2)]
mydata[mydata>3]
matrix(mydata,2,3); matrix(mydata,3,2)

# Recycling rule

x=c(1,2,3,4); y=c(2,4,6,8); z=c(10,20)
x+y; x*y; x+z; x/z

# Vectorized calculations

x=cbind(x1=3,x2=c(4:1,2:5)); x
apply(x,2,mean) # column means
apply(x,1,mean) # row means

# Generating sequences

x<-1:20; x
s1=seq(-5,5,by=0.2)
s2=seq(length=51,from=-5,by=0.2)
s3=rep(s1,times=2)
s1; s2; s3

# Input external data

sunspots=read.table("C:/Wu/Data sets/sunspots.txt")
scan("C:/Wu/Data sets/sunspots.txt")


#------------------
# Graphical output
#------------------

x=rnorm(300)
plot(x)
qqnorm(x); qqline(x)
hist(x)

y=seq(-1,1,length=100)
plot(y,exp(y),type="l",main="") # 'l' means lines; see ?plot

plot(rnorm(50),rnorm(50),pch=19) # 'pch=19' means solid circle; see ?points

m=matrix(x,100,3)
pairs(m)

# Multiple figure environments

par(mfrow=c(2,3)) # subsequent figures will be drawn in an nr-by-nc array by rows
par(mfcol=c(2,3)) # subsequent figures will be drawn in an nr-by-nc array by columns
plot(rnorm(100))
plot(rnorm(100),col=2) # run palette() to see the coding of colors
plot(rnorm(100),col=3)
plot(rnorm(100),col=4)
plot(rnorm(100),col=5)
plot(rnorm(100),col=6)

plot(rnorm(100))
plot(rnorm(100),col='red')
plot(rnorm(100),col='green3')
plot(rnorm(100),col='blue')
plot(rnorm(100),col='cyan')
plot(rnorm(100),col='magenta')


#------------------
# R for regression
#------------------

set.seed(2013)
x=1:20
y=2*x+rnorm(20,0,5)
plot(x,y)
fit=lm(y~x); summary(fit); abline(fit)
fit$resid; resid(fit)
fit$fitted; fitted(fit)
lm(y~x-1); lm(y~0+x)


#------------------
# R for time series
#------------------

mydata=c(1,2,3,2,1)
is.ts(mydata)
myts=as.ts(mydata); myts
myts=ts(mydata,start=1950); myts
myts=ts(mydata,start=c(1950,3),frequency=4); myts
time(myts)

window(myts,start=c(1951,1),end=c(1951,3))

x=ts(1:10); cbind(x,lag(x),lag(x,-1))

diff(x)
diff(x,lag=2)
diff(x,differences=2) # same as diff(diff(x))

x=-5:5; y=5*cos(x)
op=par(mfrow=c(3,2)) # multifigure setup
plot(x,main="plot(x)")
plot(x,y,main="plot(x,y)")
plot.ts(x,main="plot.ts(x)")
plot.ts(x,y,main="plot.ts(x,y)")
ts.plot(x,main="ts.plot(x)")
ts.plot(ts(x),ts(y),col=1:2,main="ts.plot(x,y)")
par(op) # reset the graphics parameters



