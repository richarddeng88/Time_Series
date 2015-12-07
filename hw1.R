library(astsa) # package contains the data sets and scripts that are used in the text
library(TSA) 

jj<-jj
plot(jj,type="o")  #company quarterly earning

gt <- gtemp
plot(gt, type="o")

w <- rnorm(500,0,1)
par(mfrow=c(3,1))
plot.ts(w,main="white noise")

acf(w, 20, main="white noise ACF with lag=20")
acf(w, 500, main="white noise ACF with lag=500")


v=filter(w,sides=2, rep(1/3,3))
plot.ts(v,main="moving average")
acf(w, 20, main="moving average ACF with lag=20")
acf(w, 50, main="moving average ACF with lag=50")

par(mfrow=c(1,1))