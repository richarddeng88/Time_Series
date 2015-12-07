# Q3.9
ar1=arima.sim(list(order=c(1,0,0),ar=.6),n=100)
ma1=arima.sim(list(order=c(0,0,1),ma=.9),n=100)
arma11=arima.sim(list(order=c(1,0,1),ar=.6,ma=.9),n=100)
par(mfcol=c(1,2))
acf(ar1); pacf(ar1)
acf(ma1); pacf(ma1)
acf(arma11); pacf(arma11)

# Q3.10
library(astsa)
data(cmort)
reg=ar(cmort,aic=FALSE,order.max=2,method="ols")
# reg=ar.ols(cmort, order=2, demean=F, intercept=T)
reg; 
predict(reg,n.ahead=4)

# Q3.18
reg1=ar(cmort,aic=FALSE,order.max=2,method="ols")
reg1$asy.se.coef
reg2=ar(cmort,aic=FALSE,order.max=2,method="yule-walker")
reg2$asy.var.coef
sqrt(diag(reg2$asy.var.coef))


# Q3.21
phi=rep(0,10); theta=rep(0,10); sigma2=rep(0,10)
for (i in 1:10){
    x=arima.sim(n=200,list(ar=.9,ma=.5,sd=1))
    fit=arima(x, order=c(1,0,1))
    phi[i]=fit$coef[1]
    theta[i]=fit$coef[2]
    sigma2[i]=fit$sigma2
}
phi
theta
sigma2

# Q3.31
plot(gnp)
gnpgr=diff(log(gnp)) # growth rate
plot(gnpgr)
par(mfrow=c(2,1))
acf(gnpgr,lag.max=40,main="Growth rate")
acf(gnpgr,lag.max=40,type="partial",main="Growth rate")

fit.ar1=arima(gnpgr,order=c(1,0,0)) # AR(1)
tsdiag(fit.ar1,gof.lag=20)
Box.test(fit.ar1$residuals,lag=20,type="Box-Pierce")
Box.test(fit.ar1$residuals,lag=20,type="Ljung-Box")
McLeod.Li.test(y=fit.ar1$residuals,gof.lag=20)
qqnorm(fit.ar1$residuals); qqline(fit.ar1$residuals)
shapiro.test(fit.ar1$residuals)
library(tseries)
jarque.bera.test(fit.ar1$residuals)


# Q3.32
par(mfrow=c(1,1))
plot(oil,ylab="",main="Crude oil prices (in dollars per barrel)")
oil.gr=diff(log(oil))
plot(oil.gr,ylab="",main="Growth rate of crude oil prices")
par(mfrow=c(2,1))
acf(oil.gr,lag.max=40,main="Growth rate of crude oil prices")
acf(oil.gr,lag.max=40,type="partial",main="Growth rate of crude oil prices")
fit.ar3=arima(oil.gr,order=c(3,0,0))
fit.ma3=arima(oil.gr,order=c(0,0,3))
fit.arma11=arima(oil.gr,order=c(1,0,1))
tsdiag(fit.ma3,gof.lag=20)
Box.test(fit.ma3$residuals,lag=20,type="Box-Pierce")
Box.test(fit.ma3$residuals,lag=20,type="Ljung-Box")
library(TSA);par(mfrow=c(1,1))
McLeod.Li.test(y=fit.ma3$residuals,gof.lag=20)
