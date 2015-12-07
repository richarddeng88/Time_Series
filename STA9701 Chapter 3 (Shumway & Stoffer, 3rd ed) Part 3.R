library(astsa)

# ------------------------------------------------------------------------------------
# A simulated ARIMA(1,1,0) process

set.seed(20)
arima.1=arima.sim(list(order=c(1,1,0),ar=0.8),n=300)
plot(arima.1,xlab="",ylab="",main="A simulated ARIMA(1,1,0)")
acf2(arima.1,40)

# lag-1 differencing
plot(diff(arima.1),xlab="",ylab="",main="Differenced series")
acf2(diff(arima.1),40)


# ------------------------------------------------------------------------------------
# Simulated IMA(1,1) process

set.seed(666)
x=arima.sim(list(order=c(0,1,1),ma=-0.8),n = 100)
(x.ima=HoltWinters(x,beta=FALSE,gamma=FALSE)) # alpha=0.1663072 is 1-lambda
plot(x.ima)


# ------------------------------------------------------------------------------------
# Model specification; a simulated ARIMA(1,1,0) process

# See "A simulated ARIMA(1,1,0) process" above


# ------------------------------------------------------------------------------------
# Model specification; BIC

set.seed(10)
test=arima.sim(model=list(ar=c(rep(0,11),0.5),ma=c(rep(0,11),0.7)),n=200)
library(TSA); res=armasubsets(y=test,nar=14,nma=14,y.name='test',ar.method='ols')
detach("package:TSA", unload=TRUE)
par(mfrow=c(1,1)); plot(res)


# ------------------------------------------------------------------------------------
# Analysis of GNP data & model choice for GNP data

data(gnp)
plot(gnp)
acf2(gnp,48)

gnpgr=diff(log(gnp)) # growth rate
plot(gnpgr)
acf2(gnpgr,24)

sarima(gnpgr,1,0,0) # AR(1)
sarima(gnpgr,0,0,2) # MA(2)
ARMAtoMA(ar=.35,ma=0,10) # psi-coefficients


# ------------------------------------------------------------------------------------
# ACF & PACF of some pure SARMA models

# SMA models
sma.acf=ARMAacf(ar=,ma=c(rep(0,11),0.4),lag.max=50)[-1]
sma.pacf=ARMAacf(ar=,ma=c(rep(0,11),0.4),lag.max=50,pacf=TRUE)

op=par(mfrow=c(1,2))
plot(sma.acf,type="h",xlab="Lag",ylab="ACF",ylim=c(-0.2,1)); abline(h=0)
plot(sma.pacf,type="h",xlab="Lag",ylab="PACF",ylim=c(-0.2,1)); abline(h=0)
par(op)

# SAR models
sar.acf=ARMAacf(ar=c(rep(0,11),0.5),ma=,lag.max=50)[-1]
sar.pacf=ARMAacf(ar=c(rep(0,11),0.5),ma=,lag.max=50,pacf=TRUE)

op=par(mfrow=c(1,2))
plot(sar.acf,type="h",xlab="Lag",ylab="ACF",ylim=c(-0.2,1)); abline(h=0)
plot(sar.pacf,type="h",xlab="Lag",ylab="PACF",ylim=c(-0.2,1)); abline(h=0)
par(op)

# SARMA models
sarma.acf=ARMAacf(ar=c(rep(0,11),0.8),ma=-0.5,lag.max=50)[-1]
sarma.pacf=ARMAacf(ar=c(rep(0,11),0.8),ma=-0.5,lag.max=50,pacf=TRUE)

op=par(mfrow=c(1,2))
plot(sarma.acf,type="h",xlab="Lag",ylab="ACF",ylim=c(-0.4,1)); abline(h=0)
plot(sarma.pacf,type="h",xlab="Lag",ylab="PACF",ylim=c(-0.4,1)); abline(h=0)
par(op)


# ------------------------------------------------------------------------------------
# Federal Reserve Board Production Index

data(prodn)
plot(prodn,ylab="",xlab="",main="Production")

acf2(prodn,48)
acf2(diff(prodn),48)
acf2(diff(diff(prodn),12),48)

# Model (i) on page 161
sarima(prodn,2,1,0,0,1,1,12) # fit model
sarima.for(prodn,12,2,1,0,0,1,1,12) # forecast

# Model (ii) on page 161
sarima(prodn,2,1,0,0,1,3,12)
sarima.for(prodn,12,2,1,0,0,1,3,12)

# Model (iii) on page 161
sarima(prodn,2,1,0,2,1,1,12)
sarima.for(prodn,12,2,1,0,2,1,1,12)


