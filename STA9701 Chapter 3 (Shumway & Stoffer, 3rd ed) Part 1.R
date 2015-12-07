library(astsa)

# ------------------------------------------------------------------------------------
# Realizations of AR(1)

par(mfrow=c(2,1))
plot(arima.sim(list(order=c(1,0,0),ar=.9),n=100),xlab="",ylab="",main=(expression(AR(1)~~~phi==.9)))
plot(arima.sim(list(order=c(1,0,0),ar=-.9),n=100),xlab="",ylab="",main=(expression(AR(1)~~~phi==-.9)))


# ------------------------------------------------------------------------------------
# Realizations of MA(1)

par(mfrow=c(2,1))
plot(arima.sim(list(order=c(0,0,1),ma=.5),n=100),xlab="",ylab="",main=(expression(MA(1)~~~theta==.5)))
plot(arima.sim(list(order=c(0,0,1),ma=-.5),n=100),xlab="",ylab="",main=(expression(MA(1)~~~theta==-.5)))


# ------------------------------------------------------------------------------------
# Determining psi-coefficients

ARMAtoMA(ar=c(.5,-.1),lag.max=50) # for a list
plot(ARMAtoMA(ar=c(.5,-.1),lag.max=50)) # for a graph

ARMAtoMA(ar=.5,ma=.4,50)
plot(ARMAtoMA(ar=.5,ma=.4,50))


# ------------------------------------------------------------------------------------
# ACF & PACF of an AR(2) model

ACF=ARMAacf(ar=c(1.5,-.75),ma=0,24)[-1]
PACF=ARMAacf(ar=c(1.5,-.75),ma=0,24,pacf=TRUE)
par(mfrow=c(1,2))
plot(ACF,type="h",xlab="lag",ylim=c(-.8,1)); abline(h=0)
plot(PACF,type="h",xlab="lag",ylim=c(-.8,1)); abline(h=0)


# ------------------------------------------------------------------------------------
# Preliminary analysis of the Recruitment series

data(rec)
acf2(rec,48) # will produce values and a graphic
(regr=ar.ols(rec,order=2,demean=FALSE,intercept=TRUE))
regr$asy.se.coef # standard errors of the estimates


