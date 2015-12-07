library(astsa)

# ------------------------------------------------------------------------------------
# Forecasting the Recruitment series

regr=ar.ols(rec,order=2,demean=FALSE,intercept=TRUE)
fore=predict(regr,n.ahead=24)
ts.plot(rec,fore$pred,col=1:2,xlim=c(1980,1990),ylab="Recruitment")
lines(fore$pred,type="p",col=2)
lines(fore$pred+fore$se,lty="dashed",col=4)
lines(fore$pred-fore$se,lty="dashed",col=4)


# ------------------------------------------------------------------------------------
# Yule-Walker estimation for a simulated AR(2) process

set.seed(90210)
ar2=arima.sim(list(order=c(2,0,0),ar=c(1.5,-.75)),n=144)
plot(1:144/12,ar2,type="l",xlab="Time (one unit = 12 points)")
abline(v=0:12,lty="dotted",lwd=2)

acf(ar2,48,main="")
acf(ar2,2,plot=FALSE)$acf
acf(ar2,0,type="covariance",plot=FALSE)$acf

Rp.hat=matrix(c(1,0.843,0.843,1),nrow=2)
phi.hat=solve(Rp.hat)%*%c(0.843,0.504)
sigma.hat=10.858*(1-phi.hat[,1]%*%c(0.843,0.504)); sigma.hat


# ------------------------------------------------------------------------------------
# Yule-Walker estimation of the Recruitment series

data(rec)
rec.yw=ar.yw(rec,order=2)
rec.yw$x.mean # mean estimate
rec.yw$ar # parameter estimates
sqrt(diag(rec.yw$asy.var.coef)) # standard errors
rec.yw$var.pred # error variance estimate


