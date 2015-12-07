library(astsa)

# ------------------------------------------------------------------------------------
# Global warming

data(gtemp)
plot(gtemp,type="o",ylab="Global Temperature Deviations")

summary(fit<-lm(gtemp~time(gtemp))) # regress gtemp on time
abline(fit) # add regression line to the plot


# ------------------------------------------------------------------------------------
# El Nino and fish population

data(soi); data(rec)
par(mfrow=c(2,1))
plot(soi,ylab="",xlab="",main="Southern Oscillation Index")
plot(rec,ylab="",xlab="",main="Recruitment")

par(mfrow=c(3,1))
acf(soi,48,main="Southern Oscillation Index")
acf(rec,48,main="Recruitment")
ccf(soi,rec,48,main="SOI vs Recruitment",ylab="CCF")

fish=ts.intersect(rec,soiL6=lag(soi,-6),dframe=TRUE)
summary(lm(rec~soiL6, data=fish,na.action=NULL))


# ------------------------------------------------------------------------------------
# Pollution, temperature and mortality

data(cmort); data(tempr); data(part)
par(mfrow=c(3,1))
plot(cmort,main="Cardiovascular Mortality",xlab="",ylab="")
plot(tempr,main="Temperature",xlab="",ylab="")
plot(part,main="Particulates",xlab="",ylab="")

dev.new() # open a new graphic device for the scatterplot matrix
pairs(cbind(Mortality=cmort,Temperature=tempr,Particulates=part))

temp=tempr-mean(tempr) # center temperature
temp2=temp^2
trend=time(cmort) # time
fit=lm(cmort~trend+temp+temp2+part,na.action=NULL)
summary(fit) # regression results
summary(aov(fit)) # ANOVA table (compare to next line)
summary(aov(lm(cmort~cbind(trend,temp,temp2,part)))) # Table 2.1

num=length(cmort) # sample size
AIC(fit)/num-log(2*pi) # AIC
AIC(fit,k=log(num))/num-log(2*pi) # BIC
(AICc=log(sum(resid(fit)^2)/num)+(num+5)/(num-5-2)) # AICc


# ------------------------------------------------------------------------------------
# Global warming

data(gtemp)
plot(gtemp,type="o",ylab="Global Temperature Deviations")
summary(fit.q<-lm(gtemp~time(gtemp)+I(time(gtemp)^2)))

par(mfrow=c(2,2))
plot(gtemp,type="o",main="gtemp")
lines((1880:2009),fit.q$fitted.values,col="red")
plot(resid(fit),type="o",main="detrended")
acf(gtemp,48,main="gtemp")
acf(resid(fit),48,main="detrended")

par(mfrow=c(3,1))
plot(gtemp,type="o",main="gtemp")
plot(resid(fit),type="o",main="detrended")
plot(diff(gtemp),type="o",main="first difference")

par(mfrow=c(3,1))
acf(gtemp,48,main="gtemp")
acf(resid(fit),48,main="detrended")
acf(diff(gtemp),48,main="first difference")


# ------------------------------------------------------------------------------------
# Accidental deaths, USA, 1973-1978; deaths.txt

deaths=read.table("C:/Wu Rongning/Teaching/STA9701F2015/R code/deaths.txt",sep="")
deaths=as.matrix(deaths/1000); n.d=length(deaths)

year=seq(1973,by=1/12,length=n.d)
plot(year,deaths,type="o",pch=22,lty=1,pty=2,xlab="year",ylab="monthly accidental deaths (in thousands)")

# Harmonic regression
t=seq(1,n.d)
lambda.1=2*pi/12; lambda.2=2*pi/6
cos.1=cos(lambda.1*t); cos.2=cos(lambda.2*t); sin.1=sin(lambda.1*t); sin.2=sin(lambda.2*t)
data.deaths=data.frame(deaths,cos.1,cos.2,sin.1,sin.2)

fit.deaths=lm(deaths~1+cos.1+cos.2+sin.1+sin.2,data=data.deaths)
summary(fit.deaths)

plot(year,deaths,type="p",pch=22,lty=1,pty=2,xlab="year",ylab="monthly accidental deaths (in thousands)")
lines(year,fit.deaths$fitted.values,lty=1,col="red")


# ------------------------------------------------------------------------------------
# Paleoclimatic glacial varves

data(varve)
par(mfrow=c(2,1))
plot(varve,main="varve",ylab="")
plot(log(varve),main="log(varve)",ylab="" )


# ------------------------------------------------------------------------------------
# El Nino and fish population

data(soi); data(rec)
lag1.plot(soi,12)
lag2.plot(soi,rec,8)


# ------------------------------------------------------------------------------------
# Moving average smoother

data(cmort)
ma5=filter(cmort,sides=2,rep(1,5)/5)
ma53=filter(cmort,sides=2,rep(1,53)/53)
plot(cmort,type="p",ylab="mortality")
lines(ma5,col="blue"); lines(ma53,col="red")


# ------------------------------------------------------------------------------------
# Polynomial and periodic regression smoothers

data(cmort)
wk=time(cmort)-mean(time(cmort)) # wk is essentially t/52 centered at zero
wk2=wk^2; wk3=wk^3
cs=cos(2*pi*wk); sn=sin(2*pi*wk)
reg1=lm(cmort~wk+wk2+wk3,na.action=NULL)
reg2=lm(cmort~wk+wk2+wk3+cs+sn,na.action=NULL)
plot(cmort,type="p",ylab="mortality")
lines(fitted(reg1),col="red"); lines(fitted(reg2),col="blue")


# ------------------------------------------------------------------------------------
# Kernel smoother

data(cmort)
plot(cmort,type="p",ylab="mortality")
lines(ksmooth(time(cmort),cmort,"normal",bandwidth=5/52),col="blue")
lines(ksmooth(time(cmort),cmort,"normal",bandwidth=2),col="red")


