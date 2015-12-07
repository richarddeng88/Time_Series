library(astsa)

# ------------------------------------------------------------------------------------
# Johnson & Johnson quarterly earnings

data(jj)
plot(jj,type="o",ylab="Quarterly Earnings per Share")


# ------------------------------------------------------------------------------------
# Global warming

data(gtemp)
plot(gtemp,type="o",ylab="Global Temperature Deviations")


# ------------------------------------------------------------------------------------
# New York Stock Exchange

data(nyse)
plot(nyse,ylab="NYSE Returns")


# ------------------------------------------------------------------------------------
# El Nino and fish population

data(soi); data(rec)
par(mfrow=c(2,1))
plot(soi,ylab="",xlab="",main="Southern Oscillation Index")
plot(rec,ylab="",xlab="",main="Recruitment")


# ------------------------------------------------------------------------------------
# Gaussian white noise, MA, & AR

w=rnorm(500)
v=filter(w,rep(1/3,3),sides=1)
x=filter(rnorm(550),filter=c(1,-.9),method="recursive")[-(1:50)]

par(mfrow=c(3,1))
plot.ts(w,main="Gaussian white noise")
plot.ts(v,main="Moving average")
plot.ts(x,main="Autoregression")


# ------------------------------------------------------------------------------------
# Random walk

set.seed(154)
w=rnorm(200); x=cumsum(w)
wd=w+.2; xd=cumsum(wd)
plot.ts(xd,ylim=c(-5,55),ylab="",main="random walk")
lines(x)
lines(.2*(1:200),lty="dashed")


# ------------------------------------------------------------------------------------
# Example: sample ACF of 200 simulated values of iid N(0,1) noise

x=rnorm(200)
par(mfrow=c(2,1))
plot.ts(x,type="o",ylab="",main="Gaussian white noise"); abline(h=0)
acf(x,40,main="")


# ------------------------------------------------------------------------------------
# SOI and recruitment

data(soi); data(rec)
par(mfrow=c(3,1))
acf(soi,48,main="Southern Oscillation Index")
acf(rec,48,main="Recruitment")
ccf(soi,rec,48,main="SOI vs Recruitment",ylab="CCF")


# ------------------------------------------------------------------------------------
# Soil surface temperatures

data(soiltemp)
persp(1:64,1:36,soiltemp,phi=30,theta=30,scale=FALSE,expand=4,ticktype="detailed",xlab="rows",ylab="cols",zlab="temperature")

fs=abs(fft(soiltemp-mean(soiltemp)))^2/(64*36)
cs=Re(fft(fs,inverse=TRUE)/sqrt(64*36)) # ACVF
rs=cs/cs[1,1] # ACF
rs2=cbind(rs[1:41,21:2],rs[1:41,1:21])
rs3=rbind(rs2[41:2,],rs2)
par(mar=c(1,2.5,0,0)+.1)
persp(-40:40,-20:20,rs3,phi=30,theta=30,expand=30,scale="FALSE",ticktype="detailed",xlab="row lags",ylab="column lags",zlab="ACF")


