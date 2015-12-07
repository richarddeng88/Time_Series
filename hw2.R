# Q 2.2 a
library(astsa); data(gtemp)
mor <- cmort # mortality
tem <- tempr # temperature
par <- part # particulates

temp <- tem-mean(tem); temp2 =temp^2; trend = time(tem)
x <- ts.intersect(mor, trend, temp, temp2, part, lag(par,-4))
fit2 <- lm(x[,1]~x[,2:6])

summary(fit2)
summary(aov(fit2))

# Q 2.2 b
pairs(x)
cor(x)

# Q 2.8 
data(varve)
var <- varve
n <- length(var)
var(var[1:(n/2)]) #1st half
var(var[(n/2):n]) #2nd half

# log the data
log_var <- log(var)
var(log_var[1:(n/2)]) #1st half
var(log_var[(n/2):n]) #2nd half

hist(var)
hist(log_var)

plot(log_var)

acf(log_var)

dif_var <-diff(log_var)
par(mfrow=c(1,2))
plot(dif_var, main="differenced log(varve")
acf(dif_var, main="differenced log(varve)")

# 2.12
tem <- gtemp
plot(tem, type="l", ylab="Global Temperature Deviations")
lines(filter(tem, sides=2, rep(1,5)/5), col="blue")
lines(ksmooth(time(tem),tem, "normal", bandwidth= 2), col="red")
legend("topleft",legend=c("moving average smoother", "kernel smoother"), col = c("blue","red")
       ,lty=c(2,2))


