library(astsa)
data(gtemp)

## Q 2.2 a
mor <- cmort # mortality
tem <- tempr # temperature
par <- part # particulates

temp <- tem-mean(tem); temp2 =temp^2; trend = time(tem)
fit1 <- lm(mor ~ trend + temp + temp2 + part , na.action=NULL)
summary(fit1)
summary(aov(fit1))

x <- ts.intersect(mor, trend, temp, temp2, part, lag(par,-4))
fit2 <- lm(x[,1]~x[,2:6])
summary(fit2)
summary(aov(fit2))

# Q 2.2 b
pairs(x)
cor(x)


# example 2.10 moving average smoother
par(mfrow=c(3,1))
mor <- cmort # mortality
ma <- filter(mor, sides=2, rep(1,5)/5)
ma53 <- filter(mor,sides=2, rep(1,53)/53)
plot(mor); plot(ma) ;plot(ma53)
# lines(ma); lines(ma53)

# example 2.12 kernel smoothing
par(mfrow=c(1,1))
mor <- cmort # mortality
plot(mor, type="p", ylab="mortality")
lines(ksmooth(time(mor),mor, "normal", bandwidth= 5/52))
lines(ksmooth(time(mor),mor, "normal", bandwidth=2))




