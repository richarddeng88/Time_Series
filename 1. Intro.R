library(TSA)
data(larain)
# plot larain TS
plot(larain, type="l")

# plot larain this year compared to last year
plot(y=larain,x=zlag(larain))
    # QQ PLOT
    qqnorm(larain);qqline(larain)

# 
data(color)
plot(color,ylab="color property", xlab="batch",type="o")

# monthly temperature in a city, we see it very seasonal. 
data(tempdub)
plot(tempdub, ylab="temperature", type="o")
        # season model
        month = season(tempdub)
        model2 <- lm(tempdub~ month-1)  # not including intercept
        model3 <- lm(tempdub~ month) # including intercept
                # residual plot
                plot(y=rstudent(model3),x=as.vector(time(tempdub)), xlab="time", ylab="standardized residuals",type = "o")
                points(y=rstudent(model3),x=as.vector(time(tempdub)),pch=as.vector(season(tempdub)))
                
                hist(rstudent(model3),xlab="standardized residuals")
                qqnorm(rstudent(model3))
                acf(rstudent(model3))
            
        # sin/cos model
        har <- harmonic(tempdub,1)
        model4 <- lm(tempdub~ har)
        summary(model4)
                # plotting
                ts(fitted(model4))
                plot(ts(fitted(model4), freq=12, start=c(1964,1)),ylab="temperature", type="l")
                ylim=range(c(fitted(model4),tempdub))
                points(tempdub)

        
# sales of oilfilters
data(oilfilters)
plot(oilfilters, type="o", ylab="sales")







