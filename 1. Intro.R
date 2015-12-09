library(TSA)
data(larain)
# plot larain TS
plot(larain, type="l")
# plot larain this year compared to last year
plot(y=larain,x=zlag(larain))

data(color)
plot(color,ylab="color property", xlab="batch",type="o")













