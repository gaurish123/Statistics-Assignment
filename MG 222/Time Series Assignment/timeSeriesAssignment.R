df <- read.csv("C:\\Users\\Lenovo\\Desktop\\ML\\Time Series\\exchange_rate.data")

price <- as.ts(df$Price)

lPrice <- log(price)

#Time series plot of the observed series
plot(price)
plot(lPrice)
#there is an obvious downward trend which can be linear or non-linear
plot(sqrt(price))
#not much change in the patter use either of the timeseries

#Acf plot and pacf plot
acf(lPrice)
pacf(lPrice,ylim = c(-0.1,0.2))

plot(diff(lPrice))
#Note: Nonconstant variance in a series with no trend may have to be 
#addressed with something like an ARCH model which includes a model
#for changing variation over time.



