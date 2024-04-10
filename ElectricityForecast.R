library(TSA)
data(electricity)
elec <- electricity
plot(elec, main='Monthly US Electricity Production',
     ylab='Millions of Kwh',xlab='Year', type='l',
     ylim=c(0,400000))
plot(log(elec), main='Log Monthly US Electricity Production',
     ylab='Millions of Kwh',xlab='Year', type='l')
tseries::adf.test(elec)
# Log Transform #
elecl <- log(elec)
tseries::adf.test(elecl)
tseries::pp.test(elecl)
tseries::kpss.test(elecl)
acf(ts(elecl))
pacf(ts(elecl))
# Seasonal Difference #
elec.logd <- diff(log(elec),lag=12)
plot(elec.logd,main='Seasonal Difference')
acf(ts(elec.logd),lag.max=60,main='ACF of Seasonal Difference')
pacf(ts(elec.logd),lag.max=60,main='PACF of Seasonal Difference')
eacf(ts(elec.logd))
tseries::adf.test(elec.logd)
tseries::pp.test(elec.logd)
tseries::kpss.test(elec.logd)
# Selecting a Model
library(forecast)
auto.arima(elec.logd) #ARIMA(2,0,1)(2,0,1)[12]
Arima(elec.logd,order=c(2,0,1), ####
      seasonal=c(0,0,1))
Arima(elec.logd,order=c(1,0,3),
      seasonal=c(0,0,1))
Arima(elec.logd,order=c(2,0,2),
      seasonal=c(0,0,1))
15
Arima(elec.logd,order=c(1,0,2),
      seasonal=c(0,0,1))
Arima(elec.logd,order=c(1,0,1),
      seasonal=c(1,0,1))
model <- Arima(y=log(elec),order=c(2,0,1),
               seasonal=c(0,1,1),xreg=1:length(elec))
# Model Diagnostics
tsdiag(model)
qqnorm(rstandard(model))
qqline(rstandard(model))
shapiro.test(rstandard(model))
# Overfitting
Arima(elec.logd,order=c(3,0,1),
      seasonal=c(0,0,1))
Arima(elec.logd,order=c(2,0,2),
      seasonal=c(0,0,1))
# Prediction
pred <- predict(model,n.ahead=60,newxreg=length(elec)+1:60)
pr <- pred$pred
uci <- pr+2*pred$se
lci <- pr-2*pred$se
ymin <- min(c(as.vector(lci),elec))-100
ymax <- max(c(as.vector(uci),elec))+100000
plot(elec,ylim=c(ymin,ymax),xlim=c(1972,2012),
     main='Prediction for Electricity Production')
lines(exp(pr),col=2,lwd=1.5)
lines(exp(lci),col=3)
lines(exp(uci),col=3)
plot(elec,ylim=c(ymin,ymax),xlim=c(2000,2012),
     main='Prediction for Electricity Production')
lines(exp(pr),col=2,lwd=2)
lines(exp(lci),col=3)
lines(exp(uci),col=3)