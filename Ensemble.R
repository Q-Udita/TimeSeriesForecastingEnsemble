library("forecast")
library("tseries")
library("TTR")

ensemble <- function(res,act,fit){
  MAD = 0
  for (x in res) {
    MAD = MAD + abs(x)
  }
  MAD = MAD / 3
  
  TS=0
  for (x in res) {
    TS=TS + x
  }
  TS = TS / MAD
  EC = 0
  correctedforecast = fit
  if(TS < -3 || TS >  3) {
    for (x in res) {
      EC = EC + x
    }
    EC = (EC / 3) * 100
    correctedforecast = fit + EC
  }
  
  E = ( correctedforecast - act ) / act
  return(E)
}

#*****Following is the time series data of sales of TMT Bars of 10mm diameter from a certain stockyard.********

sales <- c(2583.787,3102.35,2511.16,2324.134,2392.93,3196.865,2861.87,2261.08,3297.57,2743.33,2350.27,3686.508,3000.43,4018.094,3187.14,1950.017,3502.03,4030.21,3420.182,3188.731,3369.622,2827.065,2892.185,3927.877,2054.093,2975.29,2998.483,3408.44,3790.459,4253.33,3374.81,2399.403,2773.33,3240.64,4335.372,4646.409,2819.9,3109.69,3234.82,3929.464,3281.91,5876.29,3372.181,3392.775,3252.63,2582.19,3043.24,4432.95,1620.514,2644.22,3222.367,2846.888,3062.23,3490.414,1994.82,2970.78,2952.076,2808.53,2926.4,3976.92,2696.08)
salestimeseries <- ts(sales,frequency = 12, start = c(2013,4))
logsalestimeseries <- log(salestimeseries)
n <-length(sales)

#********* Exponential smoothing ***************

salestimeseriesforecast <- ets(logsalestimeseries,model = "ZAA", damped = NULL, alpha = NULL,beta = NULL, gamma = NULL)
plot(salestimeseriesforecast)
salestimeseriesforecast2<-forecast.ets(salestimeseriesforecast,h=5)
plot(forecast(salestimeseriesforecast2))
acf(salestimeseriesforecast2$residuals,lag.max = 20)
Box.test(salestimeseriesforecast2$residuals,lag = 20,type = "Ljung-Box")
plot.ts(salestimeseriesforecast2$residuals)
res1 <- c(salestimeseriesforecast$residuals[n-3],salestimeseriesforecast$residuals[n-2],salestimeseriesforecast$residuals[n-1])
act <- salestimeseriesforecast$x[n]
EF <- salestimeseriesforecast$fitted[n]
EE <- ensemble(res1, act, EF)

#********* ARIMA Model ****************

adf.test(logsalestimeseries, alternative = "stationary")
auto.arima(logsalestimeseries)
salestimeseriesarima <- arima(logsalestimeseries, order = c(1,0,0))
plot(forecast(salestimeseriesarima,h=5))
acf(forecast(salestimeseriesarima,h=5)$residuals, lag.max = 20)
Box.test(forecast(salestimeseriesarima,h=5)$residuals, lag = 20, type = "Ljung-Box")
plot.ts(forecast(salestimeseriesarima,h=5)$residuals)
res2 <- c(salestimeseriesarima$residuals[n-3],salestimeseriesarima$residuals[n-2],salestimeseriesarima$residuals[n-1])
AF <- fitted(salestimeseriesarima)[n]
AE <- ensemble(res2, act, AF)

#********* MA3 Model ******************

plot(logsalestimeseries)
sm <- ma(logsalestimeseries,order=3)
lines(sm,col="red")
res3 <- c(logsalestimeseries[n-3] - sm[n-3], logsalestimeseries[n-2] - sm[n-2], logsalestimeseries[n-1] - sm[n-1])
MF <- sm[n-1]
ME3 <- ensemble(res3, act, MF)

#**********Ensembled Forecast**********

EnsembleForecastMay = ( (1/AE)*AF + (10/EE)*EF + (25/ME3)*MF) / (1/AE + 10/EE + 25/ME3)
EnsembleForecastMay <- exp(EnsembleForecastMay)
EnsembleForecastMay
actualMay <- 3174.76
accuracy <- 100 - (abs(EnsembleForecastMay-actualMay)/actualMay*100)
accuracy














