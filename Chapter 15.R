###### Chapter 15 Time series
####
# In a cross-sectional dataset, variables are measured at a single point in time.
# In contrast, longitudinal data involves measuring variables repeatedly over time. 
# Studies of time-series data involve two fundamental questions: 
# what happened (description), and 
# what will happen next (forecasting)?
# forecasting techniques called autoregressive integrated moving averages (ARIMA) 
#     models that use correlations among recent data points and among recent 
#     prediction errors to make future forecasts. Throughout, we’ll consider 
#     methods of evaluating the fit of models and the accuracy.
####
sales <- c(18, 33, 41, 7, 34, 35, 24, 25, 24, 21, 25, 20, 
           22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)
ts <- ts(sales, start = c(2014, 1), frequency = 12) # when frequency is 4, that means
#     quarterly, when frequency is 12 that means monthly, when frequency is  1, 
#     that means yearly
#
plot(ts, type = "o", pch = 17)
#
window(ts, start = c(2014, 3), end = c(2015, 7))
####
# Time series typically have a significant irregular or error component. In 
#     order to discern any patterns in the data, you’ll frequently want to plot 
#     a smoothed curve that damps down these fluctuations. 
# One of the simplest methods of smoothing a time series is to use simple moving 
#     averages. 
# For example, each data point can be replaced with the mean of that observation 
#     and one observation before and after it. This is called a centered moving 
#     average. A centered moving average is defined as 
#     St = (Yt-q + … + Yt + … + Yt+q) / (2q + 1)
#     where St is the smoothed value at time t and k = 2q + 1 is the number of 
#     observations that are averaged. The k value is usually chosen to be an 
#     odd number (3 in this example). 
# By necessity, when using a centered moving average, you lose the (k – 1) / 2
#     observations at each end of the series.
require("forecast")
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
#
ylim <- c(min(Nile), max(Nile))
plot(Nile, main = "raw time series")
plot(ma(Nile, 3), main = "simple moving average with k = 3",
     ylim = ylim)
plot(ma(Nile, 7), main = "simple moving average with k = 7",
     ylim = ylim)
plot(ma(Nile, 15), main = "simple moving average with k = 15",
     ylim = ylim)
par(opar)
# As k increases, the plot becomes increasingly smoothed. The 
#     challenge is to find the value of k that highlights the
#     major patterns in the data, without under- or over smoothing.
# This is more art than science, and you’ll probably want to try 
# several values of k before settling on one. 
####
# For time-series data with a periodicity greater than one (that 
#     is, with a seasonal component), you’ll want to go beyond a 
#     description of the overall trend. 
# Seasonal decomposition can be used to examine both seasonal and 
#     general trends.
####
# Time-series data that have a seasonal aspect (such as monthly or
#     quarterly data) can be decomposed into a trend component, a 
#     seasonal component, and an irregular component. 
# The trend component captures changes in level over time. 
# The seasonal component captures cyclical effects due to the 
#     time of year.
# The irregular (or error) component captures those influences not
#     described by the trend and seasonal effects.
####
# The decomposition can be additive or multiplicative. 
# In an additive model, the components sum to give the values of 
#     the time series. Specifically,
#     Yt = Trendt + Seasonalt + Irregulart
#     where the observation at time t is the sum of the 
#     contributions of the trend at time t,
#     the seasonal effect at time t, and an irregular effect at 
#     time t. 
# In a multiplicative model, given by the equation
#     Yt = Trendt * Seasonalt * Irregulart
#     the trend, seasonal, and irregular influences are multiplied
####
# Notice how the variability is proportional to the level: as the 
#     level increases, so does the variability. This amplification
#     (or possible damping) based on the current level of the 
#     series strongly suggests a multiplicative model.
# Consider a time series that records the monthly sales of 
#     motorcycles over a 10-year period. In a model with an 
#     additive seasonal effect, the number of motorcycles sold 
#     tends to increase by 500 in November and December (due to 
#     the Christmas rush) and decrease by 200 in January (when 
#     sales tend to be down). 
## The seasonal increase or decrease is independent of the 
#     current sales volume. 
# In a model with a multiplicative seasonal effect, motorcycle 
#     sales in November and December tend to increase by 20% and 
#     decrease in January by 10%. 
## In the multiplicative case, the impact of the seasonal effect 
#     is proportional to the current sales volume.
# This isn’t the case in an additive model. In many instances, 
#     the multiplicative model is more realistic.
####
# A popular method for decomposing a time series into trend, 
#     seasonal, and irregular components is seasonal 
#     decomposition by loess smoothing.
####
# The stl() function can only handle additive models, but this 
#     isn’t a serious limitation. Multiplicative models can be 
#     transformed into additive models using a log transformation: 
#     log(Yt) = log(Trendt * Seasonalt * Irregulart) 
#             = log(Trendt) + log(Seasonalt) + log(Irregulart)
####
t <- AirPassengers
plot(t) # From the graph, it appears that variability of the 
#     series increases with the level, suggesting a multiplicative 
#     model. 
logt <- log(t)
plot(logt, ylab = "log(AirPassengers)") # The variance has 
#     stabilized, and the logged series looks like an appropriate
#     candidate for an additive decomposition. 
fit <- stl(logt, s.window = "periodic") # set s.window to 
#     "periodic" is to force seasonal effects to be identical across
#     years.
plot(fit) #  Note that the seasonal components have been constrained to
#     remain the same across each year (using the s.window="period" option). 
#     The trend is monotonically increasing, and the seasonal effect suggests
#     more passengers in the summer (perhaps during vacations). 
# The grey bars on the right are magnitude guides, each bar represents the
#     same magnitude. This is useful because the y-axes are different for each 
#     graph.
head(fit$time.series, 3)
fitback <- xts::as.xts(exp(fit$time.series)) # converts the decomposition back to the original 
# metric. 
seasonly <- xts::apply.yearly(fitback$seasonal, func)
trend <- xts::apply.yearly(fitback$trend, func)
irregular <- xts::apply.yearly(fitback$remainder, func)
fit.year <- merge(seasonly, trend, irregular)
####
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 1))
monthplot(t, xlab = "", ylab = "") # displays the subseries for each month 
#     (all January values connected), along with the average of each
#     subseries. 
forecast::seasonplot(t, year.labels = TRUE, main = "") # displays the 
#     subseries by year
par(opar)
####
# Exponential models are some of the most popular approaches to forecasting 
#     the future values of a time series. They’re simpler than many other 
#     types of models, but they can yield good short-term predictions in a
#     wide range of applications. 
# They differ from each other in the components of the time series that are 
#     modeled. 
# A simple exponential model (also called a single exponential model) fits a
#     time series that has a constant level and an irregular component at 
#     time i but has neither a trend nor a seasonal component.
# A double exponential model (also called a Holt exponential smoothing)
#     fits a time series with both a level and a trend.
# Finally, a triple exponential model (also called a Holt-Winters
#     exponential smoothing) fits a time series with level, trend, and sea-
#     sonal components. 
####
# Allowable letters are A for additive, M for multiplicative, N for none, 
#     and Z for automatically selected.
####
# Simple exponential smoothing uses a weighted average of existing 
#     time-series values to make a short-term prediction of future values. 
# The weights are chosen so that observations have an exponentially 
#     decreasing impact on the average as you go back in time. 
# The simple exponential smoothing model assumes that an observation in the 
#     time series can be described by
#     Yt = level + irregulart
# The prediction at time Yt+1 (called the 1-step ahead forecast) is written 
#     as Yt+1 = c0Yt + c1Yt−1 + c2Yt−2 + c2Yt−2 + ...
#     where ci = α(1−α)i, i = 0, 1, 2, ... and 0≤α≤1. The ci weights sum to 
#     one, and the 1-step ahead forecast can be seen to be a weighted average
#     of the current value and all past values of the time series. T
# The alpha (α) parameter controls the rate of decay for the weights. The 
#     closer alpha is to 1, the more weight is given to recent observations.
# The closer alpha is to 0, the more weight is given to past observations. 
#     The actual value of alpha is usually chosen by computer in order to 
#     optimize a fit criterion. 
# A common fit criterion is the sum of squared errors between the actual and
#     predicted values. 
####
plot(nhtemp) # There is no obvious trend, and the yearly data lack a seasonal
#     component, so the simple exponential model is a reasonable place to 
#     start. 
forecast::ses(nhtemp) # short cut
fit <- forecast::ets(nhtemp, model = "ANN")
fit # The relatively low value of alpha (0.18) indicates that distant as well
#     as recent observations are being considered in the forecast. This
#     value is automatically chosen to maximize the fit of the model to the 
#     given dataset.
forecast::forecast(fit, 1) # is used to predict the time series k steps into the 
#     future
plot(forecast::forecast(fit, 1), xlab = "year", 
     ylab = expression(paste("Temperature(", degree * F, ")")),
     main = "new haven annual mean temperature") #  the forecasted
#     value, and the 80% and 95% confidence intervals plotted
####
forecast::accuracy(fit) #  The et represent the error or irregular component of each
#     observation.
# Mean error ME mean( et ) 
# Root mean squared error RMSE sqrt( mean( et2 ) )
# Mean absolute error MAE mean( | et | )
# Mean percentage error MPE mean( 100 * et / Yt )
# Mean absolute percentage error MAPE mean( | 100 * et / Yt | )
# Mean absolute scaled error MASE mean( | qt | ) where 
#     qt = et / ( 1/(T-1) * sum( | yt – yt-1| ) ), T is the number 
#     of observations, and the sum goes from t=2 to t=T
# The mean error and mean percentage error may not be that useful, because
#     positive and negative errors can cancel out. 
# The RMSE gives the square root of the mean square error, which in this 
#     case is 1.13°F. 
# The mean absolute percentage error reports the error as a percentage of 
#     the time-series values. It’s unit-less and can be used to compare 
#     prediction accuracy across time series. But it assumes a measurement
#     scale with a true zero point (for example, number of passengers per day)
#     . Because the Fahrenheit scale has no true zero, you can’t use it here. The mean absolute scaled error is
# the most recent accuracy measure and is used to compare the forecast 
#     accuracy across time series on different scales. There is no one 
#     best measure of predictive accuracy.
# The RMSE is certainly the best known and often cited
####
# The Holt exponential smoothing approach can fit a time series that has an 
#     overall level and a trend (slope). The model for an observation at 
#     time t is Yt = level + slope*t + irregulart
# An alpha smoothing parameter controls the exponential decay for the level, 
#     and a beta smoothing parameter controls the exponential decay for the 
#     slope. 
# Again, each parameter ranges from 0 to 1, with larger values giving more 
#     weight to recent observations.
# The Holt-Winters exponential smoothing approach can be used to fit a time 
#     series that has an overall level, a trend, and a seasonal component. 
#   Here, the model is 
#     Yt = level + slope*t + st + irregulart
#     where st represents the seasonal influence at time t. In addition to 
#     alpha and beta parameters, a gamma smoothing parameter controls the 
#     exponential decay of the seasonal component. 
# Like the others, it ranges from 0 to 1, and larger values give more
#     weight to recent observations in calculating the seasonal effect.
t <- AirPassengers 
plot(t) # seasonal, trend, level
#
fit.short <- forecast::hw(log(t))
summary(fit.short)
#
fit <- forecast::ets(log(t), model = "AAA")
fit # output much more cleaner
#
forecast::accuracy(fit)
#
pred <- predict(fit, 5)
plot(pred, main = "forecast for air trevel",
     ylab = "log(AirPassengers)", xlab = "time")
#
pred$mean <- exp(pred$mean)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
p <- cbind(pred$mean, pred$lower, pred$upper)
dimnames(p)[[2]] <- c("mean", "Lo 80", "Lo 95", "Hi 80", "Hi 95")
p
# alpha = level; beta = trend; gamma = seasonal
# The low value for the trend (.0004) doesn’t mean there is no slope; 
#     it indicates that the slope estimated from early observations didn’t 
#     need to be updated.
####
fit <- forecast::ets(t, model = "MAM") # multiplicative model to the original 
#     data
fit.short <- forecast::hw(t, seasonal = "multiplicative")
summary(fit.short) 
#
forecast::accuracy(fit)
####
#  The ets() function can also fit a damping component. Time-series 
#     predictions often assume that a trend will continue up forever 
#     (housing market, anyone?). 
# A damping component forces the trend to a horizontal asymptote over a 
#     period of time.
# In many cases, a damped model makes more realistic predictions. 
# Finally, you can invoke the ets() function to automatically select a best-fitting
#     model for the data. 
forecast::ets(JohnsonJohnson)
fit <- .Last.value
#
plot(forecast::forecast(fit), main = "johnson & johnson forecast",
     ylab = "quarterly earnings (dollars)", xlab = "time", 
     flty = 2) 
# Because no model is specified, the software performs a search over a wide
#     array of models to find one that minimizes the fit criterion 
#     (log-likelihood by default). 
# The selected model is one that has multiplicative trend, seasonal, and 
#     error components.
# The plot, along with forecasts for the next eight quarters 
#     (the default in this case).
####
# In the autoregressive integrated moving average (ARIMA) approach to 
#     forecasting, predicted values are a linear function of recent actual
#     values and recent errors of prediction (residuals)
####
# lag: shift it back by a given number of observations
# autocorrelation: measures the way observations in a ts relate to each other,
#     ACk is the correlation between a set of observations (Yt) and 
#     observations k periods earlier (Yt-k). So AC1 is the correlation 
#     between the Lag 1 and Lag 0 time series, AC2 is the correlation between
#     the Lag 2 and Lag 0 time series, and so on
# ACF (autocorrelation function) plot : plotting the correlations (AC1, AC2, ...,
#     ACk). it is used to select appropriate paramters for the ARIMA model and
#     to assess the fit of the final model
# partial autocorrelation: the correlation between Yt and Yt-k with the effects
#     of all Y values between the two (Yt-1, Yt-2, ..., Tk-1) removed.
# PACF plot: also used to determine the most apprproate parameters for the 
#     ARIMA model
# stationary ts: the statistical properties of the series do not change over
#     time, e.g., the mean and variance of Yt are constant, while the
#     autocorrelations for any lag k don't change with time
# ARIMA model is designed for stationary ts
# therefore, transform the values of the a ts in order to achieve the constant
#     variance to fit the model. using log transformation, Box-cox 
#     transformation
# Because stationary time series are assumed to have constant means, they 
#     can’t have a trend component. Many non-stationary time series can
#     be made stationary through differencing.
#  In differencing, each value of a time series Yt is replaced with 
#     Yt-1 – Yt. Differencing a time series once removes a linear trend. 
#     Differencing it a second time removes a quadratic trend. A third time
#     removes a cubic trend. It’s rarely necessary to difference more than '
#     twice. s
# If there are trends, the data are differenced. You can also use a 
#     statistical procedure called the Augmented Dickey-Fuller (ADF) test
#     to evaluate the assumption of stationarity. 
####
# To summarize, ACF and PCF plots are used to determine the parameters of 
#     ARIMA models. 
# Stationarity is an important assumption, and transformations (constant
#     variance) and differening (stationary) are used to help achieve 
#     stationarity. 
# autoregressive (AR) component, a moving averages (MA) component, or both 
#     components (ARMA). 
# Finally, we’ll examine ARIMA models that include ARMA components and 
#     differencing to achieve stationarity (Integration).
####
# In an autoregressive model of order p, each value in a time series is 
#     predicted from a linear combination of the previous p values
#     AR(p):Yt = μ + β1Yt−1 + β2Yt−2 + ... + β pYt−p + ε t
#     where Yt is a given value of the series, µ is the mean of the series, 
#     the β s are the weights, and ε t is the irregular component.
# In a moving average model of order q, each value in the time series is 
#     predicted from a linear combination of q previous errors. 
# In this case MA(q):Yt = μ − θ1ε t−1 − θ2ε t−2 ... − θqε t−q + ε t
#     where the ε s are the errors of prediction and the θ s are the weights.
#     (It’s important to note that the moving averages described here aren’t
#     the simple moving averages)
# Combining the two approaches yields an ARMA(p, q) model of the form
#     Yt = μ + β1Yt−1 + β2Yt−2 + ... + β pYt−p − θ1ε t−1 − θ2ε t−2 ... − θqε t−q + ε t
#     that predicts each value of the time series from the past p values and 
#     q residuals. 
# An ARIMA(p, d, q) model is a model in which the time series has been 
#     differenced d times, and the resulting values are predicted from the 
#     previous p actual values and q previous errors. The predictions are 
#     “un-differenced” or integrated to achieve the final prediction.
####
# The steps in ARIMA modeling are as follows:
# 1 Ensure that the time series is stationary.
# 2 Identify a reasonable model or models (possible values of p and q).
# 3 Fit the model.
# 4 Evaluate the model’s fit, including statistical assumptions and
#     predictive accuracy.
# 5 Make forecasts.
####
require("forecast")
require("tseries")
## First you plot the time series and assess its stationarity 
plot(Nile)
ndiffs(Nile) # functions to give the number of times for differencing
#
dNile <- diff(Nile) # defualt differentiates time is 1.
plot(dNile) # removing the trend
#
adf.test(dNile) # check wether is stationary or not
## Second, indentifying one or more reasonable modles
Acf(dNile) # one large autocorrelation at lag 1
Pacf(dNile) # partial autocorrelations trail off to zero as the lags get 
#     bigger
# You get p and q by comparing the ACF and PACF plots with the
#     guidelines
## Third, Fitting the model
fit <- arima(Nile, order = c(p = 0, d = 1, q = 1))
fit # the coefficient for the moving average (-0.7329) is provided with AIC
# smaller AIC suggets better model
accuracy(fit) # here, the mean absolute percent error is 12.94%.
## Forth, evaluating
# If the model is appropriate, the residuals should be normally distributed 
#     with mean zero, and the autocorrelations should be zero for every
#     possible lag.
# in other words, the residuals should be normally and independently
#     distributed
qqnorm(fit$residuals)
qqline(fit$residuals) # Normally distributed data should fall along the line. 
Box.test(fit$residuals, type = "Ljung-Box") # provides a test that the 
#     autocorrelations are all zero
## Fifth, making forecasts
forecast(fit, 3)
plot(forecast(fit, 3), xlab = "year", ylab = "Annual Flow")
####
fit <- auto.arima(sunspots)
fit # These are values that minimize the AIC criterion over a large
#     number of possible models. 
accuracy(fit)
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type = "Ljung-Box") # independent
plot(forecast(fit, 3))
####












