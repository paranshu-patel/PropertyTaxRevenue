# make sure the extension of the file is .r
# Project R code
# Using necessary library 

library(forecast)

#Creating time series data set
setwd("/Users/Desktop/Project")

# Create data frame
project.data <- read.csv("Project.csv")

# See the first 6 records of the file.
head(project.data)
tail(project.data)

#Using ts() function to create time series dataset
statetax.ts <- ts(project.data$Value, 
                   start = c(2011, 1), end = c(2021, 4), freq = 4)
statetax.ts

## Use plot() to plot time series data  
plot(statetax.ts, 
     xlab = "Time", ylab = "Tax Revenue (Millions of Dollars)", 
     ylim = c(9000, 28000), xaxt = 'n',
     main = "State Tax Revenue of Property Tax")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(2011, 2023, 1), labels = format(seq(2011, 2023, 1)))

# Use stl() function to get time series components
statetax.stl <- stl(statetax.ts, s.window = "periodic")
autoplot(statetax.stl, main = "Property State Tax Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags.
autocor <- Acf(statetax.ts, lag.max = 8, 
               main = "Autocorrelation for Property Tax Revenue")

# Create Data partitions
# Total number of period length(ridership.ts) = 44.
# nvalid = 8 Quarters (2 years), from Quarter 1 2020 to Quarter 4 2021.
# nTrain = 36 Quarters (9 years), from Quarter 1 2011 to Quarter 4 2019.
nValid <- 8
nTrain <- length(statetax.ts) - nValid 
train.ts <- window(statetax.ts, start = c(2011, 1), end = c(2011, nTrain))
valid.ts <- window(statetax.ts, start = c(2011, nTrain + 1), 
                   end = c(2011, nTrain + nValid))
# Plot training and partition data set description in the data plot
plot(statetax.ts, 
     xlab = "Time", ylab = "Tax Revenue (Millions of Dollars)", 
     ylim = c(9000, 28000), xaxt = 'n',
     main = "State Tax Revenue of Property Tax")
axis(1, at = seq(2011, 2022, 1), labels = format(seq(2011, 2022, 1)))
# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(0, 25000))
lines(c(2022, 2022), c(0, 25000))
text(2014, 25500, "Training")
text(2021, 25500, "Validation")
arrows(2011, 24500, 2019.9, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 24500, 2021.9, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Random Walk check with approach 1 - Hypothesis Test
# Use Arima() function to fit AR(1) model for Tax Revenue.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
statetax.ar1<- Arima(statetax.ts, order = c(1,0,0), method="ML")
summary(statetax.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.6342
s.e. <- 0.0957
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

# Random Walk check with approach 2 - acf of differencing at lag 1.
# Create first difference of shipments data using diff() function.
diff.statetax <- diff(statetax.ts, lag = 1)
diff.statetax

# Use Acf() function to identify autocorrealtion for first differenced
# Tax Revenue and plot autocorrelation for different lags 
# (up to maximum of 8).
Acf(diff.statetax, lag.max = 8, 
    main = "Autocorrelation for Differenced Tax Revenue")

# From both approach it states that the data is predictable

## Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automatic selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Tax Revenue (Millions of Dollars)", ylim = c(9000, 28000), 
     bty = "l", xlim = c(2011, 2024), xaxt = "n",
     main = "Holt-Winter's Model with Automatic Selection of Model Options", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(2011, 2024, 1), labels = format(seq(2011, 2024, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(statetax.ts)
legend(2011,23000, 
       legend = c("Tax Revenue", 
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n", cex = 0.6)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(0, 25000))
lines(c(2022, 2022), c(0, 25000))
text(2014, 25500, "Training")
text(2021, 25500, "Validation")
text(2023, 25500, "Future")
arrows(2011, 24500, 2019.9, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 24500, 2021.9, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2022.1, 24500, 2024, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "AAA", i.e., Additive error, 
# Additive trend and additive seasonality for the training data.
hw.AAA <- ets(train.ts, model = "AAA")
hw.AAA 

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.AAA.pred <- forecast(hw.AAA, h = nValid, level = 0)
hw.AAA.pred

# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(hw.AAA.pred$mean, 
     xlab = "Time", ylab = "Tax Revenue (Millions of Dollars)", ylim = c(9000, 28000), 
     bty = "l", xlim = c(2011, 2024), xaxt = "n",
     main = "Holt-Winter's Model AAA Options", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(2011, 2024, 1), labels = format(seq(2011, 2024, 1)))
lines(hw.AAA.pred$fitted, col = "blue", lwd = 2)
lines(statetax.ts)
legend(2011,23000, 
       legend = c("Tax Revenue", 
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n", cex = 0.6)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(0, 25000))
lines(c(2022, 2022), c(0, 25000))
text(2014, 25500, "Training")
text(2021, 25500, "Validation")
text(2023, 25500, "Future")
arrows(2011, 24500, 2019.9, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 24500, 2021.9, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2022.1, 24500, 2024, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Using accuracy() function to measure performance.
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)
round(accuracy(hw.AAA.pred$mean, valid.ts), 3)

# Using Holt's Winter AAA model for the entire data set.
hw.AAA.all <- ets(statetax.ts, model = "AAA")
hw.AAA.all

# Using forecat() function to forecast future 8 periods
HW.AAA.all.pred <- forecast(hw.AAA.all, h = 8 , level = 0)
HW.AAA.all.pred

# Plot HW predictions for original data, AAA model with 
# optimal smoothing parameters for the entire dataset
plot(HW.AAA.all.pred$mean, 
     xlab = "Time", ylab = "Tax Revenue (Millions of Dollars)", ylim = c(9000, 28000), 
     bty = "l", xlim = c(2011, 2024), xaxt = "n",
     main = "Holt-Winter's AAA Model for Entire Dataset", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(2011, 2024, 1), labels = format(seq(2011, 2024, 1)))
lines(HW.AAA.all.pred$fitted, col = "blue", lwd = 2)
lines(statetax.ts)
legend(2011,23000, 
       legend = c("Tax Revenue", 
                  "Holt-Winter's Model for entire data",
                  "Holt-Winter's Model for Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n", cex = 0.6)

# Plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2022, 2022), c(0, 25000))
lines(c(2024, 2024), c(0, 25000))
text(2013.5, 25500, "Validation")
text(2023, 25500, "Future")
arrows(2011, 24500, 2022, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2022, 24500, 2024, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Using Accuracy() measure for the Holt's Winter AAA model
round(accuracy(HW.AAA.all.pred$fitted, statetax.ts), 3)

## Two-Level model with Regression with Linear trend and seasonality 
# + AR model for residuals.
# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonal model in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

# Plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Tax Revenue (Millions of Dollars)", ylim = c(12000, 25000), 
     bty = "l", xlim = c(2011, 2024), xaxt = "n",
     main = "Regression with Linear Trend and Seasonality", 
     lwd = 2, lty = 2, col = "blue") 
axis(1, at = seq(2011, 2024, 1), labels = format(seq(2011, 2024, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lwd = 2, lty = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2011,23000, legend = c("Prperty Tax Revenue Time Series",
                              "Regression for Training Data",
                             "Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n", cex = 0.6)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(0, 25000))
lines(c(2022, 2022), c(0, 25000))
text(2014, 25000, "Training")
text(2021, 25000, "Validation")
text(2023, 25000, "Future")
arrows(2011, 24500, 2019.9, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 24500, 2021.9, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2022.1, 24500, 2024, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Plot residuals of the predictions with trend and seasonality.
plot(train.lin.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-1500, 2500), bty = "l",
     xlim = c(2011, 2023), xaxt = "n",
     main = "Regresssion Residuals for Training and Validation Data", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2011, 2023, 1), labels = format(seq(2011, 2023, 1)))
lines(valid.ts - train.lin.season.pred$mean, col = "brown", lwd = 2, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(-2000, 2500))
lines(c(2022, 2022), c(-2000, 2500))
text(2014, 2500, "Training")
text(2021, 2500, "Validation")
text(2022.5, 2500, "Future")
arrows(2011, 2350, 2019.9, 2350, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 2350, 2021.9, 2350, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2022.1, 2350, 2024, 2350, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for the model residuals 
# and plot autocorrelation for different lags (up to maximum of 8).
Acf(train.lin.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation for Regression Model Training Residuals")

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(res.ar1)

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 8).
Acf(res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for AR(1) model for Regression Residuals")

# Use Arima() function to fit AR(2) model for training residuals. The Arima model of 
# order = c(2,0,0) gives an AR(2) model.
# Use summary() to identify parameters of AR(2) model. 
res.ar2 <- Arima(train.lin.season$residuals, order = c(2,0,0))
summary(res.ar2)

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 8).
Acf(res.ar2$residuals, lag.max = 8, 
    main = "Autocorrelation for AR(2) model for Regression Residuals")

# Use forecast() function to make prediction of residuals in validation set.
res.ar2.pred <- forecast(res.ar2, h = nValid, level = 0)
res.ar2.pred


# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(2) for residuals for validation period.
valid.two.level.pred <- train.lin.season.pred$mean + res.ar2.pred$mean

valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             res.ar2.pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Tax Revenue", "Reg.Forecast", 
                     "AR(2)Forecast", "Combined.Forecast")
valid.df

# Create two-level model regression with linear trend and seasonality 
# + AR(2) model for residualsb for the entire dataset.
# Use tslm() function to create linear trend and seasonality model.
lin.season <- tslm(statetax.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions with linear trend and seasonal 
# model into the future 8 quarters.  
lin.season.pred <- forecast(lin.season, h = 8, level = 0)

# Use Arima() function to fit AR(2) model for regression residuals.
# The ARIMA model order of order = c(2,0,0) gives an AR(2) model.
# Use forecast() function to make prediction of residuals into the future 8 quarters.
residual.ar2 <- Arima(lin.season$residuals, order = c(2,0,0))
summary(residual.ar2)

# Using forecast function to forecast AR(2) model residuals for the future 8 periods
residual.ar2.pred <- forecast(residual.ar2, h = 8, level = 0)

# Use Acf() function to identify autocorrelation for the residuals of residuals 
# and plot autocorrelation for different lags (up to maximum of 8).
Acf(residual.ar2$residuals, lag.max = 8, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# Identify forecast for the future 8 periods as sum of linear trend and 
# seasonal model and AR(1) model for residuals.
lin.season.ar2.pred <- lin.season.pred$mean + residual.ar2.pred$mean

# Create a data table with linear trend and seasonal forecast 
# for 8 future periods,
# AR(2) model for residuals for 8 future periods, and combined 
# two-level forecast for 8 future periods.
table.df <- round(data.frame(lin.season.pred$mean, 
                             residual.ar2.pred$mean, lin.season.ar2.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(2)Forecast","Combined.Forecast")
table.df

# Plot historical data, predictions for historical data, and forecast 
# for 8 future periods.
plot(statetax.ts, 
     xlab = "Time", ylab = "Tax Revenue (Millions of Dollars)", 
     ylim = c(9000, 28000), xaxt = "n",
     bty = "l", xlim = c(2011, 2025), lwd = 2,
     main = "Two-Level Forecast: Regression with Trend and Seasonlity + AR(2)
     for Residuals") 
axis(1, at = seq(2011, 2024, 1), labels = format(seq(2011, 2024, 1)))
lines(lin.season$fitted + residual.ar2$fitted, col = "blue", lwd = 2)
lines(lin.season.ar2.pred, col = "blue", lty = 5, lwd = 2)
legend(2011,22000, legend = c("Tax Revenue Series for entire dataset", 
                             "Two-Level Forecast for Training Periods", 
                             "Two-Level Forecast for 8 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n", cex = 0.6)

# plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2022, 2022), c(0, 28000))
lines(c(2024, 2024), c(0, 28000))
text(2013.5, 25500, "Training")
text(2023, 25500, "Future")
arrows(2011, 24500, 2021.9, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2022.1, 24500, 2024, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Using accuracy() function to get performance measure of the model.
round(accuracy(lin.season$fitted + residual.ar2$fitted, statetax.ts), 3)

## FIT AUTO ARIMA MODEL.
# Use auto.arima() function to fit ARIMA model for the training data.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for shipments with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 8, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Tax Revenue (Millions of Dollars)", 
     ylim = c(9000, 28000), xaxt = "n", 
     bty = "l", xlim = c(2011, 2024), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(2011, 2024, 1), labels = format(seq(2011, 2024, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2011,22000, legend = c("Ridership Time Series", 
                             "Auto ARIMA Forecast for Training Period",
                             "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n", cex = 0.6)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(0, 28000))
lines(c(2022, 2022), c(0, 28000))
text(2014, 25000, "Training")
text(2021, 25000, "Validation")
text(2023, 25000, "Future")
arrows(2011, 24000, 2019.9, 24000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 24000, 2021.9, 24000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2022.1, 24000, 2024, 24000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## FIT ARIMA(1,2,0)(1,0,1) MODEL.

# Use Arima() function to fit ARIMA(1,2,0)(1,0,1) model for 
# trend and seasonality.
# Use summary() to show ARIMA model and its parameters.
train.arima.seas <- Arima(train.ts, order = c(1,2,0), 
                          seasonal = c(1,0,1)) 
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

# Use Acf() function to create autocorrelation chart of ARIMA(1,2,0)(1,0,1) 
# model residuals.
Acf(train.arima.seas$residuals, lag.max = 8, 
    main = "Autocorrelations of ARIMA(1,2,0)(1,0,1) Model Residuals")

# Plot ts data, ARIMA model, and predictions for validation period.
plot(train.arima.seas.pred, 
     xlab = "Time", ylab = "Tax Revenue (Millions of Dollars)", 
     ylim = c(9000, 28000), xaxt = "n",
     bty = "l", xlim = c(2011, 2024), 
     main = "Seasonal ARIMA(1,2,0)(1,0,1)[4] Model", lwd = 2, flty = 5) 
axis(1, at = seq(2011, 2024, 1), labels = format(seq(2011, 2024, 1)))
lines(train.arima.seas.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2011,23500, legend = c("Property Tax Time Series", 
                             "Seasonal ARIMA Forecast for Training Period",
                             "Seasonal ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n", cex = 0.6)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(0, 28000))
lines(c(2022, 2022), c(0, 28000))
text(2013, 25000, "Training")
text(2021, 25000, "Validation")
text(2023, 25000, "Future")
arrows(2011, 24000, 2019.9, 24000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 24000, 2021.9, 24000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2022.1, 24000, 2024, 24000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuaracy() function to compare performance measures
round(accuracy(train.arima.seas.pred$mean, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

# Use arima() function to fit seasonal ARIMA(1,2,0)(1,0,1) model 
# for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
arima.seas <- Arima(statetax.ts, order = c(1,2,0), 
                    seasonal = c(1,0,1)) 
summary(arima.seas)

# Apply forecast() function to make predictions for shipments with 
# seasonal ARIMA model for the future 8 periods. 
arima.seas.pred <- forecast(arima.seas, h = 8, level = 0)
arima.seas.pred

# Use Acf() function to create autocorrelation chart of ARIMA(1,2,0)(1,0,1) 
# model residuals.
Acf(arima.seas$residuals, lag.max = 8, 
    main = "Autocorrelations of ARIMA(1,2,0)(1,0,1) Model Residuals")

# Plot ts data, ARIMA model, and predictions for future periods.
plot(statetax.ts, 
     xlab = "Time", ylab = "Tax Revenue (millions of Dollars)", 
     ylim = c(9000, 28000), xaxt = "n",
     bty = "l", xlim = c(2011, 2025), lwd = 2,
     main = "Seasonal ARIMA(1,2,0)(1,0,1)[4] Model for Entire Data Set") 
axis(1, at = seq(2011, 2025, 1), labels = format(seq(2011, 2025, 1)))
lines(arima.seas$fitted, col = "blue", lwd = 2)
lines(arima.seas.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2011,20000, legend = c("Property Tax Series", 
                             "Seasonal ARIMA Forecast", 
                             "Seasonal ARIMA Forecast for 8 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n", cex = 0.6)
# Plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2022, 2022), c(0, 28000))
lines(c(2024, 2024), c(0, 28000))
text(2015, 25500, "Training")
text(2023, 25500, "Future")
arrows(2011, 24500, 2021.9, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2022.1, 24500, 2024, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuaracy() function to compare performance measures
round(accuracy(arima.seas.pred$fitted, statetax.ts), 3)

# Comparing all the model's performance using accuracy() function
# (1) Naive forecast.
# (2) Seasonal naive forecast, and
# (3) Holt's Winter AAA model
# (4) Two-level Regression Model with Linear trend and seasonality + AR(2) model for residuals
# (5) Seasonal ARIMA (1,2,0)(1,0,1) Model,
round(accuracy((naive(statetax.ts))$fitted, statetax.ts), 3)
round(accuracy((snaive(statetax.ts))$fitted, statetax.ts), 3)
round(accuracy(HW.AAA.all.pred$fitted, statetax.ts), 3)
round(accuracy(lin.season$fitted + residual.ar2$fitted, statetax.ts), 3)
round(accuracy(arima.seas.pred$fitted, statetax.ts), 3)

# Taken from analysis from above
# Forecast using Seasonal ARIMA(1,2,0)(1,0,1)[4] model for the future 8 periods
arima.seas.pred <- forecast(arima.seas, h = 8, level = 0)
arima.seas.pred

# Plot ts data, ARIMA model, and predictions for future periods.
plot(statetax.ts, 
     xlab = "Time", ylab = "Tax Revenue (millions of Dollars)", 
     ylim = c(9000, 28000), xaxt = "n",
     bty = "l", xlim = c(2011, 2025), lwd = 2,
     main = "Seasonal ARIMA(1,2,0)(1,0,1)[4] Model for Entire Data Set") 
axis(1, at = seq(2011, 2025, 1), labels = format(seq(2011, 2025, 1)))
lines(arima.seas$fitted, col = "blue", lwd = 2)
lines(arima.seas.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2011,20000, legend = c("Property Tax Series", 
                              "Seasonal ARIMA Forecast", 
                              "Seasonal ARIMA Forecast for 8 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n", cex = 0.6)
# Plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2022, 2022), c(0, 28000))
lines(c(2024, 2024), c(0, 28000))
text(2015, 25500, "Training")
text(2023, 25500, "Future")
arrows(2011, 24500, 2021.9, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2022.1, 24500, 2024, 24500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
