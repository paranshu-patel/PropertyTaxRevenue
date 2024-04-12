# PropertyTaxRevenue
A Tim-Series Analysis on Property Tax collected by states to predict the trend in tax collection and analyze changes in amount received to plan for budgets and investments. The analysis is done using statistical R language.

The R code file conatins all the code related to the Project. 

# Summary
For this project, data from the United States Census Bureau has been used to predict quarterly
property state tax revenue for the upcoming 2 fiscal year. The analysis is based on time series data
of 11 years. From the visualization we identified that the dataset has an upward trend and
seasonality with the low tax revenue at the second quarter and high tax revenue at the third quarter
of each year. Also, the data is autocorrelated, as all the autocorrelation coefficient in all 8 lags are
statistically significant.
Advanced exponential smoothing models, Two-level regression model with AR residuals and,
Autoregressive integrated moving average (ARIMA) model were utilized for this project.
Additional variations of the Holt’s Winter Model, regression with linear trend and seasonality +
AR residual model and ARIMA model were also constructed to ensure good results. The Holt’s
Winter model was enhanced with pre-defined model type inspired from automatic selection of
model parameters. The same enhancements were made for two-level model with AR(2) for
residuals and pre-defined seasonal ARIMA model inspired from automatic selection of parameter
by auto ARIMA. Model evaluation was based on the RMSE and MAPE accuracy metrics. The
best model for forecasting was the seasonal ARIMA(1,2,0)(1,0,1)[4] model.

# Introduction
State tax has revenues generated by different types of tax collected such as corporate tax,
individual tax, motor tax, etc. Property tax almost consists of 31.7 % of the total tax collected
which is the single largest source. Although the property tax is collected by local government it
is a part of state and local tax revenue by US Census Bureau. This revenue generated from the
tax collected is used for city developments and future budget creation for the development.
Employment for government officers is also created using this revenue. SO according to the
taxes collected and budget created the developments funds are used. As a highly generated tax
revenue it is considered as an integral part of state and local government revenue generation.
As further development plans are always based on budget and revenue generation having future
insights into tax revenue will help in making decisions for future development and city plans.
This can provide the government to plan for future employment roles and hire more people to
perform tasks for efficient development. As property tax is the highest revenue of all taxes
having a forecast for future taxes will prove to be more resourceful for budgeting. The scope of
this project is to forecast future property tax revenue for different quarters to stay a step ahead of
development plans and create more employment according to the revenue. Also, this forecast
will help more in finding trends and changes into the revenue for future planning and budgeting.

# Goal 
The goal of this project is to create forecast for upcoming 8 quarters i.e., 2 years of the revenue
generated by property tax from state and local governments. The objective is to create a
predictive model which properly considers both trend and seasonality components of the
historical data and effectively forecast the future periods. Since the data of the quarter will be
available every 3 months, the forecasting model should be reevaluated semi-annually so it may
utilize two new quarters when forecasting into the future. The resulting forecast will be used to
monitor property tax revenues in billions of dollars.

# Data Source
This project use data provided by the United States Census Bureau recording the quarterly
tax revenue for property tax in billions of dollars by state and local governments. The time-period
used for dataset ranges from 1992 to 2023 on quarterly basis but for the purpose of this project the
dataset with timespan of 2011 to 2021.

# Results 
The best model to forecast the future periods for the time series data is Seasonal
ARIMA(1,2,0)(1,0,1) model which gives us the best predictions. Although the other computed
models still perform good for the dataset. The forecast shows us a further increase in the
revenue generated by state and local governments through property tax and will be helpful for
the creation of future development plans and future budget for the same. This analysis will be
helpful for the responsible authority to make startegic decisions in the future.

The future forecast :
![Screenshot 2024-04-12 at 4 50 57 PM](https://github.com/paranshu-patel/PropertyTaxRevenue/assets/166874008/48a0b33b-fbb0-49ad-bbe5-44f8f205f474)
