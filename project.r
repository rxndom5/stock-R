# Load required libraries
library(quantmod)
library(forecast)

# Fetch stock data for AAPL from Yahoo Finance
getSymbols('AAPL', from = '2019-01-01', to = '2021-01-01')

# View the data
View(AAPL)

# Plot stock prices and Bollinger Bands
chartSeries(AAPL, subset = 'last 6 months', type = 'auto')
addBBands()

# Extract relevant columns
Open_prices <- AAPL[, "AAPL.Open"]
High_prices <- AAPL[, "AAPL.High"]
Low_prices <- AAPL[, "AAPL.Low"]
Close_prices <- AAPL[, "AAPL.Close"]
Volume_prices <- AAPL[, "AAPL.Volume"]
Adjusted_prices <- AAPL[, "AAPL.Adjusted"]

# Plot individual stock prices
par(mfrow = c(2, 3))
plot(Open_prices, main = 'Opening Price')
plot(High_prices, main = 'Highest Price')
plot(Low_prices, main = 'Lowest Price')
plot(Close_prices, main = 'Closing Price')
plot(Volume_prices, main = 'Volume')
plot(Adjusted_prices, main = 'Adjusted Price')

# Predict stock returns
Predic_Price <- Adjusted_prices
return_AAPL <- 100 * diff(log(Predic_Price))

# Train-test split
train_size <- 0.9
train_index <- floor(train_size * length(return_AAPL))
AAPL_return_train <- return_AAPL[1:train_index]
AAPL_return_test <- return_AAPL[(train_index + 1):length(return_AAPL)]

# ARIMA modeling
fit <- auto.arima(AAPL_return_train, seasonal = FALSE)
preds <- forecast(fit, h = length(AAPL_return_test))$mean

# Evaluate accuracy
accuracy(preds, AAPL_return_test)

# Plot forecast
par(mfrow = c(1, 1))
plot(forecast(fit, h = 15), main = "ARIMA Forecast for Apple Stock")
