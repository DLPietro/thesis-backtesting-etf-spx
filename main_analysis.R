# -------------------------------------------------------------------------
# main_analysis.R
# Empirical Comparison: S&P 500, iShares Core S&P 500 ETF (IVV), and Fidelity Contrafund (FCNTX)
# Master's Thesis in Quantitative Finance - Pietro Di Leo
# GitHub: github.com/pietrodileo/thesis-backtesting-etf-spx
# -------------------------------------------------------------------------

# STEP 1: INSTALL AND LOAD REQUIRED PACKAGES
# Run this block only once to install packages. Comment out after installation.

# Install packages (uncomment if needed)
# install.packages(c("quantmod", "xts", "zoo", "dplyr", "ggplot2", "forecast", 
#                    "PerformanceAnalytics", "rugarch", "TTR", "kableExtra", "knitr",
#                    "lmtest", "sandwich", "timeSeries"))

# Load required libraries
library(quantmod)           # Financial data import and technical analysis
library(xts)                # Extensible time series management
library(zoo)                # Time series functions
library(dplyr)              # Data manipulation
library(ggplot2)            # Data visualization
library(forecast)           # Forecasting models (ARIMA, etc.)
library(PerformanceAnalytics) # Financial performance metrics
library(rugarch)            # GARCH volatility modeling
library(TTR)                # Technical trading rules
library(kableExtra)         # Enhanced table formatting
library(knitr)              # Report generation
library(lmtest)             # Hypothesis testing (e.g., White test)
library(sandwich)           # Robust standard errors
library(timeSeries)         # Financial time series analysis

# STEP 2: SET PARAMETERS AND LOAD MARKET DATA
# Define time period and risk-free rate
start_date <- "2013-01-01"
end_date <- "2023-12-31"
risk_free_rate_annual <- 0.0289  # 2.89% annual risk-free rate

# Download S&P 500 index data
getSymbols("^GSPC", from = start_date, to = end_date)
head(GSPC)
tail(GSPC)

# Plot closing prices
chartSeries(Cl(GSPC), theme = chartTheme("white"), name = "S&P 500 Closing Prices")

# STEP 3: CALCULATE KEY METRICS FOR S&P 500
# Compute log returns
log_returns <- diff(log(Cl(GSPC)))
log_returns <- na.omit(log_returns)

# Annualized return
annual_return <- mean(log_returns) * 252
cat("Annualized Return (S&P 500):", round(annual_return * 100, 2), "%\n")

# Volatility (annualized standard deviation)
volatility <- sd(log_returns) * sqrt(252)
cat("Annualized Volatility (S&P 500):", round(volatility * 100, 2), "%\n")

# Maximum drawdown
max_drawdown <- maxDrawdown(log_returns)
cat("Maximum Drawdown (S&P 500):", round(max_drawdown * 100, 2), "%\n")

# Sharpe Ratio (annualized)
sharpe_ratio <- (annual_return - risk_free_rate_annual) / volatility
cat("Sharpe Ratio (S&P 500):", round(sharpe_ratio, 2), "\n")

# Create summary table
metrics <- c("Total Log Return", "Annualized Return", "Volatility", "Max Drawdown", "Sharpe Ratio")
values <- c(
  percent(sum(log_returns)),
  percent(annual_return),
  percent(volatility),
  percent(max_drawdown),
  round(sharpe_ratio, 2)
)
summary_df <- data.frame(Metric = metrics, Value = values)
kable(summary_df, format = "html") %>%
  kable_styling(bootstrap_options = "basic", full_width = FALSE)

# STEP 4: MULTIVARIATE ANALYSIS AND SEASONALITY
# Decompose time series to analyze trend, seasonality, and residuals
price_ts <- ts(Cl(GSPC), frequency = 12)
decomposition <- stl(price_ts, s.window = "periodic")

# Plot decomposition
plot(decomposition, main = "S&P 500 Time Series Decomposition")

# Extract seasonal component and analyze monthly patterns
seasonal_data <- data.frame(
  Date = index(GSPC),
  Price = coredata(Cl(GSPC)),
  Seasonal = coredata(decomposition$seasonal)
)
seasonal_data$Year <- as.numeric(format(seasonal_data$Date, "%Y"))
seasonal_data$Month <- format(seasonal_data$Date, "%B")

# Group by month and compute average seasonal effect
monthly_means <- seasonal_data %>%
  group_by(Month) %>%
  summarise(Mean_Seasonal = mean(Seasonal), .groups = 'drop')

# Plot seasonality
ggplot(monthly_means, aes(x = Month, y = Mean_Seasonal, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Monthly Seasonal Effect", y = "Seasonal Component") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# STEP 5: RESIDUAL ANALYSIS
# Extract residuals from decomposition
residuals <- decomposition$random
residuals <- na.omit(residuals)

# Plot residuals over time
plot(residuals, type = "l", main = "Residuals Over Time")

# Scatter plot of residuals vs predicted values
predicted_values <- decomposition$trend + decomposition$seasonal
plot(predicted_values, residuals, main = "Residuals vs Predicted Values",
     xlab = "Predicted Values", ylab = "Residuals", pch = 19, col = "black")
abline(h = 0, col = "black")

# Summary statistics of residuals
mean_residuals <- mean(residuals)
sd_residuals <- sd(residuals)
cat("Mean of Residuals:", round(mean_residuals, 4), "\n")
cat("Standard Deviation of Residuals:", round(sd_residuals, 4), "\n")

# Normality test (Shapiro-Wilk)
shapiro_test <- shapiro.test(residuals)
cat("Shapiro-Wilk Test p-value:", shapiro_test$p.value, "\n")

# Ljung-Box test for autocorrelation
Box.test(residuals, lag = 20, type = "Ljung-Box")

# STEP 6: ARIMA MODELING AND FORECASTING
# Fit ARIMA model
arma_model <- auto.arima(log_returns)
summary(arma_model)

# Check residuals
checkresiduals(arma_model)

# Forecast next 30 days
arma_forecast <- forecast(arma_model, h = 30)
plot(arma_forecast, main = "ARIMA Forecast of S&P 500 Returns")

# STEP 7: COMPARE ETF AND MUTUAL FUND
# Download IVV (iShares S&P 500 ETF) and FCNTX (Fidelity Contrafund)
getSymbols("IVV", from = start_date, to = end_date)
getSymbols("FCNTX", from = start_date, to = end_date)

# Extract closing prices
ivv_price <- Cl(IVV)
fcntx_price <- Cl(FCNTX)
sp500_price <- Cl(GSPC)

# Combine into one data frame
price_data <- merge(ivv_price, fcntx_price, sp500_price)
colnames(price_data) <- c("IVV", "FCNTX", "GSPC")

# Plot comparison
ggplot(data = price_data, aes(x = index(price_data))) +
  geom_line(aes(y = IVV, color = "IVV"), size = 1) +
  geom_line(aes(y = FCNTX, color = "FCNTX"), size = 1) +
  geom_line(aes(y = GSPC, color = "GSPC"), size = 1) +
  labs(title = "IVV, FCNTX, and S&P 500 Price Comparison", 
       x = "Date", y = "Closing Price") +
  scale_color_manual(values = c("IVV" = "blue", "FCNTX" = "red", "GSPC" = "green")) +
  theme_minimal()

# STEP 8: LINEAR REGRESSION ANALYSIS
# Calculate log returns
ivv_log_returns <- diff(log(ivv_price))
fcntx_log_returns <- diff(log(fcntx_price))
sp500_log_returns <- diff(log(sp500_price))

# Align returns
log_returns_df <- na.omit(data.frame(
  SP500 = sp500_log_returns,
  IVV = ivv_log_returns,
  FCNTX = fcntx_log_returns
))

# Regression: S&P 500 ~ IVV + FCNTX
model <- lm(SP500 ~ IVV + FCNTX, data = log_returns_df)
summary(model)

# Residual analysis
residuals_model <- residuals(model)
qqnorm(residuals_model)
qqline(residuals_model)

# White test for heteroskedasticity
white_test <- coeftest(model, vcov = sandwich)
print(white_test)

# STEP 9: PORTFOLIO SIMULATION
# Equal-weighted portfolio
portfolio_equi <- rowMeans(log_returns_df)

# Mean-Variance optimized portfolio
cov_matrix <- cov(log_returns_df)
weights_mv <- solve(cov_matrix) %*% colMeans(log_returns_df)
weights_mv <- weights_mv / sum(weights_mv)
portfolio_mv <- log_returns_df %*% weights_mv

# Mean-CVaR optimized portfolio (simplified)
portfolio_cvar <- apply(log_returns_df, 1, function(x) quantile(x, 0.05))

# Compare cumulative returns
cum_returns <- merge(
  SP500 = cumsum(sp500_log_returns),
  Equi = cumsum(portfolio_equi),
  MV = cumsum(portfolio_mv),
  CVaR = cumsum(portfolio_cvar)
)
chart.TimeSeries(cum_returns, main = "Cumulative Returns: Portfolio Comparison")
