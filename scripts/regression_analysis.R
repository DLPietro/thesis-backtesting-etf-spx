# Script: Linear Regression Analysis
# Desc: Regress IVV and FCNTX returns on S&P 500

library(quantmod)
library(PerformanceAnalytics)

# Load cleaned data
prices <- readRDS("data/raw/cleaned_prices.rds")
returns <- na.omit(Return.calculate(prices))

# Regression: IVV ~ SP500
model_ivv <- lm(IVV ~ SP500, data = returns)
summary(model_ivv)

# Regression: FCNTX ~ SP500
model_fcntx <- lm(FCNTX ~ SP500, data = returns)
summary(model_fcntx)

# Residuals
residuals_ivv <- residuals(model_ivv)
residuals_fcntx <- residuals(model_fcntx)

# Save
saveRDS(list(model_ivv, model_fcntx), "output/models/regression_models.rds")

cat("Regression analysis completed.\n")
