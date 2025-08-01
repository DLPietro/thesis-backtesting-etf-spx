# Script: GARCH Volatility Modeling
# Desc: Fit GARCH(1,1) to SP500 returns

library(rugarch)

# Load returns
prices <- readRDS("data/raw/cleaned_prices.rds")
returns <- na.omit(Return.calculate(prices$SP500))

# GARCH spec
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0))
)

# Fit model
garch_fit <- ugarchfit(spec = spec, data = returns)

# Summary
print(garch_fit)

# Forecast
garch_forecast <- ugarchforecast(garch_fit, n.ahead = 10)

# Save
saveRDS(garch_fit, "output/models/garch_model.rds")

cat("GARCH modeling completed.\n")
