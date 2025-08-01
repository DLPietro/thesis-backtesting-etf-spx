# Script: Portfolio Optimization
# Desc: Compare Equally Weighted, Mean-Variance, Mean-CVaR

library(PerformanceAnalytics)
library(quadprog)

# Load returns
prices <- readRDS("data/raw/cleaned_prices.rds")
returns <- na.omit(Return.calculate(prices[, c("IVV", "FCNTX")]))

# 1. Equally Weighted
ew_weights <- c(0.5, 0.5)

# 2. Mean-Variance
mv_weights <- portfolio.optim(returns, pm = mean(apply(returns, 2, mean)), riskless = FALSE)$pw

# 3. Mean-CVaR (simplified)
cvar_weights <- mv_weights  # Placeholder – use more advanced method in practice

# Save
weights <- data.frame(
  Asset = colnames(returns),
  Equally_Weighted = ew_weights,
  Mean_Variance = mv_weights,
  Mean_CVaR = cvar_weights
)

write.csv(weights, "output/portfolio_weights.csv", row.names = FALSE)

cat("Portfolio optimization completed.\n")
