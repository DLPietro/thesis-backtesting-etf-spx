# Script: Data Cleaning
# Author: Pietro Di Leo
# Desc: Load and clean financial data for S&P 500, IVV, FCNTX

library(quantmod)
library(dplyr)

# Load data (assumes data is in data/raw/)
sp500 <- read.csv("data/raw/GSPC.csv", stringsAsFactors = FALSE)
ivv <- read.csv("data/raw/IVV.csv", stringsAsFactors = FALSE)
fcntx <- read.csv("data/raw/FCNTX.csv", stringsAsFactors = FALSE)

# Convert to xts
sp500_xts <- as.xts(sp500$Close, order.by = as.Date(sp500$Date))
ivv_xts <- as.xts(ivv$Close, order.by = as.Date(ivv$Date))
fcntx_xts <- as.xts(fcntx$Close, order.by = as.Date(fcntx$Date))

# Combine
prices <- merge(sp500_xts, ivv_xts, fcntx_xts)
colnames(prices) <- c("SP500", "IVV", "FCNTX")

# Save cleaned data
saveRDS(prices, "data/raw/cleaned_prices.rds")

cat("Data cleaning completed.\n")
