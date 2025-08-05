# Main Analysis Script
# Runs all steps in sequence

source("scripts/data_cleaning.R")
source("scripts/regression_analysis.R")
source("scripts/garch_modeling.R")
source("scripts/portfolio_optimization.R")
source("scripts/backtesting_models.R")

cat("✅ All analyses completed.\n")
cat("View results in /output and thesis.pdf\n")
