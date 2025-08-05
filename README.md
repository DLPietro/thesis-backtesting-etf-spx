# Empirical Comparison: S&P 500, iShares Core S&P 500 ETF (IVV), and Fidelity Contrafund (FCNTX)

🎓 Master’s Thesis in Quantitative Finance  
📍 Analyzed 10 years of financial data (2013–2023) to compare the performance of a passive ETF (IVV) vs. an actively managed fund (FCNTX) against the S&P 500 index.

## 🔍 Objective
To evaluate whether active management adds value over passive indexing, using statistical models and portfolio optimization techniques.

## 📊 Key Analyses
- Linear regression on log-returns
- GARCH modeling for volatility
- Residual analysis and heteroscedasticity correction (White’s method)
- Backtesting of three portfolio strategies:
  - Equally weighted
  - Mean-Variance optimization (Sharpe Ratio)
  - Mean-CVaR optimization
- Performance comparison using:
  - Total returns
  - Sharpe Ratio
  - Maximum drawdown

## 🧪 Models Tested
- ARMA
- SVM (Support Vector Machine)
- Random Forest
- Neural Network (Psi Sigma)

## 📈 Key Findings
- The iShares ETF (IVV) closely tracks the S&P 500 with minimal tracking error.
- Fidelity Contrafund (FCNTX) underperformed the benchmark after fees.
- The Mean-CVaR portfolio showed the best risk-adjusted returns.

## 🛠️ Tools Used
- R, RStudio
- `quantmod`, `PerformanceAnalytics`, `rugarch`, `caret`, `neuralnet`
- `ggplot2` for visualization

## 📂 Data Source
- Yahoo Finance (via `getSymbols`)
- Risk-free rate: 2.89% (annual)

## 📄 Full Thesis
[Download PDF](thesis.pdf)

## 🚀 How to Run
1. Install required packages:
   ```r
   install.packages(c("quantmod", "PerformanceAnalytics", "rugarch", "caret", "neuralnet", "ggplot2", "TTR", "kableExtra", "knitr"))
