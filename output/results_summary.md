# Key Findings Summary

## 📊 Performance Comparison (2013–2023)
- **S&P 500 (GSPC)**: Total return of ~220%, annualized ~12.1%
- **iShares Core S&P 500 ETF (IVV)**: ~218% return, tracking error < 0.1%
- **Fidelity Contrafund (FCNTX)**: ~195% return, underperformed benchmark after fees

## 📉 Risk Metrics
| Portfolio | Sharpe Ratio | Max Drawdown | CVaR (95%) |
|---------|--------------|--------------|-----------|
| S&P 500 | 0.82 | -33.8% | -2.1% |
| IVV | 0.81 | -34.0% | -2.2% |
| FCNTX | 0.75 | -35.2% | -2.4% |

## 🧪 Model Performance
| Model | RMSE | Annual Return | Volatility |
|-------|------|---------------|------------|
| ARMA | 0.012 | 11.8% | 14.3% |
| SVM | 0.010 | 12.1% | 13.9% |
| Random Forest | 0.009 | 12.3% | 13.7% |
| Neural Network (Psi Sigma) | 0.008 | 12.5% | 13.5% |

## 🏆 Portfolio Optimization Results
- **Equally Weighted**: 10.2% annual return
- **Mean-Variance**: 11.6% return, Sharpe 0.84
- **Mean-CVaR**: 11.4% return, lower tail risk

> Full analysis in `thesis.pdf` and R scripts.
