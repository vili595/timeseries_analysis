# timeseries_analysis
S&P 500 Log Returns - Time Series Analysis in R

Time series analysis of S&P 500 log returns, covering model selection, diagnostics, volatility modeling and out-of-sample forecast evaluation.

Overview

This project analyzes the S&P 500 price series and its log returns to characterize their time series properties and build a forecasting model. It moves from exploratory analysis through stationarity testing, ARMA model selection, GARCH-based volatility modeling and a rolling-window forecast evaluation against a naive benchmark.

Workflow

1. Data preparation & visualization Converts the S&P 500 price series to log returns to address non-stationarity and plots both series over time.

2. Stationarity & autocorrelation checks

ACF/PACF plots of returns
Augmented Dickey-Fuller tests on price and return series

3. ARMA model selection Grid search over ARMA(p,q) combinations (p, q = 0–2), ranked by AIC/BIC, with an AR(1) model selected for further analysis.

4. Residual diagnostics

ACF/PACF and Q-Q plots of residuals
Jarque-Bera test for normality
Ljung-Box tests on residuals and squared residuals

5. ARCH effects & volatility modeling

ACF of squared returns and an ARCH-LM test to confirm volatility clustering
AR(1)-GARCH(1,1) model with a Student-t distribution, fit via rugarch
Standardized residual diagnostics on the fitted GARCH model

6. Forecasting

14-day-ahead AR(1) forecasts with 80%/95% prediction intervals, visualized with ggplot2
Rolling-window out-of-sample evaluation (80/20 split), comparing AR(1) forecast accuracy (MSFE, MAFE) against a rolling unconditional mean benchmark
Requirements

R packages: quantmod, psych, ggplot2, tseries, forecast, lmtest, sandwich, FinTS, rugarch

Notes

This is coursework/portfolio code for demonstrating time series modeling techniques (ARMA, GARCH, rolling forecast evaluation) on financial return data - not investment advice.
