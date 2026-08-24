data<-lrs34_Q2

library(quantmod)
library(psych)
library(ggplot2)
library(tseries)
library(forecast)
library(lmtest)
library(sandwich)
library(rugarch)

# We transform the original price series to lnreturns to avoid non-stationarity so we can keep going with our analysis

describe(data$ln_spy)
describe(data$spy_price)

# visualize
rplot <- plot(data$date_indx, data$ln_spy, type = "l", xlab = "Years", ylab = "Returns", main = "S&P 500 Ln Returns")
splot <- plot(data$date_indx, data$spy_price, type = "l", xlab = "Years", ylab = "Index Value", main = "S&P 500 Price Series")

png("returns_plot.png", width = 6, height = 4, units = "in", res = 300)
plot(data$date_indx, data$ln_spy, type = "l", xlab = "Years", ylab = "Returns", main = "S&P 500 Log Returns")
dev.off()

png("price_plot.png", width = 6, height = 4, units = "in", res = 300)
plot(data$date_indx, data$spy_price, type = "l", xlab = "Years", ylab = "Index Value", main = "S&P 500 Price Series")
dev.off()

# Autocorrelation
acf(data$ln_spy, main = "ACF S&P 500 Returns", ylim = c(-0.25, 0.25))
pacf(data$ln_spy, main = "PACF S&P 500 Returns")

# Stationarity concerns
adf.test(data$spy_price)
adf.test(data$ln_spy)

# Choosing suitable ARMA/ARIMA model
# AIC or BIC
p_max <- 2
q_max <- 2

# matrix to store AIC and BIC values
results <- matrix(NA, nrow = (p_max + 1) * (q_max + 1), ncol = 4)
colnames(results) <- c("p", "q", "AIC", "BIC")

# loop over all combinations
row <- 1
for (p in 0:p_max) {
  for (q in 0:q_max) {
    tryCatch({
      fit <- arima(data$ln_spy, order = c(p, 0, q),
                   include.mean = TRUE)
      results[row, ] <- c(p, q, AIC(fit), BIC(fit))
    }, error = function(e) {})
    row <- row + 1
  }
}

# print results sorted by AIC
results <- as.data.frame(results)
results[order(results$AIC), ]
results

# estimation of AR(1) model
ar1_fit <- arima(data$ln_spy, order = c(1, 0, 0), include.mean = TRUE)

# estimation results
coeftest(ar1_fit)

# extracting residuals
residuals <- residuals(ar1_fit)

# plotting residuals and other diagnostics
par(mfrow = c(2, 2))
plot(residuals, main = "Residuals of AR(1) Model",
     ylab = "Residuals", xlab = "Time")
abline(h = 0, col = "red", lty = 2)

# ACF & PACF of residuals
acf(residuals, main = "ACF of Residuals", lag.max = 30)
pacf(residuals, main = "PACF of Residuals", lag.max = 30)
# ggplot
qqnorm(residuals, main = "Normal Q-Q Plot of Residuals")
qqline(residuals, col = "red", lwd = 2)
par(mfrow = c(1, 1))  # reset layout

# Jarque-bera for normality
jarque.bera.test(residuals)

# box
Box.test(residuals,   lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(residuals^2, lag = 10, type = "Ljung-Box", fitdf = 0)

# G/ARCH section ==>

# acf of squared return
acf(data$ln_spy^2, main = "ACF Of Squared Returns")
# as also partly indicated before this graph shows strong and persistent autocorrelation through lags and is evidenc of ARCH effects

ArchTest(residuals, lags = 10)

# fit AR(1)-GARCH(1,1)

spec <- ugarchspec(
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "std"   # student-t handles heavy tails you already found
)

garch_fit <- ugarchfit(spec, data = data$ln_spy)
print(garch_fit)

# standardized residual diagnostics
std_resid <- residuals(garch_fit, standardize = TRUE)

par(mfrow = c(2, 2))

plot(std_resid, main = "Stdz. Residuals",
     ylab = "Std. Residuals", xlab = "Time", col = "steelblue")
abline(h = 0, col = "red", lty = 2)

acf(std_resid,  main = "ACF of Std. Residuals",  lag.max = 30)
pacf(std_resid, main = "PACF of Std. Residuals", lag.max = 30)

qqnorm(std_resid, main = "Q-Q Plot of Std. Residuals")
qqline(std_resid, col = "red", lwd = 2)

par(mfrow = c(1, 1))

# ljung-box AR(1)-GARCH(1,1)
Box.test(std_resid,   lag = 10, type = "Ljung-Box", fitdf = 0)
Box.test(std_resid^2, lag = 10, type = "Ljung-Box", fitdf = 0)

# ---------forecasting---------

# h-step-ahead forecasts (14 days ahead)
h  <- 14
fc <- forecast(ar1_fit, h = h, level = c(80, 95))

print(fc)

# dataframes with correct dates

# historical data with right dates
hist_df <- data.frame(
  Date  = as.Date(data$date_indx),
  Value = as.numeric(data$ln_spy)
)

last_date      <- max(hist_df$Date)
forecast_dates <- seq(last_date + 1, by = "day", length.out = h)

fc_df <- data.frame(
  Date  = forecast_dates,
  Mean  = as.numeric(fc$mean),
  Lo_80 = as.numeric(fc$lower[, 1]),
  Hi_80 = as.numeric(fc$upper[, 1]),
  Lo_95 = as.numeric(fc$lower[, 2]),
  Hi_95 = as.numeric(fc$upper[, 2])
)

print(fc_df)

n_tail <- 90
hist_df_tail <- tail(hist_df, n_tail)

ggplot() +
  # historical
  geom_line(data = hist_df_tail,
            aes(x = Date, y = Value), color = "black") +
  # 95%
  geom_ribbon(data = fc_df,
              aes(x = Date, ymin = Lo_95, ymax = Hi_95),
              fill = "steelblue", alpha = 0.2) +
  # 80%
  geom_ribbon(data = fc_df,
              aes(x = Date, ymin = Lo_80, ymax = Hi_80),
              fill = "steelblue", alpha = 0.4) +
  # mean of the prediction
  geom_line(data = fc_df,
            aes(x = Date, y = Mean),
            color = "steelblue", linewidth = 1) +
  geom_vline(xintercept = as.numeric(last_date),
             linetype = "dashed", color = "gray40") +
  labs(title = "S&P 500 Returns: Forecasts with Prediction Intervals",
       x     = "Date",
       y     = "S&P 500 Returns") +
  theme_minimal()

# out-of-sample evaluation: rolling window, h=1
n      <- length(data$ln_spy)
T_init <- floor(0.8 * n)   # fixed window size (80% of sample)
errors <- numeric(n - T_init)

for (j in seq_along(errors)) {
  window_j  <- data$ln_spy[j:(T_init + j - 1)]
  fit_j     <- Arima(window_j, order = c(1, 0, 0))
  fc_j      <- forecast(fit_j, h = 1)
  errors[j] <- data$ln_spy[T_init + j] - fc_j$mean[1]
}

MSFE <- mean(errors^2)
MAFE <- mean(abs(errors))
cat("Out-of-sample AR(1) - Rolling Window:\n")
cat("MSFE:", MSFE, "\nMAFE:", MAFE, "\n")

# benchmark: rolling unconditional mean
errors_rw <- numeric(n - T_init)

for (j in seq_along(errors_rw)) {
  window_j     <- data$ln_spy[j:(T_init + j - 1)]
  errors_rw[j] <- data$ln_spy[T_init + j] - mean(window_j)
}

MSFE_rw <- mean(errors_rw^2)
MAFE_rw <- mean(abs(errors_rw))

cat("\nBenchmark (rolling mean):\n")
cat("MSFE:", MSFE_rw, "\nMAFE:", MAFE_rw, "\n")
