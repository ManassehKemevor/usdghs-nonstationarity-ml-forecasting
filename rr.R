library(tidyverse)
library(tseries)
# 1. Load the data
data <- read_csv("clean_output/combined.csv")

# Rename the column with a space (do this once, here)
data <- data %>% rename(exchange_rate = `exchange rate`)

# 2. Look at it BEFORE dropping missing values
glimpse(data)
colSums(is.na(data))      # missing values per column

# 3. Drop rows with any missing value
data <- drop_na(data)
data <- data %>% filter(date >= "2002-01-01"& date <= "2021-12-01")

# 4. Check after dropping
nrow(data)
colSums(is.na(data))      # should all be 0
summary(data)

# 5. Time plots
plot(data$date, data$cpi,           type = "l", col = "steelblue", main = "CPI",           xlab = "Date", ylab = "")
plot(data$date, data$cocoa,         type = "l", col = "steelblue", main = "Cocoa",         xlab = "Date", ylab = "")
plot(data$date, data$crude,         type = "l", col = "steelblue", main = "Crude",         xlab = "Date", ylab = "")
plot(data$date, data$exchange_rate, type = "l", col = "steelblue", main = "Exchange rate", xlab = "Date", ylab = "")

# 6. Distributions
hist(data$cpi,           main = "CPI",           col = "steelblue", xlab = "")
hist(data$cocoa,         main = "Cocoa",         col = "steelblue", xlab = "")
hist(data$crude,         main = "Crude",         col = "steelblue", xlab = "")
hist(data$exchange_rate, main = "Exchange rate", col = "steelblue", xlab = "")

# 7. Seasonality (values by calendar month)
data$month <- lubridate::month(data$date, label = TRUE)

boxplot(cpi           ~ month, data = data, main = "CPI",           col = "lightblue", xlab = "", ylab = "")
boxplot(cocoa         ~ month, data = data, main = "Cocoa",         col = "lightblue", xlab = "", ylab = "")
boxplot(crude         ~ month, data = data, main = "Crude",         col = "lightblue", xlab = "", ylab = "")
boxplot(exchange_rate ~ month, data = data, main = "Exchange rate", col = "lightblue", xlab = "", ylab = "")

# 8. Correlation between the series
vars <- data %>% dplyr::select(cpi, cocoa, crude, exchange_rate)
cor(vars) %>% round(2)
pairs(vars, pch = 16)

# 9. Autocorrelation
acf(data$cpi,           main = "CPI")
acf(data$cocoa,         main = "Cocoa")
acf(data$crude,         main = "Crude")
acf(data$exchange_rate, main = "Exchange rate")

pacf(data$cpi,           main = "CPI")
pacf(data$cocoa,         main = "Cocoa")
pacf(data$crude,         main = "Crude")
pacf(data$exchange_rate, main = "Exchange rate")


cpiTs   <- ts(data$cpi,       start = c(2002, 1), frequency = 12)
cocoaTs <- ts(data$cocoa,     start = c(2002, 1), frequency = 12)
crudeTs <- ts(data$crude,     start = c(2002, 1), frequency = 12)
fxTs    <- ts(data$exchange_rate, start = c(2002, 1), frequency = 12)

# 11. Decomposition: trend + seasonal + remainder
plot(stl(cpiTs,   s.window = "periodic"), main = "CPI")
plot(stl(cocoaTs, s.window = "periodic"), main = "Cocoa")
plot(stl(crudeTs, s.window = "periodic"), main = "Crude")
plot(stl(fxTs,    s.window = "periodic"), main = "Exchange rate")


# ---------------------------------------------------------
# NEW: STATIONARITY
# ADF:  p < 0.05 -> stationary
# KPSS: p > 0.05 -> stationary (its null is reversed)
# ---------------------------------------------------------

# 12. LEVELS: one test at a time, all four variables together
adfLevels <- c(CPI = adf.test(cpiTs)$p.value,
               Cocoa = adf.test(cocoaTs)$p.value,
               Crude= adf.test(crudeTs)$p.value,
               Exchange_rate = adf.test(fxTs)$p.value)

kpssLevels <- c(CPI= kpss.test(cpiTs)$p.value,
                Cocoa= kpss.test(cocoaTs)$p.value,
                Crude= kpss.test(crudeTs)$p.value,
                Exchange_rate = kpss.test(fxTs)$p.value)

# Group the p-values by test
data.frame(ADF = adfLevels, KPSS = kpssLevels) %>% round(3)

# 13. Make the non-stationary series stationary: plain first differences
#     (NO scaling -- scaling on the full sample leaks test information)
#     CPI looks like month-on-month inflation (range -2.1 to 9.3), so it stays as it is
range(data$cpi)
cocoaDiff <- diff(cocoaTs)
crudeDiff <- diff(crudeTs)
fxDiff    <- diff(fxTs)
inflTs    <- window(cpiTs, start = c(2002, 2))     # CPI m/m inflation

plot(inflTs,    col = "steelblue", main = "CPI (m/m inflation)",        xlab = "", ylab = "")
plot(cocoaDiff, col = "steelblue", main = "Cocoa (first difference)",   xlab = "", ylab = "")
plot(crudeDiff, col = "steelblue", main = "Crude (first difference)",   xlab = "", ylab = "")
plot(fxDiff,    col = "steelblue", main = "USD/GHS (first difference)", xlab = "", ylab = "")

# 14. CHANGES: same two tests (CPI inflation is tested as it is)
adfChanges <- c(CPI           = adf.test(inflTs)$p.value,
                Cocoa         = adf.test(cocoaDiff)$p.value,
                Crude         = adf.test(crudeDiff)$p.value,
                Exchange_rate = adf.test(fxDiff)$p.value)

kpssChanges <- c(CPI           = kpss.test(inflTs)$p.value,
                 Cocoa         = kpss.test(cocoaDiff)$p.value,
                 Crude         = kpss.test(crudeDiff)$p.value,
                 Exchange_rate = kpss.test(fxDiff)$p.value)

data.frame(ADF = adfChanges, KPSS = kpssChanges) %>% round(3)
##############################################################################################


# =========================================================
# MULTIVARIATE: forecasting EXCHANGE RATE (fxDiff = first difference)
# Predictors: inflation, cocoaDiff, crudeDiff
# Continues from the end of rr.R
# =========================================================
library(forecast)
library(lmtest)
library(tseries)

set.seed(23)
#
# ---------------------------------------------------------
# 15. Stationary dataset
# ---------------------------------------------------------
dat <- ts.intersect(fxDiff, inflTs, cocoaDiff, crudeDiff)

# USD/GHS in LEVELS, same months as dat (used to compare everything in cedis)
fxLevel <- window(fxTs, start = c(2002, 2))

# ---------------------------------------------------------
# 16. Train / test split (time-ordered, 80/20 = last 48 months for test)
# ---------------------------------------------------------
n      <- nrow(dat)
nTrain <- floor(0.8 * n)
h      <- n - nTrain

train <- window(dat, end   = time(dat)[nTrain])
test  <- window(dat, start = time(dat)[nTrain + 1])
cat("Train:", nrow(train), " Test:", nrow(test), "\n")

yTrain <- train[, "fxDiff"]
yTest  <- test[,  "fxDiff"]

# Predictors: one matrix for training, one for all months (used by the rolling forecasts)
xTr  <- as.matrix(train[, c("inflTs", "cocoaDiff", "crudeDiff")])
xAll <- as.matrix(dat[,   c("inflTs", "cocoaDiff", "crudeDiff")])

# =========================================================
# PART A: ARIMAX
# =========================================================

# 17. Automatic model for reference
axAuto <- auto.arima(yTrain, xreg = xTr,
                     d = 0, D = 0,
                     stepwise = FALSE, approximation = FALSE,
                     seasonal.test = "seas", ic = "aicc", trace = TRUE)
print(axAuto)

# 18. Competing ARIMAX models (same predictors, different ARMA errors)
ax1 <- Arima(yTrain, order = c(0,0,0), xreg = xTr)
ax2 <- Arima(yTrain, order = c(1,0,1), xreg = xTr)
ax3 <- Arima(yTrain, order = c(1,0,2), xreg = xTr)
ax4 <- Arima(yTrain, order = c(1,0,3), xreg = xTr)


# 19. Information criteria (HQIC = AIC with penalty 2*log(log(n)))
kHq <- 2 * log(log(length(yTrain)))

icX <- data.frame(
  AIC  = AIC(ax1, ax2, ax3, ax4, axAuto)$AIC,
  BIC  = BIC(ax1, ax2, ax3, ax4, axAuto)$BIC,
  HQIC = AIC(ax1, ax2, ax3, ax4, axAuto, k = kHq)$AIC,
  row.names = c("ax1", "ax2", "ax3", "ax4", "axAuto"))
print(round(icX, 3))

# 20. Pick the best ARIMAX: the row with the lowest values in the table above
bestX <- axAuto                      # <- change to the model with the lowest AIC/BIC/HQIC
print(bestX)
coeftest(bestX)                   # are the predictors significant?

# 21. Residual diagnostics: ARIMAX
checkresiduals(bestX, lag = 24)                  # plot, ACF, histogram, Ljung-Box
shapiro.test(residuals(bestX))                   # normality
Box.test(residuals(bestX)^2, lag = 12, type = "Ljung-Box")   # ARCH effects

# 22. Rolling forecasts of the test window (model re-estimated every month)
#     Future predictor values are taken as known (actual test values).
ord <- arimaorder(bestX)[1:3]
fArimax <- function(x, h, xreg, newxreg)
  forecast(Arima(x, order = ord, xreg = xreg), h = h, xreg = newxreg)

eDiff   <- tsCV(dat[, "fxDiff"], fArimax, h = 6, xreg = xAll, initial = nTrain)
eArimax <- t(apply(as.matrix(eDiff), 1, cumsum))     # errors back in LEVELS (cedis), h = 1..6

# 23. Benchmarks on levels: random walk with drift (main) and plain random walk
eRWdrift <- tsCV(fxLevel, rwf,   h = 6, drift = TRUE, initial = nTrain)
eRW      <- tsCV(fxLevel, naive, h = 6,               initial = nTrain)

# 24. Choose the horizon (1, 3 or 6 months) and re-run from here for each
hz <- 1
# keep only the origins where ALL three models produced a forecast, so lengths always match
ok     <- complete.cases(eArimax[, hz], eRWdrift[, hz], eRW[, hz])
idx    <- which(ok)                                  # forecast origins (row numbers)
eA     <- eArimax[idx,  hz]
eD     <- eRWdrift[idx, hz]
eN     <- eRW[idx,      hz]
actual <- as.numeric(fxLevel)[idx + hz]              # test targets
origin <- as.numeric(fxLevel)[idx]                   # last observed level at each forecast
cat("Forecasts evaluated:", length(idx), "\n")

# 25. Accuracy in cedis (accuracy() from forecast gives ME, RMSE, MAE)
keep <- c("ME", "RMSE", "MAE")
testMetrics <- rbind(
  ARIMAX   = accuracy(actual - eA, actual)[, keep],
  RW_drift = accuracy(actual - eD, actual)[, keep],
  RW       = accuracy(actual - eN, actual)[, keep])

maseDen <- mean(abs(as.numeric(yTrain)))             # one-step naive error on training levels
testMetrics <- cbind(testMetrics,
                     MASE    = testMetrics[, "MAE"] / maseDen,
                     RelRMSE = testMetrics[, "RMSE"] / testMetrics["RW_drift", "RMSE"])

cat("\n--- TEST-SET METRICS (cedis), horizon =", hz, "month(s) ---\n")
print(round(testMetrics, 4))

# 26. Directional accuracy (did the forecast get the up/down move right?)
mean(sign((actual - eA) - origin) == sign(actual - origin))

# 27. Diebold-Mariano test: ARIMAX vs random walk with drift
dm.test(eA, eD, h = hz, power = 2)


# 29. Training-set metrics (one-step, same units as the 1-month test RMSE)
fittedX <- fitted(bestX)

# Align fitted values with training observations
valid <- is.finite(fittedX) & is.finite(yTrain)
actualTrain <- as.numeric(yTrain[valid])
predTrain   <- as.numeric(fittedX[valid])

trainME   <- mean(actualTrain - predTrain)
trainRMSE <- sqrt(mean((actualTrain - predTrain)^2))
trainMAE  <- mean(abs(actualTrain - predTrain))
trainMASE <- trainMAE / maseDen

trainMetrics <- data.frame(
  ME   = trainME,
  RMSE = trainRMSE,
  MAE  = trainMAE,
  MASE = trainMASE
)

cat("\n--- TRAIN-SET METRICS ---\n")
print(round(trainMetrics, 4))


# 30. Over/underfitting check (use hz = 1 so test and train RMSE are in the same units)
fitCheck <- data.frame(
  rmseTrain = trainMetrics$RMSE,
  rmseTest  = testMetrics["ARIMAX", "RMSE"]
)

fitCheck$ratio <- fitCheck$rmseTest / fitCheck$rmseTrain
print(round(fitCheck, 4))


# 31. Plot actual vs forecasts, in levels
plot(time(fxLevel)[idx + hz], actual, type = "l", lwd = 2, col = "black",
     main = paste("USD/GHS: actual vs", hz, "month-ahead forecasts (test set)"),
     xlab = "Year", ylab = "GHS per USD")

lines(time(fxLevel)[idx + hz], actual - eA, col = "steelblue", lwd = 2)
lines(time(fxLevel)[idx + hz], actual - eD, col = "firebrick", lwd = 2, lty = 2)

legend("topleft", c("Actual", "ARIMAX", "RW with drift"),
       col = c("black", "steelblue", "firebrick"), lwd = 2, lty = c(1, 1, 2), bty = "n")
