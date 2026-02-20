# ============================================
# ACCP009: Empirical Methods in Finance and Accounting
# Week 1 Practice Tasks - R Solutions
# Spring 2026
# ============================================

# SETUP
# Load required libraries
library(xts)           # Time series objects
library(zoo)           # Time series manipulation
library(moments)       # Skewness and kurtosis
library(tseries)       # Jarque-Bera test

# Set working directory (adjust to your file location)
# setwd("path/to/your/data")

# ============================================
# TASK 1: DATA IMPORT AND INITIAL EXPLORATION
# ============================================

# 1.1 Import the data
data <- read.csv("data1.csv", stringsAsFactors = FALSE)

# Convert Date column to proper date format
data$Date <- as.Date(data$Date)

# Check for missing values
cat("Missing values in dataset:\n")
print(colSums(is.na(data)))

# 1.2 Examine the data structure
cat("\n--- Data Structure ---\n")
cat("Number of observations:", nrow(data), "\n")
cat("Date range:", as.character(min(data$Date)), "to", as.character(max(data$Date)), "\n")

# Display first and last 10 rows
cat("\nFirst 10 rows:\n")
print(head(data, 10))

cat("\nLast 10 rows:\n")
print(tail(data, 10))

# 1.3 Create price charts

# Convert to xts object for better time series plotting
data_xts <- xts(data[, c("SP500", "MSFT")], order.by = data$Date)

# Plot SP500 prices
plot(data_xts$SP500, 
     main = "S&P 500 Index Prices",
     ylab = "Price",
     col = "blue",
     lwd = 1.5)

# Plot MSFT prices
plot(data_xts$MSFT,
     main = "Microsoft Stock Prices",
     ylab = "Price",
     col = "red",
     lwd = 1.5)

# Create combined plot with rebasing to 100
# Rebase both series to 100 at the start date
sp500_rebased <- 100 * data_xts$SP500 / as.numeric(first(data_xts$SP500))
msft_rebased <- 100 * data_xts$MSFT / as.numeric(first(data_xts$MSFT))

# Combine rebased series
rebased_data <- merge(sp500_rebased, msft_rebased)
colnames(rebased_data) <- c("SP500_rebased", "MSFT_rebased")

# Plot combined rebased chart
plot(rebased_data,
     main = "S&P 500 vs MSFT (Rebased to 100)",
     legend.loc = "topleft",
     col = c("blue", "red"),
     lwd = 1.5)

# ============================================
# TASK 2: RETURNS CALCULATION
# ============================================

# 2.1 Calculate simple net returns
data$SP500_simple <- c(NA, diff(data$SP500) / data$SP500[-nrow(data)])
data$MSFT_simple <- c(NA, diff(data$MSFT) / data$MSFT[-nrow(data)])

# 2.2 Calculate log returns
data$SP500_log <- c(NA, diff(log(data$SP500)))
data$MSFT_log <- c(NA, diff(log(data$MSFT)))

# Alternative method using xts (more elegant)
returns_xts <- diff(log(data_xts))
colnames(returns_xts) <- c("SP500_log", "MSFT_log")

# 2.3 Compare simple and log returns

# Remove NA values for comparison
data_complete <- na.omit(data)

# Scatter plot: Simple vs Log returns for SP500
plot(data_complete$SP500_log, data_complete$SP500_simple,
     xlab = "Log Return",
     ylab = "Simple Return",
     main = "S&P 500: Simple vs Log Returns",
     pch = 16,
     col = rgb(0, 0, 1, 0.3))
abline(0, 1, col = "red", lwd = 2)  # 45-degree line

# Scatter plot: Simple vs Log returns for MSFT
plot(data_complete$MSFT_log, data_complete$MSFT_simple,
     xlab = "Log Return",
     ylab = "Simple Return",
     main = "MSFT: Simple vs Log Returns",
     pch = 16,
     col = rgb(1, 0, 0, 0.3))
abline(0, 1, col = "blue", lwd = 2)

# Calculate differences
data_complete$SP500_diff <- data_complete$SP500_simple - data_complete$SP500_log
data_complete$MSFT_diff <- data_complete$MSFT_simple - data_complete$MSFT_log

# Maximum absolute difference
cat("\n--- Simple vs Log Returns Comparison ---\n")
cat("S&P 500 max absolute difference:", max(abs(data_complete$SP500_diff)), "\n")
cat("MSFT max absolute difference:", max(abs(data_complete$MSFT_diff)), "\n")

# 2.4 Plot return series

# Time series plots of simple returns
par(mfrow = c(2, 1))
plot(data_complete$Date, data_complete$SP500_simple,
     type = "l",
     main = "S&P 500 Simple Returns",
     xlab = "Date",
     ylab = "Return",
     col = "blue")
plot(data_complete$Date, data_complete$MSFT_simple,
     type = "l",
     main = "MSFT Simple Returns",
     xlab = "Date",
     ylab = "Return",
     col = "red")
par(mfrow = c(1, 1))

# Time series plots of log returns
par(mfrow = c(2, 1))
plot(returns_xts$SP500_log,
     main = "S&P 500 Log Returns",
     ylab = "Return",
     col = "blue")
plot(returns_xts$MSFT_log,
     main = "MSFT Log Returns",
     ylab = "Return",
     col = "red")
par(mfrow = c(1, 1))

# ============================================
# TASK 3: DESCRIPTIVE STATISTICS
# ============================================

# 3.1 & 3.2 Calculate basic statistics and distributional properties

# Function to calculate all statistics
calc_stats <- function(x) {
  c(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE),
    Kurtosis = kurtosis(x, na.rm = TRUE)
  )
}

# Remove NA values
returns <- na.omit(data[, c("Date", "SP500_log", "MSFT_log")])

# Calculate statistics for both series
sp500_stats <- calc_stats(returns$SP500_log)
msft_stats <- calc_stats(returns$MSFT_log)

# Combine into table
stats_table <- data.frame(
  Statistic = names(sp500_stats),
  SP500 = sp500_stats,
  MSFT = msft_stats
)

cat("\n--- Descriptive Statistics (Log Returns) ---\n")
print(stats_table)

# 3.3 Annualise returns and volatility
annual_return_sp500 <- mean(returns$SP500_log, na.rm = TRUE) * 252
annual_return_msft <- mean(returns$MSFT_log, na.rm = TRUE) * 252
annual_vol_sp500 <- sd(returns$SP500_log, na.rm = TRUE) * sqrt(252)
annual_vol_msft <- sd(returns$MSFT_log, na.rm = TRUE) * sqrt(252)

cat("\n--- Annualised Statistics ---\n")
cat("S&P 500: Annual Return =", round(annual_return_sp500, 4),
    ", Annual Volatility =", round(annual_vol_sp500, 4), "\n")
cat("MSFT: Annual Return =", round(annual_return_msft, 4),
    ", Annual Volatility =", round(annual_vol_msft, 4), "\n")

# ============================================
# TASK 4: DISTRIBUTION ANALYSIS
# ============================================

# 4.1 Create histograms with normal distribution overlay

par(mfrow = c(1, 2))

# S&P 500 histogram
hist(returns$SP500_log,
     breaks = 50,
     probability = TRUE,
     main = "S&P 500 Log Returns",
     xlab = "Return",
     col = "lightblue",
     border = "white")

# Overlay normal distribution
x_seq <- seq(min(returns$SP500_log), max(returns$SP500_log), length = 100)
lines(x_seq,
      dnorm(x_seq, mean = mean(returns$SP500_log), sd = sd(returns$SP500_log)),
      col = "red",
      lwd = 2)
legend("topright", legend = "Normal", col = "red", lwd = 2)

# MSFT histogram
hist(returns$MSFT_log,
     breaks = 50,
     probability = TRUE,
     main = "MSFT Log Returns",
     xlab = "Return",
     col = "lightgreen",
     border = "white")

# Overlay normal distribution
x_seq <- seq(min(returns$MSFT_log), max(returns$MSFT_log), length = 100)
lines(x_seq,
      dnorm(x_seq, mean = mean(returns$MSFT_log), sd = sd(returns$MSFT_log)),
      col = "red",
      lwd = 2)
legend("topright", legend = "Normal", col = "red", lwd = 2)

par(mfrow = c(1, 1))

# 4.2 Create Q-Q plots

par(mfrow = c(1, 2))

# S&P 500 Q-Q plot
qqnorm(returns$SP500_log, main = "Q-Q Plot: S&P 500")
qqline(returns$SP500_log, col = "red", lwd = 2)

# MSFT Q-Q plot
qqnorm(returns$MSFT_log, main = "Q-Q Plot: MSFT")
qqline(returns$MSFT_log, col = "red", lwd = 2)

par(mfrow = c(1, 1))

# 4.3 Formal normality tests (Jarque-Bera)

# Test for S&P 500
jb_sp500 <- jarque.bera.test(returns$SP500_log)
cat("\n--- Jarque-Bera Test: S&P 500 ---\n")
print(jb_sp500)

# Test for MSFT
jb_msft <- jarque.bera.test(returns$MSFT_log)
cat("\n--- Jarque-Bera Test: MSFT ---\n")
print(jb_msft)

# Interpretation helper
cat("\n--- Interpretation ---\n")
cat("Null hypothesis: Returns are normally distributed\n")
cat("If p-value < 0.05: Reject null (returns NOT normal)\n\n")
cat("S&P 500 p-value:", jb_sp500$p.value,
    ifelse(jb_sp500$p.value < 0.05, "→ REJECT normality", "→ Cannot reject normality"), "\n")
cat("MSFT p-value:", jb_msft$p.value,
    ifelse(jb_msft$p.value < 0.05, "→ REJECT normality", "→ Cannot reject normality"), "\n")

# ============================================
# TASK 5: CORRELATION ANALYSIS
# ============================================

# 5.1 Calculate Pearson correlation and test significance
cor_test <- cor.test(returns$MSFT_log, returns$SP500_log)

cat("\n--- Correlation Analysis ---\n")
cat("Correlation coefficient:", cor_test$estimate, "\n")
cat("t-statistic:", cor_test$statistic, "\n")
cat("p-value:", cor_test$p.value, "\n")
cat("95% Confidence Interval:", cor_test$conf.int, "\n")

# Interpretation
if (cor_test$p.value < 0.05) {
  cat("\nThe correlation is statistically significant (p < 0.05)\n")
} else {
  cat("\nThe correlation is NOT statistically significant (p >= 0.05)\n")
}

# 5.2 Visualise the relationship

# Scatter plot with regression line
plot(returns$SP500_log, returns$MSFT_log,
     xlab = "S&P 500 Log Return",
     ylab = "MSFT Log Return",
     main = paste0("MSFT vs S&P 500 Returns (ρ = ",
                   round(cor_test$estimate, 3), ")"),
     pch = 16,
     col = rgb(0, 0, 1, 0.3))

# Add regression line
abline(lm(returns$MSFT_log ~ returns$SP500_log),
       col = "red",
       lwd = 2)

# Add y=x reference line
abline(0, 1, col = "gray", lty = 2, lwd = 1)

# Add legend
legend("topleft",
       legend = c("Data", "Regression line", "y = x"),
       col = c(rgb(0, 0, 1, 0.5), "red", "gray"),
       pch = c(16, NA, NA),
       lty = c(NA, 1, 2),
       lwd = c(NA, 2, 1))

# 5.3 Calculate alternative correlation measures (Optional)

# Spearman's rank correlation
spearman_cor <- cor.test(returns$MSFT_log, returns$SP500_log, method = "spearman")
cat("\n--- Spearman's Rank Correlation ---\n")
cat("Spearman's rho:", spearman_cor$estimate, "\n")
cat("p-value:", spearman_cor$p.value, "\n")

# Kendall's tau
kendall_cor <- cor.test(returns$MSFT_log, returns$SP500_log, method = "kendall")
cat("\n--- Kendall's Tau ---\n")
cat("Kendall's tau:", kendall_cor$estimate, "\n")
cat("p-value:", kendall_cor$p.value, "\n")

# ============================================
# TASK 6: COMPARISON ACROSS FREQUENCIES (OPTIONAL)
# ============================================

# 6.1 Aggregate to monthly returns

# Create xts object for returns
returns_xts <- xts(returns[, c("SP500_log", "MSFT_log")], order.by = returns$Date)

# Aggregate to monthly (sum of daily log returns)
monthly_returns <- apply.monthly(returns_xts, sum)

# 6.2 Repeat descriptive analysis

# Calculate statistics for monthly returns
sp500_monthly_stats <- calc_stats(monthly_returns$SP500_log)
msft_monthly_stats <- calc_stats(monthly_returns$MSFT_log)

cat("\n--- Monthly Returns Statistics ---\n")
monthly_stats_table <- data.frame(
  Statistic = names(sp500_monthly_stats),
  SP500_Monthly = sp500_monthly_stats,
  MSFT_Monthly = msft_monthly_stats
)
print(monthly_stats_table)

# Histograms with normal overlays for monthly returns
par(mfrow = c(1, 2))

# S&P 500 monthly
hist(monthly_returns$SP500_log,
     breaks = 30,
     probability = TRUE,
     main = "S&P 500 Monthly Log Returns",
     xlab = "Return",
     col = "lightblue")
x_seq <- seq(min(monthly_returns$SP500_log), max(monthly_returns$SP500_log), length = 100)
lines(x_seq,
      dnorm(x_seq, mean = mean(monthly_returns$SP500_log), sd = sd(monthly_returns$SP500_log)),
      col = "red", lwd = 2)

# MSFT monthly
hist(monthly_returns$MSFT_log,
     breaks = 30,
     probability = TRUE,
     main = "MSFT Monthly Log Returns",
     xlab = "Return",
     col = "lightgreen")
x_seq <- seq(min(monthly_returns$MSFT_log), max(monthly_returns$MSFT_log), length = 100)
lines(x_seq,
      dnorm(x_seq, mean = mean(monthly_returns$MSFT_log), sd = sd(monthly_returns$MSFT_log)),
      col = "red", lwd = 2)

par(mfrow = c(1, 1))

# Jarque-Bera tests for monthly returns
jb_sp500_monthly <- jarque.bera.test(as.numeric(monthly_returns$SP500_log))
jb_msft_monthly <- jarque.bera.test(as.numeric(monthly_returns$MSFT_log))

cat("\n--- Jarque-Bera Tests: Monthly Returns ---\n")
cat("S&P 500 p-value:", jb_sp500_monthly$p.value, "\n")
cat("MSFT p-value:", jb_msft_monthly$p.value, "\n")

# 6.3 Compare results

cat("\n--- Comparison: Daily vs Monthly ---\n")
comparison_table <- data.frame(
  Statistic = c("Mean", "SD", "Skewness", "Kurtosis"),
  SP500_Daily = c(mean(returns$SP500_log), sd(returns$SP500_log),
                  skewness(returns$SP500_log), kurtosis(returns$SP500_log)),
  SP500_Monthly = c(mean(monthly_returns$SP500_log), sd(monthly_returns$SP500_log),
                    skewness(monthly_returns$SP500_log), kurtosis(monthly_returns$SP500_log)),
  MSFT_Daily = c(mean(returns$MSFT_log), sd(returns$MSFT_log),
                 skewness(returns$MSFT_log), kurtosis(returns$MSFT_log)),
  MSFT_Monthly = c(mean(monthly_returns$MSFT_log), sd(monthly_returns$MSFT_log),
                   skewness(monthly_returns$MSFT_log), kurtosis(monthly_returns$MSFT_log))
)
print(comparison_table)

# ============================================
# EXTENSION 1: DOWNLOAD YOUR OWN DATA
# ============================================

# Example: Download data for a different stock/index pair
# Uncomment and modify as needed

# library(quantmod)
# 
# # Download data from Yahoo Finance
# getSymbols("AAPL", src = "yahoo", from = "2018-01-01", auto.assign = TRUE)
# getSymbols("^FTSE", src = "yahoo", from = "2018-01-01", auto.assign = TRUE)
# 
# # Extract adjusted close prices
# aapl_prices <- Ad(AAPL)
# ftse_prices <- Ad(FTSE)
# 
# # Merge and proceed with analysis
# custom_data <- merge(ftse_prices, aapl_prices)
# colnames(custom_data) <- c("FTSE", "AAPL")

# ============================================
# EXTENSION 2: ROLLING STATISTICS
# ============================================

# Calculate 252-day rolling statistics
rolling_mean_sp500 <- rollapply(returns_xts$SP500_log, width = 252,
                                 FUN = mean, align = "right", fill = NA)
rolling_sd_sp500 <- rollapply(returns_xts$SP500_log, width = 252,
                               FUN = sd, align = "right", fill = NA)

rolling_mean_msft <- rollapply(returns_xts$MSFT_log, width = 252,
                                FUN = mean, align = "right", fill = NA)
rolling_sd_msft <- rollapply(returns_xts$MSFT_log, width = 252,
                              FUN = sd, align = "right", fill = NA)

# Plot rolling statistics
par(mfrow = c(2, 2))

plot(rolling_mean_sp500, main = "S&P 500: Rolling 252-day Mean",
     ylab = "Mean Return", col = "blue")
plot(rolling_sd_sp500, main = "S&P 500: Rolling 252-day Volatility",
     ylab = "Volatility", col = "blue")

plot(rolling_mean_msft, main = "MSFT: Rolling 252-day Mean",
     ylab = "Mean Return", col = "red")
plot(rolling_sd_msft, main = "MSFT: Rolling 252-day Volatility",
     ylab = "Volatility", col = "red")

par(mfrow = c(1, 1))

# ============================================
# EXTENSION 3: CONDITIONAL CORRELATION ANALYSIS
# ============================================

# Define market conditions based on S&P500 returns
sp500_sd <- sd(returns$SP500_log)

# Classify days
bull_days <- returns$SP500_log > sp500_sd
bear_days <- returns$SP500_log < -sp500_sd
normal_days <- abs(returns$SP500_log) <= sp500_sd

# Calculate correlations for each condition
cor_bull <- cor(returns$MSFT_log[bull_days], returns$SP500_log[bull_days])
cor_bear <- cor(returns$MSFT_log[bear_days], returns$SP500_log[bear_days])
cor_normal <- cor(returns$MSFT_log[normal_days], returns$SP500_log[normal_days])

cat("\n--- Conditional Correlation Analysis ---\n")
cat("Correlation on bull days (S&P500 > +1 SD):", round(cor_bull, 4), "\n")
cat("Correlation on bear days (S&P500 < -1 SD):", round(cor_bear, 4), "\n")
cat("Correlation on normal days (|S&P500| < 1 SD):", round(cor_normal, 4), "\n")

# Visualize
boxplot(returns$MSFT_log ~ cut(returns$SP500_log,
                                breaks = c(-Inf, -sp500_sd, sp500_sd, Inf),
                                labels = c("Bear", "Normal", "Bull")),
        main = "MSFT Returns by Market Condition",
        xlab = "Market Condition (based on S&P500)",
        ylab = "MSFT Return",
        col = c("red", "gray", "green"))

cat("\n--- Analysis Complete! ---\n")
