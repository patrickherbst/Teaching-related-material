********************************************************************************
* ACCP009: Empirical Methods in Finance and Accounting
* Week 1 Practice Tasks - Stata Solutions
* Spring 2026
********************************************************************************

clear all
set more off
capture log close

* Set working directory (adjust to your file location)
* cd "path/to/your/data"

********************************************************************************
* TASK 1: DATA IMPORT AND INITIAL EXPLORATION
********************************************************************************

* 1.1 Import the data
import delimited "data1.csv", clear

* Convert date string to Stata date format
generate date_stata = date(date, "YMD")
format date_stata %td
drop date
rename date_stata date

* Check for missing values
misstable summarize

* 1.2 Examine the data structure
describe
summarize

display "Number of observations: " _N
summarize date
display "Date range: " %td r(min) " to " %td r(max)

* Display first and last 10 rows
list in 1/10
list in -10/l

* 1.3 Create price charts

* Plot S&P 500 prices
tsset date
tsline sp500, title("S&P 500 Index Prices") ///
    ytitle("Price") name(sp500_price, replace)

* Plot MSFT prices
tsline msft, title("Microsoft Stock Prices") ///
    ytitle("Price") name(msft_price, replace)

* Create rebased prices for combined plot
generate sp500_rebased = 100 * sp500 / sp500[1]
generate msft_rebased = 100 * msft / msft[1]

* Combined plot
tsline sp500_rebased msft_rebased, ///
    title("S&P 500 vs MSFT (Rebased to 100)") ///
    legend(label(1 "S&P 500") label(2 "MSFT")) ///
    name(combined_rebased, replace)

********************************************************************************
* TASK 2: RETURNS CALCULATION
********************************************************************************

* 2.1 Calculate simple net returns
generate sp500_simple = (sp500 - sp500[_n-1]) / sp500[_n-1]
generate msft_simple = (msft - msft[_n-1]) / msft[_n-1]

* 2.2 Calculate log returns
generate sp500_log = ln(sp500 / sp500[_n-1])
generate msft_log = ln(msft / msft[_n-1])

* Alternative calculation
* generate sp500_log = ln(sp500) - ln(sp500[_n-1])
* generate msft_log = ln(msft) - ln(msft[_n-1])

* 2.3 Compare simple and log returns

* Scatter plot: Simple vs Log returns for S&P 500
scatter sp500_simple sp500_log, ///
    title("S&P 500: Simple vs Log Returns") ///
    xtitle("Log Return") ytitle("Simple Return") ///
    name(sp500_compare, replace) || ///
    function y=x, range(sp500_log) lcolor(red)

* Scatter plot: Simple vs Log returns for MSFT
scatter msft_simple msft_log, ///
    title("MSFT: Simple vs Log Returns") ///
    xtitle("Log Return") ytitle("Simple Return") ///
    name(msft_compare, replace) || ///
    function y=x, range(msft_log) lcolor(red)

* Calculate differences
generate sp500_diff = sp500_simple - sp500_log
generate msft_diff = msft_simple - msft_log

* Maximum absolute difference
summarize sp500_diff, detail
display "S&P 500 max absolute difference: " r(max) - r(min)

summarize msft_diff, detail
display "MSFT max absolute difference: " r(max) - r(min)

* 2.4 Plot return series

* Time series plots of simple returns
tsline sp500_simple, title("S&P 500 Simple Returns") ///
    ytitle("Return") name(sp500_simple_ts, replace)
    
tsline msft_simple, title("MSFT Simple Returns") ///
    ytitle("Return") name(msft_simple_ts, replace)

* Time series plots of log returns
tsline sp500_log, title("S&P 500 Log Returns") ///
    ytitle("Return") name(sp500_log_ts, replace)
    
tsline msft_log, title("MSFT Log Returns") ///
    ytitle("Return") name(msft_log_ts, replace)

********************************************************************************
* TASK 3: DESCRIPTIVE STATISTICS
********************************************************************************

* 3.1 & 3.2 Calculate basic statistics and distributional properties

display _newline "--- Descriptive Statistics (Log Returns) ---"

* S&P 500 statistics
summarize sp500_log, detail
display "S&P 500 Statistics:"
display "  Mean: " r(mean)
display "  Median: " r(p50)
display "  SD: " r(sd)
display "  Min: " r(min)
display "  Max: " r(max)
display "  Range: " r(max) - r(min)
display "  Skewness: " r(skewness)
display "  Kurtosis: " r(kurtosis)

* MSFT statistics
summarize msft_log, detail
display "MSFT Statistics:"
display "  Mean: " r(mean)
display "  Median: " r(p50)
display "  SD: " r(sd)
display "  Min: " r(min)
display "  Max: " r(max)
display "  Range: " r(max) - r(min)
display "  Skewness: " r(skewness)
display "  Kurtosis: " r(kurtosis)

* 3.3 Annualise returns and volatility

* Calculate annualised values
quietly summarize sp500_log
local annual_return_sp500 = r(mean) * 252
local annual_vol_sp500 = r(sd) * sqrt(252)

quietly summarize msft_log
local annual_return_msft = r(mean) * 252
local annual_vol_msft = r(sd) * sqrt(252)

display _newline "--- Annualised Statistics ---"
display "S&P 500: Annual Return = " %6.4f `annual_return_sp500' ///
    ", Annual Volatility = " %6.4f `annual_vol_sp500'
display "MSFT: Annual Return = " %6.4f `annual_return_msft' ///
    ", Annual Volatility = " %6.4f `annual_vol_msft'

********************************************************************************
* TASK 4: DISTRIBUTION ANALYSIS
********************************************************************************

* 4.1 Create histograms with normal distribution overlay

* S&P 500 histogram with normal overlay
histogram sp500_log, normal ///
    title("S&P 500 Log Returns") ///
    xtitle("Return") ///
    name(sp500_hist, replace)

* MSFT histogram with normal overlay
histogram msft_log, normal ///
    title("MSFT Log Returns") ///
    xtitle("Return") ///
    name(msft_hist, replace)

* 4.2 Create Q-Q plots

* S&P 500 Q-Q plot
qnorm sp500_log, title("Q-Q Plot: S&P 500") ///
    name(sp500_qq, replace)

* MSFT Q-Q plot
qnorm msft_log, title("Q-Q Plot: MSFT") ///
    name(msft_qq, replace)

* 4.3 Formal normality tests (Jarque-Bera)

* Note: Stata's sktest performs similar normality test
* For Jarque-Bera specifically, we calculate manually

quietly summarize sp500_log, detail
local n_sp500 = r(N)
local skew_sp500 = r(skewness)
local kurt_sp500 = r(kurtosis)
local jb_sp500 = `n_sp500' * ((`skew_sp500'^2 / 6) + ((`kurt_sp500' - 3)^2 / 24))
local pval_sp500 = chi2tail(2, `jb_sp500')

quietly summarize msft_log, detail
local n_msft = r(N)
local skew_msft = r(skewness)
local kurt_msft = r(kurtosis)
local jb_msft = `n_msft' * ((`skew_msft'^2 / 6) + ((`kurt_msft' - 3)^2 / 24))
local pval_msft = chi2tail(2, `jb_msft')

display _newline "--- Jarque-Bera Test Results ---"
display "S&P 500:"
display "  JB statistic: " %8.2f `jb_sp500'
display "  p-value: " %6.4f `pval_sp500'
display "  Conclusion: " cond(`pval_sp500' < 0.05, "REJECT normality", "Cannot reject normality")

display _newline "MSFT:"
display "  JB statistic: " %8.2f `jb_msft'
display "  p-value: " %6.4f `pval_msft'
display "  Conclusion: " cond(`pval_msft' < 0.05, "REJECT normality", "Cannot reject normality")

* Alternative: Use built-in sktest
sktest sp500_log
sktest msft_log

********************************************************************************
* TASK 5: CORRELATION ANALYSIS
********************************************************************************

* 5.1 Calculate Pearson correlation and test significance

correlate msft_log sp500_log, covariance
correlate msft_log sp500_log

* Store correlation for later use
quietly correlate msft_log sp500_log
local rho = r(rho)

* Calculate t-statistic for correlation significance
quietly count if !missing(msft_log) & !missing(sp500_log)
local n = r(N)
local t_stat = `rho' * sqrt((`n' - 2) / (1 - `rho'^2))
local df = `n' - 2
local p_value = 2 * ttail(`df', abs(`t_stat'))

display _newline "--- Correlation Analysis ---"
display "Correlation coefficient: " %6.4f `rho'
display "t-statistic: " %8.4f `t_stat'
display "p-value: " %6.4f `p_value'
display "Conclusion: " cond(`p_value' < 0.05, "Statistically significant", "NOT significant")

* 5.2 Visualise the relationship

* Scatter plot with regression line
twoway (scatter msft_log sp500_log, mcolor(blue%30)) ///
    (lfit msft_log sp500_log, lcolor(red)) ///
    (function y=x, range(sp500_log) lcolor(gray) lpattern(dash)), ///
    title("MSFT vs S&P 500 Returns (ρ = `: display %4.3f `rho'')") ///
    xtitle("S&P 500 Log Return") ytitle("MSFT Log Return") ///
    legend(label(1 "Data") label(2 "Regression line") label(3 "y = x")) ///
    name(correlation_plot, replace)

* 5.3 Calculate alternative correlation measures (Optional)

* Spearman's rank correlation
spearman msft_log sp500_log
display "Spearman's rho: " r(rho)
display "p-value: " r(p)

* Kendall's tau
ktau msft_log sp500_log
display "Kendall's tau: " r(tau_a)

********************************************************************************
* TASK 6: COMPARISON ACROSS FREQUENCIES (OPTIONAL)
********************************************************************************

* 6.1 Aggregate to monthly returns

* Generate month variable
generate month = mofd(date)
format month %tm

* Sum log returns by month
preserve
collapse (sum) sp500_log msft_log, by(month)
rename sp500_log sp500_log_monthly
rename msft_log msft_log_monthly

* 6.2 Repeat descriptive analysis

display _newline "--- Monthly Returns Statistics ---"

summarize sp500_log_monthly, detail
display "S&P 500 Monthly:"
display "  Mean: " r(mean)
display "  SD: " r(sd)
display "  Skewness: " r(skewness)
display "  Kurtosis: " r(kurtosis)

summarize msft_log_monthly, detail
display "MSFT Monthly:"
display "  Mean: " r(mean)
display "  SD: " r(sd)
display "  Skewness: " r(skewness)
display "  Kurtosis: " r(kurtosis)

* Histograms for monthly returns
histogram sp500_log_monthly, normal ///
    title("S&P 500 Monthly Log Returns") ///
    name(sp500_monthly_hist, replace)

histogram msft_log_monthly, normal ///
    title("MSFT Monthly Log Returns") ///
    name(msft_monthly_hist, replace)

* Jarque-Bera tests for monthly
quietly summarize sp500_log_monthly, detail
local jb_monthly_sp500 = r(N) * ((r(skewness)^2 / 6) + ((r(kurtosis) - 3)^2 / 24))
local pval_monthly_sp500 = chi2tail(2, `jb_monthly_sp500')

quietly summarize msft_log_monthly, detail
local jb_monthly_msft = r(N) * ((r(skewness)^2 / 6) + ((r(kurtosis) - 3)^2 / 24))
local pval_monthly_msft = chi2tail(2, `jb_monthly_msft')

display _newline "--- Monthly Jarque-Bera Tests ---"
display "S&P 500 p-value: " %6.4f `pval_monthly_sp500'
display "MSFT p-value: " %6.4f `pval_monthly_msft'

restore

********************************************************************************
* EXTENSION 1: DOWNLOAD YOUR OWN DATA
********************************************************************************

* Note: Stata does not have built-in Yahoo Finance download
* You would need to:
* 1. Download data manually from Yahoo Finance or other sources
* 2. Save as CSV
* 3. Import using: import delimited "your_data.csv", clear

********************************************************************************
* EXTENSION 2: ROLLING STATISTICS
********************************************************************************

* Calculate 252-day rolling mean and standard deviation
tsset date

* Rolling mean (252-day window)
tssmooth ma sp500_rolling_mean = sp500_log, window(251 1 0)
tssmooth ma msft_rolling_mean = msft_log, window(251 1 0)

* Rolling standard deviation (need to calculate manually)
* This is a simplified version - for exact rolling SD, use rangestat package

* Plot rolling means
tsline sp500_rolling_mean, title("S&P 500: Rolling 252-day Mean") ///
    ytitle("Mean Return") name(sp500_roll_mean, replace)
    
tsline msft_rolling_mean, title("MSFT: Rolling 252-day Mean") ///
    ytitle("Mean Return") name(msft_roll_mean, replace)

********************************************************************************
* EXTENSION 3: CONDITIONAL CORRELATION ANALYSIS
********************************************************************************

* Define market conditions based on S&P500 returns
quietly summarize sp500_log
local sp500_sd = r(sd)

* Classify days
generate market_condition = "Normal"
replace market_condition = "Bull" if sp500_log > `sp500_sd' & !missing(sp500_log)
replace market_condition = "Bear" if sp500_log < -`sp500_sd' & !missing(sp500_log)

* Calculate correlations by market condition
display _newline "--- Conditional Correlation Analysis ---"

quietly correlate msft_log sp500_log if market_condition == "Bull"
display "Correlation on bull days: " %6.4f r(rho)

quietly correlate msft_log sp500_log if market_condition == "Bear"
display "Correlation on bear days: " %6.4f r(rho)

quietly correlate msft_log sp500_log if market_condition == "Normal"
display "Correlation on normal days: " %6.4f r(rho)

* Visualize with box plot
graph box msft_log, over(market_condition) ///
    title("MSFT Returns by Market Condition") ///
    ytitle("MSFT Return") ///
    name(conditional_box, replace)

display _newline "--- Analysis Complete! ---"

* Close log if opened
* log close
