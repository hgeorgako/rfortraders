# CHAPTER 5
# Intermediate Statistics and Probability

#############################
# Stock price distributions #
#############################
# Extract prices and compute statistics
prices <- SPY$SPY.Adjusted
mean_prices <- round(mean(prices), 2)
sd_prices <- round(sd(prices), 2)

# Plot the histogram along with a legend
hist(prices, breaks = 100, prob=T, cex.main = 0.9)
abline(v = mean_prices, lwd = 2)
legend("topright", cex = 0.8, border = NULL, bty = "n",
  paste("mean=", mean_prices, "; sd=", sd_prices))

plot_4_ranges <- function(data, start_date, end_date, title) {

  # Set the plot window to be 2 rows and 2 columns
  par(mfrow = c(2, 2))
  for(i in 1:4) {
    # Create a string with the appropriate date range
    range <- paste(start_date[i], "::", end_date[i], sep = "")

    # Create the price vector and necessary statistics
    time_series <- data[range]

    mean_data <- round(mean(time_series, na.rm = TRUE), 3)
    sd_data <- round(sd(time_series, na.rm = TRUE), 3)

    # Plot the histogram along with a legend
    hist_title <- paste(title, range)
    hist(time_series, breaks = 100, prob=TRUE,
     xlab = "", main = hist_title, cex.main = 0.8)
    legend("topright", cex = 0.7, bty = 'n',
     paste("mean=", mean_data, "; sd=", sd_data))
  }

  # Reset the plot window
  par(mfrow = c(1, 1))
}

# Define start and end dates of interest
begin_dates <- c("2007-01-01", "2008-06-06",
  "2009-10-10", "2011-03-03")
end_dates <- c("2008-06-05", "2009-09-09",
  "2010-12-30", "2013-01-06")

# Create plots
plot_4_ranges(prices, begin_dates,
  end_dates, "SPY prices for:")

################
# Stationarity #
################
# Compute log returns
returns <- diff(log(prices))

# Use the same function as before to plot returns rather than prices
plot_4_ranges(returns, begin_dates, end_dates, "SPY log prices for:")

######################################
# Determining stationarity with urca #
######################################
# Get SPY data and let's confirm that it is non-stationary
require(quantmod)
getSymbols("SPY")
spy <- SPY$SPY.Adjusted

# Use the default settings
require(urca)
test <- ur.kpss(as.numeric(spy))

# The output is an S4 object
class(test)
## [1] "ur.kpss"
## attr(,"package")
## [1] "urca"

# Extract the test statistic
test@teststat
## [1] 11.63543

# Look at the critical values
test@cval
##                10pct  5pct 2.5pct  1pct
## critical values 0.347 0.463  0.574 0.739

spy_returns <- diff(log(spy))

# Test on the returns
test_returns <- ur.kpss(as.numeric(spy_returns))
test_returns@teststat
## [1] 0.336143

test_returns@cval
##                10pct  5pct 2.5pct  1pct
## critical values 0.347 0.463  0.574 0.739

test_post_2013 <- ur.kpss(as.numeric(spy_returns['2013::']))
test_post_2013@teststat
## [1] 0.06936672

############################
# Assumptions of normality #
############################
# Plot histogram and density
mu <- mean(returns, na.rm = TRUE)
sigma <- sd(returns, na.rm = TRUE)
x <- seq(-5 * sigma, 5 * sigma, length = nrow(returns))

hist(returns, breaks = 100,
  main = "Histogram of returns for SPY",
  cex.main = 0.8, prob=TRUE)
lines(x, dnorm(x, mu, sigma), col = "red", lwd = 2)

 # Set plotting window
par(mfrow = c(1, 2))

# SPY data
qqnorm(as.numeric(returns),
  main = "SPY empirical returns qqplot()",
  cex.main = 0.8)
qqline(as.numeric(returns),  lwd = 2)
grid()

# Normal random data
normal_data <- rnorm(nrow(returns), mean = mu, sd = sigma)

qqnorm(normal_data, main = "Normal returns", cex.main = 0.8)
qqline(normal_data, lwd = 2)
grid()

answer <- shapiro.test(as.numeric(returns))

answer[[2]]
## [1] 5.118396e-34

set.seed(129)
normal_numbers <- rnorm(5000, 0, 1)
ans <- shapiro.test(normal_numbers)

ans[[2]]
## [1] 0.9963835

# Corrupt a single data point
normal_numbers[50] <- 1000
ans <- shapiro.test(normal_numbers)

ans[[2]]
## [1] 1.775666e-95

###############
# Correlation #
###############
sv <- as.xts(returns_matrix[, c(1, 6)])

head(sv)
##               SPY.Close    VXX.Close
## 2009-02-02 -0.003022794 -0.003160468
## 2009-02-03  0.013949240 -0.047941603
## 2009-02-04 -0.004908132  0.003716543
## 2009-02-05  0.014770965 -0.006134680

cor(sv)
##            SPY.Close  VXX.Close
## SPY.Close  1.0000000 -0.4603908
## VXX.Close -0.4603908  1.0000000

##################
# Filtering data #
##################
# Find the outliers
outliers <- which(sv[, 2] > 1.0)

# If any outliers exist, remove them
if(length(outliers) > 0) {
  sv <- sv[-outliers, ]
}

cor(sv)
##            SPY.Close  VXX.Close
## SPY.Close  1.0000000 -0.8066466
## VXX.Close -0.8066466  1.0000000

##############
# R formulas #
##############
# Create a formula
my_formula <- as.formula("y ~ x")

# What is the output?
my_formula
## y ~ x

# What is the class of my_formula?
class(my_formula)
## [1] "formula"

# Create a linear regression object
reg <- lm(VXX.Close ~ SPY.Close, data = sv)

# Here is the output
summary(reg)

## Call:
## lm(formula = VXX.Close ~ SPY.Close, data = sv)

## Residuals:
##    Min        1Q    Median        3Q       Max
## -0.085607 -0.012830 -0.000865  0.012188  0.116349

## Coefficients:
##            Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.0024365  0.0006641  -3.669 0.000254 ***
## SPY.Close   -2.5848492  0.0552193 -46.811  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1

## Residual standard error: 0.02287 on 1187 degrees of freedom
## Multiple R-squared:  0.6486,Adjusted R-squared:  0.6483
## F-statistic:  2191 on 1 and 1187 DF,  p-value: < 2.2e-16

b <- reg$coefficients[1]
a <- reg$coefficients[2]

par(mfrow = c(2, 2))
plot(reg$residuals,
  main = "Residuals through time",
  xlab = "Days", ylab = "Residuals")
hist(reg$residuals, breaks = 100,
  main = "Distribution of residuals",
  xlab = "Residuals")
qqnorm(reg$residuals)
qqline(reg$residuals)
acf(reg$residuals, main = "Autocorrelation")


vxx_lag_1 <- lag(VXX$VXX.Close, k = 1)

head(vxx_lag_1)
##               VXX.Close
## 2009-01-30    NA
## 2009-02-02    104.58
## 2009-02-03    104.25
## 2009-02-04    99.37
## 2009-02-05    99.74
## 2009-02-06    99.13

head(VXX$VXX.Close)
##               VXX.Close
## 2009-01-30    104.58
## 2009-02-02    104.25
## 2009-02-03    99.37
## 2009-02-04    99.74
## 2009-02-05    99.13
## 2009-02-06    97.70

# Merge returns with lagged returns
sv <- merge(sv, lag(sv))

# Scatter plot of lagged SPY vs. VXX
plot(as.numeric(sv[, 3]), as.numeric(sv[, 2]),
  main = "Scatter plot SPY lagged vs. VXX.",
  xlab = "SPY lagged",
  ylab = "VXX"
  cex.main = 0.8,
  cex.axis = 0.8,
  cex.lab = 0.8)
grid()

reg2 <- lm(VXX.Close ~ SPY.Close.1, data = sv)

summary(reg2)
## Coefficients:
##           Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.004140   0.001121  -3.694 0.000231 ***
## SPY.Close.1  0.104119   0.093154   1.118 0.263918

## Residual standard error: 0.03857 on 1186 degrees of freedom
## (1 observation deleted due to missingness)
## Multiple R-squared:  0.001052,Adjusted R-squared:  0.00021
## F-statistic: 1.249 on 1 and 1186 DF,  p-value: 0.2639

ccf(as.numeric(sv[, 1]), as.numeric(sv[, 2]),
  main = "Cross correlation between SPY and VXX",
  ylab = "Cross correlation", xlab = "Lag", cex.main = 0.8,
  cex.lab = 0.8, cex.axis = 0.8)

###################################
# The linear in linear regression #
###################################
x <- seq(1:100)
y <- x ^ 2

# Generate the plot
plot(x, y)

# Fit the regression
reg_parabola <- lm(y ~ x)

# Superimpose the best fit line on the plot
abline(reg_parabola, lwd = 2)

# Look at the results
summary(reg_parabola)
## Coefficients:
##               Estimate    Std. Error t value Pr(>|t|)
## (Intercept)  -1717.000   151.683  -11.32   <2e-16 ***
## x              101.000     2.608   38.73   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1

## Residual standard error: 752.7 on 98 degrees of freedom
## Multiple R-squared:  0.9387,Adjusted R-squared:  0.9381
## F-statistic:  1500 on 1 and 98 DF,  p-value: < 2.2e-16

plot(x, sqrt(y))
reg_transformed <- lm(sqrt(y) ~ x)
abline(reg_transformed)

summary(reg_transformed)
## Coefficients:
##               Estimate Std. Error  t value     Pr(>|t|)
## (Intercept) -5.684e-14  5.598e-15 -1.015e+01   <2e-16 ***
## x            1.000e+00  9.624e-17  1.039e+16   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1

## Residual standard error: 2.778e-14 on 98 degrees of freedom
## Multiple R-squared:      1,Adjusted R-squared:      1
## F-statistic: 1.08e+32 on 1 and 98 DF,  p-value: < 2.2e-16

##############
# Volatility #
##############
# Generate 1000 IID numbers from a normal distribution.
z <- rnorm(1000, 0, 1)

# Autocorrelation of returns and squared returns
par(mfrow = c(2, 1))
acf(z, main = "returns", cex.main = 0.8,
  cex.lab = 0.8, cex.axis = 0.8)

grid()
acf(z ^ 2, main = "returns squared",
  cex.lab = 0.8, cex.axis = 0.8)
grid()

par(mfrow = c(1, 1))
acf(sv[, 1] ^ 2, main = "Actual returns squared",
  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
grid()

par(mfrow = c(1, 2))
acf(sv[, 1]^3)
acf(abs(sv[, 1])



























