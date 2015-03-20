# CHAPTER 8
# High-Frequency Data

#########################
# High-frequency quotes #
#########################

# The following data is not available for public use. 
# Those interested in using tick data can contact Tick Data, Inc.
spy_file <- "Chapter_08/SPY_2013_04_15_X_Q.asc"
spy_quotes <- read.csv(file = spy_file, header = FALSE,
  stringsAsFactors = FALSE)

head(spy_quotes, 3)
##           V1               V2  V3      V4   V5 V6 V7
## 1 04/15/2013 04:00:00.065  T 156.60   0.00  1  0  R
## 2 04/15/2013 04:00:00.626  P   0.00   0.00  0  0  R
## 3 04/15/2013 04:00:00.633  P 158.25 158.90  1 47  R

## V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21
## NA   1   T   T   1   2   A   C    NA   NA   NA   NA
## NA   2   P   P   0   2   A   C    NA   NA   NA   NA
## NA   3   P   P   1   2   A   C    NA   NA   NA   NA

names(spy_quotes) <- c("date", "time", "exchange", "bid_price",
  "ask_price", "bid_size", "ask_size", "quote_condition",
  "mode", "market_maker_id", "sequence_number", "bid_exchange",
  "ask_exchange",  "national_bbo_indicator",
  "nasdaq_bbo_indicator", "quote_cancel_correction",
  "quote_source", "short_sale_restriction_indicator",
  "limit_up_down_bbo_indicator_cqs",
  "limit_up_down_bbo_indicator_utp",
  "finra_adf_mpid_indicator")

spy_quotes_arca <- spy_quotes[spy_quotes$exchange %in% c("P"),
  c("date", "time", "bid_price",
  "ask_price", "bid_size", "ask_size")]

require(xts)

# Setting to allow us to view the millisecond precision
options(digits.secs = 3)

time_index <- as.POSIXct(paste(spy_quotes_arca$date,
  spy_quotes_arca$time), format = "%m/%d/%Y %H:%M:%OS")
spy <- xts(spy_quotes_arca[, -c(1, 2)], time_index)
rm(time_index)

plot(spy$bid_price, type = 'l',
  main = "SPY bid price",
  cex.main = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8)

spy_filtered <- spy[spy$bid_price > 0, ]

rows_removed <- nrow(spy) - nrow(spy_filtered)

rows_removed
## [1] 2

plot(spy_filtered$bid_price, type = 'l',
  main = "SPY filtered bid price",
  cex.main = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8)

summary(as.numeric(spy_filtered$ask_price))
  ## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
  ## 154.3   156.1   156.7   156.8   157.7   158.9

summary(as.numeric(spy_filtered$bid_size))
## Min. 1st Qu.  Median  Mean 3rd Qu.  Max.
## 1.00 22.00 52.00 94.93 100.00 1565.00

summary(as.numeric(spy_filtered$ask_size))
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## 1.0  24.0  59.0  110.8 118.0 1412.0

# Boolean vector where bid price >= the ask price
crossed_prices <- spy_filtered$bid_price >=
  spy_filtered$ask_price

any(crossed_prices)
## [1] FALSE

#############################
# Inter-quote arrival times #
#############################
# Extract the time index.
quote_times <- index(spy_filtered)

# Compute the consecutive time differences
time_differences <- as.numeric(diff(quote_times))

summary(time_differences)
## Min.   1st Qu. Median   Mean   3rd Qu.    Max.
## 0.0000 0.0000  0.0010  0.0645  0.0100  1010.0000

# Identify times that seem abnormal
long_times <- which(time_differences > 1000)
long_times
## [1] 884312

# Show the data around the abnormally long time
spy_abnormal <- spy_filtered[(long_times - 2):(long_times + 2), ]
##                            bid_price ask_price bid_size ask_size
## 2013-04-15 16:13:00.987    155.00    155.02      289      270
## 2013-04-15 16:13:01.256    155.00    155.02      290      270
## 2013-04-15 16:13:01.282    155.00    155.02      295      270
## 2013-04-15 16:29:50.869    154.76    154.79        3        3
## 2013-04-15 16:29:50.887    154.76    154.77        3       10

#################################
# Identifying liquidity regimes #
#################################
# Calculate the bid-ask spread.
bid_ask_spread <- spy_filtered$ask_price -
  spy_filtered$bid_price

# Filter out abnormal value
outliers <- which(bid_ask_spread > 0.25)
if(length(outliers) > 0) {
  bid_ask_spread <- bid_ask_spread[-outliers, ]
}

# Plot the spread.
plot(bid_ask_spread, type = "l",
  main = "Bid ask spread",
  cex.main = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8)

# Create three time partitions for the SPY data
early_morning <- "2013-04-15 04:00:00::2013-04-15 08:29:00"
regular_trading <- "2013-04-15 08:30:00::2013-04-15 16:15:00"
after_hours <- "2013-04-15 16:15:01::2013-04-15 20:00:00"

# Create a histogram of the bid-ask spread for each period
par(mfrow = c(3, 1))

# Morning
data <- bid_ask_spread[early_morning]
hist(data, main = early_morning, breaks = 1000,
  xlim = c(0, 0.1))
abline(v = mean(data), lwd = 2, lty = 2)

# Afternoon
data <- bid_ask_spread[regular_trading]
hist(data, main = regular_trading, breaks = 1000,
  xlim = c(0, 0.1))
abline(v = mean(data), lwd = 2, lty = 2)

# Evening
data <- bid_ask_spread[after_hours]
hist(data, main = after_hours, breaks = 1000,
  xlim = c(0, 0.1))
abline(v = mean(data), lwd = 2, lty = 2)

spy_day <- spy_filtered[regular_trading]

###################
# The micro-price #
###################
spy_micro_price <- (spy_day$bid_price * spy_day$ask_size +
  spy_day$ask_price * spy_day$bid_size) /
  (spy_day$bid_size + spy_day$ask_size)

par(mfrow = c(1, 1))
range <- 10000:10100
title <- "Micro-price between bid-ask prices"

plot(spy_day$ask_price[range],
  ylim = c(min(spy_day$bid_price[range]),
  max(spy_day$ask_price[range])),
  main = title,
  cex.main = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8)
lines(spy_day$bid_price[range])
lines(spy_micro_price[range], lty = 2)

######################################
# Distributions and autocorrelations #
######################################
spy_returns <- diff(log(spy_micro_price))

par(mfrow = c(2, 1))
plot(spy_returns,
  main = "Time series plot of micro-price returns",
  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
hist(spy_returns, breaks = 1000,
  main = "Micro-price distribution",
  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

par(mfrow = c(1, 1))
mean_spy <- mean(as.numeric(spy_returns), na.rm = TRUE)
sd_spy <- sd(as.numeric(spy_returns), na.rm = TRUE)

hist(spy_returns, breaks = 10000, prob = TRUE,
  xlim = c(-0.00003, 0.00003),
  main = "Micro-price distribution vs. Normal",
  cex.main = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8)

curve(dnorm(x, mean_spy, sd_spy), add = TRUE,
  yaxt = "n", lwd = 3, lty = 3)

spy_acf <- acf(as.numeric(spy_returns),
  na.action = na.pass,
  main = "Autocorrelation",
  cex.main = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8)

# Get the SPY traded prices from our tick data file
spy_trades_file <- "path/SPY_2013_04_15_X_T.asc"
spy_trades <- read.csv(file = spy_trades_file,
  header = FALSE, stringsAsFactors = FALSE)

names(spy_trades) <- c("date", "time", "price",
  "volume", "exchange", "sales_condition",
  "correction_indicator", "sequence_number",
  "trade_stop_indicator", "source_of_trade", "trf",
  "exclude_record_flag", "filtered_price")

# Extract only the ARCA trades
spy_trades_arca <- spy_trades[spy_trades$exchange %in% c("P"),
  c("date", "time", "price", "volume", "correction_indicator",
  "filtered_price")]

# Check if any filtered prices exist
any(!is.na(spy_trades_arca$filtered_price))
## [1] FALSE

# Check if there are any special correction indicators present
unique(spy_trades_arca$correction_indicator)
## [1] 0

# Drop the last two columns from the data frame
spy_trades_arca <- spy_trades_arca[, 1:4]

# Convert to an xts object for subsequent analysis
time_index <- as.POSIXct(paste(spy_trades_arca$date,
  spy_trades_arca$time), format = "%m/%d/%Y %H:%M:%OS")

spy_t <- xts(spy_trades_arca[, -c(1, 2)], time_index)
rm(time_index)

# First 6 entries
head(spy_t)
##                          price volume
## 2013-04-15 04:00:00.697 158.25    100
## 2013-04-15 04:00:00.697 158.24    200
## 2013-04-15 04:00:00.697 158.15    150
## 2013-04-15 04:01:42.190 158.06    200
## 2013-04-15 04:07:16.545 157.94    100
## 2013-04-15 04:12:45.265 157.92  10000

# Subset to regular trading hour range
regular_trading <- "2013-04-15 08:30:00::2013-04-15 16:15:00"
spy_t_day <- spy_t[regular_trading]

# Look at the number of trade entries
dim(spy_t_day)
## [1] 93197     2

# Look at the amount of memory taken up
object.size(spy_t_day)
## [1] 2239080 bytes

# Compute returns
spy_t_day_returns <- diff(log(spy_t_day$price))[-1]

# Plot the distribution and the autocorrelation plot
par(mfrow = c(2, 1))
plot(spy_t_day_returns, main = "SPY returns on trades",
  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
acf(as.numeric(spy_t_day_returns), main = "SPY trades acf",
  cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

# Distribution of trade returns
par(mfrow = c(1, 1))
hist(spy_t_day_returns, breaks = 1000, prob = TRUE,
  xlim = c(-0.0001, 0.0001),
  main = "Distribution of SPY trade returns",
  cex.main = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8)

curve(dnorm(x, mean(spy_t_day_returns),
  sd(spy_t_day_returns)),
  add = TRUE,
  yaxt = "n",
  lwd = 3,
  lty = 3)

# Use the rle() function to find price sequences
prices_rle <- rle(as.numeric(spy_t_day$price))

# Here are the row indexes we want to keep
end_indexes <- cumsum(prices_rle$lengths)

# Here are the start indexes we want to sum the volumes from
start_indexes <- end_indexes - prices_rle$lengths + 1

# Create a vector of total volumes for each price
volume_list <- list()
volume_vector <- as.numeric(spy_t_day$volume)
for (i in 1:length(end_indexes)) {
  volume_list[[i]] <- sum(volume_vector[start_indexes[i]:
    end_indexes[i]], na.rm = TRUE)
}

# Create a reduced data set with distinct trade prices
spy_t_day_reduced <- spy_t_day[end_indexes, ]
spy_t_day_reduced$volume <- unlist(volume_list)

head(spy_t_day_reduced, 10)
##                          price  volume
## 2013-04-15 08:30:01.964 158.17   510
## 2013-04-15 08:30:02.783 158.15   1000
## 2013-04-15 08:30:04.930 158.14   340
## 2013-04-15 08:30:11.964 158.12   100
## 2013-04-15 08:30:23.763 158.11   1100
## 2013-04-15 08:30:29.739 158.10   1720
## 2013-04-15 08:30:31.963 158.09   200
## 2013-04-15 08:30:45.164 158.08   4995
## 2013-04-15 08:30:46.888 158.07   100
## 2013-04-15 08:30:46.970 158.06   3330

head(spy_t_day, 10)
##                         price volume
## 2013-04-15 08:30:01.964 158.17   100
## 2013-04-15 08:30:01.964 158.17   410
## 2013-04-15 08:30:02.783 158.15   1000
## 2013-04-15 08:30:04.930 158.14   340
## 2013-04-15 08:30:11.964 158.12   100
## 2013-04-15 08:30:23.763 158.11   1000
## 2013-04-15 08:30:23.763 158.11   100
## 2013-04-15 08:30:28.153 158.10   400
## 2013-04-15 08:30:28.529 158.10   320
## 2013-04-15 08:30:29.739 158.10   180

# Identify the most traded prices throughout the day
hist(as.numeric(spy_t_day_reduced$price), breaks = 200,
  main = "Histogram of traded prices for SPY on 2013-04-15",
  cex.main = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8)

acf(diff(log(as.numeric(spy_t_day_reduced$price))),
  main = "Autocorrelation of trades",
  cex.main = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8)

# Random return cloud with lag 1
n <- rnorm(50, 0, .20)
n_lag1 <- c(NA, n[-length(n)])
plot(n_lag1, n)

# Create arrows between the points
s <- seq(length(n)-1)
arrows(n_lag1[s], n[s], n_lag1[s+1], n[s+1])

# SPY return cloud with lag 1
spy_t_returns <- diff(log(as.numeric(
  spy_t_day_reduced$price[100:150])))
spy_t_returns_lag1 <- c(NA, spy_t_returns[
  -length(spy_t_returns)])
plot(spy_t_returns_lag1, spy_t_returns)

s <- seq(length(spy_t_returns)-1)
arrows(spy_t_returns_lag1[s], spy_t_returns[s],
  spy_t_returns_lag1[s+1], spy_t_returns[s+1])

#############################
# The highfrequency package #
#############################
library("devtools")
install_github("highfrequency", "jonathancornelissen")





