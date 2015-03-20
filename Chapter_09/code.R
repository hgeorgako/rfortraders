# CHAPTER 9
# Options

########################
# Valuation of options #
########################
install.packages("RQuantLib")
library(RQuantLib)

lsf.str("package:RQuantLib")
## adjust : function (calendar = "TARGET", dates = Sys.Date(),
##   bdc = 0)
## advance : function (calendar = "TARGET", dates = Sys.Date(),
##   n, timeUnit, period, bdc = 0, emr = 0)
## AmericanOption : function (type, underlying, strike, dividendYield,
##   riskFreeRate, maturity, volatility, timeSteps = 150,
##   gridPoints = 149, engine = "BaroneAdesiWhaley")
## AmericanOption.default : function (type, underlying, strike,
##   dividendYield, riskFreeRate, maturity, volatility,
##   timeSteps = 150, gridPoints = 149, engine = "BaroneAdesiWhaley")
## AmericanOptionImpliedVolatility : function (type, value,
##   underlying, strike, dividendYield, riskFreeRate,
##   maturity, volatility, timeSteps = 150, gridPoints = 151)
## AmericanOptionImpliedVolatility.default : function (type, value,
##   underlying, strike, dividendYield, riskFreeRate, maturity,
##   volatility, timeSteps = 150, gridPoints = 151)
## ...

call_value <- EuropeanOption(type = "call", underlying = 100,
  strike = 100, dividendYield = 0, riskFreeRate = 0.03,
  maturity = 1.0, volatility = 0.30)

## Concise summary of valuation for EuropeanOption
## value      delta    gamma     vega    theta
## 13.2833   0.5987   0.0129  38.6668  -7.1976
## rho   divRho
## 46.5873 -59.8706

class(call_value)
## [1] "EuropeanOption" "Option"

type <- "call"
underlying <- 20:180
strike <- 100
dividendYield <- 0
riskFreeRate <- 0.03
maturity <- 1.0
volatility <- 0.10

# Function to create plots of option values and Greeks.
option_values <- function(type, underlying, strike,
  dividendYield, riskFreeRate, maturity, volatility) {

  # Output list with option values and Greeks
  out <- list()
  for(i in seq_along(underlying)) {
      out[[i]] <- EuropeanOption(type = type, underlying = i,
        strike = strike, dividendYield = dividendYield,
        riskFreeRate = riskFreeRate, maturity = maturity,
        volatility = volatility)
  }

  # Set up the plot window
  par(mfrow = c(3, 2))
  names <- c("Value", "Delta", "Gamma",
    "Vega", "Theta", "Rho")

  for(i in 1:6) {
    plot(unlist(lapply(out, "[", i)) , type = "l",
      main = paste(names[i], "vs. Underlying"),
      xlab = "Underlying", ylab = names[i])
      grid()
      abline(v = strike, col = "red")
  }
  return(out)
}

option_values(type, underlying, strike, dividendYield,
  riskFreeRate, maturity, volatility)
## ...
## [[96]]
## Concise summary of valuation for EuropeanOption
## value    delta    gamma     vega    theta
## 3.3493   0.4768   0.0415  38.2336  -3.1843
## rho   divRho
## 42.4222 -45.7715

## [[97]]
## Concise summary of valuation for EuropeanOption
## value    delta    gamma     vega    theta
## 3.8468   0.5181   0.0411  38.6575  -3.3252
## rho   divRho
## 46.4098 -50.2566

## [[98]]
## Concise summary of valuation for EuropeanOption
## value    delta    gamma     vega    theta
## 4.3853   0.5588   0.0403  38.6707  -3.4449
## rho   divRho
## 50.3788 -54.7642

option_values(type, underlying, strike, dividendYield,
  riskFreeRate, maturity = 0.1, volatility)

################################
# Exploring options trade data #
################################
# Create a vector of filenames
folder <- "path/Options/SPY_20130415_T/"
available_files <- list.files(folder)

# Explore the first few lines of the file
temp <- read.csv(file = paste0(folder, available_files[1]),
  header = FALSE, stringsAsFactors = FALSE)

column_names <- c("date", "time", "trade_indicator",
  "sequence_number", "option_exchange_code",
  "option_condition_code", "sale_price", "sale_size",
  "underlying_last_trade_price",
  "underling_last_trade_size",
  "stock_exchange_code", "stock_condition_code",
  "underlying_bid_price", "underlying_bid_size",
  "underlying_ask_price", "underlying_ask_size")
names(temp) <- column_names

output <- list()
for(i in 1:length(available_files)) {
  file_name <- available_files[i]

  type <- substr(file_name, 5, 5)
  date <- substr(file_name, 7, 14)
  date <- as.Date(date, format = "%Y%m%d")
  strike <- substr(file_name, 16, 26)
  strike <- strsplit(strike, "_XX")[[1]][1]

  temp <- read.csv(file = paste0(folder, file_name),
    header = FALSE, stringsAsFactors = FALSE)
  names(temp) <- column_names

  number_of_trades <- nrow(temp)
  avg_trade_price <- round(mean(temp$sale_price,
    na.rm = TRUE), 3)

  if(number_of_trades <= 1) {
    sd_trade_price <- 0
  } else {
    sd_trade_price <- round(sd(temp$sale_price,
      na.rm = TRUE), 3)
  }

  total_volume <- sum(temp$sale_size, na.rm = TRUE)
  avg_underlying_price <- round(mean(
    temp$underlying_bid_price, na.rm = TRUE), 2)
  underlying_range <- max(temp$underlying_ask_price) -
    min(temp$underlying_bid_price)

  output[[i]] <- data.frame(symbol = 'SPY', date = date,
    type = type, strike = strike,
    trades = number_of_trades,
    volume = total_volume,
    avg_price = avg_trade_price,
    sd_price = sd_trade_price,
    avg_stock_price = avg_underlying_price,
    stock_range = underlying_range,
    stringsAsFactors = FALSE)
}

# Convert the list into a table
results <- do.call(rbind, output)

head(results)
##  symbol       date type strike trades volume
## 1   SPY 2013-04-20  C  120.00   12    33000
## 2   SPY 2013-04-20  C  124.00    1       15
## 3   SPY 2013-04-20  C  130.00    1        2
## 4   SPY 2013-04-20  C  133.00    1        1
## 5   SPY 2013-04-20  C  140.00    1       95
## 6   SPY 2013-04-20  C  142.00    1        6

## avg_price sd_price avg_stock_price stock_range
## 35.973    0.261      155.74      0.68
## 31.380    0.000      155.44      0.01
## 26.210    0.000      156.24      0.02
## 24.600    0.000      157.58      0.01
## 16.465    0.751      156.44      1.51
## 13.920    0.000      155.87      0.01

unique_maturities <- unique(results$date)

today <- as.Date("2013-04-15")
days_to_expiration <- as.Date(unique_maturities[1]) - today

# Extract only the relevant maturity range
single_maturity_table <- results[results$date ==
  unique_maturities[1], ]

# Look at the calls and puts separately
calls <- single_maturity_table[
  single_maturity_table$type == "C", ]
puts <- single_maturity_table[
  single_maturity_table$type == "P", ]

par(mfrow = c(2, 1))
plot(calls$strike, calls$volume,
  xlab = "Strike", ylab = "Volume",
  main = "Call volume", cex.main = 0.9)
abline(v = mean(calls$avg_stock_price), lty = 2)
grid()

plot(puts$strike, puts$volume,
  xlab = "Strike", ylab = "Volume",
  main = "Put volume", cex.main = 0.9)
abline(v = mean(puts$avg_stock_price), lty = 2)
grid()

######################
# Implied volatility #
######################
# Create a vector of filenames
folder <- "Chapter_09/SPY_20130410_QT/"

available_files <- list.files(folder)

# Explore the first few lines of the file
temp <- read.csv(file = paste0(folder, available_files[1]),
header = FALSE, stringsAsFactors = FALSE)
head(temp)
# Output omitted for clarity

column_names <- c("date", "time", "trade_indicator",
  "sequence_number", "option_exchange_code",
  "option_condition_code", "bid_price", "bid_size",
  "ask_price", "ask_size", "stock_exchange_code",
  "stock_condition_code", "underlying_bid_price",
  "underlying_bid_size", "underlying_ask_price",
  "underlying_ask_size")

# Find files for July 20, 2013 expiry
files_to_use <- available_files[grep("20130720",
  available_files)]

length(files_to_use)
## [1] 142

strikes <- sapply(strsplit(files_to_use, "_"), "[", 4)
type <- sapply(strsplit(files_to_use, "_"), "[", 2)

# Extract relevant columns of data
quote_list <- list()

for(i in 1:length(files_to_use)) {
  temp <- read.csv(file = paste0(folder, files_to_use[i]),
    header = FALSE, stringsAsFactors = FALSE)
  names(temp) <- column_names

  # Extract quotes from CBOE only
  filter <- temp$trade_indicator == "Q" &
    temp$option_exchange_code == "C"

  data <- temp[filter, ]

  # Create xts object
  require(xts)
  time_index <- as.POSIXct(paste(data$date, data$time),
    format = "%m/%d/%Y %H:%M:%OS")
  data_filtered <- data[, c("bid_price", "ask_price",
    "underlying_bid_price", "underlying_ask_price")]
  data_filtered$type <- type[i]
  data_filtered$strike <- strikes[i]
  xts_prices <- xts(data_filtered, time_index)

  quote_list[[i]] <- xts_prices
}

data <- quote_list[[49]]
spread <- as.numeric(data$ask_price) -
  as.numeric(data$bid_price)
plot(xts(spread, index(data)),
  main = "SPY | Expiry = July 20, 2013 | K = 158",
  cex.main = 0.8, ylab = "Quote bid-ask spread")

time_of_interest <- "2013-04-10 10:30:00::
  2013-04-10 10:30:10"

strike_list <- list()
for(i in 1:length(quote_list)) {
  data <- quote_list[[i]][time_of_interest]
  if(nrow(data) > 0) {
    mid_quote <- (as.numeric(data$bid_price) +
      as.numeric(data$ask_price)) / 2
    mid_underlying <- (as.numeric(data$underlying_bid_price) +
      as.numeric(data$underlying_ask_price)) / 2
    strike_list[[i]] <- c(as.character(index(data[1])),
      data$type[1], data$strike[1], names(quote_list[i]),
      mid_quote[1], mid_underlying[1])
  }
}

# Aggregate the columns
df <- as.data.frame(do.call(rbind, strike_list),
  stringsAsFactors = FALSE)
names(df) <- c("time", "type", "strike",
  "mid_quote", "mid_underlying")

head(df)
# Output omitted for clarity

plot(as.numeric(df$strike), as.numeric(df$mid_quote),
  main = "Option Price vs. Strike for Calls and Puts",
  ylab = "Premium",
  xlab = "Strike",
  cex.main = 0.8)
grid()

# Filter the otm options
otm_calls <- df$type == "C" & df$mid_underlying <= df$strike
otm_puts <- df$type == "P" & df$mid_underlying > df$strike
otm <- df[otm_calls | otm_puts, ]

# Order by strike
otm <- otm[order(otm[, "strike"]), ]
plot(otm$strike, otm$mid_quote,
  main = "OTM prices",
  xlab = "Strike",
  ylab = "Premium",
  cex.main = 0.8)
grid()

# Compute the implied vols for otm options
otm$iv <- NA
for(i in 1:nrow(otm)) {
  type <- ifelse(otm$type[i] == "C", "call", "put")
  value <- as.numeric(otm$mid_quote[i])
  underlying <- as.numeric(otm$mid_underlying[i])
  strike <- as.numeric(otm$strike[i])
  dividendYield <- 0.03
  riskFreeRate <- 0.02
  maturity <- 101/252
  volatility <- 0.15
  otm$iv[i] <- AmericanOptionImpliedVolatility(type,
    value, underlying, strike,dividendYield,
    riskFreeRate, maturity, volatility)$impliedVol
}
# Generate plot
plot(otm$strike, otm$iv,
  main = "Implied Volatility skew for SPY on April 10, 2013 10:30 am",
  xlab = "Strike",
  ylab = "Implied Volatility",
  cex.main = 0.8)
grid()










