##########################
# Getting started with R #
##########################
1 + 1
sqrt(2)
20 + (26.8 * 23.4) / 2 + exp(1.34) * cos(1)
sin(1)
5^4
sqrt(-1 + 0i)

integrand <- function(x) 1 / ((x + 1) * sqrt(x))
integrate(integrand, lower = 0, upper = Inf)

x <- 3
x <- x + 1
z <- x ^ 2
z <- "hello quants"
y <- "a"
Z <- sqrt(2)
new.X <- 2.3

5+4
## [1] 9

##################
# The c() object #
##################
first_vector  <- c(1, 2, 3, 4, 5, 6)
second_vector <- c("a", "b", "hello")
third_vector  <- c("a", 2, 23)

first_vector
## [1] 1 2 3 4 5 6

third_vector
## [1] "a" "2" "23"

new_vector <- c(first_vector, 7, 8, 9)
new_vector
## [1] 1 2 3 4 5 6 7 8 9

# Extract the 4th element
example_1 <- new_vector[4]

# Extract the 5th and the 8th elements
example_2 <- new_vector[c(5, 8)]

example_2
## [1] 5 8

x <-c(1,5,10,15,20)
## [1] 1 5 10 15 20

x2 <- 2 * x
## [1] 2 10 20 30 40

x3 <- x ^ 2
## [1] 1 25 100 225 400

x4 <- x / x2
## [1] 0.5 0.5 0.5 0.5 0.5

x5 <- round(x * (x / x2) ^ 3.5 + sqrt(x4), 3)
## [1] 0.795 1.149 1.591 2.033 2.475

x6 <- round(c(c(x2[2:4], x3[1:2]), x5[4]), 2)
## [1] 10.00 20.00 30.00 1.00 25.00 2.03

#######################
# The matrix() object #
#######################
my_matrix <- matrix(c(1, 2, 3, 4, 5, 6),
    nrow = 2, ncol = 3)

my_matrix
##    [,1] [,2] [,3]
## [1,] 1 3 5
## [2,] 2 4 6

my_matrix <- matrix(c(1, 2, 3, 4, 5, 6),
    nrow = 2, ncol = 3, byrow = TRUE)

my_matrix
## [,1] [,2] [,3]
## [1,] 1 2 3
## [2,] 4 5 6

dimnames(my_matrix) <- list(c("one", "hello"),
    c("column1", "column2", "c3"))

my_matrix
## column1 column2 c3
## one   1 2 3
## hello 4 5 6

attributes(my_matrix)
## $dim
## [1] 2 3

## $dimnames
## $dimnames[[1]]
## [1] "one"   "hello"

## $dimnames[[2]]
## [1] "column1" "column2" "c3"

ans <- my_matrix[1, 3]
ans
## [1] 3

new_matrix_1 <- my_matrix * my_matrix

new_matrix_1
## [,1] [,2] [,3]
## [1,] 1 4 9
## [2,] 16 25 36

new_matrix_2 <- sqrt(my_matrix)

new_matrix_2
## [,1]    [,2]    [,3]
## [1,] 1  1.414214 1.732051
## [2,] 2  2.236068 2.449490

mat1 <- matrix(rnorm(1000), nrow = 100)
round(mat1[1:5, 2:6], 3)
##        [,1]   [,2]   [,3]   [,4]   [,5]
## [1,] -1.544  1.281  1.397  0.407 -0.459
## [2,]  0.483  0.046 -1.817 -0.289  0.597
## [3,]  0.405  1.045 -0.726 -0.163  0.258
## [4,]  0.141 -0.294 -1.225 -0.217 -0.771
## [5,] -0.537  0.226  0.126 -1.584 -1.237

mat2 <- mat1[1:25, ] ^ 2
head(round(mat2, 0), 9)[,1:7]        
## [,1] [,2] [,3] [,4] [,5] [,6] [,7]
## [1,]   1    2    2    2    0    0    7
## [2,]   0    0    0    3    0    0    0
## [3,]   0    0    1    1    0    0    1
## [4,]   0    0    0    2    0    1    4
## [5,]   1    0    0    0    3    2    1
## [6,]   2    1    3    1    1    1    1
## [7,]   0    0    0    0    0    1    0
## [8,]   1    2    0    0    1    2    0
## [9,]   0    0    3    0    2    2    0

###########################
# The data.frame() object #
###########################
df <- data.frame(price  = c(89.2, 23.2, 21.2),
  symbol = c("MOT", "AAPL", "IBM"),
  action = c("Buy", "Sell", "Buy"))

df
##   price symbol action
## 1  89.2    MOT    Buy
## 2  23.2   AAPL   Sell
## 3  21.2    IBM    Buy

df3 <-data.frame(price  = c(89.2, 23.2, 21.2),
  symbol = c("MOT", "AAPL", "IBM"),
  action = c("Buy", "Sell", "Buy"),
  stringsAsFactors = FALSE)

class(df3$symbol)
## [1] "character"

price <- df[1, 1]

price
## [1] 89.2

df2 <- data.frame(col1 = c(1, 2, 3),
  col2 = c(1, 2, 3, 4))

## Error in data.frame(col1 = c(1,2,3),
## col2 = c(1,2,3,4)) : arguments imply
## differing number of rows: 3, 4

symbols <- df$symbol

symbols
## [1] MOT  AAPL IBM
## Levels: AAPL IBM MOT

class(symbols)
## [1] "factor"

symbols <- df3$symbol

symbols
## [1] "MOT"  "AAPL" "IBM"

#####################
# The list() object #
#####################
my_list <- list(a = c(1, 2, 3, 4, 5),
  b = matrix(1:10, nrow = 2, ncol = 5),
  c = data.frame(price = c(89.3, 98.2, 21.2),
  stock = c("MOT", "IBM", "CSCO")))

my_list
## $a
## [1] 1 2 3 4 5

## $b
## [,1] [,2] [,3] [,4] [,5]
## [1,] 1 3 5 7 9
## [2,] 2 4 6 8 10

## $c
## price stock
## 1 89.3 MOT
## 2 98.2 IBM
## 3 21.2 CSCO

first_element <- my_list[[1]]

first_element
## [1] 1 2 3 4 5

class(first_element)
## [1] "numeric"

second_element <- my_list[["b"]]
second_element
## [,1] [,2] [,3] [,4] [,5]
## [1,] 1 3 5 7 9
## [2,] 2 4 6 8 10

class(second_element)
## [1] "matrix"

part_of_list <- my_list[c(1, 3)]

part_of_list
## $a
## [1] 1 2 3 4 5

## $c
## price stock
## 1 89.3 MOT
## 2 98.2 IBM
## 3 21.2 CSCO

class(part_of_list)
## [1] "list"

size_of_list <- length(my_list)

size_of_list
## [1] 3

########################
# The new.env() object #
########################
env <- new.env()
env[["first"]] <- 5
env[["second"]] <- 6
env$third <- 7

env
## <environment: 0x101ef2f18>

ls(env)
## [1] "first"  "second" "third"

get("first", envir = env)
## 5

rm("second", envir = env)
ls(env)
## [1] "first" "third"

env_2 <- env
env_2$third <- 42

get("third", envir = env)
## [1] 42

#############################
# Using the plot() function #
#############################
# Create a vector of numbers x and plot them
x <- c(1, 2, 3.2, 4, 3, 2.1, 9, 19)
plot(x)

# Convert the graph into a line plot
plot(x, type = "l")

# Set up the canvas
plot(rnorm(1000), main = "Some returns", cex.main = 0.9,
  xlab = "Time", ylab = "Returns")

# Superimpose a basic grid
grid()

# Create a few vertical and horizontal lines
abline(v = 400, lwd = 2, lty = 1)
abline(h = 2, lwd = 3, lty = 3)

# Create a 2-row, 2-column format
par(mfrow = c(2, 2))

# First plot (points).
plot(rnorm(100), main = "Graph 1")

# Second plot (lines).
plot(rnorm(100), main = "Graph 2", type = "l")

# Third plot (steps) with a vertical line
plot(rnorm(100), main = "Graph 3", type = "s")
abline(v = 50, lwd = 4)

# Fourth plot
plot(rnorm(100), type = "h", main = "Graph 4")

# Reset the plot window
par(mfrow = c(1, 1))

plot(rnorm(100), main = "A line plot",
  cex.main = 0.8,
  xlab = "x-axis",
  ylab = "y-axis",
  type = "l")

# Extra text
mtext("Some text at the top", side = 3)

# At x = 40 and y = -1 coordinates
legend(40, -1, "A legend")

formals(plot.default)
## $x

## $y
## NULL

## $type
## [1] "p"
## ...

##########################
# Functional programming #
##########################
ans <- sum(1:100)

ans
## [1] 5050

answer <- 0
for(i in 1:100) {
  answer <- answer + i
}
answer
## [1] 5050

crack_eggs <- function(number_of_eggs) {
  # Code that determines whether eggs have been cracked.
  # If they have, set have_all_eggs_been_cracked <- TRUE,
  # otherwise, set to FALSE
  
  return(have_all_eggs_been_cracked)
}

# Create 100 standard normals
x <- rnorm(100, mean = 0, sd = 1)

# Find the length of the vector x.
length(x)

# Compute the mean of x
mean(x)

# Compute the standard deviation of x
sd(x)

# Compute the median value of the vector x
median(x)

# Compute the range (min, max) of a variable
range(x)

# Find the sum of all the numbers in x
sum(x)

# Do a cumulative sum of the values in x
cumsum(x)

# Display the first 3 elements of x
head(x, 3)

# Display summary statistics on x
summary(x)

# Sort x from largest to smallest.
sort(x, decreasing = TRUE)

# Compute the successive difference in x
diff(x)

# Create an integer sequence from 1 to 10
1:10

# A sequence from 1 to 10 in steps of 0.1
seq(1, 10, 0.1)

# Print the string hello to the screen
print("hello")

#########################
# Branching and looping #
#########################

# Define a boolean variable
my_boolean <- 1 == 2

if (my_boolean) {
  print("not correct")
} else {
  print("XYZ")
}

for(i in 1:5) {
    cat(i, "\n")
}
## 1
## 2
## 3
## 4
## 5

some_list <- list()
for(z in c("hello", "goodbye")) {
  some_list[[z]] <- z
}

some_list
## $hello
## [1] "hello"

## $goodbye
## [1] "goodbye"

#############################
# A recommended style guide #
#############################
#sum numbers
x<-0;for(i in 1:10){x=x+1};x

# Sum numbers
x <- 0
for(i in 1:10) {
  x <- x + 1
}
x

##################################
# A pairwise correlation example #
##################################
filter_and_sort_symbols <- function(symbols) {
  # Name: filter_symbols
  # Purpose: Convert to upper case if not
  # and remove any non valid symbols
  # Input: symbols = vector of stock tickers
  # Output: filtered_symbols = filtered symbols
  
  # Convert symbols to uppercase
  symbols <- toupper(symbols)
  
  # Validate the symbol names
  valid <- regexpr("^[A-Z]{2,4}$", symbols)
  
  # Return only the valid ones
  return(sort(symbols[valid == 1]))
}

filter_and_sort_symbols(c("MOT", "cvx", "123", "Gog2", "XLe"))
## "MOT" "CVX" "XLE"

extract_prices <- function(filtered_symbols, file_path) {
  # Name: extract_prices
  # Purpose: Read price data from specified file
  # Inputs: filtered_symbols = vector of symbols,
  #         file_path = location of price data
  # Output: prices = data.frame of prices per symbol

  # Read in the .csv price file
  all_prices <- read.csv(file = file_path, header = TRUE,
    stringsAsFactors = FALSE)

  # Make the dates row names
  rownames(all_prices) <- all_prices$Date

  # Remove the original Date column
  all_prices$Date <- NULL

  # Extract only the relevant data columns
  valid_columns <- colnames(all_prices) %in% filtered_symbols

  return(all_prices[, valid_columns])
}

A <- c(1, 2, 5, 6, 9)
B <- c(0, 3, 6, 9, 10)

A %in% B
## [1] FALSE FALSE FALSE  TRUE  TRUE

filter_prices <- function(prices) {
  # Name: filter_prices
  # Purpose: Identify the rows with missing values
  # Inputs: prices = data.frame of prices
  # Output: missing_rows = vector of indexes where
  # data is missing in any of the columns

  # Returns a boolean vector of good or bad rows
  valid_rows <- complete.cases(prices)

  # Identify the index of the missing rows
  missing_rows <- which(valid_rows == FALSE)

  return(missing_rows)
}

compute_pairwise_correlations <- function(prices) {
  # Name: compute_pairwise_correlations
  # Purpose: Calculates pairwise correlations of returns
  # and plots the pairwise relationships
  # Inputs: prices = data.frame of prices
  # Output: correlation_matrix = A correlation matrix

  # Convert prices to returns
  returns <- apply(prices, 2, function(x) diff(log(x)))
  
  # Plot all the pairwise relationships
  pairs(returns, main = "Pairwise return scatter plot")
  
  # Compute the pairwise correlations
  correlation_matrix <- cor(returns, use = "complete.obs")

  return(correlation_matrix)
}

# Stock tickers entered by user
symbols <- c("IBM", "XOM", "2SG", "TEva",
  "G0og", "CVX", "AAPL", "BA")

# Location of our database of prices
file_path <- "Chapter_02/prices.csv"

# Filter and sort the symbols
filtered_symbols <- filter_and_sort_symbols(symbols)
filtered_symbols
## [1] "AAPL" "BA"   "CVX"  "IBM"  "TEVA" "XOM"

# Extract prices
prices <- extract_prices(filtered_symbols, file_path)

# Filter prices
missing_rows <- filter_prices(prices)
missing_rows
## integer(0)

# Compute correlations
correlation_matrix <- compute_pairwise_correlations(prices)
correlation_matrix










