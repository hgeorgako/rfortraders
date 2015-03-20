# CHAPTER 11
# Speed, Testing, and Reporting

##################################
# Runtime execution improvements #
##################################
sum_with_loop_in_r <- function(max_value) {
  sum <- 0
  for(i in 1:max_value) {
    sum <- sum + i
  }
  return(sum)
}

sum_with_vectorization_in_r = function(max_value) {
  numbers <- as.double(1:max_value)
  return(sum(numbers))
}

#######################
# Benchmarking R code #
#######################
library(microbenchmark)
microbenchmark(loop = sum_with_loop_in_r(1e5),
  vectorized = sum_with_vectorization_in_r(1e5))

## Unit: microseconds
## expr        min       lq        median
## loop        57615.323 59424.740 60992.7720
## vectorized  260.602   273.673   286.5495

## uq        max       neval
## 89608.441 96694.469 100
## 294.236   414.349   100

compiled_sum_with_loop_in_r <- cmpfun(sum_with_loop_in_r)
  microbenchmark(loop = sum_with_loop_in_r(1e5),
  compiled = compiled_sum_with_loop_in_r(1e5),
  vectorized = sum_with_vectorization_in_r(1e5))

## Unit: microseconds
## expr        min       lq         median
## loop        56746.652 58343.8945 60602.445
## compiled    4688.146  4758.6770  4892.246
## vectorized  249.457   273.8635   284.050

##  uq         max        neval
##  86599.9875 96736.750  100
##  5498.9710  46484.009  100
##  292.3135   473.927    100

#####################
# The Rcpp solution #
#####################
long add_cpp(long max_value) {
  long sum = 0;
  for(long i = 1; i <= max_value; ++i) {
    sum = sum + i;
  }
  return sum;
}

lapply
function (X, FUN, ...)
{
  FUN <- match.fun(FUN)
  if (!is.vector(X) || is.object(X))
    X <- as.list(X)
    .Internal(lapply(X, FUN))
}

library(Rcpp)

# Create a C++ function
cppFunction('
  long add_cpp(long max_value) {
    long sum = 0;
    for(long i = 1; i <= max_value; ++i) {
     sum = sum + i;
    }
    return sum;
   }'
)

add_cpp
## function (max_value)
## .Primitive(".Call")(<pointer: 0x10f52fbb0>, max_value)

add_cpp(1e5)
## [1] 5000050000

microbenchmark(loop = sum_with_loop_in_r(1e5),
  compiled = compiled_sum_with_loop_in_r(1e5),
  vectorized = sum_with_vectorization_in_r(1e5),
  compiled_cpp = add_cpp(1e5))

## Unit: microseconds
## expr         min       lq         median
## loop         73049.461 76640.5945 79635.8810
## compiled     7359.040  7487.9655  7795.9125
## vectorized   804.773   932.9285   1031.9695
## compiled_cpp 79.573    88.2615    98.9385

## uq         max        neval
## 80676.6600 94618.174  100
## 12101.8610 135353.743 100
## 1373.8565  2148.409   100
## 105.2440   135.781    100

sourceCpp('Chapter_11/add_2_file.cpp')

add_2_cpp(100)
## [1] 5050

###################################
# Calling R from C++ with RInside #
###################################
#include <RInside.h>
int main(int argc, char *argv[]) {

 // create an embedded R instance
 RInside R(argc, argv);

 // assign a char* (string) to "txt"
 R["txt"] = "Hello, world!\n";

 // eval the init string, ignoring any returns
 R.parseEvalQ("cat(txt)");

 exit(0);
}

####################################
# Writing unit tests with testthat #
####################################
# Define function
convert_to_returns <- function(prices) {
  return(9)
}

require(testthat)

# Group related functionality together with context()
context("Price to log-return conversion")

# Define the expectations using expect_that()
test_that("convert_to_returns produces the correct values", {

  # For these inputs
  input_prices <- c(100, 101, 102, 103, 99)

  # Expect these outputs
  expected_returns <- c(0.009950331,
    0.009852296, 0.009756175, -0.039609138)

  # Verify the expectation of equality
  expect_equal(expected_returns,
    convert_to_returns(input_prices))
})

## Error: Test failed: 'convert_to_returns produces
## the correct values'
## Not expected: expected_returns not equal to
## convert_to_returns(input_prices)
## Numeric: lengths (1, 4) differ.

# Define function
convert_to_returns <- function(prices) {
  return(diff(log(prices)))
}

# Verify the error message
input_prices <- c(100)
msg <- "Not enough price entries."

expect_message(msg, convert_to_returns(input_prices))
## Error: expected_message no messages shown

# Function with corner case check
convert_to_returns <- function(prices) {
  if(length(prices) < 2) {
    message("Not enough price entries.")
  }
  return(diff(log(prices)))
}

test_file("example_test_file.r")

#################################
# Using knitr for documentation #
#################################
\documentclass{article}
\usepackage[T1]{fontenc}

\begin{document}

This is an example LaTeX document with some embedded
R code woven in for convenience.

<<foo, fig.height = 4>>=

x = 1:10
y = x^2
plot(x, y, main = "This is a graph")
@

Inline expressions can be written by using the
\verb|\Sexpr{}| convention, e.g. $\pi=\Sexpr{pi}$
and \Sexpr{2.3492e7} and \Sexpr{x / 2.0}.

\subsection*{A different subsection}
We can insert graphs without displaying the code.
This can be done using the \texttt{echo = FALSE}
command within the code chunk argument list.

<<foo2, fig.height = 3, echo = FALSE>>=
x = 1:10
y=x^3
plot(x, y, main = "This is a second graph")
@

Any R code can be run within the code chunks
provided by knitr. This next example loads up
\texttt{ggplot2}, and the code creates a nice looking
density histogram.

<<foo3, fig.height = 6, tidy = FALSE >>=
require(ggplot2)
my_data = data.frame(returns = c(0.03, 0.04, 0.05,
  0.032, 0.01, 0.23, 0.4, 0.05, 0.066, 0.5),
  stock = c("SPY", "CVX", "CVX", "SPY",
  "XOM", "XOM", "CVX", "SPY", "SPY", "XOM"))

ggplot(my_data, aes(x = returns, fill = stock)) +
  geom_density(alpha = 0.2)
@
\end{document}













