# CHAPTER 10
# Optimization

###########################
# The motivating parabola #
###########################
# Create the function
f <- function(x) {
 return((1 + x) ^ 2)
}

# Create the derivative
fp <- function(x) {
 return(2 * (1 + x))
}

# Plot the function and its derivative
x <- seq(-5, 5, 0.1)
plot(x, f(x), type = 'l', lwd = 2,
  main = "f(x) and f'(x)",
  cex.main = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8)
grid()
lines(x, fp(x), lty = 3, lwd = 2)
abline(h = 0)
abline(v = 0)

###################
# Newton's method #
###################
f <- function(x) {
    return(x ^ 2 - 4 * x + 1)
}

uniroot(f, c(-8, -1))
## $root
## [1] -4.236068

## $f.root
## [1] -2.568755e-07

## $iter
## [1] 8

## $estim.prec
## [1] 6.103516e-05

uniroot(f, c(-1, 2))
## $root
## [1] 0.236044

## $f.root
## [1] -0.0001070205

## $iter
## [1] 6

## $estim.prec
## [1] 6.103516e-05

# Newton's method with a first order approximation
newton <- function(f, tol = 1E-12, x0 = 1, N = 20) {
  # N = total number of iterations
  # x0 = initial guess
  # tol = abs(xn+1 - xn)
  # f = function to be evaluated for a root

  h <- 0.001
  i <- 1; x1 <- x0
  p <- numeric(N)
  while (i <= N) {
    df_dx <- (f(x0 + h) - f(x0)) / h
    x1 <- (x0 - (f(x0) / df_dx))
    p[i] <- x1
    i <- i + 1
    if (abs(x1 - x0) < tol) {
      break
    }
    x0 <- x1
  }
  return(p[1:(i-1)])
}

newton(f, x0 = -10)
## [1] -6.312270 -4.735693 -4.281609 -4.236513
## [5] -4.236068 -4.236068 -4.236068 -4.236068

newton(f, x0 = 10)
## [1] 4.2085746 1.5071738 0.4665599 0.2468819
## [5] 0.2360964 0.2360680 0.2360680 0.2360680

options(digits = 14)
newton(f, x0 = 10)
## [1] 4.20857464272283 1.50717378796250 0.46655991958819
## [5] 0.24688186711921 0.23609640037269 0.23606798403444
## [8] 0.23606797750125 0.23606797749979 0.23606797749979

newton(f, x0 = 0.25)
## [1] 0.23611419684515 0.23606798830978 0.23606797750221
## [4] 0.23606797749979 0.23606797749979

# Create an expression
e <- expression(sin(x))

# Compute the derivative
D(e, "x")
## cos(x)

f_expr <- expression(x ^ 2 + 4 * x - 1)

eval(f_expr, list(x = 2))
## [1] 11

newton_alternate <- function(f, tol = 1E-12, x0 = 1, N = 20) {
  # N = total number of iterations
  # x0 = initial guess
  # tol = abs(xn+1 - xn)
  # f = expression to be evaluated for a root

  # Compute the symbolic derivative
  df_dx = D(f, "x")

  i <- 1; x1 <- x0
  p <- numeric(N)
  while (i <= N) {
    x1 <- (x0 - eval(f, list(x = x0)) /
      eval(df_dx, list(x = x0)))
    p[i] <- x1
    i <- i + 1
    if (abs(x1 - x0) < tol) {
      break
    }
    x0 <- x1
  }
  return(p[1:(i-1)])
}

newton_alternate(f_expr, x0 = 10)
 ## [1] 4.20833333333333 1.50685123042506 0.46631585084907
 ## [4] 0.24681560399775 0.23609368309733 0.23606797764754
 ## [7] 0.23606797749979 0.23606797749979

newton_alternate(f_expr, x0 = -10)
## [1] -6.3125000000000 -4.7359601449275 -4.2817360731259
## [4] -4.2365249924418 -4.2360680241934 -4.2360679774998
## [7] -4.2360679774998

############################
# The brute-force approach #
############################
# Create a set of random points x
set.seed(123)
x <- rnorm(100, 0, 1)

# Make y a function of x
y <- 3.2 + 2.9 * x + rnorm(100, 0, 0.1)

plot(x, y)

objective_function <- function(y, x, a, b) {
  value <- sum((y - (a * x + b)) ^ 2)
  return(value)
}

# Create a range of a and b values and loop through all of them
a <- seq(-10, 10, 0.25)
b <- seq(-10, 10, 0.25)

output <- list()
z <- 1
for(i in 1:length(a)) {
  for(j in 1:length(b)) {
    output[[z]] <- c(objective_function(y, x, a[i], b[j]),
      a[i], b[j])
    z <- z + 1
  }
}

# Create a matrix out of the list and find the minimum value
mat <- do.call(rbind, output)
colnames(mat) <- c("obj", "a", "b")

smallest <- which(mat[, "obj"] == min(mat[, "obj"]))

mat[smallest, ]
##   obj      a      b
## 2.16076 3.00000 3.25000

a = seq(-5, 5, 0.1)
b = seq(-5, 5, 0.1)

##   obj       a      b
## 0.9077592 2.9000 3.2000

###########################
# R optimization routines #
###########################
args(optim)
function (par, fn, gr = NULL, ...,
  method = c("Nelder-Mead",
  "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
  lower = -Inf,
  upper = Inf,
  control = list(),
  hessian = FALSE)

############################
# A curve-fitting exercise #
############################
# Create fictitious yields
rates = c(0.025, 0.03, 0.034, 0.039, 0.04,
  0.045, 0.05, 0.06, 0.07, 0.071,
  0.07, 0.069, 0.07, 0.071, 0.072,
  0.074, 0.076, 0.082, 0.088, 0.09)
maturities = 1:20

plot(maturities, rates, xlab = "years",
  main = "Yields",
  cex.main = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8)
grid()

poly_5 <- function(x, p) {
  f <- p[1] + p[2] * x + p[3] * x^2 +
    p[4] * x^3 + p[5] * x^4 + p[6] * x^5
  return(f)
}

obj_5 <- function(x, y, p) {
  error <- (y - poly_5(x, p)) ^ 2
  return(sum(error))
}

# Fit the parameters. Assume 0 for all initial values
out_5 = optim(obj_5, par = c(0, 0, 0, 0, 0, 0),
  x = maturities, y = rates)

out
## $par
## [1]  2.4301235956099e-02  1.3138951147963e-03
## [3]  5.5229326602931e-04  7.5685740385076e-07
## [5] -4.2119475163787e-06  1.5330958809806e-07

## $value
## [1] 0.00017311660458207

## $counts
## function gradient
##    501       NA

## $convergence
## [1] 1

## $message
## NULL

lines(poly_5(maturities, out_5$par), lwd = 1.5, lty = 2)

poly_7 <- function(x, p) {
  f <- p[1] + p[2] * x + p[3] * x^2 +
    p[4] * x^3 + p[5] * x^4 +
    p[6] * x^5 + p[6] * x^6 +
    p[7] * x^7
  return(f)
}

obj_7 <- function(x, y, p) {
  error <- (y - poly_7(x, p)) ^ 2
  return(sum(error))
}

# Fit the parameters. Assume 0 for all initial values
out_7 <- optim(obj_7, par = c(0, 0, 0, 0, 0, 0, 0, 0),
  x = maturities, y = rates)

lines(poly_7(maturities, out_7$par), lwd = 1.5, lty = 3)

# Specify two polynomials to be used for fitting purposes
poly_5 <- function(x, a) {
  f <- a[1] + a[2] * x + a[3] * x ^ 2 +
    a[4] * x ^ 3 + a[5] * x ^ 4 +
    a[6] * x ^ 5
return(f)
}

poly_3 <- function(x, offset, intercept, b) {
  f <- intercept + b[1] * (x - offset) +
    b[2] * (x - offset) ^ 2 +
    b[3] * (x - offset) ^ 3
  return(f)
}

obj_3_5 <- function(x, y, offset, p) {

  # All points are at infinity initially
  fit <- rep(Inf, length(x))
  ind_5 <- x <= offset
  ind_3 <- x > offset

  fit[ind_5] <- poly_5(x[ind_5], p[1:6])
  fit[ind_3] <- poly_3(x[ind_3], offset,
    poly_5(offset, p[1:6]), p[7:9])

  error <- (y - fit) ^ 2
  return(sum(error))
}

# Fit the parameters.  Assume 0 for all initial values
offset <- 9
out_3_5 <- optim(obj_3_5, par = rep(0, 9),
  x = maturities, y = rates, offset = offset)

plot(maturities, rates, xlab = "years",
  main = "Yields",
  cex.main = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.8)
grid()
lines(poly_5(maturities[maturities <= offset],
  out_3_5$par[1:6]), lwd = 2)
lines(c(rep(NA, offset),
  poly_3(maturities[maturities > offset], offset,
  poly_5(offset, out_3_5$par[1:6]),
  out_3_5$par[7:9])), lwd = 2)
abline(v = offset)

# Fit loess to the data
obj <- loess(rates ~ maturities, span = 0.5)

# Plot the data and the fit
plot(maturities, rates, main = "Rates", cex.main = 0.8)
lines(predict(obj), lty = 2)

##########################
# Portfolio optimization #
##########################
install.packages("DEoptim")

require(DEoptim)

# Drawdown function
compute_drawdown <- function(x, returns_default = TRUE,
  geometric = TRUE) {
  # x = Vector of raw pnl or returns
  # If returns_default = FALSE, the geometric
  # argument is ignored and the pnl is used.
  # Output = the maximum drawdown
  if(returns_default) {
    # Cumulative return calculation
    if(geometric) {
      cumulative_return <- cumprod(1 + x)
    } else {
      cumulative_return <- 1 + cumsum(x)
    }
    max_cumulative_return <- cummax(c(1, cumulative_return))[-1]
    drawdown <- -(cumulative_return / max_cumulative_return - 1)
  } else {
    # PnL vector is used
    cumulative_pnl <- c(0, cumsum(x))
    drawdown <- cummax(cumulative_pnl) - cumulative_pnl
    drawdown <- drawdown[-1]
  }
  # Drawdown vector for either pnl or returns
  return(drawdown)
}

obj_max_drawdown <- function(w, r_matrix, small_weight) {
  # w is the weight of every stock
  # r_matrix is the returns matrix of all stocks

  # Portfolio return
  portfolio_return <- r_matrix %*% w

  # Max drawdown
  drawdown_penalty <- max(compute_drawdown(portfolio_return))

  # Create penalty component for sum of weights
  weight_penalty <- 100 * (1 - sum(w)) ^ 2

  # Create a penalty component for negative weights
  negative_penalty <- -sum(w[w < 0])

  # Create penalty component for small weights
  small_weight_penalty <- 100 * sum(w[w < small_weight])

  # Objective function to minimize
  obj <- drawdown_penalty + weight_penalty +
    negative_penalty + small_weight_penalty
  return(obj)
}

# Calculate a returns matrix for multiple stocks
symbol_names <- c("AXP", "BA", "CAT", "CVX",
  "DD", "DIS", "GE", "HD", "IBM",
  "INTC", "KO", "MMM", "MRK",
  "PG", "T", "UTX", "VZ")

# Load these prices into memory
price_matrix <- NULL
for(name in symbol_names) {
  # Extract the adjusted close price vector
  price_matrix <- cbind(price_matrix, get(name)[, 6])
}

colnames(price_matrix) <- symbol_names

# Compute returns
returns_matrix <- apply(price_matrix, 2, function(x) diff(log(x)))

# Specify a small weight below which the allocation should be 0%
small_weight_value <- 0.02

# Specify lower and upper bounds for the weights
lower <- rep(0, ncol(returns_matrix))
upper <- rep(1, ncol(returns_matrix))

optim_result <- DEoptim(obj_max_drawdown, lower, upper,
  control = list(NP = 400, itermax = 300, F = 0.25, CR = 0.75),
  returns_matrix, small_weight_value)

weights <- optim_result$optim$bestmem

sum(weights)
## 0.9978

weights <- weights / sum(weights)

# Equally weighted portfolio
equal_weights <- rep(1 / 17, 17)
equal_portfolio <- returns_matrix %*% equal_weights
equal_portfolio_cumprod <- cumprod(1 + equal_portfolio)

# Optimal max drawdown portfolio
optimized_portfolio <- returns_matrix %*% weights
drawdown_portfolio_cumprod <- cumprod(1 + optimized_portfolio)

main_title <- "Equal vs. Optimized Weights"
plot(drawdown_portfolio_cumprod, type = 'l', xaxt = 'n',
  main = main_title, xlab = "", ylab = "cumprod(1 + r)")
lines(equal_portfolio_cumprod, lty = 3)
grid(col = 'black')

# Set x-axis labels
label_location <- seq(1, length(drawdown_portfolio_cumprod),
  by = 90)
labels <- rownames(returns_matrix)[label_location]
axis(side = 1, at = label_location, labels = labels,
  las = 2, cex.axis= 0.8)

# Equal weighted
max(compute_drawdown(equal_portfolio))
## [1] 0.597

# Optimized for the smallest max drawdown
max(compute_drawdown(optimized_portfolio))
## [1] 0.515
