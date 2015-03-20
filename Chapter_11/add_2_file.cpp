#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
long add_2_cpp(long max_value) {
  long sum = 0;
  for(long i = 1; i <= max_value; ++i) {
    sum = sum + i;
  }
  return sum;
}