#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

std::vector<double> fcpp(std::vector<double> rh, std::vector<double> tas) {
  double hn = 6;
  std::vector<double> pwc(rh.size(), NAN);
  for (size_t i=0; i<rh.size(); i++) {
    
    if (std::isnan(rh[i])) continue;
    
    double wbgt = tas[i] * atan(0.151977 * sqrt(rh[i] + 8.313659)) + atan(tas[i] + rh[i]) - atan(rh[i] - 1.676331) + 0.00391838 * pow(rh[i], 1.5) * atan(0.023101 * rh[i]) - 4.686035;
    
    pwc[i] = tas[i] < 15 ? 100 : 100/(1 + pow((-12.28 * log(rh[i]) + 87.99)/tas[i], -2.21 * log(rh[i]) + 2.63));
      
      if (wbgt >= 35) {
        pwc[i] -=  2 * hn + 4.86;
      } else if (wbgt >= 33) {
        pwc[i] += ((wbgt - 33)/(35 - 33)) * (-(2   * hn + 4.86)) + (-1 * (wbgt - 35)/(35 - 33)) * (-(1.1 * hn + 0.98));
      } else if (wbgt >= 29) {
        pwc[i] += ((wbgt - 29)/(33 - 29)) * (-(1.1 * hn + 0.98)) + (-1 * (wbgt - 33)/(33 - 29)) * (-(0.65 * hn + 1.3));
      } else if (wbgt > 15) {
        pwc[i] += ((wbgt - 15)/(29 - 15)) * (-(0.65 * hn + 1.3));
      }
  }
  return pwc;
}


/*** R




library(terra)
rh <- rast(nrow=5, ncol=5)
values(rh) <- runif(ncell(rh), 50,100)
tmp <- rast(rh) 
values(tmp) <- runif(ncell(rh), 10,40)
v <- values(c(rh, tmp))
x <- c(rh, tmp)

# your fun
a <- lapp(x, f_THI_humans_adj_new)
# CPP fun
b <- lapp(x, fcpp)
plot(a,b);abline(0,1)

#there is no difference with a few cells, but that might be because of the fixed cost
#and different with larger rasters with higher variable cost, especially with many NAs

library(microbenchmark)
microbenchmark(lapp(x, f_THI_humans_adj_new), lapp(x, fcpp))

*/
