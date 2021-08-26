#include <Rcpp.h>
// using namespace Rcpp;
// Source: https://bmcnoldy.rsmas.miami.edu/Humidity.html
// TtDewp = 243.04 * (log10(hurs/100) + ((17.625 * tmp)/(243.04 + tmp))) /(17.625 - log10(hurs/100) - ((17.625 * tmp)/(243.04 + tmp))) 

// [[Rcpp::export]]
Rcpp::NumericVector f_tDewp(Rcpp::NumericVector hurs, Rcpp::NumericVector tmp) {
  Rcpp::NumericVector tDew(hurs.size(), Rcpp::NumericVector::get_na());
 hurs = log10(hurs/100.0);
 tDew = 243.04 * (hurs + ((17.625 * tmp)/(243.04 + tmp))) /(17.625 - hurs) - ((17.625 * tmp)/(243.04 + tmp));
   return tDew;
}

