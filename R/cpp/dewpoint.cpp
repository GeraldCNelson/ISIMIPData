#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

std::vector<double> f_tDewp(std::vector<double> relh, std::vector<double> tmp) {
  std::vector<double> tDew(relh.size(), NAN);
  for (size_t i=0; i<relh.size(); i++) {
    
    if (std::isnan(relh[i])) continue;
    // pow is raise to power
//    relh[i] = std::min(relh[i], 100.0);
//    relh[i] = std::max(relh[i], 0.0);
 //   Rcpp::Rcout << tmp[i] << std::endl;
    double exp = 7.5 * tmp[i]/(237.7 + tmp[i]);
    double svp = 0.611 * pow(10.0, exp);
    double vp = svp * relh[i]/100.0;
    double y = log10(vp/0.611)/7.5;
    tDew[i] = (y * 237.7)/(1.0 - y);
//      Rcpp::Rcout << i << std::endl;
  }
  return tDew;
}

