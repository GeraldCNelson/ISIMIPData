#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

std::vector<double> f_THI_humans_adj(std::vector<double> rh, std::vector<double> tas) {
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

// [[Rcpp::export]]
std::vector<double> f_THI_humans(std::vector<double> rh, std::vector<double> tas) {
  std::vector<double> pwc(rh.size(), NAN);
  for (size_t i=0; i<rh.size(); i++) {
    if (std::isnan(rh[i])) continue;
    pwc[i] = 100/(1 + pow((-12.28 * log(rh[i]) + 87.99)/tas[i], -2.21 * log(rh[i]) + 2.63));
  }
  return pwc;
}

// [[Rcpp::export]]
std::vector<double> f_THI_generic_generic(std::vector<double> rh, std::vector<double> tmax) {
  std::vector<double> thi(rh.size(), NAN);
  for (size_t i=0; i<rh.size(); i++) {
    if (std::isnan(rh[i])) continue;
    if (tmax[i] < 20.0) {
      thi[i] = 0;
    } else {
      thi[i] =  (1.8 * tmax[i] + 32.0) - ((0.55 - 0.0055 * rh[i]) * (1.8 * tmax[i] - 26.8));
    }
  }
  return thi;
}


// [[Rcpp::export]]
std::vector<double> f_THI_bos_Taurus(std::vector<double> rh, std::vector<double> tmax) {
  std::vector<double> thi(rh.size(), NAN);
  for (size_t i=0; i<rh.size(); i++) {
    
    if (std::isnan(rh[i])) continue;
    if (tmax[i] < 20.0) {
      thi[i] = 0;
    } else {
      thi[i] =  (1.8 * tmax[i] + 32.0) - ((0.55 - 0.0055 * rh[i]) * (1.8 * tmax[i] - 26.8));
    }
  }
  return thi;
}

// [[Rcpp::export]]
std::vector<double> f_THI_bos_Indicus(std::vector<double> rh, std::vector<double> tmax) {
  std::vector<double> thi(rh.size(), NAN);
  for (size_t i=0; i<rh.size(); i++) {
    
    if (std::isnan(rh[i])) continue;
    if (tmax[i] < 20.0) {
      thi[i] = 0;
    } else {
      thi[i] =  (1.8 * tmax[i] + 32.0) - ((0.55 - 0.0055 * rh[i]) * (1.8 * tmax[i] - 26.8));
    }
  }
  return thi;
}

// [[Rcpp::export]]
std::vector<double> f_THI_sheep(std::vector<double> rh, std::vector<double> tmax) {
  std::vector<double> thi(rh.size(), NAN);
  for (size_t i=0; i<rh.size(); i++) {
    
    if (std::isnan(rh[i])) continue;
    if (tmax[i] < 20.0) {
      thi[i] = 0;
    } else {
      thi[i] =  tmax[i] - ((0.31 - (0.31 * (rh[i] / 100))) * (tmax[i] - 14.4));
    }
  }
  return thi;
}

// [[Rcpp::export]]
std::vector<double> f_THI_pigs(std::vector<double> rh, std::vector<double> tmax) {
  std::vector<double> thi(rh.size(), NAN);
  for (size_t i=0; i<rh.size(); i++) {
    
    if (std::isnan(rh[i])) continue;
    if (tmax[i] < 20.0) {
      thi[i] = 0;
    } else {
      thi[i] =  tmax[i] - (0.55 - (0.0055 * rh[i]) * (tmax[i] - 14.5));
    }
  }
  return thi;
}

// [[Rcpp::export]]
std::vector<double> f_THI_chicken(std::vector<double> rh, std::vector<double> tmax) {
  std::vector<double> thi(rh.size(), NAN);
  for (size_t i=0; i<rh.size(); i++) {
    
    if (std::isnan(rh[i])) continue;
    
    double wbgt = tmax[i] * atan(0.151977 * sqrt(rh[i] + 8.313659)) + atan(tmax[i] + rh[i]) - atan(rh[i] - 1.676331) + 0.00391838 * pow(rh[i], 1.5) * atan(0.023101 * rh[i]) - 4.686035;
    
    if (tmax[i] < 20.0) {
      thi[i] = 0;
    } else {
      thi[i] =  0.85 * tmax[i] + 0.15 * wbgt;
    }
  }
  return thi;
}

/* R functions from thiCalcs2.R reproduced above in c++
 *  f_THI_bos_Taurus <- function(rh, tmax) {
 if (is.na(rh[1])) {return(rh)}
 thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8))
 thi[tmax < 20] <- 0
 thi <- round(thi, 3)
 }
 
 f_THI_bos_Indicus <- function(rh, tmax) {
 if (is.na(rh[1])) {return(rh)}
 thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8)) 
 thi[tmax < 20] <- 0
 thi <- round(thi, 3)
 }
 
# commented out because goats use the same THI function as cattle
# f_THI_goat <- function(rh, tmax) {
#   thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8)) 
#   thi[tmax < 20] <- 0
#   thi <- round(thi, 3)
# }
# 
 
 f_THI_sheep <- function(rh, tmax) {
 if (is.na(rh[1])) {return(rh)}
 thi <- tmax - ((0.31 - (0.31 * (rh / 100))) * (tmax - 14.4)) 
 thi[tmax < 20] <- 0
 thi <- round(thi, 3)
 }
 
 f_THI_pigs <- function(rh, tmax) {
 if (is.na(rh[1])) {return(rh)}
 thi <- tmax - (0.55 - (0.0055 * rh) * (tmax - 14.5))
 thi[tmax < 20] <- 0
 thi <- round(thi, 3)
 }
 
 f_THI_chicken <- function(rh, tmax) {
 if (is.na(rh[1])) {return(rh)}
#Tao X, Xin H, 2003. Acute synergistic effects of air temperature, humidity, and velocity on homeostasis of market-size broilers. Trans ASAE 46:491–497. https://doi.org/10.13031/2013.12971
 wbulb <- tmax * atan(0.151977* (rh + 8.313659)^(1/2)) + atan(tmax + rh) - atan(rh - 1.676331) + 0.00391838 * (rh)^ (3/2) * atan(0.023101 * rh) - 4.686035
 thi <- (0.85 * tmax + 0.15 * wbulb) # using broiler formula. Note. no rh needed
 thi[tmax < 20] <- 0
#  thi[thi > 100] <- 100
 thi <- round(thi, 3)
 }
 */
