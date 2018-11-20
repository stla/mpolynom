
#include <Rcpp.h>
#include "polynomial.hpp"

Rcpp::List rcpp_differentiate(
    Rcpp::NumericVector Coeffs, Rcpp::NumericVector Degrees, Rcpp::IntegerVector DWR
);
// [[Rcpp::export]]
Rcpp::List rcpp_differentiate(
    Rcpp::NumericVector Coeffs, Rcpp::NumericVector Degrees, Rcpp::IntegerVector DWR
){
  int o1 = Coeffs.size(); 
  double c1[o1]; double e1[o1];
  for(int i=0; i<o1; i++){
    c1[i] = Coeffs(i);
    e1[i] = Degrees(i);
  }
  int m = DWR.size();
  int dif[m];
  for(int i=0; i<m; i++){
    dif[i] = DWR(i);
  }
  double c[o1]; double e[o1]; int o;
  polynomial_dif(m, o1, c1, e1, dif, o, c, e);
  Rcpp::NumericVector dCoeffs(o);
  Rcpp::NumericVector dDegrees(o);
  for(int i=0; i<o; i++){
    dCoeffs(i) = c[i];
    dDegrees(i) = e[i];
  }
  Rcpp::List out = Rcpp::List::create(dCoeffs, dDegrees);
  return out;
}

