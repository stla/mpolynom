
#include <Rcpp.h>
#include "polynomial.hpp"

Rcpp::List rcpp_differentiate(
    Rcpp::NumericVector Coeffs, Rcpp::NumericVector Degrees, Rcpp::IntegerVector DWR
);
// [[Rcpp::export]]
Rcpp::List rcpp_differentiate(
    Rcpp::NumericVector Coeffs, Rcpp::IntegerVector Degrees, Rcpp::IntegerVector DWR
){
  int o1 = Coeffs.size(); 
  double c1[o1]; int e1[o1];
  for(int i=0; i<o1; i++){
    c1[i] = Coeffs(i);
    e1[i] = Degrees(i);
  }
  int m = DWR.size();
  int dif[m];
  for(int i=0; i<m; i++){
    dif[i] = DWR(i);
  }
  double c[o1]; int e[o1]; int o;
  polynomial_dif(m, o1, c1, e1, dif, o, c, e);
  Rcpp::NumericVector dCoeffs(o);
  Rcpp::IntegerVector dDegrees(o);
  for(int i=0; i<o; i++){
    dCoeffs(i) = c[i];
    dDegrees(i) = e[i];
  }
  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("coefficients")=dCoeffs, 
    Rcpp::Named("degrees")=dDegrees);
  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_evaluate(
    Rcpp::NumericVector Coeffs, Rcpp::IntegerVector Degrees, Rcpp::NumericMatrix Values
){
  int o = Coeffs.size(); 
  double c[o]; int e[o];
  for(int i=0; i<o; i++){
    c[i] = Coeffs(i);
    e[i] = Degrees(i);
  }
  int m = Values.ncol();
  int n = Values.nrow();
  double* x;
  x = new double[n*m];
  Rcpp::NumericMatrix tValues = Rcpp::transpose(Values);
  for(int i=0; i<n*m; i++){
    x[i] = tValues[i];
  }
  double* out0 = polynomial_value(m, o, c, e, n, x);
  Rcpp::NumericVector out(n);
  for(int i=0; i<n; i++){
    out[i] = out0[i];
  }
  delete[] x;
  delete[] out0;
  return out;
}

// [[Rcpp::export]]
int rcpp_rank_grlex(
    Rcpp::NumericVector Powers
){
  int m = Powers.size();
  int x[m];
  for(int i = 0; i<m; i++){
    x[i] = Powers[i];
  }
  int rank = mono_rank_grlex(m, x);
  return rank;
}

// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_unrank_grlex(
    int m, int rank
){
  int* powers = mono_unrank_grlex(m, rank);
  Rcpp::IntegerVector out(m);
  for(int i=0; i<m; i++){
    out[i] = powers[i];
  }
  return out;
}