#' Evaluate a polynomial
#' @description Evaluation of a multivariate polynomial at some values of the 
#' variables.
#'
#' @param P a \link[multipol]{multipol}
#' @param Values a matrix of values at which the polynomial will be evaluate; 
#' \code{ncol(Values)} is the number of variables, \code{nrow(Values)} is the 
#' number of evaluation points
#'
#' @return A vector of length \code{nrow(Values)}.
#' @export
#'
#' @examples
#' # define the polynomial 10*x^2*y*z^2
#' P <- as.multipol(array(0, dim=1+c(2,1,2)))
#' P[2,1,2] <- 10
#' # evaluate at (1,1,1) and (2,2,2):
#' evalPoly(P, rbind(c(1,1,1),c(2,2,2)))
evalPoly <- function(P, Values){
  repr <- representation(P)
  rcpp_evaluate(repr$coefficients, repr$degrees, Values)
}