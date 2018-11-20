#' Differenatiate a polynom
#' @description Differentiation of a multivariate polynomial.
#'
#' @param P a \link[multipol]{multipol}
#' @param differentation integer vector, indicates the number of 
#' differentiations in each variable
#'
#' @return A multipol.
#' @export
#' @importFrom prodlim row.match
#' @examples
#' # define the polynomial 10*x^2*y*z^2
#' P <- as.multipol(array(0, dim=1+c(2,1,2)))
#' P[2,1,2] <- 10
#' # differentiate two times with respect to x
#' differentiate(P, c(2,0,0))
differentiate <- function(P, differentation){
  monomials <- which(P!=0, arr.ind = TRUE) - 1L
  allDegrees <- degreesTable(max(rowSums(monomials)))
  degrees <- row.match(as.data.frame(monomials), allDegrees)
  reprP <- representation(P, degrees)
  reprPdiff <- rcpp_differentiate(reprP$coefficients, reprP$degrees, 
                                  as.integer(differentation))
  #names(reprPdiff) <- c("coefficients", "degrees") # à faire dans Rcpp
  backrepresentation(reprPdiff, allDegrees[reprPdiff$degrees,,drop=FALSE])
}