#' Monomials of three variables
#' @description Create monomials of three variables
#' @name monomials
#' @rdname monomials
#' 
#' @param pc,py,pz the powers of the monomial, integers
#' @param coef coefficient of the monomial
#' @return A multipol.
#' @examples 
#' # create the monomial 4*x*y^2*z^3
#' xyz(1, 2, 3, 4)
#' # or
#' 4*x.*y.^2*z.^3
NULL

#' @name monomials
#' @rdname monomials
#' @export
xyz <- function(px,py,pz,coef){
  A <- array(0, c(px+1,py+1,pz+1))
  P <- as.multipol(A)
  f <- function(){
    P[px,py,pz] <- coef
    P
  }
  f()
}

#' @name monomials
#' @rdname monomials
#' @export
x. <- xyz(1,0,0,1)

#' @name monomials
#' @rdname monomials
#' @export
y. <- xyz(0,1,0,1)

#' @name monomials
#' @rdname monomials
#' @export
z. <- xyz(0,0,1,1)
