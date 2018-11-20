revM <- function(M) M[nrow(M):1,]

#' Degrees table
#' @description  Gives the correspondence between a triplet of degrees and an integer.
#' @importFrom plyr rbind.fill
#' @importFrom arrangements combinations
#' @export
degreesTable <- function(kmax){
  tabl <- rbind(NA_character_, 
                do.call(rbind.fill.matrix, 
                        lapply(1:kmax, function(k){
                          revM(combinations(c("x","y","z"), k=k, replace = TRUE))
                        })
                )
  )
  t(apply(tabl, 1, function(row){
    row <- na.omit(row)
    c(sum(row=="x"),sum(row=="y"),sum(row=="z"))
  }))
}

#' Representation of a polynomial
#' @description Representation of a multivariate polynomial.
#' 
#' @rdname representation
#' @name representation
#' @param P a multipol
#' @param degrees integer vector, the codings of the degrees of the monomials 
#' of \code{P} with non-zero coefficients; this is for internal purpose, set to 
#' \code{NULL} for an automatic derivation of this vector
#' @param repr a representation of a polynomial: a list with \code{degrees} 
#' and \code{coefficients}
#' @param monomials a matrix giving the degrees of the monomials forming the 
#' polynomial corresponding to \code{repr$degrees}
#'
#' @return The \code{representation} function returns a list with two elements: 
#' the coefficients in a numeric vector (\code{coefficients}) and an integer 
#' vector representing the monomial terms (\code{degrees}).
#' The \code{backrepresentation} function returns a \link[multipol]{multipol}.
#' 
#' @note These functions, especially \code{backrepresentation}, are rather for 
#' internal purposes.
#' 
#' @importFrom prodlim row.match
#'
#' @examples
#' P <- as.multipol(array(1:27, dim=c(3,3,3)))
#' representation(P)
NULL

#' @name representation
#' @rdname representation
#' @export
representation <- function(P, degrees=NULL){
  monomials <- which(P!=0, arr.ind = TRUE) - 1L
  if(is.null(degrees)){
    allDegrees <- degreesTable(max(rowSums(monomials))) 
    degrees <- row.match(as.data.frame(monomials), allDegrees)
  }
  # coefficients <- apply(monomials, 1, function(ijk){ 
  #   ijk <<- ijk
  #   P[ijk[1],ijk[2],ijk[3]]
  # })
  coefficients <- c(P[monomials])
  list(degrees=degrees, coefficients=coefficients)
}

#' @name representation
#' @rdname representation
#' @export
backrepresentation <- function(repr, monomials){
  degmax <- apply(monomials, 2, max)
  degrees <- repr$degrees
  P <- as.multipol(array(0, dim=1+degmax))
  P[monomials] <- repr$coefficients
  P
}