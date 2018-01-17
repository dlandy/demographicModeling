# Utility functions

#' logOdds
#' 
#' This function is a transformation function that takes the log odds
#' By design, it is robust to the inclusion of the occasional 0,
#' which it maps to a very small value, which can optionally be specified
#' It's appropriate for mapping (0, 1):-> (-inf, inf).
#' @param x a vector of positive values, between 0 and inf
#' @param smallValue A value to set nominal 0's to, to avoid errors in plotting simulated data, or aberrant responses
#' @return a vector containing transformed values
#' @export
#' @examples
#' logOdds(1:100/100)
logOdds <- function(x, smallValue = 10^-5){ # Assumes x is between 0 and 1
  if(max(x)>1){
    stop("Some inputs to logOdds were >1")
  }
  if(min(x)<0){
    stop("Some inputs to logOdds were <0")
  }
  x[x>=1-smallValue] <- 1-smallValue
  x[x<=smallValue] <- smallValue
  d <- log(x/(1-x))
  d
}

logOddsInverse <- function(x) {
  # d <- log(x/(1-x))
  e <- exp(x)
  d <- e/(1+e)
  d
}