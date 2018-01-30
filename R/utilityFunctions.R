# Utility functions
library(doMC)

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
    stop("Some inputs to logOdds were >1: ", max(x))
  }
  if(min(x)<0){
    stop("Some inputs to logOdds were <0: ", min(x))
  }
  x[x>=1-smallValue] <- 1-smallValue
  x[x<=smallValue] <- smallValue
  d <- log(x/(1-x))
  d
}

#' logOddsInverse
#' 
#' This function is a transformation function that inverts the log odds.
#' It's appropriate for mapping (-inf, inf) :->  (0, 1)
#' @param x a vector of positive values, between 0 and inf
#' @return a vector containing transformed values
#' @export
#' @examples
#' logOdds(1:100/100)
logOddsInverse <- function(x) {
  # d <- log(x/(1-x))
  e <- exp(x)
  d <- e/(1+e)
  d
}




#' gammaFromModeSD
#' 
#' This function takes a mode and SD, and generates the corresponding shape and rate parameters that 
#' produce a gamma with that set of values.
#' @param mode the desired mode value
#' @param sd the desired sd
#' @return a vector with the shape and rate 
#' @export
#' @examples
#' gammaFromModeSD(1, 1)
gammaFromModeSD <- function(mode = .05, sd = 10){
  
  # Here are the corresponding rate and shape parameter values:
  ra = ( mode + sqrt( mode^2 + 4*sd^2 ) ) / ( 2 * sd^2 )
  sh = 1 + mode * ra 
  
  plot(0:1000/100, dgamma(0:1000/100, shape=sh, rate=ra))
  c(sh, ra)
}


#' calcPK
#' 
#' Calculate political knowledge
#' @param veep
#' @param court
#' @param veto
#' @param controller
#' @param conservative
#' @return a vector with the shape and rate 
#' @export
calcPK <- function(veep, court, veto, controller, conservative, year=2018){
  veepList <- c("vicepresident", "vp", "vice-president", "vicepresidency", 
                "vice",  "viceprisedent", "47thvicepresident", "veep", "vicepresident?",
                "vicepresiden", "viceprisident","vicepresident." , "47th and current Vice President of the United States", "Exiting vice president","48thvicepresident", "48th and current Vice President of the United States", "48thandcurrentvicepresident"
                
  )
  
  vetoList <- c("2/3", "66", "67", "66%", "two thirds", 
                "A two-thirds majority vote in both the House of Representatives and in the Senate", 
                "Two-thirds", "67%",  "2/3s", "two-thirds vote", "Two thirds", 
                "2/3RDS" , ".66", "2 thirds", "66.67", "two-thirds",  "2/3rd", 
                "two third", "2/3rds", "Two Thirds",  "two thirds", "Two-Thirds","2 /3", 
                "2/3rds majority", "66.6", "A two-thirds majority", "3-Feb", "two-thirds majority", "Two-thirds", "2 out of 3 ", "two thirds", "66"
  ) #Hand-coded by inspection of all correct responses
  
  
  
  ((gsub("oftheusa|oftheus|oftheunitedstates|ofusa|ofus|oftheu.s.|ofamerica|us", "",
        tolower(gsub(" ", "", veep))) %in% veepList)
  +  (court %in% c("The Supreme Court"))
  +  (veto %in% vetoList)
  +  (controller %in% c("Republican Party"))
  +  (conservative %in% c("Republican Party")))
}


#' sErrors
#' 
#' This function is a transformation function that takes the log odds
#' By design, it is robust to the inclusion of the occasional 0,
#' which it maps to a very small value, which can optionally be specified
#' It's appropriate for mapping (0, 1):-> (-inf, inf).
#' @param x a vector of  values,
#' @return the standard error
#' @export
#' @examples
#' sErrors(rnorm(100))
sErrors <- function(x) sd(x, na.rm=TRUE)/sqrt(length(x))








