# This file establishes an environment (consts_env) that contains 
# a constant for multi-threaded mcMap-ping in the various
# *_byname and *apply_byname functions.
# It also supplies functions to get and set the number of cores to be used
# across the entire package.
library(parallel)

#' Get number of cores to be used for parallelized matsbyname functions
#' 
#' Get the package-wide value for the number of cores to be used for parallized
#' \pkg{matsbyname} functions.
#' The default value is \code{1}.
#' 
#' If the \code{mc.cores} argument is set on a \pkg{matsbyname} function, 
#' the argument's value is used instead of the package-wide default value obtained from this function.
#' 
#' @return the package-wide value for number of cores to be used for \pkg{matsbyname} functions
#' 
#' @export
#'
#' @examples
#' get_mc.cores()
get_mc.cores <- function(){
  return(get("mc.cores", envir = consts_env))
}

#' Set number of cores to be used for matsbyname functions
#' 
#' Set a package-wide variable for the number of cores to be used for 
#' \pkg{matsbyname} functions.
#' The default value is \code{1}.
#' 
#' Note that all \pkg{matsbyname} functions also have a \code{mc.cores} argument.
#' If set, the \code{mc.cores} argument overrides the package-wide value.
#' 
#' @param mc.cores the number of cores to be set as the package-wide default
#'
#' @return nothing; this function is called for its side effects
#' 
#' @export
#'
#' @examples
#' set_mc.cores(1)
set_mc.cores <- function(mc.cores){
  if (mc.cores <= 0) {
    stop("mc.cores must be >= 1.")
  }
  assign("mc.cores", mc.cores, envir = consts_env)
}

consts_env <- new.env()

# Set default to 1 core.
set_mc.cores(1)

# Use this line for local testing only. 
# NEVER submit to CRAN with this line uncommented, 
# because CRAN will not accept multi-threaded test code.
# If this is a problem, I should be able to catch it, because
# a test in test_Environment.R will fail if this line is uncommented.
# set_mc.cores(detectCores(logical = FALSE))
