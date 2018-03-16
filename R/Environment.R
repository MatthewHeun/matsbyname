# This file establishes an environment (consts_env) that contains 
# a constant for multi-threaded mcMap-ping in the various
# *_byname and *apply_byname functions.
# It also supplies functions to get and set the number of cores to be used
# across the entire package.
library(parallel)

get_mc.cores <- function(){
  return(get("mc.cores", envir = consts_env))
}

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
# NEVER submit to CRAN with this line uncommented.
# set_mc.cores(detectCores(logical = FALSE))
