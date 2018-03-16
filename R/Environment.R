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
  assign("mc.cores", mc.cores, envir = consts_env)
}

consts_env <- new.env()
set_mc.cores(1)
# set_mc.cores(detectCores(logical = FALSE))
