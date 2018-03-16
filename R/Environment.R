# This file establishes an environment that contains 
# a constant for multi-threaded mapping in the various
# *apply_byname functions.
library(parallel)

get_cores <- function(){
  return(get("n.cores", envir = consts_env))
}

set_cores <- function(cores = detectCores(logical = FALSE)){
  assign("n.cores", cores, envir = consts_env)
}

get_multicore <- function(){
  return(get("mc", envir = consts_env))
}

set_multicore <- function(multicore = FALSE){
  assign("mc", multicore, envir = consts_env)
}

consts_env <- new.env()
set_multicore()
set_cores()
