#'  lfstat::lfcheck
#'  

#' @description Originally a hidden function in lfstat.  This version copies source code into the CSHShydRology package
#' so functions parallel to the original lfstat::functions use the same code.
#' 
#' lfcheck is not exported from lfstat.  
#' 



lfcheck <- function(lfobj){
  if(!is.lfobj(lfobj)){
    stop("This functions is designed for objects of the class 'lfobj'. ",
         "Please use 'createlfobj()' or see '?createlfobj' for more information")
  }
}

is.lfobj <- function(x) {
  inherits(x, "lfobj") &
    all(c("day", "month", "year", "flow", "hyear") %in% colnames(x))
}

