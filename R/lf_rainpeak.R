#'  lfstat::rainpeak
#'  

#' @description Originally a hidden function in lfstat.  This version copies source code into the CSHShydRology package
#' so functions parallel to the original lfstat::functions use the same code.
#' 
#' lfrainpeak is not exported from lfstat.  
#' 
#'
#'
#'


rainpeak <- function(x, p=0.95){
  if(is.logical(p)){
    if(length(x) == length(p)){
      return(p)
    } else {
      stop("p and lfobj$flow differ in length")
    }
  }
  
  rp <- FALSE
  for(ii in 2:(length(x)-1)){
    rp[ii] <- (x[ii]*p >= x[ii-1] & x[ii]*p >= x[ii+1])
    #to remove NAs
    rp[is.na(rp)] <- FALSE
  }
  rp[length(x)] <- FALSE
  rp
}
