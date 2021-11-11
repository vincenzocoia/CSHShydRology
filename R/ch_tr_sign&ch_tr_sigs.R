#'  Convert slopes to integers 1-2-3 negative, none, positive
#'  
#'  
#'  @param array of values
#'  @return array of indices 1, 2, 3



ch_tr_sign <- function (x, offset=2){
  x <- unlist(x)
  x <- x/abs(x)
  x <- replace(x, is.nan(x), 0)
  x <- x + offset
  x <- as.numeric(x)
}
  

#'  Convert slopes to integers 1-2-3 negative, none, positive
#'  
#'  Convert significances to 1 NS and 2 significant using a pvalue that can be set (default is 0.05)
#'  
#'  @param array of values
#'  @return array of indices 1 and 2 where 1 is NS and 2 is Significat
#'  
ch_tr_signif <- function(x, pvalue = 0.05){
  x <- unlist(x)
  x <- replace(x, x  > pvalue, 1)
  x <- replace(x, x <= pvalue, 2)
  x <- as.numeric(x)
}


ch_check_rho <- function(rho1, rho2) {
  rhoa <- rho1
  for(ll in 1 : length(rho1)){
    if(rho2[ll] > rho1[ll]) rhoa[ll] <- 0.0
    if(rho2[ll] >= 0.5 * rho1[ll]) rhoa[ll] <- max(0, rho1[ll] - rho2[ll])
  }
  return(rhoa)
}