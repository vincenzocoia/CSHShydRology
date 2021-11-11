#'  check rho for ccm
#'  
#'  compares to values of rho, returns rho1 if >0 and > rho2


ch_check_rho <- function(rho1, rho2) {
  rhoa <- rho1
  for(ll in 1 : length(rho1)){
    if(rho2[ll] > rho1[ll]) rhoa[ll] <- 0.0
    if(rho2[ll] >= 0.5 * rho1[ll]) rhoa[ll] <- max(0, rho1[ll] - rho2[ll])
  }
  return(rhoa)
}