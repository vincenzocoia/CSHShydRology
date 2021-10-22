#'  ch_circ_mean_reg
#'  
#'  
#'  Calculate the circular mean, median, and regularity using a 365 days.
#'  Days of year are converted to degrees
#'  internally, results are returned as positive days of year
#'  
#'  
#'  @param doy and array of day year of event; can be amax or pot.  
#'  
#'  @return 
#'  \item n
#'  \item mean
#'  \item median
#'  \item rho
#'  
#'  @references Pewsey, A., M. Neuhäuser, and G. D. Ruxton. 2014. Circular Statistics in R, 
#'  192 pp., Oxford University Press.
#'  Whitfield, P. H. 2018. Clustering of seasonal events: A simulation study using 
#'  circular methods. Communications in Statistics - Simulation and Computation 47(10): 3008-3030.
#'  Burn, D. H., and P. H. Whitfield. 2021*. Changes in the timing of flood events resulting 
#'  from climate change.

#' @import circular
#'  
#' @example 
#' data(W05AA008)
#' am <- ch_sh_get_amax(W05AA008)
#' m_r <- ch_circ_mean_reg(am$doy)

require(circular)

ch_circ_mean_reg <- function (doys){
  
  n <- length(doys)
  doys <- doys * 360/365
  
  
   x <- circular(doys, units = "degrees", zero=pi/2, rotation="clock")
  
 meanday <-  mean.circular(x)
 if(meanday < 0) meanday <- 360 + meanday
 medianday <- median.circular(x)
 if(medianday < 0) medianday <- 360 + medianday
 rho <- rho.circular(x)
 
 result <- list(n, as.numeric(meanday)*365/360, as.numeric(medianday)*365/365, rho)
 names(result) <- c("n", "mean", "median", "regularity")
 
  return(result)
}