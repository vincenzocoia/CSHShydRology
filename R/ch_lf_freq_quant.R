#'  wrapper for tyears evaluating distributions
#'
#' @description a wrapper for lfstat::tyears determining magnitudes of low flows for a selection of return periods. The
#' wrapper adds station name to the plot.
#'
#' @param lfobj - a low flow object such as one  created by ch_lfobj from a DF (dataframe)
#' @param dist - a distributions such as: "wei","gev","ln3","gum","pe3"
#' @param rp - a set of return periods  eg. rp <- c(1.5, 5, 10, 100)
#' @param plot - generate a plot, default is TRUE.
#' 
#' @return a plot demonstrating the fit to the data and the flows at rp quantile
#' @return a list containing stationid, dist, rp, and flow values for different return periods
#'  
#' @import lfstat
#' @export
#' 
#' @example
#' rp <-  c(1.5, 5, 10, 100)
#' mz <- ch_lf_freq_quant(lf_W05AA008, dist = "ln3", rp = rp )
#' 
#' # applying a moving average before fitting
#' D7 <- lf_W05AA008
#' D7$flow <- ma(D7$flow, n = 7)   # n is number of days to be averaged
#' mz <- ch_lf_freq_quant(D7, dist = "gevR", rp = rp )

ch_lf_freq_quant  <- function (lfobj, dist = "gevR", rp = rp)
  {


################################################################
par(oma = c(1, 1, 2, 1))
par(mar = c(4, 4, 4, 2))

y <- tyears(lfobj, dist = dist, event = rp)
rpline(y, return.period = rp, suffix = c("a", "m\u00B3"), digits = 2)

mtext(attr(lfobj, "lname"), side = 3, line =0.5, outer = TRUE, font = 2, cex = 1.3)
summary(y)

result <- list(attr(lfobj, "station"), dist, rp,  y)
 names(result) <- c("station", "dist", "rp", "Qs")                    

 return(result)

}