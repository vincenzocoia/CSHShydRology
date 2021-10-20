#'  wrapper for lfstat::tyears evaluating suitable low flow probability distributions
#'
#' @description 
#' This is wrapper that adds the station name to the plot showing the fit of selected distributions
#'
#' @param lfobj - a low flow object such as one  created by ch_lfobj from a DF (dataframe)
#' @param dist - a list of distributions e.g. "wei","gev","ln3","gum","pe3"
#' @param event - a set of return periods  eg. rp <- c(1.5, 5, 10, 100)
#' @param plot - generate a plot, default is TRUE.
#' 
#' @return a table of flows for different return periods
#' @return a plot demonstrating the fit to the data
#' 
#' @import from lfstat tyears
#' @export
#' 
#' @example
#' rp <-  c(1.5, 5, 10, 100)
#' mz <- ch_lf_test_dist(D2,dist = c("gum", "wei", "ln3", "pe3", "gevR"), event = rp )
#' 


ch_lf_test_dist <- function (lfobj, dist = c("gum", "wei", "ln3", "pe3"), event = rp)
{
  
par(oma = c(1, 1, 2, 1))
par(mar = c(4, 4, 4, 2))

z <-tyears(lfobj, dist = dist, event = event, plot = TRUE)
mtext(attr(lf_W05AA008, "lname"), side = 3, line =0.5, outer = TRUE, font = 2, cex = 1.3)

summary(z)
print("values in parenthesis in the legend are r.squared")
print(z$r.squared)
return(z)
}
##########################