#'  replacement for lfstat::sbplot

#'
#' @description 
#' Replacement for lfstat::sbplot.  The original provides a bar chart, while this version shows box and whisker plots for each month and the magnitude
#' of the 0.1 quantile a in WMO Manual.
#'
#' @param lfobj - a low flow object such as one  created by ch_lfobj from a DF (dataframe)
#' @param hyearorder - if TRUE uses the current month attached to the lfobj to order months, 
#' if FALSE uses calendar year.
#' @param col - a colour for the discharge default is "green"
#' @param bfcol - a colour fro the baseflow, default is "blue"
#' @param main - when default the title is obtained from the lfobj. option to a different title
#' @param plot - generate a plot default is TRUE
#' 
#' @return a plot showing monthly flows and the 0.1 quantile which can
#' @return a list with station and table of Q10 values by month 
#' 
#' @import lfstat
#' @export
#' 
#' @example
#' data(W05AA008)
#' metadata <- data(HYDAT_list)
#' mdf <- ch_cut_block(W05AA008, "1970/01/01", "2000/12/31")
#' lf_W05AA008 <- ch_lfobj(mdf)
#' 
#' mr <- ch_lf_sbplot(lf_W05AA008)
#' 
#'  

ch_lf_sbplot  <- function (lfobj, hyearorder = TRUE, plot = TRUE , quantile = 0.1) 
{
  lfcheck(lfobj)

    hyr <- as.integer(attr(lfobj,"lfobj")) -1
     
    monthreorder <- lfobj$month
  
  if (hyearorder) {

  for (kk in 1 :length(monthreorder)){
    if (monthreorder[kk] <= hyr) monthreorder[kk] <- monthreorder[kk] + 12
  }
    monthreorder <- monthreorder - 9

      }


  mquan <- aggregate(flow ~ month, data = lfobj, FUN = "quantile", probs = 0.1)
  
  mqreorder <- mquan$month

  for (kl in 1 :length(mqreorder)){
    if (mqreorder[kl] <= 9) mqreorder [kl] <- mqreorder[kl] + 12
  }

  mqreorder <- mqreorder - 9
  mquan$mreorder <- mqreorder
  
 par(mar = c(4, 5, 3, 2))
  #######################   
  mmplot <- {boxplot(flow ~  reorder(month, monthreorder), data = lfobj, log="y", 
                    xlab = "month", ylab = attr(lfobj, "ylabel"), las=1,
                    col = "cyan", main = attr(lfobj, "lname"))
     points(mquan$mreorder, mquan$flow, pch=15, col="red")
     legend ("topleft", legend = c("daily flows", paste(quantile, "quantile")), col=c("cyan" ,"red"), 
             pch = 15, bty = "n")
  }
 
  if(plot) mmplot
  result <- list(attr(lfobj, "lname"), mquan)
  names(result) <- c("station", "Q10_by_month" )
 return (result)
}