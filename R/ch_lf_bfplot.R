#'  a replacement for bfplot in lfstat
#'
#' @description 
#' replacement for bfplot in lfstat
#'
#' @param lfobj - a low flow object such as one  created by ch_lfobj from a DF (dataframe)
#' @param year - any of the following: "any", "all", a single year e.g. 1990, or a range 1990:1995.
#' @param col - a colour for the discharge, default is "green"
#' @param bfcol - a colour for the baseflow, default is "blue"
#' @param main - default is that the plot title is obtained from the lfobj. main= an option to a different title
#' @param ylog - option to use a log scale for y axis
#' 
#' @return a plot showing the daily flow and baseflow
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
#' ch_lf_bfplot(lf_W05AA008)
#' 
#'

ch_lf_bfplot <- function (lfobj, year = "any", col = "green", bfcol = "blue", main=NULL, 
          ylog = FALSE, ...) 
{
    lfnacheck(lfobj)
  
  if (is.null(main)) main = attr(lfobj, "lname")
  
  if (ylog) {
    ln <- "y"
  }
  else {
    ln <- ""
  }

  ##################   any / all years
  
    if (year == "any" || year == "all" ) {
    plot(lfobj$flow, type = "l", col = col, xaxt = "n", main = main,
         xlab = "", ylab = attr(lfobj, "ylabel"), log = ln, 
         xaxs = "i", mgp = c(1.5, 0.5, 0), tcl = -0.3)
    lines(lfobj$baseflow, col = bfcol)
    monstart <- which(lfobj$day == 1)
    if (length(monstart) > 60) {
      monstart <- which(lfobj$day == 1 & lfobj$month == 
                          1)
    }
    year <- lfobj$year[monstart]
    months <- lfobj$month[monstart]
    label <- paste(months, year, sep = "/")
 
    
    axis(1, at = monstart, labels = label)
    grid(nx = NA, ny = NULL)
    abline(v = monstart, col = "lightgray", lty = "dotted")
    return()
    }
  
  
  ##################   a single specific year
  
  if (length (year) == 1 && is.numeric(year)) 
    {
    
    plot(lfobj$flow[lfobj$hyear == year], main=main,
         type = "l",
         col = col,
         xaxt = "n",
         xlab = "",
         ylab = attr(lfobj, "ylabel"),
         log = ln,
         xaxs = "i",
         mgp = c(1.5,0.5,0),
         tcl = -.3)
    dummi <- year
    months <- which(lfobj$day[lfobj$hyear == year] == 1)
    monthsex <-  which(subset(lfobj, hyear == dummi, month)[months[1], ] == lfobj$month & lfobj$day == 1 & lfobj$hyear == (year + 1))
    lab <-rbind(subset(x = lfobj, hyear == dummi, select = c(month, year))[months, ],
                c(lfobj$month[monthsex],lfobj$year[monthsex]))
   
    months <- c(months,366)
    label <- paste(lab$month,lab$year,sep = "/")
    
    axis(1, at = months, labels = label, mgp = c(1.5,0.5,0), tcl = -.3,cex.axis = 0.8)
    grid(nx = NA, ny = NULL)
    abline(v = months,col = "lightgray", lty ="dotted")
    lines(lfobj$baseflow[lfobj$hyear == year],
          col = bfcol)
  }
  
  
  ##################  a select range of years
  
  if (length (year) > 1 && is.numeric(year)) 
    {

        l_obj <- lfobj[lfobj$hyear >= year[1],]
    l_obj <-l_obj[l_obj$hyear <= year[length(year)],]
    
    plot(l_obj$flow, type = "l", col = col, main = main, 
         xaxt = "n", xlab = "", ylab = attr(lfobj, "ylabel"), xaxt = "n",
         log = ln, xaxs = "i", mgp = c(1.5, 0.5, 0), tcl = -0.3)
    dummi <- year[1]
    months <- which(l_obj$day[l_obj$hyear == year] == 1)
    monthsex <- which(subset(l_obj, hyear == dummi, month)[months[1], 
    ] == l_obj$month & l_obj$day == 1 & l_obj$hyear == (year + 1))
    
    lab <- rbind(subset(x = l_obj, hyear == dummi, select = c(month, 
                year))[months, ], c(l_obj$month[monthsex], l_obj$year[monthsex]))
    
    print(year)
    years <- c(0 : length(year)) 
    years <- years *366
    years <- years + 1
    label <- c(year, "")
  
    axis(1, at = years, labels = label, mgp = c(1.5, 0.5, 0), 
         tcl = -0.3, cex.axis = 0.8)
    grid(nx = NA, ny = NULL)
    abline(v = years, col = "lightgray", lty = "dotted")
    lines(l_obj$baseflow, col = bfcol)
  }
}