#'  ch_polar_plot_peaks
#'
#'  Polar / circular plots of peak flows
#'
#' @description
#' Creates a polar plot of flow peaks in one of several different forms.  
#' Basic plot has shading for nival and pluvial centroids.
#'
#' @param title a title to be added to the plot
#' @param direction  a value or array of mean/median direction, circular mean or 
#' median of points from ch_circ_mean_reg (optional)
#' @param regularity a value or array of regularity from ch_circ_mean_reg (optional).
#' @param syms a value or an array of symbols to be used for centroids.  To be in color, 
#' must be one of 21 to 25 to get a symbol with border, elsewise a red symbol is plotted.
#' @param cols an array of colors, either numbers or names to apply to centroid points (optional, default is "red")
#' @param days an array of days of year to be plotted on perimeter (optional).
#' @param labels an array of labels to be placed beside points with direction and regularity (optional)
#' @param label_pos an array of positions indicating when label be placed (1, 2, 3, or 4 - below, left, 
#' above, right)(optional - default is below)
#' @param shading  if  = TRUE adds shading and labels for nival and pluvial regimes default = FALSE
#' @param pt_col colour used for points for events. default = "darkblue". If pt_col is an array it is used to colour 
#' the individual points of days
#' @param pt_cex
#'  
#'  @references Pewsey, A., M. Neuhäuser, and G. D. Ruxton. 2014. Circular Statistics in R, 
#'  192 pp., Oxford University Press.
#'  
#'  Whitfield, P. H. 2018. Clustering of seasonal events: A simulation study using 
#'  circular methods. Communications in Statistics - Simulation and Computation 47(10): 3008-3030.
#'  
#'  Burn, D. H., and P. H. Whitfield. 2021*. Changes in the timing of flood events resulting 
#'  from climate change.

#' @import circular
#'  
#' @example 
#' # base plot
#' ch_polar_plot_peaks()
#' ch_polar_plot_peaks(shading = TRUE)
#' 
#' # plot of annual maximum series
#' data(W05AA008)
#' am <- ch_sh_get_amax(W05AA008)
#' ch_polar_plot_peaks(days = am$doy, title = "05AA008")
#' 
#' #remove partial years
#' am <-am[am$days >= 365,]
#' ch_polar_plot_peaks(days = am$doy, title = "05AA008")
#' 
#' #plot the centroid
#' m_r <- ch_circ_mean_reg(am$doy)
#' ch_polar_plot_peaks(direction = m_r$mean, regularity = m_r$regularity, title = "05AA008")
#' 
#' # plot peaks and centroid
#' ch_polar_plot_peaks(days = am$doy, direction = m_r$mean, regularity = m_r$regularity, title = "05AA008")
#' 


ch_polar_plot_peaks <- function (title = NA, direction=NULL, regularity = NULL, days = NULL, shading = FALSE,
                                 labels=NULL, label_pos = NULL, syms=NULL, cols = NULL, 
                                 pt_col = "darkblue", pt_cex = 1, ...){
  
  if (is.null(label_pos)) label_pos <- rep(1,length(direction))
  if (is.null(cols)) cols <- rep("red", length(direction))
  if (is.null(syms)) syms <- rep(22, length(direction))
  
  ##### set some details by Paul
  dlabels <- c("1 Jan","1 Feb","1 Mar","1 Apr", "1 May", "1 Jun", "1 Jul","1 Aug","1 Sep",
               "1 Oct","1 Nov","1 Dec", "")
  
  dbreaks <- c(1,32,60,91,121,152,182,213,244,274,305, 335, 366)
  dbreaks <-dbreaks/365*2*pi
  
  
  
  #######  
  if(!shading){
    
    pt_a <-seq(dbreaks[4], dbreaks[8], length.out = 20)
    nival_r <-c(pt_a,rev(pt_a))
    nival_l  <- c(rep(1.0,20),rep(0.65,20))
    
    twhite <- RavenR::rvn_col_transparent("white", 50)
    
    plotrix::radial.plot(nival_l,nival_r,rp.type="p",
                         main = title,
                         radial.lim = c(0,1),
                         start = 3*pi/2,
                         clockwise = TRUE,
                         labels = dlabels,
                         label.pos = dbreaks,
                         line.col = twhite,
                         poly.col= twhite)
  }
  #######  
  if(shading){
    
    tblue <- RavenR::rvn_col_transparent("lightblue", 50)
    tgreen <- RavenR::rvn_col_transparent("green", 30)
    
    
    pt_a <-seq(dbreaks[4], dbreaks[8], length.out = 20)
    nival_r <-c(pt_a,rev(pt_a))
    nival_l  <- c(rep(1.0,20),rep(0.65,20))
    
    pt_b <-seq(dbreaks[1], dbreaks[2], length.out = 40)
    pluvial_r1 <-c(pt_b,rev(pt_b))
    pluvial_l1  <- c(rep(1.0,40),rep(0.45,40))
    
    pt_c <-seq(dbreaks[10], dbreaks[13], length.out = 40)
    pluvial_r2 <-c(pt_c,rev(pt_c))
    pluvial_l2  <- c(rep(1.0,40),rep(0.45,40))
    
    plotrix::radial.plot(nival_l,nival_r,rp.type="p",
                         main = title,
                         radial.lim=c(0,1),
                         start=3*pi/2,
                         clockwise=TRUE,
                         labels=dlabels,
                         label.pos=dbreaks,
                         line.col = tblue,
                         poly.col= tblue)
    
    text(0.1,0.88,"Nival", col="gray80",pos = 4)
    
    plotrix::radial.plot(pluvial_l1,pluvial_r1,rp.type="p",
                         radial.lim=c(0,1),
                         start=3*pi/2,
                         clockwise=TRUE,
                         line.col = tgreen,
                         poly.col= tgreen,
                         add = TRUE)
    
    
    plotrix::radial.plot(pluvial_l2,pluvial_r2,rp.type="p",
                         radial.lim=c(0,1),
                         start=3*pi/2,
                         clockwise=TRUE,
                         line.col = tgreen,
                         poly.col= tgreen,
                         show.grid=TRUE,
                         add = TRUE)
    
    text(0.1, -0.88,"Pluvial", col="gray80",pos = 4)
    
  }
  
  if(!is.null(direction) && !is.null(regularity)){
    
    direction <- direction / 365 * 2 * pi
    ptc <- rep("black",length(direction))
    if(max(syms <= 20)) ptc <-cols
    plotrix::radial.plot(regularity,direction,
                         rp.type="s",
                         point.symbols=syms,
                         point.col=ptc,
                         bg=cols,
                         start=3*pi/2,
                         labels=dlabels,
                         label.pos=dbreaks,
                         clockwise=TRUE,
                         cex=1.5,
                         radial.lim=c(0,1),
                         show.grid.labels=4,
                         show.grid = TRUE,
                         show.radial.grid = TRUE,
                         add=TRUE)
  }
  
  
  if(!is.null(labels)) {
    
    plotrix::radial.plot.labels(regularity, direction,  labels=labels, pos = label_pos,
                                start=3*pi/2, cex=0.7,
                                radial.lim=c(0,1),
                                clockwise=TRUE)
    
  }
  
  
  if(!is.null(days)){
    
    if(length(pt_col) == 1) pt_col <- rep(pt_col,length(days))
    if(length(pt_cex) == 1) pt_cex <- rep(pt_cex,length(days))
    # link days and poiunt colours and sort into day order   
    # order days in ascending order
    adays <- data.frame(days,pt_col, pt_cex)
    adays <- adays[order(days),]
    
    # convert doy to radians
    days <- adays$days / 365 * 2 * pi
    id <- rep(1.005, length(days))
    
    for( ii in 2:length(days)){
      if (days[ii] == days[ii-1]) id[ii] <-id[ii-1] + 0.02
    }
    
    plotrix::radial.plot(id,days,
                         rp.type="s",
                         point.symbols = 20,
                         point.col = adays$pt_col,
                         cex = adays$pt_cex,
                         
                         start = 3*pi/2,
                         labels = dlabels,
                         label.pos = dbreaks,
                         clockwise = TRUE,
                       
                         radial.lim = c(0,1),
                         show.grid.labels = 4,
                         show.grid = TRUE,
                         show.radial.grid = TRUE,
                         add = TRUE)
  }
  
  
}