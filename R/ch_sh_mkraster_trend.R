#' analysis of trends in n-day periods  using Dery 2009 approach
#' 
#' @description
#' Creates a raster plot of Mann Kendall trends over periods of year in n year block 
#' which may be binned by a number of days (step = ),
#' and the max, min and median across years. The plot contains four panels based upon binned data.
#'
#' @details
#' Produces two plots.  The first shows the hydrographs for the first and last n_av year periods
#' as in Dery et al (2009).  The second plot show a raster plot for n-day bins indicating the Sen's slope 
#' of each bin if mask=FALSE.  Missing data are shaded dark gray.  If mask = TRUE only significant values
#' are unmasked.  The result returned contains all the values used
#' 
#' @author Paul Whitfield 
#' 
#' @param DF - dataframe of daily flow data as read by ch_read_ECDE_flows
#' @param step - a number indicating the number of days used for binning (smoothing) eg. 1, 5, 11.
#' @param mask - mask non-significant trends (default = TRUE) or show all (mask = FALSE).
#' @param sig_level - what level of trends are masked. Default is 0.05 but other values can be used.
#' @param n_av - number of years to be used to average for starting and ending comparison. Default is 5.
#' @param cent - either "mean" (default) or "median" for value in bins.
#' @param metadata a dataframe of station metadata, default is HYDAT_list.
#' @param alt_year can be used to prepend years so figures can be of same scales as in Dery et al. 2009. Default = NULL.
#' @param water_year default is FALSE, if TRUE starts plots on October 1st.
#'
#' @return a list containing:
#' \itemize{
#'   \item{sID}{Station ID eg. 05BB001}
#'   \item{step}{number of days in a bin}
#'   \item{cent}{mean or median used bin}
#'   \item{periods}{number of periods in a year}
#'   \item{period}{period numbers i.e. 1:365/step}
#'   \item{z_flows}{standardized flows for each period in each year}
#'   \item{MK_tau}{MK_tau for each period in each year}
#'   \item{p_MKtau}{probability of MK_tau for each period in each year}
#'   \item{n_av}{number of years averaged at start and end of data}
#'   \item{starting_flow}{flows for\code{n_av} years at the beginning of record}
#'   \item{ending_flow}{flows for\code{n_av} years at the end of record}
#'  }
#'   
#' @keywords plot
#' @import stats graphics grDevices
#' @importFrom Kendall MannKendall
#' @importFrom fields image.plot
#' @importFrom graphics axis legend par plot points polygon
#' @export 
#' @seealso \code{\link{ch_flow_raster_trend}}
#' 
#' @references Déry SJ, Stahl K, Moore RD, Whitfield PH, Menounos B, 
#' Burford JE. 2009b. Detection of runoff timing changes in pluvial, 
#' nival, and glacial rivers of Western Canada. Water Resources Research, 
#' 45. DOI: 10.1029/2008WR006975.
#'
#' @import from rkt  rkt
#' @import from fields  image.plot
#' 
#'
#'  
#' @examples
#'  data(W05AA008)
#'  mplot <- ch_sh_mkraster_trend(W05AA008, step = 5)
#'

ch_sh_mkraster_trend <- function(DF, step = 5, mask = TRUE, sig_level = 0.05, n_av = 5, cent = "mean",
                                alt_year= NULL, water_year= FALSE, metadata = NULL )
{

### set fixed labels etc.
  l_disch <- expression(paste("m"^{3}, "/sec"))
  l_disch2 <- expression(paste("\nm" ^{3}, "/sec"))


  month  <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "")  
  mday   <- c(0, 31, 59, 90, 120, 151, 181, 212,243, 273, 304, 334,365)
  wmonth <- c( "O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S", "")
  wmday  <- c(0, 31, 61, 92, 123, 151, 182, 212,243, 273, 304, 335,365)
 
  mond <- (mday / 365*as.integer(365 / step)) + 1
  wmond <-(wmday / 365*as.integer(365 / step)) + 1
  
  DOY <- paste("Period of Year (", step, " day)", sep = "")
  DOWY <- paste("Period of Water Year (", step, " day)", sep = "")
  zs <- "Z_score"
  n_av <- n_av - 1
  
### get title information
  station <- DF[1, 1]
  sname <- ch_get_wscstation(station, metadata = metadata)
  title=sname$Station_lname
  
  
  Date <- DF$Date
  Flow <- DF$Flow
  
  ###  nornmalize flows to zero mean and unit variance
  Flow <- (DF$Flow - (mean (DF$Flow, na.rm = TRUE))) / sd(DF$Flow, na.rm = TRUE)

             # get doy and year
  doy_vals <- ch_doys(Date)
  Year <- doy_vals$year
  doy <- doy_vals$doy

  if (water_year)
  {
    doy <- doy_vals$dwy
    Year <- doy_vals$wyear
    DOY <- DOWY
  }
  
  if (step >= 31) {
    print("step has been reset to the maximum allowed [30] ")
    step <- 30
  }
  
  days <- 365
  periods <- days / step
  periods <- round(periods, digits = 0)
  period <- c(1:periods)
  ## Some records have stretches of missing years so the data needs to be reconfigured to individual years which have no record.
  
  mYear <- max(Year, na.rm = TRUE)
  nYear <- min(Year, na.rm = TRUE)
  n1year <- nYear
 
  if (!is.null(alt_year)) 
    {
    n1year <- nYear
    nYear <- alt_year
  }
  
  sYear <- nYear - 1
  nyears <- mYear - sYear ## total number of years
  n1years <-mYear - n1year + 1
  yoffset <- nyears-n1years

  ayears <- c(nYear: mYear) ## actual years in range
  uYear <- unique(Year)
  uyr <- length(uYear)
  

  mslice <- ch_slice(doy, step) ###  create a factor for n day periods
  myear <- as.factor(Year)
  fac <- list(myear, mslice)
  
 if (cent == "median") q_sliced <- tapply(Flow, fac, median)  # get median value for each bin.
 if (cent == "mean") q_sliced <- tapply(Flow, fac, mean)      # get mean value for each bin.
  

qsliced <- array(NA, dim=c(nyears, periods))
   

    for (k in 1: uyr) {
    for (kk in 1:periods) {
        qsliced[ (uYear[k] - sYear), kk] <- q_sliced[k, kk]
    }
  }
  
  qsliced <- matrix(qsliced, nrow=nyears, ncol=periods)
  rownames(qsliced) <- ayears
  colnames(qsliced) <- period

 ################################################################################
  qstart <- array(NA, dim=periods)
  qend <- array(NA, dim=periods)
  
  for (i in 1:periods){
    qstart[i] <- mean(qsliced[1 + yoffset: 1 + yoffset + n_av, i], na.rm = TRUE)
    qend [i]  <- mean(qsliced[nyears:(nyears - n_av), i], na.rm=TRUE)
  }
  

  ######################################### plot starting and  ending hydrograph

  par(mar = c(5, 5, 4, 6))
  y1lims <- c(min(qstart, qend, na.rm = TRUE), max(qstart, qend, na.rm = TRUE))
  lcol <- c("black", "red", "white")
  
  plot(1:periods,qstart, 
       type = "l", lwd = 2, ylim = y1lims, xlab = "", ylab=zs, las = 1,
       xlim = c(1, periods), col = lcol[1], main = title)
  points(1:periods, qend, type = "l", lwd = 2, col = lcol[2])
  mtext(DOY,side = 1, line = 1.9, cex = 0.9) 
  if(!water_year) axis(1, line = 3.0, at = mond, month)
  if(water_year) axis(1, line = 3.0, at = wmond, wmonth)
  
  lbls <- c(paste(ayears[1 + yoffset],"-",ayears[1 + yoffset + n_av]),paste(ayears[nyears - n_av],"-",ayears[nyears]),
            paste(n_av + 1,"year mean"))
  legend("topleft", legend = lbls, col = lcol, lwd = 2, bty = "n")           
  ############################################################  end plot of start and end periods
  
  
  sens <- matrix(NA, nrow=nyears, ncol=periods)
  tau  <- matrix(NA, nrow=nyears, ncol=periods)
  p_mk <- matrix(NA, nrow=nyears, ncol=periods)
  #########################################################  new code to get sen slope etc
  for (ka in 1: (nyears- n_av)) {
  
    for (kb in 1:periods) {
      my <- as.numeric(ayears[ka:nyears])
      md <- qsliced[ka:nyears, kb]

    if (!is.na(md[1])) 
        {
      mt <- cbind(my,md)
      mt <- mt[complete.cases(mt),]
      mk <-rkt::rkt(mt[ , 1], mt[ , 2])
      
      sens[ka,kb] <- mk$B
      tau[ka,kb] <- mk$tau
      p_mk[ka,kb] <- mk$sl
    }}
  }
  
  
  #########################################################  end of new code to get sen slope etc
  
  senst  <- t(sens)
  p_mkt  <- t(p_mk)
  
  ###########################################
  par(mar = c(5, 5, 4, 2))

    red2blue <- c("red", "gray97" , "darkblue")
  rbcol <- colorRampPalette(red2blue)
  smin <- min(sens,-max(sens, na.rm = TRUE), na.rm=TRUE)
  smax <- max(sens, -min(sens, na.rm = TRUE), na.rm=TRUE)

  fields::image.plot(1:periods, 1:nyears, senst, axes = FALSE, col = rbcol(51),
        zlim = c(smin, smax),  legend.shrink = 0.4, main=title, 
        legend.lab = expression(paste("Sen's slope m"^{3}, "/sec/year")), 
        legend.cex = 0.8, 
        xlab = "", ylab = "")  
  
  sstep <-round(periods/15)
  speriod <- ch_sub_set_Years(period,sstep)
  axis(1, at = speriod$position, labels = speriod$label, cex = 1.2, las = 1)
  
  nn <- 1
  if (length(ayears) >= 70) nn <- 10 
  if (length(ayears) >= 40) nn <-  5
  if (length(ayears) >= 20) nn <-  2
  sYears <- ch_sub_set_Years(ayears, nn)
  
  
  axis(2, at = sYears$position, labels = sYears$label, cex.axis = 0.7, las = 1)
  mtext(DOY,side = 1, line = 1.9, cex = 0.9)  
  box()
  
  ####################axis(1, line = 3.5, at = mond, month, las=1)
  if(!water_year) axis(1, line = 3.0, at = mond, month)
  if(water_year) axis(1, line = 3.0, at = wmond, wmonth)

  ################################################################### create a mask for NA and add
  na_mask <-senst
  na_mask[is.na(na_mask)] <-99
  na_mask[na_mask < 99] <- NA
  image(1:periods, 1:nyears,na_mask, axes = FALSE, add=TRUE,col = "gray25")
  ###################################################################create a map base on significance

 if(mask) {
   p_mk1 <-p_mkt
   p_mk1[p_mk1 > sig_level] <- 1
   p_mk1[p_mk1 <= sig_level] <- NA
  image(1:periods, 1:nyears, p_mk1, axes = FALSE, add=TRUE,col = "gray65")
 }  

  
###########################################################################
###########################################################################

  sID <- substr(title, 1, 7)
  n_av <- 1 + n_av
  
  result <- list(sID, step, cent, periods, period, qsliced, tau, p_mk, n_av, qstart, qend)
  names(result) <-c("sID", "step", "cent", "periods", "period", "z_flows","MK_tau", "p_mktau", "n_avg","starting_flow",
                   "ending_flow")
  return(result)
}
