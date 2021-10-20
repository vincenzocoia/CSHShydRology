#' Center of volume using Dery et al 2009 approach
#' 
#' @description
#' Creates four plot based on Dery et al 2009  and Whitfield 2013.
#' plot "a" Mean daily flow and cumulative flow vs day of water year with day of max and cov
#' plot "b" time series of total annual discharge
#' plot "c" time series of COV and 0.25 and 0.75 with MK test
#' plot "d" plot of COV vs Total annual discharge with test of linear relationship
#' plots "all" all four on one page
#' plots "each" all four plots, each on a separate page 
#' @details
#' Use can choose an individual plot a-d or  a plots of all four "all" (default)
#' 
#' @author Paul Whitfield 
#' 
#' @param DF - dataframe of daily flow data as read by ch_read_ECDE_flows
#' @param plots - one of "a", "b", "c", "d", "each" or "all"  
#' @param water_year - if TRUE uses a water year starting on 1 October, if FALSE uses 1 January.
#' @param metadata a dataframe of station metadata, default is HYDAT_list.
#'
#' @return a list containing:
#' \itemize{
#'   \item{Station}{Station ID eg. 05BB001}
#'   \item{years}{list of years that may span missing years}
#'   \item{mean_flow}{mean daily flow for DF, length of for 366 days}
#'   \item{cumulative_flow}{mean cumulative daily flow for DF, length of for 366 days}
#'   \item{doy}{a data frame with years, TAF, d_25, COV, d_75)
#'  }
#'   
#' @keywords plot
#' @import stats graphics grDevices
#' @importFrom Kendall MannKendall
#' @export 
#' 
#' @references Déry SJ, Stahl K, Moore RD, Whitfield PH, Menounos B, 
#' Burford JE. 2009b. Detection of runoff timing changes in pluvial, 
#' nival, and glacial rivers of Western Canada. Water Resources Research, 
#' 45. DOI: 10.1029/2008WR006975.
#' @reference Whitfield PH. 2013. Is 'Center of Volume' a robust indicator of
#'  changes in snowmelt timing? Hydrological Processes, 27: 2691-2698.

#'
#'  
#' @examples
#'  data(W05AA008)
#'  case <- ch_sh_cov(W05AA008, plots="all", water_year = TRUE)
#' 
#'


ch_sh_cov <- function ( DF, plots = "all", water_year= FALSE, metadata = NULL) {

  plots <- tolower(plots)
  DOY <- "Day of Year"
  
  station <- DF[1, 1]
  sname <- ch_get_wscstation(station, metadata = metadata)
  title=sname$Station_lname

# extract daily flow series
flow <- DF$Flow
date <- DF$Date

# 
# determine day of year (doy) for each daily obs
doy <- as.numeric( format( date, format = '%j') )
year <- as.numeric(format (date, format = '%Y') )

# set pointers for years

    min_year <- min(year, na.rm = TRUE)

if (min_year <= 1899) print ("strange year found", min_year)

   max_year <- max(year, na.rm=TRUE)
   na_year <- c(min_year : max_year)
   nyear <-length(na_year)
   iyear <- year - min_year + 1


# get doy indexes and water year

   if(water_year){
    doys <- ch_doys(date)
    doy <- doys$dwy
    year  <- doys$wyear
    DOY <- "Day of Water Year"
   }
   

# put data into a doy by year matrix
# define the matrix

   w_data <- matrix (
     data=NA,
     nrow = nyear,
     ncol = 366)


for (i in 1:length(flow)) {
  w_data[iyear[i], doy[i]] <- flow[i]
}

   
   
# Create the c_matrix and fill each row with the cumulative sum
c_data <- matrix (
  data = NA,
  nrow = nyear,
  ncol = 366)

# set the first column to be the first x value
c_data[ , 1] <- w_data[ , 1]

# accumulate across the days. [day 366 are preset to 0.]
for (i in 2 : 366){
   c_data[ , i] <- c_data[ , i-1] + w_data[ , i]
}

##################################################
# define the cumulative annual flow as the sum on day 365 omitting day 366

# find the day of the water year that 25%, 50%, and 75% of the flow has occurred

  c_volume <- array(NA, dim = c(nyear , 4))
  c_volume [ , 1] <- c_data[ ,365]
  c_volume [ , 2] <- c_volume[ , 1] * 0.25
  c_volume [ , 3] <- c_volume[ , 1] * 0.50
  c_volume [ , 4] <- c_volume[ , 1] * 0.75


  c_day <- array(NA, dim=c(nyear, 6))
  c_day[ , 1] <- na_year 
  # Convert from total Q in m3/s to m3 (x106)
  c_day[ ,2] <- c_volume[ , 1] * 60 * 60 * 24 / 1e6
  

# determine the day of the year that a specific flow accumulation occurred

  for (k in 1 : nyear) 
    {
    if (is.na (c_volume[k , 1])) {
        c_day[k, 3:5] <- NA 
          next
          }
    for (l in 3:5) 
    { 
  
    for (j in 1:365) 
      {
      if (c_data[k, j] <= c_volume[k, l - 1]) c_day[k, l] <- j
      
      }  ## j
    } ## l
   } #k

#################################################
  # Convert from total Q in m3/s to m3 (x106)
  c_day[ ,2] <- c_volume[ , 1] * 60 * 60 * 24 / 1e6

# calculate number of days between C.25 and c.75

  c_day[ ,6] <- c_day[ , 5] - c_day[ , 3]

  colnames (c_day)  <- c("year", "TAF", "d_25", "COV", "d_75", "range")

# extract COV and Range

  COV <- c_day[ , 4]
  R <- c_day[ , 6]

  
  
  
################################################ layout the plot page
mcex <- 1.3
if(plots == "all") 
  {
  par(mfrow = c(2, 2))
  mcex <- 0.95
}

if(plots == "each") 
{
  par(mfrow = c(1, 1))
  mcex <- 0.95
}
par(mar = c(4,5,3,1))

############################## Panel "1" plot mean hydrograph and cumulative flow
 qd <- array(NA, dim=366)
 
 for( kr in 1 :366) {
   qd[kr] <- mean(w_data[,kr], na.rm = TRUE)
 }

 qc <- array(NA, dim=366)
 
 for( kr in 1 : 366) {
   qc[kr] <- mean(c_data[,kr], na.rm = TRUE)
 }
 
 qd <- qd / max(qd, na.rm = TRUE)
 qc <- qc / max(qc, na.rm = TRUE)
 
 if (plots ==  "all" || plots == "each" || plots == "a") 
 {
  plot(1:365, qd[1:365], type = "l", lwd = 3.5 , col = "darkblue", lty = 1, 
       xlab = DOY, cex.main = mcex, ylim = c(0., 1.),
       ylab = "Daily mean flow / Maximum - Proportion of cumulative flow", main = title)
  points(1:365, qc[1:365], type = "l", col = "darkgreen", lty = 3, lwd = 3)
  
  abline(h = 0.0)
  abline (v = mean(c_day[, 4], na.rm = TRUE), col = "red", lwd=1.5)
  abline(v = which.max(qd), col = "blue", lwd = 1.5)
 }
  
  ############################## Panel '2' plot the TAF time series
 if (plots == "all" || plots == "each" || plots == "b")
   { 
  plot(na_year, c_day[ , 2], 
       ylab = "Total Annual Discharge (Mm\263)", las = 1,
       cex.main = mcex,
       ylim = c(0,max(c_day[ , 2], na.rm = TRUE)), 
       xlab = "", col = "darkgreen", pch = 19, type="o",
       main = title)
 }

 

  mktest <- Kendall::MannKendall(c_day[ , 4])
  
############################## Panel '3' plot the COV and range
  if (plots == "all" || plots == "each" || plots == "c")
    { 
  plot (c_day[ , 1], c_day[ , 4], 
         ylim = c(1,366), las = 1,
         ylab = paste("COV", DOY), 
         xlab = "",
         cex.main = mcex,
         pch = 19, col = "cyan" , cex = 0.7,
         main = title)
 points (c_day[ , 1], c_day[ , 3], type="l", col = "royalblue" )
 points (c_day[ , 1], c_day[ , 5], type="l", col = "royalblue" )
 

 midy <- as.integer(length(na_year)/3)
 
 text(c_day[midy,1], 55, paste("MK tau =", round(as.numeric(mktest[1]), 3)), cex=1.0)
 text(c_day[midy,1], 25, paste(" p_value=", round(as.numeric(mktest[2]), 3)), cex=1.0)
  }
 
############################## Panel  '4' plot the COV time series against total annual flow
  rg <- summary(lm(c_day[, 4] ~c_day[ , 2]))
  
  if (plots == "all" || plots == "each" || plots == "d")
   {  
   plot(c_day[ , 2],c_day[ ,4], 
        ylab="Total Annual Discharge (Mm\263)", las = 1,
        xlab = paste("COV", DOY), xlim = c(1,365),
        ylim = c(0,365),
        pch = 15, col = "darkblue",
        cex.main = mcex,
        main = title)
  
  
   if (rg$coefficients[2, 4] <= 0.05)
     {
     abline(a=rg$coefficients[1,1],b=rg$coefficients[2,1], lty=3, col="red")
     text(min(c_day[ ,2], na.rm=TRUE), 33, pos=4, paste("slope", round(rg$coefficients[2,1],3),
                                     "p_value=", round(rg$coefficients[2,4], 3)))
     text(min(c_day[ ,2], na.rm = TRUE), 10, pos=4, paste("intercept=", round(rg$coefficients[1,1],3),
                                       "p_value=", round(rg$coefficients[1,4],3)))                                 
     } 

  }
  if (plots == "all") par(mfrow = c(1, 1))
  
  result <- list(station, water_year, na_year, qd, qc, c_day)
  names(result) <- c("Station", "water_year","years", "mean_flow", "cumulative_flow", "doy" )
  return(result)
}
  