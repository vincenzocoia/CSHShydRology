#'  ch_lf_get_MAM
#'
#' Extract the mean annual minimum series from daily streamflow dataframe from ECDE
#'
#' @description 
#' Extracts mean annual minimum values, the date of occurrence, the day of year, and the completeness 
#' from ECDE dataframe. 
#' 
#' uses functions from timeDate [as.timeDate, dayOfYear]
#'
#' @param df - a dataframe of daily streamflow data from ECDE
#' @param n n is the number of days in the annual mean. Default is n = 7.
#' @return dataframe with the following variables
#' \item year
#' \item annual maximum
#' \item date of annual maximum
#' \item day of year of annual maximum
#' \item days number of days with observations
#'
#' @export
#' 
#' @author Paul Whitfield
#' 
#' @example
#' data(W05AA008)
#' mam7 <- ch_sh_get_MAM(W05AA008, n = 7, hyear = 10)
#' str(mam7)
#'
#'
#'
#'

ch_sh_get_MAM <- function (dataframe , n = 7, hyear = 10) {
  
  data <- dataframe$Flow
  Date <- dataframe$Date
  
 # shift to hydrologic year
  mdoys <- ch_doys(Date, water_yr = hyear)
  wyear <- mdoys$wyear
  wdoy <- mdoys$dwy
  
  Year <- as.numeric(unique(wyear))
   # arrays to store results
  
  
  mindate <- array(NA, dim=length(Year))
  doy <- array(NA, dim= length(Year))
  days <- array(NA, dim= length(Year))
  
 
  class(mindate) <- "Date"
  year <- as.factor(wyear)

 
  mam <- as.numeric(tapply(data,wyear,min))
  
  dataframe <-data.frame(dataframe,year)
  
  for(k in 1:length(Year)) {
    ndata <- dataframe[dataframe$year==Year[k],]
    days[k] <- length(ndata$Flow)
    
    ndata <- ndata[ndata$Flow==mam[k],]
    mindate [k] <- ndata[1, 3]
    mindate_a <-timeDate::as.timeDate(mindate[k])
    doy[k] <- timeDate::dayOfYear(mindate_a)
  }
  
  result <- data.frame(Year,mam, mindate, doy, days)
  
  return(result)
  
}