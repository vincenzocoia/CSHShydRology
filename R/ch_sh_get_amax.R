#'  ch_sh_get_amax
#'
#' Extract the annual maximum series from daily streamflow dataframe from ECDE
#'
#' @description 
#' Extracts annual maximum values, the date of occurrenc, the day of year, and the completness 
#' from ECDE dataframe. 
#' 
#' uses functions from timeDate [as.timeDate, dayOfYear]
#'
#' @param df - a dataframe of daily streamflow data from ECDE
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
#' amax <- ch_sh_get_amax(W05AA008)
#' str(amax)
#'
#'
#'
#'

ch_sh_get_amax <- function (dataframe) {
  
  data <- dataframe$Flow
  Date <- dataframe$Date
  year <- format(Date, "%Y")
  Year <- as.numeric(unique(year))
  maxdate <- array(NA, dim=length(Year))
  doy <- array(NA, dim= length(Year))
  days <- array(NA, dim= length(Year))
  class(maxdate) <- "Date"
  year <- as.factor(year)
  
  amax <- as.numeric(tapply(data,year,max))
  
  dataframe <-data.frame(dataframe,year)
  
  for(k in 1:length(Year)) {
    ndata <- dataframe[dataframe$year==Year[k],]
    days[k] <- length(ndata$Flow)
    
    ndata <- ndata[ndata$Flow==amax[k],]
    maxdate [k] <- ndata[1, 3]
    maxdate_a <-timeDate::as.timeDate(maxdate[k])
    doy[k] <- timeDate::dayOfYear(maxdate_a)
  }
  
  result <- data.frame(Year,amax, maxdate, doy, days)
  
  return(result)
  
}