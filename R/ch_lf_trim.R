#'   remove partial years from original dataframe
#'   
#' @description  lfstat often uses partial years before and after a time period of interest 
#' to get annual minimum without a way to remove them.  This function trims to a time period
#'   
#' @param DF dataframe of daily streamflow data
#' @param st_year  the year in which records will start if available, if not records will be trimmed to hydrological years
#' @param hyear  month in which the hydrological year starts  default is hyear = 10
#'   
#' @result  a dataframe trimmed into hydrological years with partial years removed before a st_year and for whole years following
#'
#'
#' @example
#' data(W05AA008)
#' part_df <- ch_lf_trim(W05AA008, st_year = 1970, hyear = 10)
#'


ch_lf_trim <- function (DF, st_year, hyear=10) {
  
  st_date <- as.Date(ISOdate(st_year,hyear,1))

 
  dy <-c(31, 28, 31, 30 ,31, 30, 31, 31, 30, 31, 30, 31)
  
  first_day <- DF$Date[1]
  last_day <- DF$Date[length(DF[,1])]
  

  
  start_date <- as.Date(ISOdate( format( first_day, "%Y"), hyear,1))
  end_date   <- as.Date(ISOdate( format( last_day, "%Y"), hyear-1, dy[hyear-1]))
  

  
  if(st_date >= first_day){    #### case where desired records start after records exist
   
    DF <- DF[DF$Date >= st_date, ]
    DF <- DF[DF$Date <= end_date, ]
    return(DF)
  }
 
  DF <-DF[DF$Date >= start_date, ]  # case where records start later than the desired start
  DF <- DF[DF$Date <= end_date, ]
  return(DF)
  
  
} #end function