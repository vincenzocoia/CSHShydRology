#'     check Df for full years
#'     
#' @description  lfstat often uses partial years to get annual minimum without a way to remove them.  
#' This function provides a method to use to remove partial years.
#'   
#' @param DF dataframe of daily streamflow data
#' @param hyear  month in which the hydrological year starts  default is hyear = 10
#' @param threshold number of days that are consider enought to be a whole year, default is 365
#'      
#' @result  a dataframe with years and counts, counts less than the threshold are NA.  This can then be merged with a 
#' mam series by linking years, and retaining only complete cases as in the example
#'
#' @import from lfstat MAM
#'
#' @example
#' data(W05AA008)
#' missing <- ch_lf_check(W05AA008, hyear = 10, threshold = 365)
#'    
#' lf_W05AA008 <- ch_lf_obj(W05AA008)
#' mam7 <- MAM(lf_file, n=7, year = "any" , yearly = TR   
#'    
#'  new_df <- merge(mam7, missing, by.x = "hyear", by.y = "year")   
#'  new_df <- new_df[complete.cases(new_df),]  
#'  #### any years without 365 days of data will have been removed  


ch_lf_check  <- function (DF, hyear=10, threshold = 365)
{
  first_day <- DF$Date[1]
  last_day <- DF$Date[length(DF[,1])]
  
  years <- c(format(first_day, "%Y"):format(last_day, "%Y"))
  result <- array(NA, dim=c (length(years)-1, 2))
  
  
  for (k in 2: length(years)){
      df <- DF[DF$Date >= as.Date(ISOdate(years[k-1],hyear,1)),]
      df <- df[df$Date <= as.Date(ISOdate(years[k],hyear,1)),]
      result[k-1,1] <- years[k-1]
      result[k-1,2] <- length(df[,1]) -1
  }
 
  result[result < threshold] <- NA
  colnames(result) <- c("year", "count")
  return (result)
  
}