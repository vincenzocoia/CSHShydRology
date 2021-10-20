#' ch_lf_xts_obj
#'
#'
#'

ch_lf_xts_obj  <- function (DF)
{
  sID <- DF[1,1]
  station <- ch_get_wscstation(sID, metadata = NULL)
  DF$flow <- DF$Flow
  
  result <- xts( DF[ ,c("Flow")], order.by = DF$Date)
  attr(result, "station") <-as.character(station$Station)
  attr(result, "sname") <- as.character(station$StationName)
  attr(result, "lname") <- as.character(station[21])
  attr(result, "unit") <- "m^3/s"
  attr(result, "ytext") <- expression(paste("Discharge m" ^{3}, "/sec"))
  
  
  return(result)
}