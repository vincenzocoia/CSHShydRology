#'  convert a DF to a lfobject
#'  
#'  @description
#' Creates a lfobj from a DF obtained by ch_read_ECDE_flows or ch_tidy_hydat_ECDE.
#' "lfobj" is required by low flow functions from lfstat.  
#'
#' @details
#' A lfobj is created and  metadata attached as attributes.  
#' 
#' @author Paul Whitfield 
#' 
#' @param DF - dataframe of daily flow data as read by ch_read_ECDE_flows
#' @param metatdata - if NULL (default) uses HYDAT_list
#'
#' @return a lfobj with attached attributes. station, sname, lname, ylabel, hydrologicalyear, units 
#' @return if the object contains missing values a warning and a table of counts by year is printed.
#'   
#' @import lfstat 

#' 
#' @keywords low flow
#' @export 
#'
#' @examples 
#' data(W05AA008)
#' metadata <- data(HYDAT_list)
#' lf_W05AA008 <- ch_lfobj(mdf)
#'
#'



ch_lfobj  <- function (DF, hyear = 10)
{
  sID <- DF[1,1]
  station <- ch_get_wscstation(sID, metadata = NULL)
  DF$Flow <- DF$Flow
  
  options(warn=-1)
  result <- xts::xts( DF[ ,c("Flow")], order.by = DF$Date)

  result <- as.lfobj(result)
  hyear_start(result) <- hyear
  options(warn=0)
  flowunit(result) <-  "m^3/s"
  attr(result, "station") <- as.character(station$Station)
  attr(result, "river") <- as.character(station$StationName)
  attr(result, "lname") <- as.character(station[21])
  attr(result, "unit") <- "m^3/s"
  attr(result, "ylabel") <- expression(paste("Discharge (m" ^{3}, "/sec)"))
  
  nas <- lfnacheck(result)

  if (max(nas$hydrologicalyear[,2]) >= 1){
    
    print("Notice: data contains missing values")
    print(nas$hydrologicalyear)
    print("You may need to use a subset if there are missing years, or use 'fill_na'[vector] or 'lfnainterpolate' [lfobj].")
    }
  
    return(result)
}