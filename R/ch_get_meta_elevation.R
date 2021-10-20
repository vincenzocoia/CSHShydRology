#'  ch_get_meta_elevation
#'
#' an issue here with needing to have a username
#'GTOPO30 is a global digital elevation model (DEM) with a horizontal grid spacing of 30 arc seconds (approximately 1 kilometer). 
#'GTOPO30 was derived from several raster and vector sources of topographic information. Documentation : USGS GNtopo30
#'
#' @description 
#' Get elevation and add to metadata dataframe.  Water Survey of Canada does not currently provide station elevation 
#' in the metadata.  This function uses a function from geonames to extract elevations from a the USGS Gtopo30
#'
#' @param df - a dataframe with WCS metadata
#' @return df - a dataframe with WCS metadata with elevation from Gtopo30
#'
#' @import GNtopo30
#' @export
#' 
#' @author Paul Whitfield
#' 
#' @example
#' metadata <- data(HYDAT_list)
#' meta0 <- metadata[1:10,]
#' str(meta0)
#' meta1 <- ch_get_meta_elevation(meta0)
#' str(meta1)
#' 
#' 
#'

require(geonames)
options(geonamesUsername="paul_whitfield")

ch_get_meta_elevation <- function (df){
  
  elevation <- array(NA, dim=length(df[,1]))
  
  
  for (k in 1:length(df$Latitude)){
    
    elout <- GNgtopo30(lat=df$Latitude[k],lng=df$Longitude[k])
    elevation[k] <-as.numeric(elout[2])
  }
  df <- data.frame(df,elevation)
  return(df)
}