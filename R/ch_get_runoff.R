#' ch_get_runoff
#'  Q (mm) = Q(m^3/s) * 1000 * 24 * 3600 / Area (m^2)
#' @param case what time unit for calculation.  Default is annual using mean annual flow. Other options day (single day),
#' month (average for 30.44 days * 12 = 365.25), and any month of the year using the number of days in that month and 28.25 for February.
#' @param maf in m3/sec
#' @param area in km2
#' 
#' @return runoff in mm for the specific time period
#'
#' @author Paul Whitfield
#' 
#' @example 
#'  annual_runoff <- ch_get_runoff("year", 4.673, 403 )
#' # annual runoff = 365.9772 mm
#' 
#' 
ch_get_runoff <- function (case = "year", maf_m3s, area_km2){
  case <- substr(case, 1,3)
  case <- tolower(case)

  if (case == "yea")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 365.25 / (area_km2 * 1000 * 1000)
  if (case == "mon")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 30.4375 / (area_km2 * 1000 * 1000)
  if (case == "day") runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 1 / (area_km2 * 1000 * 1000)
  
  if (case == "jan")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 31 / (area_km2 * 1000 * 1000)
  if (case == "feb")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 28.25 / (area_km2 * 1000 * 1000)
  if (case == "mar")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 31 / (area_km2 * 1000 * 1000)
  if (case == "apr")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 30 / (area_km2 * 1000 * 1000)
  if (case == "may")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 31 / (area_km2 * 1000 * 1000)
  if (case == "jun")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 30 / (area_km2 * 1000 * 1000)
  if (case == "jul")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 31 / (area_km2 * 1000 * 1000)
  if (case == "aug")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 31 / (area_km2 * 1000 * 1000)
  if (case == "sep")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 30 / (area_km2 * 1000 * 1000)
  if (case == "oct")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 31 / (area_km2 * 1000 * 1000)
  if (case == "nov")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 30 / (area_km2 * 1000 * 1000)
  if (case == "dec")  runoff_mm <- maf_m3s * 1000 * 60 * 60 * 24 * 31 / (area_km2 * 1000 * 1000)
  
    return(runoff_mm)
}

