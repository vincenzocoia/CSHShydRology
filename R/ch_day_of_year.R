#'  Get the day of year of a date
#'
#'
#' @param date date as character "YYYY-MM-DD"  or as a date
#' @return integer day of year with 1 January being day 1
#'
#'
#' @author Paul Whitfield
#' @example
#' ch_day_of_year("2001-10-01")
#' # returns 274


ch_day_of_year <- function(date) {
  if(is.character(date)) date = as.Date(date, format ="%Y-%m-%d")
  date <- timeDate::as.timeDate(date)
  result <- as.numeric(timeDate::dayOfYear(date))
  return(result)
}
  