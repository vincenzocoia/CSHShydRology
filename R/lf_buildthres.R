#'  lf_buildthres
#'
#' @description Originally a hidden function in lfstat.  This version copies source code into the CSHShydRology package
#' so functions parallel to the original lfstat::functions use the same code.
#' 
#' lfstat::buildthres is not exported from lfstat. 
#'
#'


#BUILDING threshold
buildthres <- function(lfobj,
                       threslevel = 70,
                       thresbreaks = c("fixed", "monthly", "daily", "seasonal"),
                       breakdays = c("01/06", "01/10"),
                       na.rm = TRUE){
  thresbreaks <- match.arg(thresbreaks)
  threshold <- data.frame(day = rep(1:31, 12), month = sort(rep(1:12, 31)))
  
  if(thresbreaks == "fixed")
  {threshold$flow <- quantile(lfobj$flow, 1-threslevel/100, na.rm = na.rm)}
  if(thresbreaks == "monthly"){
    mon <- aggregate(flow~month, lfobj, quantile, probs = 1-threslevel/100)
    threshold <- merge(threshold, mon, sort = FALSE)
  }
  if(thresbreaks == "daily"){
    mon <- aggregate(flow~day + month, lfobj, quantile, probs = 1-threslevel/100)
    threshold <- merge(threshold, mon, sort = FALSE)
  }
  if(thresbreaks == "seasonal"){
    ii <- subset(lfobj, year != hyear, month)
    if(nrow(ii)==0){hyearstart <-1} else if(max(ii) <5.5){hyearstart <- max(ii)+1}else{hyearstart <- min(ii)}
    if(length(breakdays) == 1){
      breakdays <- c(paste("1/", hyearstart, sep = ""), breakdays)
    }
    bdays <- data.frame(matrix(ncol = 2))
    names(bdays) <- c("day", "month")
    str <- strsplit(breakdays, "/")
    for(ii in seq_along(str)){
      bdays[ii, ] <- as.numeric(c(str[[ii]]))
    }
    threshold$breakp <- FALSE
    for(ii in seq_along(bdays$day)){
      threshold$breakp[threshold$day == bdays[ii, "day"] & threshold$month == bdays[ii, "month"]]<-TRUE
    }
    threshold$season = cumsum(threshold$breakp)
    threshold$season[threshold$season == 0] <- max(threshold$season)
    threshold$breakp <- NULL
    something <- merge(lfobj, threshold, by = c("day", "month"), sort = FALSE)
    sea <- aggregate(flow ~ season, something, quantile, probs = 1-threslevel/100)
    threshold <- merge(threshold, sea, by = "season", sort = FALSE)
    threshold$season <- NULL
  }
  threshold}
