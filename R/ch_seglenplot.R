#'
#'
#'
#'
#'

ch_lf_seglenplot <- function (lfobj, threslevel = 70, 
                              thresbreaks = c("fixed", "monthly", "seasonal"), 
                              thresbreakdays = NULL, 
                              rainpeaklevel = 0.95, 
                              na.rm = TRUE,
                              plot = TRUE) 
{
  lfcheck(lfobj)
  rain2day <- rainpeak(lfobj$flow, rainpeaklevel)
  thresbreaks <- match.arg(thresbreaks)
  
  if (thresbreaks != "seasonal") {
    thres <- buildthres(lfobj = lfobj, threslevel = threslevel, 
                        thresbreaks = thresbreaks, na.rm = na.rm)
  }
  
  else {
    if (is.null(thresbreakdays)) 
      stop("No thresbreakdays specified!")
    thres <- buildthres(lfobj = lfobj, threslevel = threslevel, 
                        thresbreaks = thresbreaks, breakdays = thresbreakdays, 
                        na.rm = na.rm)
  }
  
  check <- NULL
  temp <- merge(x = lfobj, y = thres, by = c("day", "month"), 
                sort = FALSE)
  temp <- temp[order(temp$year, temp$month, temp$day), ]
  
  startpoint <- which(1 == diff(lfobj$flow < temp$flow.y & 
                        !(c(FALSE, rain2day[-length(rain2day)]) & c(FALSE, (lfobj$flow > 
                            temp$flow.y)[-length(rain2day)])) & !(c(FALSE, FALSE, 
                            rain2day[-c(length(rain2day) - 1, length(rain2day))]) & 
                            c(FALSE, FALSE, (lfobj$flow > temp$flow.y)[-c(length(rain2day) - 
                                                                                                                                                                                1, length(rain2day))]))))
  segment <- rep(FALSE, length(lfobj$flow))
  segment[startpoint] <- TRUE

    dif <- diff(lfobj$flow)
  
    for (ii in 1:(length(segment) - 1)) {
    
      if (segment[ii]) 
      segment[ii + 1] <- (dif[ii] < 0)
  }

      run <- rle(segment)
  tab <- table(run$length[run$value])
  if(plot)
    {barchart(tab[!(names(tab) %in% c("1", "2", "3"))], 
                main = attr(lfobj, "lname"), 
                xlab = paste("Duration in days using Q", 
                  threslevel, " as threshold", sep = ""), horizontal = FALSE)
  }
}