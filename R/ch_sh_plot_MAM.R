#'  ch_sh_plot_MAM
#'  
#'  
#'  
#'  
#'  


ch_sh_plot_MAM <- function (mam,  n = 7, station = station, metadata = HYDAT_list ) {
  
  sname <- ch_get_wscstation(station, metadata = metadata)
  title <- sname$Station_lname
  
  ylabel = paste("Annual ",n, " day minimum daily discharge  (m3/sec)")
  
  plot(mam[,1], mam[,2], typ="o", lty=3, lwd=0.25,
       xlab= "", ylab = ylabel, las=1,
       ylim = c(0,max(mam[,2], na.rm = TRUE)),
       main = title,
       pch = 19, col = "darkgreen")
  
  auto <- acf(mam[,2], max.lag=1, na.action = na.pass, plot=FALSE)
  crit05 <- 1.96/sqrt((length(mam[,2])-1))
  crit01 <- 2.576/sqrt((length(mam[,2])-1))
  if (auto$acf[2] < crit05) text(min(mam[,1]), 0., paste("lag_1 correlation",round(auto$acf[2],digits =3),"NS"), pos = 4, cex=0.8)
  if (auto$acf[2] >= crit05 && auto$acf[2] <= crit01)text(min(mam[,1]), 0., paste("lag_1 correlation",round(auto$acf[2],digits =3),"*"), pos = 4, cex=0.8)
  if (auto$acf[2] >= crit01)text(min(mam[,1]), 0., paste("lag_1 correlation",round(auto$acf[2],digits =3),"**"), pos = 4, cex=0.8)
  
  mk <- rkt::rkt(mam[ ,1], mam[ , 2])

  fit <- lm(mam ~ Year, data <- mam7)
  
  text(max(mam[, 1]), max(mam[,2])/20, paste("MK tau",round(mk$tau,digits =3),"p_value",round(mk$sl, digits=3),
                               "Sen's slope", round(mk$B, digits = 5)), pos = 2, cex=0.8)
  
  if(mk$sl <= 0.05) abline(a=fit$coefficients, lty=2, col="darkgreen")
  
  he <- pracma::hurstexp(mam[ , 2], display=FALSE)
  text(min(mam[ , 1]),max(mam[ , 2]), paste("Hurst exponent",round(he$Hs,digits =3)), pos = 4, cex = 0.8)
  
  ################# second panel 
  
  plot(mam[,1], mam[,5], typ="o", lty=4, lwd=0.25,
       xlab= "", ylab = "Day of Water Year", las=1,
       ylim = c(0,366),
       main = title,
       pch = 18, col = "darkblue")
  
  mkt <- rkt::rkt(mam[ ,1], mam[ , 5])
  fitt <- lm(dwy ~ Year, data <- mam7)
  
  text(min(mam[, 1]),0., paste("MK tau",round(mkt$tau,digits =3),"p_value",round(mkt$sl, digits=3),
                                             "Sen's slope", round(mkt$B, digits = 5)), pos = 4, cex=0.8)
  if(mkt$sl <= 0.05) abline(a=fitt$coefficients, lty=2, col="darkblue")
  result <- list (station, n, mam, auto, mk, he, mkt)
  names(result) <- c("station", "days", "mam", "acf", "MK_mam", "He", "MK_dowy")
  return(result)
}