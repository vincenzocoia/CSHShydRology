#'
#'
#'
#'

# required libraries
require(OpenStreetMap)
require(sf)
require(rgdal)

library(magrittr)
library(tidyr)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

##########################################################
# define output files

output_file <- "test.csv"
plot_file <- "plot v1a.pdf"

data_directory <- "C:/p_ne_10"

##########################################################
# set latitude and longitude box
# Canada   (41.6, 83.1) ( -51.50, -141.0 )
# BC       (49.0, 60.0) (-114.05, -139.1 )
# AB       (49.0, 60.0) (-110.00, -139.1 )
# SK       (49.0, 60.0) (-101.35, -110.0 )
# MB       (49.0, 60.0) (-101.35, -110.0 )
latitude <- c(48.0, 61.0)
longitude <- c(-109.5, -120.5)


#Canada
latitude <- c(41.6,  83.1)
longitude <- c(-51.5, -141.2)

#east BC + Alberta
latitude <- c(48.0,  61.0)
longitude <- c(-109.8, -128.5)


#MB to central BC ~CCRN
latitude <- c(48.0,  61.0)
longitude <- c(-95.2, -128.5)


#########################################################
# get map data

# m_map <- ch_get_mapper_data(latitude,longitude, map_proj = "Albers", map_directory = "C:/map_data")


 m_map <- ch_get_mapper_data(latitude,longitude, map_proj = "Albers", map_directory = "C:/p_ne_10")
  

 
###################  get site locations from a file

setwd("C:/p_Rocky Mountain low flow")
m_data <-read.csv("rm_metadata.csv")
mlong <- m_data$Longitude
mlat <- m_data$Latitude
mcode <- m_data$RHBN
mcode[!is.na(mcode)] <- 2
mcode[is.na(mcode)] <- 1
mcode <- as.integer(mcode)


ncode <- rep(1,length(mlong))
location_a <- data.frame(mlong, mlat)
location_b <- data.frame(mlong, mlat,ncode)
location_d <- data.frame(mlong,mlat,mcode)


mo_col <- c("black","darkgreen")

mess <- ch_mapper(m_map, locations = location_a, lo_pch = 17, lo_col = "red")
mess <- ch_mapper(m_map, locations = location_a, lo_pch = 17, lo_col = "red", legend = TRUE)

mess <- ch_mapper(m_map, locations = location_b, lo_pch = c(18,15), lo_col = mo_col)

mess <- ch_mapper(m_map, locations = location_d, lo_pch = c(19,15), lo_col = mo_col, 
                  lo_text = c("natural","RHBN"), pl_cex=1.1,legend = TRUE)
      
######################################  create a trend file

t_data <- read.csv("result v2.csv")

tr_data <- merge(m_data, t_data, by.x="Station", by.y = "station")

trend_d <- data.frame(tr_data$Longitude, tr_data$Latitude, tr_data$Sen_slope, tr_data$MK_p_value)

mess <- ch_mapper(m_map, trends = trend_d)
mess <- ch_mapper(m_map, trends = trend_d, legend = TRUE)

##########################################  create a variable plot

v_data <- read.csv("c:/p_Rocky Mountain low flow/result v3 E_1a.csv")

vr_data <- merge(m_data, v_data, by.x="Station", by.y = "station")

trend_d <- data.frame(tr_data$Longitude, tr_data$Latitude, tr_data$Sen_slope, tr_data$MK_p_value)

trend_v <- data.frame(vr_data$Longitude, vr_data$Latitude, vr_data$Hs)

mess <- ch_mapper(m_map, variables = trend_v, legend = TRUE, vr_text = "Hurst exponent (Hs)")

##################################################
setwd("C:/p_ch_functions")
xlabels <- read.csv("test_labels.csv")

mess <- ch_mapper(m_map, variables = trend_v, legend = TRUE, vr_text = "Hurst exponent (Hs)", x_labels = xlabels)


#####################################################


setwd("C:/p_ccrn/basin shapefiles/Mackenzie River Basin")
mbasin <-st_read("Mackenzie River Basin.shp")
setwd("C:/p_ccrn/basin shapefiles/Nelson Churchill Basin")
nbasin <-st_read("Nelson_outline.shp")

basins <- list(mbasin, nbasin)


mess <- ch_mapper(m_map, locations = location_a, lo_pch = 17, lo_col = "red", basins = basins)

