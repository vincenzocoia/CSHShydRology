#' ch_get_mapper_data
#'
#' @description  Prepares for mapping by acquiring the base map and ancillary data: boundaries and rivers
#'
#'
#'
#'



ch_get_mapper_data <- function (maplat, maplong, map_proj = NA, map_directory = "C:/map_data"){
  
  uleft <- c(maplat[2],maplong[2])
  lright <- c(maplat[1],maplong[1])
  
  cdn_latlong = "+proj=longlat"
  cdn_aea = "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

 
  if(is.na(map_proj)) map_proj <- cdn_latlong 
  if(tolower(map_proj) == "albers") map_proj <- cdn_aea
  if(tolower(map_proj) == "equalarea") map_proj <- cdn_aea
  
  # get basic map
   map_a <- openmap(uleft, lright,
             type="nps",
             minNumTiles=7)
  
   # change projection
   map_d <-openproj(map_a, projection= map_proj)
   

##################################################################  
# if map directory exists load rivers and boundaries

     if(dir.exists(map_directory)){
    
    setwd(map_directory)

    plines10 <- ne_load(scale = 10, type = "states", category = 'cultural', destdir = getwd(), returnclass="sf")
    rivers10 <- ne_load(scale = 10, type = "rivers_lake_centerlines", category = 'physical', destdir = getwd(), returnclass="sf")
    
    
  }
    
#####################################################################
# if map_directory does not exist create it and download data
  
  if(!dir.exists(map_directory)){
    
    print(paste("Creating a new directory for map data",map_directory))
    
    dir.create(map_directory)
    setwd(map_directory)
    
    plines10 <- ne_download(scale = 10, type = "states", category = 'cultural', destdir = getwd(), returnclass="sf")
    rivers10 <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = 'physical',destdir = getwd(), returnclass="sf")
    
    
    }
    

    map_data <- list(map_d, plines10, rivers10, map_proj, maplat, maplong)
    names(map_data) <- c("map_d", "plines10", "rivers10", "map_proj", "latitude","longitude")
    return(map_data)
  
  
  
}



