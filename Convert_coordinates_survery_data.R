library(tidyverse)
library(sp)
library(rgdal)
library(sf)
library(rgeos)
surv <- read_csv("survey_data.csv")
surv <- surv[!is.na(surv$Easting),]
x <- surv$Easting
y <- surv$Northing
z <- surv$Elevation
coords -CRS("+init=EPSG:6447")  # georgia state plane west NAD83 2011
xyz <- as.data.frame(cbind(x,y,z), colnames=c("x","y","z"))
pts <- SpatialPointsDataFrame(xyz[,c("x","y")], 
                       data= surv,
                       proj4string = coords)
pts.transform <- spTransform(pts, CRS("+init=EPSG:26767"))  # change projection to NAD27 / Georgia West 
pts.transform@coords
pts.transform@data
revised_survey_table<-tibble(cbind(pts.transform@data, pts.transform@coords))
write_csv(revised_survey_table,"revised_survey_data.csv")



