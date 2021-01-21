library(tidyverse)
library(rgdal)
library(sp)

mycsv<-read_csv("M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Leapfrog/Exports/collar_new.csv")
coords<-CRS("+init=EPSG:26767")
pts <- SpatialPointsDataFrame(mycsv[,c(2,3)], 
                              data= mycsv,
                              proj4string = coords)
writeOGR(pts, dsn = getwd(), layer = "wells", driver = "ESRI Shapefile", overwrite_layer=TRUE)
