rm(list=ls())
library(rgdal)
library(readr)
library(raster)
library(sp)
library(dplyr)
dpt<-read_csv("M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Leapfrog/MiHPT_Data_Processed_more2.csv")
setwd("C:/Users/tmoloney/Documents/work/AFP6/")
collar<-read_csv("M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Leapfrog/Exports/collar.csv")
dpt <- collar %>% filter(LOCID %in% unique(dpt$HoleID))
dpt_spdf <- SpatialPointsDataFrame(dpt[,2:3], 
                                     data= dpt,
                                     proj4string = CRS("+init=EPSG:26767")) # georgia state plane west NAD 27


bedrock<- raster("M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Leapfrog/Exports/Top of Rock contacts.grd")
dpt_spdf$bedrock<-extract(bedrock,dpt_spdf)
dpt$bedrock<-dpt_spdf$bedrock
dpt$depth_bedrock<-dpt$z_on_topography-dpt$bedrock
dpt<-dpt[,c(1:6,13,14)]
view(dpt)
write_csv(dpt,"dpt_bedrock.csv")


pwr<-raster("M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Leapfrog/Exports/PWR(Thickness).grd")
plot(pwr)

contour(pwr, levels=seq(5,120,5), add=TRUE)
cl<-rasterToContour(pwr, levels=seq(5,120,5))
writeOGR(cl, dsn = "M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Leapfrog/Exports", layer = "contour_pwr_thick", driver = "ESRI Shapefile", overwrite_layer=TRUE)
