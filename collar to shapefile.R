library(tidyverse)
library(rgdal)
library(sp)

mycsv<-read_csv("M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Leapfrog/Exports/collar_new.csv")
coords<-CRS("+init=EPSG:26767")


ok<-mycsv %>% filter(!(LOCID %in% revised_survey_table$ID_Name)) 
change<-mycsv %>% filter(LOCID %in% revised_survey_table$ID_Name) 
myjoin<-merge(change,revised_survey_table,by=1, all.y=TRUE)
myjoin<-myjoin %>% mutate(ECOORD=x,NCOORD=y, ELEV=Elevation,ECOORD.1=x,NCOORD.1=y) %>% dplyr::select(1:12)
mycsv<-tibble(rbind(ok,myjoin))
nrow(ok)
nrow(myjoin)
nrow(mycsv)
write_csv(mycsv, "M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Leapfrog/Exports/collar_new_written_from_R.csv")
pts <- SpatialPointsDataFrame(mycsv[,c(2,3)], 
                              data= mycsv,
                              proj4string = coords)
writeOGR(pts, dsn = getwd(), layer = "wells", driver = "ESRI Shapefile", overwrite_layer=TRUE)
write_csv(mycsv[399:496,], "M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Leapfrog/Exports/dpt_only.csv")


gis_locs<-readOGR("M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Shapefiles/locs_gis_export2.shp")
plot(gis_locs)
gis_locs@data
tbl<-tibble(cbind(gis_locs@data, gis_locs@coords))
tbl<- tbl %>% filter(coords.x1 < 4e5, coords.x1 > 3e5)
View(tbl)
tbl_spdf <- SpatialPointsDataFrame(tbl[,54:55], 
                                   data= tbl,
                                   proj4string = CRS("+init=EPSG:26767")) # georgia state plane west NAD 27
plot(tbl_spdf)

mycsv<-mycsv[399:496,]
tbl<-tbl %>% filter(!(STATIONID %in% mycsv$LOCID)) %>% mutate(
  LOCID = STATIONID, 
  ECOORD = coords.x1, 
  NCOORD = coords.x2, 
  ELEV=GS_Elev, 
  z_on_topography= GS_Elev, 
  maxdepth=Hist_Well_,
  MPELEV= MP_Elev, 
  IBDEPTH=Top_Scr,
  IEDEPTH=Bot_Scr,
  zero=0, 
  ECOORD.1 = coords.x1,
  NCOORD.1 = coords.x2) %>% 
  dplyr::select(56:67)
new_collar<-tibble(rbind(tbl,mycsv))

pts <- SpatialPointsDataFrame(new_collar[,c(2,3)], 
                              data= new_collar,
                              proj4string = coords)
writeOGR(pts, dsn = getwd(), layer = "wells_new", driver = "ESRI Shapefile", overwrite_layer=TRUE)
write_csv(new_collar, "M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Leapfrog/Exports/collar_new_written_from_R.csv")
writeOGR(pts, dsn="M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Shapefiles", layer = "wells", driver = "ESRI Shapefile", overwrite_layer = TRUE)

prb<-readOGR("M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Leapfrog/Exports/PRB_wall.shp")
plot(prb)
points(tbl_spdf,pch=1)
points(pts,pch=2)
points(locs_spdf,pch=3)

locs<-locs_spdf@data
locs<-locs %>% filter(!(STATIONID %in% mycsv$LOCID)) %>% mutate(
  LOCID = STATIONID, 
  ECOORD = coords.x1, 
  NCOORD = coords.x2, 
  ELEV=GS_Elev, 
  z_on_topography= GS_Elev, 
  maxdepth=Hist_Well_,
  MPELEV= MP_Elev, 
  IBDEPTH=Top_Scr,
  IEDEPTH=Bot_Scr,
  zero=0, 
  ECOORD.1 = coords.x1,
  NCOORD.1 = coords.x2) %>% 
  dplyr::select(56:67)
new_collar<-tibble(rbind(locs,mycsv))
