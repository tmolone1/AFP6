## this file converts the coordinates in the file "survey_data_not_stoopid.csv" from NAD 83 Georgia State Plane West 
## to NAD 27 Georgia State Plane West
## and finds other locations not available in that data file in the gps survey file "AFP6 Boring Coordinates July 2020 GPS.csv"...
## converting those coordinates from NAD 83 Georgia State Plane West 2011 to NAD 27 Georgia State Plane West
## the coordinates are combined into a single file for output as a csv.
## I believe these are the correct transformations but as of 1/21/2021, the locations do not seem to be plotting correctly.  It is 
## possible that the the coordinates in the file "survey_data_not_stoopid.csv" are actually in NAD 83 Georgia State Plane West 2011
## in which case different transformations should be used in lines 39-44.  This warrants investigation, but the only method I have to resolve 
## it is trial and error in the absence of better info about the supplied coordinates.



library(tidyverse)
library(sp)
library(rgdal)
library(rgeos)
library(stringr)

#shp<-readOGR("C:/Users/User/OneDrive - Trihydro/Projects/Midnight Sun Trinity JV/AFP6/data/survey_points.shp")

surv_shp<-readOGR("survey_points.shp")
surv_shp<-tibble(cbind(surv_shp@data,surv_shp@coords))
surv_shp<-surv_shp[-c(1,12,14,47,59),]

surv <- read_csv("survey_data_not_stoopid.csv")
surv_shp$ID_Name<-surv$ID_Name
old_gps_surv <- read_csv("AFP6 Boring Coordinates July 2020 GPS.csv")

surv<-surv_shp
surv %>% filter(str_detect(surv$ID_Name,"M"))

didnt_get_these_i_guess <- setdiff(old_gps_surv$Name, surv$ID_Name)

surv_extended<- rbind(surv, surv %>% 
  filter(surv$ID_Name %in% gsub("A", "M", didnt_get_these_i_guess)) %>%
  mutate(ID_Name = gsub("M", "A",ID_Name)))

still_dont_know_about_these <- setdiff(old_gps_surv$Name, surv_extended$ID_Name)
gps_is_good_enough <- old_gps_surv %>% filter(Name %in% still_dont_know_about_these)


surv_clipped <- rbind(
surv_extended %>% filter(!(str_detect(ID_Name,"BR") | str_detect(ID_Name,"OB"))),
surv_extended %>% filter(str_detect(ID_Name,"BR") | str_detect(ID_Name,"OB"), str_detect(Reason, "Elev"))
)

# # surveyed points
# x <- surv_clipped$Easting
# y <- surv_clipped$Northing
# z <- surv_clipped$Elevation
# coords<-CRS("+init=ESPG:8729")  # georgia state plane west NAD83 ### NOTE: not 2011!
# xyz <- as.data.frame(cbind(x,y,z), colnames=c("x","y","z"))
# pts <- SpatialPointsDataFrame(xyz[,c("x","y")], 
#                        data= surv_clipped,
#                        proj4string = coords)
# pts.transform <- spTransform(pts, CRS("+init=EPSG:26767") )  # change projection to NAD27 / Georgia West 
# #pts.transform@coords
# #pts.transform@data
# table_surv<-tibble(cbind(pts.transform@data, pts.transform@coords))
table_surv<-surv_clipped %>% dplyr::select(ID_Name, Easting, Northing, Elevation, X, Y)

# gpsd points
x <- gps_is_good_enough$Easting
y <- gps_is_good_enough$Northing
z <- gps_is_good_enough$`Elevation (ft amsl)`
coords<-CRS("+init=EPSG:6447")  # georgia state plane west NAD83  2011
xyz <- as.data.frame(cbind(x,y,z), colnames=c("x","y","z"))
pts <- SpatialPointsDataFrame(xyz[,c("x","y")], 
                              data= gps_is_good_enough,
                              proj4string = coords)
pts.transform <- spTransform(pts, CRS("+init=EPSG:26767"))  # change projection to NAD27 / Georgia West 
#pts.transform@coords
#pts.transform@data
table_gps<-tibble(cbind(pts.transform@data, pts.transform@coords))
table_gps<-table_gps %>% dplyr::select(Name, Easting, Northing, `Elevation (ft amsl)`, x, y) %>% mutate(`Elevation (ft amsl)` = rep(NA,nrow(table_gps)))
names(table_gps)<-names(table_surv)

nrow(table_surv)


revised_survey_table<-rbind(table_surv,table_gps)


write_csv(revised_survey_table,"revised_survey_data.csv")
#write_csv(tbl,"shp.csv")

revised_survey_table %>% filter(ID_Name %in% c("DPT09A","DPT09M"))
tbl %>% filter(ID_Name %in% c("DPT9A","DPT9M"))

