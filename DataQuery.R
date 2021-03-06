rm(list=ls())
library(RODBC)
library(tidyverse)
library(sp)
library(rgdal)
library(rgeos)
mdbConnect<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb)};DBQ=M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Analytical Data/AFP06.mdb")
mdbConnect<-odbcConnectAccess("M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Analytical Data/AFP06.mdb")
LDI<-sqlFetch(mdbConnect, "LDI")
WCI<-sqlFetch(mdbConnect, "WCI")
WINT<-sqlFetch(mdbConnect, "WINT")
WMI<-sqlFetch(mdbConnect, "WMI")
add<-read_csv("AFP6 Boring Coordinates July 2020 GPS.csv")
spdf_add<- SpatialPointsDataFrame(add[,c(3,4)],
                                  data= add,
                                  proj4string = CRS("+init=EPSG:6447")) # georgia state plane west NAD83 2011
spdf.transform <- spTransform(spdf_add, CRS("+init=EPSG:26767"))  # change projection to NAD27 / Georgia West 
converted<-as_tibble(cbind(spdf.transform$Name,spdf.transform@coords))
colnames(converted)<-c("LOCID","ECOORD","NCOORD")
data<-read_excel("TBL3MasterWellList.xlsx")

# get screen intervals from WINT table and measuring point elevations from WCI table, merge together into a single table
scrn_ints<-WINT[WINT$CLASS=="SCRN",c(2,8,9)]
mpelev<-WCI[,c("LOCID","INSDATE","MPELEV")]
resurvey<-WMI[WMI$MTYPE=="RSVEY",c("LOCID","LOGDATE","MPELEV")]   # replace the WCI mpelevs with resurveys from WMI table
mps<-merge(mpelev[mpelev$LOCID %in% setdiff(mpelev$LOCID,unique(resurvey$LOCID)),],resurvey, by= c(1,2,3), all.x=TRUE, all.y=TRUE)
mps<-mps[,c(1,3)]
rm(mpelev, resurvey)
foo<-merge(mps, scrn_ints, by="LOCID", all=TRUE)
foo1<-read_csv("foo1.csv") # intervals for new wells
foo<-rbind(foo,foo1)

locs_complete<-readOGR("locs_complete.shp")
coords<-tibble(locs_complete$LOCID,locs_complete@coords[,1],locs_complete@coords[,2])
coords<-coords[coords[,2]>0,]
colnames(coords)<-c("LOCID","ECOORD","NCOORD")
coords<-rbind(coords,converted)
coords$ECOORD<-as.numeric(coords$ECOORD)
coords$NCOORD<-as.numeric(coords$NCOORD)

nad83<-coords[coords[,2]>500000,]
nad27<-coords[coords[,2]<500000,]
grnd<-LDI[,c("LOCID","ELEV")]
bind<-add[,c(2,5)]
names(bind)<-names(grnd)
grnd<-rbind(grnd,bind)

# separate out locations with coordinates in NAD 83 and convert to NAD27
#####nad83<-LDI[LDI$DATUM=="NAD83_STP" & !is.na(LDI$NCOORD),]
spdf<- SpatialPointsDataFrame(nad83[,c(2,3)],
                              data= nad83,
                              proj4string = CRS("+init=EPSG:2240")) # georgia state plane west NAD83
spdf.transform <- spTransform(spdf, CRS("+init=EPSG:26767"))  # change projection to NAD27 / Georgia West 
converted<-as_tibble(cbind(spdf.transform$LOCID,spdf.transform@coords))
colnames(converted)<-c("LOCID","ECOORD","NCOORD")

# merge converted NAD27 coordinates back with locations from the LDI file that were already in NAD27
#nad27<-LDI[LDI$DATUM=="NAD27_STP" & !is.na(LDI$NCOORD),]
#nad27<-nad27[,c("LOCID","ECOORD","NCOORD","ELEV")]
colnames(nad27)<-colnames(converted)
tbl<-merge(converted,nad27,by=c(1,2,3),all=TRUE)  
tbl[,2]<-as.numeric(unlist(tbl[,2]))
tbl[,3]<-as.numeric(unlist(tbl[,3]))
tbl<-merge(tbl,grnd, by="LOCID", all.x=TRUE) # all locations, some have no ground elev, mp elev, or intervals
tbl3<-data %>% 
  select(c(`Well Identification`,Easting,Northing,10))
tbl<-tbl %>% filter(LOCID %in% data$`Well Identification`)
tbl<-merge(tbl,tbl3,by=1:4,all.y=TRUE)
tbl<-tbl %>% filter(!is.na(ECOORD)) %>%
  mutate(ECOORD=as.numeric(ECOORD), NCOORD=as.numeric(NCOORD))
scrns<-merge(tbl,foo, by = "LOCID", all=TRUE)  # all locations found in interval tables WINT and WCI
#scrns<-scrns[!is.na(scrns$IBDEPTH),] # removed locations without an interval
scrns<-scrns[!is.na(scrns$ECOORD),] 
scrns$maxdepth<-scrns$IEDEPTH
scrns$zero<-0
scrns[is.na(scrns$ELEV),]$ELEV<-scrns[is.na(scrns$ELEV),]$MPELEV #if no ground info, assume the MPelev is the ground
scrns[which(duplicated(scrns[,2:3])),]$ECOORD<-scrns[which(duplicated(scrns[,2:3])),]$ECOORD+.1
scrns[which(duplicated(scrns[,2:3])),]$NCOORD<-scrns[which(duplicated(scrns[,2:3])),]$NCOORD-.1

# create shapefile
spdf<- SpatialPointsDataFrame(tbl[,c("ECOORD","NCOORD")],
                              data= tbl,
                              proj4string = CRS("+init=EPSG:26767")) # georgia state plane west NAD27
bound<-readOGR("Polyline.shp")
scrns_df<- SpatialPointsDataFrame(scrns[,c("ECOORD","NCOORD")],
                                  data= scrns,
                                  proj4string = CRS("+init=EPSG:26767")) # georgia state plane west NAD27

# identify locations with 20 feet of each other, generally wells screened at different levels, and force the code to adjust coordinates until they plot right)
d <- gDistance(scrns_df, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
newdata <- cbind(scrns_df, scrns_df[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))
colnames(newdata@data)[ncol(newdata)]<-"distance"

pairs<-newdata@data[,c("LOCID","LOCID.1","distance")]
choose<-pairs[pairs$distance<0.5,c(1,2)]
picks<-vector(mode="list", length=nrow(choose))
num<-as_tibble(seq(1:4))
num$list <- list(c(1,2),c(2,2),c(1,3),c(2,3))
k<-min(newdata$distance)
while (k<0.5) {
  for (i in seq(1,nrow(choose))) {
    pick<-scrns[scrns_df$LOCID %in% choose[i,],]
    j<-sample(1:4,1)
    pick[num$list[[j]][1],num$list[[j]][2]]<-pick[num$list[[j]][1],num$list[[j]][2]]+0.5
    scrns[scrns_df$LOCID %in% choose[i,],]<-pick
  }
  scrns_df<- SpatialPointsDataFrame(scrns[,c("ECOORD","NCOORD")],
                                    data= scrns,
                                    proj4string = CRS("+init=EPSG:26767")) # georgia state plane west NAD27
  d <- gDistance(scrns_df, byid=T)
  min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
  newdata <- cbind(scrns_df, scrns_df[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))
  colnames(newdata@data)[ncol(newdata)]<-"distance"
  k<-min(newdata$distance)
}
scrns_df$LOCID[which(duplicated(scrns_df$LOCID))]<-paste(scrns_df$LOCID[which(duplicated(scrns_df$LOCID))],"A")

# setup a grid for interpolations
grd <- as.data.frame(spsample(scrns_df[scrns_df$ECOORD>389252.9 & scrns_df$ECOORD<392685 & scrns_df$NCOORD>1429204.3 & scrns_df$NCOORD<1432304,], "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add scrns's projection information to the empty grid
proj4string(grd) <- proj4string(scrns_df)
plot(grd)
points(scrns_df)
df<-as_tibble(scrns_df[!is.na(over(scrns_df, grd)),])

isco<-read_delim("ISCO.txt", delim=" ")
isco<-merge(isco, data %>% filter(`Well Identification` %in% unique(isco$WellID)) %>% select(c(`Well Identification`,Northing,Easting)), by=1)
isco<-isco %>% mutate(screentop=str_split(isco$ScreenedInterval,"-",simplify=TRUE)[,1], screenbot=str_split(isco$ScreenedInterval,"-",simplify=TRUE)[,2])

# locs_complete<-readOGR("locs_complete.shp")
# coords<-tibble(locs_complete$LOCID,locs_complete@coords[,1],locs_complete@coords[,2])
# coords<-coords[coords[,2]>0,]
# nad83<-coords[coords[,2]>500000,]
# nad27<-coords[coords[,2]<500000,]
# View(coords)
# spdf1<- SpatialPointsDataFrame(coords[,c(2,3)],
#                                   data=coords,
#                                   proj4string = CRS("+init=EPSG:26767")) # georgia state plane west NAD27
# plot(spdf1)

# write output
write_csv(tbl, "all_locations.csv")
write_csv(scrns, "wellscreens.csv")
write_csv(df, "collar.csv")
save(scrns, file = 'scrns.Rda')
save(df, file='scrns_select.Rda')
writeOGR(spdf, dsn = getwd(), layer = "wellscreens", driver = "ESRI Shapefile", overwrite_layer=TRUE)
write_csv(isco, "isco.csv")

#### Get Lithology Table

LTD<-sqlFetch(mdbConnect, "LTD")
lith<-merge(LTD[,c(2:4,6,9,10,12,14)],scrns_df, by="LOCID")
lith<-lith[,1:8]
t<-read_csv("lithcodes.csv")
i=1
lith$lithcode<-NA
for (i in 1:nrow(t)) {
  lith$lithcode[lith$ASTMCODE==t$`unique(lith$ASTMCODE)`[i]]<-t$lithcode[i]
}
write_csv(lith, "lithology.csv")
