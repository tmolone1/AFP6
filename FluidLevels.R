rm(list=ls())
library(RODBC)
library(tidyverse)
library(sp)
library(rgdal)
library(rgeos)
mdbConnect<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb)};DBQ=M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Analytical Data/AFP06.mdb")
mdbConnect<-odbcConnectAccess("M:/ItoN/MidnightSunTrinityJV/Modeling/AFP6/Analytical Data/AFP06.mdb")
load("scrns_select.Rda")
GWD<-sqlFetch(mdbConnect, "GWD")

wells<-unique(df$LOCID)
for (well in wells) {welldata<-GWD[GWD$LOCID==well,]
welldata<-arrange(welldata, LOGDATE)
last10<-welldata
if (nrow(last10)>10) {last10<-welldata[(nrow(welldata)-9):(nrow(welldata)),]
}
n<-as.numeric(last10$STATDEP)
ndfreq<-sum(is.na(n))/length(n)
last_meas<-last10[nrow(last10),8]
last_date<-last10[nrow(last10),3]




well_results<-data.frame(well, pers, ndfreq, median_thick,last_thick,last_date)
names(well_results)<-names(pers_df)
pers_df<-rbind(pers_df,well_results)
errorCondition(next(well))
}
pers_df<-as_tibble(pers_df)
pers_df$last_thick<-as.numeric(pers_df$last_thick)
pers_df$last_thick[is.na(pers_df$last_thick)]<-0
save(pers_df,file="LNAPL_pers_thick.Rda")
