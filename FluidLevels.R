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
lvls_df<-tibble(well=character(), start_date=date(), last_date=date(), last_meas=numeric(), last10mean=numeric())

wells<-unique(df$LOCID)
for (well in wells) {welldata<-GWD[GWD$LOCID==well,]
welldata<-arrange(welldata, LOGDATE)
last10<-welldata
if (nrow(last10)>10) {last10<-welldata[(nrow(welldata)-9):(nrow(welldata)),]
}
n<-as.numeric(last10$STATDEP)
last10mean<-mean(na.omit(n))
last_meas<-last10[nrow(last10),8]
last_date<-as.Date(last10[nrow(last10),3])
start_date<-as.Date(welldata[1,3])
well_results<-data_frame(well, start_date, last_date, last_meas, last10mean)
names(well_results)<-names(lvls_df)
lvls_df<-rbind(lvls_df,well_results)
errorCondition(next(well))
}

lvls_merge<-merge(df,lvls_df,by.x="LOCID",by.y='well')
lvls_merge<-lvls_merge %>% mutate(water_elev=MPELEV-last10mean)
lvls_merge<-lvls_merge[is.numeric(lvls_merge$water_elev),]

write_csv(lvls_merge,"fluid_levels.csv")

