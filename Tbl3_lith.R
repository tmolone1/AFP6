rm(list=ls())
library(readxl)
library(tidyverse)
library(rgdal)
data<-read_excel("TBL3MasterWellList.xlsx")
unique(data$`Well Identification`)
load("scrns_select.Rda")
setdiff(unique(data$`Well Identification`),df$LOCID)
setdiff(df$LOCID,unique(data$`Well Identification`))
locs<-intersect(df$LOCID,unique(data$`Well Identification`))
shp<-readOGR("wellscreens.shp")
plot(shp)
plot(shp[shp$LOCID %in% intersect(df$LOCID,unique(data$`Well Identification`)),])

pvt<-data[data$`Well Identification` %in% df$LOCID,c(4,11:15,17,30)]
long<-pivot_longer(pvt, 2:8, names_to = "Lithology")

locs<-unique(long$`Well Identification`)
liths<-unique(long$Lithology)
i=1
long$to<-NA
for (loc in locs) {dat<-long[long$`Well Identification`==loc,]
  for (i in 1:5) {
    if (i<5) {dat[dat$Lithology==liths[i],"to"]<-dat[dat$Lithology==liths[i+1],"value"]
    i=i+1
    }
    
    
    
dat[dat$Lithology==liths[i],"to"]<-dat[dat$Lithology==liths[i],"value"]+1
    i=1
  }
  }
}






write_csv(long,"tbl3_lith.csv")
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