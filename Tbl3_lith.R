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

pvt<-data[data$`Well Identification` %in% df$LOCID,c(4,11:15)]
long<-pivot_longer(pvt, 2:6, names_to = "Lithology")

locs<-unique(long$`Well Identification`)
liths<-unique(long$Lithology)
long$to<-NA
out<-tibble(head(long))
out[seq(1:nrow(out)),seq(1:ncol(out))]<-NA
for (loc in locs) {dat<-long[long$`Well Identification`==loc,]
dat<-long[long$`Well Identification`==loc,]
dat$value<-as.numeric(dat$value)
dat$to<-as.numeric(dat$to) 
dat<-dat %>% arrange(value) 
liths<-dat$Lithology
i=1
    for (lith in liths) {n<-dat[(which(dat$Lithology==lith)+i),]$value
      while (is.na(n)) {
        if (which(dat$Lithology==lith)<nrow(dat)) {i<-i+1
          n<-dat[(which(dat$Lithology==lith)+i),]$value
        }
        if (is.na(n)) {
         n<-max(na.omit(dat$value))+1
         dat[dat$Lithology==lith,]$to<-n
        }
        dat[dat$Lithology==lith,]$to<-n
      }
    dat[dat$Lithology==lith,]$to<-n
    if (is.na(dat[(which(dat$Lithology==lith)),]$value)) {
      dat[dat$Lithology==lith,]$to<-NA
    }
    }
out<-rbind(out,dat)
}
out<-out[!is.na(out$value),]


write_csv(out,"tbl3_lith.csv")
