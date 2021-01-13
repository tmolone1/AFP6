
# this code processes UVOST data (%RE and K from hydraulic profiling), calculating means of the raw data over specified depth intervals (0.5 feet)
# the processed data is written to a csv which can be readily brought into a Leapfrog Hydro 3D model
rm(list=ls())
library(readtext)
library(dplyr)
library(tidyverse)
library(stringi)
library(stringr)
library(readxl)
setwd("C:/Users/tmoloney/Documents/work/AFP6/APS")
files<-list.files()

roundup<-function (x,mult) {
  y = ceiling(x/mult)*mult
  return(y)
}
#file<-files[4]
for (file in files) {
  df <- read_excel(file, col_types = rep("numeric", 8), sheet = "Processed Ik")
 DPT_ID <- gsub("_Groundwater Profiling Log_MSTJV.xlsx", "", file)
  df_short <- tibble(df[,c(3,8)]) # select the parameters of interest: depth, K, EC, ECD, PID, and FID
  colnames(df_short)<-c("depth", "IK")
  df_short <- df_short %>% mutate(depth = -depth)
  df_short <- df_short %>% filter(!is.na(depth))
  df_short <- df_short %>%  
    mutate(ints=cut(as.numeric(.[[1]]), breaks=(0:roundup(max(df_short$depth),1)))) %>% #subdivide the data by intervals and aggregate
    group_by(ints) %>%  
    summarize(mean.IK = mean(na.omit(IK)))  # calculate stats for each depth interval
  df_short$DPT_ID<- DPT_ID  # assign the ID
  if(exists("df_ALL")) df_ALL<- df_ALL else df_ALL<- df_short[FALSE,]
  df_ALL<-rbind(df_short[FALSE,],df_ALL,df_short)
}

#process the top and bottom depths of the intervals
uchar<-as.character(df_ALL$ints)
split<-stringi::stri_split_regex(uchar,",",simplify=TRUE)
top<-as.numeric(gsub("[^0-9\\.]", "", split[,1]))
bottom<-as.numeric(gsub("[^0-9\\.]", "", split[,2]))
df_ALL<-cbind(df_ALL,top,bottom)
df_ALL <- df_ALL %>% mutate(top = replace(top, top > 500, 0))
df_ALL<- df_ALL %>% mutate(DPT_ID = paste0(DPT_ID, "A"))
df_ALL$DPT_ID
df_ALL <- df_ALL[,c(3:5,2)]

#coords<- read_excel("M:/ItoN/Marathon/ProjectDocs/Gallup/Remediation/Projects/Reports/2019/LIF/UVOST/GPS Coordinates_Update.xlsx", skip = 2)
#coords<-coords[,c(1,4,5,6)]
#df_ALL<-merge(df_ALL, coords, by.x = "HoleID", by.y = "Location Name", all.x = TRUE)

# attach(df_ALL)
# Hue<-(mean.Ch1/mean.Signal)*255+(mean.Ch2/mean.Signal)*135+(mean.Ch3/mean.Signal)*15+(mean.Ch4/mean.Signal)*-10
# Hue<-replace(Hue,Hue>255, 185)
# plot(Hue)
# df_ALL$Hue<-Hue
#K_ints<-df_ALL[which(df_ALL$mean.K>0.11),]

#write output
write.csv(df_ALL,"APS_Data_Processed.csv")
#write.csv(coords,"coords.csv")
#write.csv(K_ints,"K_ints.csv")

