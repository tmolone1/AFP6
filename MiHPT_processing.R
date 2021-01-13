
# this code processes UVOST data (%RE and K from hydraulic profiling), calculating means of the raw data over specified depth intervals (0.5 feet)
# the processed data is written to a csv which can be readily brought into a Leapfrog Hydro 3D model
rm(list=ls())
library(readtext)
library(dplyr)
library(tidyverse)
library(stringi)
library(stringr)
library(readxl)
setwd("C:/Users/tmoloney/Documents/work/AFP6/MiHPT")
files<-list.files()

roundup<-function (x,mult) {
  y = ceiling(x/mult)*mult
  return(y)
}

for (file in files) {
  df <- read_excel(file, col_types = rep("numeric", 12))
  MiHPT_ID <- gsub(".xlsx", "", file)
  df_short <- tibble(df[,c(1,12,4:7)]) # select the parameters of interest: depth, K, EC, ECD, PID, and FID
  colnames(df_short)<-c("depth", "K", "EC", "ECD", "PID", "FID")
  df_short <- df_short %>%  # with this data frame
    mutate(ints=cut(as.numeric(.[[1]]), breaks=(0:roundup(max(df[,1]),1)))) %>% #subdivide the data by intervals and aggregate
    group_by(ints) %>%  
    summarize(mean.K = mean(na.omit(K)), 
              mean.EC = mean(na.omit(EC)), 
              mean.ECD = mean(na.omit(ECD)), 
              mean.PID = mean(na.omit(PID)),
              mean.FID = mean(na.omit(FID)), 
              max.K = max(na.omit(K)), 
              max.EC = max(na.omit(EC)), 
              max.ECD = max(na.omit(ECD)), 
              max.PID = max(na.omit(PID)), 
              max.FID = max(na.omit(FID)))  # calculate stats for each depth interval
  df_short$MiHPT_ID<- MiHPT_ID  # assign the ID
  df_short$max.depth=max(df$`Depth (ft)`)
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
df_ALL$MiHPT_ID

#coords<- read_excel("M:/ItoN/Marathon/ProjectDocs/Gallup/Remediation/Projects/Reports/2019/LIF/UVOST/GPS Coordinates_Update.xlsx", skip = 2)
#coords<-coords[,c(1,4,5,6)]
#df_ALL<-merge(df_ALL, coords, by.x = "HoleID", by.y = "Location Name", all.x = TRUE)

# attach(df_ALL)
# Hue<-(mean.Ch1/mean.Signal)*255+(mean.Ch2/mean.Signal)*135+(mean.Ch3/mean.Signal)*15+(mean.Ch4/mean.Signal)*-10
# Hue<-replace(Hue,Hue>255, 185)
# plot(Hue)
# df_ALL$Hue<-Hue
round(cor(na.omit(drop)),
         digits = 2 # rounded to 2 decimals
      )
df_ALL3<-df_ALL3%>%mutate(DPT_ID=gsub("DPT", "", df_ALL3$DPT_ID))
df_ALL3<-df_ALL3%>%mutate(DPT_ID=gsub("A", "", df_ALL3$DPT_ID))
df_ALL<-df_ALL%>%select(12,14,15,2:6)
df_ALL<-df_ALL%>%mutate(MiHPT_ID=gsub("MIHPT-", "", df_ALL$MiHPT_ID))
merged<-merge(df_ALL,df_ALL3,by=1:3)
drop<-merged%>%select(4:9)
library(corrplot)
corrplot(cor(na.omit(drop)),
         method = "number",
         type = "upper", # show only upper side
         bg="darkgray"
)
# correlation tests for whole dataset
library(Hmisc)
res <- rcorr(as.matrix(na.omit(drop))) # rcorr() accepts matrices only

# display p-values (rounded to 3 decimals)
round(res$P, 3)


K_ints<-df_ALL[which(df_ALL$mean.K>0.11),]

#write output
write.csv(df_ALL,"Data_Processed.csv")
write.csv(coords,"coords.csv")
write.csv(K_ints,"K_ints.csv")

