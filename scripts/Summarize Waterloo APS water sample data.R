rm(list=ls())
library(tidyverse)
library(readxl)
library(stringr)
library(janitor)
options(scipen=0)
#read data and tidy
water_samples<-read_excel("./data/Table 3 APS Analytical Summary Table_Export.xlsx")
water_samples<-clean_names(water_samples)
split<-str_split(water_samples$sample_id,"-",simplify=TRUE)
colnames(split)<-c("site","hole_id","depth_ft_bgs","unknown")
split<-as_tibble(split,)
water_samples<-as_tibble(cbind(water_samples,split))
rm(split)

# remove dups, trip blanks, and equipment blanks
remove<-c("DPT","DPTEB","DPTTB")
water_samples<-water_samples %>% filter(!hole_id %in% remove)  
rm(remove)

#remove MS/MSD and convert a sample with a random "B" tag to a numeric depth
water_samples$depth_ft_bgs<-str_remove_all(water_samples$depth_ft_bgs,"B")
water_samples$depth_ft_bgs_num<-as.numeric(water_samples$depth_ft_bgs)
water_samples<-water_samples %>% filter(!is.na(depth_ft_bgs_num)) 

#edit hole ids to match collar table
water_samples$hole_id<-str_remove_all(water_samples$hole_id," ")
water_samples$hole_id<-str_remove(water_samples$hole_id,"DPT")
water_samples$hole_id<-str_pad(water_samples$hole_id,pad = '0',2,side ='left')
water_samples$hole_id<-str_c(water_samples$hole_id,"A")
water_samples$hole_id<-str_c("DPT",water_samples$hole_id)

#process zs and Js in result column
water_samples$result_num<-ifelse(str_detect(water_samples$result,"U"),0,water_samples$result)
##water_samples$result<-str_replace(water_samples$result,"z","<")
water_samples$result_num<-str_remove(water_samples$result_num,"J")
water_samples$result_num<-parse_number(water_samples$result_num)
water_samples$result_label<-ifelse(str_detect(water_samples$result,"U"),"ND",water_samples$result_num)

# remove ms/msds
remove<-c("MS", "MSD")
water_samples<-water_samples %>% filter(!unknown %in% remove)  
rm(remove)

#summarize max concentrations by location for selects constituents of interest
analys<-c("Benzene","Trichloroethene", "Vinyl chloride" ,"cis-1,2-Dichloroethene", "Chromium VI")
tbl<-water_samples %>% filter(analyte %in% analys) %>% group_by(hole_id, analyte) %>% summarize(max.detect=max(result_num))
tbl$depth<-NA
tbl$units<-NA
#i<-47
for (i in 1:nrow(tbl)) {
  tbl$depth[i]<-water_samples %>% filter(hole_id %in% tbl$hole_id[i], analyte %in% tbl$analyte[i], result_num %in% tbl$max.detect[i]) %>% select(depth_ft_bgs_num)
  tbl$units[i]<-water_samples %>% filter(hole_id %in% tbl$hole_id[i], analyte %in% tbl$analyte[i], result_num %in% tbl$max.detect[i]) %>% select(units)
}
tbl$depth<-as.character(output_column(tbl$depth))
tbl$units<-as.character(output_column(tbl$units))
write_csv(tbl,"./outputs/summary_max_concentrations.csv")

