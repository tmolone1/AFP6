source("./scripts/Summarize Waterloo APS water sample data.R")
width<-1
shifts<-c(0,0,1,-1,0)
# analy<-"Vinyl chloride"
for (analy in analys) {
  shift<-shifts[which(analys==analy)]
tbl<-water_samples %>% filter(analyte == analy, !(unknown %in% (c("MS","MSD")))) %>% 
  mutate(from=depth_ft_bgs_num-width+shift,to=depth_ft_bgs_num+width+shift) %>% select(hole_id,collect_date,analyte,from,to,result_num,units,result_label,result,qualifier,mdl,pql)

write_csv(tbl,path=paste0("./outputs/",analy,".csv"))
}
