source("./scripts/Summarize Waterloo APS water sample data.R")
width<-0.5
analy<-"Benzene"
for (analy in analys) {
tbl<-water_samples %>% filter(analyte == analy, !(unknown %in% (c("MS","MSD")))) %>% 
  mutate(from=depth_ft_bgs_num-width,to=depth_ft_bgs_num+width) %>% select(hole_id,collect_date,analyte,from,to,result_num,units,result,qualifier,rdl)

write_csv(tbl,path=paste0("./outputs/",analy,".csv"))
}
