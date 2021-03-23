datToolbox <- datToolbox %>% 
  group_by(Date) %>% 
  pivot_wider(names_from = Reach, values_from = Tot_DCU_cfs:ten_day_avg_URGWOM_cfs) %>% 
  select(Date, Year, Month.n, Day, Ag_DCU_cfs_5:OpenWater_DCU_cfs_8, Rain_cfs_5:Rain_cfs_8)

complete(Date1 = seq.Date(min(Date1), max(Date1), by = "day"), 
         fill = list(value = NA))