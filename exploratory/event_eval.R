#shows that by and large, clicks that do make it into the events(marked as detector_101), are producing good matches
# there are some good matches among others but the means are low
#because clicks that do make it into events are marked 101 and it means that they not only have a good match but that they are also close to other clicks with good matches
#next: statistical test, first combine all non-event clicks and event clicks
#also see whether clicks in kogia events generally match against the kogia tempalte

template %>%
  select(db, type, ends_with("thresh")) %>% 
  mutate(best=pmax(template$Pd_1_thresh, template$Pd_2_thresh, template$Pp_thresh,
     template$Ksp_thresh)) %>%
  ggplot(aes(x=as.factor(type), y=best))+
  geom_boxplot()
  
