# Clustering NBHF Clicks using densityClust, by Species
# 2/7/2024
# by Jackson VFB

library(tidyverse)
library(identidrift)
library(densityClust)

samples <- nbhf_clicks %>%
  nest(data=-species) %>%
  mutate(ev_samp_size=map_dbl(data, \(d) d %>% count(eventId) %>% pull(n) %>% median())) %>%
  mutate(samp=map2(data, ev_samp_size, \(d, s) d %>% group_by(eventId) %>% slice_sample(n=s) %>% ungroup())) %>%
  mutate(samp=map(samp, \(s) select(s, eventId, duration:centerkHz_3dB))) %>%
  mutate(samp=map(samp, \(s) s %>% mutate(id=1:n()) %>% drop_na())) %>%
  select(species, samp) %>% 
  unnest(samp)


sp <- c("ks", "pd", "pp")
clicks <- lapply(sp, \(x) filter(samples, species==x))
dists <- lapply(clicks, \(c) c %>% select(-c(species, eventId)) %>% column_to_rownames("id") %>% scale() %>% dist())
cls <- lapply(dists, densityClust)
cls <- lapply(cls, findClusters)
lapply(cls, plotDensityClust)
mapply(function(x, y) table(x$eventId, y$clusters), clicks, cls)

mapply(function(x,y) saveRDS(x, file=paste0(y, "_ev_clusters.rds")), cls, sp)