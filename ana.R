events <- nbhf_clicks %>%
  distinct(species, eventId)

rp <- nbhf_clicks %>%
  distinct(species, eventId, .keep_all=TRUE) %>%
  count(species, eventLabel)

clickSum <- nbhf_clicks %>%
  count(species)

eventSum <- events %>%
  count(species)

nbhf_clicks %>%
  count(eventId)

nbhf_clicks %>%
  group_by(eventId) %>%
  count() %>%
  pull(n) %>%
  quantile()

nbhf_clicks %>%
  count(species, eventId) %>%
  ggplot(aes(x=species, y=n))+
  geom_violin()


# d -----------------------------------------------------------------------

c <- nbhf_clicks %>%
  mutate(num=as.factor(case_when(peak2!=0 & peak3==0 ~ 2,
                                 peak3!=0 ~ 3,
                                 .default = 1)))


c %>%
  ggplot(aes(x=peak, color=species))+
  geom_density()+
  facet_wrap(~num)
         