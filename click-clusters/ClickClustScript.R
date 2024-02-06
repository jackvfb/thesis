# Clustering NBHF Clicks using densityClust
# 12/19/2023
# by Jackson VFB

# In this script I use densityClust to cluster clicks in the NBHF training set.

# Note there is one stage of this that requires user input, which will need to 
# be done manually every time the script is run. It is the stage where the user 
# must select thresholds from the decision graph (line 77). These thresholds are
# chosen to segregate points with high values of rho, delta, and their product,
# gamma. These become the centroids of the clusters that are later plotted by
# densityClust. Since this decision is somewhat arbitrary, it will depend on who
# is running this script.

# SETUP -------------------------------------------------------------------

library(tidyverse)
library(identidrift)
library(PAMpal)
library(densityClust)
library(rfPermute)
library(randomForest)

# SUBSET DATA -------------------------------------------------------------

# There are too many clicks in the full training set to cluster in a reasonable
# amount of time using my computer. I will reduce size of data set. The size of
# the sample is limited by Dall's porpoise, which has not much more than 500 clicks
# in the database.

samp <- nbhf_clicks %>% 
  group_by(species) %>% 
  slice_sample(n=200) %>% # select 200 clicks at random from each species
  ungroup()


# FORMAT DATA -------------------------------------------------------------

# select variables of interest
c <- samp %>% 
  select(species, duration:centerkHz_3dB) %>%
  mutate(id = 1:n()) %>%
  filter(complete.cases(.))

# separate data into 1, 2, 3, peaked
c1 <- c %>%
  filter(peak2 == 0) # 1 peak only

c2 <- c %>%
  filter(peak2 != 0 & peak3 == 0) # 2 peaks only

c3 <- c %>%
  filter(peak3 != 0) # 3 peaks only



# MDS, by number of Peaks -----------------------------------------------------------

# make distance matrix, then vizualize clusters using MDS
doMDS <- function(x) {
  x.dist <- x %>%
    select(-species) %>%
    column_to_rownames("id") %>%
    scale() %>% # important step is to scale
    dist()
  
  x.mds <- x.dist %>%
    cmdscale(k=4) %>% # Use max four dimensions
    as.data.frame %>%
    setNames(paste0("PC", 1:ncol(.))) %>%
    mutate(species = x$species)
  
  return(x.mds)
}

c1.mds <- doMDS(c1)
c2.mds <- doMDS(c2)
c3.mds <- doMDS(c3)

# Some weird artifacts in the clusters. Separation looks weak.
c1.mds %>%
  ggplot(aes(PC1, PC2, color=species)) +
  geom_point()

c2.mds %>%
  ggplot(aes(PC1, PC2, color=species)) +
  geom_point()

c3.mds %>%
  ggplot(aes(PC1, PC2, color=species)) +
  geom_point()

# More artifacts. Separation looks weak.
c1.mds %>%
  ggplot(aes(PC2, PC3, color=species)) +
  geom_point()

c2.mds %>%
  ggplot(aes(PC2, PC3, color=species)) +
  geom_point()

c3.mds %>%
  ggplot(aes(PC2, PC3, color=species)) +
  geom_point()

# Can also try with all clicks, but there are some banding artifacts

# DENSITY CLUSTERS --------------------------------------------------------
# Clusters did not appear to separate very well using MDS. Try density clustering.

c.dist <- c %>%
  select(-species) %>%
  column_to_rownames("id") %>%
  scale() %>% # important step is to scale
  dist()

c.cl <- densityClust(c.dist) # allow dc (aura) to be generated automatically
c.cl <- findClusters(c.cl) # user chooses from decision graph at this stage
plotDensityClust(c.cl) # with four centroids chosen, most of the gamma-plot elbow seems to be captured.
saveRDS(c.cl, "C:/Users/jackv/Documents/thesis-clusters/c_cl.rds") # if needed again
table(c$species, c.cl$clusters)

# RANDOM FOREST --------------------------------------------------------------
# How different are the clusters? Unsupervised learning to identify the clusters.
# Now use supervised learning to quantify how different the clusters really are.

#format data
rf.dat <- c %>%
  mutate(clust = c.cl$clusters) %>%
  mutate(clust = as.factor(clust)) %>% 
  select(-c(species, id)) %>% 
  as.data.frame()

# train forest
cl.rf <- randomForest(formula = clust ~ .,
                   data = rf.dat,
                   sampsize = balancedSampsize(rf.dat$clust),
                   proximity = TRUE,
                   importance = TRUE)

# assess performance
plotConfMat(cl.rf)
#see distributions of important predictors
plotImpPreds(cl.rf, rf.dat, "clust")
