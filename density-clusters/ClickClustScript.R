# Clustering NBHF Clicks using densityClust
# 2/10/2024
# by Jackson VFB

# In this script I use densityClust to cluster clicks in the NBHF training set.

# Note there is one stage of this that requires user input, which will need to 
# be done manually every time the script is run. It is the stage where the user 
# must select thresholds from the decision graph. These thresholds are
# chosen to segregate points with high values of rho, delta, and their product,
# gamma. These become the centroids of the clusters that are later plotted by
# densityClust. Since this decision is somewhat arbitrary, it will depend on who
# is running this script.

# SETUP -------------------------------------------------------------------

library(tidyverse)
# next package is my own which is not on CRAN.
library(identidrift) # Must be installed from my GH "jackvfb/identidrift"
library(PAMpal)
library(densityClust)
library(rfPermute)
library(randomForest)

# SUBSET DATA -------------------------------------------------------------

# There are too many clicks in the full training set to cluster in a reasonable
# amount of time using my computer. I will reduce size of data set. Value chosen
# so as not to exceed size of smallest (species) group

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

# DENSITY CLUSTERS --------------------------------------------------------

c.dist <- c %>%
  select(-species) %>%
  column_to_rownames("id") %>%
  scale() %>% # important step is to scale
  dist(method="euclidean")

c.cl <- densityClust(c.dist) # allow dc (aura) to be generated automatically
c.cl <- findClusters(c.cl) # user chooses from decision graph at this stage
plotDensityClust(c.cl)
# saveRDS(c.cl, "density-clusters/saved_clusters/myCluster.rds")
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
