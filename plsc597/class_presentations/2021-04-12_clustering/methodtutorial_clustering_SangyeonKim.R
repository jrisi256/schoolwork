##############methods tutorial for clustering##############
##############210413##############
##############Sangyeon Kim#########

##############Data description: NYT U.S. Covid Data############
#In this methods tutorial, I will use data retrieved from "https://github.com/nytimes/covid-19-data/blob/"
#They provide both state-level and county-level covid data including "cases (cumulative counts)" and "deaths. 
#In addition, I have generated "daily counts" and "daily deaths" variables for all data points. 
#I will cover only cover portion of state-level U.S. covid data (from 2020/05/01 to 2020/08/31) 
#as it will take too much time to cover the whole dataset.

library(here)

#load a data file
load(here("class_presentations",
          "2021-04-12_clustering",
          "covid_after5before9.rda"))

#load required packages
library(mlr)
library(tidyverse)
library(GGally)
library(clue)
library(clusterSim)
library(clValid)

#In this tutorial, I will see whether state level daily covid counts can be clustered by using their daily case counts, cumulative case counts, deaily death counts and cumulative death counts.
#As the severity of 
###########1. K-means Clsutering##########
#data preprocessing and explorative plotting
covTib <- as_tibble(covid_scale) #make the datafile tibble
covScaled <- covTib %>% scale() #create a scaled matrix as without scaling values with absolutely higher magnitude would be overrepresented (in this case, "cases" and "deaths")

#descriptive plots to show how clusterings in the dataset look like
ggpairs(covid_scale,
        upper = list(continuous = "density"),
        lower = list(continuous = wrap("points", size = 0.5)),
        diag = list(continuous = "densityDiag")) +
  theme_bw()

#create a task
covTask <- makeClusterTask(data = as.data.frame(covScaled))

#create a learner
kMeans <- makeLearner("cluster.kmeans",
                      par.vals = list(iter.max = 100, nstart = 10)) #iter.max: setting the maximum number of iteratiosn, nstart: setting the number of random (initial) starts.

#setting parameters
kMeansParamSpace <- makeParamSet(
  makeDiscreteParam("centers", values = 3:12), #number of starting centers
  makeDiscreteParam("algorithm",
                    values = c("Hartigan-Wong", "Lloyd", "MacQueen"))) #three algorithms
gridSearch <- makeTuneControlGrid() #so total 18 sets to tune: 6(3to 8 centers) * 3(three algorithms)
kFold <- makeResampleDesc("CV", iters = 10) 

#setting a seed
set.seed(1)
tunedK <- tuneParams(kMeans, task = covTask,
                     resampling = kFold,
                     par.set = kMeansParamSpace,
                     control = gridSearch,
                     measures = list(db,G1)) #parameter tuning. Note: "dunn" cannot be applied to this code as it is defined in "function" not as "measure. I will use it in the hierarchical clustering later.
tunedK # 8 centers with MaxQueen algorithm

kMeansTuningData <- generateHyperParsEffectData(tunedK) #generate tuning data from parameter tuning
kMeansTuningData$data 

gatheredTuningData <- gather(kMeansTuningData$data,
                             key = "Metric",
                             value = "Value",
                             c(-centers, -iteration, -algorithm)) #make a tibble object for plotting

#plotting algorithm 
ggplot(gatheredTuningData, aes(centers, Value, col = algorithm)) +
  facet_wrap(~ Metric, scales = "free_y") +
  geom_line() +
  geom_point() +
  theme_bw()
#Davies-Bouldin index (db test); the smaller the better, pseudo F statistic (G1 test): the bigger the better

#training data with tuned parameters
#setting a seed again
set.seed(1)
tunedKMeans <- setHyperPars(kMeans, par.vals = tunedK$x) 
tunedKMeansModel <- train(tunedKMeans, covTask)
kMeansModelData <- getLearnerModel(tunedKMeansModel)
kMeansModelData$iter
#create cluster variable to original dataset
covTib <- mutate(covTib,
                  kMeansCluster = as.factor(kMeansModelData$cluster))

#plotting to see how cluster looks like
ggpairs(covTib, aes(col = kMeansCluster),
        upper = list(continuous = "density")) +
  theme_bw()

#1) When we see the plot, we can find that most of observations with small number of cumulative cases ("cases" in the plot) and cumulative deaths ("deaths" in the plot) are clustered in cluster 6(blue).
#so we can say that majority of states with low severity of Covid, that comprises most of the observations.
#2) The second largest cluster (cluster4 - light green) has similar trait with cluster 6, but has bit larger values of cumulative cases and cumulative deaths.
#so we can say that there are another chunck of states with low severity of Covid out of whole dataset, but relatively more severe compared to cluster 6.
#3) The smallest cluster (cluster 5 - Turquoise) shows the exact opposite characteristics of cluster 4 and 6: it has a huge number of cumulative cases, cumulative deaths and daily deaths.
#This shows that the US covid severity has been mainly driven by few states with heavy severity, which exactly matches with the reality at that point.
#4) Cluster 3 (heavy green) shows an interesting pattern: it has very high value of cumulative deaths and cumulative cases, but very low values of daily cases and daily deaths.
#The cluster should be comprised of states which underwent alarming covid situations at the very beginning of the pandemic but became stabilized around May, such as Washington.
#5) Clsuter 7 (purple) has observations with high cumulative cases and daily cases but with very low daily deaths and cumulative deaths. So these states were successfully dealing with deaths, but not with cases.
#Or, the population of these states might be much younger than others.

##################2. Hierarchical clustering ##############
#setting a seed again
set.seed(1)
covDist <- dist(covScaled, method = "euclidean") #costructing distance matrix, which is needed for hierarchical clustering
covHclust <- hclust(covDist, method = "ward.D2") #selecting "ward.D2" as a linkage method  
covDend <- as.dendrogram(covHclust) #creating a dendrogram object
plot(covDend, leaflab = "none") #plotting dendrogram

#creating function to save various measures (Davies-Bouldin index (db test), Dunn index (dunn), pseudo F statistic (G1 test)
cluster_metrics <- function(data, clusters, dist_matrix) {
  list(db = clusterSim::index.DB(data, clusters)$DB,
       G1 = clusterSim::index.G1(data, clusters),
       dunn = clValid::dunn(dist_matrix, clusters),
       clusters = length(unique(clusters))
  )
}

#bootstrapping sample to compare the model fit with stability. we will sample 3 becuase of the time limit
covBoot <- map(1:3, ~ {
  covScaled %>%
    as_tibble() %>%
    sample_n(size = nrow(.), replace = TRUE)
})

#setting a seed again
set.seed(1)
#Getting measures with K (number of cluster returned)
metricsTib <- map_df(covBoot, function(boot) {
  d <- dist(boot, method = "euclidean") #with euclidean distance
  cl <- hclust(d, method = "ward.D2") 
  map_df(3:12, function(k) {
    cut <- cutree(cl, k = k)
    cluster_metrics(boot, clusters = cut, dist_matrix = d)
  })
})

#making tibble object to be plotted
metricsTib <- metricsTib %>%
  mutate(bootstrap = factor(rep(1:3, each = 10))) %>%
  gather(key = "Metric", value = "Value", -clusters, -bootstrap)

#plotting how each K performed
ggplot(metricsTib, aes(as.factor(clusters), Value)) +
  facet_wrap(~ Metric, scales = "free_y") +
  geom_line(size = 0.1, aes(group = bootstrap)) +
  geom_line(stat = "summary", fun.y = "mean", aes(group = 1)) +
  stat_summary(fun.data="mean_cl_boot",
               geom="crossbar", width = 0.5, fill = "white") +
  theme_bw()
#Davies-Bouldin index (db test); the smaller the better,Dunn indext (dunn):the bigger the better, pseudo F statistic (G1 test): the bigger the better
#db: 4, dunn: 3~4, G1: 5
#K=4 looks the best

#plotting dendrogram with K=4
covCut <- cutree(covHclust, k = 4)
plot(covDend, leaflab = "none")
rect.hclust(covHclust, k = 4)

#creating tibble objet with a hierarchical cluster variable
covTib <- mutate(covTib, hclustCluster = as.factor(covCut))
ggpairs(covTib, aes(col = hclustCluster),
        upper = list(continuous = "density"),
        lower = list(continuous = wrap("points", size = 0.5))) +
  theme_bw()

#1) So when we do the hierarchical clustering, the algorithm combines clusterings in K-means clustering. For example, cluster 1 of hierarchical clustering is combination of cluster 4 and 6.
# Cluster 2 is a combination of cluster 1,2,8. Cluster 3 is a combination of cluster 5 and 7. cluster 4 is cluster 3 of k-means clustering.
#2) Hierarchical clustering basically shows a better big picture compared to k-means clustering. As I mentioned in the last section, cluster 4 and 6 from k-means clustering share very many features.
#3) However, I still think K-means clustering decribes the data better. For example, if we use hierarchical clustering, we would miss the difference between cluster 5 and 7 of K-means clustering, where the latter has significantly lower values of death related variables. 

#################3.bootstrapping to meausre stability of clusters#############
#hierarchical
library(fpc)
#setting a seed again
#set.seed(1)
#par(mfrow = c(3, 4))
#clustBoot <- clusterboot(covDist, B = 10,
#                         clustermethod = disthclustCBI,
#                         k = 4, cut = "number", method = "ward.D2",
#                         showplots = TRUE)
#clustBoot #Clusterwise Jaccard bootstrap (omitting multiple points) mean:0.9093808 0.3776772 0.9273452 0.9976190
#takes about 10 min in my machine

#K-means 
#setting a seed again
set.seed(1)
par(mfrow = c(3, 4))
clustBootl <- clusterboot(covScaled,
                         B = 10,
                         clustermethod = kmeansCBI,
                         k = 8, algorithm = "Hartigan-Wong",
                         showplots = TRUE)
clustBootl # Clusterwise Jaccard bootstrap (omitting multiple points) mean: 0.5056760 0.9433902 0.6184655 0.7970980 0.4347813 0.9353252 0.5055202 0.6882938
