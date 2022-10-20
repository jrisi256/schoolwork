# negative gossip datasets is from Yucel, M., Sjobeck, G. R., Glass, R., & Rottman, J. (2021). 
# full data can be accessed here: https://data.mendeley.com/datasets/kpjjvg39k3

# the current dataset is a directed social network indicating negative gossip between two actors
# an edge from i to j indicates that i has spread negative rumor about j 

library(here)
library(network)
library(sna)

#set seed
set.seed(20)

# read in the data
# in this tutorial we will only use part of the full dataset 
Gossip_data <- read.csv(here("class9/CugQap_Zhang/Gossip_data.csv"))
ngos <- Gossip_data[,3:46]

# reading matrix 
nngos <- network(as.matrix(ngos))

# plotting the network
plot(nngos)

# ==============================================================================
# CUG Test 

# we are interested in if the reciprocity and transitivity of this data is 
# statistically significantly high or low

# testing reciprocity with 200 simulated networks with the same edge number

# textbook recommended using 1000 reps but for this tutorial we only use 200 
# to save time 

grecip(nngos)
bedge = cug.test(dat = nngos, grecip, cmode = "edge", reps = 200)
bedge

# testing transitivity with 200 simulated networks controlling for dyad distribution
gtrans(nngos)
tran = cug.test(dat = nngos, gtrans, cmode = "dyad.census", reps = 200)
tran

# ==============================================================================
# QAP test 
# create distance matrix for homophily
dist <- as.matrix(dist(Gossip_data[,187]))

# independent variable: student's well-being, the higher the number the higher the well-being
wb <- as.numeric(unlist(Gossip_data[,187]))

# create sender covariate
send <- matrix(wb, nrow(Gossip_data), nrow(Gossip_data), byrow = FALSE)

# create receiver covariate 
rec <- matrix(wb, ncol(ngos), ncol(ngos), byrow = TRUE)

# create 'graph stack' of covariates
covariates <- list(dist = dist, send = send, rec = rec)

# logistics regression
logi <- netlogit(nngos, covariates, nullhyp = "classical")
logi 

# QAP
qap <- netlogit(nngos, covariates, nullhyp = "qap", reps = 100)
qap
