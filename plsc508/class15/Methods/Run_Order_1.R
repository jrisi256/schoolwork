# THIS FILE CAN BE RUN AS IS, CREATES 1000 SIMULATIONS OF BINARY TREATMENT
# SEE RUN_ORDER_3.R FOR FULL COMMENTARY AND EXPLANATION

# TOTAL RUNTIME: ~1 SECOND

#rm(list=ls())
# Set your working directory
#setwd("/Users/tylersuman/Documents/PLSC 508/Methods")
 
library(foreign)
library(here)
butler <- read.dta(here("class15", "Methods", "nm.replication.dta"))

### Random assignment using the matching categories defined by B&N

# The original data source provides matched pairs of legislators
# that are most ideologically similar to each other as measured by
# party affiliation and district vote share for George W. Bush (2004).

# To create random treatment assignments, treatment is randomly assigned
# within these matched pairs such that one legislator in the pair is treated
# and one legislator is not treated. 

# The following function takes these matched pairs and constructs a random treatment.

block.ra <- function(blockvar, m=NULL){
  # If not all data have a matched pair, then stop 
  if(!all(mean(table(blockvar))==table(blockvar))){stop("Blocks not equal size")}
  # Create empty data frame to hold treatment assignment
  assign <- rep(NA, length(blockvar))
  # Get total number of matched pairs
  blocks <- unique(blockvar)
  # Randomly assign treatment for each element in each matched pair
  for(k in 1:length(blocks)){
    N.block <- length(assign[blockvar==blocks[k]])
    m.block <- floor(N.block/2)
    if(!is.null(m)){m.block<-m}
    assign[blockvar==blocks[k]] <- ifelse(1:N.block %in% sample(1:N.block,m.block),1,0)
  }
  return(assign)
}

set.seed(1234567) 
# Create 1000 random treatments
Z_block <- replicate(1000, block.ra(blockvar=butler$match_category))

save(Z_block,file="CoppockJEPS_10000randomizations.rdata")
