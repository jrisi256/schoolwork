#### “Can Learning Constituency Opinion Affect How Legislators Vote? Results from a Field Experiment.” 
#### Quarterly Journal of Political Science 6(1): 55–83. DOI: 10.1561/100.00011019

# TOTAL RUNTIME: ~6 MINUTES


### ARTICLE SUMMARY ### (full text included in zip archive)

# Original experiment (Bulter and Nickerson, 2011) used a field experiment
# to examine whether lawmakers responded to selected treatment.
# Treatment: Individual lawmakers exposed to new voter preference information
# Assumption: Lawmakers respond to their own treatment but not to treatment of others
# Finding: Lawmakers change their voting behavior based on receiving new voter preferences

# This article (Coppock, 2014) removes assumption of treatment independence
# under the assertion that lawmakers tend to share information with each other.

# First, we test for significance of spillover effect in original dataset
# Second, we test for spillover effect in 1000 simulated datasets

# The final product (graph) shows that the estimated spillover effect is significant!

#rm(list=ls())
# Set your working directory
#setwd("/Users/tylersuman/Documents/PLSC 508/Methods")
library(here)
load(here("class15", "Methods", "CoppockJEPS.RData"))
# t1 <- Sys.time() 

library(lattice)


# Used in creating intervals on graph later on
find_breaks <- function(x){
  breaks <- rep(NA, length(x)-1)
  for(i in 1:length(breaks)){
    breaks[i+1] <- x[i]!=x[i+1]
  }
  return(which(breaks))
}

### Constructing a Causal Model

# We need to create a function that translates outcomes observed under
# actual treatment assignment to outcomes that would be observed under
# a hypothetical treatment assignment.

Z.obs <- CoppockJEPS$treatment # Z-vector is actual (observed) treatment
Y.obs <- CoppockJEPS$sb24      # Y-vector is observed outcomes (votes)
exposure.obs <- CoppockJEPS$s.difs.dw # vector of 'exposure' values (see below)

# The exposure variable above is the sum of the causal effects of treatment.
# That is, this variable captures the hypothetical 'spill-over' effects of treatment.
# This variable is constructed in Run_Order_2.R

# The basis for this measure is an ideological similarity score built on prior voting.
# 'Raw exposure' is the sum(similarity score[i] * treatment[i]) for all legislators [i]
# where treatment[i] = 1 if treated, = 0 if not treated.

### TWO THEORETICAL HURDLES FOR ANY APPLICATION ###

# (1) Legislators whose set of similarity scores are higher than average
#     are more likely to experience spillovers.
#     This would mean that the exposure variable would be correlated
#     with ideology and associated unobservable characteristics.

# (2) Raw Exposure is mechanically correlated with direct treatment: 
#     when legislator i is treated, only 34 of the remaining 69 legislators can be treated, 
#     resulting in a lower level of raw exposure.

### SOLUTION ###
# Make the variable of interest 'net exposure', or raw exposure minus expected exposure


# The model form (below) is a simple linear model where the outcome variable
# is a product of the observed treatment and estimated spillover effects

fit.obs <- lm(Y.obs ~ Z.obs + exposure.obs)

# This model returns significant, non-zero coefficients for both IVs - great!

ssr.obs <- sum(residuals(fit.obs)^2)
direct.obs <- fit.obs$coefficients[2]
indirect.obs <- fit.obs$coefficients[3]

# So far, these results show that, for the real case using real observations,
# there is evidence of a spillover effect.


# Now, let's validate these results using a series of simulations.


# These two vectors contain the hypothetical exposure values for each legislator
# Each legislator gets two values, one for each possible treatment state
exposure.expected.1 <- CoppockJEPS$exposure.expected.1.dw
exposure.expected.0 <- CoppockJEPS$exposure.expected.0.dw


# These are used in simulating Y-values (see below)
directs <-seq(from=-.7, to=0.2, by=.025)
indirects <-seq(from=-.7, to=0.2, by=.025)


### MAIN SIMULATION LOOP ###

sims <- 500
pmat.ssr <- matrix(NA, length(directs), length(indirects))
set.seed(343)
for(j in 1:length(directs)){
  for(k in 1:length(indirects)){
    direct.sim <- directs[j]
    indirect.sim <- indirects[k]
    
    ssr.sims <- rep(NA,sims)
    for(i in 1:sims){
      # Calling a pre-made simulated random treatment vector
      Z.sim <- Z_block[,sample(1:1000, 1)]
      # Creating raw exposure measure unique to this random treatment vector
      exposure.sim <- (similarity.matrix.dw %*% Z.sim)
      # Correcting the raw exposure measure by subtracting expected exposure and standardizing
      exposure.sim.corrected <- exposure.sim - (Z.sim * exposure.expected.1 + (1-Z.sim)*exposure.expected.0)
      s.exposure.sim <- (exposure.sim.corrected - mean(exposure.sim.corrected))/sd(exposure.sim.corrected)
      # Generating simulated observed outcomes based on original values
      # but reflecting the simulation's unique random treatment
      pure.Y0 <- Y.obs + (-1*exposure.obs*indirect.sim)
      pure.Y0[Z.obs==1] <- pure.Y0[Z.obs==1] - direct.sim
      Y.sim <- pure.Y0 + direct.sim*Z.sim + indirect.sim*s.exposure.sim
      #Creating model with same specification as previous
      fit.sim <- lm(Y.sim ~ Z.sim + s.exposure.sim)
      ssr.sims[i] <- sum(residuals(fit.sim)^2)
    }
    
    # Matrix of p-values for each simulated coefficient pair
    pmat.ssr[j,k] <- mean(ssr.obs > ssr.sims)
    
  }
}


### CREATING FIGURE ###

# Find first/last rows from p-value matrix that contain at least one p-value  over 0.1
# Necessary for creating error bars
direct_breaks <- find_breaks(apply(pmat.ssr, MARGIN=1, FUN=max) >.1)
# Find first/last columns from p-value matrix that contain at least one p-value over 0.1
indirect_breaks <- find_breaks(apply(pmat.ssr, MARGIN=2, FUN=max) >.1)

graph.frame <- expand.grid(x=directs, y=indirects)
graph.frame$z <- as.vector(pmat.ssr)
col.l <- colorRampPalette(c('white', 'black'))(1000)
depth.breaks <- do.breaks(c(0,1), 20)
fig2 <- levelplot(z~x*y, graph.frame, cuts=20, col.regions=col.l,
                  colorkey=FALSE,
                  at=depth.breaks,
                        ylab = "Simulated Spillover Effect",
                        xlab = "Simulated Treatment Effect",
                        scales=list(x=list(at=round(seq(-.7, .2, by=.1), digits=1), labels=round(seq(-.7, .2, by=.1), digits=1)),
                                    y=list(at=round(seq(-.7, .2, by=.1), digits=1), labels=round(seq(-.7, .2, by=.1), digits=1))),
                        panel = function(...) {
                          panel.levelplot(...)
                          panel.abline(h = 0, lty=2)
                          panel.abline(v = 0, lty=2)
                          larrows(y0=-.5, y1= -.5, x0=directs[direct_breaks[1]], x1=directs[direct_breaks[2]], angle=90,code=3)
                          larrows(x0=-.6, x1= -.6, y0=indirects[indirect_breaks[1]], y1=indirects[indirect_breaks[2]], angle=90,code=3)
                        },
                  legend = 
                    list(right = 
                           list(fun = draw.colorkey,
                                args = list(key = list(col = col.l, at = depth.breaks),
                                            draw = FALSE))),
)


pdf("Simulation_Results.pdf")
print(fig2)
dev.off()

