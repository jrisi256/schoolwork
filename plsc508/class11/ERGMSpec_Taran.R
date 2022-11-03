# The dataset for this tutorial comes from a study of citation patterns in state court decisions
# in 2010 by Rachael Hinkle (Buffalo) and Michael Nelson (here!). The dataset includes weighted
# dyadic ties--the count of citations from each state supreme court i to each other state supreme
# court j. We convert these into a binary network where state sup. court i sends a tie to state sup. court j
# if the number of citations in 2010 of court j by court i is equal to or above the national median.

# Citation:
# Hinkle, Rachael K. and Michael J. Nelson. 2016. “The Transmission of Legal Precedent
# among State Supreme Courts in the Twenty-First Century.”
# State Politics & Policy Quarterly 16(4):391–410.

## Data pre-processing

# Load necessary packages

library(tidyr)
library(here)
library(dplyr)
library(ggplot2)
library(statnet)
library(haven)
library(job)

# Load citation data
dfa_edgelist <- read_dta(here("class11/TutorialData/data.for.analysis.dta")) %>% 
  # Filter out DC, as some dyadic data isn't available for it
  filter(citingCourt != "DC" & citedCourt != "DC") %>%
  # Network edges are valued, so we convert to a binary network by preserving ties if 
  # the weight is equal to or above the median 
  mutate(tie = ifelse(citeCount >= quantile(citeCount, .8), 1, 0)) %>%
  # Filter to only include dyads where a tie, as defined, exists
  filter(tie > 0) %>%
  # Reduce to a two-column edgelist
  dplyr::select(citingCourt, citedCourt)

# Load state (node) attributes
vertex_attrs <- read_dta(here("class11/TutorialData/StateLevelVariables_1.dta")) %>%
  left_join(read_dta(here("class11/TutorialData/berryData.dta"))) %>% filter(state != "DC")

# Load geographic (edge) attributes
geo_edge_attrs <- read_dta(here("class11/TutorialData/ivDyads.dta")) %>% 
  filter(citingCourt != "DC" & citedCourt != "DC")

# Initialize network with 52 nodes (for 52 courts -- Oklahoma and Texas have two high courts)
dfa_net <- network.initialize(52, directed = T)

# Set node names
network.vertex.names(dfa_net) <- as.character(vertex_attrs$state)

# Add in edges from edgelist
dfa_net[as.matrix(dfa_edgelist)] <- 1

# Visualize the network
set.seed(2022)
plot(dfa_net, displaylabels=T, label.cex=.5)

# Convert contiguity column into a matrix where entry (i,j) is 1 if states i and j are contiguous 
# and 0 if states i and j are otherwise (including all diagonal entries). ERGM cannot accept
# any NAs in edge attribute matrices, so we have to assign something along the diagonal.
contig_matrix <- as.matrix(geo_edge_attrs %>% 
                             dplyr::select(-c(distance)) %>% 
                             spread(citingCourt, contiguous))
rownames(contig_matrix) <- colnames(contig_matrix)[-1]
contig_matrix <- contig_matrix[,-1]
mode(contig_matrix) <- "numeric"
contig_matrix[is.na(contig_matrix)] <- 0

# Assign state citizen ideology, state court professionalism, and state federal circuits as node attributes
set.vertex.attribute(dfa_net, "CitizenIdeo", as.vector(vertex_attrs$citi6013))
set.vertex.attribute(dfa_net, "SquireIndex", as.vector(vertex_attrs$SquireIndex))
set.vertex.attribute(dfa_net, "Circuit", as.vector(vertex_attrs$Circuit))
set.vertex.attribute(dfa_net, "LegCap", as.vector(vertex_attrs$LegCap)/1000)

## Modeling

# Let's specify an initial ERGM model where the only predictor is the count of edges in the network.
# We are essentially modeling a network that only controls for the number of edges,
# such that any simulated object from this ERGM will have the same density as the
# state court network.

model1 <- ergm(dfa_net ~ edges, control = control.ergm(seed = 2022))

# View the model coefficient. 
summary(model1)

# ERGM coefficients are in log-odds, so let's convert it into an odds ratio.
exp(coef(model1))/(1 + exp(coef(model1)))

# This is identical to the density of the network!
gden(dfa_net)

# If we simulate 100 networks from this ergm, the densities are centered around the true density.
hist(gden(simulate(model1, nsim = 100)))

# Does this model explain the network structure well? We can check the goodness-of-fit.
gof1 <- gof(model1)
plot(gof1)

# We can also add node-level covariates to model specification.

# We might expect that states in the same federal circuit are likely to frequently cite each other.
# We can use "nodematch" to estimate the effect of circuit homophily.

model2 <- ergm(dfa_net ~ edges + nodematch("Circuit"), control = control.ergm(seed = 2022))

summary(model2)

# Check the goodness of fit.
gof2 <- gof(model2)
plot(gof2)

# ERGMs can also include terms for network-level variation, such as transitivity and reciprocity.
# Here, we might expect that state courts are more likely to frequently cite
# state court that frequently cites them. 

grecip(dfa_net, measure = "edgewise")

# We can also see the number of ties which are reciprocated in the network by calling the summary function
# on the ERGM formula. 

summary(dfa_net ~ mutual)
summary(dfa_net ~ mutual)*2 / summary(dfa_net ~ edges)

# "mutual" is the ERGM term for reciprocity. We can include it here.

model3 <- ergm(dfa_net ~ edges + nodematch("Circuit") + mutual, control = control.ergm(seed = 2022))

summary(model3)

# Since this model uses a dependence term, we need to check the MCMC trace plots for randomness +
# the histograms for normality.

par(mar = c(1, 1, 1, 1))
mcmc.diagnostics(model3)

# Check the goodness of fit.
gof3 <- gof(model3)
plot(gof3)

# We might also expect that court "professionalization", measured by Squire's index 
# established in 2008, influences the likelihood of forming a frequent citation tie.

# "nodecov" is the ERGM term for continuous nodal covariates. Here, we model
# whether or not a tie is likely to be formed when the sum of two nodes' professionalization
# indices increases.

model4 <- ergm(dfa_net ~ edges + 
                 nodematch("Circuit") + 
                 nodecov("SquireIndex") + 
                 mutual, 
               control = control.ergm(seed = 2022))

summary(model4)

mcmc.diagnostics(model4)

gof4 <- gof(model4)
plot(gof4)

# But wait! Couldn't the effect of court professionalization be different when it comes to
# being cited and citing other state courts? For example, professionalized courts might be
# more likely to be cited by other courts -- increasing their indegree -- but less likely
# to go and cite other courts because they already might have strong internal precedential
# regimes -- decreasing their outdegree. 

# "i" and "o" can be inserted after "node" to account for this for many node-level variables
# including "nodecov". Let's do that, and see if the effects are distinct.

model5 <- ergm(dfa_net ~ edges + 
                 nodematch("Circuit") + 
                 nodeicov("SquireIndex") +
                 nodeocov("SquireIndex") +
                 mutual, 
               control = control.ergm(seed = 2022))

summary(model5)

mcmc.diagnostics(model5)

gof5 <- gof(model5)
plot(gof5)

# "absdiff" lets us identify how the difference in two nodes' values for some continuous attribute
# affects the likelihood of a tie forming. We might expect that state courts that have more ideologically
# incongruent citizenry are less likely to form a tie.

model6 <- ergm(dfa_net ~ edges + 
                 nodematch("Circuit") + 
                 nodeicov("SquireIndex") +
                 nodeocov("SquireIndex") +
                 absdiff("CitizenIdeo") + 
                 mutual, 
               control = control.ergm(seed = 2022))

summary(model6)

mcmc.diagnostics(model6)

gof6 <- gof(model6)
plot(gof6)

# One last thing: maybe states that are contiguous are more likely to cite each other, by virtue of
# simply being nearby. Edge covariates can be included by passing a matrix as an argument to "edgecov".

model7 <- ergm(dfa_net ~ edges + 
                 nodematch("Circuit") + 
                 nodeicov("SquireIndex") +
                 nodeocov("SquireIndex") +
                 absdiff("CitizenIdeo") + 
               mutual + edgecov(contig_matrix), 
               control = control.ergm(seed = 2022))

summary(model7)

mcmc.diagnostics(model7)

gof7 <- gof(model7)
plot(gof7)

# DON'T RUN! "transitive" adds a statistic for the number of transitive triads in the network,
# allowing us to model if a transitive frequent citation tie is more likely. But this takes too long
# and ultimately throws an error.

model8 <- ergm(dfa_net ~ edges + 
                 nodematch("Circuit") + 
                 nodeicov("SquireIndex") +
                 nodeocov("SquireIndex") +
                 absdiff("CitizenIdeo") + 
                 mutual + edgecov(contig_matrix) + transitive, 
               control = control.ergm(parallel = 8, seed = 2022))

# A non-degenerate model can include transitivity with the gwesp term.

job::job({model9 <- ergm(dfa_net ~ edges + 
                 nodematch("Circuit") + 
                 nodeicov("SquireIndex") +
                 nodeocov("SquireIndex") +
                 absdiff("CitizenIdeo") + 
                 mutual + edgecov(contig_matrix) +
                 nodeicov("LegCap") + 
                 nodeocov("LegCap") + gwidegree(1, fixed = TRUE) + 
                 gwodegree(1, fixed = TRUE) + gwesp(.5, fixed = TRUE),
               control = control.ergm(seed = 2022, parallel = 8, MCMC.samplesize = 2000))})

summary(model9)

mcmc.diagnostics(model9)

gof9 <- gof(model9)

plot(gof9)

