###The data file contains an undirected social network of frequent associations between 62 dolphins in a community living off Doubtful Sound, New Zealand. (compiled by D. Lusseau, K. Schneider, O. J. Boisseau, P. Haase, E. Slooten, and S. M. Dawson, Behavioral Ecology and Sociobiology 54, 396-405 (2003)).
###Data collected by marine biologists observing interactions between 62 bottlenose dolphins.
## Additional information on the network can be found in:
# D. Lusseau, The emergent properties of a dolphin social network, Proc. R. Soc. London B (suppl.) 270, S186-S188 (2003).
# D. Lusseau, Evidence for social role in a dolphin social network, Preprint q-bio/0607048 (http://arxiv.org/abs/q-bio.PE/0607048)

#preparation
library(latentnet)
library(igraph)
library(intergraph)

#load the data
dolphins <- read.graph(file = "http://users.dimi.uniud.it/~massimo.franceschet/teaching/datascience/network/R/dolphin.gml", 
                       format = "gml")


#explore the df
summary(dolphins) #Print a summary of the network: the network is undirected.

vcount(dolphins) #There are 62 nodes.
V(dolphins)  #Nodes: dolphins 
V(dolphins)$label #Dolphin names

ecount(dolphins) #There are 159 ties.
E(dolphins)  #Ties: undirected relations denoting frequent interactions between dolphins

#visualize the network using Fruchterman-Reingold algorithm
plot(dolphins, layout = layout_with_fr, edge.arrow.size = .3, 
     edge.curved = 0, vertex.color = rgb(.25, .5, .3, alpha = .5), 
     vertex.frame.color = "#555555", vertex.label = V(dolphins)$label, 
     vertex.label.color = "black", vertex.label.cex = .5)

#visualize the network by gender
V(dolphins)$sex #Summary about the gender of dolphins
male = V(dolphins)$sex == "M"
male
sum(male)
female = V(dolphins)$sex == "F"
sum(female)

V(dolphins)$color = "white"
V(dolphins)[male]$color = "blue"
V(dolphins)[female]$color = "pink" 
coords = layout.fruchterman.reingold(dolphins)
plot(dolphins, layout = coords, vertex.label = V(dolphins)$label, vertex.size = 5) 

#set the seed
set.seed(125)

#convert to network
dol <- asNetwork(dolphins)

#estimate a 2-dimensional latent space model using Euclidean distance
dolphins.2d <- ergmm(dol ~ euclidean(d = 2), verbose = TRUE) #d: the dimension of the latent space
summary(dolphins.2d) #a BIC of 1191.95 (model fit)

#find the latent position
dolphins.2d$mcmc.pmode$Z #Z is latent position

#plot estimated latent locations
dev.off()
par(mfrow = c(1,2))
plot(dolphins.2d, plot.vars = FALSE, pad = 0) 
plot(dolphins.2d, what = "pmean", plot.vars = FALSE, pad = 0) #pmean: plot the posterier means
#the estimated latent positions are not very similar to the locations of nodes in the original network plot.

SEXcolors = c("blue","red")[match(V(dolphins)$sex, c("M","F"))]
plot(dolphins.2d, label = V(dolphins)$sex, vertex.col = SEXcolors,
     what = "pmean", main = "MCMC positions", print.formula = FALSE, labels = TRUE) #by gender, with colored nodes

#estimate a 2-d latent space model with homophily effect for gender
dolphins.h <- ergmm(dol ~ nodefactor("sex") + nodematch("sex") + euclidean(d = 2), verbose = TRUE) #nodematch: count the N of cases where two connected nodes have the same attribute.
summary(dolphins.h)                                                                                #nodefactor: difference in forming ties conditional on gender
#results suggest that there is not a statistically significant difference between how male and female dolphins make connections in this network.                                                                              
#however, there is a high probability of interacting with a dolphin when he/she is from the same gender.

#estimate a 2-d latent space model with bilinear() 
dolphins.b <- ergmm(dol ~ bilinear(d = 2), verbose = TRUE) #bilinear: adds a term of inner product of Zi * Zj.
summary(dolphins.b) #a overall BIC of 1058.86 (better than the model using Euclidean distance).

#find the latent position
dolphins.b$mcmc.pmode$Z

#plot estimated latent locations
par(mfrow = c(1,2))
plot(dolphins.b, plot.vars = FALSE, pad = 0)
plot(dolphins.b, what = "pmean", plot.vars = FALSE, pad = 0)

SEXcolors = c("blue","red")[match(V(dolphins)$sex, c("M","F"))]
plot(dolphins.b, label = V(dolphins)$sex, vertex.col = SEXcolors,
     what = "pmean", main = "MCMC positions", print.formula = FALSE, labels = TRUE) #by gender

#check for model convergence - if the LSM has converged to stable parameter estimates
dolphins.cov <- ergmm(dol ~ euclidean(d = 2), control = control.ergmm(burnin = 25000, sample.size = 30000), verbose = TRUE) #increase the sample size to give the MCMC chain more steps to converge

par(mar = c(2, 2, 2, 2)) #adjust plot margins
mcmc.diagnostics(dolphins.cov) #check the MCMC convergence diagnostics
#the autocorrelation plots show that as the lag increases, the autocorrelation decreases quickly.
#the trace plots indicate that the MCMC samples do not show any apparent anomaly.

#fit between the model and the network
#calculate the density of ties in the LSM
dol.density <- sapply(simulate(dolphins.2d, 500)[[2]], network.density) #simulate 500 networks from the model, calculate the density for each network.

#plot the distribution 
dev.off()
hist(dol.density, main = "Distribution of Network Densities Simulated from the Model", 
     xlab = "Network Density", xlim = c(0.07, .18))

#comparing the densities of the simulated networks   
abline(v = network.density(dol), col = "hot pink", lty = 1, lwd = 3) #lty: line type; lwd: line width
abline(v = quantile(dol.density, probs = c(.025, .5, .975)), lty = 2, lwd = 3) #2.5%, 50%, 97.5% quantiles of the densities
#the observed network density is not quite similar to the median density of the stimulated networks
#thus, the 2-d model does not reproduce the observed network.

#GOF diagnostics
par(mfrow = c(1, 2))
plot(gof(dolphins.2d, GOF = ~ degree + dist, nsim = 500)) #simulate 500 networks from the 2-d model, calculate the degree distribution and geodesic distance distribution for each simulated network.
#the estimated model does not fit the observed network well.
#the geodesic distances in the observed network are mostly greater than the simulated networks.
#high density in the estimated model.


###############################
##optional            #########
###############################

#estimate a 2-dimensional latent space model with 3 spherical Gaussian clusters 
dolphins.2d3G <- ergmm(dol ~ euclidean(d = 2, G = 3), verbose = TRUE) #use euclidean(d=2,G=3), we can add a group structure in latent position of vertices
summary(dolphins.2d3G)
mcmc.diagnostics(dolphins.2d3G)

dev.off()
plot(dolphins.2d3G, pie = TRUE)
#after adding the clusters, the BIC is 1158.72.
#model different parameters for different groups so the mean and variance will be cluster-specific, chosen based on the cluster that node is associated with.


#choose the number of clusters based on BIC
#fit a set of candidate models
dolphins.fits <- list(ergmm(dol ~ euclidean(d = 2, G = 1)),
                      ergmm(dol ~ euclidean(d = 2, G = 2)),
                      ergmm(dol ~ euclidean(d = 2, G = 3)),
                      ergmm(dol ~ euclidean(d = 2, G = 4)))

#compute the BICs
BICs <- reshape(as.data.frame(t(sapply(dolphins.fits, function(x)c(G = x$model$G, unlist(bic.ergmm(x))[c("Y","Z","overall")])))),
                list(c("Y","Z","overall")), idvar = "G", v.names = "BIC", timevar = "Component",
                times = c("likelihood","clustering","overall"), direction = "long")
print(BICs)

#plot BIC versus number of clusters
with(BICs, interaction.plot(G, Component, BIC, type = "b", xlab = "Clusters", ylab = "BIC"))

#summarize and plot the fit has the lowest BIC:
bestG <- with(BICs[BICs$Component == "overall", ], G[which.min(BIC)])
summary(dolphins.fits[[bestG]])  #lowest BIC = 1147.20 (G = 2)














