#The data for this tutorial was compiled by Evelina Gabasova. It contains the social network of Star Wars characters extracted from movie scripts (episode I to VII). 
#ties connect characters if they speak together within the same scene.  
#citation: Gabasova, E. (2016). Star Wars social network. DOI: https://doi.org/10.5281/zenodo.1411479.

#load the required libraries
library(here)
library(igraph)
library(network)

#set the seed
set.seed(5)

#First, we are going to generate a scale free sample graph and it's distribution according to the Barabasi & Albert model
g <- sample_pa(10000)
d <- degree(g, mode = "in")

#create a histogram of the distributions
hist(d)
#This histogram shows that the distribution is not normal and instead follows a power law distribution

#fitting a power law distribution to our sample dataset
# d + 1 is to capture isolates
fit1 <- fit_power_law(d + 1, 1)
fit2 <- fit_power_law(d + 1, 1, implementation = "R.mle")

#Calculating coefficients
fit1$alpha
stats4::coef(fit2)

#alpha is the the exponent of the fitted power-law distribution.
#We get an alpha value of 2.3 for the exponent, which is within the parameters presented by Barabasi and Albert generated in our paper (Y should be between 2.1 and 4) 
#If an alpha falls within that range, you can conclude that it came from a preferential attachment model


#Testing the preferential attachment model with a real life data set
#Import our data
starwars.links <- read.csv(here("class5", "final-starwars-links.csv"),
                           header = T)

#Visualize the data
#Thin the network to make it more readable. This will limit the data to only ties of 20 or more scenes
thinned_links <- subset(starwars.links, scenes > 19)

#import the attributes
attributes <- read.csv(here("class5", "starwars-characters.csv"),
                       header = T)

#Create network object
thinned.net <- network(thinned_links)

#Add link attributes
set.vertex.attribute(thinned.net, names(attributes), attributes)

#Plot network
plot(thinned.net,
     displaylabels = T,
     label = get.vertex.attribute(thinned.net,"name"),
     vertex.cex = 2,
     label.cex = 0.5,
     label.pos = 5,
     vertex.col = "blue")


#Use the BA model on our real life data set
#note: First we will use the full dataset, not the trimmed data
newg <- graph_from_data_frame(starwars.links)
newd <- degree(newg, mode = "in")
hist(newd)
#gives us a somewhat similar looking histogram as our sample distribution, although it is not perfect


#fit the power law distribution on this new data
fit3 <- fit_power_law(newd + 1, 1)
fit4 <- fit_power_law(newd + 1, 1, implementation="R.mle")

#Calculate the coefficients
fit3$alpha
stats4::coef(fit4)
#We get one alpha of 1.5 with this full data set, which is lower then the BA model alpha.
# We can conclude that it does not come from a preferential treatment model

#Now, let's test this with a smaller sample of the data
#Distribution
newg2 <- graph_from_data_frame(thinned_links)
newd2 <- degree(newg2, mode = "in")
hist(newd2)

#Fitting the power law model
fit5 <- fit_power_law(newd2 + 1, 1)
fit6 <- fit_power_law(newd2 + 1, 1, implementation="R.mle")

#Calculate coefficients
fit5$alpha
stats4::coef(fit6)

#1.7 is still not preferential attachment even with a thinned out network
#However, the alpha got closer to the BA range
#This shows how the way you measure your ties might change the conclusions you can make.
#If you focus on stronger ties by thinning the network, you might see more of a preferential attachment
#If you focus on weaker ties (don't thin the network), you might not see that preferential attachment as much. 
