#The dataset for this tutorial was constructed by Meltem Yucel, Gustav R. Sjobeck, Rebecca Glass and Joshua Rottman.
#https://data.mendeley.com/datasets/kpjjvg39k3/4
#The data tracks interactions among students of a small liberal arts college that were members of the rowing team. Subjects reported how often they gossiped about other members of the team, who they "hooked up" with on the team, and who on the team they consider to be friends.
#Participants also reported on "sabotage" behaviors among teammates and their well being and feelings of loneliness. 
#There are 44 participants/cases in the data, there would be 45 but one participant (m13) was excluded.

library(here)

#First we import the data set:
friends <- read.csv(here("class4",
                         "Reciprocity and Transitivity Materials",
                         "Friends_matrix.csv"))

#Step one is to get set up: 
library(network)
library(sna)

#Now lets set up the data as a matrix:
f_matrix <- network(as.matrix(friends))

#Now we can plot our matrix:
plot(f_matrix)

#Now that we have our network plotted, we can start looking into some stats. 
#First lets look at reciprocity: 
grecip(f_matrix)
#What is returned in the console tells us a few things about our network. The first thing ("Mut") tells us that the dyads present in our network are mutual.
#.7579281 is the graph reciprocity value itself. 
#The reciprocity value/coefficient represents the proportion of dyads in a network that are symmetric (nodeA <-> nodeB) out of all dyads in the network. So, in our network about 76% of dyads are symmetric.
#There is pretty high reciprocity in our network (this value is a coefficient), so about 76% of the time someone identifies an individual as a friend, that friendship is reciprocated.
#There is an argument called "measure" (m=x) that one can change to get different values. When you do measure=correlation a correlation measure of reciprocity will be returned (correlation between y_ij and y_ij across all dyads of y and j). 
#When we set measure to correlation it is possible that we will get a negative value, which can be interpreted like a correlation coefficient. 
#Eg:
grecip(f_matrix, measure = "correlation")

#Now we can look at transitivity, which applies to triads in our data:
gtrans(f_matrix)
#Our transitivity score for this data is .6698287. This is also a coefficient, for our data transitivity is rather high as well (about 67%).
#It is important to note that transitivity is relevant to triads (eg. A -> B -> C, so A -> C). Essentially, the transitivity coefficient is how likely nodes are to cluster together.
#The coefficient can also be thought of as the ratio of the observed number of closed triplets to the maximum number of closed triplets possible. 
#So, in our network, about 68% of the triads present are transitive, or "closed".
#Side note: The Transitivity function in sna also has a measure argument (just as reciprocity does). 
#The default (weak) measure is equal to the proportion of triads that have the characteristics described in line 36, however this argument can be changed to obtain different variants of transitivity measures. 

#Extra application: 
#Many times scholars will seek to compare the reciprocity and transitivity of data of interest to that of a random graph. We see this done in the second application article for today. 
#In order to do this lets generate a random graph, it is best if this random graph has the same number of cases as ours does, think of this as a control:
rand <- rgraph(44)
#One graph will not do for comparison, we need to create a baseline collection of random graphs (just how one wouldn't want to have just one individual in a control group):
rand.dist <-rgraph(44, m=100)

#Now lets get the reciprocity and transitivity from our random graphs: 
grecip(rand.dist)
#This returns a big list of all the reciprocity scores for each random graph in our collection.
#To simplify things lets find the average reciprocity score for our collection:
mean(grecip(rand.dist))
#The average reciprocity for our random graphs is .4997674
#Note that the reciprocity for our data (f_matrix) is much higher than that of our random graphs/the baseline.

#Now lets do the same thing for transitivity:
gtrans(rand.dist)
mean(gtrans(rand.dist))
#The mean transitivity for our random graphs is .4989586. This is also much lower than our transitivity for our data (f_matrix)

#Overall we can observe that our network has higher levels of reciprocity and transitivity/clustering than the baselines. 



