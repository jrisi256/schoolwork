# The dataset we will be working with today for the centrality tutorial is from Gerhard Van de Bunt (https://www.stats.ox.ac.uk/~snijders/siena/vdBunt_data.htm). 
# The data describe the relationships among a group of freshman in a university over 7 different timepoints. 
# The first 4 timepoints are 3 weeks apart, and the last 3 timeponts are 6 weeks apart. 
# The original group was made up of 49 students but due to drop-outs, there remained 32 students for whom almost complete data are available.
# References for articles using this dataset: 
#Van de Bunt, G.G. 1999. Friends by choice. An actor-oriented statistical network model for friendship networks through time. Amsterdam: Thesis Publishers.
#Van de Bunt, G.G., M.A.J. van Duijn, and T.A.B. Snijders. 1999. Friendship networks through time: An actor-oriented statistical network model. Computational and Mathematical Organization Theory, 5, 167-192.

# First we want to install certain R packages to allow us to build our network.

# don't need to re-install igraph if installed
#install.packages("igraph")

# The file we want to use is a DAT file which is not designed for R. Dr. Desmarais was kind enough to convert and format it into a more readable file that works with R.

library(here)
adjmat <- as.matrix( # converts into matrix
read.table(here("class3", "Rao_centrality_tutorial_9_8_22", "VRND32T6.DAT"),
sep =" ", # fields are separated by spaces
header=F, # no header
)[,-1] # removing first column, since file has column of spaces
)
# need to replace '9' with missing value symbol (NA)
adjmat[adjmat==9] <- NA
vertattr <- read.table(here("class3", "Rao_centrality_tutorial_9_8_22", "VARS.DAT"), sep =" "
)[,-c(1,3,4,5)] # lots of spaces separate fields, removing erroneous columns
names(vertattr) <- c('gender','program','smoking')

# We will now make the ordinal variable of friendship relation ((0 = unknown, 1 = best friend, 2 = friend, 3 = friendly relation, 4 = neutral, 5 = troubled relation, 6 = item non-response, 9 = actor non-response) a binary variable.
# We will recode deeper friendship codes 1 and 2 as 1 which will be 'friend', and less deep friendship codes 3 and 4 as 0 which is 'not friend' to make interpretation more simple.
adjmat[adjmat > 2] <- 0
adjmat[adjmat==1] <- 1
adjmat[adjmat==2] <- 1

# We now have an adjacency matrix, but want to convert this matrix into an igraph which we can work with in R for our centrality calculations.
library(igraph) # I installed igraph but use the 'library' command to load the code and data stored in that package into this active R session.
matrix <- as.matrix(adjmat) # turns into matrix object
g <- graph.adjacency(matrix, mode="directed", weighted=NULL) # Network is directed, set weight as null to create unweighted graph

# We can now look at our data: 
g
plot(g)
plot.igraph(g, edge.arrow.size=0.2) # Adjust arrow head size to make smaller, nodes labeled as vertex (node) number

# We can review our raw data:
View(adjmat)

# We can now look at centrality calculations (degree, betweenness, closeness and Eigenvector centrality). 
degree(g, mode = "all") # We see that person 16 has the highest degree centrality with 10 links (most links (total received and given) in network)
betweenness(g) # We see that person 16 has the highest betweenness score (bridges between nodes in network)- most times this node lay on the shortest path between other nodes, was a bridge between other nodes in network.
closeness(g, mode = "all") # Closeness centrality is low for all participants- so it was a tight network.
evcent(g) #Eigenvector centrality measures links between nodes to other nodes, and additionally how many links their connections have. Person 24 has the highest value.
evcent(g)$vector # This calculation just pulls out the vector of Eigenvector centrality scores (ie the variable) which we will use for further analysis. 

# Another way to look at the data is to store the centrality functions in objects, rather as a print-out. This may be
# preferable if we want to do follow-up analysis of the scores. It also may be important if we are working with a 
# larger network and it takes a while to calculate the scores. 

deg_cent<-degree(g, mode = "all") # Here we are storing the results of the degree() function in an object called deg_cent. 
# This is convenient because with this command we can save the results, add this variable to another dataset, run other functions on the variable, 
# without running the degree() function again.
bet_cent<-betweenness(g)
close_cent<-closeness(g, mode = "all")
ev_cent<-evcent(g)$vector

# We can also visualize the raw data in a histogram:
hist(deg_cent) 
hist(bet_cent)
hist(close_cent)
hist(ev_cent) 

# We can now do some secondary analyses to see, for example, if there are differences in centrality measures by gender which is in our vertattr dataset ((1 = F, 2 = M))
# One way we can visualize these differences easily is to view the data in a boxplot form. 

boxplot(deg_cent~vertattr$gender) # Males (2) had more connections than females (1).
boxplot(bet_cent~vertattr$gender) # Median betweenness centrality measurements for males (2) and females (1) were quite close with a larger range for males.
boxplot(close_cent~vertattr$gender,log="y") # We put this on natural log scale to more easily see the distribution. Males (2) had higher median closeness centrality scores- so were less close/tight with their ties than females (1).
boxplot(ev_cent~vertattr$gender) # Males (2) had higher median Eigenvector centrality scores than females (1), thus males were more connected to more connected people compared to females.
