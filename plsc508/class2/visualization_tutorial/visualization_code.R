# data for the first example comes from http://networkdata.ics.uci.edu/netdata/html/krackHighTech.html. 
# It is a famous network dataset of 21 managers at a high tech firm that includes networks of advice, where edge i->j indicates
# that i reported getting advice from j, and reporting structure where i->j indicates that
# i reports to j. There is also attribute data that includes Age, Tenure at the company, level in the 
# org chart, and a department indicator. This was published in 

# Krackhardt, David. Assessing the Political Landscape: Structure, Cognition, and Power in Organizations.
# Administrative Science Quarterly Vol. 35, No. 2 (Jun., 1990), pp. 342-369


# Read in adjacency matrices
## read.csv creates a data frame object from a CSV file
## Need to indicate that there's no header row in the CSV
library(here)
advice <- read.csv(here("class2", "visualization_tutorial", "Advice.csv"), header=F)

reportsto <- read.csv(here("class2", "visualization_tutorial", "ReportsTo.csv"), header = F)

# Read in vertex attribute data
attributes <- read.csv(here("class2", "visualization_tutorial", "KrackhardtVLD.csv"))
# Read in the library for network analysis
library(network)
adviceNet <- network(as.matrix(advice))

# Add the vertex attributes into the network
set.vertex.attribute(adviceNet,names(attributes),attributes)

# Add the organizational chart as a network variable
set.network.attribute(adviceNet,"reportsto",reportsto)

# Simple plot
## Set random number seed so the plot is replicable
set.seed(5)
## Plot the network
plot(adviceNet,
     displaylabels = T,
     label = get.vertex.attribute(adviceNet, "Level"),
     vertex.cex = 2, label.cex = 1,
     edge.col = rgb(150, 150, 150, 100, maxColorValue = 255),
     label.pos = 5,
     vertex.col = "lightblue")

# check out all the options with ?plot.network
# Creating Network Objects: Defense Pacts (edgelist) (2000)
# gathered from the MIDs project
# published in 

# Cranmer, Skyler J., Bruce A. Desmarais, and Justin H. Kirkland. "Toward a network theory of alliance formation." 
# International Interactions 38, no. 3 (2012): 295-324.

# Read in vertex dataset
allyV <- read.csv(here("class2", "visualization_tutorial", "allyVLD.csv"),stringsAsFactors=F)

# Read in edgelist
allyEL <- read.csv(here("class2", "visualization_tutorial", "allyEL.csv"), stringsAsFactors=F)

# Read in contiguity
contig <- read.csv(here("class2", "visualization_tutorial", "contiguity.csv"), stringsAsFactors=F,row.names=1)

require(network)
# (1) Initialize network
# store number of vertices
n <- nrow(allyV)
AllyNet <- network.initialize(n,dir=F)

# (2) Set vertex labels
network.vertex.names(AllyNet)  <- allyV$stateabb

# (3) Add in the edges
# Note, edgelist must match vertex labels
AllyNet[as.matrix(allyEL)]  <- 1

# (4) Store country code attribute
set.vertex.attribute(x=AllyNet,             # Network in which to store
                     "ccode",            # What to name the attribute
                     allyV$ccode)            # Values to put in

# (5) Store year attribute
set.vertex.attribute(AllyNet,"created",allyV$styear)

# (6) Store network attribute
set.network.attribute(AllyNet,"contiguous",as.matrix(contig))

# Simple plot
set.seed(5)
plot(AllyNet,displaylabels=T,label.cex=.5,edge.col=rgb(150,150,150,100,maxColorValue=255))

# Better way to do this? Cannot represent vertices with no edges!
net3 <- network(allyEL, vertex.attr = allyV, matrix.type = "edgelist", loops = T, multiple = F, ignore.eval = F,
                directed = T)
plot(net3, displaylabels = T, label.cex=.5,edge.col=rgb(150,150,150,100,maxColorValue=255))

# visualization using ggplot compatible grammar
library(GGally)

# need to make character attribute to generate legend
set.vertex.attribute(adviceNet,"cLevel",as.character(get.vertex.attribute(adviceNet,'Level')))

set.seed(5)
ggnet2(adviceNet, color = "cLevel", legend.size = 12,
       legend.position = "bottom",arrow.size=14,color.legend="Level",
       color.palette = "Dark2")



