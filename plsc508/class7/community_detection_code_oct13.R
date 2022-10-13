#Community Detection method tutorial 
#Oct_13_2022

############################## Load library 
library(here)
library(igraph)

#Work through GN algorithm with generated small dataset 
#before application of other algorithms to real data

graph1 <- make_empty_graph(n = 17, directed = FALSE) %>% 
  add_edges(c(1,2,  2,3, 2,4, 2,11, 2,17, 1,11, 16,17, 6,7, 7, 10, 17,12,  3,4,
              4,10, 4,6, 6,10, 6,3, 3,7, 4,7, 9,12, 9,17, 9,8, 9,5, 5,17, 13,14,
              13,15, 13,16, 14,16, 14,15)) %>% 
  set_edge_attr("color", value = "blue")
#plot
plot(graph1,edge.width=0.7)

#There are 17 nodes and 27 edges in the network
#Visually, there are 4 communities in this network



#Girvan and Newman algorithm:
#(1)Calculating betweeness of all edges in the network
#(shortest paths going through an edge.
graph1_edge_betweeness <-edge_betweenness(graph1,e = E(graph1),directed = FALSE)
#View(graph1_edge_betweeness)

#(2) Removing the edge with highest betweeness centrality

#get the edge with highest betweeness centrality
max(graph1_edge_betweeness)
#The edge with highest edge betweeness centrality is between nodes 2,17 (edge#5)

#Remove edge (to seperate possible communities from rest of network so we would detect them)
graph1 <- delete_edges(graph1, get.edge.ids(graph1, c(2,17)))
plot(graph1, edge.width=0.7)

#(3) Re-calculate betweeness of remaining edges in the network
graph1_edge_betweeness <-edge_betweenness(graph1,e = E(graph1),directed = FALSE)
#View(graph1_edge_betweeness)


#(4) Removing the edge with highest betweeness centrality

#get the edge with highest betweeness centrality
max(graph1_edge_betweeness)
#The edge with highest edge betweeness centrality is between nodes 16,17 


#(5) Remove edge (to seperate possible communities from rest of network so we would detect them)
graph1 <- delete_edges(graph1, get.edge.ids(graph1, c(16,17)))
plot(graph1, edge.width=0.7)

#(6) Re-calculate betweeness of remaining edges in the network
graph1_edge_betweeness <-edge_betweenness(graph1,e = E(graph1),directed = FALSE)
#View(graph1_edge_betweeness)


#(7) Removing the edge with highest betweeness centrality

#get the edge with highest betweeness centrality
max(graph1_edge_betweeness)
#The edge with highest edge betweeness centrality is between nodes 2,4


#(8) Remove edge (to seperate possible communities from rest of network so we would detect them)
graph1 <- delete_edges(graph1, get.edge.ids(graph1, c(2,4)))
plot(graph1, edge.width=0.7)

#(9)Re-calculate betweeness of remaining edges in the network
graph1_edge_betweeness <-edge_betweenness(graph1,e = E(graph1),directed = FALSE)
#View(graph1_edge_betweeness)


#(10) Removing the edge with highest betweeness centrality

#get the edge with highest betweeness centrality
max(graph1_edge_betweeness)
#The edge with highest edge betweeness centrality is between nodes 2,3


#(11) Remove edge (to seperate possible communities from rest of network so we would detect them)
graph1 <- delete_edges(graph1, get.edge.ids(graph1, c(2,3)))
plot(graph1, edge.width=0.7)

#Keep going...

#(12)Re-calculate betweeness of remaining edges in the network
graph1_edge_betweeness <-edge_betweenness(graph1,e = E(graph1),directed = FALSE)
#View(graph1_edge_betweeness)


#(10) Removing the edge with highest betweeness centrality

#get the edge with highest betweeness centrality
max(graph1_edge_betweeness)
#The edge with highest edge betweeness centrality is between nodes 9,8


#(11) Remove edge (to seperate possible communities from rest of network so we would detect them)
graph1 <- delete_edges(graph1, get.edge.ids(graph1, c(9,8)))
plot(graph1, edge.width=0.7)


#We can keep going on until there are no edges left, but we can stop before the
#removal of 9,8 edge removal because it was clear that we have communities with
#dense edges within clusters and sparse edges between clusters (other communities). 


################################################################################################


#####Community Detection - Dataset
#Dataset name: "email-Eu-core network"
#Dataset source: Stanford Network Analysis Project website
# (https://snap.stanford.edu/data/email-Eu-core.html)


# -The dataset contains internal email network data from a large
# European research institution.
# -It is directed (has sender/reciever) info regarding emails.
# -There is an edge between i->j if i has sent an email at least one time.
# -The dataset has 1005 Nodes and 25571 Edges.
# -Communities ground truth:
#  "The dataset also contains (ground-truth) community memberships of 
#  the nodes. Each individual belongs to exactly one of 42 departments" 
	



################## Load files 
#Read gzfiles as data.files 
#1.Email communication links between members of the institution
email_network <-
    read.table(gzfile(here("class7", "email-Eu-core.txt.gz")), sep=" ")

#Each row in this network has 2 entries with the Node IDs forming each edge.



#2. Department membership labels
member_dept <-
    read.table(gzfile(here("class7", "email-Eu-core-department-labels.txt.gz")),
               sep=" ")

#The 1st column is NODEID (node-id (a member of the institute))
#The 2nd column is DEPARTMENT membership (id of the member's department ( 0-41))


#Create igraph object from data frames
email_communities <-
    graph_from_data_frame(email_network, directed = T, vertices = member_dept)


#Convert the igraph object to non-directed 
email_communities_undirected <-as.undirected(
  email_communities,
  mode = c( "mutual"),
  edge.attr.comb = igraph_opt("edge.attr.comb"))



#Undirected dataset has 230 components
#View(x<- components(email_communities_undirected)) #returns component membership&size
#View(count<- count_components(email_communities_undirected)) #returns number of components

############################### Community detection algorithm 

#1.Fastgreedy algorthim 
#cluster_fast_greedy() function tries to find dense subgraph (community)
#graphs via directly optimizing a modularity score.
cluster_fastgreedy = cluster_fast_greedy(
  email_communities_undirected, 
  merges = TRUE,
  modularity = TRUE,
  membership = TRUE,
  weights = NULL
)

# number of communities
length(cluster_fastgreedy)
#The fastgreedy algorithm identified 255 communities  

# memberships of nodes (the membership vector of the community structure.)
membership(cluster_fastgreedy)

#Community sizes
sizes_fastgreedy <- sizes(cluster_fastgreedy)
#View community sizes 
head(sizes_fastgreedy)


#Modularity measure
modularity(cluster_fastgreedy)
#The optimized modularity value of the community structure is 0.4077783

#plot communities
plot_fastgreedy <- plot(cluster_fastgreedy, email_communities_undirected, layout=layout_nicely(email_communities, dim = 2))

#plot communities dendogram
plot_dendrogram(
  cluster_fastgreedy,
  mode = "auto",
  palette = categorical_pal(8)
)

#######################2.Infomap##########################

cluster_infomap <-cluster_infomap(
  email_communities_undirected,
  nb.trials = 30,
  modularity = TRUE
)

# number of communities
length(cluster_infomap)
#The algorithm identified 252 communities  

# memberships of nodes
membership(cluster_infomap)

#Community sizes
sizes_infomap <- sizes(cluster_infomap)
#View community sizes 
head(sizes_infomap)


#Modularity measure
modularity(cluster_infomap)
#The optimized modularity value of the community structure is 0.4791394

#plot communities
plot(cluster_infomap, email_communities_undirected, layout=layout_nicely(email_communities, dim = 2))




####################################3. Leading eigenvector #################################### 

cluster_leading_eigen <-cluster_leading_eigen(email_communities_undirected)
 
# number of communities
length(cluster_leading_eigen)
#The  algorithm identified 239 communities  

# memberships of nodes
membership(cluster_leading_eigen)

#Community sizes
sizes_leading_eigen <- sizes(cluster_leading_eigen)
#View community sizes 
head(sizes_leading_eigen)


#Modularity measure
modularity(cluster_leading_eigen)
#The optimized modularity value of the community structure is 0.4091464

#plot communities
plot(cluster_leading_eigen, email_communities_undirected, layout=layout_nicely(email_communities, dim = 2))


#####################################4.Label Propagation
#This is a fast, nearly linear time algorithm for detecting community 
#structure in networks. In works by labeling the vertices with unique 
#labels and then updating the labels by majority voting in the neighborhood
#of the vertex.
cluster_label_prop<- cluster_label_prop(email_communities_undirected)

# number of communities
length(cluster_label_prop)
#The  algorithm identified 257 communities  

# memberships of nodes
membership(cluster_label_prop)

#Community sizes
sizes_leading_eigen <- sizes(cluster_label_prop)
#View community sizes 
head(cluster_label_prop)


#Modularity measure
modularity(cluster_label_prop)
#The optimized modularity value of the community structure is 0.01860538
# Notice the difference between modularity value of this method vs. others. 
# This is because label propogation algorithm does not depend on modularity 
# for community detection. Instead, label propogation assigns an initial label
# for each node randomly then engages in a random ordering process wherein nodes
# update their label to match the most common label of adjacent nodes. It continues
# until every node has the label that matches the maximum number of its neighbors.

# You can still use modularity for cross-algorithm comparison, but label
# propagation may not always do so well.

#plot communities
plot(cluster_label_prop, email_communities_undirected, layout=layout_nicely(email_communities, dim = 2))

###############5.Multilevel optimization of modularity
# based on the modularity measure and a hierarchical approach.
cluster_multilevel <-cluster_louvain(email_communities_undirected)

# number of communities
length(cluster_multilevel)
#The  algorithm identified 238 communities  

# memberships of nodes
membership(cluster_multilevel)

#Community sizes
sizes_leading_eigen <- sizes(cluster_multilevel)
#View community sizes 
head(cluster_multilevel)


#Modularity measure
modularity(cluster_multilevel)
#The optimized modularity value of the community structure is 0.4849411

#plot communities
plot(cluster_multilevel, email_communities_undirected, layout=layout_nicely(email_communities, dim = 2))



##############################6. walktrap

#Community structure via short random walks
#This function tries to find densely connected subgraphs,
#also called communities in a graph via random walks. 
#The idea is that short random walks tend to stay in the same community.
walktrap_clusters <- cluster_walktrap(
  email_communities_undirected,
  merges = TRUE,
  modularity = TRUE,
  membership = TRUE)


# number of communities
length(walktrap_clusters)
#The  algorithm identified 312 communities  

# memberships of nodes
membership(walktrap_clusters)

#Community sizes
sizes_leading_eigen <- sizes(walktrap_clusters)


#Modularity measure
modularity(walktrap_clusters)
#The optimized modularity value of the community structure is 0.4209934

#plot communities
plot(walktrap_clusters, email_communities_undirected, layout=layout_nicely(email_communities, dim = 2))

######################Spinglass
######################Edge betweeness 
#betweeness_cluster<-  cluster_edge_betweenness(email_communities,
#  directed = TRUE,
#  edge.betweenness = TRUE,
#  merges = TRUE,
#  bridges = TRUE,
#  modularity = TRUE,
#  membership = TRUE)


#Spinglass & Edge betweeness take a LONG time...


###########Compare algorithms to assess similarity
print(c <-compare(
  cluster_fastgreedy,
  walktrap_clusters,
  method = c("nmi")))

#'nmi' is the normalized mutual information measure 
#proposed by Danon et al. (2005)
#Value indicates similarity in algorithms' estimation of communities 
#Closer to 1 indicates more similarity 


#fastgreedy&infomap are more similar than fastgreedy&walktrap
print(c <-compare(
  cluster_fastgreedy,
  cluster_infomap,
  method = c("nmi")))


#fastgreedy&previous 2 are more similar to each other than fastgreedy&label_propogation 
print(c <-compare(
  cluster_fastgreedy,
  cluster_label_prop,
  method = c("nmi")))


###################### Verification of communities & ground truths ##############
# We can analyze the overlap between the detected communities and the actual 
# departments (the ground truth) that each actor belongs to. 

#We will compare the department membership communities & 2 different community
# detection algorithms (fastgreedy and label propogation)

#Declare a numeric vector as a membership vector
dept_membership <- as_membership(member_dept$V2)

#Create a cluster based on dept membership
dept_cluster <- make_clusters(
  email_communities_undirected,
  membership = dept_membership,
  algorithm = NULL,
  merges = NULL,
  modularity = FALSE
)



#Plot communities
#The first plot is the one we plotted for Fastgreedy algorithm
plot(cluster_fastgreedy, email_communities_undirected, layout=layout_nicely(email_communities, dim = 2), reaks=30 , border=F , col=rgb(0.1,0.8,0.3,0.5) , xlab="Fastgreedy Algorithm" , main="")

#The second plot is based on department membership
plot(dept_cluster, email_communities_undirected, layout=layout_nicely(email_communities, dim = 2), reaks=30 , border=F , col=rgb(0.1,0.8,0.3,0.5) , xlab="Department Membership" , main="")
# Plotting dept membership results in a differently shaded networks 


# Compare whether the department membership corresponds to communities membership 
print(c <-compare(
  cluster_fastgreedy,
  dept_cluster,
  method = c("nmi")))

# The result (0.5036467) confirms that there is low overlap between 
# communities detected by the algorithms and communities based 
# on department membership (which represents the ground truth of 
# communities). However, this does mean that algorithms are not 
# accurate. For example, if the focus of the research communities
# is multidisciplinary research, then a community that does not
# resemble department membership makes sense. 


#Comparison of dept membership with label propagation

#The third plot is based on department membership
plot(cluster_label_prop, email_communities_undirected, layout=layout_nicely(email_communities, dim = 2), reaks=30 , border=F , col=rgb(0.1,0.8,0.3,0.5) , xlab="Label propogation" , main="")
# Plotting dept membership results in a differently shaded networks 


# Compare whether the department membership corresponds to communities membership 
print(c <-compare(
  cluster_label_prop,
  dept_cluster,
  method = c("nmi")))

# Result (0.3233396) indicates an even lower similarity between community detecting using label
# propagation & department membership 