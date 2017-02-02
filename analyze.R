# MS&E382 Winter 2017
# Lab 1


######################################################################################
#
# Import the necessary libraries
#
######################################################################################
# install.packages('magrittr', repos = "http://cran.us.r-project.org")
# install.packages('igraph', repos = "http://cran.us.r-project.org")
# install.packages('httr', repos = "http://cran.us.r-project.org")
# install.packages('data.table', repos = "http://cran.us.r-project.org")
library(magrittr)
library(httr)
library(data.table)
library(igraph)
# Set your directory for the project
setwd("/Users/Yaqian/Desktop/Lab1")
# Load the data
load(file='network.RData')

### Dichotomize values in the network

# Mean cut
network_table$mean.cut <- ifelse(network_table$CoOccurrences >= mean(network_table$CoOccurrences),yes = 1,no = 0)

# Quartile cuts - get the 25% percentile, 50% percentile, and 75% percentile
quartiles <- quantile(network_table$CoOccurrences)

network_table$per25.cut <- ifelse(network_table$CoOccurrences >= quartiles[2],yes = 1,no = 0)

network_table$per50.cut <- ifelse(network_table$CoOccurrences >= quartiles[3],yes = 1,no = 0)

network_table$per75.cut <- ifelse(network_table$CoOccurrences >= quartiles[4],yes = 1,no = 0)

# Create a graph
# Data is stored in a data frame

g_valued <- graph_from_data_frame(d = network_table[,1:3,with=FALSE],directed = FALSE,vertices = total_table)

g_mean  <- graph_from_data_frame(d = network_table[mean.cut==1][,c(1,2),with=FALSE],directed = FALSE,vertices = total_table)

g_25per <- graph_from_data_frame(d = network_table[per25.cut==1][,c(1,2),with=FALSE],directed = FALSE,vertices = total_table)

g_50per <- graph_from_data_frame(d = network_table[per50.cut==1][,c(1,2),with=FALSE],directed = FALSE,vertices = total_table)

g_75per <- graph_from_data_frame(d = network_table[per75.cut==1][,c(1,2),with=FALSE],directed = FALSE,vertices = total_table)


# Count the number of nodes in all of your graphs
numNode_valued <- vcount(g_valued)
numNode_mean <- vcount(g_mean)
numNode_25per <- vcount(g_25per)
numNode_50per <- vcount(g_50per)
numNode_75per <- vcount(g_75per)
numNode <- data.frame(numNode_valued,numNode_mean,numNode_25per,numNode_50per,numNode_75per)
print(numNode)

# Count the number of edges in all of your graphs
numEdges_valued <- ecount(g_valued)
numEdges_mean <- ecount(g_mean)
numEdges_25per <- ecount(g_25per)
numEdges_50per <- ecount(g_50per)
numEdges_75per <- ecount(g_75per)
numEdge <- data.frame(numEdges_valued,numEdges_mean,numEdges_25per,numEdges_50per,numEdges_75per)
print(numEdge)

# What is the density of your networks? 
graphDensity1 <- graph.density(g_valued)
graphDensity2 <- graph.density(g_mean)
graphDensity3 <- graph.density(g_25per)
graphDensity4 <- graph.density(g_50per)
graphDensity5 <- graph.density(g_75per)
graphDensity <- data.frame(graphDensity1,graphDensity2,graphDensity3,graphDensity4,graphDensity5)
print(graphDensity)

## Sanity check - do these values equal 2 * numEdges / (numVertices * (numVertices-1)) ?? numEdges / (numVertices * (numVertices-1) / 2 )

######################################################################################
#
# Part II: Visualize your networks
#
######################################################################################

colbar = rainbow(length(word_list)) ## we are selecting different colors to correspond to each 
V(g_valued)$color = colbar
V(g_mean)$color = colbar
V(g_25per)$color = colbar
V(g_50per)$color = colbar
V(g_75per)$color = colbar

# Set layout here 
L = layout_with_fr(g_valued)  # Fruchterman Reingold
L.dh = layout_with_dh(g_valued) ## Davidson and Harel
# L.drl = layout_with_drl(g_valued) ## Force-directed
# L.kk = layout_with_kk(g_valued) ## Spring

# Plot graph
# no lable: vertex.label=NA
plot(g_valued,vertex.color=V(g_valued)$color, layout = L, vertex.label.dist=0.5,vertex.size=6, main='valued')

plot(g_mean,vertex.color=V(g_mean)$color, layout = L, vertex.label.dist=0.5,vertex.size=6, main='mean')

plot(g_25per,vertex.color=V(g_25per)$color, layout = L, vertex.label.dist=0.5,vertex.size=6, main='25 per')

plot(g_50per,vertex.color=V(g_50per)$color, layout = L, vertex.label.dist=0.5,vertex.size=6, main='50 per')

plot(g_75per,vertex.color=V(g_75per)$color, layout = L, vertex.label.dist=0.5,vertex.size=6, main='75 per')

plot(g_25per,vertex.color=V(g_mean)$color, layout = L.dh, vertex.label=NA,vertex.size=6, main='25 per (Davidson and Harel layout)')



######################################################################################
#
# Part III: Visualize communities in the networks
#
######################################################################################

# Plot the number of clusters in the graph and their size
# there are also other algorithms for this you may want to explore
cluster1 <- cluster_walktrap(g_valued)
cluster2 <- cluster_walktrap(g_mean)
cluster3 <- cluster_walktrap(g_25per)
cluster4 <- cluster_walktrap(g_50per)
cluster5 <- cluster_walktrap(g_75per)

# Find the number of clusters
membership(cluster1)   # affiliation list
length(sizes(cluster1)) # number of clusters

membership(cluster2)   # affiliation list
length(sizes(cluster2)) # number of clusters

membership(cluster3)   # affiliation list
length(sizes(cluster3)) # number of clusters

membership(cluster4)   # affiliation list
length(sizes(cluster4)) # number of clusters

membership(cluster5)   # affiliation list
length(sizes(cluster5)) # number of clusters

# Find the size the each cluster 
# Note that communities with one node are isolates, or have only a single tie
sizes(cluster1)
sizes(cluster2)
sizes(cluster3)
sizes(cluster4)
sizes(cluster5)


# Visualize clusters - this puts colored circles around the nodes in a community
plot(cluster1, g_valued, col = V(g_valued)$color, layout = L, vertex.label.dist=0.5, vertex.size=6, main='valued clusters')
plot(cluster2, g_mean, col = V(g_mean)$color, layout = L, vertex.label.dist=0.5, vertex.size=6, main='mean clusters')
plot(cluster3, g_25per, col = V(g_25per)$color, layout = L, vertex.label.dist=0.5, vertex.size=6, main='25 per clusters')
plot(cluster4, g_50per, col = V(g_50per)$color, layout = L, vertex.label.dist=0.5, vertex.size=6, main='50 per clusters')
plot(cluster5, g_75per, col = V(g_75per)$color, layout = L, vertex.label.dist=0.5, vertex.size=6, main='75 per clusters')



# Centrality calculations
# ---------------------------------------

# Compute centralities
totalDegree <- degree(g_25per,mode="all")
sort(totalDegree,decreasing=TRUE)[1:5]

b <- betweenness(g_25per,directed=TRUE)
sort(b,decreasing=TRUE)[1:5]

c <- closeness(g_25per)
sort(c,decreasing=TRUE)[1:5]

eigc <- eigen_centrality(g_25per,directed=TRUE)
sort(eigc$vector,decreasing=TRUE)[1:5]

eigc <- eigen_centrality(g_25per,directed=TRUE)
sort(eigc$vector,decreasing=TRUE)[1:5]

# For undirected matrices the adjacency matrix is symmetric and the hub scores are the same as authority scores,
a <- authority_score(g_25per, scale = TRUE)
sort(a$vector,decreasing=TRUE)[1:5]

# For undirected matrices the adjacency matrix is symmetric and the hub scores are the same as authority scores,
u <- hub_score(g_25per, scale = TRUE)
sort(u$vector,decreasing=TRUE)[1:5]


# Centrality Visualization
# ---------------------------------------

## Plot based on the centrality
g2 <- g_25per
V(g2)$size <- totalDegree*5 #can adjust the number
plot(g2, layout = L, vertex.label=NA,main='25 per')

# What is the betweenness centrality score for each vertex? 
g4 <- g_25per
V(g4)$size <- b*5  #can adjust the number
plot(g4, layout = L, vertex.label=NA,main='25 per betweenness centrality')

# What is the closeness centrality score for each vertex? 
g5 <- g_25per
V(g5)$size <- c*500  #can adjust the number
plot(g5, layout = L, vertex.label=NA,main='25 per closeness centrality')

# What is the eigenvector centrality score for each vertex? 
g6 <- g_25per
V(g6)$size <- eigc$vector*5 #can adjust the number
plot(g6, layout = L, vertex.label=NA,main='25 per eigenvector centrality')

# What is the authority score for each vertex? 
g7 <- g_25per
V(g7)$size <- a$vector*5 #can adjust the number
plot(g7, layout = L, vertex.label=NA,main='25 per authority score')

# What is the hub score for each vertex? 
g8 <- g_25per
V(g8)$size <- u$vector*5 # can adjust the number
plot(g8, layout = L, vertex.label=NA,main='25 per hub score')


######################################################################################
#
# Part IV: Valued Network Centrality and Centralization
#
######################################################################################

# Weighted degree centrality
graph.strength(g_valued,weights = E(g_valued)$CoOccurrences)

# Weighted betweenness centrality, normalized
betweenness(g_valued,directed = FALSE,weights = E(g_valued)$CoOccurrences,normalized = TRUE)

# Weighted closeness centrality, normalized
closeness(g_valued,mode = "all",weights = E(g_valued)$CoOccurrences,normalized = TRUE)

# Weighted eigenvector centrality, normalized
evcent(g_valued,directed = FALSE,weights = E(g_valued)$CoOccurrences)

# Degree centralization of split graphs
degree2 <- centralization.degree(g_mean,normalized = TRUE)
degree3 <- centralization.degree(g_25per,normalized = TRUE)
degree4 <- centralization.degree(g_50per,normalized = TRUE)
degree5 <- centralization.degree(g_75per,normalized = TRUE)
data.frame(degree2,degree3,degree4,degree5)

# Betweenness centralization of split graphs
between2 <- centralization.betweenness(g_mean,normalized = TRUE)
between3 <- centralization.betweenness(g_25per,normalized = TRUE)
between4 <- centralization.betweenness(g_50per,normalized = TRUE)
between5 <- centralization.betweenness(g_75per,normalized = TRUE)
data.frame(between2,between3,between4,between5)

# Closeness centralization of split graphs
close2 <- centralization.closeness(g_mean,normalized = TRUE)
close3 <- centralization.closeness(g_25per,normalized = TRUE)
close4 <- centralization.closeness(g_50per,normalized = TRUE)
close5 <- centralization.closeness(g_75per,normalized = TRUE)
data.frame(close2,close3,close4,close5)

# Eigenvector centralization of split graphs
eigen2 <- centralization.evcent(g_mean,normalized = TRUE)
eigen3 <- centralization.evcent(g_25per,normalized = TRUE)
eigen4 <- centralization.evcent(g_50per,normalized = TRUE)
eigen5 <- centralization.evcent(g_75per,normalized = TRUE)
data.frame(eigen2,eigen3,eigen4,eigen5)

# Calculate degree distribution
deg <- degree(g_mean,v=V(g_mean), mode="all")

# Degree distribution is the cumulative frequency of nodes with a given degree
deg_distr <-degree.distribution(g_mean, cumulative=T, mode="all")

# Fit a power law to the degree distribution
# The output of the power.law.fit() function tells us what the exponent of the power law is ($alpha)
# and the log-likelihood of the parameters used to fit the power law distribution ($logLik)
# Also, it performs a Kolmogov-Smirnov test to test whether the given degree distribution could have
# been drawn from the fitted power law distribution.
# The function thus gives us the test statistic ($KS.stat) and p-vaule ($KS.p) for that test
power <- power.law.fit(deg_distr)
power

# Plot the degree distribution raw data
plot(deg_distr, ylim=c(.01,10), bg="black",pch=21, xlab="Degree", ylab="Cumulative Frequency",main='degree distribution')
# log-log plot
plot(deg_distr, log="xy", ylim=c(.01,10), bg="black",pch=21, xlab="Degree (log)", ylab="Cumulative Frequency (log)", type='l', main='degree distribution')


# Average clustering coefficient (ACC)
transitivity(g_mean, type = c("average"))

# Characteristic path length (CPL)
average.path.length(g_mean)

# Generate 1 random networks & compute ACC & CPL
g <- erdos.renyi.game(500, 350, type = "gnm")
transitivity(g, type = c("average"))
average.path.length(g)

acc <- c()
cpl <- c()
# Generate 100 random graphs & report the average ACC & CPL
for(i in 1:100){
  g <- erdos.renyi.game(500, 350, type = "gnm")
  cpl <- c(cpl, average.path.length(g))
  acc <- c(acc, transitivity(g, type = c("average")))
}
print(paste('Average clustering coefficient:', mean(acc)))
print(paste('Average clustering coefficient:', mean(cpl)))
