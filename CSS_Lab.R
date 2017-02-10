
# Install the NetData package & SNA package 
# Note: you only need to install packages ONCE
# install.packages('NetData',lib="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
# install.packages('sna', lib="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
# install.packages('intergraph', lib="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

# Load libraries
# Note: need to load these packages every time
library('intergraph')
library('igraph')
library('network')
library('sna')
library('NetData')

# Make sure you've set your directory to the folder where the "krackhardt_css_data.RData" is saved
# Load the Krackhardt advice/friendship data
# For more info see: https://cran.r-project.org/web/packages/NetData/NetData.pdf
load('krackhardt_css_data.RData')

# ----------------------------------------------------------------------------------------------------
# PART I: Make aggregated networks
#
# These networks combine respondent self report data in different ways
# Read each line of code and try to understand how it is combining the individual networks
# into a single network (e.g. by taking the the union, intersection, etc.)
# ----------------------------------------------------------------------------------------------------

# Read the consensus function documentation so you understand what the code below is doing
??consensus 

# Make single networks from the friendship responses
fr_column <- consensus(friendship_nets, mode="digraph", diag=FALSE, method="OR.col")
fr_row    <- consensus(friendship_nets, mode="digraph", diag=FALSE, method="OR.row")
fr_intersection <- consensus(friendship_nets, mode="digraph", diag=FALSE, method="LAS.intersection")
fr_union  <- consensus(friendship_nets, mode="digraph", diag=FALSE, method="LAS.union")
fr_median <- consensus(friendship_nets, mode="digraph", diag=FALSE, method="central.graph")

# Make single networks from the advice responses
ad_column <- consensus(advice_nets, mode="digraph", diag=FALSE, method="OR.col")
ad_row    <- consensus(advice_nets, mode="digraph", diag=FALSE, method="OR.row")
ad_intersection <- consensus(advice_nets, mode="digraph", diag=FALSE, method="LAS.intersection")
ad_union  <- consensus(advice_nets, mode="digraph", diag=FALSE, method="LAS.union")
ad_median <- consensus(advice_nets, mode="digraph", diag=FALSE, method="central.graph")

# ----------------------------------------------------------------------------------------------------
# PART II: Visualization and Inspection
#
# TIP: Use the arrows in the Plots tab to navigate the plots you generate.
# ----------------------------------------------------------------------------------------------------
# Remove the 'sna' and 'network' packages from your environment. 
# They share some function names with igraph so doing this is advised
detach(package:sna)
detach(package:network)
library('igraph')
library('intergraph')

# Plot a single aggregated network
net_layout   <- layout.fruchterman.reingold(fr_union_net) # this makes a spring embedded layout
net_layout2  <- layout.circle(fr_union_net)               # this makes a circle layout
#plot1
fr_union_net <- graph.adjacency(fr_union)                 # make an igraph network object from the friend union adjacency matrix
plot(fr_union_net, layout=net_layout, edge.color='darkorchid1',edge.arrow.size=0.1,main="Frienship Union Network")  # plots the friendship union network, look at the 'Plots' tab
#plot2
fr_intersection_net <- graph.adjacency(fr_intersection)        
plot(fr_intersection_net, layout=net_layout, edge.color='coral',edge.arrow.size=0.1,main="Frienship Intersection Network") 
#plot3
ad_union_net <- graph.adjacency(ad_union)                 
plot(ad_union_net, layout=net_layout, edge.color='pink',edge.arrow.size=0.1,main="Advice Union Network") 
#plot4
ad_intersection_net <- graph.adjacency(ad_intersection)        
plot(ad_intersection_net, layout=net_layout, edge.color='red',edge.arrow.size=0.1,main="Advice Intersection Network") 

# Plot a single user's self reported network
# Plot5
respondent_1_adnet <- asIgraph(advice_nets[[1]]) # this makes an igraph network of the respondent's network from the indexed # in the [[]]
plot(respondent_1_adnet, layout=net_layout, edge.color='orange', edge.arrow.size=0.1,main="respondent 1's self reported advice network")      # plots respondent 1's self reported advice network
# Plot6
respondent_1_frnet <- asIgraph(friendship_nets[[1]])
plot(respondent_1_frnet, layout=net_layout, edge.color='blue', edge.arrow.size=0.1, main="respondent 1's self reported friendship network")  

# Plot the intersection of two networks
ad_union_net <- graph.adjacency(ad_union)                           # makes a network object from the union of the advice networks
union_intersection <- graph.intersection(fr_union_net,ad_union_net) # this makes an igraph network object from two matrices
plot(union_intersection, layout=net_layout, edge.color='green',edge.arrow.size=0.1, main="intersection of the fr_union and ad_union networks")     # plot the intersection of the friendship & advice union networks


# ----------------------------------------------------------------------------------------------------
# PART III: STRUCTURAL EQUIVALENCE
# ----------------------------------------------------------------------------------------------------
detach(package:igraph) 
library('sna')

# ----------------------------------------------------------------------------------------------------
# PART IV: DIFFERENCES & CORRELATION
# ----------------------------------------------------------------------------------------------------
??qaptest                # read the documentation on qaptest
??plot.qaptest           # read the documentation to help you interpret the qaptest plot. 

# Compare each respondent's advice network with the median network
ad_median_net <- network(ad_median, directed=TRUE)      # create a median advice network

correlations <- c()           # initialize empty vector to store correlations
count = 0                     # initialize a count object so we can track which network we're on in the below loop
for (i in advice_nets){       # loop over each advice network and analyze individually; set i=advice_nets[[1]] to see what's going on for 1 individual
  count = count + 1           
  print(count)                
  ad_qap <- qaptest(list(i,ad_median_net),gcor,g1=1,g2=2,reps=1000)   # use QAP to determine significance
  s <- summary(ad_qap)                                  # summary of current individual's QAP test.
  print(s)                                             
  correlations <- c(correlations, s$testval)            # save the correlation for individual i
}

plot.qaptest(ad_qap)     # plots qaptest
                         # Note this is just the plot for the final fr_ad_qap from the above loop!

message(paste("Proportion of draws which were >= observed value:", ad_qap$pgreq))
message(paste("Proportion of draws which were <= observed value:", ad_qap$pleeq))



# Compare each respondent's friendship network with the median network
fr_median_net <- network(fr_median, directed=TRUE)   # create a median friendship network to compare against

count = 0                         # initialize a count object so we can track which network we're on in the below loop
for (i in friendship_nets){       # loop over each friendship network and analyze individually
  count = count + 1               
  print(count)                    
  fr_qap <- qaptest(list(i,fr_median_net),gcor,g1=1,g2=2,reps=1000)    # use QAP to determine significance
  print(summary(fr_qap))                                               # prints a summary of one individual's qaptest
}

plot.qaptest(fr_qap) 
message(paste("Proportion of draws which were >= observed value:", fr_qap$pgreq))
message(paste("Proportion of draws which were <= observed value:", fr_qap$pleeq))

# Compare each respondent's friendship network with their advice network
for (i in seq(1,21)){
  print(i)                                             
  fr_ad_qap <- qaptest(list(friendship_nets[[i]],advice_nets[[i]]),gcor,g1=1,g2=2,reps=1000) # does a qap test comparing that individual's friendship & advice networks
  print(summary(fr_ad_qap))                                                            
}

plot.qaptest(fr_ad_qap)  # plots qaptest
message(paste("Proportion of draws which were >= observed value:", fr_ad_qap$pgreq))
message(paste("Proportion of draws which were <= observed value:", fr_ad_qap$pleeq))


# Calculate centralities for the advice union consensus network
detach(package:sna) 
library('igraph')
g.adj <-graph.adjacency(ad_union) 
d <- degree(g.adj)
b <- betweenness(g.adj,directed=TRUE)
c <- closeness(g.adj)
e <- eigen_centrality(g.adj,directed=TRUE)$vector

# Calculate the linear correlation between each centrality measure and the accuracy in predicting the median network
cor(correlations, d, method="kendall") 
cor(correlations, b, method="kendall") 
cor(correlations, c, method="kendall") 
cor(correlations, e, method="kendall") 

# Optional: repeat the same procedure for the frienship network. 
# Note: you must first adapt the code for the advice network to store the correlatoins


