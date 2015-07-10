# Author: Divya Mistry
# Date: 7/26/2014
#
# This was written for someone else to help them quickly visualize bipartite
# network. Nodes are expected to have node label that is separate from a unique
# node identifier, and also has a node-type associated with it.
# This code is modified from that original code, and assumes that
# nodes.csv and edges.csv are present in the following format:
#
# ### EDGE FILE FORMAT ###
# source,target
# node1,n1
# node0,n1
# node0,n0
##
# ### NODE FILE FORMAT ###
# node,label,type
# node0,L1,aa
# node1,L2,aa
# n1,K1,bb
# n0,K7,bb

# Plotting Label1-Label2 connected bipartite graph using igraph package
if (!require('igraph')) install.packages('igraph')
require(igraph)

# read the nodes and edges separately. Look at the sample CSV files for the
# format of these files
nodes<-read.csv('SI-files/nodes.csv',header=T)
edges<-read.csv('SI-files/edges.csv',header=T)

# read in the edge list using vertex properties from nodes. Choose the right
# version below based on whether graph is directed or undirected
# gg <- graph.data.frame(d=edges,vertices=nodes, directed=TRUE)
gg <- graph.data.frame(d=edges,vertices=nodes, directed=FALSE)

# differentiate 'aa' and 'bb' node types visually
# An aa node is SQUARE and YELLOW
# A bb node is ROUND and GREEN
V(gg)$shape <- ifelse(V(gg)$type == "aa", "square", "circle")
V(gg)$color <- ifelse(V(gg)$type == "aa", "yellow", "green")

# define a bipartite layout
bplayout = layout.bipartite(gg, types= (V(gg)$type == "aa") )

# plot the graph using the layout
plot.igraph(gg, vertex.size=30, edge.width=2, edge.curved=F, layout=bplayout)
