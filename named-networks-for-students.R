# Author: Divya Mistry
# Date: 1/23/2014
# 

library(igraph)

num_nodes <- 100
max_num_edges <- (num_nodes)*(num_nodes - 1)/2
min_num_edges <- num_nodes - 1

# Random graph, no structure.
# igraph implements the G(n,m) model under erdos-renyi graphs.
# Here I generate a random network with 100 nodes, and number of edges 
# between 99 and 4950 chosen uniformly randomly
random_num_edges <- 3934 #floor(runif(1, min=min_num_edges, max=max_num_edges))
gnm_random_graph_undirected <- erdos.renyi.game(n=num_nodes, type = 'gnm',
                                     p.or.m=random_num_edges, directed=FALSE)
random_edge_weights <- as.numeric(format(runif(ecount(gnm_random_graph_undirected)),digits=2))
edge_list<-data.frame(get.edgelist(gnm_random_graph_undirected),random_edge_weights,row.names=NULL)
colnames(edge_list)<-c('A','B','Wt')
write.csv(edge_list, file='gnm_random_graph_undirected.csv', row.names=FALSE)

# Erdos-Renyi. Random graph, specific edge probability. I uniformly choose probability of edge.
random_edge_probab <- 0.1922 #runif(1)
gnp_random_graph_undirected <- erdos.renyi.game(n=num_nodes, type='gnp',
                                                p.or.m=random_edge_probab, directed=FALSE)
random_edge_weights <- as.numeric(format(runif(ecount(gnp_random_graph_undirected)),digits=2))
edge_list<-data.frame(get.edgelist(gnp_random_graph_undirected),random_edge_weights,row.names=NULL)
colnames(edge_list)<-c('A','B','Wt')
write.csv(edge_list, file='gnp_random_graph_undirected.csv', row.names=FALSE)

# Small-world Watts-Strogatz graph
# I choose between 1 and 10 neighbors, and random probablity of rewiring
num_of_neighbors <- 3 #floor(runif(1, min=1, max=10))
rewiring_prob <- 0.346 #runif(1)
ws_random_graph <- watts.strogatz.game(dim=1, size=num_nodes, nei=num_of_neighbors, p=rewiring_prob)
random_edge_weights <- as.numeric(format(runif(ecount(ws_random_graph)), digits=2))
edge_list<-data.frame(get.edgelist(ws_random_graph),random_edge_weights, row.names=NULL)
colnames(edge_list)<-c('A','B','Wt')
write.csv(edge_list, file='ws_random_graph.csv', row.names=FALSE)

# Scale-free Barabasi-Albert graph
ba_random_graph_undirected <- barabasi.game(n=num_nodes,directed=FALSE)
random_edge_weights <- as.numeric(format(runif(ecount(ba_random_graph_undirected)),digits=2))
edge_list<-data.frame(get.edgelist(ba_random_graph_undirected), random_edge_weights, row.names=NULL)
colnames(edge_list)<-c('A','B','Wt')
write.csv(edge_list, file='ba_random_graph_undirected.csv', row.names=FALSE)

# Heirarchical graph
# We fit two small Barabasi-Albert graphs for the heirarchy
graph_to_fit <- barabasi.game(n=num_nodes/2,directed=FALSE) + barabasi.game(n=num_nodes/2,directed=FALSE)
hr_random_graph_fit <- hrg.fit(graph_to_fit)
hr_random_graph <- hrg.dendrogram(hr_random_graph_fit)
random_edge_weights <- as.numeric(format(runif(ecount(hr_random_graph)),digits=2))
edge_list<-data.frame(get.edgelist(hr_random_graph), random_edge_weights, row.names=NULL)
colnames(edge_list)<-c('A','B','Wt')
write.csv(edge_list, file='hr_random_graph.csv', row.names=FALSE)

# Forest Fire graph
fw_burning <- 0.2918 #runif(1)
ff_random_graph_undirected <- forest.fire.game(nodes=num_nodes, fw.prob=fw_burning, directed=FALSE)
random_edge_weights <- as.numeric(format(runif(ecount(ff_random_graph_undirected)),digits=2))
edge_list<-data.frame(get.edgelist(ff_random_graph_undirected), random_edge_weights, row.names=NULL)
colnames(edge_list)<-c('A','B','Wt')
write.csv(edge_list, file='ff_random_graph_undirected.csv', row.names=FALSE)
