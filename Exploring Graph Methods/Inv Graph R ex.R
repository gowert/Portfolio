library(graph)

source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
biocLite("RBGL")
biocLite("gRbase")

library(RBGL)

# Set up Graph
inv_edges = read.csv("GitHub/capstone/Graphing/weighted_investor_edges.csv")
inv_edges
rel_data = cbind(inv_edges[1], inv_edges[2])
w_data = cbind(inv_edges[3])

# Requires library(graph)
w_inv_graph = ftM2graphNEL(data.matrix(rel_data), data.matrix(w_data))
w_inv_graph

phist = hist(path_matrix)
phist$counts
phist$breaks
cumsum(phist$counts)/sum(phist$counts)

# Requires RGBL
path_matrix = johnson.all.pairs.sp(w_inv_graph)
path_matrix[1:10, 1:10]
dev.off()
hist(path_matrix) # Within two steps is a relatively close relationship

mean(path_matrix[3, c(3, 5, 10)])

close_to_inf = function(path_matrix, comp_node, inf_nodes){
  person_in_comp = c()
  for (comp_inv in comp_node){
    path_to_in = path_matrix[comp_inv, inf_nodes]
    
    #http://stackoverflow.com/questions/24030926/remove-infinite-values-from-a-matrix-in-r
    path_to_in[!is.finite(path_to_in)] = NA
    avg_success_path = mean(path_to_in, na.rm=TRUE) #library(stats) IQR
    person_in_comp = c(person_in_comp, avg_success_path)
    }
  return(person_in_comp)
}

close_to_inf(path_matrix, c(1,2,3, 500), inf_nodes = c(200, 400, 600))

# Exploring Degrees throughout graph
degree(w_inv_graph) 
mean(degree(w_inv_graph))
dev.off()
hist(unlist(degree(w_inv_graph)))
max(unlist(degree(w_inv_graph)))

# Centrality of a Node
adj(w_inv_graph, 1424)
length(unlist(adj(w_inv_graph, 1424)))
degree(w_inv_graph, 1424) # Number of edges coincident on 1424
degree(w_inv_graph, 1424, cmode = 'indegree')
sub_nodes = unlist(adj(w_inv_graph, 1424))
sub_g_1424 = subGraph(sub_nodes, w_inv_graph)
dev.off()
plot(sub_g_1424)


set.vertex.attribute(w_inv_graph, 
                     "Degree", 
                     degree(w_inv_graph)
)

nodeData(w_inv_graph, )

# Graph Basics in R and Bioconductor
undir_inv = ugraph(w_inv_graph)
undir_inv = removeSelfLoops(undir_inv)
hcsub = highlyConnSG(undir_inv)
hcsN = unlist(apply(hcsub$clusters, FUN = length))
hcsMax = hcsub$cluster[[which.max(hcsN)]]
#hcSCG = subGraph(hcsMax, w_inv_graph)
#plot(hcSCG)

