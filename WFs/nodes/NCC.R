#
#  NCC: Node Clustering Coefficient (igraph)
#       

library(igraph)

# wf SIGNATURE
wf.NAME = "NCC"
wf.DESC = ""
wf.ARGS = list()
wf.MIXEDGES = FALSE
wf.VERBOSE = FALSE


# wf PARAMETERS
# None.

NCC <- function(graph)
{    
    V(graph)$measure = transitivity(graph, type="local", vids=V(graph))
    V(graph)$measure[ which(is.na(V(graph)$measure), arr.ind = T) ] = 0
    
    return(graph)
}
