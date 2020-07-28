#
#  EB: Edge Betweenness (Brandes)
#       

library(igraph)

# wf SIGNATURE
wf.NAME = "EB"
wf.DESC = "For each undirected edge {u,v}, assigns weight(u,v) = EB(u, v)."
wf.ARGS = list()
wf.MIXEDGES = FALSE

# wf PARAMETERS
# None.

EB <- function(graph)
{    
    #cat(paste("Computing EB...", sep = ""))

    E(graph)$measure = edge.betweenness(graph)
    
    #cat("OK\n")
    return(graph)
}
