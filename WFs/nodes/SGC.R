#
#  SC: Subgraph centrality (igraph)
#       

library(igraph)

# wf SIGNATURE
wf.NAME = "SGC"
wf.DESC = ""
wf.ARGS = list()
wf.MIXEDGES = FALSE
wf.VERBOSE = FALSE


# wf PARAMETERS
# None.

SGC <- function(graph)
{    
  V(graph)$measure = subgraph_centrality(graph)
  V(graph)$measure[ which(is.na(V(graph)$measure), arr.ind = T) ] = 0
  
  return(graph)
}
