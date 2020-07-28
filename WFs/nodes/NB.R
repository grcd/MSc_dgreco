#
#  NB: Node Betweenness (igraph)
#       

library(igraph)

# wf SIGNATURE
wf.NAME = "NB"
wf.DESC = ""
wf.ARGS = list()
wf.MIXEDGES = FALSE
wf.VERBOSE = FALSE


# wf PARAMETERS
# None.

NB <- function(graph)
{    
  V(graph)$measure = betweenness(graph, directed=F)
  V(graph)$measure[ which(is.na(V(graph)$measure), arr.ind = T) ] = 0
  
  return(graph)
}
