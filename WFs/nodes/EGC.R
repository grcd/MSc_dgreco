#
#  EGC: Eigenvector centrality (igraph)
#       

library(igraph)

# wf SIGNATURE
wf.NAME = "EGC"
wf.DESC = ""
wf.ARGS = list()
wf.MIXEDGES = FALSE
wf.VERBOSE = FALSE

# wf PARAMETERS
# None.

EGC <- function(graph)
{    
  egc.data = eigen_centrality(graph)
  V(graph)$measure = egc.data$vector
  V(graph)$measure[ which(is.na(V(graph)$measure), arr.ind = T) ] = 0
  
  return(graph)
}
