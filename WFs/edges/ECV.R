
# ---------------------------------------------------------------------
# ECV Edge Clustering Value as defined in LiPan11 (see Draft_Hierarchy)
# --------------------------------------------------------------------
# 

library(igraph)

# wf SIGNATURE
wf.NAME = "ECV"
wf.DESC = "For each undirected edge {u,v}, assigns weight(u,v) = ECV(u, v)."
wf.ARGS = c()
wf.MIXEDGES = TRUE
wf.VERBOSE = FALSE

# wf PARAMETERS:
# None.

ECV = function(graph) {
    
    #   ptm = proc.time()           # START timing
    #cat(paste("* Computing ECV..", sep = ""))

#    old_adj = graph[ , ]
      
    tA = Sys.time();
    B <- as.matrix(graph[,])    # B is the Adjacency Matrix of graph
    
    CN <- B %*% B               # B %*% B number of common neighbors of i and j
    diag(CN) = 0                
    
    NB = as.matrix(colSums(B))  # NB is a column matrix; NB[i] = #neighbors of i
    denom = NB %*% t(NB)  
    diag(denom) = 0
    
    W = CN^2 / denom          # the ECV matrix as defined in the paper

    tB = Sys.time()
    dt = round(difftime(tB, tA, units="mins"), digits=3)
    if (wf.VERBOSE)
      cat(sprintf("* TIMING: ECV matrix computation took %s minutes \n", dt))
    
    #   print(proc.time() - ptm)    # END timing

    #  SOLUZIONE N.1   (lenta)
    
    tA = Sys.time()
    # E(graph)$measure = 0.0
    # for (edge in E(graph)) {
    #     nodes = ends(graph, edge);
    # 
    #     i = nodes[, 1];
    #     j = nodes[, 2];
    # 
    #     E(graph)[edge]$measure = W[i, j]
    # }

    #  SOLUZIONE N.2:  STANDBY
    #    OK:   conserva i vertex $name
    #    OK:   è piu veloce del metodo precedente
    #    BAD:  cambia l'ordine degli archi
    #    BAD:  se l'arco ha peso 0, lo rimuove cosa che non voglio
    W = as.matrix(W)
    W[ which(W == 0) ] = .Machine$double.xmin   #  Se li lascio a 0, graph_from_adjacency_matrix li elimina
    W[ is.na(W) ] = .Machine$double.xmin        #  Se li lascio a NaN, saranno creati come archi
    W = W * as.matrix(get.adjacency(graph, type="both"))  # i non archi in W vengono messi a 0, gli archi con peso 0 rimangono con .Machine$double.xmin
    
    graph=graph_from_adjacency_matrix(W, mode="undirected", weighted=TRUE, diag=FALSE)

    E(graph)$measure = E(graph)$weight
    graph = remove.edge.attribute(graph, "weight")
    E(graph)[ which(E(graph)$measure == .Machine$double.xmin) ]$measure = 0.0  #  rimetto gli archi con peso .Machine$double.xmin a 0 
      
    tB = Sys.time()
    dt = round(difftime(tB, tA, units="mins"), digits=3)
    if (wf.VERBOSE)
      cat(sprintf("* TIMING: ECV re-assigning took %s minutes \n", dt))

    #cat("DONE\n")
    
    # mask_adj = as.matrix(1*xor(old_adj, graph[,]))  # restore all the edges (deleted by a 0.0 weight)
    # ii = which( mask_adj == 1, arr.ind=T)
    # graph[ ii[, 1], ii[, 2]] = 1
    
    return(graph)
}
