#
#  TOM: Topological Overlap Measure
# 

library(igraph)

# wf SIGNATURE
wf.NAME = "TOM"
wf.DESC = "For each undirected edge {u,v}, assigns weight(u,v) = TOM(u, v)"
wf.ARGS = list()
wf.MIXEDGES = TRUE

TOM <- function(graph)
{   
    m = 1
    #cat(paste("* Computing TOM", "...", sep = ""))

    AdjMatrix = as.matrix(get.adjacency(graph))
    #AdjMatrix[which(AdjMatrix==2)] = 1
    
    #ptm = proc.time()       # ADDON: timing
    W = GTOMmdist1(AdjMatrix, m)
    #print(proc.time() - ptm)

    tA = Sys.time()
    
    # # SOLUZIONE N.1   (lenta)
    # E(graph)$measure = 0.0
    # for (edge in E(graph)) {
    #     nodes = ends(graph, edge);
    #     
    #     i = nodes[, 1];
    #     j = nodes[, 2];
    #     
    #     E(graph)[edge]$measure = W[i, j] 
    # }
        
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
    cat(sprintf("* TIMING: TOM re-assigning took %s minutes \n", dt))
    
    #cat("DONE\n")
    return(graph)
}


GTOMmdist1 = function(adjmat1,m=1) {
    if (m!=round(abs(m))){
        stop("m must be a positive integer!!!", call.=TRUE);
    }
    
    if (any(adjmat1!=0 & adjmat1!=1)){
        idx = which(adjmat1 !=0 & adjmat1 != 1)
        print(adjmat1[idx])
        stop("The adjacency matrix must be binary!!!", call.=TRUE);
    }

    B <- adjmat1;
    if (m>=2) {
        for (i in 2:m) {
            diag(B) <- diag(B) + 1;
            B = B %*% adjmat1;
        }
    }                   # number of paths with length at most m connecting each pair
    
    B <- (B>0);                    # m-step reachability matrix (boolean)
    diag(B) <- 0;                  # exclude each node being its own neighbor
    B <- B %*% B;                  # number of common k-step neighbors that each pair of nodes share
    
    Nk <- diag(B);                 # number of common k-step neighbors that each node possesses
    B <- B +adjmat1;
    diag(B) <- 1;
    denomTOM=outer(Nk,Nk,FUN="pmin")+1-adjmat1;
    diag(denomTOM) <- 1;
    B/denomTOM                 # GTOMm matrix
}
