
# -------------------------------------------------------------------------------
# ECC3 Edge Clustering Coefficient_3 as defined in Radicchi (see Draft_Hierarchy)
# -------------------------------------------------------------------------------
#   VERSIONE COMPLEMENTATA    1.0-ECC3
#
#   SPIEGAZIONE
#   z_ij_3:
#       Il numero di triangoli costruiti sull'arco (i, j) è pari al numero
#       di vicini comuni w ad i e j.
#       Infatti:
#           Se w è comune ad i e j, allora esistono due archi (i, w) e (j, w).
#           Ma allora esiste un triangolo {(i,j), (i, k), (j, k)} contenente (i, j).
#
#   NOTE IMPLEMENTATIVE 
#   Si usano le primitive di igraph R. 
#   Per grafi molto densi si può dare una versione che usa la matrice di adiacenza 
#   e il prodotto matriciale veloce.
#
#   NOTA DI UTILIZZO:
#   Nativamente, quanto più ECC3(i,j) è alta tanto più è probabile che i e j
#   appartengano alla stessa community e che (i,j) sia un arco intra-community,
#
#   Tuttavia, Radicchi et al. usano ECC3 nello stesso modo in cui Girvan-Newman usano EB:
#	  ad ogni iterazione si rimuovono gli archi *meno* rilevanti per le communities, cioè
#   gli archi con **minore ECC3**.
#   
#   Coerentemente, ECC3 è nel setting decrementale e si pesano gli archi con -ECC3. 
#   Di conseguenza, più piccolo è ECC3, maggiore sarà -ECC3 e l'arco con ECC3 minimo
#   (cioè il più inter-community) avrà il peso -ECC3 maggiore fra gli altri e verrà
#   ranked #1.

library(igraph)

# wf SIGNATURE
wf.NAME = "ECC3"
wf.DESC = "For each undirected edge {u,v}, assigns weight(u,v) = ECC3(u, v)."
wf.ARGS = c()
wf.MIXEDGES = TRUE
wf.VERBOSE = FALSE

# wf PARAMETERS:
# None.

ECC3 = function(graph) {
    
#    ptm = proc.time()           # START timing
    #cat(paste("* Computing ECC3..", sep = ""))
  tA = Sys.time()
  
    # E(graph)$measure = 0.0
    # for (edge in E(graph)) {
    #     nodes = ends(graph, edge);
    # 
    #     i = nodes[, 1];
    #     j = nodes[, 2];
    # 
    #     k_i = length(neighbors(graph, i))   # at least 1, because (i,j) IS an edge
    #     k_j = length(neighbors(graph, j))
    # 
    #     z_ij_3 = length(intersect(neighbors(graph, i), neighbors(graph, j)))
    #     ecc3 = (z_ij_3 + 1) / min(k_i-1, k_j-1)  # if k_i (or k_j) = 1, then denominator = 0, +Inf
    # 
    #     E(graph)[edge]$measure = -ecc3
    # 
    # }
    
    n = vcount(graph)

    B = as.matrix(graph[,]) 
    m1 = matrix(rep(colSums(B)-1, each=n), nrow=n)
    m2 = t(m1)
    MIN = pmin(m1, m2)

    W = ((B %*% B) + 1) / MIN
    W = -W
    
    tB = Sys.time()
    dt = round(difftime(tB, tA, units="mins"), digits=3)
    if (wf.VERBOSE)
      cat(sprintf("* TIMING: ECC3 computation took %s minutes \n", dt))
    
    
    tA = Sys.time()

    W = as.matrix(W)
    W[ which(W == 0) ] = .Machine$double.xmin   #  Se li lascio a 0, graph_from_adjacency_matrix li elimina
    W[ is.na(W) ] = .Machine$double.xmin        #  Se li lascio a NaN, saranno creati come archi
    
        
    W = W * as.matrix(get.adjacency(graph, type="both"))  # ATTENZIONE: -Inf * 0 = NaN
    W[ is.na(W) ] = 0        #  i NaN sono i -Inf **che non sono archi**
    
    graph=graph_from_adjacency_matrix(W, mode="undirected", weighted=TRUE, diag=FALSE)

    E(graph)$measure = E(graph)$weight
    graph = remove.edge.attribute(graph, "weight")
    E(graph)[ which(E(graph)$measure == .Machine$double.xmin) ]$measure = 0.0  #  rimetto gli archi con peso .Machine$double.xmin a 0
    E(graph)[ which( is.na(E(graph)$measure) ) ]$measure = -Inf  #  rimetto gli archi con peso .Machine$double.xmin a 0
    
    tB = Sys.time()
    dt = round(difftime(tB, tA, units="mins"), digits=3)
    if (wf.VERBOSE)
      cat(sprintf("* TIMING: ECC3 re-assigning took %s minutes \n", dt))
    
    
#   print(proc.time() - ptm)    # END timing
    #cat("DONE\n")

    return(graph)   
}
