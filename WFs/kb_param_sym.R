
#  KB_NORM_LA.R
#
#  Calcola la kb_norm in forma quasi puramente matriciale.
#  E' spiegata in kb_norm_LA_REASONING, dove viene costruita.


library(igraph)
source("utils/getTiming.R")

# wf SIGNATURE
wf.NAME = "kb_param_sym"
wf.DESC = "For each undirected edge {u,v}, assigns weight(u,v) = kb_param_sym(u,v,a,b,c) (LA version)."
wf.ARGS = c()
wf.MIXEDGES = TRUE
wf.VERBOSE = FALSE

# wf PARAMETERS
KB.a = 0.61      # 0.61  for parametric optimized dispersion
KB.b = 0.0       # 0.0   ""
KB.c = 5.0       # 5.0   ""

kb_param_sym <- function(graph, a=KB.a, b=KB.b, c=KB.c, logfile="")
{
  # MATRIX COMPUTATION ------------------------------------------------------
  
  tA0 = Sys.time()
  
  n = vcount(graph)
  B = as.matrix(graph[,])
  
  W = matrix(-1, n, n)
  rownames(W) = rownames(B)
  colnames(W) = colnames(B)
  
  us = names(which(colSums(B) != 0))
  for (u in us) {
    
    vs = names(which( B[u, ] == 1 ))  # prendo i vicini di u
    for (v in vs) {
      #cat(sprintf("u=%s, v=%s\n", u, v))
      
      # NOTA BENE:  sto pesando solo le coppie u-v che sono anche ARCHI
      
      # --
      cuv = B[u, ] * B[v, ]  # vicini comuni
      if (sum(cuv) <= 1) {   # 0 vicini comuni: misura non definita: no coppie s,t: -1
        W[u, v] = .Machine$double.xmin
        next                # 1 vicino comune: misura non definita: no coppie s,t: -1
      }
      
      # ATTENZIONE: finora W[u, v] = -1 significa 3 cose
      #               1. u,v  non è un arco
      #               2. u,v  arco ma 0 vicini comuni
      #               3. u,v  arco ma 1 vicino comune
      #             questo verrà sistemato a valle, vedi POSTPROCESSING
      
      cuv = as.logical(cuv)  # maschera logica vicini comuni
      
      CUV = B[cuv, cuv]                  # sottografo vicini comuni
      
      L2C = ((CUV+1)%%2) * (((( ((CUV %*% CUV)*((diag(nrow(CUV))+1)%%2)) != 0)*1)+1)%%2)
      diag(L2C) = 0

      W[u, v] = sum(L2C)/2  # disp_uv
    }
  }
  
  #  Trasformo secondo i parametri
  W = ((W + b)^a) / ((B %*% B) + c)
  
  if (wf.VERBOSE)
    getTiming(tA0, Sys.time(), sprintf("kb_param_sym matrix computation"), file=logfile)
  
  
  # RE-ASSIGNING ------------------------------------------------------------
  
  tA = Sys.time()
  
  W[ which(B == 0) ] = 0.0    # tolgo i non-archi
  W[ which(W == 0) ] = -2.0   # però mi conservo gli archi con peso 0 che verrebbero eliminati da graph_from_adjacency_matrix
  W[ which(B == 0) ] = 0.0    # ri-tolgo i non-archi

  graph=graph_from_adjacency_matrix(W, mode="max", weighted=TRUE, diag=FALSE)
  
  E(graph)$measure = E(graph)$weight
  graph = remove.edge.attribute(graph, "weight")
  
  E(graph)[ which(E(graph)$measure == -2.0) ]$measure = 0.0  #  ripristino archi con peso 0.0 

  if (wf.VERBOSE)
    getTiming(tA, Sys.time(), "kb_norm re-assigning", file=logfile)
  
  return(graph)  
}
