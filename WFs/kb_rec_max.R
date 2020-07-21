
#  KB_NORM_MAX_LA.R
#
#  Calcola la kb_rec_max in forma matriciale (LA) corretta

library(igraph)
source("utils/getTiming.R")

# wf SIGNATURE
wf.NAME = "kb_rec_max"
wf.DESC = "For each undirected edge {u,v}, assigns weight(u,v) == MAX{rec(u,v), rec(v,u)} (LA version)."
wf.ARGS = c()
wf.MIXEDGES = TRUE
wf.VERBOSE = FALSE

# wf PARAMETERS
KB.K = 3                                # number of iterations (K=3 optimal as depicted in Kleinberg)

kb_rec_max <- function(graph, K=KB.K, logfile="")
{
  # MATRIX COMPUTATION ------------------------------------------------------
  
  tA0 = Sys.time()
  
  n = vcount(graph)
  B = as.matrix(graph[,])

  X_i_minus_1 = B
  X_i = X_i_minus_1
  EMB = B %*% B
  #diag(EMB) = 0
  
  
  # PRE-COMPUTING fixed quantities
  us = names(which(colSums(B) != 0))
  NB = list()
  for (u in us) 
    NB[[ u ]] = names(which( B[u, ] == 1 ))
  
  
  for (i in 1:K) {  # i-th iteration
    
    #cat(sprintf("ITERATION: %d\n", i))
    
    #us = names(which(colSums(B) != 0))
    for (u in us) {
      
      #vs = names(which( B[u, ] == 1 ))  # prendo i vicini di u
      for (v in NB[[ u ]]) {
        
        if (EMB[u, v] <= 1) {   
          next                
        }
        
        #cat(sprintf("i=%d, u=%s, v=%s\n", i, u, v))
        
        cuv = B[u, ] * B[v, ]  # vicini comuni
        cuv = as.logical(cuv)  # maschera logica vicini comuni
        CUV = B[cuv, cuv]      # sottografo vicini comuni
        
        #  L22[s,t] è il numero di cammini di lunghezza 2 in Gu tra s e t NON PASSANTI per u e v
        #  - Se esiste arco (s,t) allora non conta il numero di 2-cammini: dist(s,t) = 0
        #  - Se NON esiste, allora mi chiedo: "Esiste almeno UN 2-cammino tra s e t?"
        #      cioè:  "L22[s, t] è diverso da 0?"
        #
        #  DIST[s,t], booleana è la dv(s,t) della formula
        
        gu = B[u, ] 
        gu = gu - diag(n)[ which( rownames(B) == v),  ] 
        gu = as.logical(gu)  # con u e v FALSE
        
        L2_all_in_Gu_minus_uv = B[gu, gu] %*% B[gu, gu]  # numero di cammini L2 nel sottografo gu senza u e v
        diag(L2_all_in_Gu_minus_uv) = 0
        L22 = L2_all_in_Gu_minus_uv[rownames(B)[cuv], rownames(B)[cuv]]  #  da cui prendo solo le coppie (s,t) (in Cuv) 
        DIST = ((CUV+1)%%2) * ((((L22 != 0)*1)+1)%%2)
        diag(DIST) = 0
        
        
        #  Xs  è la matrice che rappresenta il termine x_s che viene moltiplicato per d(s,t)
        #      In sostanza, è una matrice delle stesse dimensioni di DIST tale che Xs[j, ]
        #      ha ncol(DIST) copie di X[u, s]
        #
        #  Xt  che rappresenta il fattore x_t, si ottiene semplicemente come trasposta di Xs.
        Xs = X_i_minus_1[u, cuv] %*% matrix(1.0, nrow=1, ncol=EMB[u, v])
        #Xt = t(Xs)
        
        #  S1 è il primo addendo a numeratore della formula di rec(u, v)
        #  S2 è il secondo addendo
        #  EMB è la embeddedness, il numero di vicini comuni
        
        S1 = sum((X_i_minus_1[u, cuv])^2)
        S2 = sum(DIST * Xs * t(Xs))/2        # matrice simmetrica, divido per 2 la somma
        
        X_i[u, v] = (S1 + 2*S2)  # per il momento non dividiamo per emb_uv, lo facciamo a valle        
      }
    }
    
    X_i_minus_1 = X_i / EMB     
    X_i_minus_1[ X_i_minus_1 == Inf ] = 1
  }
  
  
  if (wf.VERBOSE)
    getTiming(tA0, Sys.time(), sprintf("kb_rec_max matrix computation"), file=logfile)
  
  
  # RE-ASSIGNING ------------------------------------------------------------

  #X_i_minus_1[ X_i_minus_1 == Inf ] = 1  #  Inf sono le coppie di nodi senza vicini comuni
  
  tA = Sys.time()
  
  X_i_minus_1[is.na(X_i_minus_1)] = 0.0
  graph=graph_from_adjacency_matrix(X_i_minus_1, mode="max", weighted=TRUE, diag=FALSE)
  
  E(graph)$measure = E(graph)$weight
  graph = remove.edge.attribute(graph, "weight")
  
  if (wf.VERBOSE)
    getTiming(tA, Sys.time(), "kb_rec_max re-assigning", file=logfile)
  
  return(graph)  
}
