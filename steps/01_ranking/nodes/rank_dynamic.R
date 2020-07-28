
source("utils/getTiming.R")

rank_dynamic <- function(G, wf, wf.args, wf.MIXEDGES, roundTo, verbose=F) {
  
  # Il grafo G1 è quello da cui rimuoviamo, ad ogni iterazione, i nodi di peso massimo.
  # L'i-esima iterazione estra gli archi di peso massimo da G1 ed assegna rank=i ai corrispondenti
  # archi di G.
  #
  # E' qui utile ricordare che la pesatura **può cambiare l'ordine degli archi** (MIX.EDGES=T).
  # In quest'ultimo caso, deve essere garantito che almeno il $name dei nodi sia mantenuto, così
  # da mettere in corrispondenza gli archi di G1 con G in ogni caso.
  #
  # Questo è il motivo per il quale abbiamo bisogno di nodi etichettati con $name come requisito
  # per i grafi in input.

  V(G)$rank = as.integer(0)
  E(G)$rank = as.integer(0)
  
  #  G1 è una versione "nuda e cruda" del grafo G originario (no attributes)
  G1 = G
  
  vattribs = setdiff(vertex_attr_names(G1), c("id", "name"))
  eattribs = setdiff(edge_attr_names(G1), c("id", "name"))
  
  for (va in vattribs)
    G1 = delete_vertex_attr(G1, va)
  for (ea in eattribs)
    G1 = delete_edge_attr(G1, ea)

  r = 1
  while (vcount(G1) > 0) {
    
    #   pesa G1
    #cat(paste("|V|=", vcount(G1), ", |E|=", ecount(G1)), "\n", sep = "");
    tA1=Sys.time();
    G1 = do.call(wf, append(list(graph=G1), wf.args))
    if (is.null(G1))
      return(NULL)

    if (verbose)
      if (r %% 50 == 0)
        getTiming(tA1, Sys.time(), sprintf("%s |E|=%d, last rank=%d, weightening", wf, ecount(G1), r))
    
    
    #   Check if the weight attribute "measure" exists, otherwise stop.
    if (! "measure" %in% names(vertex.attributes(G1)) )
      stop("ERROR: $measure attribute does not exists .")
    
    
    #   round $measure to match eps precision
    V(G1)$measure = round(V(G1)$measure, roundTo);
    
    
    #   substitute NaN, +Inf, -Inf with -Inf, so as to rank them with the *lowest* rank.    
    V(G1)[ which(!is.finite(V(G1)$measure)) ]$measure = -Inf;            
    
    
    #   node ranking
    maxw = max(V(G1)$measure)
    max_nodes = which( V(G1)$measure == maxw )  # indexes in G1
    max_nodes = V(G1)[ max_nodes ]$name         # name vertex is the common reference, indexes *MAY CHANGE*
    V(G)[ max_nodes ]$rank = r
    
    r = r + 1
    
    G1 = delete_vertices(G1, V(G1)[ max_nodes ])
  }
  
  #  induced edge ranking
  vr = matrix( data = V(G)[ as.vector(t(ends(G, E(G)))) ]$rank,
               nrow = ecount(G),
               ncol = 2,
               byrow = T)
  E(G)$rank = apply(vr, 1, max)
  
  ii = order(E(G)$rank)
  E(G)$rank[ ii ] = as.integer(factor(E(G)$rank[ ii ], labels=1:length(unique(E(G)$rank))))  
  
  return(G)
}  
