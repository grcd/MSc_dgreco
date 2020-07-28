
source("utils/getTiming.R")

rank_dynamic <- function(G, wf, wf.args, wf.MIXEDGES, roundTo, verbose=F) {
  
  # Inizializza rank=0 per tutti gli archi del grafo iniziale, G.
  E(G)$rank = as.integer(0)

  # Il grafo G1 è quello da cui rimuoviamo, di volta in volta, gli archi di peso massimo
  # L'i-esima iterazione estra gli archi di peso massimo da G1 ed assegna rank=i ai corrispondenti
  # archi di G.
  #
  # E' qui utile ricordare che la pesatura **può cambiare l'ordine degli archi** (MIX.EDGES=T).
  # In quest'ultimo caso, deve essere garantito che almeno il $name dei nodi sia mantenuto, così
  # da mettere in corrispondenza gli archi di G1 con G in ogni caso.
  #
  # Questo è il motivo per il quale abbiamo bisogno di nodi etichettati con $name come requisito
  # per i grafi in input.
  
  G1 = G
  r = 1
  while (ecount(G1) > 0) {
    
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
    if (! "measure" %in% names(edge.attributes(G1)) )
      stop("ERROR: $measure attribute does not exists .");
    
    
    #   round $measure to match eps precision
    E(G1)$measure = round(E(G1)$measure, roundTo);
    
    
    #   substitute NaN, +Inf, -Inf with -Inf, so as to rank them with the *lowest* rank.    
    E(G1)[ which(!is.finite(E(G1)$measure)) ]$measure = -Inf;            
    
    
    #   prendi gli archi di peso massimo in G1
    maxw = max(E(G1)$measure)
    
    max_edges_in_G1 = which( E(G1)$measure == maxw )
    
    ee = as.vector(t( ends(G1, E(G1)[ max_edges_in_G1 ]) ))
    
    ids_in_G = get.edge.ids(G,                # 2. prendo gli archi corrispondenti in G
                            ee,
                            directed = F,
                            error = F,
                            multi = F)
    E(G)[ ids_in_G ]$rank = r                 # 3. gli assegno il rank corrente
    
    r = r + 1  # 5. incrementa il rank
    
    G1 = delete.edges(G1, E(G1)[ max_edges_in_G1 ])  # 5. togli gli archi di peso massimo e ripeti
   
  }
  
  return(G)
}  
