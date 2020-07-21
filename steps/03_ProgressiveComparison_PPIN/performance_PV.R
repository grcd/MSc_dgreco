
#   PERFORMANCE_PV (PV, GR, K, id_to_index='', rt='', sigma_thr=0.2)
#
#   Prende in input:
#     * PV è la matricedelle percentage-view di G. In particolare, l'elemento PV[f, j] = pr_j 
#       è il rank limite per costruire la j-esima percentage view indotta da f.
#     * GR è una list di grafi: G$f è il grafo G ranked secondo f.
#     * K è una list contenente i complessi relativi alla rete G.
#     * sigma_thr è la threshold per OSmatch
#     * id_to_index, rt non sono utilizzati e presenti per retro-compatbilità.
#
#   L'insieme names(GR) deve coincidere con rownames(PV): tutte le f.
#   I nodi di G devono avere l'attributo $name allineato con le proteine dei complessi in K. 
#   L'i-esimo complesso è accessibile in K[[i]]: il vettore delle proteine del complesso.
#
#   Restituisce in output una list() contenente le matrici
#       R       tali che    R[f, i]=Recall at the i-th percentage view according to f
#       P                   P[f, i]=Precision at the i-th percentage view according to f
#       F                   Fm[f, i]=F-Measure at the i-th percentage view according to f
#   e le list()
#       CK                  CK[[ f ]][[ i ]]=Complexes matched at the i-th percentage view of f
#       id_max              id_max[[ f ]]=max i, such that F[f, i] is maximum among all j in PV[f, ]
#
#
#   NOTE
#     In pratica, ogni esecuzione di evaluate_with_complexes riempie una riga 
#     della Tabella degli Esperimenti (si veda Fig.2, stato-dei-lavori.pdf  ).
#     Allo stesso tempo, mi conservo tutti i dati per eventuali analisi successive.

source("steps/03_ProgressiveComparison_PPIN/OS_match.R");

performance_PV <- function(PV, GR, K, sigma_thr=0.2, id_to_index='', ft='', SUBG_DIR='', GRAPH_NAME='subgraph', verbose=F) {
  
  if (ft == "incremental") {
    return(performance_PV_INC(PV, GR, K, sigma_thr, id_to_index=id_to_index, SUBG_DIR=SUBG_DIR, GRAPH_NAME=GRAPH_NAME, verbose=verbose));
  } else if (ft == "decremental") {
    return(performance_PV_DEC(PV, GR, K, sigma_thr, id_to_index=id_to_index, SUBG_DIR=SUBG_DIR, GRAPH_NAME=GRAPH_NAME, verbose=verbose));
  } else {
    warning("performance_PV: invalid ft (function type), must be either \"incremental\" or \"decremental\"");
    return(list());         
  }
  
}

performance_PV_INC <- function(PV, GR, K, sigma_thr=0.2, id_to_index='', SUBG_DIR='', GRAPH_NAME='subgraph', verbose=F) {
  
  if (length(GR) == 0) {
    warning("performance_PV: no graphs provided.")
    return(list());
  } 
  
  if (length(K) == 0) {
    warning("performance_PV: no complexes provided.")
    return(list());
  } 
  
  if (!(all(names(GR) == rownames(PV)))) {
    warning("performance_PV: missing graphs or percentage views. Check GR and PV.")
    return(list())
  }
  
  # Recall    R[f, i] contains recall score at the i%-th percentage view induced by f
  R = matrix(data=c(0.0), nrow=nrow(PV), ncol=ncol(PV))
  rownames(R) = rownames(PV)
  colnames(R) = colnames(PV)
  
  # Precision P[f, i] as above
  P = matrix(data=c(0.0), nrow=nrow(PV), ncol=ncol(PV))
  rownames(P) = rownames(PV)
  colnames(P) = colnames(PV)
  
  # F-Measure Fm[f, i] as above
  Fm = matrix(data=c(0.0), nrow=nrow(PV), ncol=ncol(PV))
  rownames(Fm) = rownames(PV)
  colnames(Fm) = colnames(PV)
  
  
  n_K = length(K);  # total number of complexes
  
  CK = list();      # a list of lists, CK[[ f ]][[ i ]] contains matched complexes
  # at the i%-th percentage view induced by f.
  
  id_max = list();  # id_max[[ f ]] contains the i such that Fm[f, i] is maximum for f
  
  
  for (f in rownames(PV)) {
    
    G = GR[[ f ]]
    CK[[ f ]] = list()
    id_max[[ f ]] = -1
    
    maxr = max(E(G)$rank)
    for (pi in 1:ncol(PV)) {     # for every INCREMENTAL percentage rank view
      
      #cat(sprintf("r=%d\n", r))
      CK[[ f ]][[ pi ]] = c()
      
      #   0.  TP=true positive counter, CK=matched complexes ids 
      TP = 0;
      matched_complexes = c();
      
      #   1. construct the edge-rank view induced subgraph
      # edges = union(edges, E(G)[ which( E(G)$rank <= PV[f, pi] ) ])
      edges = E(G)[ which( E(G)$rank <= PV[f, pi] ) ]
      subg  = subgraph.edges(G, edges, delete.vertices=TRUE);
      
      #   2. extract connected components P
      cc = components(subg);
      
      #   2.5  write the subgraph for later plotting
      if (SUBG_DIR != '') {
        V(subg)$membership = cc$membership
        write.graph(subg, 
                    file=file.path(SUBG_DIR, f, paste(GRAPH_NAME, "_", f, "_PV_", pi, ".gml", sep="")), 
                    format="gml")
      }    
      
      #   3. for each P_i, compare with all complexes K_j
      n_cc = cc$no;           # number of connected components
      for (i in 1:(n_cc)) {
        
        P_i = V(G)[which(cc$membership == i)]$name;
        for (j in 1:n_K)  {
          
          K_j = K[[j]];
          if ( OS_match(P_i, K_j, sigma_thr) ) {
            TP = TP + 1;
            matched_complexes = c(matched_complexes, j);
          }
        }
      }
      
      #   4.  compute scores for the edge
      R[f, pi]  = TP / n_K;    # n_K  counts all true complexes, that is: n_K=TP+FN
      P[f, pi]  = TP / n_cc;   # n_cc counts all the positive predictions, that is: n_cc=TP+FP
      Fm[f, pi] = (2*(R[f, pi]*P[f, pi])) / (R[f, pi]+P[f, pi]);
      
      if (is.na(R[f, pi]))
        R[f, pi] = 0.0;
      
      if (is.na(P[f, pi]))
        P[f, pi] = 0.0;
      
      if (is.na(Fm[f, pi]))
        Fm[f, pi] = 0.0;      
      if (length(matched_complexes) == 0) {
        CK[[ f ]][[ pi ]] = c("empty");
      } else {
        CK[[ f ]][[ pi ]] = matched_complexes;
      }
      
      if (id_max[[ f ]] == -1) {
        id_max[[ f ]] = 1;
      } else if (Fm[f, pi] > Fm[f, id_max[[ f ]]]) {
        id_max[[ f ]] = pi;  # maximum F-measure for f
      }
      
      if (verbose)
        cat(paste("* f=", f, " percentage_view=", pi, " R=", format(R[f, pi], nsmall = 4), " P=", format(P[f, pi], nsmall = 4), " Fm=", format(Fm[f, pi], nsmall = 4), " -- complexes=", paste(CK[[ f ]][[ pi ]], collapse=", "), "\n", sep=""));
      
    }
    
  }  
  return(list(R=R, P=P, Fm=Fm, CK=CK, id_max=id_max));
}


performance_PV_DEC <- function(PV, GR, K, sigma_thr=0.2, id_to_index='', SUBG_DIR='', GRAPH_NAME='subgraph', verbose=F) {
  
  if (length(GR) == 0) {
    warning("performance_PV: no graphs provided.")
    return(list());
  } 
  
  if (length(K) == 0) {
    warning("performance_PV: no complexes provided.")
    return(list());
  } 
  
  if (!(all(names(GR) == rownames(PV)))) {
    warning("performance_PV: missing graphs or percentage views. Check GR and PV.")
    return(list())
  }
  
  # Recall    R[f, i] contains recall score at the i%-th percentage view induced by f
  R = matrix(data=c(0.0), nrow=nrow(PV), ncol=ncol(PV))
  rownames(R) = rownames(PV)
  colnames(R) = colnames(PV)
  
  # Precision P[f, i] as above
  P = matrix(data=c(0.0), nrow=nrow(PV), ncol=ncol(PV))
  rownames(P) = rownames(PV)
  colnames(P) = colnames(PV)
  
  # F-Measure Fm[f, i] as above
  Fm = matrix(data=c(0.0), nrow=nrow(PV), ncol=ncol(PV))
  rownames(Fm) = rownames(PV)
  colnames(Fm) = colnames(PV)
  
  
  n_K = length(K);  # total number of complexes
  
  CK = list();      # a list of lists, CK[[ f ]][[ i ]] contains matched complexes
  # at the i%-th percentage view induced by f.
  
  id_max = list();  # id_max[[ f ]] contains the i such that Fm[f, i] is maximum for f
  
  
  for (f in rownames(PV)) {
    
    G = GR[[ f ]]
    CK[[ f ]] = list()
    id_max[[ f ]] = -1
    
    maxr = max(E(G)$rank)
    for (pi in 1:ncol(PV)) {     # for every INCREMENTAL percentage rank view
      
      #cat(sprintf("r=%d\n", r))
      CK[[ f ]][[ pi ]] = c()
      
      #   0.  TP=true positive counter, CK=matched complexes ids 
      TP = 0;
      matched_complexes = c();
      
      #   1. construct the edge-rank view induced subgraph
      # edges = union(edges, E(G)[ which( E(G)$rank <= PV[f, pi] ) ])
      edges = E(G)[ which( E(G)$rank <= PV[f, pi] ) ]
      edges = setdiff(E(G), edges)
      subg  = subgraph.edges(G, edges, delete.vertices=TRUE);
      
      #   2. extract connected components P
      cc = components(subg);
      
      #   2.5  write the subgraph for later plotting
      if (SUBG_DIR != '') {
        V(subg)$membership = cc$membership
        write.graph(subg, 
                    file=file.path(SUBG_DIR, f, paste(GRAPH_NAME, "_", f, "_PV_", pi, ".gml", sep="")), 
                    format="gml")
      }    
      
      #   3. for each P_i, compare with all complexes K_j
      n_cc = cc$no;           # number of connected components
      for (i in 1:(n_cc)) {
        
        P_i = V(G)[which(cc$membership == i)]$name;
        for (j in 1:n_K)  {
          
          K_j = K[[j]];
          if ( OS_match(P_i, K_j, sigma_thr) ) {
            TP = TP + 1;
            matched_complexes = c(matched_complexes, j);
          }
        }
      }
      
      #   4.  compute scores for the edge
      R[f, pi]  = TP / n_K;    # n_K  counts all true complexes, that is: n_K=TP+FN
      P[f, pi]  = TP / n_cc;   # n_cc counts all the positive predictions, that is: n_cc=TP+FP
      Fm[f, pi] = (2*(R[f, pi]*P[f, pi])) / (R[f, pi]+P[f, pi]);
      
      if (is.na(R[f, pi]))
        R[f, pi] = 0.0;
      
      if (is.na(P[f, pi]))
        P[f, pi] = 0.0;
      
      if (is.na(Fm[f, pi]))
        Fm[f, pi] = 0.0;
      
      if (length(matched_complexes) == 0) {
        CK[[ f ]][[ pi ]] = c("empty");
      } else {
        CK[[ f ]][[ pi ]] = matched_complexes;
      }
      
      if (id_max[[ f ]] == -1) {
        id_max[[ f ]] = 1;
      } else if (Fm[f, pi] > Fm[f, id_max[[ f ]]]) {
        id_max[[ f ]] = pi;  # maximum F-measure for f
      }
      
      if (verbose)
        cat(paste("* f=", f, " percentage_view=", pi, " R=", format(R[f, pi], nsmall = 4), " P=", format(P[f, pi], nsmall = 4), " Fm=", format(Fm[f, pi], nsmall = 4), " -- complexes=", paste(CK[[ f ]][[ pi ]], collapse=", "), "\n", sep=""));
      
    }
    
  }  
  return(list(R=R, P=P, Fm=Fm, CK=CK, id_max=id_max));
}