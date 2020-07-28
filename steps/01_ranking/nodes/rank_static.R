
source("utils/getTiming.R")

#   RANK_STATIC.R   compute the static rank of 'graph' using the wf weight function
#                   and an external 'rankgraph' FAST program (written in C ).
#
#       INPUT:  G           graph to rank
#               wf          weight function (R)
#               wf.args     wf arguments
#
#       OUTPUT: G_r         with edges labelled with $rank class

RANK.execpath   = Global.EXEC_DIR
RANK.inputFile  = file.path(RANK.execpath, "RR_input.tmp")
RANK.outputFile = file.path(RANK.execpath, "RR_output.tmp")

rank_static <- function(G, wf, wf.args, wf.MIXEDGES, roundTo, verbose=F) {

  #  PROBLEMI
  #     1. wf può cambiare l'ordine originario degli archi
  #     2. wf può spazzare via tutti gli altri attributi del grafo
  #
  #  PER TESTARE se una data wf fa (1-2), vedi test_code/test_wf_MIXEDGES.R.
  #
  #  SOLUZIONE:   Per tutte le wf che lo fanno, aggiungi un flag nella signature:  wf.MIXEDGES.
  #               Qui: 
  #                   1. lavora su una copia di G: G1
  #                   2. pesa e fai il ranking di G1: ottieni gli attributi $measure, $rank sugli archi
  #                   3. incolla E(G1)$measure e $rank sui corrispondenti archi di G, usando gli ESTREMI
  #                      degli archi.
  
  G1 = G
  
  vattribs = setdiff(vertex_attr_names(G1), c("id", "name"))
  eattribs = setdiff(edge_attr_names(G1), c("id", "name"))
  
  for (va in vattribs)
    G1 = delete_vertex_attr(G1, va)
  for (ea in eattribs)
    G1 = delete_edge_attr(G1, ea)
  
  V(G1)$rank = as.integer(0)
  E(G1)$rank = as.integer(0)
  
  #   Node weightening
  tA = Sys.time()
    G1 = do.call(wf, append(list(graph = G1), wf.args))
    if (is.null(G1))
      return(NULL)
  tB = Sys.time()
  
  dt = round(difftime(tB, tA, units="mins"), digits=3)
  if (verbose)
    getTiming(tA, Sys.time(), sprintf("%s weighting (|E|=%d)", wf, ecount(G1)))
  
  #   Check if the weight attribute "measure" exists, otherwise stop.
  if (!"measure" %in% vertex_attr_names(G1))
    stop("ERROR: $measure attribute does not exists .")  
  
  #   round $measure to match eps precision
  V(G1)$measure = round(V(G1)$measure, roundTo)
  
  #   substitute NA, NaN, +Inf, -Inf with -Inf, so as to rank them with the *lowest* rank.
  V(G1)[which(!is.finite(V(G1)$measure))]$measure = -Inf
  
  
  #   Write out weights list in RANK.inputFile
  write.table(
    V(G1)$measure,
    file = RANK.inputFile,
    row.names = FALSE,
    col.names = FALSE
  )
  if (!file.exists(RANK.inputFile))
    stop("Unable to write RR.inputFile")
  
  
  #   EXECUTE rankgraph:   ./RANK.execpath RR.inputFile RR.outputFile m
  cmd = paste(
    paste(RANK.execpath, "rankgraph", sep = "/"),
    RANK.inputFile,
    RANK.outputFile,
    toString(vcount(G1)),
    sep = " "
  )
  
  if (system(
    cmd,
    intern = FALSE,
    ignore.stdout = TRUE,
   ignore.stderr = TRUE
  ) != 0)
    # error
    stop("Unable to execute RANK.execpath")
  
  
  #   Check if RR.outputFile exists
  if (!file.exists(RANK.outputFile))
    stop("Unable to write RR.outputFile")
  
  
  #   Copy/paste rankings into the edges
  V(G1)$rank = read.table(file = RANK.outputFile)[, 1]
  
  
  #   Remove tmp files
  if (file.exists(RANK.inputFile))
    file.remove(RANK.inputFile)
  
  if (file.exists(RANK.outputFile))
    file.remove(RANK.outputFile)
  

  #  Mappa $measure e $rank di G1 su G
  if (! wf.MIXEDGES) {  #  l'ordine degli archi è conservato, copia diretta degli attributi

    V(G)$measure = V(G1)$measure
    V(G)$rank = V(G1)$rank
    
    #   Induced rank on edges: loop-free solution
    vr = matrix( data = V(G)[ as.vector(t(ends(G, E(G)))) ]$rank,
                 nrow = ecount(G),
                 ncol = 2,
                 byrow = T)
    E(G)$rank = apply(vr, 1, max)
    
    ii = order(E(G)$rank)
    E(G)$rank[ ii ] = as.integer(factor(E(G)$rank[ ii ], labels=1:length(unique(E(G)$rank))))    
    
  } else {  #  l'ordine degli archi di G1 è diverso da quello di G,
            #  usiamo gli estremi come mapping (più lento)
    
    # to be implemented.
    # Tutte le misure che uso al momento sono MIXEDGES=F.
    
  }
  
  return(G)
}
