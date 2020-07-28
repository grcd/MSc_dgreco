
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
  #E(G1)$ref = 1:ecount(G1)  # solo per le kb_* non matriciali
  E(G1)$measure = 0.0
  E(G1)$rank = as.integer(0)
  
  tA = Sys.time()

  G1 = do.call(wf, append(list(graph = G1), wf.args))
  if (is.null(G1))
    return(NULL)
  
  tB = Sys.time()
  dt = round(difftime(tB, tA, units="mins"), digits=3)
  if (verbose)
    getTiming(tA, Sys.time(), sprintf("%s |E|=%d, weighting", wf, ecount(G1)))
  

  #   Check if the weight attribute "measure" exists, otherwise stop.
  if (!"measure" %in% names(edge.attributes(G1)))
    stop("ERROR: $measure attribute does not exists .")
  
  
  #   round $measure to match eps precision
  E(G1)$measure = round(E(G1)$measure, roundTo)
  
  
  #   substitute NA, NaN, +Inf, -Inf with -Inf, so as to rank them with the *lowest* rank.
  E(G1)[which(!is.finite(E(G1)$measure))]$measure = -Inf
  
  
  #   Write out weights list in RANK.inputFile
  write.table(
    E(G1)$measure,
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
    toString(ecount(G1)),
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
  E(G1)$rank = read.table(file = RANK.outputFile)[, 1]
  
  
  #   Remove tmp files
  if (file.exists(RANK.inputFile))
    file.remove(RANK.inputFile)
  
  if (file.exists(RANK.outputFile))
    file.remove(RANK.outputFile)
  

  #  Mappa $measure e $rank di G1 su G
  if (! wf.MIXEDGES) {  #  l'ordine degli archi è conservato, copia diretta degli attributi

    #E(G)$measure = E(G1)$measure
    E(G)$rank = E(G1)$rank
    
  } else {  #  l'ordine degli archi di G1 è diverso da quello di G,
            #  usiamo gli estremi come mapping (più lento)
    
    ee       = as.vector(t(ends(G1, E(G1))))  # 1. ee contiene gli le coppie di vertici degli archi in G1 
    ids_in_G = get.edge.ids(G,                # 2. prendo gli id degli archi corrispondenti MA in G
                            ee,
                            directed = F,
                            error = F,
                            multi = F)
    #E(G)[ ids_in_G ]$measure = E(G1)$measure  # 3. PASSAGGIO CHIAVE: l'ordine adesso è lo stesso
    E(G)[ ids_in_G ]$rank = E(G1)$rank    
    
  }
  
  return(G)
}
