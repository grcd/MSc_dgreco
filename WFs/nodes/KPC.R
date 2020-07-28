library(igraph)

# wf SIGNATURE
wf.NAME = "KPC"
wf.DESC = ""
wf.ARGS = c()
wf.MIXEDGES = FALSE
wf.VERBOSE = FALSE

# wf PARAMETERS
KPC.alpha  = 0.2
KPC.length = 20
  
# wf EXECUTABLE PARAMETERS
RANK.execpath   = Global.EXEC_DIR
KPC.execpath = file.path(RANK.execpath, "kpath_centrality")     # executable path

KPC.inputFile = file.path(RANK.execpath, "R_tmp_KPC_input.gml")
KPC.outputFile = file.path(RANK.execpath, "R_tmp_KPC_output.csv")
KPC.delimiter = " "

KPC <- function(graph, alpha=KPC.alpha, kpcl=KPC.length)
{  
    if (ecount(graph) < 1) {
      V(graph)$measure = -Inf
      return(graph)
    }
      
    original_ids = V(graph)$id
  
    # to run kpath_centrality we need ids in [0, |V|-1]  
    V(graph)$id = 0:(vcount(graph)-1)
    
    # scrivi il grafo in un file_tmp, nel formato corretto per KPC (gml)
    write.graph(graph, KPC.inputFile, format = "gml")
    if (! file.exists(KPC.inputFile))
        stop(paste(wf.NAME, "Unable to write KPC.inputFile", sep = ": "))
    
    # calcola la misura usando l'eseguibile esterno
    #cat(paste("Computing Edge Centrality with ", strParams, "..", sep = ""))
    cmd = paste(KPC.execpath, KPC.inputFile, KPC.outputFile, alpha, kpcl, shQuote(KPC.delimiter, "sh"), sep = " ")
    
#    cat(sprintf("cmd is: %s\n", cmd))
    if (system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE) != 0 ) { # error        
        warning(paste(wf.NAME, "Unable to write KPC.outputFile", sep = ": "))
        cat("ERROR.\n")
        return(NULL)
    }
    #cat("OK\n")
    
    kk = read.csv(KPC.outputFile, skip=3)
    V(graph)$measure = kk[, 3]
    V(graph)$measure[ which(is.na(V(graph)$measure), arr.ind = T) ] = 0
    V(graph)$id = original_ids

    #   ALLA FINE DEI GIOCHI rimuovi i file temporanei
    if (file.exists(KPC.inputFile))
        file.remove(KPC.inputFile)

    if (file.exists(KPC.outputFile))
        file.remove(KPC.outputFile)

    return(graph)
}
