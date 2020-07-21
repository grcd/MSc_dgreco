#   EC-based DISTANCE MEASURE [1]
#
#   ATTENZIONE: la formulazione di De Meo et. al è CONTROINTUITIVA al massimo.
#  
#               Per loro:   proximity(i,j)  BASSA   : nodi VICINI       (L^k(e_ki) ~= L^k(e_kj))
#                                           ALTA    : nodi LONTANI
#
#               CONCLUDE(i,j) calcola d_ij = 1.0 - proximity(i,j) e dicono che le comunità sono splittate
#               per mezzo di archi (i,j) con BASSA d_ij (cioè ALTA proximity).
#
#   NOI         Vogliamo la proximity, ed in particolare:
#
#                   1.  Decremental: togli prima gli archi inter-, quindi archi con d_ij = CONCLUDE(i,j) bassa
#                       e cioè  EC_proximity(u,v) = 1 - CONCLUDE(u,v) , ALTA
#
#                   2.  Siccome la vogliamo usare complementata anche in Incremental, allora:
#                               1 - EC_proximity(u,v) = CONCLUDE(u,v) e dunque prima gli archi intra-.
#
#   References: 
#       [1]     Meo, P. D., Ferrara, E., Fiumara, G. & Provetti, A. (2013). 
#               Mixing local and global information for community detection in large networks. CoRR, abs/1303.1738. 

library(igraph)
source("config/PIPELINE_PARAMS.R")

# wf SIGNATURE
wf.NAME = "ECP"
wf.DESC = "For each undirected edge {u,v}, assigns weight(u,v) = ECProximity(u,v)"
wf.ARGS = c()
wf.MIXEDGES = TRUE
wf.VERBOSE = FALSE

# wf EXECUTABLE PARAMETERS
EC.execpath = file.path(Global.WFS_DIR, "CONCLUDE.jar")     # executable path
EC.inputFile = "R_tmp_ECP_input"
EC.outputFile = "R_tmp_ECP_output"
EC.outputFileWeights = paste("weights-", EC.outputFile, sep = "")
EC.delimiter = " "

ECP <- function(graph)
{  
    # scrivi il grafo in un file_tmp, nel formato corretto per KB
    write.graph(graph, EC.inputFile, format = "ncol")
    if (! file.exists(EC.inputFile))
        stop(paste(wf.NAME, "Unable to write KB.inputFile", sep = ": "))
    
    # calcola la misura usando l'eseguibile esterno
    strParams = ""
    #cat(paste("Computing EC_Proximity", strParams, "..", sep = ""))
    cmd = paste("java -jar", EC.execpath, EC.inputFile, EC.outputFile, shQuote(EC.delimiter, "sh"), "compute-weights-only", sep = " ")

    #cat(sprintf("cmd is: %s\n", cmd))

    tA = Sys.time()  # timing 1
    
    if (system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE) != 0 ) { # error        
        warning(paste(wf.NAME, "Unable to write EC_proximity.outputFile", sep = ": "))
        cat("ERROR.\n")
        return(NULL)
    }
    #cat("OK\n")
    
    tB = Sys.time()
    dt = round(difftime(tB, tA, units="mins"), digits=3)
    if (wf.VERBOSE)
      cat(sprintf("* TIMING: ECP weights computation took %s minutes \n", dt))
    
    tA = Sys.time()  # timing 2
    
    # se esiste aggiorna graph con output_file_tmp che contiene il grafo pesato
    graph = read.graph(EC.outputFileWeights, format = "ncol", directed=TRUE)
    graph = as.undirected(graph, mode = "collapse", edge.attr.comb = list(weight="max"))

    E(graph)$measure    = 1.0 - E(graph)$weight
    graph               = remove.edge.attribute(graph, "weight")
    
    tB = Sys.time()
    dt = round(difftime(tB, tA, units="mins"), digits=3)
    #cat(sprintf("* TIMING: ECP re-reading took %s minutes \n", dt))
    
    #  ALLA FINE DEI GIOCHI rimuovi i file temporanei
    if (file.exists(EC.inputFile))
        file.remove(EC.inputFile)

    if (file.exists(EC.outputFile))
        file.remove(EC.outputFile)

    if (file.exists(EC.outputFileWeights))
        file.remove(EC.outputFileWeights)

    
    return(graph)
}
