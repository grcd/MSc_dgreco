

rm(list = ls()) 

#WORKINGDIR = "/Users/danielegreco/Desktop/TESI_GRECO/CODICE/ARCHI_PPI/Pipeline_Ranking"
# WORKINGDIR="/Volumes/TERABOB/TESI_LM_BACKUP_MACCHINA_DIP_JUL18/ARCHI_DIP_MacOSX/Pipeline_Ranking/"
# setwd(WORKINGDIR)

library(igraph)
source("config/PIPELINE_PARAMS.R")
source(sprintf("steps/01_ranking/%s/rank_static.R", Global.ElementType))
source(sprintf("steps/01_ranking/%s/rank_dynamic.R", Global.ElementType))


#  ------------------------ LOCAL PARAMETERS -----------------------

G.file      = Global.G.file
WFS_DIR     = Global.WFS_DIR
NEXT_DIR    = Global.ranking.NEXTDIR

rank_types  = Global.rank_types
roundTo     = Global.ranking.roundTo

incrementals = Global.ranking.incrementals
decrementals = Global.ranking.decrementals

#  ------------------------ HELPER FUNCTIONS ---------------------

# add_cytoscape_import <- function(file, name) {
#   cmd = paste("network import file file=", "\"", file, "\"" ," indexColumnSourceInteraction=1 indexColumnTargetInteraction=2", "\n", sep = "")
#   cmd = paste(cmd, "network rename name=", "\"", name, "\"", " sourceNetwork=", "\"", file, "\"", "\n", sep = "")
#   cmd = paste(cmd, "layout force-directed network=", "\"", name, "\"", "\n", sep = "")
#   cmd = paste(cmd, "vizmap apply styles=PINK", "\n\n", sep = "")
#   return(cmd)
# }


#  ------------------------ CODE ---------------------------------

##### 1. Delete previous (temporary) results directory
old_data = list.files(".", pattern = "results_R2" , include.dirs = T)
for (od in old_data) 
  unlink(od, recursive = T)

if (!dir.exists(NEXT_DIR)) {
  dir.create(NEXT_DIR, recursive = T)
}
if (!dir.exists(NEXT_DIR))
  stop("Unable to create NEXT_DIR directory.")


##### 2. For each type of ranking specified in PIPELINE_PARAMS.R
for (rank_type in rank_types) {

	### Temporary results dir
  OUTPUT_DIR  = file.path("results_R2")
  
  ### Get the list of weight functions available, each one of them being a separate R script
  WFSET = list.files(WFS_DIR, pattern = "[[:alnum:]]\\.R$")
  if (length(WFSET) == 0)
    stop("No weight functions available. Exit.\n")
  
  ###  Read the graph
  if (!file.exists(G.file))
    stop("Graph file does not exists. Exit.")
  G1 = read.graph(file = G.file, format = "gml")
  GRAPH_NAME = unlist(strsplit(basename(G.file), split = "\\."))[1]
  
  ### Create current session temporary output directory
  OUTPUT_DIR  = file.path(paste(
    OUTPUT_DIR,
    GRAPH_NAME,
    rank_type,
#    format(Sys.time(), "%Y_%m_%d_%H%M%S"), # current run date-time
    sep = "_"
  ))
  dir.create(OUTPUT_DIR)
  if (!dir.exists(OUTPUT_DIR))
    stop(paste("Unable to create OUTPUT_DIR: ", OUTPUT_DIR, collapse = ""))
  dir.create(file.path(OUTPUT_DIR, "CSVs"))
  dir.create(file.path(OUTPUT_DIR, "GMLs"))
  
  ###  Initialize the results (R) matrix:  
  #     i-th row corresponds to the i-th topological (WFSET[i]) ranking of G
  #     j-th col corresponds to the j-th edge
  #     R[i,j] = rank of edge e_j, according to weight function w_i = WFSET[i]
  R = matrix(-1, nrow = length(WFSET), ncol = ecount(G1))
  rownames(R) = rep("undefined", nrow(R))     # wf name
 
  ####  perform ranking of G for each wf in WFSET
  tA = Sys.time()
  i = 1
  for (wf.script in WFSET) {
    
    #  no additional arguments for each weight function
    wf.args = list()
    
    #  To avoid re-reading the graph at each iteration, we work with a copy of it
    G = G1
    
    #  load the weight function
    source(file.path(WFS_DIR, wf.script))
  
    #  check wf signature 
    if (! exists("wf.NAME")) {
      warning("ERROR: invalid signature, no wf.NAME provided. ")
      next
    }
    
    if (! exists("wf.MIXEDGES")) { 
      warning("ERROR: invalid signature, no wf.MIXEDGES provided.")
      next
    }
    
    #  wf.NAME must be in the selected weight functions, specified in PIPELINE_PARAMS.R
    if (!(wf.NAME %in% incrementals) && !(wf.NAME %in% decrementals))
      next
    
    #  SPECIAL CASES: no dynamic ranking for these (see explanation below)
    if ((rank_type == "dynamic") & 
        (wf.NAME %in% c("EC",           # Edge Centrality binary crashes for "small" graphs (reported)
                        "kb_param_sym", # kb_* implementation is slow for a single run
                        "kb_rec_max",   # (as above)
                        "kb_rec_min",   # (as above)
                        "ECP")))        # ECP is also too slow for a single run?
      next
    
    #  Once here, we can say that weight function signature is OK and proceed
    cat(paste("\n-- WEIGHT FUNCTION: ", wf.NAME, "\n", sep = ""))
    cat(paste("   ", wf.DESC , "\n", sep = ""))
    
    oname = paste(GRAPH_NAME, rank_type, wf.NAME, "gml", sep = ".")   # output graph file
    cat("\n")
    cat(paste("INPUT : ", GRAPH_NAME, "\n", sep = ""))
    cat(paste("OUTPUT: ", oname, "\n", sep = ""))
    
    #  Ranking 
    cat("Ranking...\n")
    
    if (rank_type == "static") {
      tA = Sys.time()
      G = rank_static(G, wf.NAME, wf.args, wf.MIXEDGES, roundTo, verbose=Global.ranking.verbose)
      getTiming(tA, Sys.time(), "static ranking")
      
    } else if (rank_type == "dynamic") {
      tA = Sys.time()
      G = rank_dynamic(G, wf.NAME, wf.args, wf.MIXEDGES, roundTo, verbose=Global.ranking.verbose)
      getTiming(tA, Sys.time(), "dynamic ranking")
      
    } else {
      # do nothing
      warning("WARNING: Invalid rank_type, nothing done.")
      next
    }
    
    # write the ranked graph
    if (is.null(G)) { ## in case of error...
      tmpConn = file(file.path(OUTPUT_DIR, "GMLs", oname), "w+")
      writeLines(c("Error."), con = tmpConn)
      close(tmpConn)
      warning("ERROR.")
      
      R = R[-i, ]     # ...delete from the ranking matrix R the row correspoding to this weight function
      i = i - 1       # decrement the row index accordingly.
      
    } else { ## OK!
      
      # write the resulting ranked graph
      cat("Writing GML output ranked graph...")
      write.graph(G,
                  file.path(OUTPUT_DIR, "GMLs", oname),
                  format = "gml")
      cat("OK.\n")
      
      # update wf.NAME row of R with edges rank values
      rownames(R)[i] = wf.NAME
      R[i, ] = E(G)$rank
      i = i + 1
      
    }
    
  }
  
  # Move ranked graphs from temporary output dir to NEXT_DIR
  file.copy(from=file.path(OUTPUT_DIR, "GMLs"),
            to=NEXT_DIR,
            recursive = T)

  #  remove "undefined" rows from the ranking matrix
  delrows = which(rownames(R) == "undefined")
  if (length(delrows) > 0) {
    #R = R[-delrows, ]
    R = matrix(R[-delrows, ],  # tutto perch?? se estraggo una sola riga diventa un vettore e non pi?? una matrice
               nrow=nrow(R)-length(delrows), 
               ncol=ncol(R), 
               dimnames = list(rownames(R)[-delrows]))
  }
  
  # Write the ranking matrix R in multiple formats
  R2.file.csv  = file.path(OUTPUT_DIR,
                           "CSVs",
                           paste("R2", GRAPH_NAME, rank_type, "csv", sep = "."))
  
  R2.file.rtable  = file.path(OUTPUT_DIR,
                              paste("R2", GRAPH_NAME, rank_type, "rtable", sep = "."))
  
  cat("Writing R table...\n")
  write.csv(R, R2.file.csv)
  write.table(R, R2.file.rtable)
  
  
  # Split R in different R with only "incremental" functions and 
  # and   R with only "decremental" functions (needed for subsequent pipeline steps)
  
  #   R2  incremental
  wfs = intersect(incrementals, rownames(R))
  if (length(wfs) > 0) {
    #wfs = incrementals[incrementals %in% rownames(R)]
    R2.file.csv  = file.path(OUTPUT_DIR,
                             "CSVs",
                         paste("R2", 
                               GRAPH_NAME, 
                               rank_type, 
                               "incremental",
                               "csv",
                               sep = "."))
    
    R2.file.rtable  = file.path(OUTPUT_DIR,
                         paste("R2", 
                               GRAPH_NAME, 
                               rank_type, 
                               "incremental",
                               "rtable",
                               sep = "."))
    
    # write.csv(R[wfs, ], R2.file.csv)
    # write.table(R[wfs, ], R2.file.rtable)
  
    write.csv(matrix(R[wfs, ], nrow=length(wfs), ncol=ncol(R), dimnames = list(c(wfs))), 
              R2.file.csv)

    write.table(matrix(R[wfs, ], nrow=length(wfs), ncol=ncol(R), dimnames = list(c(wfs))), 
                R2.file.rtable)
    file.copy(from= R2.file.rtable, 
              to=   file.path(NEXT_DIR, basename(R2.file.rtable)))
  }
  
  #   R2  decremental
  wfs = intersect(decrementals, rownames(R))
  if (length(wfs) > 0) {
    #wfs = decrementals[decrementals %in% rownames(R)]
    R2.file.csv  = file.path(OUTPUT_DIR,
                             "CSVs",
                         paste("R2", 
                               GRAPH_NAME, 
                               rank_type, 
                               "decremental",
                               "csv",
                               sep = "."))
    R2.file.rtable  = file.path(OUTPUT_DIR,
                         paste("R2", 
                               GRAPH_NAME, 
                               rank_type, 
                               "decremental",
                               "rtable",
                               sep = "."))
    
    # write.csv(R[wfs, ], R2.file.csv)
    # write.table(R[wfs, ], R2.file.rtable)
    write.csv(matrix(R[wfs, ], nrow=length(wfs), ncol=ncol(R), dimnames = list(c(wfs))), 
              R2.file.csv)

    write.table(matrix(R[wfs, ], nrow=length(wfs), ncol=ncol(R), dimnames = list(c(wfs))), 
                R2.file.rtable)
    file.copy(from= R2.file.rtable, 
              to=   file.path(NEXT_DIR, basename(R2.file.rtable)))
  }  
  
  #  Write $id corresponding to column
  id_to_index = matrix(E(G)$id, nrow=ecount(G), ncol=1)
  id_to_index.file.csv  = file.path(OUTPUT_DIR,
                                    "CSVs",
                       paste("R2_id_to_index", 
                             GRAPH_NAME,
                             "csv",
                             sep = "."))
  id_to_index.file.rtable  = file.path(OUTPUT_DIR,
                                paste("R2_id_to_index", 
                                      GRAPH_NAME,
                                      "rtable",
                                      sep = "."))
  
  write.csv(id_to_index, id_to_index.file.csv, row.names=F)
  write.table(id_to_index, id_to_index.file.rtable, row.names=F, col.names=F)
  file.copy(from= id_to_index.file.rtable, 
            to=   file.path(NEXT_DIR, basename(id_to_index.file.rtable)))
  

  file.copy(from= file.path(OUTPUT_DIR, "CSVs"),
            to=   NEXT_DIR,
            recursive=T)
  
  tB = Sys.time()
  dt = round(difftime(tB, tA, units="mins"), digits=3)
  cat(sprintf("* TIMING: pipeline ranking took %s minutes \n", dt))
  
}

# Remove the temporary output dir
unlink("results_*", recursive=TRUE)
