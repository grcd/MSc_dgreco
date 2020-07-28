
#   RANK_NOWF.R:  assigns a rank to each edge according to its weight
#                 The weight corresponds to the $measure attribute
#                 The rank is assigned to the $rank attribute.
# 
#       INPUT:  graph       graph to rank having $measure attribute
#       OUTPUT: graph       whose edges have $rank attribute

rm(list = ls()) 

library(igraph)
source("config/PIPELINE_PARAMS.R")
source("utils/getTiming.R")

RANK.execpath   = Global.EXEC_DIR
RANK.inputFile  = file.path(RANK.execpath, "RR_input.tmp")
RANK.outputFile = file.path(RANK.execpath, "RR_output.tmp")

rank_nowf <- function(G, roundTo) {
  
  V(G)$rank = as.integer(0)
  E(G)$rank = as.integer(0)
  
  #   Check if the weight attribute "measure" exists, otherwise stop.
  if (!"measure" %in% names(vertex.attributes(G)))
    stop("ERROR: $measure attribute does not exists .")
  
  
  #   round $measure to match eps precision
  V(G)$measure = round(V(G)$measure, roundTo)
  
  
  #   substitute NaN, +Inf, -Inf with -Inf, so as to rank them with the *lowest* rank.
  V(G)[which(!is.finite(V(G)$measure))]$measure = -Inf
  
  
  #   Write out weights list in RANK.inputFile
  write.table(
    V(G)$measure,
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
    toString(vcount(G)),
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
  V(G)$rank = read.table(file = RANK.outputFile)[, 1]
  
  
  #   Remove tmp files
  if (file.exists(RANK.inputFile))
    file.remove(RANK.inputFile)
  
  if (file.exists(RANK.outputFile))
    file.remove(RANK.outputFile)
  
  
  #   Induced rank on edges: loop-free solution
  vr = matrix( data = V(G)[ as.vector(t(ends(G, E(G)))) ]$rank,
               nrow = ecount(G),
               ncol = 2,
               byrow = T)
  E(G)$rank = apply(vr, 1, max)
  
  ii = order(E(G)$rank)
  E(G)$rank[ ii ] = as.integer(factor(E(G)$rank[ ii ], labels=1:length(unique(E(G)$rank))))    
  
  
  return(G)
}

# - CODE ------------------------------------------------------------------

G.file      = Global.G.file
roundTo     = Global.ranking.roundTo
NEXT_DIR    = Global.ranking.NEXTDIR
OUTPUT_DIR  = file.path("results_R1")

old_data = list.files(".", pattern = "results_R1" , include.dirs = T)
for (od in old_data) 
  unlink(od, recursive = T)

if (!dir.exists(NEXT_DIR)) {
  dir.create(NEXT_DIR, recursive = T)
}
if (!dir.exists(NEXT_DIR))
  stop("Unable to create NEXT_DIR directory.")


#  Read the graph
G = read.graph(file = G.file, format = "gml")

#  Create current session output directory
GRAPHNAME = unlist(strsplit(basename(G.file), split = "\\."))[1]
OUTPUT_DIR  = file.path(paste(
  OUTPUT_DIR,
  GRAPHNAME,
  "functional",
  #format(Sys.time(), "%Y_%m_%d_%H%M%S"),
  # current run date-time
  sep = "_"
))

dir.create(OUTPUT_DIR)
if (!dir.exists(OUTPUT_DIR))
  stop(paste("Unable to create OUTPUT_DIR: ", OUTPUT_DIR, collapse = ""))
dir.create(file.path(OUTPUT_DIR, "CSVs"))
dir.create(file.path(OUTPUT_DIR, "GMLs"))

#  initialize R matrix:  i-th row correponds to i-th wf ranking of G
R = matrix(-1, nrow = 1, ncol = ecount(G))
rownames(R) = c("R1")

#  rank according to $measure attribute (if any)
G = rank_nowf(G, roundTo)

if (is.null(G)) {
  tmpConn = file(file.path(OUTPUT_DIR, "GMLs", oname), "w+")
  writeLines(c("Error."), con = tmpConn)
  close(tmpConn)
  warning("ERROR.")
  
} else {
  
  # write the resulting ranked graph
  cat("Writing GML output ranked graph...")
  write.graph(G,
              file.path(OUTPUT_DIR,
                        "GMLs",
                        paste(GRAPHNAME, "functional", "gml", sep=".")),
              format = "gml")
  
  # Copy ranked graph in NEXT_DIR
  file.copy(from=file.path(OUTPUT_DIR,
                           "GMLs",
                           paste(GRAPHNAME, "functional", "gml", sep=".")),
            to=file.path(NEXT_DIR, "GMLs"))
  
  cat("OK.\n")
  
  R[1, ] = E(G)$rank

}


#  Write output ranking vector
R1.file.csv  = file.path(OUTPUT_DIR, 
                         "CSVs",
                          paste("R1", GRAPHNAME, "csv", sep = "."))
R1.file.rtable  = file.path(OUTPUT_DIR,
                     paste("R1", GRAPHNAME, "rtable", sep = "."))

cat("Writing R table...\n")
write.csv(R, R1.file.csv)
write.table(R, R1.file.rtable)
file.copy(from= R1.file.rtable, 
          to=   file.path(NEXT_DIR, basename(R1.file.rtable)))


#  Write $id corresponding to column
id_to_index = matrix(E(G)$id, nrow=ecount(G), ncol=1)
#rownames(id_to_index) = E(G)$id
id_to_index.file.csv  = file.path(OUTPUT_DIR,
                                  "CSVs",
                              paste("R1_id_to_index", 
                                    GRAPHNAME, 
                                    "csv",
                                    sep = "."))
id_to_index.file.rtable  = file.path(OUTPUT_DIR,
                                  paste("R1_id_to_index", 
                                        GRAPHNAME,
                                        "rtable",
                                        sep = "."))

write.csv(id_to_index, id_to_index.file.csv, row.names=F)
write.table(id_to_index, id_to_index.file.rtable, row.names=F, col.names=F)
