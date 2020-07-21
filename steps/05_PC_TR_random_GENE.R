library(igraph)

source("config/PIPELINE_PARAMS.R")

source("steps/05_MCRandomExperiments_only_PC/GENE/percentage_view.R")
source("steps/05_MCRandomExperiments_only_PC/GENE/performance_PV.R")
source("steps/05_MCRandomExperiments_only_PC/GENE/adjust_GSset.R")
source("steps/05_MCRandomExperiments_only_PC/GENE/plot_performance_PV.R")
source("utils/getTiming.R")

#  ------------------------ PARAMETERS ---------------------------------


DATA_DIR = file.path(Global.DATA_DIR) # dove trovo i grafi
RANKING_DIR = Global.RR_NEXTDIR        # dove trovo i ranking R2
GS.file  = Global.GS.file             # Gold Standard file
G.file   = Global.G.file              # Graph gml file
NEXT_DIR = Global.RE_PC_TR_NEXTDIR
roundTo  = Global.PC.roundTo
N = Global.N

old_data = list.files(".", pattern = "results_PC_TR" , include.dirs = T)
for (od in old_data) 
  unlink(od, recursive = T)

if (!dir.exists(NEXT_DIR)) {
  dir.create(NEXT_DIR, recursive = T)
}
if (!dir.exists(NEXT_DIR))
  stop("Unable to create NEXT_DIR directory.")

for (percs in Global.percs) {
  
  #  ------------------------ CODE ---------------------------------
  
  granularity = as.character(percs[1])
  
  GRAPH_NAME  = strsplit(basename(G.file), "\\.")[[1]][1]
  OUTPUT_DIR  = file.path(paste("results_PC_TR_random",
                                GRAPH_NAME, 
                                granularity, # current run date-time
                                sep="_"))  
 TMP_DIR = OUTPUT_DIR  

  
  # ID_TO_INDEX.FILE =  file.path(RANKING_DIR,
  #                               paste("R2_id_to_index",
  #                               GRAPH_NAME,
  #                               "rtable",
  #                               sep = "."))
  
  # FCOLORS = list()
  # FIncrementalColors =    c("#a6cee3",  # colori per curve incrementali
  #                           "#1f78b4",
  #                           "#b2df8a",
  #                           "#33a02c",
  #                           "#fb9a99",
  #                           "#e31a1c",
  #                           "#fdbf6f",
  #                           "#ff7f00",
  #                           "#cab2d6",
  #                           "#6a3d9a",
  #                           "#ffff00")
  # FDecrementalColors =    c("#b15928",  # colori per curve decrementali
  #                           "#878787",
  #                           "#c51b7d")
  # FCOLORS[['incremental']] = FIncrementalColors
  # FCOLORS[['decremental']] = FDecrementalColors
  
  t0 = Sys.time()
  
  #  Check if data directory exists
  if (!dir.exists(DATA_DIR))
    stop(paste("DATA_DIR doesn't exists: ", DATA_DIR, collapse = ""))
  
  #  Check if data directory exists
  if (!dir.exists(RANKING_DIR))
    stop(paste("RANKING_DIR doesn't exists: ", RANKING_DIR, collapse = ""))
  
  #  Need column ids for performance_PV 
  # if (! file.exists(ID_TO_INDEX.FILE)) 
  #   stop("No id_to_index mapping. Quit.")
  # id_to_index = read.table(ID_TO_INDEX.FILE, colClasses = "character")
  
  #  Create current session output directory
  dir.create(OUTPUT_DIR)
  if (!dir.exists(OUTPUT_DIR))
    stop(paste("Unable to create OUTPUT_DIR: ", OUTPUT_DIR, collapse = ""))
  
  #  read the gml graph
  G = read.graph(file=G.file, format="gml")
  
  #  read the gold standard set
  GS_1 = as.character(read.table(file=GS.file, stringsAsFactors=F)[, 1])
  complGS = adjust_GSset("decremental", GS_1, G)  # adjust gold-standard set for the decremental case
  
  # Inizializzo:
  #   RPF random performances
  #   RPV random percentage view usate per calcolare RPF
  # La struttura è quella utilizzata da draft_PC.R:
  #   RPF[[ "EC" ]][[ "static" ]][[ "incremental" ]]
  #

  rank_types = Global.rank_types
  func_types = c()
  if (length(Global.ranking.incrementals))
    func_types = c(func_types, "incremental")
  if (length(Global.ranking.decrementals))
    func_types = c(func_types, "decremental")
  
  RPF = list()
  RPV = list()
  
  for (FTYPE in func_types) {
    RPV[[ "TR" ]][[ FTYPE ]] = list()
    RPF[[ "TR" ]][[ FTYPE ]] = list()
  }
  
  #  read the topological rankings and process one at a time
  for (rf in list.files(RANKING_DIR, pattern="Y_TR.*.Rdata")) {
    
    Y.file  = file.path(RANKING_DIR, rf)
    
    #  read the rankings file
    tA = Sys.time()
    cat(sprintf("* Processing %s (granularity=%s)...\n", basename(Y.file), granularity))  
    load(Y.file)  # load Y
    getTiming(tA, Sys.time(), sprintf("reading: %s", basename(Y.file)))
    
    #  before executing the actual pipeline, we create the output directory
    #  subtree to store the results.
    tokens = strsplit(basename(Y.file), "\\.")[[1]]
    f_type    = tokens[2]  #  incremental, decremental
    
    if (!dir.exists( file.path(OUTPUT_DIR, f_type) ))
      dir.create( file.path(OUTPUT_DIR, f_type) )
    CURRENT_DIR = file.path(OUTPUT_DIR, f_type)
    
    #  We proceed with the pipeline steps:
    if (f_type == "decremental") {
      GS = complGS
    } else {
      GS = GS_1
    }
    
    if (!dir.exists(file.path(CURRENT_DIR, "RPV")))
      dir.create(file.path(CURRENT_DIR, "RPV"))
    
    if (!dir.exists(file.path(CURRENT_DIR, "RPF")))
      dir.create(file.path(CURRENT_DIR, "RPF"))
    
    
    #  PV:  random percentage view la calcolo una sola volta, è uguale per tutte le f
    tA = Sys.time()
    
    RPV[[ "TR" ]][[ f_type ]] = matrix(data=c(as.integer(0)), nrow=N, ncol=length(percs))
    rownames(RPV[[ "TR" ]][[ f_type ]]) = as.character(1:N)
    colnames(RPV[[ "TR" ]][[ f_type ]]) = c(as.character(percs))
    
    for (i in 1:N) {  #  i:  i-th random percentage view for function f
      RPV[[ "TR" ]][[ f_type ]][i, ] = percentage_view(Y[[ 1 ]][i, ], percs) 
    }
    getTiming(tA, Sys.time(), sprintf("random_percentage_view TR: %s", f_type))
    
    
    #  P:  random performance 
    tA = Sys.time()
    
    RPF[[ "TR" ]][[ f_type ]] = matrix(data=c(0.0), nrow=N, ncol=length(percs))
    rownames(RPF[[ "TR" ]][[ f_type ]]) = as.character(1:N)
    colnames(RPF[[ "TR" ]][[ f_type ]]) = c(as.character(percs))
    
    RPF[[ "TR" ]][[ f_type ]] = performance_PV(Y[[ 1 ]],                  # R2
                                               RPV[[ "TR" ]][[ f_type ]], # PV
                                               G, 
                                               GS)
    getTiming(tA, Sys.time(), sprintf("performance_PV: %s", f_type))
    
    
    # Scrivo in output
    write.table(RPV[[ "TR" ]][[ f_type ]],                       #  performance view
                file=file.path(CURRENT_DIR, 
                               "RPV",
                               sprintf("RPV.txt")),
                sep='\t',
                quote=T)
    
    write.table(RPF[[ "TR" ]][[ f_type ]],                         #  and the entire P matrix as well
                file=file.path(CURRENT_DIR,
                               "RPF",
                               sprintf("RPF.txt")),
                sep='\t',
                quote=T)
    
    write.table(round(RPF[[ "TR" ]][[ f_type ]], digits=roundTo),  #  also the rounded version used for plots
                file=file.path(CURRENT_DIR, 
                               "RPF",
                               sprintf("RPF_rounded.txt")),
                sep='\t',
                quote=T)
    
    
    
    #  4. Plot performance
    #  - single wf
    # for (i in 1:nrow(R2)) {  #  i:  function index
    #   f = row.names(R2)[i]
    #   outf = sprintf("%s.pdf", f)
    #   plot_performance_PV(percs, 
    #                       P[i, ], 
    #                       roundTo, 
    #                       title="",
    #                       color=FCOLORS[[ f_type ]][i], 
    #                       Flabel=row.names(R2)[i], 
    #                       outputFile=file.path(CURRENT_DIR, f, outf), 
    #                       legendPos="bottomright",
    #                       xlab="percentage of edges", 
    #                       ylab="fraction of relevant edges")
    # }
    
    #  - overall comparison
    # plot_performance_PV(percs, P, 
    #                     roundTo, 
    #                     title="",
    #                     color=FCOLORS[[ f_type ]], 
    #                     Flabel=row.names(R2), 
    #                     outputFile=file.path(CURRENT_DIR, "overall_performance.pdf"),
    #                     legendPos="topleft",
    #                     xlab="percentage of edges", 
    #                     ylab="fraction of relevant edges")
  }
  
  save(RPV, file=file.path(OUTPUT_DIR, "RPV.Rdata"))
  save(RPF, file=file.path(OUTPUT_DIR, "RPF.Rdata"))
  
  getTiming(t0, Sys.time(), "ALL")
  
  file.copy(from=OUTPUT_DIR, to=NEXT_DIR, recursive=T)
}

# Remove the temporary output dir
unlink("results_*", recursive=TRUE)
