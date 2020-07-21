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
NEXT_DIR = Global.RE_PC_EC_NEXTDIR
roundTo  = Global.PC.roundTo
N = Global.N

old_data = list.files(".", pattern = "results_PC_EC" , include.dirs = T)
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
  OUTPUT_DIR  = file.path(paste("results_PC_EC_random",
                                GRAPH_NAME, 
                                granularity, # current run date-time
                                sep="_"))
  TMP_DIR = OUTPUT_DIR  
  
  # OUTPUT_DIR  = file.path(paste("results_PC_EC_random",
  #                               GRAPH_NAME, 
  #                               format(Sys.time(), "%Y_%m_%d_%H%M%S"), # current run date-time
  #                               sep="_"))  
  
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
  
  for (RANKTYPE in rank_types) {
    for (FTYPE in func_types) {
      RPV[[ "EC" ]][[ RANKTYPE ]][[ FTYPE ]] = list()
      RPF[[ "EC" ]][[ RANKTYPE ]][[ FTYPE ]] = list()
    }
  }
  
  #  read the topological rankings and process one at a time
  for (rf in list.files(RANKING_DIR, pattern="Y_EC.*.Rdata")) {
    
    Y.file  = file.path(RANKING_DIR, rf)
    
    #  read the rankings file
    tA = Sys.time()
    cat(sprintf("* Processing %s (granularity=%s)...\n", basename(Y.file), granularity))  
    load(Y.file)  # load Y
    getTiming(tA, Sys.time(), sprintf("reading: %s", basename(Y.file)))
    
    #  before executing the actual pipeline, we create the output directory
    #  subtree to store the results.
    tokens = strsplit(basename(Y.file), "\\.")[[1]]
    rank_type = tokens[2]  #  static, dynamic
    f_type    = tokens[3]  #  incremental, decremental
    
    CURRENT_DIR = OUTPUT_DIR
    if (!dir.exists( file.path(OUTPUT_DIR, rank_type) ))
      dir.create( file.path(OUTPUT_DIR, rank_type) )
    
    CURRENT_DIR = file.path(OUTPUT_DIR, rank_type)
    if (!dir.exists( file.path(OUTPUT_DIR, rank_type, f_type) ))
      dir.create( file.path(OUTPUT_DIR, rank_type, f_type) )
    
    CURRENT_DIR = file.path(OUTPUT_DIR, rank_type, f_type)
    
    #  We proceed with the pipeline steps:
    
    if (f_type == "decremental") {
      GS = complGS
    } else {
      GS = GS_1
    }
    
    dir.create(file.path(CURRENT_DIR, "RPV"))
    dir.create(file.path(CURRENT_DIR, "RPF"))
    
    for (f in names(Y)) {
      
      RPV[[ "EC" ]][[ rank_type ]][[ f_type ]][[ f ]] = matrix(data=c(as.integer(0)), nrow=N, ncol=length(percs))
      RPF[[ "EC" ]][[ rank_type ]][[ f_type ]][[ f ]] = matrix(data=c(0.0), nrow=N, ncol=length(percs))
      
      rownames(RPV[[ "EC" ]][[ rank_type ]][[ f_type ]][[ f ]]) = as.character(1:N)
      rownames(RPF[[ "EC" ]][[ rank_type ]][[ f_type ]][[ f ]]) = as.character(1:N)
      colnames(RPV[[ "EC" ]][[ rank_type ]][[ f_type ]][[ f ]]) = c(as.character(percs))
      colnames(RPF[[ "EC" ]][[ rank_type ]][[ f_type ]][[ f ]]) = c(as.character(percs))
      
      tA = Sys.time()
      i = 1
      PV_f = percentage_view(Y[[ f ]][i, ], percs)  #  basta farlo solo una volta perché le classi di ranking di Y[[ f ]][1..100] hanno cardinalità fissata da R2[f, ]
      for (i in 1:N) {  #  i:  i-th random percentage view for function f
        
        RPV[[ "EC" ]][[ rank_type ]][[ f_type ]][[ f ]][i, ] = PV_f
        
      }
      rm(PV_f)
      getTiming(tA, Sys.time(), sprintf("random_percentage_view: %s", f))
      
      write.table(RPV[[ "EC" ]][[ rank_type ]][[ f_type ]][[ f ]],
                  file=file.path(CURRENT_DIR, 
                                 "RPV",
                                 sprintf("RPV_%s.txt", f)),
                  sep='\t',
                  quote=T)
      
      
      #  3. Compute P according to the gold standard set and PV
      tA = Sys.time()
      RPF[[ "EC" ]][[ rank_type ]][[ f_type ]][[ f ]] = performance_PV(Y[[ f ]], 
                                                                       RPV[[ "EC" ]][[ rank_type ]][[ f_type ]][[ f ]], 
                                                                       G, 
                                                                       GS)
      getTiming(tA, Sys.time(), sprintf("performance_PV: %s", f))
      
      
      # for (i in 1:N)  { # i:  function index
      #   f = row.names(R2)[i]
      #   write.table(t(P[i, ]),     
      #               file=file.path(CURRENT_DIR, f, sprintf("P_%s.txt", f)),
      #               sep='\t',
      #               quote=T,
      #               row.names=F)
      # }
      write.table(RPF[[ "EC" ]][[ rank_type ]][[ f_type ]][[ f ]],          #  and the entire P matrix as well
                  file=file.path(CURRENT_DIR,
                                 "RPF",
                                 sprintf("RPF_%s.txt", f)),
                  sep='\t',
                  quote=T)
      
      write.table(round(RPF[[ "EC" ]][[ rank_type ]][[ f_type ]][[ f ]],digits=roundTo),  #  also the rounded version used for plots
                  file=file.path(CURRENT_DIR, 
                                 "RPF",
                                 sprintf("RPF_rounded_%s.txt", f)),
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
  }
  
  save(RPV, file=file.path(OUTPUT_DIR, "RPV.Rdata"))
  save(RPF, file=file.path(OUTPUT_DIR, "RPF.Rdata"))
  
  getTiming(t0, Sys.time(), "ALL")
  
  file.copy(from=OUTPUT_DIR, to=NEXT_DIR, recursive=T)
  
}

# Remove the temporary output dir
unlink("results_*", recursive=TRUE)
