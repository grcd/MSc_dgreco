
rm(list=ls())
source("config/PIPELINE_PARAMS.R")
source("steps/03_ProgressiveComparison_GENE/percentage_view.R")
source("steps/03_ProgressiveComparison_GENE/performance_PV.R")
source("steps/03_ProgressiveComparison_GENE/adjust_GSset.R")
source("steps/03_ProgressiveComparison_GENE/plot_performance_PV.R")
source("utils/getTiming.R")

#  ------------------------ PARAMETERS ---------------------------------

DATA_DIR = file.path(Global.DATA_DIR) # dove trovo i grafi
RANKING_DIR = Global.ranking.NEXTDIR  # dove trovo i ranking R2
GS.file  = Global.GS.file             # Gold Standard file
G.file   = Global.G.file              # Graph gml file
NEXT_DIR = Global.PC_NEXTDIR
SAVE_SUBGRAPHS = Global.PC.saveSubgraphs
roundTo  = Global.PC.roundTo 

old_data = list.files(".", pattern = "results_PC" , include.dirs = T)
for (od in old_data) 
  unlink(od, recursive = T)

if (!dir.exists(NEXT_DIR)) {
  dir.create(NEXT_DIR, recursive = T)
}
if (!dir.exists(NEXT_DIR))
  stop("Unable to create NEXT_DIR directory.")


for (percs in Global.percs) {
  #  ------------------------ CODE ---------------------------------
  
  GRAPH_NAME  = strsplit(basename(G.file), "\\.")[[1]][1]
  # OUTPUT_DIR  = file.path(paste("results_PC",
  #                               GRAPH_NAME, 
  #                               format(Sys.time(), "%Y_%m_%d_%H%M%S"), # current run date-time
  #                               sep="_"))  
  
  granularity = percs[1]
  
  OUTPUT_DIR  = file.path(paste("results_PC",
                                GRAPH_NAME, 
                                as.character(granularity), # current run date-time
                                sep="_"))
  TMP_DIR = OUTPUT_DIR  
  
  ID_TO_INDEX.FILE =  file.path(RANKING_DIR,
                                paste("R2_id_to_index",
                                      GRAPH_NAME,
                                      "rtable",
                                      sep = "."))
  
  FCOLORS = list()
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
  FCOLORS[['incremental']] = Global.FIncrementalColors
  FCOLORS[['decremental']] = Global.FDecrementalColors
  
  t0 = Sys.time()
  
  #  Check if data directory exists
  if (!dir.exists(DATA_DIR))
    stop(paste("DATA_DIR doesn't exists: ", DATA_DIR, collapse = ""))
  
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
  
  #  read the topological rankings and process one at a time
  for (rf in list.files(RANKING_DIR, pattern=paste("R2", GRAPH_NAME, sep=".")) ) {
    
    R2.file  = file.path(RANKING_DIR, rf)
    
    #  read the rankings file
    tA = Sys.time()
    cat(sprintf("* Processing %s...\n", basename(R2.file)))  
    R2 = as.matrix(read.table(R2.file))
    getTiming(tA, Sys.time(), sprintf("reading R2.file: %s", basename(R2.file)))
    
    #  before executing the actual pipeline, we create the output directory
    #  subtree to store the results.
    tokens = strsplit(basename(R2.file), "\\.")[[1]]
    rank_type = tokens[3]  #  static, dynamic
    f_type    = tokens[4]  #  incremental, decremental
    
    CURRENT_DIR = OUTPUT_DIR
    if (!dir.exists( file.path(OUTPUT_DIR, rank_type) ))
      dir.create( file.path(OUTPUT_DIR, rank_type) )
    
    CURRENT_DIR = file.path(OUTPUT_DIR, rank_type)
    if (!dir.exists( file.path(OUTPUT_DIR, rank_type, f_type) ))
      dir.create( file.path(OUTPUT_DIR, rank_type, f_type) )
    
    CURRENT_DIR = file.path(OUTPUT_DIR, rank_type, f_type)
    for (f in row.names(R2))  
      dir.create(file.path(CURRENT_DIR, f))
    
    
    #  We proceed with the pipeline steps:
    
    #  1.  Initialize PV (Percentage Views) and P (Performance) matrices
    PV = matrix(data=c(0.0), nrow=nrow(R2), ncol=length(percs))
    P  = matrix(data=c(0.0), nrow=nrow(R2), ncol=length(percs))
    rownames(PV) = rownames(R2)
    rownames(P) = rownames(R2)
    colnames(PV) = c(as.character(percs))
    colnames(P) = c(as.character(percs))
    
    
    #  2. Compute PV according to percs for each ranking
    if (f_type == "decremental") {
      GS = complGS
    } else {
      GS = GS_1
    }
    
    
    for (i in 1:nrow(R2)) {  #  i:  function index
      tA = Sys.time()
      PV[i, ] = percentage_view(R2[i, ], percs)
      getTiming(tA, Sys.time(), sprintf("percentage_view: %s", rownames(R2)[i]))
      
      f = rownames(R2)[i]
      write.table(t(PV[i, ]),   #  we save each PV[i, ] separately (crash safe)
                  file=file.path(CURRENT_DIR, f, sprintf("PV_%s.txt", f)),
                  sep='\t',
                  quote=T,
                  row.names=F)
      
      #  G_f is G ranked according to f
      gname = paste(GRAPH_NAME, rank_type, f, "gml", sep = ".")
      G_f = read.graph(file=file.path(Global.ranking.NEXTDIR, "GMLs", gname), 
                       format="gml")
      
      E(G_f)$pvrank = as.integer(0)
      for (pi in 1:ncol(PV)) {
        if (pi == 1) {
          E(G_f)[ which( E(G_f)$rank <= PV[f, pi]  ) ]$pvrank = pi
        } else {
          E(G_f)[ which( (E(G_f)$rank > PV[f, pi-1]) && (E(G_f)$rank <= PV[f, pi]) ) ]$pvrank = pi
        }
      }
      
      # Write G_f for subsequent plotting using Cytoscape and my homemade plugin
      gname = paste(GRAPH_NAME, rank_type, f, paste("PV", granularity, sep="_"), "gml", sep = ".")
      cat(sprintf("%s\n", gname))
      write.graph(G_f,
                  file=file.path(CURRENT_DIR, f, gname), 
                  format="gml")
      
    }
    write.table(PV,          #  and the entire PV matrix as well
                file=file.path(CURRENT_DIR, "PV.txt"),
                sep='\t',
                quote=T)
    
    
    #  3. Compute P according to the gold standard set and PV
    tA = Sys.time()
    P = performance_PV(R2, PV, G, GS)
    getTiming(tA, Sys.time(), sprintf("performance_PV"))
    
    
    for (i in 1:nrow(P))  { # i:  function index
      f = row.names(R2)[i]
      write.table(t(P[i, ]),     
                  file=file.path(CURRENT_DIR, f, sprintf("P_%s.txt", f)),
                  sep='\t',
                  quote=T,
                  row.names=F)
    }
    write.table(P,          #  and the entire P matrix as well
                file=file.path(CURRENT_DIR, "P.txt"),
                sep='\t',
                quote=T) 
    
    write.table(round(P,digits=roundTo),  #  also the rounded version used for plots
                file=file.path(CURRENT_DIR, "P_rounded.txt"),
                sep='\t',
                quote=T) 
    
    
    #  4. Plot performance
    #  - single wf
    for (i in 1:nrow(R2)) {  #  i:  function index
      f = row.names(R2)[i]
      outf = sprintf("%s.pdf", f)
      plot_performance_PV(percs, 
                          P[i, ], 
                          roundTo, 
                          title="",
                          color=FCOLORS[[ f_type ]][i], 
                          Flabel=row.names(R2)[i], 
                          outputFile=file.path(CURRENT_DIR, f, outf), 
                          legendPos="bottomright",
                          xlab="percentage of edges", 
                          ylab="fraction of relevant edges")
    }
    
    #  - overall comparison
    plot_performance_PV(percs, P, 
                        roundTo, 
                        title="",
                        color=FCOLORS[[ f_type ]], 
                        Flabel=row.names(R2), 
                        outputFile=file.path(CURRENT_DIR, "overall_performance.pdf"),
                        legendPos="topleft",
                        xlab="percentage of edges", 
                        ylab="fraction of relevant edges")
    
  }
  
  getTiming(t0, Sys.time(), sprintf("%s ALL", as.character(percs[[1]])))
  
  #  ------------------------ CODE 
  
  cat(sprintf("Copying data into %s\n", NEXT_DIR))
  dir.create(file.path(NEXT_DIR, OUTPUT_DIR))
  file.copy(from= OUTPUT_DIR, 
            to=   file.path(NEXT_DIR),
            recursive=T)
  
}

unlink("results_*", recursive=TRUE)
