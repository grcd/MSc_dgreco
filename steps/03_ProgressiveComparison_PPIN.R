
rm(list=ls())

library(igraph) 
source("config/PIPELINE_PARAMS.R")
source("steps/03_ProgressiveComparison_PPIN/percentage_view.R")
source("steps/03_ProgressiveComparison_PPIN/performance_PV.R")  
source("steps/03_ProgressiveComparison_PPIN/adjust_GSset.R")    
source("steps/03_ProgressiveComparison_PPIN/read_GSset.R")      
source("steps/03_ProgressiveComparison_PPIN/plot_performance_PV.R")
source("utils/getTiming.R")

#  ------------------------ PARAMETERS ---------------------------------

DATA_DIR = file.path(Global.DATA_DIR) # dove trovo i grafi
RANKING_DIR = Global.ranking.NEXTDIR  # dove trovo i ranking R2
GS.file  = Global.GS.file             # Gold Standard file, Complx file
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
  
  OUTPUT_DIR  = file.path(paste("results_PC",
                                GRAPH_NAME, 
                                as.character(percs[1]), # current run date-time
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
  GS_1 = read_GSset(file=GS.file)
  complGS = adjust_GSset("decremental", GS_1, G)  # adjust gold-standard set for the decremental case
  
  # GS_1 = as.character(read.table(file=GS.file, stringsAsFactors=F)[, 1])
  # complGS = adjust_GSset("decremental", GS_1, G)  # adjust gold-standard set for the decremental case
  
  #  read the topological rankings and process one at a time
  for (rf in list.files(RANKING_DIR, pattern=paste("R2", GRAPH_NAME, sep=".")) ) {
    
    GR = list()                             # ranked graphs, 5?? differenza
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
    
    #  also we need to load the corresponding ranked graphs
    for (f in row.names(R2)) {
      gr.name = paste(GRAPH_NAME, rank_type, f, "gml", sep = ".")
      gr.file = file.path(RANKING_DIR, "GMLs", gr.name)
      GG = read.graph(file=gr.file, format="gml")
      GR[[ f ]] = GG
    }
    
    
    #  We proceed with the pipeline steps:
    
    #  1.  Initialize PV (Percentage Views) and P (Performance) matrices
    PV = matrix(data=c(0.0), nrow=nrow(R2), ncol=length(percs))
    rownames(PV) = rownames(R2)
    colnames(PV) = c(as.character(percs))
    
    #  2. Compute PV according to percs for each ranking a
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
    }
    write.table(PV,          #  and the entire PV matrix as well
                file=file.path(CURRENT_DIR, "PV.txt"),
                sep='\t',
                quote=T)
    
    #  3. Compute P according to the gold standard set and PV
    
    SUBG_DIR = ''
    if (SAVE_SUBGRAPHS) {
      for (f in rownames(PV))
        dir.create(file.path(CURRENT_DIR, "subgraphs", f), recursive = T)
      SUBG_DIR = file.path(CURRENT_DIR, "subgraphs")
    }
      
    tA = Sys.time()
    P = performance_PV(PV, GR, GS, ft=f_type, SUBG_DIR=SUBG_DIR, GRAPH_NAME=GRAPH_NAME, verbose=Global.PC.verbose)
    getTiming(tA, Sys.time(), sprintf("performance_PV"))
    
    #  PPI:  In this case, P is a list containing the following matrices:
    #           $R      [f, ]         R[f, i]=Recall at the i-th percentage view according to f
    #           $P      [f, ]         P[f, i]=Precision at the i-th percentage view according to f
    #           $Fm     [f, ]         Fm[f, i]=F-Measure at the i-th percentage view according to f
    #       and the following lists:
    #           $CK     [[ f ]]       CK[[ f ]][[ i ]]=Complexes matched at the i-th percentage view of f
    #           $id_max [[ f ]]       id_max[[ f ]]=max i, such that F[f, i] is maximum among all j in PV[f, ]
    #
    
    ## Single f results
    for (f in rownames(PV))  { # i:  function index
      write.table(t(P$R[f, ]),     
                  file=file.path(CURRENT_DIR, f, sprintf("R_%s.txt", f)),
                  sep='\t',
                  quote=T,
                  row.names=F)
      
      write.table(t(P$P[f, ]),     
                  file=file.path(CURRENT_DIR, f, sprintf("P_%s.txt", f)),
                  sep='\t',
                  quote=T,
                  row.names=F)
      
      write.table(t(P$Fm[f, ]),     
                  file=file.path(CURRENT_DIR, f, sprintf("Fm_%s.txt", f)),
                  sep='\t',
                  quote=T,
                  row.names=F)
    }
    
    ## Global results
    # Recall      
    write.table(P$R,          #  and the entire P matrix as well
                file=file.path(CURRENT_DIR, "R.txt"),
                sep='\t',
                quote=T) 
    
    write.table(round(P$R,digits=roundTo),  #  also the rounded version used for plots
                file=file.path(CURRENT_DIR, "R_rounded.txt"),
                sep='\t',
                quote=T) 
    
    # Precision      
    write.table(P$P,          #  and the entire P matrix as well
                file=file.path(CURRENT_DIR, "P.txt"),
                sep='\t',
                quote=T) 
    
    write.table(round(P$P,digits=roundTo),  #  also the rounded version used for plots
                file=file.path(CURRENT_DIR, "P_rounded.txt"),
                sep='\t',
                quote=T)
    
    # F-Measure      
    write.table(P$Fm,          #  and the entire P matrix as well
                file=file.path(CURRENT_DIR, "Fm.txt"),
                sep='\t',
                quote=T) 
    
    write.table(round(P$Fm,digits=roundTo),  #  also the rounded version used for plots
                file=file.path(CURRENT_DIR, "Fm_rounded.txt"),
                sep='\t',
                quote=T)
    
    # Additional data: CK, id_max
    global_CK = P$CK
    global_id_max = P$id_max
    
    save(global_CK,     file=file.path(CURRENT_DIR, "CK_matchedComplexes.Rdata"))
    save(global_id_max, file=file.path(CURRENT_DIR, "id_max.RData"))
    
    
    #  4. Plot performance
    #  - single wf
    for (f in rownames(PV)) {  #  i:  function index
      outf_R = sprintf("R_%s.pdf", f)
      plot_performance_PV(percs, 
                          P$R[f, ], 
                          roundTo, 
                          title="",
                          color=FCOLORS[[ f_type ]][i], 
                          Flabel=f, 
                          outputFile=file.path(CURRENT_DIR, f, outf_R), 
                          legendPos="bottomright",
                          xlab="percentage of edges", 
                          ylab="Recall")
      
      outf_P = sprintf("P_%s.pdf", f)
      plot_performance_PV(percs, 
                          P$P[f, ], 
                          roundTo, 
                          title="",
                          color=FCOLORS[[ f_type ]][i], 
                          Flabel=f, 
                          outputFile=file.path(CURRENT_DIR, f, outf_P), 
                          legendPos="bottomright",
                          xlab="percentage of edges", 
                          ylab="Precision")
      
      outf_Fm = sprintf("Fm_%s.pdf", f)
      plot_performance_PV(percs, 
                          P$Fm[f, ], 
                          roundTo, 
                          title="",
                          color=FCOLORS[[ f_type ]][i], 
                          Flabel=f, 
                          outputFile=file.path(CURRENT_DIR, f, outf_Fm), 
                          legendPos="bottomright",
                          xlab="percentage of edges", 
                          ylab="F-Measure")
      
    }
    
    #  - overall comparison
    # Recall
    plot_performance_PV(percs, P$R, 
                        roundTo, 
                        title="",
                        color=FCOLORS[[ f_type ]], 
                        Flabel=row.names(PV), 
                        outputFile=file.path(CURRENT_DIR, "R_overall_performance.pdf"),
                        legendPos="topleft",
                        xlab="percentage of edges", 
                        ylab="Recall")
    
    # Precision
    plot_performance_PV(percs, P$P, 
                        roundTo, 
                        title="",
                        color=FCOLORS[[ f_type ]], 
                        Flabel=row.names(PV), 
                        outputFile=file.path(CURRENT_DIR, "P_overall_performance.pdf"),
                        legendPos="topleft",
                        xlab="percentage of edges", 
                        ylab="Precision")
    
    # F-measure
    plot_performance_PV(percs, P$Fm, 
                        roundTo, 
                        title="",
                        color=FCOLORS[[ f_type ]], 
                        Flabel=row.names(PV), 
                        outputFile=file.path(CURRENT_DIR, "Fm_overall_performance.pdf"),
                        legendPos="topleft",
                        xlab="percentage of edges", 
                        ylab="F-Measure")
    
  }
    
  cat(sprintf("Copying data into %s\n", NEXT_DIR))
  dir.create(file.path(NEXT_DIR, OUTPUT_DIR))
  file.copy(from= OUTPUT_DIR, 
            to=   file.path(NEXT_DIR),
            recursive=T)
}

getTiming(t0, Sys.time(), sprintf("%s ALL", as.character(percs[[1]])))

#  ------------------------ CODE 

# Remove the temporary output dir
unlink("results_*", recursive=TRUE)
