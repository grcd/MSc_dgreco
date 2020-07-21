
#  PIPELINE  RANDOM EXPERIMENTS GC + SIGNIFICANCE GC

rm(list = ls())

library(igraph)

source("config/PIPELINE_PARAMS.R")
source("steps/06_GC_EC_TR_MCRandomExperiments_and_Significance/compute_singleF_random_Khaus.R")         # step 4
source("steps/06_GC_EC_TR_MCRandomExperiments_and_Significance/significance_test.R")                    
source("steps/06_GC_EC_TR_MCRandomExperiments_and_Significance/adjust_ranking.R")
source("utils/getTiming.R")

# PARAMETERS
DATA_DIR = file.path(Global.DATA_DIR, Global.G.name)
NEXT_DIR = Global.SIGNIFICANCE_GC_NEXTDIR
roundTo = Global.ranking.roundTo
Ts = Global.SIGNIFICANCE_GC.Tvalues

N = Global.N
seed = Global.seed

G.file        = Global.G.file
RANKING_DIR   = Global.ranking.NEXTDIR
A_DIR         = Global.GC_NEXTDIR
PERMS_FILE    = Global.PERMS_FILE
RANDRANKS_DIR = Global.RR_NEXTDIR


old_data = list.files(".", pattern = "results_GC" , include.dirs = T)
for (od in old_data) 
  unlink(od, recursive = T)


if (!dir.exists(NEXT_DIR)) {
  dir.create(NEXT_DIR, recursive = T)
}
if (!dir.exists(NEXT_DIR))
  stop("Unable to create NEXT_DIR directory.")


random_types = c("EC", "TR")
rank_types = Global.rank_types
func_types = c()
if (length(Global.ranking.incrementals))
  func_types = c(func_types, "incremental")
if (length(Global.ranking.decrementals))
  func_types = c(func_types, "decremental")


#  Colori
FIncrementalColors = Global.FIncrementalColors 
FDecrementalColors = Global.FDecrementalColors


# CODE ----------------------------------------------------------------------
tA = Sys.time()

if (!dir.exists(DATA_DIR))
  stop(paste("DATA_DIR doesn't exists: ", DATA_DIR, collapse = ""))

if (!dir.exists(RANDRANKS_DIR))
  stop(paste("RANDRANKS_DIR doesn't exists: ", RANDRANKS_DIR, collapse = ""))

if (!file.exists(G.file))
  stop("No graph is given. Quit.")

# Read graph
G = read.graph(G.file, format = 'gml')
GRAPHNAME = strsplit(basename(G.file), "\\.")[[1]][1]

# Load it ONCE: TR
Y_filename = paste("Y_TR.incremental", paste("N", Global.N, sep=""), "Rdata", sep=".")

if (file.exists(file.path(RANDRANKS_DIR, Y_filename))) {
  cat("* Loading TR random rankings: incremental\n")
  load(file.path(RANDRANKS_DIR, Y_filename))
  Y_TR.inc = Y
}

Y_filename = paste("Y_TR.decremental", paste("N", Global.N, sep=""), "Rdata", sep=".")

if (file.exists(file.path(RANDRANKS_DIR, Y_filename))) {
  cat("* Loading TR random rankings: decremental\n")
  load(file.path(RANDRANKS_DIR, Y_filename))
  Y_TR.dec = Y
}

# Load it ONCE: EC
if ("static" %in% Global.rank_types) {
	
	Y_filename = paste("Y_EC.static.incremental", paste("N", Global.N, sep=""), "Rdata", sep=".")
	
  if (file.exists(file.path(RANDRANKS_DIR, Y_filename))) {
    cat("* Loading EC random rankings: static.incremental\n")
    load(file.path(RANDRANKS_DIR, Y_filename))
    Y_EC.static.inc = Y
  }
 
	Y_filename = paste("Y_EC.static.decremental", paste("N", Global.N, sep=""), "Rdata", sep=".")
	
  if (file.exists(file.path(RANDRANKS_DIR, Y_filename))) {
    cat("* Loading EC random rankings: static.decremental\n")
    load(file.path(RANDRANKS_DIR, Y_filename))
    Y_EC.static.dec = Y
  }
}

if ("dynamic" %in% Global.rank_types) {

	Y_filename = paste("Y_EC.dynamic.incremental", paste("N", Global.N, sep=""), "Rdata", sep=".")
	
  if (file.exists(file.path(RANDRANKS_DIR, Y_filename))) {
    cat("* Loading EC random rankings: dynamic.incremental\n")
    load(file.path(RANDRANKS_DIR, Y_filename))
    Y_EC.dynamic.inc = Y
  }

	Y_filename = paste("Y_EC.dynamic.decremental", paste("N", Global.N, sep=""), "Rdata", sep=".")
	
  if (file.exists(file.path(RANDRANKS_DIR, Y_filename))) {
    cat("* Loading EC random rankings: dynamic.decremental\n")
    load(file.path(RANDRANKS_DIR, Y_filename))
    Y_EC.dynamic.dec = Y
  }
}

# Load it ONCE:  R1
cat("* Loading R1 functional ranking\n")
R1.file = file.path(RANKING_DIR, 
                    paste("R1", 
                          GRAPHNAME, 
                          "rtable", 
                          sep="."))             
R1 = read.table(R1.file)                    # R1: functional ranking
inv_R1 = adjust_ranking("decremental", R1)  # adjust in case of decremental measures

for (RRTYPE in random_types) {
  OUTPUT_DIR = file.path(paste("results_GC",        # results_GC_WGN_EC_N100_<date>
                               #format(Sys.time(), "%Y_%m_%d_%H%M%S"),
                               GRAPHNAME,
                               RRTYPE,
                               paste("N", N, sep=""), 
                               sep = "_"))
  TMP_DIR = OUTPUT_DIR

  if (!dir.exists(OUTPUT_DIR)) {
    dir.create(OUTPUT_DIR, recursive = T)
  } else {
    warning("OUTPUT_DIR exists yet, specify another results directory.")
  }
  odir = OUTPUT_DIR
  
  
  for (RANKTYPE in rank_types) {
    OUTPUT_DIR = file.path(odir, RANKTYPE)  # results_TR_<date>/static
    
    dir.create(OUTPUT_DIR, recursive = T)
    if (!dir.exists(OUTPUT_DIR))
      stop(sprintf("OUTPUT_DIR not created: %s\n", OUTPUT_DIR))
    
    
    for (FTYPE in func_types) {
      # -----------------------  GC_EC_TR_pipeline.R code -----------------------
      
      #RRTYPE   = "TR"               # RRTYPE:   random rankings type (EC or TR)
      #RANKTYPE = "static"           # RANKTYPE: asd
      #FTYPE    = "incremental"      # FTYPE:    functions type
      
      A.file  = file.path(A_DIR,
                          paste("A",
                                GRAPHNAME,
                                RANKTYPE,
                                FTYPE,
                                "rtable", sep = "."))  # A: actual K_haus(R1, R2_f) values
      A = read.table(A.file)
      
      cat(sprintf("------------------------------------------\n"))
      cat(sprintf("GC_%s SIGNIFICANCE TEST, with params:\n", RRTYPE))
      cat(sprintf(
        "    - G=%s\n    - N=%d\n    - Ts={%s}\n",
        GRAPHNAME,
        N,
        paste(Ts, collapse = " ")
      ))
      cat(sprintf("    - %s\n    - %s\n", RANKTYPE, FTYPE))
      cat(sprintf("------------------------------------------\n"))
      
      cat(sprintf("A.file: %s\n",  basename(A.file)))
      cat(sprintf("R1.file: %s\n", basename(R1.file)))
      cat(sprintf("Running...\n"))
      
      dir.create(file.path(OUTPUT_DIR, FTYPE), 
                 recursive = T)
      # dir.create(file.path(OUTPUT_DIR, FTYPE, "PERMS_permutations"),
      #            recursive = T)
      # dir.create(file.path(OUTPUT_DIR, FTYPE, "Y_random_rankings"),
      #            recursive = T)
      dir.create(file.path(OUTPUT_DIR, FTYPE, "R_random_values"),
                 recursive = T)
      dir.create(file.path(OUTPUT_DIR, FTYPE, "S_significance"),
                 recursive = T)
      
      Y = list()
      if (RRTYPE == "TR") {
        
        if (FTYPE == "incremental") {
          Y = Y_TR.inc
        } else {
          Y = Y_TR.dec
        }
        
      } else {  # FTYPE = "EC"
        
        if (RANKTYPE == "static" && FTYPE == "incremental") {
          Y = Y_EC.static.inc
        } else if (RANKTYPE == "static" && FTYPE == "decremental") {
          Y = Y_EC.static.dec
        } else if (RANKTYPE == "dynamic" && FTYPE == "incremental") {
          Y = Y_EC.dynamic.inc
        } else if (RANKTYPE == "dynamic" && FTYPE == "decremental") {
          Y = Y_EC.dynamic.dec
        } else {
          stop("Invalid RANKTYPE FTYPE arguments.")
        }
      }
      
      # 4:  compute random values r_f_1, r_f_2, ..., r_f_j = K_haus(R1, RR_f_j)
      cat(sprintf("* Computing random experiments..\n"))
      
      R = matrix(0.0, nrow = nrow(A), ncol = N)  # R[f, j] = K_haus(R1, Y[[ f ]][j, ] )
      rownames(R) = rownames(A)
      
      for (f in rownames(A)) {
        cat(sprintf("\t* %s\n", f))
        
        tA = Sys.time()
        
        if (RRTYPE == "TR") {
          outputF = file.path(OUTPUT_DIR,
                              FTYPE,
                              "R_random_values",
                              paste("R_TR_", f, ".csv", sep = ""))
          
          R[f, ] = compute_singleF_random_Khaus(R1, Y[[1]], outputF='') # scrivo i singoli vettori di r_i
          
        } else {
          outputF = file.path(OUTPUT_DIR,
                              FTYPE,
                              "R_random_values",
                              paste("R_EC_", f, ".csv", sep = ""))
          
          R[f,] = compute_singleF_random_Khaus(R1, Y[[f]], outputF='') # scrivo i singoli vettori di r_i
          # e.g: R_R1_TOM.csv: i valori di K_haus(R1, R2_TOM_j) per j=1..N=100
        }
        
        getTiming(tA, Sys.time(), sprintf("%s", f))
      }
      
      if (RRTYPE == "TR") {
        outputF = file.path(OUTPUT_DIR,
                            FTYPE,
                            "R_random_values",
                            "R_TR.Rdata")
        
      } else {
        outputF = file.path(OUTPUT_DIR,
                            FTYPE,
                            "R_random_values",
                            "R_EC.Rdata")
        
      }
      
      # Scrivo la matrice per intero sia in csv che in Rdata
      write.table(R, file=outputF, col.names = F, sep = ",")     
      save(R, file=outputF)
      
      # -------------------------------------------------------------------------------------
      
      # 5:  significance
      cat(sprintf("* Computing significance..\n"))
      
      # A ?? il vettore colonna dei valori effettivi (actual): [ x_i ].
      # Le righe di A ed R sono allineate: corrispondono alla stessa F.
      # S ha le stesse dimensioni di A.
      # S[i, ] ?? la frazione degli R[i, ] che sono > di A[i].
      S = significance_test(A, R)  # minus=T
      rownames(S) = rownames(A)
      
      outputF = file.path(OUTPUT_DIR,
                          FTYPE,
                          "S_significance",
                          "S.csv")
      write.table(S, outputF, sep = ",", col.names = F)                  # scrivo S sia in .csv che .Rdata
      
      outputF = file.path(OUTPUT_DIR,
                          FTYPE,
                          "S_significance",
                          "S.Rdata")
      save(S, file=outputF)
      
      
      # 6:  thresholding significance
      for (thr in Ts) {
        Sb = S
        Sb = Sb >= thr
        Sb = matrix(apply(Sb, 2, as.integer), nrow=nrow(S), ncol=ncol(S), dimnames = list(rownames(S), colnames(S)))
        
        outputF = file.path(OUTPUT_DIR,
                            FTYPE,
                            "S_significance",
                            paste("S_", thr , ".csv", sep = ""))
        write.table(Sb, outputF, sep = ",", col.names = F)              # scrivo S sia in .csv che .Rdata
        
        outputF = file.path(OUTPUT_DIR,
                            FTYPE,
                            "S_significance",
                            paste("S_", thr , ".Rdata", sep = ""))
        save(Sb, file=outputF)
      }
      
      
      # 7:  plot?  TODO.
      cat(sprintf("DONE! \n"))
      
    }
  }
  
  #  dir.create(file.path(NEXT_DIR, odir))
  file.copy(from=odir, to=file.path(NEXT_DIR), recursive=T)

}

unlink("results_*", recursive=TRUE)
getTiming(tA, Sys.time(), "Pipeline GC")



