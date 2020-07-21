
rm(list = ls()) 
source("config/PIPELINE_PARAMS.R")
source("steps/02_GlobalComparison/external_Khaus.R")
source("steps/02_GlobalComparison/adjust_ranking.R")


# INPUT PARAMETERS
GRAPHNAME = Global.G.name 
DATA_DIR = Global.ranking.NEXTDIR  
NEXT_DIR = Global.GC_NEXTDIR


OUTPUT_DIR = file.path(paste("results_GC",        # results_GC_<RRTYPE>_<date>
                             GRAPHNAME,
                             #format(Sys.time(), "%Y_%m_%d_%H%M%S"),
                             format(Sys.time(), "%Y_%m_%d"),
                             sep = "_"))


# CODE
old_data = list.files(".", pattern = "results_GC" , include.dirs = T)
for (od in old_data) 
  unlink(od, recursive = T)

rank_types = Global.rank_types
func_types = c()
if (length(Global.ranking.incrementals))
  func_types = c(func_types, "incremental")
if (length(Global.ranking.decrementals))
  func_types = c(func_types, "decremental")

if (!dir.exists(NEXT_DIR)) {
  dir.create(NEXT_DIR, recursive = T)
}
if (!dir.exists(NEXT_DIR))
  stop("Unable to create NEXT_DIR directory.")


if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = T)
} else {
  #stop("OUTPUT_DIR exists yet, specify another results directory.")
  #file.remove()
}

#  leggi ranking funzionale, ed invertilo per usarlo con misure decrementali
R1.file = file.path(DATA_DIR,
                    paste("R1", GRAPHNAME, "rtable", sep = "."))  # R1: ranking funzionale 

R1_inc = read.table(R1.file)
R1_dec = adjust_ranking("decremental", R1_inc)

for (RANKTYPE in rank_types) { # static, dynamic
  for (FTYPE in func_types) {    # incremental, decremental

    R2.file = file.path(DATA_DIR,
                        paste("R2",            # R2: R2.HDN.dynamic.incremental.rtable
                              GRAPHNAME,
                              RANKTYPE,
                              FTYPE,
                              "rtable", sep = "."))  

    A.file  = paste("A",  # A: file di output
                    GRAPHNAME,
                    RANKTYPE,
                    FTYPE,
                    "rtable", sep = ".") # A: actual K_haus(R1, R2_f) values

     cat(sprintf("* A.file: %s\n", A.file))
    
    R2 = read.table(R2.file)
    A = matrix(-1.0, nrow=nrow(R2), ncol=1)
    rownames(A) = rownames(R2)
    colnames(A) = c("R1")

    for (i in 1:nrow(A)) {
      cat(sprintf("Computing k_haus(R1, R2_%s)\n", rownames(A)[i]))
      if (RANKTYPE == "incremental") {
        A[i, 1] = external_Khaus(R1_inc[1, ], R2[i, ], ncol(R2))
      } else {
        A[i, 1] = external_Khaus(R1_dec[1, ], R2[i, ], ncol(R2))
      } 
    }

    A.outputfile = file.path(OUTPUT_DIR, A.file)
    write.table(A, file=A.outputfile)
    file.copy(from= A.outputfile, 
              to=   file.path(NEXT_DIR, basename(A.outputfile)))
    
    
  }

}

# Remove the temporary output dir
unlink("results_*", recursive=TRUE)
