
#   K_HAUS
#   external_Khaus      Computes k_haus, using the external C written executable.

source("config/PIPELINE_PARAMS.R")
KHAUS.execpath = file.path(Global.EXEC_DIR, "khaus")      

#
#   K_haus(R_i, R_j)
#       Computes the K_haus distance between the partial rankings R_1 , R_2
#       using the closed form in [ref. Fagin "Comparing Partial Rankings].
#
#       Given a domain D with |D| = n elements {a_1, a_2, ... a_n}, 
#       R_1 and R_2 are vector of integers such that:
#
#           1. length(R_1) = length(R_2) = n
#           2. R_1[k] = rank of (or bucket containing) a_k
#
#       Returns K_haus(R_1, R_2) >= 0 (success) or -1 (error).


external_Khaus <- function(R_1, R_2, m, verbose=F) {
  # m is:  |E|
  write.table(cbind(t(R_1), t(R_2)), file = "khausin.tmp", sep = " ", row.names = FALSE, col.names = FALSE)
  if (! file.exists("khausin.tmp")) {
    cat("ERROR: khaus.tmp does not exists.\n")
    return(-1.0)
  }
  
  cmd = paste(KHAUS.execpath, "khausin.tmp", m, "khausout.tmp", sep = " ")
  if (verbose==T)
    cat(paste("DEBUG:", cmd, "\n", sep = " "))	## DEBUGGING 
  
  if (system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE) != 0 ) { # error
    cat("ERROR: Unable to write khausout.tmp.\n")
    return(-1.0)
  }
  
  khaus = (read.table("khausout.tmp"))[1,1]
  
  file.remove("khausin.tmp")
  file.remove("khausout.tmp")   
  
  return(khaus)
  
}
