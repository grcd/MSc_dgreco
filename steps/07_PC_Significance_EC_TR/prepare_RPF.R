rm(list = ls())

#WORKINGDIR = "/Users/grcdaniele/Desktop/Draft_Hiearrachy/RReprise/NEW/Pipeline_Significance_PC/"
#WORKINGDIR = "/home/daniele.greco/R/Pipeline_RANKING_WGN/Pipeline_Significane_PC"
WORKINGDIR="/Volumes/TERABOB/TESI_LM_BACKUP_MACCHINA_DIP_JUL18/ARCHI_DIP_MacOSX/Pipeline_Significance_PC/"
setwd(WORKINGDIR)

RANDPERF_TR_DIR = 
RANDPERF_EC_DIR = 
rank_types   = c("static", "dynamic")
func_types   = c("incremental", "decremental")
roundTo      = 2


#  Carica matrici RPF
RPF = list()

RPF[[ "EC" ]] = list()
RPF[[ "TR" ]] = list()


# TR
for (FTYPE in func_types) {
  RPF.file = file.path(RANDPERF_TR_DIR,
                       FTYPE,
                       "P.txt")
  
  RPF = as.matrix(read.table(RPF.file, 
                             check.names=F))  # check.names avoids the annoying 'X' in column names
  
  RPF[[ "TR" ]][[ FTYPE ]] = round(RPF, digits=roundTo)
}


# EC
for (RANKTYPE in rank_types) {
  RPF[[ "EC" ]][[ RANKTYPE ]] = list()
  
  for (FTYPE in func_types) {
    RPF[[ "EC" ]][[ RANKTYPE ]][[ FTYPE ]] = list()
  
    RPF.file = file.path(RANDPERF_EC_DIR,
                         RANKTYPE,
                         FTYPE,
                         "P.txt")
    
    RPF = as.matrix(read.table(RPF.file, 
                               check.names=F))  # check.names avoids the annoying 'X' in column names

    RPF[[ "EC" ]][[ RANKTYPE ]][[ FTYPE ]] = round(RPF, digits=roundTo)
  }
}
