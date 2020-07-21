
source("steps/05_MCRandomExperiments_only_PC/GENE/compute_performance.R")

# performance_PV <- function(R2, PV, G, GS, id_to_index='') {
#   #  R2:  ranking matrix, R2[f, ] single f ranking vector
#   #  PV:  percentage view matrix, PV[f, ] percentage view vector aligned with R2[f, ]
#   #  G:   igraph graph  (with $id edge attribute)
#   #  GS:  gold-standard set  (with $id(s))
# 
#   P = matrix(data=c(0.0), nrow=nrow(PV), ncol=ncol(PV))
#   rownames(P) = rownames(PV)
#   colnames(P) = colnames(PV)
# 
#   for (f in 1:nrow(PV)) {
#     for (j in 1:ncol(PV)) {  #  loop over percentage-views
# #      tA = Sys.time()
#         col_indexes = which(R2[f, ] <= PV[f, j])
# #      getTiming(tA, Sys.time(), "performance_PV:: which()")
# 
# #      tA = Sys.time()
#         col_ids = ''
#         if (id_to_index == '') {
#           col_ids = as.character(col_indexes)
#         } else {
#           col_ids = as.character(id_to_index[col_indexes, 1])  # we need an $id -> index conversion
#         }
# #      getTiming(tA, Sys.time(), "performance_PV:: col_ids")
# 
# #      tA = Sys.time()
#       P[f, j] = compute_performance(col_ids, GS)
# #      getTiming(tA, Sys.time(), "performance_PV:: compute_performance")
# 
#     }
#   }
# 
#   return(P)
# }


performance_PV <- function(R2, PV, G, GS, id_to_index='') {
  #  R2:  ranking matrix, R2[f, ] single f ranking vector
  #  PV:  percentage view matrix, PV[f, ] percentage view vector aligned with R2[f, ]
  #  G:   igraph graph  (with $id edge attribute)
  #  GS:  gold-standard set  (with $id(s))
  
  P = matrix(data=c(0.0), nrow=nrow(PV), ncol=ncol(PV))
  rownames(P) = rownames(PV)
  colnames(P) = colnames(PV)
  
  for (f in 1:nrow(R2)) {
    
    ii = order(R2[f, ])  # gli indici di colonna sono gli id degli archi
    
    maxr = R2[f, ii[length(ii)] ]  # max rank
    if (maxr == 1) {  # tutti gli archi hanno rank=1, una sola classe
      
      P[f, 1:(ncol(PV)-1)] = 0
      P[f, ncol(PV)] = 1
      
    } else {  # ci sono almeno due classi
      
      R2_f = matrix(R2[f, ii], nrow=1, ncol=ncol(R2))

      xx = cbind(as.integer(1), R2_f)
      xy = cbind(R2_f, maxr)
      boundaries = (which( (xy-xx) == 1 )) - 1  # c'è almeno un '1'
      
      #  prima P[f,j] elemento a parte
      j = 1
      
      while( PV[f, j] == 0)
        j = j + 1
      
      left  = 1
      right = boundaries[ PV[f, j] ]
      
      #      ss = colnames(R2)[ R2[f, ii[left:right]] ]
      ss = as.character(ii[left:right])
      P[f, j] = compute_performance(ss, GS)
      j = j + 1
      
      #  da 2 ad ncol(PV)-1
      while (j < ncol(PV)) {
        left  = right + 1
        right = boundaries[ PV[f, j] ]
        
        ss = c(ss, as.character(ii[left:right]))
        
        P[f, j] = compute_performance(ss, GS)
        
        j = j + 1
      }
      
      #  ultimo 
      left = right + 1
      right = length(ii)
      ss = c(ss, as.character(ii[left:right]))
      P[f, j] = compute_performance(ss, GS)
      
    }
  }
  
  return(P)
}
