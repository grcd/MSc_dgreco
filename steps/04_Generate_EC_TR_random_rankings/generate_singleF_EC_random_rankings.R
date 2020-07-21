
# Restituisce Y, una matrice con N=nrow(perms) random rankings EC (Equal Classes) 
# usando le cardinalità delle classi di R2_f.
# Y[i, ] è l' i-esimo random ranking ottenuto dalla i-esima permutazione perms[i, ].

generate_singleF_EC_random_rankings <- function(perms, R2_f, G, id_to_index='', 
                                                outputF='') 
  # id_to_index:  column vector, dimensions: ecount(G) x 1
  #               id_to_index["4", 1] is the index of edge with $id="4"
{
  max_r = max(R2_f) # number of classes of R2_f
  
  N = nrow(perms)   # number of random-rankings to generate
  
  Y = matrix(as.integer(0), nrow=N, ncol=ncol(R2_f))
  #colnames(Y) = colnames(R2_f)

  #tA = Sys.time()
  c = table(matrix(data=as.integer(R2_f), nrow=1))  # c(i): number of elements having rank=i
  #getTiming(tA, Sys.time(), "generate_singleF_random_rankings EC: table")
  
  #tA = Sys.time()
  for (j in 1:N) {  # use perms[j, ], generate Y[j, ]
#   tB = Sys.time()
    k = 1
    for ( r in 1:max_r ) {

#        tC1 = Sys.time()
        left  = k
        right = k + as.integer(c[r]) - 1
#        getTiming(tC1, Sys.time(), "\t tC1")

#        tC2 = Sys.time()        
        ss = perms[j, left:right]  # perms are given as $ids (not indexes)
#        getTiming(tC2, Sys.time(), "\t tC2")
        
#        tC3 = Sys.time()
        #ss = id_to_index[as.character(ss), 1]  # we need an $id -> index conversion
#        getTiming(tC3, Sys.time(), "\t tC3")
        
#        tC4 = Sys.time()
        Y[j, as.integer(ss)] = r  # before we can actually assign a rank 
#        getTiming(tC4, Sys.time(), "\t tC4")
        
        k = right + 1
    }
#    getTiming(tB, Sys.time(), sprintf("    %s-th single iteration", j))
  }
  #getTiming(tA, Sys.time(), "generate_singleF_random_rankings EC: for (j in 1:N)")
  
  
  #tA = Sys.time()
  if (outputF != '')
    write.table(Y, file=outputF, sep=',', row.names=F, col.names=F)
  #getTiming(tA, Sys.time(), "generate_singleF_random_rankings EC: write.table")
  
  return(Y)
}
