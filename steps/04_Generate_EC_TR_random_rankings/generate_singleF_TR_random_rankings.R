
# source("feasibleRanking.R")

generate_singleF_TR_random_rankings <- function(perms, R2_f, G, id_to_index='',
                                                outputF='') 
{
  N = nrow(perms)
  
  Y = matrix(as.integer(0), nrow=N, ncol=ncol(perms))
  colnames(Y) = colnames(R2_f)

  for (j in 1:N) {  # use perms[j, ], generate Y[j, ]

    #  generate a random ranking vector
    Y_f = sample(1:ecount(G), size=ecount(G), replace=T)
    
    #  re-labelling in order to avoid rank gaps
    ii = order(Y_f)
    Y_f[ ii ] = as.integer(factor(Y_f[ ii ], labels=1:length(unique(Y_f))))

    # OLD CODE using feasibleRanking, very slow.
    #
    # Y_f = t(as.matrix( sample(1:ecount(G), size=ecount(G), replace=T) ))
    # while (! feasibleRanking(Y_f)) {
    #   Y_f = t(as.matrix( sample(1:ecount(G), size=ecount(G), replace=T) ))
    # }

    if (id_to_index == '') {  
      ss = as.integer(perms[j, ])  # we assume id = index
    } else {
      ss = id_to_index[as.character(perms[j, ]), 1]  # in general, it could not be the case: we need a map $id to column index
    }
    
    Y[j, ss] = Y_f
    
    cat(sprintf("\t\t * random rank %d: OK\n", j))
  }
  
  if (outputF != '')
    write.table(Y, file=outputF, sep=',', row.names=F, col.names=F)
    
  return(Y)
}
