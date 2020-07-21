
source("steps/06_GC_EC_TR_MCRandomExperiments_and_Significance/external_Khaus.R")

compute_singleF_random_Khaus <- function(R1, Y,
                                         outputF='') 
{
  N = nrow(Y)
  R = matrix(data=0.0, nrow=1, ncol=N)

  for (j in 1:N) {  # compute khaus(R1[1, ], Y[j, ])
    R[1, j] = compute_khaus(R1, matrix(data=as.integer(Y[j, ]), nrow=1))
  }
  
  if (outputF != '')
    write.table(R, file=outputF, sep=',', row.names=F, col.names=F)
  
  return(R)
}

compute_khaus <- function(R1, Y)
{
  return(external_Khaus(R1, Y, ncol(Y)))
}


