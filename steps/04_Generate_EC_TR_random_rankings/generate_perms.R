
# GENERATE_PERMS

generate_perms <- function(G, N, seed, 
                           outputF='')
{
  # Error handling
  # Code
  
  set.seed(seed)
  
  n_edges = ecount(G)
  perms = matrix(data="", nrow=N, ncol=n_edges)
  
  for (j in 1:N) 
    perms[j, ] = E(G)$id[ sample(1:n_edges, ecount(G), replace=F) ]  # NOTICE: we take $id, not index

  if (outputF != '') {
    # write to an output file
    write.table(perms, outputF, sep=',', row.names=F, col.names=F, quote=TRUE)
  }
  
  return(perms)
}
