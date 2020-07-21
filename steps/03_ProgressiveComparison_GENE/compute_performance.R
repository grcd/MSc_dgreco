
#   COMPUTE_PERFORMANCE.R

compute_performance <- function(S, GS) {
  # Computes the fraction of elements in S found in GS.
  #
  # Args:
  #   S:  recall set
  #   GS: gold standard set
  # Returns:
  #   the fraction of elements in S found in GS
  
  return( (length(intersect(S, GS)))/length(GS) )
  
}
