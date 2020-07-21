
invert_ranking <- function(R) 
{
  max_r = max(R)
  return( (max(R)+1) - R )  
}
