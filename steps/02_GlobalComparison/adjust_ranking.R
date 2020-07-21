
source("steps/02_GlobalComparison/invert_ranking.R")

adjust_ranking <- function(type, R) 
{
  if (type == "decremental") 
    R = invert_ranking(R)
  else if (type == "incremental") {
    # do nothing
  } else {
    stop(sprintf("Unrecognized type: %s", type))
  }  
  
  return(R)
}
