
adjust_GSset <- function(type, GS, G) {
  if (type == "incremental") {
    # PPI: do nothing
  } else if (type == "decremental") {  
    # PPI: do nothing, same as incremental case
  } else {
    stop(sprintf("Unrecognized type: %s", type))
    return(NULL)
  }  
  
  return(GS)
}
  
