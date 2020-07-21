
adjust_perms <- function(type, perms) 
{
  if (type == "decremental") 
    perms = t(apply(perms, 1, rev))
  else if (type == "incremental") {
    # do nothing
  } else {
    stop(sprintf("Unrecognized type: %s", type))
  }
  
  return(perms)
}
