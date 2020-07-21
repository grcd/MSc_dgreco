
adjust_GSset <- function(type, GS, G) {
  if (type == "incremental") {
    # do nothing
  } else if (type == "decremental") {  # DECREMENTAL SETTING: "Quanti archi in ~GS ho scoperto?"
    GS = setdiff( E(G)$id, GS ) # NOTE: we use ~GS, the complement of GS, here
  } else {
    stop(sprintf("Unrecognized type: %s", type))
  }  
  
  return(GS)
}
  
