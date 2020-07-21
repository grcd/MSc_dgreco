getTiming <- function(tA, tB, checkpoint_msg, units="secs", roundTo=1, file="") {

  dt = round(difftime(tB, tA, units=units), digits=roundTo)
  str = sprintf("* TIMING: %s took %s %s", checkpoint_msg, dt, units)

  if (file == "")
    cat(sprintf("%s\n", str))
  else {
    #  assumes 'file' holds a correctly open connection to a file
    write(str, file=file, append=TRUE)
  }
    
    
}
