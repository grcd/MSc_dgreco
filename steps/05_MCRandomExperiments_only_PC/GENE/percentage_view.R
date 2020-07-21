
#  PERCENTAGE_VIEW 
#
#  Compute a single PV[f, ] percentage view for the ranking R2 (supplied
#  as R2[f, ] by the caller) according to the given percentages percs .
#
#  Args:
#    R2:     could also be a data.frame/matrix type, not a vector
#    percs:  vector

percentage_view <- function(R2, percs) {

  R2 = as.integer(R2)  # from data.frame/matrix type, to vector
  
  # Check arguments
  if ( length(percs) == 0 ) {
    warning("percentage_view: no percentages, percs is empty.")
    return(NULL)
  }
  
  if (percs[1] == 0) {
    warning("percentage_view: first percentage value must be > 0.")
    return(NULL);
  }
  
  if ( percs[1] == 1 ) 
    return( c(max(R2)) )
  
  # sanity checks OK: length(percs) >= 1  and  percs[1] != 0.0
  n_edges = length(R2)
  bounds  = ceiling(percs * n_edges)
  pv      = rep(0, length(percs))
  ii      = order(R2, decreasing=F)
  
  # handle bounds[1] separately
  left = 1
  right = bounds[1]
  
  ranks = sort(unique(R2[ ii[left:right] ]))
  
  if (length(ranks) == 1) {  # only one rank
    maxr = ranks[1]
    if (R2[ ii[right+1] ] == maxr) { # class 'maxr' is not entirely contained within left:right
      pv[1] = 0  # pv[i] = pv[i-1]
    } else {
      pv[1] = maxr;
    }
  } else {  # two or more ranks
    maxr   = ranks[length(ranks)]    # max rank
    maxr_2 = ranks[length(ranks)-1]  # 2nd max rank after maxr_1st
    
    if (R2[ ii[right+1] ] == maxr) { # class 'maxr' is not entirely contained within left:right
      pv[1] = maxr_2  #  in this case, maxr_2 IS entirely contained
    } else {
      pv[1] = maxr;
    }    
  }
  
  
  # handle bounds[2:(k-1)]  ########## ??? 2:(k-1) or 2:k ??? #######
  if (length(bounds) > 1 ) {
    for (i in 2:(length(bounds))) {
      
      # WHAT IF: bounds[2] = n_edges ? We are done
      if (bounds[i] == n_edges) {      
        pv[i] = max(R2)
        break;
      }
      
      left = bounds[i-1]+1
      right = bounds[i]
      #cat(sprintf("i=%d, left=%d, right=%d\n", i, left, right))
      
      ranks = sort(unique(R2[ ii[left:right] ]))
      
      if (length(ranks) == 1) {  # only one rank
        maxr = ranks[1]
        if (R2[ ii[right+1] ] == maxr) { # class 'maxr' is not entirely contained within left:right
          pv[i] = pv[i-1]  # pv[i] = pv[i-1]
        } else {
          pv[i] = maxr;
        }
      } else {  # two or more ranks
        maxr   = ranks[length(ranks)]    # max rank
        maxr_2 = ranks[length(ranks)-1]  # 2nd max rank after maxr_1st
        
        if (R2[ ii[right+1] ] == maxr) { # class 'maxr' is not entirely contained within left:right
          pv[i] = maxr_2  #  in this case, maxr_2 IS entirely contained
        } else {
          pv[i] = maxr;
        }    
      }    
    }
  }

  return(pv)
}
