
significance_test <- function(A, R,
                              threshold='',
                              roundTo=3,
                              minus=T)
{
  #  A: actual values, column vector:  x_i
  #  R: random values, row vector(s):  r_i1, r_i2, ..., riN (ex: N=100)
  #  A[i] is tested with R[i, ] using Monte Carlo Hypothesis Testing

  S = matrix(data=0.0, nrow=nrow(A), ncol=1)
  N = ncol(R)  #  N:  number of random experiments
  
  A = round(A, digits=roundTo)
  R = round(R, digits=roundTo)
  
  for (i in 1:nrow(A)) {  # compare x_i=A[i] with {r1, r2, ..., rN}=R[i, ]
    x_i = A[i, 1]
    if (minus) {
      S[i] = (length(which( R[i, ] > x_i )) / N)  # S[i]=0.75: x_i più piccolo del 75% degli r_i
    } else {
      S[i] = (length(which( R[i, ] < x_i )) / N)  # S[i]=0.75: x_i più grande del 75% degli r_i
    }
  }
  
  if ((threshold != '') && (threshold >= 0.0) && (threshold <= 1.0))
    S = S >= threshold
  
  return(S)
}
