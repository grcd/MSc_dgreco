
#   OS_MATCH(P, K, sigma)
#
#   Calcola l'overlapping score tra gli insiemi P, K e restituisce TRUE se
#   tale valore è al di sopra di sigma, FALSE altrimenti.

OS_match <- function(P, K, sigma_thr) {
    
    if (length(P) == 0 || length(K) == 0) {
        os_score = 0;
    } else {
        os_score = ((length(intersect(P, K)))^2) / ((length(P) * length(K)));
    }

    #print(os_score); # DEBUG
    if (os_score >= sigma_thr) {
        return(TRUE);
    } else {
        return(FALSE);
    }
}
