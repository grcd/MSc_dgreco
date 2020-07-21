#	READ_GSSET(filename)
#
#	Legge i complessi di proteine forniti da Rombo et al.
#	Ritorna una lista in cui ogni elemento è una lista di protein-names ($pname dei grafi corrispondenti)

read_GSset <- function(filename) {
    K=list(0);
    
    fileConn = file(filename, open="r");
    tmp = read.delim(file=filename, row.names=c(1), stringsAsFactors = F)
    close(fileConn)
    
    for (i in 1:nrow(tmp)) {
      
      ll = unlist(strsplit(tmp[i,], ","));
      K[[i]] = ll;
      
    }
    
    return(K)
}  