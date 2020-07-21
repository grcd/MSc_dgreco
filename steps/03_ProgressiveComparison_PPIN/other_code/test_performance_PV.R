
# TEST_PERFORMANCE_PV.R

rm(list=ls())

WORKINGDIR = "/Volumes/TERABOB/TESI_LM_BACKUP_MACCHINA_DIP_JUL18/ARCHI_DIP_MacOSX/Pipeline_ProgressiveComparison/"
library(igraph)
setwd(WORKINGDIR)

source("percentage_view.R")
source("feasibleRanking.R")
source("performance_PV.R")
source("adjust_GSset.R")

# Auxiliary functions ---------------------------------------------------

generate_ranking_with_breaks <- function(m, breaks) {
  # Args:
  #    m:       number of elements (edges)
  #    breaks:  rank breaks
  # 
  # Returns a vector of length `m` with rank breaks.
  #
  # For example:
  #    m=10, breaks=c(0, .20, .40, .60, .80, 1) returns 
  #    1 1 2 2 3 3 4 4 5 5
  breaks = ceiling(m * breaks) 
  return(as.integer(cut(1:m, breaks=breaks, labels=1:(length(breaks)-1))))
}


# Test code -------------------------------------------------------------

if (! ("percentage_view" %in% ls(all.names = TRUE)) )
  stop("percentage_view not defined.")

if (! ("performance_PV" %in% ls(all.names = TRUE)) )
  stop("performance_PV not defined.")


G = make_full_graph(15, directed=F, loops=F)
G = delete_edges(G, E(G)[101:105])
E(G)$id = as.character(1:100)

m=ecount(G)
breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)   
percs  = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)
#GS = as.character(c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91))
GS = as.character(c(1,2,3,4,5,91,92,93,94,95))

R2 = t(as.matrix(generate_ranking_with_breaks(m, breaks)))
PV = t(as.matrix(percentage_view(R2, percs)))
P = performance_PV(R2, PV, G, GS)


GS = adjust_GSset("decremental", GS, G)
