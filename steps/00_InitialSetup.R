
##### SANITY CHECKS  for executing the whole pipeline -------------------------------------

rm(list=ls())
library(igraph)
source("utils/getTiming.R")
source("config/PIPELINE_PARAMS.R")

### 1. Checking for external executables to be in the right directory
if (!dir.exists(Global.EXEC_DIR))
  stop("NOT FOUND: Global.EXEC_DIR")
cat(sprintf("* Global.EXEC_DIR found: OK\n"))

if (!file.exists(file.path(Global.EXEC_DIR, "rankgraph")))
  stop("NOT FOUND: rankgraph executable")
cat(sprintf("* rankgraph found: OK\n"))

if (!file.exists(file.path(Global.EXEC_DIR, "khaus")))
  stop("NOT FOUND khaus executable")
cat(sprintf("* khaus found: OK\n"))


### 2. Checking for weight functions, if any
if (!dir.exists(Global.WFS_DIR))
  stop("NOT FOUND: WFS_DIR weight functions directory")

wfs = list.files(Global.WFS_DIR, pattern=".R")
if (length(wfs) < 1)
  stop("No weight functions found.")

cat(sprintf("* Weight functions found:\n"))
for (ff in wfs) 
  cat(sprintf("\t%s\n", ff))


### 3. Checking for existence of the the graph file and, more importantly, graph edges and nodes format
# Checking whether file exists and igraph is able to read it
if (!file.exists(Global.G.file))
  stop("No graph set. CHECK Global.G.file")
G = read.graph(file=Global.G.file, format="gml")


# Checking NODES attributes
attr_G_nodes = names(vertex.attributes(G))

## FORMAT #1: each node must have an 'id' numeric attribute from 1 to card(G)
if (! ("id" %in% attr_G_nodes))
  stop("INVALID Graph format: 'id' vertex attribute missing\n.")
if (!(all(V(G)$id == 1:vcount(G))))
  stop("INVALID Graph format: vertex ids must be from 1 to |V|\n.")

## FORMAT #2: each node must also have a 'name' textual attribute with values from '1' to 'card(G)'
if (! ("name" %in% attr_G_nodes))
  stop("INVALID Graph format: 'name' node attribute missing\n.")
if (!(all(as.integer(V(G)$name) == 1:vcount(G))))
  stop("INVALID Graph format: vertex names must be character from 1 to |V|\n.")

# Checking EDGES attributes (analogous to the NODES attributes, see above)
attr_G_edges = names(edge.attributes(G))
if (! ("id" %in% attr_G_edges))
  stop("INVALID Graph format: 'id' edge attribute missing\n.")
if (!(all(as.integer(E(G)$id) == 1:ecount(G))))
  stop("INVALID Graph format: edge ids must be character from 1 to |E|\n.")


### 4. Print graph filename
cat(sprintf("* Selected graph: %s\n", basename(Global.G.file)))


### 5. Checking Gold Standard for Progressive Comparison
if (!file.exists(Global.GS.file))
  stop("No Gold Standard for PC set. Check Global.GS.file")
cat(sprintf("* Gold Standard for PC found: OK\n"))


### 6. Checking for any incremental/decremental weight functions  selected in PIPELINE_PARAMS.R
if (length(Global.ranking.incrementals) + length(Global.ranking.decrementals) == 0)
  stop("ERROR: Neither incremental nor decremental weight functions selected. Check Global.ranking.incrementals, Global.ranking.decrementals")


### 7. Create output directory 
if (!dir.exists(Global.DATA_DIR)) {
	dir.create(Global.DATA_DIR)
}
if (!dir.exists(Global.DATA_DIR)) {
	stop(sprintf("ERROR: Unable to create the output directory, %s. Check for permissions.", Global.DATA_DIR))
}


