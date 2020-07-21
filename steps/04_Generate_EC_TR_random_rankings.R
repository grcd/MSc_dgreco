rm(list=ls())

library(igraph)
source("config/PIPELINE_PARAMS.R")
source("steps/04_Generate_EC_TR_random_rankings/generate_perms.R")                       
source("steps/04_Generate_EC_TR_random_rankings/generate_singleF_random_rankings.R")
source("steps/04_Generate_EC_TR_random_rankings/adjust_perms.R")
source("utils/getTiming.R")


#  PARAMETERS
N           = Global.N
seed        = Global.seed
DATA_DIR    = Global.DATA_DIR
RANKING_DIR = Global.ranking.NEXTDIR
PERMS_FILE  = Global.PERMS_FILE
G.file      = Global.G.file
NEXT_DIR    = Global.RR_NEXTDIR

#  CODE
old_data = list.files(".", pattern = "random_rankings_EC_TR" , include.dirs = T)
for (od in old_data) 
  unlink(od, recursive = T)

if (!dir.exists(NEXT_DIR)) {
  dir.create(NEXT_DIR, recursive = T)
}
if (!dir.exists(NEXT_DIR))
  stop("Unable to create NEXT_DIR directory.")

OUTPUT_DIR  = file.path("random_rankings_EC_TR")
if (dir.exists(OUTPUT_DIR)) {
  warning("WARNING: OUTPUT_DIR exists. Please delete it.")
} else {
  dir.create(OUTPUT_DIR)
}

set.seed(seed)
G = read.graph(file=G.file, format = 'gml')
GRAPH_NAME = Global.G.name


#####  1. genero le permutazioni se non sono già disponibili (PERMS_FILE in PIPELINE_PARAMS.R)

if (! file.exists(PERMS_FILE)) { 
  # If no permutations have been generated, we do it now
  cat(sprintf("* No permutations found: generating permutations...\n"))
  
  tA = Sys.time()
  perms = as.matrix(generate_perms(G, N, seed, outputF=PERMS_FILE))
  inv_perms = adjust_perms("decremental", perms)
  getTiming(tA, Sys.time(), "generating perms")
  
} else {
  
  tA = Sys.time()
  perms = as.matrix(read.table(file=PERMS_FILE, sep=",", colClasses = "character"))
  perms = perms[1:N, ]
  inv_perms = adjust_perms("decremental", perms) # per il caso decrementale
  getTiming(tA, Sys.time(), "reading perms")
  
}


##### 2. Genero ranking casuali EC. 
#####
#####    Un ranking casuale EC rispetto ad un ranking topologico ottenuto da una 
#####    particolare funzione peso F (R2_F), mantiene il numero di classi e la 
#####    cardinalità di ciascuna classe di R2_F, ma riempie le classi con archi
#####    casuali.

cat(sprintf("-------------------------------\n"))
cat(sprintf("* RANDOM RANKINGS: EqualClasses\n"))
cat(sprintf("-------------------------------\n"))

for (rf in list.files(RANKING_DIR, pattern=paste("R2", GRAPH_NAME, sep="."))) {

  R2.file  = file.path(RANKING_DIR, rf)
  tokens = strsplit(rf, "\\.")[[1]]
  RANKTYPE = tokens[3]  # static, dynamic
  FTYPE = tokens[4]     # incremental, decremental

  cat(sprintf("- Processing %s...\n", basename(R2.file)))

  tA = Sys.time()
  R2 = read.table(R2.file)
  colnames(R2) = as.character(1:ncol(R2))
  getTiming(tA, Sys.time(), sprintf("reading %s", basename(R2.file)))

  Y = list()

  for (k in 1:nrow(R2)) {
    f = rownames(R2)[k]

    outputF = file.path(OUTPUT_DIR,
                        paste("Y_EC.",
                              f, ".",
                              RANKTYPE, ".",
                              FTYPE, ".",
                              "N", N, ".csv", sep = ""))

    #cat(sprintf("\t- %s\n", f))
    cat(sprintf("\t - %s\n", basename(outputF)))
    # Da un ranking topologico R2[f, ] e N=100 permutazioni perms
    # tira fuori N random EC ranking disposti "uno sotto l'altro"
    # nella matrice restituita ed assegnata a Y[[ f ]].
    # Gi?? che ci siamo, la scriviamo in output nel file Y_f.csv .
    tA = Sys.time()

    if (FTYPE == "incremental") {
      Y[[f]] = generate_singleF_random_rankings("EC",
                                                perms,
                                                R2[k, ],
                                                G,
                                                id_to_index='',
                                                outputF)
    } else if (FTYPE == "decremental") {
      Y[[f]] = generate_singleF_random_rankings("EC",
                                                inv_perms,
                                                R2[k, ],
                                                G,
                                                id_to_index='',
                                                outputF)

    } else {
      warning("Invalid FTYPE, Y[[f]] is NULL")
      Y[[f]]=NULL
    }

    getTiming(tA, Sys.time(), "generate_singleF_random_rankings EC")

  }
  outputF = file.path(OUTPUT_DIR,
                      paste("Y_EC.",
                            RANKTYPE, ".",
                            FTYPE, ".",
                            "N", N, sep = ""))
  tA = Sys.time()
  #write.table(Y, file=outputF, sep=',', row.names=F, col.names=F)
  save(Y, file = paste(outputF, ".Rdata", sep = "")) # e scriviamo il listone as well.
  getTiming(tA, Sys.time(), "save Y")

}



##### 3. Genero ranking TotalRandom (TR).
#####
#####    Rispetto agli EqualClasses, qui perdiamo la corrispondenza con un particolare
#####    R2_F: sia il numero, sia la cardinalità delle classi è casuale.

cat(sprintf("------------------------------\n"))
cat(sprintf("* RANDOM RANKINGS: TotalRandom\n"))
cat(sprintf("------------------------------\n"))

#  TR incremental
outputF = file.path(OUTPUT_DIR,
                    paste("Y_TR.incremental.", "N", N, ".csv", sep = ""))
cat(sprintf("%s\n", outputF))

Y = list()
Y[[1]] = generate_singleF_random_rankings("TR",  # versione incremental
                                          perms,
                                          R2_f=c(),
                                          G,
                                          id_to_index='',
                                          outputF)

outputF = file.path(OUTPUT_DIR,
                    paste("Y_TR.incremental.", "N", N, sep = ""))
save(Y, file = paste(outputF, ".Rdata", sep = ""))


#  TR decremental
outputF = file.path(OUTPUT_DIR,
                    paste("Y_TR.decremental.", "N", N, ".csv", sep = ""))
cat(sprintf("%s\n", outputF))
Y = list()
Y[[1]] = generate_singleF_random_rankings("TR", # versione decremental
                                          inv_perms,
                                          c(),
                                          G,
                                          id_to_index='',
                                          outputF)

outputF = file.path(OUTPUT_DIR,
                    paste("Y_TR.decremental.", "N", N, sep = ""))
save(Y, file = paste(outputF, ".Rdata", sep = ""))


# Copy into NEXTDIR
for (ff in list.files(OUTPUT_DIR, full.names = T))
  file.copy(from=ff, to=file.path(NEXT_DIR, basename(ff)))

# Remove the temporary output dir
unlink(OUTPUT_DIR, recursive=TRUE)
