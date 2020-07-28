
# Qui l'idea è che ho una sezione iniziale con variabili globali e parametri e poi
# una per ogni blocco della pipeline, con i parametri specifici per ciascuno di
# essi.


##### 00. Global variables and parameters ------------------------------------------------

### Edges or NodeEq?
#Global.ElementType = "edges"  
Global.ElementType = "nodes"  


### Subdirectories for reading/storing script and data
Global.GRAPHS_DIR = file.path(getwd(), "datasets")  ## contains datasets and associated metadata
Global.EXEC_DIR   = file.path(getwd(), "bin")       ## contains external executables for fast ranking and khaus computation
Global.DATA_DIR   = file.path(getwd(), "data")      ## output directory: one per network
Global.WFS_DIR    = file.path(getwd(), sprintf("WFs/%s", Global.ElementType)) ## contains one script per weight function used


### Verbose or not?
Global.ranking.verbose=T  # verbose for Pipeline_RANKING
Global.GC.verbose=T       # verbose for Pipeline_GlobalComparison (GC)
Global.PC.verbose=T       # verbose for Pipeline_ProgressiveComparison (PC)
Global.RE.verbose=T       # verbose for Pipeline_RandomExperiments (RE)

### Get the network and set network type (either GENE or PPIN)
# Global.G.file       = file.path(Global.GRAPHS_DIR, "PPI-D1.gml")  # graph file
# Global.network_type = "PPIN"
Global.G.file 			 = file.path(Global.GRAPHS_DIR, "HDNG.gml")  # graph file
Global.network_type = "GENE"

### Get the graph name by splitting the network filename
Global.G.name = unlist(strsplit(basename(Global.G.file), split="\\."))[1]  # graph name 

### Which type(s) or ranking to execute?
# Global.rank_types		= c("static", "dynamic") 
Global.rank_types		= c("static")



###### 01. Pipeline_Ranking parameters --------------------------------------------------

### rounding for node/edge weights
Global.ranking.roundTo = 3  

### select which weight functions use
if (Global.ElementType == "edges") {
	
	Global.ranking.incrementals = c("ECV",
																	"GTOM2",
																	"TOM",
																	"kb_rec_max",
																	"kb_rec_min",
																	"kb_param_sym")
	
	Global.ranking.decrementals = c("EB",
																	"ECC3",
																	"ECP")
	
	# Global.ranking.incrementals = c("ECV", "GTOM2")
	# Global.ranking.decrementals = c("EB", "ECC3")
	
} else if (Global.ElementType == "nodes") {
	
	Global.ranking.incrementals = c("NCC", "EGC")
	Global.ranking.decrementals = c("NB", "SGC", "KPC")
	
} else {
	stop(sprintf("Error: invalid Global.ElementType (value=%s).", Global.ElementType))
}

### Output directory for the ranking step
Global.ranking.NEXTDIR = file.path(Global.DATA_DIR, Global.G.name, "ranking_data")



##### 02 Pipeline_GlobalComparison parameters ----------------------------------------

### where to store results of this step
Global.GC_NEXTDIR = file.path(Global.DATA_DIR, Global.G.name, "GC")  # dove sposta i file A.*



##### 03 Pipeline_ProgressiveComparison: Pipeline_PC.R -----------------------------------

### Files containing the gold standard either for GENE or PPIN networks
Global.GS.file = file.path(Global.GRAPHS_DIR, Global.G.name, "GSP.txt")             ## GENE
# Global.GS.file = file.path(Global.GRAPHS_DIR, Global.G.name, "Cmplx-D1_ids.txt")  ## PPIN

### rounding for node/edge score
Global.PC.roundTo = 2
Global.PC.saveSubgraphs = FALSE

### percentages for progressive comparison: each element of the list is a percentage
### split
Global.percs = list(
  c( .1, .2, .3, .4, .5, .6, .7, .8, .9, 1),  # 10% increments
  c( .05, .10, .15, .20, .25, .30, .35, .40,  # 5%  increments
     .45, .50, .55, .60, .65, .70, .75, .80, 
     .85, .90, .95, 1))

### colors for graphing incrementals curves
Global.FIncrementalColors =    c("#a6cee3",  
                                 "#1f78b4",
                                 "#b2df8a",
                                 "#33a02c",
                                 "#fb9a99",
                                 "#e31a1c",
                                 "#fdbf6f",
                                 "#ff7f00",
                                 "#cab2d6",
                                 "#6a3d9a",
                                 "#ffff00")

### colors for graphing incrementals curves
Global.FDecrementalColors =    c("#b15928",  
                                 "#878787",
                                 "#c51b7d")

### where to store results of this step
Global.PC_NEXTDIR = file.path(Global.DATA_DIR, Global.G.name, "PC")



# 04 Pipeline_GenerateRandomRankings: generate_EC_TR_random_rankings.R ---------------

### How many random rankings to generate?
Global.N = 3

### Seed for reproducibility
Global.seed = 3

### A single random ranking is generated starting from a random permutation of edges.
### First line of perms_N.csv contains the 1st permutation to construct the 1st random ranking
### Second line of perms_N.csv contains the 2nd permutation to construct the 2nd random ranking, and so on.
Global.PERMS_FILE = file.path(Global.GRAPHS_DIR, Global.G.name, paste("perms_", Global.N, ".csv", sep = ""))

### where to store results of this step
Global.RR_NEXTDIR = file.path(Global.DATA_DIR, Global.G.name, "random_rankings_EC_TR")



# 05 RandomExperiments PC: Pipeline_PC_EC_random.R, Pipeline_PC_TR_random.R ---------------
### where to store results of this step for EqualClasses random rankings
Global.RE_PC_EC_NEXTDIR = file.path(Global.DATA_DIR, Global.G.name, "random_experiments_EC") # RE=RandomExperiments

### where to store results of this step for TotalRandom random rankings
Global.RE_PC_TR_NEXTDIR = file.path(Global.DATA_DIR, Global.G.name, "random_experiments_TR") # RE=RandomExperiments



# 06 RandomExperiments GC + Significance GC: GC_EC_TR_multi_pipeline.R ---------------------
### TODO: questo lo posso levare perché non applichiamo alcuna correzione di Bonferroni
Global.SIGNIFICANCE_GC.Tvalues = c(1.0)

### where to store results of this step for TotalRandom random rankings
Global.SIGNIFICANCE_GC_NEXTDIR = file.path(Global.DATA_DIR, Global.G.name, "significance_GC")


# 07 Pipeline_Significance_PC: Significance_PC_EC_TR.R -------------------------------------
### TODO: questo lo posso levare perché non applichiamo alcuna correzione di Bonferroni
Global.SIGNIFICANCE_PC.Tvalues = Global.SIGNIFICANCE_GC.Tvalues

### where to store results of this step for TotalRandom random rankings
Global.SIGNIFICANCE_PC_NEXTDIR = file.path(Global.DATA_DIR, Global.G.name, "significance_PC")

