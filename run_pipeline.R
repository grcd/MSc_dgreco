
rm(list=ls())
setwd("/Users/danielegreco/Desktop/PROJECTS/ROMBO/R/RPipeline") ## Set base dir
source("utils/getTiming.R")
source("config/PIPELINE_PARAMS.R")

# ---------------------------------- AUXILIARY FUNCTIONS -----------------------------

announce <- function(msg) {
	msg = sprintf("** %s\n", msg)
	
	cat("\n")
	cat("**\n")
	cat(msg)
	cat("**\n")
	cat("\n")
}

flush_previous_data <- function() {
	# Deletes data from a previous run.
	#
	# Args: 
	#   no arguments.
	# Returns: 
	#   no value.
	
	# See PIPELINE_PARAMS.R 
	dirs = c(Global.ranking.NEXTDIR,
					 Global.GC_NEXTDIR,
					 Global.PC_NEXTDIR,
					 Global.RR_NEXTDIR,
					 Global.RE_PC_EC_NEXTDIR,
					 Global.RE_PC_TR_NEXTDIR,
					 Global.SIGNIFICANCE_GC_NEXTDIR,
					 Global.SIGNIFICANCE_PC_NEXTDIR)
	
	for (dd in dirs) 
		if (dir.exists(dd))
			unlink(dd, recursive = T)
}


# ---------------------------------- PIPELINE SEQUENCE -----------------------------
cat(sprintf("PIPELINE has started...\n"))


##### STEP-1: flushing all the previous results in Global.DATA_DIR
cat(sprintf("FLUSHING previous data..\n"))
flush_previous_data();  


##### STEP0: Testing pre-conditions
announce("Step 0, Pipeline_InitialSetup")

tA = Sys.time()
e1 = new.env()
sys.source("steps/00_InitialSetup.R", envir = e1)
getTiming(tA, Sys.time(), "Step 0, Pipeline_InitialSetup")
  

##### STEP1: Ranking, computes so-called topological ranking (R2s within the master thesis)
announce("Step 1, RANKING: R2 topological") 

tA = Sys.time()
  e1 = new.env()
  sys.source("steps/01_ranking.R", envir = e1)
getTiming(tA, Sys.time(), "Step 1, RANKING: R2 topological")

G = read.graph(file=Global.G.file, format="gml")
flag_GC = "measure" %in% names(edge.attributes(G))  
if (flag_GC) {  # NOTICE: R1 is computed iff G has an external weight attached to its edges (E(G)$measure)

  announce("Step 1, RANKING: R1 functional")  # computes functional ranking (R1) iff E(G)$measure defined 
  
  tA = Sys.time()
    e1 = new.env()
    ranking_nowf = sprintf("steps/01_ranking_nowf_%s.R", Global.ElementType)
    sys.source(ranking_nowf, envir = e1)
  getTiming(tA, Sys.time(), "Step 1, Pipeline_RANKING: R1 functional")
}


##### STEP2: Global Comparison
if (flag_GC) {  # As a consequence, we compute a Global Comparison iff R1 has been computed above.
  announce("Step 2, Pipeline_GlobalComparison")

  tA = Sys.time()
    e1 = new.env()
    sys.source("steps/02_GlobalComparison.R", envir = e1)
  getTiming(tA, Sys.time(), "Step 2, Pipeline_GlobalComparison")
} else {
  cat(sprintf("\t** SKIPPED: no functional weights available.\n"))
}


##### STEP3: Progressive Comparison
announce("Step 3, Pipeline_ProgressiveComparison")

tA = Sys.time()
  e1 = new.env()
  if (Global.network_type == "PPIN") {
  	 sys.source("steps/03_ProgressiveComparison_PPIN.R", envir = e1)

  } else if (Global.network_type == "GENE") {
  		sys.source("steps/03_ProgressiveComparison_GENE.R", envir = e1)

  } else {
  	stop("Error: invalid network type")
  }
getTiming(tA, Sys.time(), "Step 3, Pipeline_ProgressiveComparison")



##### STEP4: Generate random rankings for subsequent Monte Carlo Hypothesis Testing, only if they do not exists.
#####        Permutations are stored (and readed from) in PERMS_FILE (parameter in PIPELINE_PARAMS.R)
announce("Step 4, Pipeline_GenerateRandomRankings")

tA = Sys.time()
if (! dir.exists(Global.RR_NEXTDIR)) {
  e1 = new.env()
  sys.source("steps/04_Generate_EC_TR_random_rankings.R", envir = e1)
} else {
  cat("Pipeline_GenerateRandomRankings: random rankings found\n.")
}
getTiming(tA, Sys.time(), "Step 4, Pipeline_GenerateRandomRankings")


##### STEP5: Compute Progressive Comparison for EC and TR random rankings generated at step 4
announce("Step 5, Pipeline_RandomExperiments_PC")

tA = Sys.time()

	### EC
  e1 = new.env()
  if (Global.network_type == "PPIN") {
  	sys.source("steps/05_PC_EC_random_PPIN.R", envir = e1)
  } else if (Global.network_type == "GENE") {
  	sys.source("steps/05_PC_EC_random_GENE.R", envir = e1)
  } else {
  	stop("Error: invalid network type")
  }

  ### TR
  e1 = new.env()
  if (Global.network_type == "PPIN") {
  	sys.source("steps/05_PC_TR_random_PPIN.R", envir = e1)
  } else if (Global.network_type == "GENE") {
  	sys.source("steps/05_PC_TR_random_GENE.R", envir = e1)
  } else {
  	stop("Error: invalid network type")
  }

getTiming(tA, Sys.time(), "Step 5, Pipeline_RandomExperiments_PC")

 
##### STEP6: GC of random rankings + Significance GC
#####        Of note, here we compute the global comparison of random rankings AND
#####        test for significance in a single script; whereas, we separate progressive
#####        comparison of random rankings and their significance in different steps (Steps 5 and 7)
announce("Step 6, Pipeline_RandomExperiments_GC + Significance_GC")

if (flag_GC) {
  tA = Sys.time()
    e1 = new.env()
    sys.source("steps/06_GC_EC_TR_MCRandomExperiments_and_Significance.R", envir = e1)
  getTiming(tA, Sys.time(), "Step 6, Pipeline_RandomExperiments_GC + Significance_GC")
} else {
  cat(sprintf("** SKIPPED **: Significance GC, no functional weights available."))
}

##### STEP7: PC of random rankings + Significance PC
announce("Step 7, Significance PC")

tA = Sys.time()
if (Global.network_type == "GENE") {

	e1 = new.env()
	sys.source("steps/07_Significance_PC_EC_TR_GENE.R", envir = e1)
	
} else if (Global.network_type == "PPIN") {
	
	e1 = new.env()
	sys.source("steps/07_Significance_PC_EC_TR_PPIN.R", envir = e1)
	
} else {
	stop("Error: invalid network type")
}

getTiming(tA, Sys.time(), "Step 7, Significance_PC")