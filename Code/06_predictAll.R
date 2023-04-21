## 06_PredictAll.R
# This script creates runs the Whole predictive analysis.
#
# Copyright Antoine Lizee 2015-10

source("Code/Helpers.R")
source("Code/Helpers/CVer.R")
source("Code/01_Label.R")


# Get the datasets -----------------------------------------------------

# Other version finally removed at commit 8d628e9, see 03_ file for the creation of the cleanWithAll dataset

# unFreeze("cleanWithTreatments", "v1")
unFreeze(
  # "cleanWithTreatments"  ## Older version
  "cleanWithAll"
  # "cleanWithoutMRIandGenetics"
  , "v1"
)


# Define Predictors -------------------------------------------------------

source("Code/05_predictors.R")


# Run ---------------------------------------------------------------------

featCats <- sort(unique(catNames(predictTable)[-1]))

allSingles <- createCatGroups(featCats, 1)
allDoubles <- createCatGroups(featCats, 2)
allTriplets <- createCatGroups(featCats, 3)

Socle <- c("Core", "Patient", "Treatments")
Classical <- c("Clinical", "FSSs", "MRI")
PatientCentric <- c("QOL", "Life", "MSFC")

catGroups <- uniquifyCatGroups(c(
  
  # # All doubles + triplets with Co or FS (latest)
  # allTriplets[ sapply(allTriplets, function(group) {
  #   length(group) < 3 | any(c("Core", "FSS") %in% group )
  # })],

  allSingles,
  
  # Prove that Co and FS are the same
  list(c("Core", "FSSs")),

  # Build on top of Co
  allDoubles[ sapply(allDoubles, function(group) any(group == "Core")) ],

  # Build on top of Co + (MS or QOL)
  allTriplets[ sapply(allTriplets, function(group) any(group == "Core") & any(group %in% c("MSFC", "QOL"))) ],

  # # Build on top of Co, including triplets
  # allTriplets[ sapply(allTriplets, function(group) any(group == "Core")) ],

  # Show that Co or FS are necessary
  list(featCats[!featCats %in% c("Core", "FSSs")]),
  list(featCats[!featCats %in% c("FSSs")]),
  list(featCats[!featCats %in% c("Core")]),
  
  # all groups
  list("."),

  # Framework with Socle / Classical / Patient Centric
  list(Socle, c(Socle, Classical), c(Socle, PatientCentric))
))

runAll(predictTable, 5, 10,
       predictors = list(
         GLM = GLMPred,
         RF = RFPred,
         # RF2 = RFPred2,
         # RF3 = RFPred3,
         RFO = RFOPred,
         RP = RPPred,
         SVM = SVMPred,
         # SofSVM = SofSVMPred,
         NB = NBPred,
         KNN = KNNPred,
         # KNNC = KNNCPred,
         XGB = XGBPred
       ),
       catGroups = catGroups,
       parallel = TRUE,
       nParallel = 5,
       prefix = "All",
       partition = epicids
)


