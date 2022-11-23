## 06_PredictAll.R
# This script creates runs the Whole predictive analysis.
#
# Copyright Antoine Lizee 2015-10

source("Code/Helpers.R")
source("Code/Helpers/CVer.R")
source("Code/01_Label.R")


# Create the datasets -----------------------------------------------------

if (create <- FALSE) {
  unFreeze("RawExtractedWithTreatments", "v2")
  source("Code/02_Classify.R")
  
  # Create the delayed edss table
  edssExpTable <- delayedEdss(tableFinal %>% 
                                select(EPICID, VisitID, t = ExamDate, EDSS))
  
  # Create decision table
  decTable <- classify(edssExpTable)
  
  # Create final, labeled dataset
  tableClass <- decTable %>% select(VisitID, class) %>% 
    left_join(tableFinal) %>% 
    filter(!is.na(class)) %>%
    doLabel()
  
  fullPredictTable <- tableClass %>% 
    select(-c(Life.MemoryDecreaseLastYear, MRI.New_T2_Lesions)) %>%   # Remove very sparse features
    select(-starts_with("Genetics"), -c(Life.Overweight)) %>%   # Remove sparse features sparsity-correlated
    select(-Treatments.DMTname) %>%  # Remove raw DMT name information
    na.omit.verbose()
  
  predictTable <- fullPredictTable %>% select(-starts_with("Meta"))
  epicids <- (fullPredictTable %>% select(starts_with("MetaSubj")))[[1]]
  
  freeze(c("fullPredictTable", "predictTable", "epicids"), "cleanWithTreatments", "v1")
} else {
  unFreeze("cleanWithTreatments", "v1")
}

# Define Predictors -------------------------------------------------------

source("Code/05_predictors.R")


# Run ---------------------------------------------------------------------

featCats <- sort(unique(catNames(predictTable)[-1]))
catGroups <- createCatGroups(featCats, 3)
catGroups <- c(
  catGroups[ sapply(catGroups, function(group) {
    length(group) < 3 | any(c("Core", "FSS") %in% group )
  })],
  list(".")  # all groups
)

runAll(predictTable, 3, 2, 
       predictors = list(GLM = GLMPred,
                         RF = RFPred,
                         RF2 = RFPred2,
                         RF3 = RFPred3,
                         RFO = RFOPred,
                         RP = RPPred,
                         SVM = SVMPred,
                         # SofSVM = SofSVMPred,
                         NB = NBPred,
                         KNN = KNNPred,
                         KNNC = KNNCPred,
                         XGB = XGBPred),
       catGroups = catGroups,
       parallel = TRUE,
       nParallel = 2,
       prefix = "Subj",
       partition = epicids)


