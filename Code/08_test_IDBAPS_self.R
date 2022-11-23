
rm(list = ls())

source("Code/Helpers.R")
source("Code/Helpers/CVer.R")

source("Code/01_Label.R")
source("Code/02_Classify.R")

# Create the data set for testing -----------------------------------------

dsSize <- c("long", "wide")[2]

unFreeze("IDBAPS", "v2")

if (dsSize == "wide") {
  fullPredictTableI <- tableClassI %>% 
    na.omit.verbose() 
} else {
  fullPredictTableI <- tableClassI %>% 
    select(which(catNames(.) != "MSFC")) %>% 
    na.omit.verbose()
}

table(fullPredictTableI$Predicted.class)

predictTableI <- fullPredictTableI %>% select(-starts_with("Meta"))
epicidsI <- (fullPredictTableI %>% select(starts_with("MetaSubj")))[[1]]


# Get the predictors ------------------------------------------------------

source("Code/05_predictors.R")


# Run and test for limited testing dataset --------------------------------

if (dsSize == "wide") {
  featCats <- c("Core", "MSFC", "Patient")
} else {
  featCats <- c("Core", "Patient")
}

createCatGroups <- function(featCats, level = length(featCats)) {
  cCG <- function(featCats, level) {
    if (level == 0) {
      return(list(NULL))
    } else {
      sapply(featCats, 
             function(featCat) lapply(cCG(featCats, level - 1),
                                      'c', featCat))
    }
  }
  unique(lapply(lapply(c(cCG(featCats, level)), sort), unique))
}

catGroups <- createCatGroups(featCats)

runAll(predictTableI, 4, 10,
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
                         KNNC = KNNCPred),
       catGroups = catGroups,
       nParallel = 2,
       # parallel = TRUE,
       prefix = "IdBaps",
       partition = epicidsI)






