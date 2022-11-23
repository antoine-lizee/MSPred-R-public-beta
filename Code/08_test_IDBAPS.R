
rm(list = ls())

source("Code/Helpers.R")
source("Code/Helpers/CVer.R")

source("Code/01_Label.R")
source("Code/02_Classify.R")

# Create the data set for testing -----------------------------------------

dsSize <- c("long", "wide")[2]

unFreeze("IDBAPSNoConf", "v2")

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


# Create the dataset from the ref pop -----------------------------------------------------

unFreeze("RawExtracted", "v2")

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
  select(-c(Life.MemoryDecreaseLastYear, MRI.New_T2_Lesions),
         -starts_with("Genetics")) %>% 
  na.omit.verbose()

predictTable <- fullPredictTable %>% select(-starts_with("Meta"))
epicids <- (fullPredictTable %>% select(starts_with("MetaSubj")))[[1]]


# Merge data sets ---------------------------------------------------------

nTrain <- nrow(predictTable)
nTest <- nrow(predictTableI)
predictTableTot <- predictTable %>% bind_rows(predictTableI, .id = "partition") %>% 
  select(which(!apply(., 2, anyNA)))
testPartition <- as.integer(predictTableTot$partition)
predictTableTot <- predictTableTot %>% select(-partition)


# Get the predictors ------------------------------------------------------

source("Code/05_predictors.R")


# Run and test for limited testing dataset --------------------------------

if (dsSize == "wide") {
  featCats <- c("Core", "MSFC", "Patient")
} else {
  featCats <- c("Core", "Patient")
}

catGroups <- createCatGroups(featCats)

runAll(predictTableTot, 2, 1,
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
       prefix = "Testv2NoConf",
       partition = testPartition)






