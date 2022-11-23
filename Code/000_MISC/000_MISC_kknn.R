
library(dplyr)
library(e1071)
library(ggplot2)


# Prepare data ------------------------------------------------------------


source("Code/Helpers.R")
source("Code/Helpers/CVer.R")

unFreeze("RawExtracted", "v2")
source("Code/01_Label.R")
source("Code/02_Classify.R")


# Create the datasets -----------------------------------------------------

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

Y <- predictTable$Predicted.class
catGroup <- c("Clinical", "MRI")
catGroup <- c("Core", "FSSCs")
X <- predictTable %>% select(grep(paste(catGroup, collapse = "|"), catNames(.)))

# set.seed(123456789)
N <- nrow(X)
foldIdx <- sample(rep(1:(nF <- 5), length.out = N))

i <- 2
Xtrain <- X[foldIdx != i,]
Xtest <- X[foldIdx == i,]
Ytrain <- Y[foldIdx != i]
Ytest <- Y[foldIdx == i]


# Redefine the functions from predictAll ----------------------------------

## FROM: https://github.com/KlausVigo/kknn
trainPredictKNN <- function(Xtrain, Ytrain, Xtest) {
  library(kknn)
  ks <- c(6:19, seq(20, 200, 10))
  kknn.mod <- train.kknn(formula = Y ~ ., 
                         data = data.frame(Y=as.numeric(Ytrain), Xtrain),
                         ks = ks)
  AUCs <- sapply(lapply(kknn.mod$fitted.values, as.numeric), AUROC, ytest = Ytrain)
  kknn.mod$best.parameters$k <- attr(kknn.mod$fitted.values[[which.max(AUCs)]],'k')
  kknn.mod$best.performance <- AUCs[which.max(AUCs)]
  cats("KNN for groups %-35s (%3d features) ->  best k: %-3d / kernel: %-10s [perf: %.3f]\n", 
       paste(unique(catNames(Xtrain)), collapse = " - "), ncol(Xtrain), 
       kknn.mod$best.parameters$k, kknn.mod$best.parameters$kernel,
       kknn.mod$best.performance)
  predict(kknn.mod, Xtest)
}
KNNPred <- list(trainPredict = trainPredictKNN)

trainPredictKNNC <- function(Xtrain, Ytrain, Xtest) {
  library(kknn)
  kknn.modC <- train.kknn(formula = Y ~ ., 
                          data = data.frame(Y=factor(as.numeric(Ytrain)), Xtrain),
                          ks = c(6:19, seq(20, 200, 10)))
  cats("KNNC for groups %-35s (%3d features) ->  best k: %d / kernel: %s\n", 
       paste(unique(catNames(Xtrain)), collapse = " - "), ncol(Xtrain), 
       kknn.modC$best.parameters$k, kknn.modC$best.parameters$kernel)
  predict(kknn.modC, Xtest, type = "prob")[,2]
}
KNNCPred <- list(trainPredict = trainPredictKNNC)


# Double check ------------------------------------------------------------

tt <- list() 
for (i in 1:nF) 
  tt[[i]] <- trainPredictKNN(X[foldIdx != i,], Y[foldIdx != i], X[foldIdx == i,])

ttC <- list() 
for (i in 1:nF) 
  ttC[[i]] <- trainPredictKNNC(X[foldIdx != i,], Y[foldIdx != i], X[foldIdx == i,])






