## 04_Predict
# This script creates the first predictive analysis of
# the dataset.
#
# Copyright Antoine Lizee 2015-10

library(randomForest)
library(ROCR)

source("Code/Helpers.R")

# Create the datasets -----------------------------------------------------

predictTable <- table %>% 
  select(-c(Life.MemoryDecreaseLastYear, MRI.New_T2_Lesions),
         -starts_with("Genetics")) %>% 
  na.omit.verbose()


# Predict -----------------------------------------------------------------

if (generalRF <- F) {
  rf1 <- randomForest(Predicted.class ~ .,
                      ntree = 1000,
                      data = predictTable,
                      importance = TRUE)
  
  rf1.cat <- randomForest(Predicted.class ~ .,
                          ntree = 1000,
                          data = predictTable %>% 
                            mutate(Predicted.class = as.factor(Predicted.class)),
                          importance = TRUE)
  
  pdf("varImp1.pdf", w = 10, h = 7)
  varImpPlot(rf1)
  dev.off()
}

# CVs ---------------------------------------------------------------------

# cvRes <- rfcv(predictTable[-1], predictTable$Predicted.class)
# cvResClassif <- rfcv(predictTable[-1], factor(predictTable$Predicted.class))


# Assessment --------------------------------------------------------------

# preds <- lapply(cvRes$predicted, ROCR::prediction, labels = predictTable$Predicted.class)
# perfs <- lapply(preds, ROCR::performance, "tpr", "fpr")
# sapply(lapply(lapply(preds, ROCR::performance, "auc"), slot, "y.values"), '[[', 1)
# ROCR::plot(perfs)


# Variable X --------------------------------------------------------------

featCats <- catNames(predictTable)[-1]
baseCats <- c("None")
cvN <- 30
foldN <- 4
foldIdxs <- replicate(sample(1:foldN, nrow(predictTable), replace = T), n = cvN)
RFCVs <- sapply(unique(featCats), simplify = FALSE, function(featCat) {
  cat(sprintf("Treating Category %s...\n", featCat))
  X <- predictTable %>% select(grep(paste(c(baseCats, featCat), collapse = "|"), colnames(.)))
  cat(sprintf("Selected columns:\n%s\n", paste(colnames(X), collapse = ", ")))
  Y <- predictTable$Predicted.class
  Yps <- replicate(rep(NA, length(Y)), n = cvN)
  for (i_cv in 1:cvN) {
    cat(sprintf("CV num %d, Folds:", i_cv))
    foldIdx <- foldIdxs[, i_cv]
    for (i in 1:foldN) {
      cat(sprintf("- %d ", i))
      rf <- randomForest(X[foldIdx != i,], Y[foldIdx != i], X[foldIdx == i,], Y[foldIdx == i])
      Yps[foldIdx == i, i_cv] <- rf$test$predicted
    }
    cat("\n")
  }
  return(Yps)
})

save(list = c("RFCVs"), file = sprintf("Output/postRF_%s.RData", paste0(baseCats, collapse = "-")))

# aucs <- lapply(RFCVs, function(cvRes) {
#   preds <- lapply(cvRes$predicted, ROCR::prediction, labels = predictTable$Predicted.class)
#   perfs <- lapply(preds, ROCR::performance, "tpr", "fpr")
#   sapply(lapply(lapply(preds, ROCR::performance, "auc"), slot, "y.values"), '[[', 1)
# })

preds <- lapply(RFCVs, function(RFCV) apply(RFCV, 2, ROCR::prediction, labels = predictTable$Predicted.class))
# perfs <- lapply(preds, ROCR::performance, "tpr", "fpr")
preds <- unlist(preds)
aucs <- sapply(lapply(lapply(preds, ROCR::performance, "auc"), slot, "y.values"), '[[', 1)

aucsDf <- data.frame(AUC = aucs, 
                     Category = sub("[[:digit:]]{,3}$", "", names(aucs)),
                     N = as.numeric(sub("^[[:alpha:]]*", "", names(aucs))))

ggplot(data = aucsDf, aes(x = Category, y = AUC, color = Category)) + theme_bw() +
  geom_boxplot(outlier.size = 0) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.8) +
  labs(title = "Comparing AUC from 30 cv runs")

ggplot(data = aucsDf %>% mutate(no = order()), aes(x = N, y = AUC, color = Category)) + theme_bw() +
  # geom_boxplot(outlier.size = 0) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Comparing AUC from 30 cv runs")

