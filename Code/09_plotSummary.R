
getAUCDf <- function(CVs) {
  
preds <- lapply(CVs, function(CV) apply(CV, 2, ROCR::prediction, labels = predictTable$Predicted.class))
# perfs <- lapply(preds, ROCR::performance, "tpr", "fpr")
preds <- unlist(preds)
aucs <- sapply(lapply(lapply(preds, ROCR::performance, "auc"), slot, "y.values"), '[[', 1)

aucsDf <- data.frame(AUC = aucs, 
                     Category = sub("[[:digit:]]{,3}$", "", names(aucs)),
                     N = as.numeric(sub("^[[:alpha:]]*", "", names(aucs))))
}

aucsDfTot <- rbind(data.frame(getAUCDf(RFCVs), method = "RF"),
                   data.frame(getAUCDf(GLMCVs), method = "GLM"))

aucsDfTot <- aucsDfTot %>% 
  mutate(Category = reorder(Category, AUC, mean))

ggplot(data = aucsDfTot, aes(x = interaction(method, Category), y = AUC, color = Category)) + theme_bw() +
  geom_boxplot(outlier.size = 0) +
  geom_point(position = position_jitter(width = 0.1), size = 2, alpha = 0.8) +
  labs(title = "Comparing AUC from 30 cv runs")