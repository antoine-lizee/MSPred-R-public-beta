# 05_predictGLM.R
#
#
# Copyright Antoine Lizee 2015-10

library(glmnet)
source("Code/Helpers.R")
source("Code/Helpers/cv.glmnet.R")


# Create the datasets -----------------------------------------------------

predictTable <- table %>% 
  select(-c(Life.MemoryDecreaseLastYear, MRI.New_T2_Lesions),
         -starts_with("Genetics")) %>% 
  na.omit.verbose()


# Predict -----------------------------------------------------------------

if (generalGLM <- F) {
  
  library(doParallel)
  cl <- parallel::makeCluster(2)
  doParallel::registerDoParallel(cl)
  
  alphas <- c(0, 0.01, 0.02, 0.05, 0.1, 0.5, 0.9, 0.95, 0.98, 0.99, 1)
  
  X = data.matrix(predictTable[-1])
  Y = predictTable[[1]]
  
  folds <- sample(1:10, size=nrow(X), replace=TRUE)
  cv.fits <- lapply(alphas,
                    function(alpha) my.cv.glmnet(X, Y, foldid = folds, keep = TRUE,
                                                 alpha = alpha, type.measure = "auc", family = "binomial", parallel = TRUE))
  
  
  parallel::stopCluster(cl)
  
  # See the cv plots
#   plotCvFit <- function(cv.fit, alpha) {
#     titleString = sprintf("alpha = %.2f, min.cv = %.3f, 1se.cv = %.3f\n ",
#                           alpha, cv.fit$cvm[cv.fit$lambda == cv.fit$lambda.min], cv.fit$cvm[cv.fit$lambda == cv.fit$lambda.1se])
#     plot(cv.fit, main = titleString)
#   }
#   par(ask = TRUE); mapply(plotCvFit, cv.fits, alphas); par(ask = FALSE)
  
  # Get summary plots
  lambdas <- sapply(cv.fits, '[', c("lambda.min", "lambda.1se"))
  values <- sapply(cv.fits, function(cv.fit) {
    c(unlist(cv.fit[c("lambda.min", "lambda.1se")]), 
      cv.min = cv.fit$cvm[cv.fit$lambda == cv.fit$lambda.min], 
      cv.1se = cv.fit$cvm[cv.fit$lambda == cv.fit$lambda.1se])
  })
  colnames(values) <- alphas
  values.df <- reshape2::melt(values, varnames = c("valueType", "alpha"))
  
  qp1 <- qplot(data = values.df %>% filter(grepl("cv", valueType)), x = alpha, y = value, color = valueType, geom = c("line", "point")) + 
    labs(title = "Performance for different values of alpha", y = "Cross-Validated AUROC") + theme_bw()
  qp2 <- qplot(data = values.df %>% filter(grepl("lambda", valueType)), x = alpha, y = value, color = valueType, geom = c("line", "point")) +
    labs(title = "Optimal penalty for different values of alpha", y = "Cross-Validated lambdas") + theme_bw()
  
  ggsave("Output/05_AUCvAlphaGen.pdf", qp1)
  ggsave("Output/05_LambdavAlphaGen.pdf", qp2)
  
}


# Variable X --------------------------------------------------------------

featCats <- catNames(predictTable)[-1]
baseCats <- c("None")
cvN <- 10
foldN <- 5
foldIdxs <- replicate(sample(1:foldN, nrow(predictTable), replace = T), n = cvN)
GLMCVs <- sapply(unique(featCats), simplify = FALSE, function(featCat) {
  cat(sprintf("Treating Category %s...\n", featCat))
  X <- data.matrix(predictTable %>% select(grep(paste(c(baseCats, featCat), collapse = "|"), colnames(.))))
  cat(sprintf("Selected columns:\n%s\n", paste(colnames(X), collapse = ", ")))
  Y <- predictTable$Predicted.class
  Yps <- replicate(rep(NA, length(Y)), n = cvN)
  for (i_cv in 1:cvN) {
    cat(sprintf("CV num %d, Folds: ", i_cv))
    foldIdx <- foldIdxs[, i_cv]
    for (i in 1:foldN) {
      cat(sprintf("- %d ", i))
      cv.fit <- cv.glmnet(X[foldIdx != i,], Y[foldIdx != i], alpha = 0.999, 
                          type.measure = "auc", family = "binomial")
      Yps[foldIdx == i, i_cv] <-  predict(cv.fit, X[foldIdx == i,], s = "lambda.1se")
    }
    cat("\n")
  }
  return(Yps)
})

save(list = c("GLMCVs"), file = sprintf("Output/postGLM_%s.RData", paste0(baseCats, collapse = "-")))

preds <- lapply(GLMCVs, function(GLMCV) apply(GLMCV, 2, ROCR::prediction, labels = predictTable$Predicted.class))
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



