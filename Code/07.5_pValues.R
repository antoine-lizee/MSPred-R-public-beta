# 07.5
# Sub script for Analysis, computed p-values
# to compare performance with different methods.
#
# Copyright Antoine Lizee @ UCSF 2016


# Delong ------------------------------------------------------------------

library(pROC)

## 'All' feature set, first CV 
perfDfSub <- perfDf %>% filter(cv == 1, catGroup == ".")
# Create auc objects
as <- lapply(perfDfSub$pred, function(pred) roc(pred@labels[[1]], pred@predictions[[1]], direction = "<"))
# Compute pval matrix
C <- sapply(as, function(a1) sapply(as, function(a2) roc.test(a1, a2)$p.value))
dimnames(C) <-  list(perfDfSub$method, perfDfSub$method)
C <- C[order(-perfDfSub$auc), order(-perfDfSub$auc)]

pheatmap(C, cluster_rows = FALSE, cluster_cols = FALSE,
         main = "p-values computed based on first CV for all features",
         filename = file.path(yExtr$RD, "07.5_pValMatDelong_Allcv1.pdf"),
         w = 10, h = 7) 

## 'GLM' algo, first CV 
perfDfSub <- perfDf %>% filter(cv == 1, method == "GLM")
# Create auc objects
as <- lapply(perfDfSub$pred, function(pred) roc(pred@labels[[1]], pred@predictions[[1]], direction = "<"))
# Compute pval matrix
C <- sapply(as, function(a1) sapply(as, function(a2) roc.test(a1, a2)$p.value))
dimnames(C) <-  list(perfDfSub$method, perfDfSub$method)
C <- C[order(-perfDfSub$auc), order(-perfDfSub$auc)]

C10 <- apply(log10(C), 1, pmax, -4)

pheatmap(C10, cluster_rows = FALSE, cluster_cols = FALSE,
         main = "p-values computed based on first CV for GLM",
         filename = file.path(yExtr$RD, "07.5_pValMatDelong_GLMcv1.pdf"),
         w = 10, h = 7) 
# pheatmap((C > 0.05) + 0, cluster_rows = FALSE, cluster_cols = FALSE,
#          main = "p-values computed based on first CV for GLM, 5e-2") 


# Use the different runs as paired observations ---------------------------

## Look across feature sets
perfDfVec <- perfDf %>% 
  mutate(meth_cv = paste(method, cv, sep = "_")) %>% 
  select(catGrDot, meth_cv, auc) %>% 
  tidyr::spread(meth_cv, auc)
  
C <- apply(perfDfVec[-1], 1, 
           function(vec) sapply(
             apply(perfDfVec[-1], 1, wilcox.test, y = vec, paired = TRUE), #t.test wiclox.test
             '[[', 'p.value'))
dimnames(C) <- list(perfDfVec$catGrDot, perfDfVec$catGrDot)
perfDfVecM <- apply(perfDfVec[,-1], 1, mean)
C <- C[order(-perfDfVecM), order(-perfDfVecM)]
C[is.na(C)] <- 1

C10 <- apply(log10(C), 1, pmax, -4)

pheatmap(C, cluster_rows = FALSE, cluster_cols = FALSE,
         breaks = 10^(c(-50, seq(-3, 0, length.out = 100))), 
         legend = FALSE,
         display_numbers = TRUE, number_format = "%.1g",
         na_col = "grey",
         main = "p-values computed from wilcox test",
         filename = file.path(yExtr$RD, "07.5_pValMatCats.pdf"),
         fontsize_number = 5,
         w = 10, h = 7) 
# pheatmap(C10, cluster_rows = FALSE, cluster_cols = FALSE) 
# pheatmap((C > 0.05) + 0, cluster_rows = FALSE, cluster_cols = FALSE)

## Look across Method
perfDfVec <- perfDf %>% 
  mutate(cat_cv = paste(catGroup, cv, sep = "_")) %>% 
  select(method, cat_cv, auc) %>% 
  tidyr::spread(cat_cv, auc)

C <- apply(perfDfVec[-1], 1, 
           function(vec) sapply(
             apply(perfDfVec[-1], 1, wilcox.test, y = vec, paired = TRUE), #t.test wiclox.test
             '[[', 'p.value'))
dimnames(C) <- list(perfDfVec$method, perfDfVec$method)
perfDfVecM <- apply(perfDfVec[-1,], 1, mean)
C <- C[order(-perfDfVecM), order(-perfDfVecM)]
C[is.na(C)] <- 1

C10 <- apply(log10(C), 1, pmax, -4)

pheatmap(C, cluster_rows = FALSE, cluster_cols = FALSE,
         breaks = 10^(c(-50, seq(-3, 0, length.out = 100))), 
         legend = FALSE,
         display_numbers = TRUE, number_format = "%.2g",
         na_col = "grey",
         main = "p-values computed from wilcox test",
         filename = file.path(yExtr$RD, "07.5_pValMatMeths.pdf"),
         w = 5, h = 4) 
# pheatmap(C10, cluster_rows = FALSE, cluster_cols = FALSE) 
# pheatmap((C > 0.05) + 0, cluster_rows = FALSE, cluster_cols = FALSE)



