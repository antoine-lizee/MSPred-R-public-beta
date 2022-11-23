# 07.4
# Sub script for Analysis which creates the tables
# summarizing performance across algorithms and feature sets.
#
# Copyright Antoine Lizee @ UCSF 2016


# Prepare data ------------------------------------------------------------

perfSummaryDf <- perfDfRunS %>% 
  filter(method %in% c("GLM", "NB", "RFO", "KNN", "SVM"),
         catGroup %in% c("MR", "Co", "FS", "MS", "CoMR", "CoQO", "CoFS", "FSMS", "."))


# Table with ggplot -------------------------------------------------------

ggSum <- perfSummaryDf %>% 
  mutate(method = reorder(method, auc_mean, mean),
         catGrDot = reorder(catGrDot, auc_mean, mean)) %>% 
  ggplot(aes(x = catGrDot, y = method)) +
  geom_tile(aes(fill = auc_mean)) +
  geom_text(aes(label = sprintf("%.2f", auc_mean))) +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = FALSE) +
  labs(x = NULL, y = NULL) +
  # theme_minimal()
  theme_classic() + theme(axis.text.x = element_text(angle = -30, hjust = 0))
ggsave(ggSum, filename = file.path(yExtr$RD, "07.4_summaryTableOrdered.pdf"),
       w = 5, h = 3)

ggSumF <- perfSummaryDf %>% 
  mutate(method = reorder(method, auc_mean, mean),
         catGrDot = reorder(catGrDot, auc_mean, mean)) %>% 
  ggplot(aes(x = catGrDot, y = method)) +
  geom_tile(aes(fill = auc_mean)) +
  geom_text(aes(label = sprintf("%.2f\n\u00B1%.3f", auc_mean, auc_se))) +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = FALSE) +
  labs(x = NULL, y = NULL) +
  # theme_minimal()
  theme_classic() + theme(axis.text.x = element_text(angle = -30, hjust = 0))
ggsave(ggSumF, filename = file.path(yExtr$RD, "07.4_summaryTableOrderedFull.pdf"),
       w = 5, h = 3)


# Table with pheatmap -----------------------------------------------------

perfSummaryMat <- perfSummaryDf %>%
  select(catGrDot, method, auc_mean) %>% 
  tidyr::spread(catGrDot, auc_mean)
perfSummaryMat <- as.matrix(
  data.frame(perfSummaryMat[,-1], 
             row.names = perfSummaryMat[[1]])
  )

pheatmap(perfSummaryMat,
         color = colorRampPalette(
           RColorBrewer::brewer.pal(n = 9, 
                                    name = "Greens")[-c(5,7,9)])(100),
         display_numbers = TRUE,
         treeheight_row = 0,
         treeheight_col = 0,
         # cluster_rows = F,
         # cluster_cols = F,
         fontsize = 11,
         fontsize_number = 11,
         legend = FALSE,
         filename = file.path(yExtr$RD, "07.4_summaryTable.pdf"),
         w = 5, h = 3)
