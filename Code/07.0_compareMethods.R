# 07.0
# Exploratory Sub-script for Analysis which let us
# compare methods to each other.
#
# Copyright Antoine Lizee @ UCSF 2016

# Define function ---------------------------------------------------------

perfDfQp <- perfDfVar %>% 
  filter(auc > 0.62) %>% droplevels

plotCompMeth <- function(meth1, meth2, label = TRUE) {

  perfDfQpComp <- perfDfQp %>% filter(method == meth1) %>% select(catGroup, catGrDot, cv, x1 = auc, y1 = aucpr5) %>% 
    inner_join(perfDfQp %>% filter(method == meth2) %>% select(catGroup, catGrDot, cv, x2 = auc, y2 = aucpr5)) %>% 
    mutate(x = x1 - x2, y = y1 - y2)
  
  perfDfQpCompLabels <- perfDfQpComp %>% 
    group_by(catGroup, catGrDot) %>% 
    summarise_each(funs(mean)) %>% ungroup()
  
  ggplot(data = perfDfQpComp %>% mutate(alpha = "bloup"), 
         aes(x = x, y = y, col = catGroup)) +
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
    geom_point(aes(alpha = alpha)) +
    {if (label) {
      geom_label(data = perfDfQpCompLabels, aes(label = catGroup), size = 3, show.legend = FALSE)
    } else {
      geom_text(data = perfDfQpCompLabels, aes(label = catGroup), show.legend = FALSE)
    }} +
    theme_bw() +
    scale_alpha_manual(values = 0.5, guide = FALSE) +
    labs(title = sprintf("%s - %s", meth1, meth2), x = "auc", y = "aucpr5")
  
}


# Output ------------------------------------------------------------------

ggsave(file.path(yExtr$RD, "07.0_RFvRFO.pdf"), plotCompMeth("RF", "RFO"), w = 9, h = 8)
ggsave(file.path(yExtr$RD, "07.0_RFvRF2.pdf"), plotCompMeth("RF", "RF2"), w = 9, h = 8)
ggsave(file.path(yExtr$RD, "07.0_RFvRF3.pdf"), plotCompMeth("RF", "RF3"), w = 9, h = 8)
ggsave(file.path(yExtr$RD, "07.0_KNNvKNNC.pdf"), plotCompMeth("KNN", "KNNC"), w = 9, h = 8)
ggsave(file.path(yExtr$RD, "07.0_SVMvSVMPR.pdf"), plotCompMeth("SVM", "SVMPR"), w = 9, h = 8)
