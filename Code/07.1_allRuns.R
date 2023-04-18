# 07.1
# Sub script for Analysis which creates the plots
# Summarizing all the runs.
#
# Copyright Antoine Lizee @ UCSF 2016


# Full (messy) plot -------------------------------------------------------

qq1 <- qplot(data = perfDfVar %>% mutate(alpha = "bloup"), x = auc, y = aucpr5, 
             geom = "point", alpha = alpha, col = method) + 
  theme_bw() +
  scale_alpha_manual(values = 0.1, guide = FALSE)

ggsave(qq1, filename = file.path(yExtr$RD, "07.1_pr5VrocAll.pdf"),
       w = 12, h = 7)


# Show Method Performance ---------------------------------------------------------

perfDfQp <- perfDfVar %>% 
  filter(auc > 0.62) %>% droplevels

perfDfQpLabels <- perfDfQp %>% 
  group_by(catGroup, catGrDot, method) %>% 
  summarise_each(funs(mean)) %>% ungroup()

getQq <- function(x = "auc", y = "aucpr5", 
                  xlabel = x, ylabel = y, label = TRUE) {
  ggplot(data = perfDfQp %>% mutate(alpha = "bloup"), 
         aes_string(x = x, y = y, col = "catGroup")) +
    geom_point(aes(alpha = alpha)) +
    {if (label) {
      geom_label(data = perfDfQpLabels, aes(label = catGroup), size = 3, show.legend = FALSE)
    } else {
      geom_text(data = perfDfQpLabels, aes(label = catGroup), show.legend = FALSE)
    }} +
    theme_bw() +
    scale_alpha_manual(values = 0.15, guide = FALSE) +
    labs(x = xlabel, y = ylabel) +
    facet_wrap(~ method)
}

ggsave(getQq(xlabel = "Area under the ROC curve",
             ylabel = "Area under the first half of the Precisoin Recall curve"),
       filename = file.path(yExtr$RD, "07.1_pr5Vroc.pdf"),
       w = 12, h = 7)
ggsave(getQq(y = "aucpr", 
             xlabel = "Area under the ROC curve",
             ylabel = "Area under the Precisoin Recall curve"), 
       filename = file.path(yExtr$RD, "07.1_prVroc.pdf"),
       w = 12, h = 7)
ggsave(getQq(y = "macc",
             xlabel = "Area under the ROC curve",
             ylabel = "Maximum Accuracy"),
       filename = file.path(yExtr$RD, "07.1_totaccVroc.pdf"),
       w = 12, h = 7)


# Show all runs ----------------------------------------------------------

featCats <- catNames(cns = yExtr$cns)
featCount <- data.frame(cat = featCats) %>% 
  count(cat) %>% 
  mutate(cat2 = substr(cat, 1, 2))

perfDfLabels <- perfDf %>% group_by(catGrDot) %>%
  do(data_frame(mPerf = mean(.$auc),
                stdPerf = sd(.$auc),
                n = sum(featCount[sapply(featCount$cat2, grepl, .$catGrDot[1]),]$n))) %>% 
  ungroup() %>% 
  mutate(catGrDot = reorder(catGrDot, mPerf))

ggTot <- ggplot(data = perfDf %>% 
                  mutate(catGrDot = factor(catGrDot, levels = levels(perfDfLabels$catGrDot))), 
                aes(x = method, y = auc, color = catGrDot)) + theme_bw() +
  geom_hline(yintercept = 0.5, linetype = '33') +
  geom_boxplot(outlier.size = 0) +
  geom_point(position = position_jitter(width = 0.1), size = 2, alpha = 0.4, shape = 20) +
  labs(title = "Comparing AUC from several cv runs") +
  theme(axis.text.x = element_text(angle = -60, hjust = 0)) + 
  facet_wrap(~ catGrDot, nrow = 3) +
  geom_text(data = perfDfLabels, aes(label = sprintf('%d', n)), x = 2.5, y = 0.45, color = "black")

ggsave(ggTot, filename = file.path(yExtr$RD, "07.1_totPerf.pdf"), w = 25, h = 10)#, device = cairo_pdf)


# Alternate vertical viz --------------------------------------------------

dodge <- position_dodge(width = 0.75)
jitterdodge <- position_jitterdodge(dodge.width = 0.75, jitter.width = 0.1)

ggTotCat <- ggplot(data = perfDf %>% 
                     filter(method %in% c("RF", "SVM", "NB", "GLM", "KNN", "RP")) %>% droplevels %>% 
                     mutate(catGrDot = reorder(catGrDot, auc, mean)), 
                   aes(x = catGrDot, y = auc)) + 
  theme_bw() +
  geom_hline(yintercept = 0.5, linetype = "33") +
  geom_point(aes(color = method, fill = method, group = method), position = jitterdodge, 
             size = 2, alpha = 0.4, shape = 21) +
  stat_summary(aes(color = method, fill = method, group = method), position = dodge,
               fun.data = "mean_cl_boot", geom = "linerange", size = 0.5) +
  stat_summary(aes(color = method, fill = method, group = method), position = dodge,
               fun.y = "mean", shape = 4, geom = "point") +
  stat_summary(geom = "errorbar", fun.data = "mean_cl_boot",
               color = "grey80", width = 1, size = 0.5)  +
  stat_summary(geom = "errorbar", fun.y = "mean", fun.ymax = "mean", fun.ymin = "mean", 
               color = "black", size = 0.8)  +
  labs(title = "Comparing AUC from several cv runs") +
  scale_y_continuous(minor_breaks = 0.01) +
  coord_flip()

ggsave(ggTotCat,  filename = file.path(yExtr$RD, "07.1_totPerfV.pdf"), w = 10, h = 15)#, device = cairo_pdf)

