# 07.3
# Sub script for Analysis which creates the plots
# Summarizing all the runs.
#
# Copyright Antoine Lizee @ UCSF 2016


# Preparing data ----------------------------------------------------------

perfDfAgg <- perfDf %>% 
  group_by(catGrDot) %>% mutate(mPcat = mean(auc),
                                mP5cat = mean(aucpr5)) %>% 
  group_by(method) %>% mutate(mPmeth = mean(auc),
                              mP5meth = mean(aucpr5)) %>% 
  ungroup

## Normalize
perfDfNorm <- perfDfAgg %>%
  rowwise() %>% 
  do(data_frame(method = .$method, 
                catGrDot =  .$catGrDot,
                catGroup = .$catGroup,
                cv = .$cv,
                mPmeth = .$mPmeth, 
                mP5meth = .$mP5meth,
                mPcat = .$mPcat,
                mP5cat = .$mP5cat,
                auc = .$auc,
                aucpr = .$aucpr,
                aucpr5 = .$aucpr5,
                tpr = .$roc@y.values[[1]],
                fpr = .$roc@x.values[[1]],
                acc = .$acc@y.values[[1]],
                cutoff = .$acc@x.values[[1]],
                precision = .$pr@y.values[[1]],
                recall = .$pr@x.values[[1]])
  ) %>% 
  ungroup

## Get for the example cat
perfDfMeth <- perfDfNorm %>% filter(cv == 1, 
                                    mPcat == max(mPcat), 
                                    method %in% c("GLM", "NB", "RF", "RF2", "RF3", "SVM"))

## Get for the example method
perfDfCat <- perfDfNorm %>% filter(cv == 1, 
                                   # mP5meth == max(mP5meth), 
                                   method == "SVM",
                                   catGroup %in% c("Cl", "Co", "CoMR", "CoQO", "CoMS", "FSMS", "."))

## Some plotting stuff
ROC <- "Receiver Operating Characteristic"
PR <- "Precision-Recall"
segDf <- data.frame(x1 = 0, x2 = 1,
                    y1 = c(p1, 0), y2 = c(p1, 1),
                    type = c(PR, ROC))
ggSeg <- geom_segment(data = segDf, aes(x = x1, xend = x2, y = y1, yend = y2),
                      linetype = "32", size = 0.2)
ggSegPR <- geom_segment(data = segDf[1,], aes(x = x1, xend = x2, y = y1, yend = y2),
                        linetype = "32", size = 0.2)
ggSegROC <- geom_segment(data = segDf[2,], aes(x = x1, xend = x2, y = y1, yend = y2),
                         linetype = "32", size = 0.2)

## The grid way (and even, not fully)
ggMeth <- rbind(
  perfDfMeth %>% 
    select(x = fpr, y = tpr, method) %>% mutate(type = ROC),
  perfDfMeth %>% 
    select(x = recall, y = precision, method) %>% mutate(type = PR)) %>% 
  ggplot() + 
  geom_line(aes(x = x, y = y, group = method, color = method)) +
  ggSeg +
  geom_text(data = perfDfCat[1,],
            aes(label = sprintf("group: %s", catGrDot)), x = 0.75, y = 0.1) +
  coord_equal(expand = TRUE) + theme_bw() +
  labs(x = NULL, y = NULL, color = "Algorithm: ") +
  facet_grid(. ~ type)

ggCat <- rbind(
  perfDfCat %>% 
    select(x = fpr, y = tpr, catGrDot) %>% mutate(type = ROC),
  perfDfCat %>% 
    select(x = recall, y = precision, catGrDot) %>% mutate(type = PR)) %>% 
  ggplot() + 
  geom_line(aes(x = x, y = y, group = catGrDot, color = catGrDot)) +
  ggSeg +
  geom_text(data = perfDfMeth[1,],
            aes(label = sprintf("method: %s", method)), x = 0.75, y = 0.1) +
  coord_equal(expand = TRUE) + theme_bw() +
  labs(x = NULL, y = NULL, color = "Feature\nGroups:") +
  facet_grid(. ~ type)

pdf(file.path(yExtr$RD, "07.3_perfRows.pdf"), w = 9, h = 8)
multiplot(ggCat, ggMeth)
dev.off()

## The multi-plot way
ggMethROC <- ggplot(perfDfMeth) + theme_bw() +
  geom_line(aes(x = fpr, y = tpr, color = method)) +
  labs(#title = ROC, 
    x = "false positive rate",
    y = "true positive rate") +
  ggSegROC +
  geom_text(data = perfDfMeth[1,],
            aes(label = sprintf("group: %s", catGrDot)), x = 0.75, y = 0.1) +
  coord_equal(expand = FALSE) + scale_color_discrete(guide = FALSE)

ggCatROC <- ggplot(perfDfCat) + theme_bw() +
  geom_line(aes(x = fpr, y = tpr, color = catGrDot)) +
  labs(#title = ROC, 
    x = "false positive rate",
    y = "true positive rate") +
  ggSegROC +
  geom_text(data = perfDfMeth[1,],
            aes(label = sprintf("method: %s", method)), x = 0.75, y = 0.1) +
  coord_equal(expand = FALSE) + scale_color_discrete(guide = FALSE)

ggMethPR <- ggplot(perfDfMeth) + theme_bw() +
  geom_line(aes(x = recall, y = precision, color = method)) +
  labs(#title = PR, 
    x = "recall",
    y = "precision",
    color = "Algorithm: ") +
  ggSegPR +
  geom_text(data = perfDfMeth[1,],
            aes(label = sprintf("group: %s", catGrDot)), x = 0.75, y = 0.1) +
  ylim(c(0, NA))
#   scale_x_continuous(expand = c(0, 0)) + 
#   scale_y_continuous(limits = c(0, 1), expand = c(0, 0))

ggCatPR <- ggplot(perfDfCat) + theme_bw() +
  geom_line(aes(x = recall, y = precision, color = catGrDot)) +
  labs(#title = PR, 
    x = "recall",
    y = "precision",
    color = "Feature\nGroups:") +
  ggSegPR +
  geom_text(data = perfDfMeth[1,],
            aes(label = sprintf("method: %s", method)), x = 0.75, y = 0.1) +
  ylim(c(0, NA))

pdf(file.path(yExtr$RD, "07.3_perfMat.pdf"), w = 11, h = 8)
multiplot(ggCatROC, ggMethROC, ggCatPR, ggMethPR, 
          layout = matrix(c(1,1,3,3,3,2,2,4,4,4), byrow = TRUE, nrow = 2))
dev.off()




