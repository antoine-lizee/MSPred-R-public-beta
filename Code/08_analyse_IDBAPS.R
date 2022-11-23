# 08
# Read and run analysis on the results from the ML framework for the testing run.
#
# Copyright Antoine Lizee 2016 antoine.lizee@ucsf.edu

source("Code/Helpers.R")
source("Code/Helpers/CVer.R")
source("Code/01_Label.R")


# Extract data ----------------------------------------------------------

yExtr <- extractYpred("Testv2-") #2093: Core only #1985: all three runs
perfDfTot <- perfYpredDf(yExtr = yExtr, perFolds = TRUE)
foldM <- as.numeric(names(giveMeTable(yExtr$FIs))[2])
perfDf <- perfDfTot %>% 
  mutate(run = factor(fold == foldM, labels = c("IDBAPS -> EPIC", "EPIC -> IDBAPS"))) %>% 
  # filter(! fold == foldM) %>% 
  filter(catGroup %in% c("MS", "Co", "CoMS")) %>% 
  mutate(catGroup = sapply(strsplit(catGroup, ""), function(splits) {
    if (length(splits) == 1) return(splits)
    paste(paste0(
      splits[seq(1,length(splits), by = 2)], splits[seq(2,length(splits), by = 2)]),
      collapse = " \U00B7 ") # "\U2027", "\U00B7", "\U2981" [not working], "\U2022" [requires cairo_pdf] (getting bigger)
  }))


# All runs ----------------------------------------------------------------

perfDfLabels <- perfDf %>% group_by(catGroup) %>%
  do(data_frame(mPerf = mean(.$auc),
                stdPerf = sd(.$auc))) %>% 
  ungroup() %>% 
  mutate(catGroup = reorder(catGroup, mPerf))

ggTot <- ggplot(data = perfDf %>% 
                  mutate(catGroup = factor(catGroup, levels = levels(perfDfLabels$catGroup))), 
                aes(x = method, y = auc)) + theme_bw() +
  geom_hline(yintercept = 0.5, linetype = '33') +
  geom_point(aes(fill = run, color = run, shape = run), size = 2) +
  scale_shape_manual(values = c(21, 23)) +
  labs(title = "Comparing AUC from several cv runs") +
  theme(axis.text.x = element_text(angle = -60, hjust = 0)) + 
  facet_grid(~ catGroup)

ggTot
ggsave(ggTot, filename = file.path(yExtr$RD, "totPerfTest.pdf"), w = 10, h = 6)#, device = cairo_pdf)


# Singular plots ----------------------------------------------------------

perfDfAgg <- perfDf %>% 
  group_by(catGroup) %>% mutate(mPcat = mean(auc),
                                mP5cat = mean(aucpr5)) %>% 
  group_by(method) %>% mutate(mPmeth = mean(auc),
                              mP5meth = mean(aucpr5)) %>% 
  ungroup

perfDfNoCV <- perfDf %>% 
  group_by(method, catGroup) %>% summarise_each(funs(mean), auc, aucpr, aucpr5) %>% 
  ungroup

## Normalize
perfDfNorm <- perfDfAgg %>%
  rowwise() %>% 
  do(data_frame(method = .$method, 
                catGroup =  .$catGroup,
                run = .$run,
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

ROC <- "Receiver Operating Characteristic"
PR <- "Precision-Recall"

segDf <- data.frame(x1 = 0, x2 = c(1,0),
                    y1 = c(0, 0), y2 = c(1, 0),
                    type = c(ROC, PR))
ggSeg <- geom_segment(data = segDf, aes(x = x1, xend = x2, y = y1, yend = y2),
                      linetype = "32", size = 0.2)

perfDfPlot <- perfDfNorm %>% 
  filter(method %in% c("GLM", "SVM"), catGroup %in% c("Co", "MS", "Co · MS"))

ggPerfRuns <- perfDfPlot %>% select(x = fpr, y = tpr, method, run, catGroup) %>% mutate(type = ROC) %>% 
  bind_rows(perfDfPlot %>% select(x = recall, y = precision, method, run, catGroup) %>% mutate(type = PR)) %>%
  ggplot() + 
  ggSeg +
  geom_line(aes(x = x, y = y, color = run, linetype = method)) +
  facet_grid(type~catGroup) +
  theme_bw() + theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(face = "italic", size = 12)
  )

ggsave(ggPerfRuns, filename = file.path(yExtr$RD, "08_run_comparison.pdf"), w = 10, h = 8)


