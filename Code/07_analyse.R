# 07_Analyse.R
# Read and run analysis on the results from the ML framework.
#
# Copyright Antoine Lizee 2016 antoine.lizee@ucsf.edu

source("Code/Helpers.R")
source("Code/Helpers/CVer.R")
source("Code/01_Label.R")


# Extract & Compute performance data ----------------------------------------------------------

yExtr <- extractYpred("2022")
perfDfTot <- perfYpredDf(yExtr = yExtr)
perfDf <- perfDfTot %>% 
  filter(!method %in% c("SofSVM")) %>% 
  mutate(
    catGrDot = sapply(strsplit(catGroup, ""), function(splits) {
    if (length(splits) == 1) return("All")
    paste(paste0(
      splits[seq(1,length(splits), by = 2)], splits[seq(2,length(splits), by = 2)]),
      collapse = " \U00B7 ") # "\U2027", "\U00B7", "\U2981" [not working], "\U2022" [requires cairo_pdf] (getting bigger)
  }))

perfDfVar <- perfDf %>% 
  select(-pred, -roc, -pr, -rch, -acc)

perfDfCatMeth <- perfDf %>% 
  group_by(method, catGrDot) %>% 
  mutate(medAucpr5 = median(aucpr5),
         medAuroc = median(auc)) %>%
  ungroup()

perfDfRunS <- perfDf %>% 
  group_by(method, catGrDot, catGroup) %>% 
  summarise_each(funs(mean,
                      se = sd(.)/sqrt(length(.))), 
                 auc, aucpr, aucpr5) %>% 
  ungroup

ns <- table(yExtr$y)
p1 <- ns["TRUE"] / sum(ns)


# Write 2by2s -------------------------------------------------------------

source("Code/07.2_pointEstimates.R")


# plot all runs -----------------------------------------------------------

source("Code/07.1_allRuns.R")


# Show individual curves --------------------------------------------------

source("Code/07.3_singleCurves.R")


# Report performance tables ------------------------------------

source("Code/07.4_performanceTable.R")


# Compute p-values --------------------------------------------------------

source("Code/07.5_pValues.R")

