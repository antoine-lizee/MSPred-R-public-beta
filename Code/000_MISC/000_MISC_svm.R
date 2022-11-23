
library(dplyr)
library(e1071)
library(ggplot2)


# Prepare data ------------------------------------------------------------

source("Code/Helpers.R")
source("Code/Helpers/CVer.R")

unFreeze("RawExtracted", "v2")
source("Code/01_Label.R")
source("Code/02_Classify.R")

# Create the delayed edss table
edssExpTable <- delayedEdss(tableFinal %>% 
                              select(EPICID, VisitID, t = ExamDate, EDSS))

# Create decision table
decTable <- classify(edssExpTable)


# Create the datasets -----------------------------------------------------

tableClass <- decTable %>% select(VisitID, class) %>% 
  left_join(tableFinal) %>% 
  filter(!is.na(class)) %>%
  doLabel()

fullPredictTable <- tableClass %>% 
  select(-c(Life.MemoryDecreaseLastYear, MRI.New_T2_Lesions),
         -starts_with("Genetics")) %>% 
  na.omit.verbose()

predictTable <- fullPredictTable %>% select(-starts_with("Meta"))
epicids <- (fullPredictTable %>% select(starts_with("MetaSubj")))[[1]]

Y <- predictTable$Predicted.class
catGroup <- c("Core", "FSSs")
X <- predictTable %>% select(grep(paste(catGroup, collapse = "|"), catNames(.)))

set.seed(123456789)
N <- nrow(X)
foldIdx <- sample(rep(1:(nF <- 5), length.out = N))

i <- 2
Xtrain <- X[foldIdx != i,]
Xtest <- X[foldIdx == i,]
Ytrain <- Y[foldIdx != i]
Ytest <- Y[foldIdx == i]


# run trial runs for predictors -------------------------------------------

t0 <- Sys.time()
svms <- createArgsDf(meth = c("fac", "num"),
                     ga = c(1*2^seq(-10,0,0.5)), 
                     cost = 10^seq(-1,2,by = 0.5), 
                     i = 1:nF, maxRuns = 10000) %>% tbl_df %>% 
  rowwise() %>% 
  mutate(model = list(e1071:::svm.default(x = data.matrix(X[foldIdx != i,]), 
                                          y = switch(substitute(meth),
                                                     fac = as.factor(Y[foldIdx != i]),
                                                     num = as.numeric(Y[foldIdx != i])),
                                          probability = substitute(meth) == "fac",
                                          gamma = substitute(ga),
                                          cost = substitute(cost)))) %>% 
  ungroup

cat(sprintf("Run in %s\n", format(Sys.time() - t0)))

svmsRes <- svms %>% 
  mutate( 
    res = mapply(function(modeli, probi, ii) {
      res <- e1071:::predict.svm(modeli, data.matrix(X[foldIdx == ii,]), probability = probi)
      if (probi) {
        res <- attr(res, "probabilities")[,1]
      }
      res
    }, model, meth == "fac", i, SIMPLIFY = FALSE ),
    perf = mapply(function(resi, ii) {
      AUROC(resi, Y[foldIdx == ii])
    }, res, i),
    perfpr = mapply(function(resi, ii) {
      AUCPR5(resi, Y[foldIdx == ii])
    }, res, i))

ggEx <- ggplot(svmsRes %>% filter(ga < 0.5)) + 
  stat_summary_2d(aes(x=as.factor(log2(ga)), 
                     y=interaction(meth, as.factor(log10(cost))), 
                     z = perf))
ggEx
ggsave("Output/SVMe1071Ex.pdf", ggEx, h = 6, w = 10)

ggExPR <- ggplot(svmsRes %>% filter(ga < 0.5)) + 
  stat_summary_2d(aes(x=as.factor(log2(ga)), 
                     y=interaction(meth, as.factor(log10(cost))), 
                     z = perfpr))
ggExPR
ggsave("Output/SVMe1071Ex_pr.pdf", ggExPR, h = 6, w = 10)

# svmsRes <- svms %>% 
#   mutate( res = mapply(function(modeli, probi) {
#     res <- e1071:::predict.svm(modeli, data.matrix(Xtest), probability = probi)
#     if (probi) {
#       res <- attr(res, "probabilities")[,1]
#     }
#     res
#   },
#   model, prob, SIMPLIFY = FALSE )) %>% 
#   select(-model, -prob) %>% 
#   tidyr::spread(meth, res) %>% rowwise() %>% 
#   mutate(WT = wilcox.test(fac, num)$p.value,
#          # plot = {plot(fac, num, xlab = "fac", ylab = "num", main = ga); TRUE},
#          perffac = AUROC(fac, Ytest),
#          perfnum = AUROC(num, Ytest))

# ggplot(svmsRes) + geom_tile(aes(x=as.factor(ga), y=as.factor(cost), fill = abs(0.5 - perffac)))
# ggplot(svmsRes) + geom_tile(aes(x=as.factor(ga), y=as.factor(cost), fill = abs(0.5 - perfnum)))


# result: probability = TRUE doesn't change anything but computation time when the problem is a regression. 




