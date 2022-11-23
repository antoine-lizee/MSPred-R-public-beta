
library(dplyr)
library(e1071)
library(ggplot2)


# Prepare data ------------------------------------------------------------


source("Code/Helpers.R")
source("Code/Helpers/CVer.R")

unFreeze("RawExtracted", "v2")
source("Code/01_Label.R")
source("Code/02_Classify.R")


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
catGroup <- c("Core", "FSSCs")
X <- predictTable %>% select(grep(paste(catGroup, collapse = "|"), catNames(.)))

set.seed(123456789)
N <- nrow(X)
foldIdx <- sample(rep(1:(nF <- 5), length.out = N))

i <- 2
Xtrain <- X[foldIdx != i,]
Xtest <-X[foldIdx == i,]
Ytrain <-Y[foldIdx != i]
Ytest <-Y[foldIdx == i]


# Runs --------------------------------------------------------------------

svms <- createArgsDf(learner_type = c("pegasos", "sgd-svm", 
                                      "passive-aggressive", "margin-perceptron", 
                                      "romma", "logreg-pegasos"),
                     loop_type = c("stochastic", "balanced-stochastic", 
                                   "rank", "roc", "query-norm-rank", 
                                   "combined-ranking", "combined-roc"),
                     lambda = 10^seq(-2, 2, length.out = 4*5 + 1),
                     i = 1:nF, maxRuns = 20000) %>% tbl_df %>% 
  mutate(model = mapply(function(learner_type, loop_type, lambda) { 
    res = sofia(Y ~ ., 
                data = data.frame(X[foldIdx != i,], Y = Y[foldIdx != i]), 
                learner_type = learner_type,
                loop_type = loop_type, 
                lambda = lambda)},
    learner_type, loop_type, lambda, SIMPLIFY = FALSE))

# Comparing the two versions of prediction_type is stupid since on is function of the other.

svmsRes <- svms %>% rowwise() %>% 
  mutate( log = list(predict(model, data.frame(X[foldIdx == i,], Y = 1), prediction_type = "logistic")),
          time = model$training_time) 

svmsPerf <- svmsRes %>% filter(!any(is.na(log))) %>%
  mutate(perf = AUROC(log, Y[foldIdx == i]))

ggEx <- ggplot(svmsPerf %>% filter(perf > 0.66)) + 
  stat_summary_2d(aes(x=as.factor(log10(lambda)), 
                      y=interaction(learner_type, loop_type), 
                      z = perf))
ggEx
ggsave("Output/SVMSophiaEx.pdf", ggEx, h = 6, w = 10)

ggt <- ggplot(svmsPerf %>% filter(perf > 0.66)) + 
  stat_summary_2d(aes(x=as.factor(log10(lambda)), 
                      y=interaction(learner_type, loop_type), 
                      z = time))
ggt
ggsave("Output/SVMSophiaTime.pdf", ggt, h = 6, w = 10)





