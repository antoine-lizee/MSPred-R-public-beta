
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
catGroup <- c("Clinical", "MRI")
catGroup <- c("Core", "FSSCs")
X <- predictTable %>% select(grep(paste(catGroup, collapse = "|"), catNames(.)))

set.seed(123456789)
N <- nrow(X)
foldIdx <- sample(rep(1:(nF <- 5), length.out = N))

i <- 2
Xtrain <- X[foldIdx != i,]
Xtest <- X[foldIdx == i,]
Ytrain <- Y[foldIdx != i]
Ytest <- Y[foldIdx == i]


# Explo -------------------------------------------------------------------

library(rpart)
rp <- rpart(formula = Y ~ ., 
            data = data.frame(Xtrain, Y=as.numeric(Ytrain)),
            method = "class",
            # parms = list(prior = unname(table(Ytrain))/length(Ytrain)),
            control = rpart.control())
predict(rp, Xtest)


# Runs --------------------------------------------------------------------

rparts <- createArgsDf(type = c("class", "anova"),
                       split = c("gini", "information"),
                       minsplit = (2:6) * 10,
                       i = 1:nF, maxRuns = 20000) %>% tbl_df %>% 
  mutate(model = mapply(function(type, split, minsplit, i) { 
    res = rpart(Y ~ ., 
                data = data.frame(X[foldIdx != i,], Y = as.numeric(Y[foldIdx != i])),
                method = type,
                parms = list(split = split),
                control = rpart.control(minsplit = minsplit))
  },
  type, split, minsplit, i, SIMPLIFY = FALSE)) 

rpartsRes <- rparts %>% rowwise() %>% 
  do(do.call(data_frame, 
             c(.[-5], cpmax = list(.$model$cptable[-1,"CP"]),
               model = list(lapply(.$model$cptable[-1,"CP"], prune, tree = .$model))))) %>% 
  mutate(res = if(substitute(type) == "class") {
    list(predict(model, X[foldIdx == i,])[,2])
  } else {
      list(predict(model, X[foldIdx == i,]))
    })

rpartsPerf <- rpartsRes %>%
  mutate(perf = AUROC(res, Y[foldIdx == i]))

ggEx <- ggplot(rpartsPerf %>% filter(cpmax == 0.01)) + 
  stat_summary_2d(aes(x=as.factor(minsplit), 
                      y=interaction(type, split), 
                      z = perf))

ggEx <- ggplot(rpartsPerf %>% filter(type == "class", split == "gini")) + 
  stat_summary_2d(aes(x= as.factor(minsplit), 
                      y= cpmax, 
                      z = perf))
ggEx
ggsave("Output/RpartEx.pdf", ggEx, h = 6, w = 10)





