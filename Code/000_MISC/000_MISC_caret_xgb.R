
Xtrain <- data.matrix(predictTable %>% select(-Predicted.class))
Ytrain <- factor(predictTable$Predicted.class, labels = c("stable", "increase"))

trControl <- caret::trainControl(
  "repeatedcv", number = 2, repeats = 4, verboseIter = FALSE
  , summaryFunction = caret::twoClassSummary
  , classProbs = TRUE
  # , search = "random"
)

tuneGrid_1 <- expand.grid(nrounds = c(5, 10, 20, 30, 40, 50, 70, 100, 150, 200),  # computing a more frugal grid in our case
                          max_depth = 1:3,
                          subsample = 0.85,
                          eta = c(0.1, 0.3),
                          min_child_weight = 1,
                          colsample_bytree = c(0.4, 0.7),
                          gamma = 0)

## Computing a full grid to understand
tuneGrid_2 <- expand.grid(nrounds = c(5, 10, 20, 30, 40, 50, 70, 100, 150, 200),
                          max_depth = 1:3,
                          subsample = c(0.85, 1), 
                          eta = c(0.05, 0.1, 0.3),
                          colsample_bytree = c(0.7, 0.9),
                          min_child_weight = c(0.1, 1, 2, 5),
                          gamma = c(0, 0.1, 1, 5)
)

## First optim:
tuneGrid_2.1 <- expand.grid(nrounds = c(5, 10, 20, 30, 40, 50, 70, 100, 150, 200, 250, 300, 350, 400),
                            max_depth = 1:3,
                            subsample = c(0.75, 0.85, 0.95), 
                            eta = c(0.05, 0.1),
                            colsample_bytree = c(0.8),
                            min_child_weight = c(1, 10),
                            gamma = c(0)
)

model.xgb <- caret::train(Xtrain, Ytrain, method = "xgbTree"
                          , trControl = trControl
                          , verbose = TRUE
                          # , tuneLength = 100  # 5 for grid, 100 for random if using the default grid
                          , metric = "ROC"
                          , tuneGrid = tuneGrid_2.1
)
plot(model.xgb)

res <- model.xgb$results
res %>%
  filter(colsample_bytree == 0.8
         # , eta == 0.1
         , subsample == 0.85
         # , gamma == 0
         # , min_child_weight == 1
  ) %>%
  ggplot(data = ., aes(x = nrounds, y = ROC
                       , color = factor(min_child_weight), shape = factor(subsample)
                       , group = interaction(min_child_weight, subsample))) +
  geom_line() + geom_point() +
  facet_grid(max_depth ~ eta)

res %>% 
  arrange(desc(ROC)) %>%
  distinct(eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample, .keep_all = TRUE) %>%  # Keep the best nrounds only
  head(20)

#From the first tune grid
# ROC(nrounds) is intersting, it looks like most of the peaks happen before 50 (the min computed) Only exception is for depth = 1 (which seems)
# ROC(colsample_bytree) behavior is consistent across nrounds and eta (0.8~>0.6). It's also not really significant.
# ROC(eta) looks like lower (0.3 vs 0.4) is better, and that lower needs higher nrounds
# ROC(subsample) looks higher (1 ~ 0.) is better, and that lower needs higher nrounds

#From the second, exhaustive tune grid
# min_child_weight has no impact, even at gamma = 0. We might want to try much larger values (tested 0.1 - 5)
# subsample: ROC(0.85) > ROC(1) in regions of interest (before over fitting bc of nrounds). Esp clear for low eta (0.05)
# gamma: 5 has a pretty big effect in some cases to reduce overfitting. (esp for eta=0.3 and depth = 3) Apart from that, nothing clear.
# eta: matters a lot, in relationship with nrounds. for depth>1, eta=0.05 is important. low eta also reduces variability

#From the 3rd, 2.1
# subsample: doesn't seem super important anymore, with 0.95 being probably a bit worse. Using c(0.7, 0.85) for all seem like a good option
# min_child_weight: very large value (10) seem to hinder performance somewhat, but not really for top performers

# Note:
# The interactions between the trControl and the train functions are REALLY annoying.
# In particular, those three things are related:
# - trainControl( summaryFunction = caret::twoClassSummary, ...)  : else it's estimated like a regression
# - trainControl( classProbs = TRUE, ...)  : else the above bugs and tells you it can't work
# - train( metrc = "ROC", ...) : else it takes the first computed by the summaryFunction (which happens to be ROC)

# Note: xgboost vs gbm
# caret also proposes to use the boosted model proposed in the library gbm.
# after quick research, we decided to leave that out since it would probably give similar or lower performance.

# Note: doc
# This link is useful to understand the default tuning from caret:
# https://github.com/topepo/caret/blob/master/models/files/xgbTree.R