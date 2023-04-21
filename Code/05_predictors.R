
# Define Predictors: Random Forests -------------------------------------------------------

trainPredictRF <- function(Xtrain, Ytrain, Xtest) {
  suppressPackageStartupMessages(library(randomForest))
  knownWarning <- "The response has five or fewer unique values.  Are you sure you want to do regression?"
  withCallingHandlers(
    rf1 <- randomForest(Xtrain, Ytrain, Xtest),
    warning = function(w) if (conditionMessage(w) == knownWarning) invokeRestart("muffleWarning")
  )
  return(rf1$test$predicted)
}
RFPred <- list(trainPredict = trainPredictRF)

trainPredictRF2 <- function(Xtrain, Ytrain, Xtest) {
  suppressPackageStartupMessages(library(randomForest))
  knownWarning <- "The response has five or fewer unique values.  Are you sure you want to do regression?"
  withCallingHandlers(
    rf2 <- randomForest(Xtrain, Ytrain, Xtest, mtry = floor(sqrt(ncol(Xtest)))),
    warning = function(w) if (conditionMessage(w) == knownWarning) invokeRestart("muffleWarning")
  )
  return(rf2$test$predicted)
}
RFPred2 <- list(trainPredict = trainPredictRF2)

trainPredictRF3 <- function(Xtrain, Ytrain, Xtest) {
  # Note: I believe that it's basically the same thing as RF2 (main diff when using classif is default's mtry)
  suppressPackageStartupMessages(library(randomForest))
  rf3 <- randomForest(Xtrain, factor(Ytrain), Xtest)
  return(unname(rf3$test$votes[,2]))
}
RFPred3 <- list(trainPredict = trainPredictRF3)

# Optimal
trainPredictRFO <- function(Xtrain, Ytrain, Xtest) {
  suppressPackageStartupMessages(library(e1071))
  suppressPackageStartupMessages(library(randomForest))
  nX <- ncol(Xtrain)
  mtrys = c(2, 3, 4, 5, 7, 9)
  mtrys <- mtrys[mtrys <= nX * 0.5 | mtrys <= 3 & mtrys <= nX ]
  predict.fun <- function(model, data, ...) {
    predict(model, data, type = "prob", ...)[,2]
  }
  rfo.mod <- tune.randomForest(x = Xtrain, y = as.factor(Ytrain), 
                               mtry = mtrys,
                               predict.func = predict.fun,
                               tunecontrol = tune.control(cross = 5,
                                                          error.fun = function(Yt, Yp) -AUROC(Yp, Yt)))
  cats(" -> best mtry: %2d [perf: %.3f]",
       rfo.mod$best.parameters$mtry, -rfo.mod$best.performance, timing = FALSE)
  predict.fun(rfo.mod$best.model, Xtest)
}

RFOPred <- list(trainPredict = trainPredictRFO)


# Define Predictors: GLM -------------------------------------------------------

trainPredictGLM <- function(Xtrain, Ytrain, Xtest) {
  suppressPackageStartupMessages(library(glmnet))
  cv.fit <- cv.glmnet(data.matrix(Xtrain), Ytrain, alpha = 0.999, 
                      type.measure = "auc", family = "binomial")
  lmi <- which(cv.fit$lambda == cv.fit$lambda.min)
  l1i <- which(cv.fit$lambda == cv.fit$lambda.1se)
  lbi <- floor((lmi + l1i)/2)  # best!?
  cats(" -> best lambda: %.4f / nz coeffs: %2d [perf: %.3f]",
       cv.fit$lambda[lbi], cv.fit$nzero[lbi], cv.fit$cvm[lbi], timing = FALSE)
  c(predict(cv.fit, data.matrix(Xtest), s = cv.fit$lambda[lbi]))
}
GLMPred <- list(trainPredict = trainPredictGLM)


# Define Predictors: SVM -------------------------------------------------------

trainPredictSVM <- function(Xtrain, Ytrain, Xtest) {
  suppressPackageStartupMessages(library(e1071))
  predict.fun <- function(model, data) {
    res <- predict(model, data, probability = TRUE)
    attr(res, "probabilities")[,'TRUE']
  }
  svm.mod <- tune(svm, 
                  train.x = data.matrix(Xtrain), train.y = as.factor(Ytrain), 
                  ranges = list(gamma = 2^seq(-10,0), 
                                cost = 10^(seq(-1, 1.5, 0.5)),
                                probability = TRUE),
                  predict.func = predict.fun,
                  tunecontrol = tune.control(cross = 5,
                                             error.fun = function(Yt, Yp) -AUROC(Yp, Yt)))
  cats(" -> best gamma: %9.3g / best cost: %6.3g [perf: %.3f]",
       svm.mod$best.parameters$gamma, svm.mod$best.parameters$cost,
       -svm.mod$best.performance, timing = FALSE)
  predict.fun(svm.mod$best.model,  data.matrix(Xtest))
}

SVMPred <- list(trainPredict = trainPredictSVM)

trainPredictSofiaSVM <- function(Xtrain, Ytrain, Xtest) {
  suppressPackageStartupMessages(library(RSofia))
  suppressPackageStartupMessages(library(e1071))
  predict.fun <- function(model, newdata, ...) {
    predict(model, newdata, prediction_type = "logistic", ...)
  }
  sofsvm.mod <- tune(  train.x = Y ~ ., 
                       data = data.frame(Xtrain, Y = Ytrain + 0), 
                       method = function(learner_type, loop_type, ...) {
                         RSofia::sofia(learner_type = as.character(learner_type),
                                       loop_type = as.character(loop_type), 
                                       ...)
                       }, 
                       ranges = list(learner_type = c("pegasos", "sgd-svm", 
                                                      "logreg-pegasos"),
                                     loop_type = c("roc", "combined-roc"),
                                     lambda = 10^seq(-3, 2, by = 0.25)),
                       predict.func = predict.fun,
                       tunecontrol = tune.control(cross = 5,
                                                  error.fun = function(Yt, Yp) -AUROC(Yp, Yt)))
  cats(" -> best learner.loop type: %-15s / best cost: %-6.3g [perf: %.3f]",
       paste(sofsvm.mod$best.parameters$learner_type, 
             sofsvm.mod$best.parameters$loop_type, sep = "-"),
       sofsvm.mod$best.parameters$lambda, -sofsvm.mod$best.performance, timing = FALSE)
  suppressWarnings(predict.fun(sofsvm.mod$best.model, data.frame(Xtest, Y = 1)))
}

SofSVMPred <- list(trainPredict = trainPredictSofiaSVM)


# Define Predictors: Rpart based RF -------------------------------------------------------

trainPredictRP <- function(Xtrain, Ytrain, Xtest) {
  suppressPackageStartupMessages(library(e1071))
  rpartrf.mod <- tune.rpart(formula = Y ~ ., 
                            data = data.frame(Y=as.numeric(Ytrain), Xtrain), 
                            minsplit = (2:11)*10,
                            cp = c(0.0001, 0.0003, 0.001, 0.002, 0.005, 0.01, 0.02, 0.05),
                            tunecontrol = tune.control(cross = 5,
                                                       error.fun = function(Yt, Yp) -AUROC(Yp, Yt)))
  cats(" -> best min split: %-2d / best cp: %-6.3g [perf: %.3f]",
       rpartrf.mod$best.parameters$minsplit, rpartrf.mod$best.parameters$cp,
       -rpartrf.mod$best.performance, timing = FALSE)
  predict(rpartrf.mod$best.model, Xtest)
}

RPPred <- list(trainPredict = trainPredictRP)


# Define Predictors: KNNs -------------------------------------------------

# Get optimal, patched version:
# devtools::install_github("antoine-lizee/kknn")

## FROM: https://github.com/KlausVigo/kknn
trainPredictKNN <- function(Xtrain, Ytrain, Xtest) {
  suppressPackageStartupMessages(library(kknn))
  ks <- c(3:19, seq(20, 200, 10))
  kknn.mod <- train.kknn(formula = Y ~ ., 
                         data = data.frame(Y=as.numeric(Ytrain), Xtrain),
                         ks = ks, kernel = c("biweight", "optimal"))
  AUCs <- sapply(lapply(kknn.mod$fitted.values, as.numeric), AUROC, labels = Ytrain)
  kknn.mod$best.parameters <- attributes(kknn.mod$fitted.values[[which.max(AUCs)]])[c("kernel", "k")]
  kknn.mod$best.performance <- AUCs[which.max(AUCs)]
  cats(" ->  best k: %-3d / kernel: %-10s [perf: %.3f]",
       kknn.mod$best.parameters$k, kknn.mod$best.parameters$kernel,
       kknn.mod$best.performance, timing = FALSE)
  predict(kknn.mod, Xtest)
}
KNNPred <- list(trainPredict = trainPredictKNN)

trainPredictKNNC <- function(Xtrain, Ytrain, Xtest) {
  ## Seems significantly inferior than the above, probably because of the loss function ("misclassification")
  suppressPackageStartupMessages(library(kknn))
  ks <- c(3:19, seq(20, 200, 10))
  kknn.modC <- train.kknn(formula = Y ~ ., 
                          data = data.frame(Y=factor(as.numeric(Ytrain)), Xtrain),
                          ks = ks, kernel = c("biweight", "optimal"))
  AUCs <- sapply(lapply(kknn.modC$fitted.values, as.numeric), AUROC, labels = Ytrain)
  kknn.modC$best.parameters <- attributes(kknn.modC$fitted.values[[which.max(AUCs)]])[c("kernel", "k")]
  kknn.modC$best.performance <- AUCs[which.max(AUCs)]
  cats(" ->  best k: %-3d / kernel: %-10s [perf: %.3f]",
       kknn.modC$best.parameters$k, kknn.modC$best.parameters$kernel,
       kknn.modC$best.performance, timing = FALSE)
  predict(kknn.modC, Xtest, type = "prob")[,2]
}
KNNCPred <- list(trainPredict = trainPredictKNNC)


# Bayesian ----------------------------------------------------------------

trainPredictNB <- function(Xtrain, Ytrain, Xtest) {
  suppressPackageStartupMessages(library(e1071))
  # cast booleans to factors
  Xtrain[] <- bool2facs(Xtrain)
  Xtest[] <- bool2facs(Xtest)
  # quick CV
  ls <- 0:15; N <- nrow(Xtrain)
  foldIdxs <- sample(rep(1:(nF <- 5), length.out = N))
  perfs <- sapply(ls, function(l) {
    Ypred <- double(N)
    for (fold in 1:nF) {
      Ypred[foldIdxs == fold] <- predict(naiveBayes(formula = Y ~ ., 
                                                    data = data.frame(Y=Ytrain[foldIdxs != fold], Xtrain[foldIdxs != fold,]),
                                                    laplace = l),
                                         Xtrain[foldIdxs == fold,],
                                         type = "raw")[,2]
    }
    ROCR::performance(ROCR::prediction(Ypred, Ytrain), "auc")@y.values[[1]]
  })
  # Evaluate the best
  bestL <- ls[which.max(perfs)]
  cats(" -> best l: %2d [perf: %.3f]",
       bestL, max(perfs), timing = FALSE)
  nb.mod <- naiveBayes(formula = Y ~ ., 
                       data = data.frame(Y=Ytrain, Xtrain),
                       laplace = bestL)
  predict(nb.mod, Xtest, type = "raw")[,2]
}
NBPred <- list(trainPredict = trainPredictNB)


# Boosted models ----------------------------------------------------------

trainPredictXGBoost <- function(Xtrain, Ytrain, Xtest) {
  
  trControl <- caret::trainControl(
    "repeatedcv", number = 2, repeats = 3, verboseIter = FALSE
    , summaryFunction = caret::twoClassSummary
    , classProbs = TRUE
    , allowParallel = FALSE
  )
  tuneGrid <- expand.grid(nrounds = c(10, 20, 30, 40, 50, 70, 100, 150, 200, 250),  # only one run is done for all values of nrounds
                          max_depth = 1:3,
                          subsample = c(0.7, 0.85),
                          eta = c(0.05, 0.1, 0.2),
                          colsample_bytree = 0.8,
                          min_child_weight = 1,
                          gamma = 0
  )
  Ytrain_factors <- factor(Ytrain, labels = c("stable", "increase"))  # required by caret
  
  out <- capture.output(
    model.xgb <- caret::train(data.matrix(Xtrain) + 0  # add + 0 in case the data.matrix is int (because from factors only)
                              , Ytrain_factors
                              , method = "xgbTree"
                              , trControl = trControl
                              , metric = "ROC"
                              , tuneGrid = tuneGrid
                              , verbose = FALSE
                              , nthread = 1
    )
  )
  stupid_warning_msg <- "WARNING: amalgamation/../src/c_api/c_api.cc:785: `ntree_limit` is deprecated, use `iteration_range` instead."
  remaining_output <- out[!grepl(stupid_warning_msg, out)]
  if (length(remaining_output) > 0) cat(remaining_output, sep = "\n")
  bt <- (model.xgb$results %>% arrange(desc(ROC)))[1, ]
  cats(" -> best nrounds: %d  / eta %.2f / max_depth %d [perf: %.3f]",
       bt$nrounds, bt$eta, bt$max_depth, bt$ROC, timing = FALSE)
  predict(model.xgb, data.matrix(Xtest) + 0, type = "prob")[, 2]
}
XGBPred <- list(trainPredict = trainPredictXGBoost)
