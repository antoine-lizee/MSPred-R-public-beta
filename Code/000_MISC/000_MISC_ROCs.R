


# Create simulation data --------------------------------------------------

rp = 0.2
n = 1000
np = rp * n
nf = (1-rp) * n
labels <- rep(c(TRUE, FALSE), c(np, nf))
preds3 <- c(rnorm(np, mean = 1, sd = 1.3), rnorm(nf, mean = 0))
preds2 <- c(rnorm(np, mean = 1, sd = 1.3), 
            rnorm(nf/2, mean = -0.3), 
            rnorm(nf/2, mean = 0.3))
preds <- c(rnorm(np/2, mean = 0.3), 
           rnorm(np/2, mean = 1), 
           rnorm(nf, mean = 0))

# preds <- cbind(preds, preds2, preds3); labels <- replicate(3, labels);

qplot(preds, color = labels, geom = "density")

library(ROCR)
totPred <- prediction(preds, labels)
totROC <- performance(totPred, "tpr", "fpr")
totRCH <- performance(totPred, "rch")
totPR <- performance(totPred, "prec", "rec")
totAUROC <- performance(totPred, "auc")@y.values[[1]]
ys <- rev(unname(split(preds, labels)))
totAUROC2 <- do.call(wilcox.test, ys)$statistic / prod(sapply(ys, length))
totAURCH <- caTools::trapz(totRCH@x.values[[1]], totRCH@y.values[[1]])


# Plots -------------------------------------------------------------------

plotROC <- function(perf) {
  qplot(x = perf@x.values[[1]], y= perf@y.values[[1]], geom = "line") +
    theme_bw() +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), linetype = "33") + 
    labs(x = "false positive rate (1 - spec)", y = "true positive rate (sens)") +
    coord_equal()
}

plotPR <- function(perf) {
  p1 <- rev(perf@y.values[[1]])[1]
  qplot(x = perf@x.values[[1]], y= perf@y.values[[1]], geom = "line") +
    theme_bw() +
    geom_segment(aes(x = 0, xend = 1, y = p1, yend = p1), linetype = "33") +
    labs(x = "recall (sens)", y = "precision (PPV)") +
    ylim(c(0, 1))
}

multiplot(plotROC(totROC),
          plotPR(totPR) + coord_flip(), cols = 2)

## Look at the convexhull version of the ROC:
ggplot(data = data.frame(
  x = unlist(lapply(list(totRCH, totROC), 
                    function(perf) perf@x.values[[1]])),
  y = unlist(lapply(list(totRCH, totROC), 
                    function(perf) perf@y.values[[1]])),
  type = rep(c("ROCC", "ROC"),
             sapply(list(totRCH, totROC), 
                    function(perf) length(perf@y.values[[1]])))),
       aes(x = x, y = y, col = type)) +
  geom_line() +
  theme_bw() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
               linetype = "33", col = "black") +
  labs(x = "recall (sens)", y = "precision (PPV)") +
  ylim(c(0, 1))


# Compare ROC computations ------------------------------------------------

AUC1 <- function(preds, labels) {
  ys <- rev(unname(split(preds, labels)))
  do.call(wilcox.test, ys)$statistic / prod(sapply(ys, length))
}

AUC2 <- function(preds, labels) {
  performance(prediction(preds, labels), "auc")@y.values[[1]]
}

AUC3 <- function(preds, labels) {
  rch <- ROCR::performance(prediction(preds, labels), "rch")
  caTools::trapz(rch@x.values[[1]], rch@y.values[[1]])
}
# 
# microbenchmark::microbenchmark(AUC1(preds, labels),
#                                AUC2(preds, labels),
#                                AUC3(preds, labels),
#                                times = 100)


# Run simulations ---------------------------------------------------------

ns <- 100
sAUCs <- replicate({
  sIdx <- sample(1:length(preds), ns)
  AUC2(preds[sIdx], labels[sIdx])
}, n = 100)

qplot(sROCs, geom = "density") + 
  geom_vline(xintercept = totAUROC) +
  geom_vline(xintercept = mean(sROCs), color = "red")

ns <- 100
sAUCHRs <- replicate({
  sIdx <- sample(1:length(preds), ns)
  AUC3(preds[sIdx], labels[sIdx])
}, n = 100)

qplot(sAUCHRs, geom = "density") + 
  geom_vline(xintercept = AUC3(preds, labels)) +
  geom_vline(xintercept = mean(sAUCHRs), color = "red")


# Show impact of class imbalance ------------------------------------------


aucClassImbalance <- function(positiveRatio = 1) {
  n <- 500
  np = floor(positiveRatio / (1 + positiveRatio) * n)
  nf = n - np
  preds <- c(rnorm((np + 1)/2, mean = 0.3), 
             rnorm(np/2, mean = 1), 
             rnorm(nf, mean = 0))
  labels <- rep(c(TRUE, FALSE), c(np, nf))
  AUC2(preds, labels)
}

nIter <- 1000
aucsCI <- do.call(rbind,
                  lapply(c(0.1, 1/3, 1, 3, 10),
                         function(pR) {
                           data.frame(ratio = pR, auc = replicate(aucClassImbalance(pR), n = nIter))
                         }))

ggplot(aucsCI, aes(x = auc, fill = log10(ratio), color = log10(ratio), group = ratio)) + 
  geom_vline(data = aucsCI %>% group_by(ratio) %>% summarise(auc = mean(auc)) %>% ungroup, 
             aes(xintercept = auc, color = log10(ratio), group = ratio)) +
  geom_density(alpha = 0.2, adjust = 0.5, size = 2) +
  scale_fill_distiller(type = "div", palette = 7) +
  scale_color_distiller(type = "div", palette = 7) +
  theme_bw()


# Some other stuff re cost / metrics, etc... ------------------------------

# Compute cost accross threshold for different cost ratios.
df0 <- data.frame(x = pred@n.pos.pred[[1]], fn = pred@fn[[1]], fp = pred@fp[[1]])
dfCost <- do.call("rbind",
                  lapply(c(0.1, 0.2, 0.5, 1, 2), 
                         function(cost) data.frame(x = df0$x, y = (df0$fn + cost * df0$fp) / (1 + cost), c = cost)))

ggplot(dfCost) + geom_line(aes(x = x, y = y, color = factor(c)))
# -> For many cost ratios, the minimum is at the extremes. (that's what you get with class imbalance)

# Show that with sensitivity / specificity overlaid.
dfTot <- rbind(dfCost %>% mutate(c = as.character(c)),
               data.frame(x = pred@n.pos.pred[[1]], 
                          y = mapply(function(tp, fn) tp / (tp + fn), pred@tp[[1]], pred@fn[[1]]) * 500,
                          c = "sens"),
               data.frame(x = pred@n.pos.pred[[1]], 
                          y = mapply(function(tn, fp) fp / (tn + fp), pred@tn[[1]], pred@fp[[1]]) * 500,
                          c = "1 - spec"))

ggplot(dfTot) + geom_line(aes(x = x, y = y, color = c))

