
library(ROCR)

# Performance estimation functions ----------------------------------------

calcAucpr <- function(pr) {
  caTools::trapz(pr@x.values[[1]], c(pr@y.values[[1]][2], pr@y.values[[1]][-1]))
}

calcAucpr5 <- function(pr) {
  x <- pr@x.values[[1]]
  y <- c(pr@y.values[[1]][2], pr@y.values[[1]][-1])
  idx <- x < 0.5
  caTools::trapz(x[idx], y[idx])
}

calcAurch <- function(rch) {
  caTools::trapz(rch@x.values[[1]], rch@y.values[[1]])
}

calcAuroc <- function(pred) {
  ROCR::performance(pred, "auc")@y.values[[1]]
}

AUROC <- function(preds, labels) {
  calcAuroc(ROCR::prediction(preds, labels))
}

AUCPR5 <- function(preds, labels) {
  calcAucpr5(ROCR::performance(ROCR::prediction(preds, labels), "prec", "rec"))
}


# 2by2 table extraction ---------------------------------------------------

calc2by2 <- function(pred, thre = "macc", costRatio = NULL) {
  # Getting the point of estimation:
  if (!is.null(costRatio)) {
    idxThre = mapply(function(fns, fps) which.min(fns + fps * costRatio), 
                     pred@fn, pred@fp)
  } else {
    if (is.character(thre)) {
      idxThre <- switch(thre,
                        "macc" = sapply(ROCR::performance(pred, "acc")@y.values,
                                        which.max),
                        "mf" = sapply(ROCR::performance(pred, "f")@y.values,
                                      which.max),
                        "mss" = sapply(mapply('+', #gives very similar results from maxf 
                                              ROCR::performance(pred, "sens")@y.values, 
                                              ROCR::performance(pred, "spec")@y.values, SIMPLIFY = FALSE),
                                       which.max),
                        stop("ERROR: CVER: ARG: thresold identifier not recognized"))
    } else {
      if (!is.numeric(thre)) stop("ERROR: CVER: ARG: thresold must be a string or a numeric")
      ranges = sapply(pred@predictions, range)
      if (any(ranges[1,] > thre || ranges[2,] < thre)) warning("WARN: CVER: thresold is not in range, will lead to extreme value.")
      idxThre <- sapply(pred@cutoffs, function(cuts) which.min(abs(cuts - thre))) 
    }
  }
  res <- sapply(lapply(c("fp" = "fp", "tp" = "tp", "tn" = "tn", "fn" = "fn", "cutoff" = "cutoffs"), slot, object = pred), 
                function(ns) mapply('[', ns, idxThre))
  if (is.null(dim(res))) res <- t(res) # Ensure consistency with only one set of predictions
  return(as.data.frame(res) %>% 
           mutate(idx = idxThre,
                  sens = tp / (tp + fn),
                  spe = tn / (tn + fp),
                  ppv = tp / (tp + fp),
                  npv = tn / (tn + fn),
                  rec = sens,
                  tpr = sens,
                  fpr = 1 - spe,
                  acc = (tp + tn) / (tp + tn + fp + fn),
                  costRatio = {if (is.null(costRatio)) 1 else costRatio},
                  cost = (fn + fp * costRatio) / (1 + costRatio)
           ))
}

format2by2 <- function(df1, sep = " ") {
  stopifnot(nrow(df1) == 1)
  tot = sum(df1[c("fp", "fn", "tp", "tn")])
  matrix(c(sprintf("Total:%s%d", sep, tot), sprintf("Real Positives:%s%d", sep, df1$tp + df1$fn), sprintf("Real Negatives:%s%d", sep, df1$tn + df1$fp), "",
           sprintf("Predicted Positives:%s%d", sep, df1$tp + df1$fp), df1$tp, df1$fp, sprintf("Precision, PPV:%s%.2f", sep, df1$ppv),
           sprintf("Predicted Negatives:%s%d", sep, df1$tn + df1$fn), df1$fn, df1$tn, sprintf("NPV:%s%.2f", sep, df1$npv),
           "", sprintf("Sensitivity, Recall:%s%.2f", sep, df1$sens), sprintf("Specificity, (1-FPR):%s%.2f", sep, df1$spe), ""),
         byrow = T, nrow = 4 )
}

format2by2F <- function(df1, sep = " ") {
  stopifnot(nrow(df1) == 1)
  tot = sum(df1[c("fp", "fn", "tp", "tn")])
  matrix(c("", "Total", "Real Positives:", "Real Negatives", "",
           "", tot, df1$tp + df1$fn, df1$tn + df1$fp, "",
           "Predicted Positives", df1$tp + df1$fp, df1$tp, df1$fp, sprintf("Precision, PPV:%s%.2f", sep, df1$ppv),
           "Predicted Negatives", df1$tn + df1$fn, df1$fn, df1$tn, sprintf("NPV:%s%.2f", sep, df1$npv),
           c("", "", sprintf("Sensitivity, Recall:%s%.2f", sep, df1$sens), 
             sprintf("Specificity, (1-FPR):%s%.2f", sep, df1$spe), 
             sprintf("Accuracy:%s%.2f", sep, df1$acc))),
         byrow = T, nrow = 5 )
}

format2by2S <- function(df, sep = " ") {
  if(nrow(df) == 1) {
    df1 <- df
    tot = sum(df1[c("fp", "fn", "tp", "tn")])
    matrix(c(tot, df1$tp + df1$fn, df1$tn + df1$fp, "",
             df1$tp + df1$fp, df1$tp, df1$fp, sprintf("%.2f", df1$ppv),
             df1$tn + df1$fn, df1$fn, df1$tn, sprintf("%.2f", df1$npv),
             c("", sprintf("%.2f", df1$sens), 
               sprintf("%.2f", df1$spe), 
               sprintf("%.2f", df1$acc))),
           byrow = T, nrow = 4 )
  } else {
    spfd <- function(num, range) sprintf("%.1f%s\u00B1%.1f", num, sep, range)
    spff <- function(num, range) sprintf("%.3f%s\u00B1%.3f", num, sep, range)
    dfn <- df %>% mutate(gp = tp + fp, gn = tn + fn) %>% 
      select(tn, tp, fn, fp, gp, gn, sens, spe, acc, npv, ppv) %>% 
      summarize_each(funs(median, mean, sd, 
                          se = sd(.)/sqrt(length(.)),
                          IQR, q1 = quantile(., 0.25), q2 = quantile(., 0.75)))
    with(df[1,], {rp <<- tp + fn; rn <<- tn + fp})
    matrix(c(rp + rn, rp, rn, "",
             spfd(dfn$gp_mean, dfn$gp_se), spfd(dfn$tp_mean, dfn$tp_se), spfd(dfn$fp_mean, dfn$fp_se), spff(dfn$ppv_mean, dfn$ppv_se),
             spfd(dfn$gn_mean, dfn$gn_se), spfd(dfn$fn_mean, dfn$fn_se), spfd(dfn$tn_mean, dfn$tn_se), spff(dfn$npv_mean, dfn$npv_se),
             c("", spff(dfn$sens_mean, dfn$sens_se), spff(dfn$spe_mean, dfn$spe_se), spff(dfn$acc_mean, dfn$acc_se))),
           byrow = T, nrow = 4)
  }
}

show2by2 <- function(mat2by2) {
  library(gridExtra)
  cs <- list(ww = "white", rp = "lightskyblue", rn = "deepskyblue",
             gp = "orange1", gn = "orange3",
             tp = "springgreen1", tn = "springgreen3", 
             fn = "snow4", fp = "snow3")
  fillS <- c(cs$ww, cs$rp, cs$rn, cs$ww,
             cs$gp, cs$tp, cs$fn, cs$ww,
             cs$gn, cs$fp, cs$tn, cs$ww,
             rep(cs$ww, 4))
  tG <- tableGrob(mat2by2,
                  theme = ttheme_minimal(
                    core = list(fg_params = list(fontface = 2))))
  tG$grobs[17:32] <- mapply(function(rectGrob, col) {rectGrob[['gp']] <- gpar(fill = col); rectGrob}, 
                            tG$grobs[17:32],
                            fillS, SIMPLIFY = FALSE)
  grid.newpage()
  grid.draw(tG)
}

