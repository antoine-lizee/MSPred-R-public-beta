# 07.0
# Exploratory Sub-script for Analysis which let us
# compute and see marginal probabilities and 
# related bayesian outcome metrics.
#
# Copyright Antoine Lizee @ UCSF 2016


# Extract all runs for single couple ------------------------------------------------------

methodi <-  "KNNC"
catGroupi <-  "."

predLabDf <- yExtr$ypredDf %>% filter(method == methodi, catGroup == catGroupi) %>% 
  rowwise() %>% 
  do(data.frame(preds = c(.$Yp),
                labels = rep(yExtr$y, ncol(yExtr$FIs)),
                cv = rep(1:ncol(yExtr$FIs), each = length(yExtr$y)))) 


# Show pdfs of outome -----------------------------------------------------

predLabDf %>% 
  ggplot() +
  geom_density(aes(x = preds, fill = labels, group = interaction(cv, labels)), adjust = 0.5, color = NA, alpha = 0.4) + 
  geom_density(aes(x = preds, color = labels, group = interaction(cv, labels)), adjust = 0.2, size = 0.6) + 
  facet_grid(cv ~ .) +
  theme_bw()


# Explicitely compute them and the prob ratio ----------------------------------------------

adj = 0.5
# Rank-normalized prediction version
ggProbNorm <- predLabDf %>% group_by(cv) %>% 
  do(data.frame(
    sapply(density(rank(.$preds)/length(.$preds), 
                   weights = .$labels / sum(.$labels), 
                   adjust = adj)[c("x", "y")], 
           'identity'),
    y2 = density(rank(.$preds)/length(.$preds), 
                 weights = (!.$labels) / sum(!.$labels), 
                 adjust = adj)$y)) %>% 
  ggplot() +
  geom_line(aes(x = x, y = y, group = cv), color = "red") +
  geom_line(aes(x = x, y = y2, group = cv), color = "blue") +
  geom_line(aes(x = x, y = pmin(y2/y, 4), group = cv), color = "black") +
  facet_grid(cv ~ .) +
  theme_bw()
ggsave(ggProbNorm, 
       filename = file.path(yExtr$RD, sprintf("07.0_predProbsNorm%s%s.pdf", catGroupi, methodi)),
       w = 6, h = 10)

# Direct predictor version
ggProb <- predLabDf %>% group_by(cv) %>% 
  do(data.frame(
    sapply(density(.$preds, 
                   weights = .$labels / sum(.$labels), 
                   adjust = adj)[c("x", "y")], 
           'identity'),
    y2 = density(.$preds, 
                 weights = (!.$labels) / sum(!.$labels), 
                 adjust = adj)$y)) %>% 
  ggplot() +
  geom_line(aes(x = x, y = y, group = cv), color = "red") +
  geom_line(aes(x = x, y = y2, group = cv), color = "blue") +
  geom_line(aes(x = x, y = pmin(y2/y, 4), group = cv), color = "black") +
  facet_grid(cv ~ .) +
  theme_bw()
ggsave(ggProb,
       filename = file.path(yExtr$RD, sprintf("07.0_predProbs%s%s.pdf", catGroupi, methodi)),
       w = 6, h = 10)
  



