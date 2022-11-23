
edss_formula <- as.formula(Core.EDSS ~ Clinical.ATM + Clinical.OpticNeuritis + Clinical.INO +
  Clinical.MotorWeakness + Clinical.SensoryDisturbance + Clinical.Ataxia +
  Clinical.BladderDisturbance + Clinical.BowelDisturbance + Clinical.Myelopathy +
  Clinical.CogDisturbance + Clinical.VisualLoss)
weights_1 <- 1 / table(predictTable$Core.EDSS)[as.character(predictTable$Core.EDSS)]
weights_2 <- 1 + 0.2 * (3 - predictTable$Core.EDSS) ^ 2

edss_model_glm <- predictTable %>% glm(edss_formula, data=.)
edss_model_glm_w1 <- predictTable %>% glm(edss_formula, data=., weights = weights_1)
edss_model_glm_w2 <- predictTable %>% glm(edss_formula, data=., weights = weights_2)

edss_model_nnet <- predictTable %>% nnet::nnet(
  edss_formula, data=.
  , size = 10, decay = 1e-5, linout = TRUE, skip = TRUE, maxit = 1000
  )

edss_model_nnet_w1 <- predictTable %>% nnet::nnet(
  edss_formula, data=.
  , size = 10, decay = 1e-5, linout = TRUE, skip = TRUE, maxit = 1000, weights = weights_1
)

edss_model_nnet_w2 <- predictTable %>% nnet::nnet(
  edss_formula, data=.
  , size = 10, decay = 1e-5, linout = TRUE, skip = TRUE, maxit = 1000, weights = weights_2
)

ggplot(data = predictTable %>% mutate(model_predictions = edss_model_nnet_w2$fitted.values)
       # , aes(x = Core.EDSS, y = model_predictions)) +
       , aes(y = Core.EDSS, x = model_predictions)) +
  geom_smooth(method = 'lm') +
  # geom_boxplot(aes(group = Core.EDSS)) +
  geom_jitter(alpha = 0.1) +
  coord_equal(xlim=c(-0.5, 6.5), ylim=c(-0.5, 6.5))
