
directoryhelp::dirSetF('bayesMeanScale')
setwd("Testing Models")

betaModel     <- readRDS('beta-model.RDS')
cloglogModel  <- readRDS('cloglog-model.RDS')
logitModel    <- readRDS('logit-model.RDS')
probitModel   <- readRDS('probit-model.RDS')
gammaModel    <- readRDS('gamma-model.RDS')
gaussianModel <- readRDS('gaussian-model.RDS')
poissonModel  <- readRDS('poisson-model.RDS')
negbinomModel <- readRDS('negbinom-model.RDS')

weightedLogitModel <- readRDS('weighted-logit-model.RDS')
offsetPoissonModel <- readRDS('offset-poisson-model.RDS')

bigGaussianModel <- readRDS("big-gaussian-model.RDS")

directoryhelp::dirSetF('bayesMeanScale')
setwd("Testing Frequentist Predictions")

probitFreqPreds   <- readRDS('probit-freq-preds.RDS')
logitFreqPreds    <- readRDS('logit-freq-preds.RDS')
betaFreqPreds     <- readRDS('beta-freq-preds.RDS')
cloglogFreqPreds  <- readRDS('cloglog-freq-preds.RDS')
gammaFreqPreds    <- readRDS('gamma-freq-preds.RDS')
gaussianFreqPreds <- readRDS('gaussian-freq-preds.RDS')
poissonFreqPreds  <- readRDS('poisson-freq-preds.RDS')
negbinomFreqPreds <- readRDS('negbinom-freq-preds.RDS')

weightedLogitFreqPreds <- readRDS('weighted-logit-freq-preds.RDS')
offsetPoissonFreqPreds <- readRDS('offset-poisson-freq-preds.RDS')

# test that the big model can run #

test_that('make sure big model runs without a problem', {

   expect_no_error(bayesPredsF(bigGaussianModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')), at_means=T))

 })

# test for some expected errors #

test_that('make sure bayesPredsF throws expected errors', {

  expect_error(bayesPredsF(probitModel))
  expect_error(bayesPredsF(probitModel, at=list(d=c(1,2))))
  expect_error(bayesPredsF(probitModel, at=list(c=c(1,2))))
  expect_error(bayesPredsF(probitModel, at=list(a=c('Y', 'N'))))

})

# test on cloglog model #

cloglogPreds <- bayesPredsF(cloglogModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N'))) %>%
  dplyr::left_join(cloglogFreqPreds, by=c('a', 'b', 'c')) %>%
  dplyr::mutate(mean_diff   = abs(mean - Prediction),
                lower_diff  = abs(hpd_lower - lower),
                upper_diff  = abs(hpd_upper - upper),
                mean_bound  = abs(Prediction*1.01 - Prediction),
                lower_bound = abs(lower*1.01 - lower),
                upper_bound = abs(upper*1.01 - upper))

test_that('make sure bayesPredsF is working for cloglog model', {

  for(i in 1:nrow(cloglogPreds)){
    expect_lt(cloglogPreds$mean_diff[i], cloglogPreds$mean_bound[i])
    expect_lt(cloglogPreds$lower_diff[i], cloglogPreds$lower_bound[i])
    expect_lt(cloglogPreds$upper_diff[i], cloglogPreds$upper_bound[i])
  }

})


# test on logit model #

logitPreds <- bayesPredsF(logitModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N'))) %>%
  dplyr::left_join(logitFreqPreds, by=c('a', 'b', 'c')) %>%
  dplyr::mutate(mean_diff   = abs(mean - Prediction),
                lower_diff  = abs(hpd_lower - lower),
                upper_diff  = abs(hpd_upper - upper),
                mean_bound  = abs(Prediction*1.01 - Prediction),
                lower_bound = abs(lower*1.01 - lower),
                upper_bound = abs(upper*1.01 - upper))

logitPredsETI <- bayesPredsF(logitModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')), hdi_interval=F) %>%
  dplyr::left_join(logitFreqPreds, by=c('a', 'b', 'c')) %>%
  dplyr::mutate(mean_diff   = abs(mean - Prediction),
                lower_diff  = abs(hpd_lower - lower),
                upper_diff  = abs(hpd_upper - upper),
                mean_bound  = abs(Prediction*1.01 - Prediction),
                lower_bound = abs(lower*1.01 - lower),
                upper_bound = abs(upper*1.01 - upper))

weightedLogitPreds <- bayesPredsF(weightedLogitModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')), hdi_interval=F) %>%
  dplyr::left_join(weightedLogitFreqPreds, by=c('a', 'b', 'c')) %>%
  dplyr::mutate(mean_diff   = abs(mean - Prediction),
                lower_diff  = abs(hpd_lower - lower),
                upper_diff  = abs(hpd_upper - upper),
                mean_bound  = abs(Prediction*1.01 - Prediction),
                lower_bound = abs(lower*1.01 - lower),
                upper_bound = abs(upper*1.01 - upper))

test_that('make sure bayesPredsF is working for logit model', {

  for(i in 1:nrow(logitPreds)){
    expect_lt(logitPreds$mean_diff[i], logitPreds$mean_bound[i])
    expect_lt(logitPreds$lower_diff[i], logitPreds$lower_bound[i])
    expect_lt(logitPreds$upper_diff[i], logitPreds$upper_bound[i])
  }

  for(i in 1:nrow(logitPredsETI)){
    expect_lt(logitPredsETI$mean_diff[i], logitPredsETI$mean_bound[i])
    expect_lt(logitPredsETI$lower_diff[i], logitPredsETI$lower_bound[i])
    expect_lt(logitPredsETI$upper_diff[i], logitPredsETI$upper_bound[i])
  }

  for(i in 1:nrow(weightedLogitPreds)){
    expect_lt(weightedLogitPreds$mean_diff[i], weightedLogitPreds$mean_bound[i])
    expect_lt(weightedLogitPreds$lower_diff[i], weightedLogitPreds$lower_bound[i])
    expect_lt(weightedLogitPreds$upper_diff[i], weightedLogitPreds$upper_bound[i])
  }

})

# test on probit model #

probitPreds <- bayesPredsF(probitModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N'))) %>%
  dplyr::left_join(probitFreqPreds, by=c('a', 'b', 'c')) %>%
  dplyr::mutate(mean_diff   = abs(mean - Prediction),
                lower_diff  = abs(hpd_lower - lower),
                upper_diff  = abs(hpd_upper - upper),
                mean_bound  = abs(Prediction*1.01 - Prediction),
                lower_bound = abs(lower*1.01 - lower),
                upper_bound = abs(upper*1.01 - upper))

test_that('make sure bayesPredsF is working for probit model', {

  for(i in 1:nrow(probitPreds)){
    expect_lt(probitPreds$mean_diff[i], probitPreds$mean_bound[i])
    expect_lt(probitPreds$lower_diff[i], probitPreds$lower_bound[i])
    expect_lt(probitPreds$upper_diff[i], probitPreds$upper_bound[i])
  }

})

# test on gamma model #

gammaPreds <- bayesPredsF(gammaModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N'))) %>%
  dplyr::left_join(gammaFreqPreds, by=c('a', 'b', 'c')) %>%
  dplyr::mutate(mean_diff   = abs(mean - Prediction),
                lower_diff  = abs(hpd_lower - lower),
                upper_diff  = abs(hpd_upper - upper),
                mean_bound  = abs(Prediction*1.01 - Prediction),
                lower_bound = abs(lower*1.01 - lower),
                upper_bound = abs(upper*1.01 - upper))

test_that('make sure bayesPredsF is working for gamma model', {

  for(i in 1:nrow(gammaPreds)){
    expect_lt(gammaPreds$mean_diff[i], gammaPreds$mean_bound[i])
    expect_lt(gammaPreds$lower_diff[i], gammaPreds$lower_bound[i])
    expect_lt(gammaPreds$upper_diff[i], gammaPreds$upper_bound[i])
  }

})

# test on gaussian model #

gaussianPreds <- bayesPredsF(gaussianModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N'))) %>%
  dplyr::left_join(gaussianFreqPreds, by=c('a', 'b', 'c')) %>%
  dplyr::mutate(mean_diff   = abs(mean - Prediction),
                lower_diff  = abs(hpd_lower - lower),
                upper_diff  = abs(hpd_upper - upper),
                mean_bound  = abs(Prediction*1.01 - Prediction),
                lower_bound = abs(lower*1.01 - lower),
                upper_bound = abs(upper*1.01 - upper))

gaussianPredsETI <- bayesPredsF(gaussianModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')), hdi_interval=F) %>%
  dplyr::left_join(gaussianFreqPreds, by=c('a', 'b', 'c')) %>%
  dplyr::mutate(mean_diff   = abs(mean - Prediction),
                lower_diff  = abs(hpd_lower - lower),
                upper_diff  = abs(hpd_upper - upper),
                mean_bound  = abs(Prediction*1.01 - Prediction),
                lower_bound = abs(lower*1.01 - lower),
                upper_bound = abs(upper*1.01 - upper))

test_that('make sure bayesPredsF is working for gaussian model', {

  for(i in 1:nrow(gaussianPreds)){
    expect_lt(gaussianPreds$mean_diff[i], gaussianPreds$mean_bound[i])
    expect_lt(gaussianPreds$lower_diff[i], gaussianPreds$lower_bound[i])
    expect_lt(gaussianPreds$upper_diff[i], gaussianPreds$upper_bound[i])
  }

  for(i in 1:nrow(gaussianPredsETI)){
    expect_lt(gaussianPredsETI$mean_diff[i], gaussianPredsETI$mean_bound[i])
    expect_lt(gaussianPredsETI$lower_diff[i], gaussianPredsETI$lower_bound[i])
    expect_lt(gaussianPredsETI$upper_diff[i], gaussianPredsETI$upper_bound[i])
  }

})

# test on Poisson model #

poissonPreds <- bayesPredsF(poissonModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N'))) %>%
  dplyr::left_join(poissonFreqPreds, by=c('a', 'b', 'c')) %>%
  dplyr::mutate(mean_diff   = abs(mean - Prediction),
                lower_diff  = abs(hpd_lower - lower),
                upper_diff  = abs(hpd_upper - upper),
                mean_bound  = abs(Prediction*1.01 - Prediction),
                lower_bound = abs(lower*1.01 - lower),
                upper_bound = abs(upper*1.01 - upper))

offsetPoissonPreds <- bayesPredsF(offsetPoissonModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N'))) %>%
  dplyr::left_join(offsetPoissonFreqPreds, by=c('a', 'b', 'c')) %>%
  dplyr::mutate(mean_diff   = abs(mean - Prediction),
                lower_diff  = abs(hpd_lower - lower),
                upper_diff  = abs(hpd_upper - upper),
                mean_bound  = abs(Prediction*1.01 - Prediction),
                lower_bound = abs(lower*1.01 - lower),
                upper_bound = abs(upper*1.01 - upper))

test_that('make sure bayesPredsF is working for Poisson model', {

  for(i in 1:nrow(poissonPreds)){
    expect_lt(poissonPreds$mean_diff[i], poissonPreds$mean_bound[i])
    expect_lt(poissonPreds$lower_diff[i], poissonPreds$lower_bound[i])
    expect_lt(poissonPreds$upper_diff[i], poissonPreds$upper_bound[i])
  }

  for(i in 1:nrow(offsetPoissonPreds)){
    expect_lt(offsetPoissonPreds$mean_diff[i], offsetPoissonPreds$mean_bound[i])
    expect_lt(offsetPoissonPreds$lower_diff[i], offsetPoissonPreds$lower_bound[i])
    expect_lt(offsetPoissonPreds$upper_diff[i], offsetPoissonPreds$upper_bound[i])
  }

})



