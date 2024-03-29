

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
  merge(., cloglogFreqPreds, by=c('a', 'b', 'c')) %>%
  setDT() %>%
  .[, `:=`(mean_diff   = abs(mean - Prediction),
           lower_diff  = abs(hpd_lower - lower),
           upper_diff  = abs(hpd_upper - upper),
           mean_bound  = abs(Prediction*1.01 - Prediction),
           lower_bound = abs(lower*1.01 - lower),
           upper_bound = abs(upper*1.01 - upper))]

test_that('make sure bayesPredsF is working for cloglog model', {

  for(i in 1:nrow(cloglogPreds)){
    expect_lt(cloglogPreds$mean_diff[i], cloglogPreds$mean_bound[i])
    expect_lt(cloglogPreds$lower_diff[i], cloglogPreds$lower_bound[i])
    expect_lt(cloglogPreds$upper_diff[i], cloglogPreds$upper_bound[i])
  }

})


# test on logit model #

logitPreds <- bayesPredsF(logitModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N'))) %>%
  merge(., logitFreqPreds, by=c('a', 'b', 'c')) %>%
  setDT() %>%
  .[, `:=`(mean_diff   = abs(mean - Prediction),
           lower_diff  = abs(hpd_lower - lower),
           upper_diff  = abs(hpd_upper - upper),
           mean_bound  = abs(Prediction*1.01 - Prediction),
           lower_bound = abs(lower*1.01 - lower),
           upper_bound = abs(upper*1.01 - upper))]

logitPredsETI <- bayesPredsF(logitModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')), hdi_interval=F) %>%
  merge(., logitFreqPreds, by=c('a', 'b', 'c')) %>%
  setDT() %>%
  .[, `:=`(mean_diff   = abs(mean - Prediction),
           lower_diff  = abs(hpd_lower - lower),
           upper_diff  = abs(hpd_upper - upper),
           mean_bound  = abs(Prediction*1.01 - Prediction),
           lower_bound = abs(lower*1.01 - lower),
           upper_bound = abs(upper*1.01 - upper))]

weightedLogitPreds <- bayesPredsF(weightedLogitModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')), hdi_interval=F) %>%
  merge(., weightedLogitFreqPreds, by=c('a', 'b', 'c')) %>%
  setDT() %>%
  .[, `:=`(mean_diff   = abs(mean - Prediction),
           lower_diff  = abs(hpd_lower - lower),
           upper_diff  = abs(hpd_upper - upper),
           mean_bound  = abs(Prediction*1.01 - Prediction),
           lower_bound = abs(lower*1.01 - lower),
           upper_bound = abs(upper*1.01 - upper))]

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
  merge(., probitFreqPreds, by=c('a', 'b', 'c')) %>%
  setDT() %>%
  .[, `:=`(mean_diff   = abs(mean - Prediction),
           lower_diff  = abs(hpd_lower - lower),
           upper_diff  = abs(hpd_upper - upper),
           mean_bound  = abs(Prediction*1.01 - Prediction),
           lower_bound = abs(lower*1.01 - lower),
           upper_bound = abs(upper*1.01 - upper))]

test_that('make sure bayesPredsF is working for probit model', {

  for(i in 1:nrow(probitPreds)){
    expect_lt(probitPreds$mean_diff[i], probitPreds$mean_bound[i])
    expect_lt(probitPreds$lower_diff[i], probitPreds$lower_bound[i])
    expect_lt(probitPreds$upper_diff[i], probitPreds$upper_bound[i])
  }

})

# test on gamma model #

gammaPreds <- bayesPredsF(gammaModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N'))) %>%
  merge(., gammaFreqPreds, by=c('a', 'b', 'c')) %>%
  setDT() %>%
  .[, `:=`(mean_diff   = abs(mean - Prediction),
           lower_diff  = abs(hpd_lower - lower),
           upper_diff  = abs(hpd_upper - upper),
           mean_bound  = abs(Prediction*1.01 - Prediction),
           lower_bound = abs(lower*1.01 - lower),
           upper_bound = abs(upper*1.01 - upper))]

test_that('make sure bayesPredsF is working for gamma model', {

  for(i in 1:nrow(gammaPreds)){
    expect_lt(gammaPreds$mean_diff[i], gammaPreds$mean_bound[i])
    expect_lt(gammaPreds$lower_diff[i], gammaPreds$lower_bound[i])
    expect_lt(gammaPreds$upper_diff[i], gammaPreds$upper_bound[i])
  }

})

# test on Poisson model #

poissonPreds <- bayesPredsF(poissonModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N'))) %>%
  merge(., poissonFreqPreds, by=c('a', 'b', 'c')) %>%
  setDT() %>%
  .[, `:=`(mean_diff   = abs(mean - Prediction),
           lower_diff  = abs(hpd_lower - lower),
           upper_diff  = abs(hpd_upper - upper),
           mean_bound  = abs(Prediction*1.01 - Prediction),
           lower_bound = abs(lower*1.01 - lower),
           upper_bound = abs(upper*1.01 - upper))]

offsetPoissonPreds <- bayesPredsF(offsetPoissonModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N'))) %>%
  merge(., offsetPoissonFreqPreds, by=c('a', 'b', 'c')) %>%
  setDT() %>%
  .[, `:=`(mean_diff   = abs(mean - Prediction),
           lower_diff  = abs(hpd_lower - lower),
           upper_diff  = abs(hpd_upper - upper),
           mean_bound  = abs(Prediction*1.01 - Prediction),
           lower_bound = abs(lower*1.01 - lower),
           upper_bound = abs(upper*1.01 - upper))]

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



