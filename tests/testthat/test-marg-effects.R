

# test on Beta model #

betaMargSmall <- bayesMargEffF(betaModel, marginal_effect='c', start_value='Y', end_value='N', digits=4)$diffTable %>%
  merge(., betaFreqMargSmall, by.x='marg_effect', by.y='factor') %>%
  setDT() %>%
  .[, `:=`(mean_diff   = abs(mean - AME),
           lower_diff  = abs(hpd_lower - lower),
           upper_diff  = abs(hpd_upper - upper),
           mean_bound  = abs(AME*1.1 - AME),
           lower_bound = abs(lower*1.1 - lower),
           upper_bound = abs(upper*1.1 - upper))]

test_that('make sure bayesMargEffF is working for beta model', {

  for(i in 1:nrow(betaMargSmall)){
    expect_lt(betaMargSmall$mean_diff[i], betaMargSmall$mean_bound[i])
    expect_lt(betaMargSmall$lower_diff[i], betaMargSmall$lower_bound[i])
    expect_lt(betaMargSmall$upper_diff[i], betaMargSmall$upper_bound[i])
  }

})

# test on Gaussian model #

gaussianMarg <- bayesMargEffF(gaussianModel, marginal_effect='c', start_value='Y', end_value='N', digits=4)$diffTable %>%
  merge(., gaussianFreqMarg, by.x='marg_effect', by.y='factor') %>%
  setDT() %>%
  .[, `:=`(mean_diff   = abs(mean - AME),
           lower_diff  = abs(hpd_lower - lower),
           upper_diff  = abs(hpd_upper - upper),
           mean_bound  = abs(AME*1.1 - AME),
           lower_bound = abs(lower*1.1 - lower),
           upper_bound = abs(upper*1.1 - upper))]

test_that('make sure bayesMargEffF is working for Gaussian model', {

  for(i in 1:nrow(gaussianMarg)){
    expect_lt(gaussianMarg$mean_diff[i], gaussianMarg$mean_bound[i])
    expect_lt(gaussianMarg$lower_diff[i], gaussianMarg$lower_bound[i])
    expect_lt(gaussianMarg$upper_diff[i], gaussianMarg$upper_bound[i])
  }

})

# test on logit model #

logitMarg <- bayesMargEffF(logitModel, marginal_effect='c', start_value='Y', end_value='N', digits=4, at=list(a=c(9, 10, 11)))$diffTable %>%
  merge(., logitFreqMarg, by.x=c('marg_effect', 'a'), by.y=c('factor', 'a')) %>%
  setDT() %>%
  .[, `:=`(mean_diff   = abs(mean - AME),
           lower_diff  = abs(hpd_lower - lower),
           upper_diff  = abs(hpd_upper - upper),
           mean_bound  = abs(AME*1.1 - AME),
           lower_bound = abs(lower*1.1 - lower),
           upper_bound = abs(upper*1.1 - upper))]

test_that('make sure bayesMargEffF is working for logit model', {

  for(i in 1:nrow(logitMarg)){
    expect_lt(logitMarg$mean_diff[i], logitMarg$mean_bound[i])
    expect_lt(logitMarg$lower_diff[i], logitMarg$lower_bound[i])
    expect_lt(logitMarg$upper_diff[i], logitMarg$upper_bound[i])
  }

})

# test on Poisson model #

poissonMarg <- bayesMargEffF(poissonModel, marginal_effect='c', start_value='Y', end_value='N', digits=4, at=list(a=c(9, 10, 11)), at_means=T)$diffTable %>%
  merge(., poissonFreqMarg, by.x=c('marg_effect', 'a'), by.y=c('factor', 'a')) %>%
  setDT() %>%
  .[, `:=`(mean_diff   = abs(mean - AME),
           lower_diff  = abs(hpd_lower - lower),
           upper_diff  = abs(hpd_upper - upper),
           mean_bound  = abs(AME*1.1 - AME),
           lower_bound = abs(lower*1.1 - lower),
           upper_bound = abs(upper*1.1 - upper))]

test_that('make sure bayesMargEffF is working for Poisson model', {

  for(i in 1:nrow(poissonMarg)){
    expect_lt(poissonMarg$mean_diff[i], poissonMarg$mean_bound[i])
    expect_lt(poissonMarg$lower_diff[i], poissonMarg$lower_bound[i])
    expect_lt(poissonMarg$upper_diff[i], poissonMarg$upper_bound[i])
  }

})

