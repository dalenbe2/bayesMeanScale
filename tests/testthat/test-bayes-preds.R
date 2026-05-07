

test_that("make sure bayesPredsF is working properly", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  
  set.seed(500)
  
  modelData       <- rstanarm::wells
  modelData$assoc <- ifelse(modelData$assoc==1, 'Y', 'N')
  modelData$z     <- .1
  
  rowMiss <- sample(1:nrow(modelData), size=10, replace=F)
  colMiss <- sample(1:ncol(modelData), size=10, replace=T)
  
  for(i in 1:10){
    
    modelData[rowMiss[[i]], colMiss[[i]]] <- NA
    
  }
  
  logitModel  <- suppressWarnings(rstanarm::stan_glm(switch ~ dist*educ + arsenic + I(arsenic^2) + assoc, offset=rep(.1, nrow(modelData)), data=modelData, family=binomial, refresh=0, chains=2, iter=500))
  
  logitModel2 <- suppressWarnings(rstanarm::stan_glm(switch ~ log(dist) + educ + arsenic + I(arsenic^2) + as.factor(assoc), data=modelData, family=binomial, refresh=0, chains=2, iter=500))
  
  m1JointPost <- as.data.frame(logitModel)
  m2JointPost <- as.data.frame(logitModel2)
  
  m1Formula   <- formula(logitModel)
  m2Formula   <- formula(logitModel2)
  
  m1Offset    <- .1
  
  completeData <- na.omit(modelData)
  
  expect_no_error(bayesPredsF(logitModel2, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12)), at_means=T, n_draws=500))
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12)), hdi_interval=F, at_means=T, n_draws=500))
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12), dist=c(10, 20), arsenic=c(2,3), assoc=c('Y')), n_draws=500, data_slice=100))
  
  expect_no_warning(bayesPredsF(logitModel2, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_warning(bayesPredsF(logitModel, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_warning(bayesPredsF(logitModel, at=list(educ=c(0, 12)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesPredsF(logitModel, at=list(educ=c(0, 12)), at_means=T, n_draws=500))
  expect_no_warning(bayesPredsF(logitModel, at=list(educ=c(0, 12)), hdi_interval=F, at_means=T, n_draws=500, data_slice=100))
  
  expect_no_error(bayesPredsF(m2JointPost, model_data=completeData, model_formula=m2Formula, link_function='logit', at=list(educ=c(0, 12)), n_draws=500))
  expect_no_error(bayesPredsF(m1JointPost, model_data=completeData, model_formula=m1Formula, link_function='logit', model_offset=m1Offset, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_error(bayesPredsF(m1JointPost, model_data=completeData, model_formula=m1Formula, link_function='logit', model_offset=m1Offset, at=list(educ=c(0, 12)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesPredsF(m1JointPost, model_data=completeData, model_formula=m1Formula, link_function='logit', model_offset=m1Offset, at=list(educ=c(0, 12)), at_means=T, n_draws=500))
  expect_no_error(bayesPredsF(m1JointPost, model_data=completeData, model_formula=m1Formula, link_function='logit', model_offset=m1Offset, at=list(educ=c(0, 12)), hdi_interval=F, at_means=T, n_draws=500))
  expect_no_error(bayesPredsF(m1JointPost, model_data=completeData, model_formula=m1Formula, link_function='logit', model_offset=m1Offset, at=list(educ=c(0, 12), dist=c(10, 20), arsenic=c(2,3), assoc=c('Y')), n_draws=500, data_slice=100))
  
  expect_no_warning(bayesPredsF(m2JointPost, model_data=completeData, model_formula=m2Formula, link_function='logit', at=list(educ=c(0, 12)), n_draws=500))
  expect_no_warning(bayesPredsF(m1JointPost, model_data=completeData, model_formula=m1Formula, link_function='logit', model_offset=m1Offset, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_warning(bayesPredsF(m1JointPost, model_data=completeData, model_formula=m1Formula, link_function='logit', model_offset=m1Offset, at=list(educ=c(0, 12)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesPredsF(m1JointPost, model_data=completeData, model_formula=m1Formula, link_function='logit', model_offset=m1Offset, at=list(educ=c(0, 12)), at_means=T, n_draws=500))
  expect_no_warning(bayesPredsF(m1JointPost, model_data=completeData, model_formula=m1Formula, link_function='logit', model_offset=m1Offset, at=list(educ=c(0, 12)), hdi_interval=F, at_means=T, n_draws=500))
  expect_no_warning(bayesPredsF(m1JointPost, model_data=completeData, model_formula=m1Formula, link_function='logit', model_offset=m1Offset, at=list(educ=c(0, 12), dist=c(10, 20), arsenic=c(2,3), assoc=c('Y')), n_draws=500, data_slice=100))
  
  set.seed(500)
  
  predStanReg <- bayesPredsF(logitModel, at=list(educ=c(0, 12)), n_draws=2000)$predTable %>%
    subset(., select=c(mean, lower, upper))
  
  set.seed(500)
  
  predDF      <- bayesPredsF(m1JointPost, model_data=completeData, model_formula=m1Formula, link_function='logit', model_offset=m1Offset, at=list(educ=c(0, 12)), n_draws=2000)$predTable %>%
    subset(., select=c(mean, lower, upper))
  
  maxDiff <- max(abs(predStanReg - predDF))
  
  expect_lt(maxDiff, .001)
  
})


