

test_that("make sure bayesCountMargEffF is working properly", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  
  set.seed(500)
  
  crabs <- read.table("https://stat4ds.rwth-aachen.de/data/Crabs.dat", header=T)
  
  poissonModel  <- suppressWarnings(rstanarm::stan_glm(sat ~ weight + width, offset=rep(.1, nrow(crabs)), data=crabs, family=poisson, refresh=0, chains=2, iter=500))
  negBinomModel <- suppressWarnings(rstanarm::stan_glm(sat ~ weight + width, data=crabs, family=rstanarm::neg_binomial_2, refresh=0, chains=2, iter=500))
  
  poissonJointPost  <- as.data.frame(poissonModel)
  negBinomJointPost <- as.data.frame(negBinomModel)
  
  poissonFormula    <- formula(poissonModel)
  negBinomFormula   <- formula(negBinomModel)
  
  poissonOffset     <- .1
  
  expect_no_error(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value="instantaneous", end_value="instantaneous", at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  
  expect_no_error(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_error(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))

  
  
  expect_no_warning(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value="instantaneous", end_value="instantaneous", at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  
  expect_no_warning(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_warning(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  
  
  
  expect_no_error(bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value="instantaneous", end_value="instantaneous", at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  
  expect_no_error(bayesCountMargEffF(negBinomJointPost, model_data=crabs, model_formula=negBinomFormula, link_function='log', model_family='neg_binomial_2', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountMargEffF(negBinomJointPost, model_data=crabs, model_formula=negBinomFormula, link_function='log', model_family='neg_binomial_2', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountMargEffF(negBinomJointPost, model_data=crabs, model_formula=negBinomFormula, link_function='log', model_family='neg_binomial_2', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesCountMargEffF(negBinomJointPost, model_data=crabs, model_formula=negBinomFormula, link_function='log', model_family='neg_binomial_2', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_error(bayesCountMargEffF(negBinomJointPost, model_data=crabs, model_formula=negBinomFormula, link_function='log', model_family='neg_binomial_2', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  
  
  
  expect_no_warning(bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value="instantaneous", end_value="instantaneous", at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  
  expect_no_warning(bayesCountMargEffF(negBinomJointPost, model_data=crabs, model_formula=negBinomFormula, link_function='log', model_family='neg_binomial_2', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountMargEffF(negBinomJointPost, model_data=crabs, model_formula=negBinomFormula, link_function='log', model_family='neg_binomial_2', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountMargEffF(negBinomJointPost, model_data=crabs, model_formula=negBinomFormula, link_function='log', model_family='neg_binomial_2', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesCountMargEffF(negBinomJointPost, model_data=crabs, model_formula=negBinomFormula, link_function='log', model_family='neg_binomial_2', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_warning(bayesCountMargEffF(negBinomJointPost, model_data=crabs, model_formula=negBinomFormula, link_function='log', model_family='neg_binomial_2', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  
  set.seed(500)
  
  margStanReg <- bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=5000)$diffTable %>%
    subset(., select=c(mean, lower, upper))
  
  set.seed(500)
  
  margDF      <- bayesCountMargEffF(poissonJointPost, model_data=crabs, model_formula=poissonFormula, link_function='log', model_offset=poissonOffset, model_family='poisson', counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=5000)$diffTable %>%
    subset(., select=c(mean, lower, upper))
  
  maxDiff <- max(abs(margStanReg - margDF))
  
  expect_lt(maxDiff, .001)
  
})
