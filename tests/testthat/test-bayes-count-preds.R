

test_that("make sure all configurations of bayesCountPredsF run without error", {
  
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=100))
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=100))
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, n_draws=100))
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), at_means=T, n_draws=100))
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=100))
  
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=100))
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=100))
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, n_draws=100))
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), at_means=T, n_draws=100))
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=100))
  
})

test_that("make sure all configurations of bayesCountPredsF run without warning", {
  
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=100))
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=100))
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, n_draws=100))
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), at_means=T, n_draws=100))
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=100))
  
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=100))
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=100))
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, n_draws=100))
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), at_means=T, n_draws=100))
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=100))
  
})

