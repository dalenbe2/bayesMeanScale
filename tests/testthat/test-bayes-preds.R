

test_that("make sure all configurations of bayesPredsF run without error", {
  
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12))))
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12)), hdi_interval=F))
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12)), at_means=T))
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12)), hdi_interval=F, at_means=T))
  
})



