

margTestError   <- bayesMargEffF(gaussianModel, marginal_effect='c', start_value='Y', end_value='N', digits=4)
margTestNoError <- bayesMargEffF(gaussianModel, marginal_effect='c', start_value='Y', end_value='N', digits=4, at=list(a=c(9, 10, 11)))

test_that("make sure margCompareErrorCheckF is catching errors", {
  
  expect_error(margCompareErrorCheckF(1), regexp="The 'marg_list' argument must have class 'bayes.marg'!")
  expect_no_error(margCompareErrorCheckF(margTestNoError))
  expect_error(margCompareErrorCheckF(margTestError), regexp="There is only 1 marginal effect, so nothing to compare to!")
  
})
