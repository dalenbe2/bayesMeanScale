

testData <- data.frame(
  y = rnorm(10),
  a = rnorm(10)
)

freqModel <- lm(y ~ a, data=testData)

test_that("make sure margErrorCheckF is catching errors", {

  expect_error(margErrorCheckF(betaModel, marginal_effect='c', at=NULL, start_value=c('Y', 'Y'), end_value=c('N')), regexp="The arguments for 'marginal_effect,' 'start_value,' and 'end_value' must all have the same length!")
  expect_error(margErrorCheckF(freqModel, marginal_effect='a', at=NULL, start_value=1, end_value=2), regexp="The model must be a 'stanreg' object!")
  expect_error(margErrorCheckF(betaModel, marginal_effect='f', at=NULL, start_value=c('Y'), end_value=c('N')), regexp="The names of the marginal effects don't match up with the names in the model data!")
  expect_error(margErrorCheckF(betaModel, marginal_effect='a', at=list(d=c(1,2)), start_value=c('Y'), end_value=c('N')), regexp="The names for the at values don't match up with the names in the model data!")
  expect_error(margErrorCheckF(betaModel, marginal_effect=c('a', 'a'), at=NULL, start_value=c('Y', 'Y'), end_value=c('N', 'N')), regexp="When specifying multiple start values, they must be contained in a list to preserve data type!")

})
