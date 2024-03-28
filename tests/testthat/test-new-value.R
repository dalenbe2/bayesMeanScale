
directoryhelp::dirSetF('bayesMeanScale')
setwd("Testing Models")

betaModel <- readRDS('beta-model.RDS')

test_that('make sure that factor levels line up with original data', {
  expect_identical(levels(newValueF(betaModel$model, marg_effect='which_gpa', new_value='first_year')$which_gpa), sort(unique(betaModel$model$which_gpa)))
})
