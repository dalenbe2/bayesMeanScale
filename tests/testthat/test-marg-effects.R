

test_that("make sure all configurations of bayesMargEffF run without error", {
  
  expect_no_error(bayesMargEffF(logitModel2, marginal_effect='log(dist)', start_value=4, end_value=3, at=list(educ=c(0, 12))))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12))))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), hdi_interval=F))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), at_means=T))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), at_means=T, hdi_interval=F))
  
})

ame <- bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), digits=6)$diffTable %>%
  subset(., select=c(mean, lower, upper))

mem <- bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), digits=6, at_means=T)$diffTable %>%
  subset(., select=c(mean, lower, upper))

diffs <- abs(ame-mem)

test_that('make sure ame and mem are close but not exactly the same', {
  
  for(i in nrow(diffs)){
    for(j in ncol(diffs)){
      
      expect_gt(diffs[i, j], 0)
      expect_lt(diffs[i, j], ame[i, j]*1.05 - ame[i, j])
      
    }
  }

  
})