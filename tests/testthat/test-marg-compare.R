
set.seed(500)

m1AMEInteraction <- bayesMargEffF(logitModel,
                                  marginal_effect = 'dist',
                                  start_value     = 64.041,
                                  end_value       = 21.117,
                                  at              = list(educ=c(0, 5, 8)),
                                  n_draws         = 500)

housingData <- MASS::housing

rowMiss <- sample(1:nrow(housingData), size=10, replace=F)
colMiss <- sample(1:ncol(housingData), size=10, replace=T)

for(i in 1:10){
  
  housingData[rowMiss[[i]], colMiss[[i]]] <- NA
  
}


ordinalModel <- suppressWarnings(rstanarm::stan_polr(Sat ~ Infl + Type, data=housingData, prior=rstanarm::R2(0.2, 'mean'), refresh=0, iter=200))

ordinalMarg <- bayesOrdinalMargEffF(ordinalModel, 
                                    marginal_effect = "Infl", 
                                    start_value     = "Low", 
                                    end_value       = "High",  
                                    at              = list(Type=c("Tower", "Apartment")), 
                                    n_draws         = 500)

test_that("test that bayesMargCompareF runs without error", {
  
  expect_no_error(bayesMargCompareF(m1AMEInteraction))
  expect_no_error(bayesMargCompareF(m1AMEInteraction, centrality='median'))
  expect_no_error(bayesMargCompareF(ordinalMarg))
  
})

test_that("test that bayesMargCompareF runs without warning", {
  
  expect_no_warning(bayesMargCompareF(m1AMEInteraction))
  expect_no_warning(bayesMargCompareF(m1AMEInteraction, centrality='median'))
  expect_no_warning(bayesMargCompareF(ordinalMarg))
  
})