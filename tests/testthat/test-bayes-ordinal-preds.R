
housingData <- MASS::housing

rowMiss <- sample(1:nrow(housingData), size=10, replace=F)
colMiss <- sample(1:ncol(housingData), size=10, replace=T)

for(i in 1:10){
  
  housingData[rowMiss[[i]], colMiss[[i]]] <- NA
  
}
  

ordinalModel        <- rstanarm::stan_polr(Sat ~ Infl + Type, data=housingData, prior=rstanarm::R2(0.2, 'mean'), refresh=0, iter=1000)
ordinalModelCloglog <- rstanarm::stan_polr(Sat ~ Infl + Type, data=housingData, prior=rstanarm::R2(0.2, 'mean'), refresh=0, method='cloglog', iter=1000)

test_that("make sure all configurations of bayesOrdinalPredsF run without error", {
  
  expect_no_error(bayesOrdinalPredsF(ordinalModel, at=list(Type=c("Tower", "Apartment")), n_draws=500))
  expect_no_error(bayesOrdinalPredsF(ordinalModel, at=list(Type=c("Tower", "Apartment")), hdi_interval=F, n_draws=500))
  expect_no_error(bayesOrdinalPredsF(ordinalModel, at=list(Type=c("Tower", "Apartment")), at_means=T, n_draws=500))
  expect_no_error(bayesOrdinalPredsF(ordinalModel, at=list(Type=c("Tower", "Apartment")), hdi_interval=F, at_means=T, n_draws=500))
  
})

test_that("make sure all configurations of bayesOrdinalPredsF run without warning", {

  expect_no_warning(bayesOrdinalPredsF(ordinalModel, at=list(Type=c("Tower", "Apartment")), n_draws=500))
  expect_no_warning(bayesOrdinalPredsF(ordinalModel, at=list(Type=c("Tower", "Apartment")), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesOrdinalPredsF(ordinalModel, at=list(Type=c("Tower", "Apartment")), at_means=T, n_draws=500))
  expect_no_warning(bayesOrdinalPredsF(ordinalModel, at=list(Type=c("Tower", "Apartment")), hdi_interval=F, at_means=T, n_draws=500))

})

set.seed(500)

logitPreds <- bayesOrdinalPredsF(ordinalModel, at=list(Type=c("Tower")), at_means=T, n_draws=500)$predDraws
logitCheck <- rowSums(subset(logitPreds, select=c(Low, Medium, High)))

cloglogPreds <- bayesOrdinalPredsF(ordinalModelCloglog, at=list(Type=c("Tower")), at_means=F, n_draws=500)$predDraws
cloglogCheck <- rowSums(subset(cloglogPreds, select=c(Low, Medium, High)))

test_that("make sure predictions add up to 1", {
  
  expect_identical(round(min(logitCheck), 6), 1)
  expect_identical(round(max(logitCheck), 6), 1)
  
  expect_identical(round(min(cloglogCheck), 6), 1)
  expect_identical(round(max(cloglogCheck), 6), 1)
  
})