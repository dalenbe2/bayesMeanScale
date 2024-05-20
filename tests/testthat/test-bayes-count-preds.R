
set.seed(500)

test_that("make sure bayesCountPredsF is working properly", {

  skip_on_cran()
  skip_if_not_installed('rstanarm')
  
  crabs <- read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=T)
  
  rowMiss <- sample(1:nrow(crabs), size=10, replace=F)
  colMiss <- sample(1:ncol(crabs), size=10, replace=T)
  
  for(i in 1:10){
    
    crabs[rowMiss[[i]], colMiss[[i]]] <- NA
    
  }
  
  negBinomModel <- suppressWarnings(rstanarm::stan_glm(sat ~ weight + width, data=crabs, family=rstanarm::neg_binomial_2, refresh=0, iter=200))
  
  x     <- rnorm(2000, mean=3)
  w     <- rnorm(2000, mean=2)
  log_y <- .5 + .2*x - .2*w
  y     <- rpois(2000, lambda=exp(log_y)) 
  
  poissonData <- data.frame(x, w, log_y, y)
  
  rowMiss <- sample(1:nrow(poissonData), size=10, replace=F)
  colMiss <- sample(1:ncol(poissonData), size=10, replace=T)
  
  for(i in 1:10){
    
    poissonData[rowMiss[[i]], colMiss[[i]]] <- NA
    
  }
  
  poissonModel2  <- suppressWarnings(rstanarm::stan_glm(y ~ x + w, data=poissonData, family=poisson, refresh=0, iter=200))
  negBinomModel2 <- suppressWarnings(rstanarm::stan_glm(y ~ x + w, data=poissonData, family=rstanarm::neg_binomial_2, refresh=0, iter=200))
  
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))

  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))


  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))

  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  
  poissonPreds <- bayesCountPredsF(poissonModel2, counts=c(0,1), at=list(w=c(2,3)))$predTable %>%
    subset(., select=c(mean))
  
  nbPreds <- bayesCountPredsF(negBinomModel2, counts=c(0,1), at=list(w=c(2,3)))$predTable %>%
    subset(., select=c(mean))
  
  diffs <- abs(poissonPreds - nbPreds)
  
      expect_gt(diffs[1,], 0)
      expect_gt(diffs[2,], 0)
      expect_gt(diffs[3,], 0)
      expect_gt(diffs[4,], 0)
      
      expect_lt(diffs[1,], poissonPreds[1,]*1.15 - poissonPreds[1,])
      expect_lt(diffs[2,], poissonPreds[2,]*1.15 - poissonPreds[2,])
      expect_lt(diffs[3,], poissonPreds[3,]*1.15 - poissonPreds[3,])
      expect_lt(diffs[4,], poissonPreds[4,]*1.15 - poissonPreds[4,])
  
})

