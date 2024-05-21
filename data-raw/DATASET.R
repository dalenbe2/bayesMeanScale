

if(require(rstanarm)){

modelData       <- rstanarm::wells
modelData$assoc <- ifelse(modelData$assoc==1, 'Y', 'N')

rowMiss <- sample(1:nrow(modelData), size=10, replace=F)
colMiss <- sample(1:ncol(modelData), size=10, replace=T)

for(i in 1:10){
  
  modelData[rowMiss[[i]], colMiss[[i]]] <- NA
  
}

summary(modelData)

logitModel  <- rstanarm::stan_glm(switch ~ dist*educ + arsenic + I(arsenic^2) + assoc, data=modelData, family=binomial, refresh=0)
logitModel2 <- rstanarm::stan_glm(switch ~ log(dist) + educ + arsenic + I(arsenic^2) + as.factor(assoc), data=modelData, family=binomial, refresh=0)

crabs <- read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=T)

poissonModel  <- rstanarm::stan_glm(sat ~ weight + width, data=crabs, family=poisson, refresh=0)
negBinomModel <- rstanarm::stan_glm(sat ~ weight + width, data=crabs, family=neg_binomial_2, refresh=0)

x     <- rnorm(2000, mean=3)
w     <- rnorm(2000, mean=2)
log_y <- .5 + .2*x - .2*w
y     <- rpois(2000, lambda=exp(log_y))

poissonData <- data.frame(x, w, log_y, y)

poissonModel2  <- rstanarm::stan_glm(y ~ x + w, data=poissonData, family=poisson, refresh=0)
negBinomModel2 <- rstanarm::stan_glm(y ~ x + w, data=poissonData, family=neg_binomial_2, refresh=0)

## save the data for internal use ##

usethis::use_data(logitModel, logitModel2, poissonModel, negBinomModel, poissonModel2, negBinomModel2, internal=T, overwrite=T)

}
