
if(require('rstanarm')){

set.seed(500)

modelData       <- rstanarm::wells
modelData$assoc <- ifelse(modelData$assoc==1, 'Y', 'N')

rowMiss <- sample(1:nrow(modelData), size=200, replace=F)
colMiss <- sample(1:ncol(modelData), size=200, replace=T)

for(i in 1:200){
  
  modelData[rowMiss[[i]], colMiss[[i]]] <- NA
  
}

summary(modelData)

logitModel  <- suppressWarnings(rstanarm::stan_glm(switch ~ dist*educ + arsenic + I(arsenic^2) + assoc, data=modelData, family=binomial, refresh=0, iter=200))
logitModel2 <- suppressWarnings(rstanarm::stan_glm(switch ~ log(dist) + educ + arsenic + I(arsenic^2) + as.factor(assoc), data=modelData, family=binomial, refresh=0, iter=200))

crabs <- read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=T)

poissonModel <- suppressWarnings(rstanarm::stan_glm(sat ~ weight + width, data=crabs, family=poisson, refresh=0, iter=200))

## save the data for internal use ##

usethis::use_data(logitModel, logitModel2, poissonModel, internal=T, overwrite=T)

}
