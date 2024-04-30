
library(rstanarm)
library(tibble)
library(MASS)

set.seed(500)

data(wells)

modelData <- wells %>%
  mutate(assoc = if_else(assoc==1, 'Y', 'N'))

rowMiss <- sample(1:nrow(modelData), size=10, replace=F)
colMiss <- sample(1:ncol(modelData), size=10, replace=T)

for(i in 1:10){
  
  modelData[rowMiss[[i]], colMiss[[i]]] <- NA
  
}

summary(modelData)

logitModel  <- stan_glm(switch ~ dist*educ + arsenic + I(arsenic^2) + assoc, data=modelData, family=binomial, refresh=0)
logitModel2 <- stan_glm(switch ~ log(dist) + educ + arsenic + I(arsenic^2) + as.factor(assoc), data=modelData, family=binomial, refresh=0)

crabs <- read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=T)

poissonModel <- rstanarm::stan_glm(sat ~ weight + width, data=crabs, family=poisson, refresh=0, iter=1000)

## save the data for internal use ##

usethis::use_data(logitModel, logitModel2, poissonModel, internal=T, overwrite=T)
