
library(rstanarm)

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

## save the data for internal use ##

usethis::use_data(logitModel, logitModel2, internal=T, overwrite=T)
