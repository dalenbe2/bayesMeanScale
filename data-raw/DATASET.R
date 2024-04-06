
library(rstanarm)

data(wells)

modelData <- wells %>%
  mutate(assoc = if_else(assoc==1, 'Y', 'N'))

summary(modelData)

logitModel <- stan_glm(switch ~ dist*educ + arsenic + I(arsenic^2) + assoc, data=modelData, family=binomial, refresh=0)

## save the data for internal use ##

usethis::use_data(logitModel, internal=T, overwrite=T)
