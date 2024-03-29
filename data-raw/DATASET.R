
map(c('rstanarm', 'margins', 'betareg', 'MASS'), function(x) library(x, character.only=T))

N    <- 5000
nBig <- 100000

######## Beta model #######

betaData <- tibble(
  a = rnorm(N, mean=10, sd=3),
  b = rnorm(N, mean=15, sd=5),
  c = rbinom(N, size=1, prob=.3),
  beta_y = .02 + .01*a + .02*b + .01*a*b + .2*c,
  y = rbinom(N, size=1000, prob=exp(beta_y) / (1 + exp(beta_y)))/1000-.0001
) %>%
  mutate(c = if_else(c==1, 'Y', 'N'))

summary(betaData$y)

freqModel <- betareg(y ~ a*b + c, data=betaData)
betaFreqPreds <- summary(prediction(freqModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')))) %>%
  rename(a = `at(a)`,
         b = `at(b)`,
         c = `at(c)`)

betaFreqMargSmall <- summary(margins(freqModel, variables='c')) %>%
  mutate(factor = 'c',
         AME    = round(AME, 4),
         lower  = round(lower, 4),
         upper  = round(upper, 4))

betaModel <- stan_betareg(y ~ a*b + c, data=betaData, link='logit')

######## cloglog model ##########

cloglogData <- tibble(
  a = rnorm(N, mean=10, sd=3),
  b = rnorm(N, mean=15, sd=5),
  c = rbinom(N, size=1, prob=.3),
  cloglog_y = .02 + .01*a + .005*b + .0005*a*b + .05*c,
  y = rbinom(N, size=1, prob=1 - exp(-exp(cloglog_y)))
) %>%
  mutate(c = if_else(c==1, 'Y', 'N'))

summary(cloglogData$y)

freqModel <- glm(y ~ a*b + c, data=cloglogData, family=binomial(link='cloglog'))
cloglogFreqPreds <- summary(prediction(freqModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')))) %>%
  rename(a = `at(a)`,
         b = `at(b)`,
         c = `at(c)`)

cloglogModel <- stan_glm(y ~ a*b + c, data=cloglogData, family=binomial(link='cloglog'))


####### gamma model #########

gammaData <- tibble(
  a = rnorm(N, mean=10, sd=3),
  b = rnorm(N, mean=15, sd=5),
  c = rbinom(N, size=1, prob=.3),
  gamma_y = .02 + .01*a + .005*b + .0005*a*b + .05*c,
  y = rgamma(N, shape=5, rate=1/gamma_y)
) %>%
  mutate(c = if_else(c==1, 'Y', 'N'))

summary(gammaData$y)

freqModel <- glm(y ~ a*b + c, data=gammaData, family=Gamma(link='inverse'))
gammaFreqPreds <- summary(prediction(freqModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')))) %>%
  rename(a = `at(a)`,
         b = `at(b)`,
         c = `at(c)`)

gammaModel <- stan_glm(y ~ a*b + c, data=gammaData, family=Gamma(link='inverse'))


########## Poisson model #########

poissonData <- tibble(
  a = rnorm(N, mean=10, sd=3),
  b = rnorm(N, mean=15, sd=5),
  c = rbinom(N, size=1, prob=.3),
  poisson_y = .02 + .01*a + .005*b + .0005*a*b + .05*c,
  y = rpois(N, exp(poisson_y))
) %>%
  mutate(c = if_else(c==1, 'Y', 'N'))

summary(poissonData$y)

freqModel <- glm(y ~ a*b + c, data=poissonData, family=poisson(link='log'))
poissonFreqPreds <- summary(prediction(freqModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')))) %>%
  rename(a = `at(a)`,
         b = `at(b)`,
         c = `at(c)`)

poissonFreqMarg <- summary(margins(freqModel, variables='c', at=list(a=c(9,10,11), b=mean(poissonData$b)))) %>%
  mutate(factor = 'c',
         AME    = round(AME, 4),
         lower  = round(lower, 4),
         upper  = round(upper, 4))

poissonModel <- stan_glm(y ~ a*b + c, data=poissonData, family=poisson(link='log'))

######## Poisson model with offset terms ############

poissonData <- tibble(
  a = rnorm(N, mean=10, sd=3),
  b = rnorm(N, mean=15, sd=5),
  c = rbinom(N, size=1, prob=.3),
  k = rgamma(N, shape=1),
  j = rgamma(N, shape=1),
  w_offset = rgamma(N, shape=1),
  poisson_y = .02 + .01*a + .005*b + .0005*a*b + .05*c + log(k) + j + w_offset,
  y = rpois(N, exp(poisson_y))
) %>%
  mutate(c = if_else(c==1, 'Y', 'N'))

summary(poissonData$y)

freqModel <- glm(y ~ a*b + c + offset(log(k)) + offset(w_offset), offset=j, data=poissonData, family=poisson(link='log'))
summary(freqModel)

offsetPoissonFreqPreds <- summary(prediction(freqModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')))) %>%
  rename(a = `at(a)`,
         b = `at(b)`,
         c = `at(c)`)

offsetPoissonModel <- stan_glm(y ~ a*b + c + offset(log(k)) + offset(w_offset), offset=j, data=poissonData, family=poisson(link='log'))

######### negative binomial model #########

negbinomData <- tibble(
  a = rnorm(N, mean=10, sd=3),
  b = rnorm(N, mean=15, sd=5),
  c = rbinom(N, size=1, prob=.3),
  poisson_y = .02 + .01*a + .005*b + .0005*a*b + .05*c,
  y = rpois(N, exp(poisson_y))
) %>%
  mutate(c = if_else(c==1, 'Y', 'N'))

summary(negbinomData$y)

freqModel <- glm.nb(y ~ a*b + c, data=negbinomData)
negBinomFreqPreds <- summary(prediction(freqModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')))) %>%
  rename(a = `at(a)`,
         b = `at(b)`,
         c = `at(c)`)

negBinomModel <- stan_glm.nb(y ~ a*b + c, data=negbinomData)

######## Gaussian model ###########

gaussianData <- tibble(
  a = rnorm(N, mean=10, sd=3),
  b = rnorm(N, mean=15, sd=5),
  c = rbinom(N, size=1, prob=.3),
  y = 5 + 3*a + 4*b + 10*a*b + 4*c + rnorm(N, sd=50)
) %>%
  mutate(c = if_else(c==1, 'Y', 'N'))

freqModel <- lm(y ~ a*b + c, data=gaussianData)
gaussianFreqPreds <- summary(prediction(freqModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')))) %>%
  rename(a = `at(a)`,
         b = `at(b)`,
         c = `at(c)`)

gaussianFreqMarg <- summary(margins(freqModel, variables='c')) %>%
  mutate(factor = 'c',
         AME    = round(AME, 4),
         lower  = round(lower, 4),
         upper  = round(upper, 4))

gaussianModel <- stan_glm(y ~ a*b + c, data=gaussianData)

########### big Gaussian model ###########

gaussianData <- tibble(
  a = rnorm(nBig, mean=10, sd=3),
  b = rnorm(nBig, mean=15, sd=5),
  c = rbinom(nBig, size=1, prob=.3),
  y = 5 + 3*a + 4*b + 10*a*b + 4*c + rnorm(nBig, sd=300)
) %>%
  mutate(c = if_else(c==1, 'Y', 'N'))

bigGaussianModel <- stan_glm(y ~ a*b + c, data=gaussianData)

######## logit model ############

logitData <- tibble(
  a = rnorm(N, mean=10, sd=3),
  b = rnorm(N, mean=15, sd=5),
  c = rbinom(N, size=1, prob=.3),
  logit_y = .02 + .01*a + -.02*b + .01*a*b + .4*c,
  y = rbinom(N, size=1, prob=exp(logit_y) / (1 + exp(logit_y)))
) %>%
  mutate(c = if_else(c==1, 'Y', 'N'))

summary(logitData$y)

freqModel <- glm(y ~ a*b + c, data=logitData, family=binomial(link='logit'))
logitFreqPreds <- summary(prediction(freqModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')))) %>%
  rename(a = `at(a)`,
         b = `at(b)`,
         c = `at(c)`)

logitFreqMarg <- summary(margins(freqModel, variables='c', at=list(a=c(9,10,11)))) %>%
  mutate(factor = 'c',
         AME    = round(AME, 4),
         lower  = round(lower, 4),
         upper  = round(upper, 4))

logitModel <- stan_glm(y ~ a*b + c, data=logitData, family=binomial(link='logit'))

########## weighted logit model ##########

logitData <- tibble(
  a = rnorm(N, mean=10, sd=3),
  b = rnorm(N, mean=15, sd=5),
  c = rbinom(N, size=1, prob=.3),
  logit_y = .02 + .01*a + .02*b + .01*a*b + .05*c,
  size    = rpois(N, 100),
  y = rbinom(N, size=size, prob=exp(logit_y) / (1 + exp(logit_y)))/size
) %>%
  mutate(c = if_else(c==1, 'Y', 'N'))

summary(logitData$y)

freqModel <- glm(y ~ a*b + c, data=logitData, weights=size, family=binomial(link='logit'))
weightedLogitFreqPreds <- summary(prediction(freqModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')))) %>%
  rename(a = `at(a)`,
         b = `at(b)`,
         c = `at(c)`)

weightedLogitModel <- stan_glm(y ~ a*b + c, data=logitData, weights=size, family=binomial(link='logit'))

######### probit model #########

probitData <- tibble(
  a = rnorm(N, mean=10, sd=3),
  b = rnorm(N, mean=15, sd=5),
  c = rbinom(N, size=1, prob=.3),
  probit_y = .02 + .01*a + .02*b + .005*a*b + .02*c,
  y = rbinom(N, size=1, prob=pnorm(probit_y))
) %>%
  mutate(c = if_else(c==1, 'Y', 'N'))

summary(probitData$y)

freqModel <- glm(y ~ a*b + c, data=probitData, family=binomial(link='probit'))
probitFreqPreds <- summary(prediction(freqModel, at=list(a = c(9,10,11), b = c(13, 14, 15), c = c('Y', 'N')))) %>%
  rename(a = `at(a)`,
         b = `at(b)`,
         c = `at(c)`)

probitModel <- stan_glm(y ~ a*b + c, data=probitData, family=binomial(link='probit'))

## save the data for internal use ##

usethis::use_data(betaModel, betaFreqPreds, betaFreqMargSmall, cloglogModel, cloglogFreqPreds, gammaModel, gammaFreqPreds, poissonModel, poissonFreqPreds, poissonFreqMarg, offsetPoissonModel, offsetPoissonFreqPreds, negBinomModel, negBinomFreqPreds, gaussianModel, gaussianFreqPreds, gaussianFreqMarg, bigGaussianModel, logitModel, logitFreqPreds, logitFreqMarg, weightedLogitModel, weightedLogitFreqPreds, probitModel, probitFreqPreds, internal=T, overwrite=T)
