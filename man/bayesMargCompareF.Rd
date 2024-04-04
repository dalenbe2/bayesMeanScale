\name{bayesMargCompareF}
\alias{bayesMargCompareF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparison of Bayesian marginal effects 
}
\description{
Tests the differences between all marginal effects in the "bayes-marg" object that is passed to it.
}
\usage{
bayesMargCompareF(marg_list,
                  ci     = 0.95,
                  digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{marg_list}{
An object of class "bayes-marg."
}
  \item{ci}{
The level of the credible interval.
}
  \item{digits}{
The number of digits to report in the summary table.
}
}
\value{
Returns a summary table with the comparisons of marginal effects. These are sometimes called "second differences."
}
\author{
David Dalenberg
}
\examples{

library(rstanarm)
library(tibble)

logitData <- tibble(
  a = rnorm(200, mean=10, sd=3),
  b = rnorm(200, mean=15, sd=5),
  c = rbinom(200, size=1, prob=.3),
  logit_y = .02 + .01*a + -.02*b + .01*a*b + .4*c,
  y = rbinom(200, size=1, prob=exp(logit_y) / (1 + exp(logit_y)))
)

## Logit model ##

m2 <- stan_glm(y ~ a*b + c, data=logitData, family=binomial, refresh=0)

m2Marg <- bayesMargEffF(m2, marginal_effect="a", start_value=7, end_value=13, at=list(b=c(10, 15, 20)))

# compare marginal effects of 'a' between different values of 'b' #

bayesMargCompareF(m2Marg)

}
