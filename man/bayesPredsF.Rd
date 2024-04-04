\name{bayesPredsF}
\alias{bayesPredsF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian predictions on the mean scale.
}
\description{
Computes Bayesian predictions on the mean scale for models fit using the package "rstanarm". Predictions can be averaged over the values of the covariates in the data (average marginal predictions), or the covariates can be held at their means (marginal predictions at the means). Also, "at" values must be specified to fix certain covariates at particular values.
}
\usage{
bayesPredsF(model,
            n_draws      = 2000,
            ci           = .95,
            hdi_interval = TRUE,
            digits       = 4,
            at           = NULL,
            at_means     = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A model object of class "stanreg."
}
\item{n_draws}{
The number of draws to take from the posterior distribution of the expectation.
}
\item{ci}{
The level for the credible intervals.
}
\item{hdi_interval}{
If TRUE, the default, computes the highest posterior density credible interval. If FALSE, computes the equal-tailed interval.
}
\item{digits}{
The number of digits to report in the summary table.
}
\item{at}{
List of covariate values to estimate the predictions at.
}
\item{at_means}{
If FALSE, the default, the predictions are averaged across the rows of the model data for each unique combination of "at" values. If TRUE, the covariate values that are not specified in the "at" argument are held at their means.
}
}
\value{
Returns a summary table of the predictions.
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

# marginal predictions averaging over sample values of covariates #

bayesPredsF(m2, at=list(a = c(7, 10, 13)))

# marginal predictions holding covariates at means #

bayesPredsF(m2, at=list(a = c(7, 10, 13)), at_means=TRUE)

}


