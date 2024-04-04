\name{bayesMargEffF}
\alias{bayesMargEffF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian marginal effects on the mean scale.
}
\description{
Computes Bayesian marginal effects on the mean scale for models fit using the package "rstanarm". Marginal effects can be averaged over the values of the covariates in the data (average marginal effects), or the covariates can be held at their means (marginal effects at the means). Also, "at" values can be specified to fix certain covariates at particular values. The marginal effects must be specified in terms of discrete changes. When investigating continuous variables this might be specified as the change between the mean and 1 standard deviation above the mean, for example.
}
\usage{
bayesMargEffF(model,
              n_draws         = 2000,
              marginal_effect,
              start_value,
              end_value,
              ci              = .95,
              hdi_interval    = TRUE,
              digits          = 4,
              at              = NULL,
              at_means        = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A model object of class "stanreg."
}
\item{n_draws}{
The number of draws to take from the posterior distribution of the expectation.
}
\item{marginal_effect}{
A character vector of terms that you want to estimate the marginal effect for.
}
\item{start_value}{
The start value(s) for the marginal effect(s). Must be a list when specifying multiple values.
}

\item{end_value}{
The end value(s) for the marginal effect(s). Must be a list when specifying multiple values.
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
Optional list of covariate values to estimate the marginal effect at.
}
\item{at_means}{
If FALSE, the default, the marginal effects are averaged across the rows of the data. If TRUE, the covariate values that are not specified in the "at" argument are held at their means.
}
}
\value{
A list of class "bayes.marg" with the following components: 

\item{diffTable}{
summary table of the marginal effects
}
\item{diffDraws}{
posterior draws of the marginal effects
}

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

# marginal effect averaging over sample values of covariates #

bayesMargEffF(m2, marginal_effect="a", start_value=7, end_value=13)$diffTable

# marginal effect holding covariates at their means #

bayesMargEffF(m2, marginal_effect="a", start_value=7, end_value=13, at_means=TRUE)$diffTable

}
