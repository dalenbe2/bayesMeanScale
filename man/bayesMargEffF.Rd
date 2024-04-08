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
              centrality      = 'mean',
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
If TRUE, the default, computes the highest density credible interval. If FALSE, computes the equal-tailed interval.
}
\item{centrality}{
Centrality measure for the posterior distribution. Options are "mean" or "median".
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
A list of class "bayes_mean_scale_marg" with the following components: 

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

data(wells)

## Logit model ##

m1 <- stan_glm(switch ~ dist + educ + arsenic + assoc, 
               data    = wells, 
               family  = binomial, 
               refresh = 0, 
               iter    = 1000, 
               chains  = 1)

# marginal effect averaging over sample values of covariates #

bayesMargEffF(m1, 
              marginal_effect = 'arsenic', 
              start_value     = 2.2, 
              end_value       = .82)$diffTable

# marginal effect holding covariates at their means #

bayesMargEffF(m1, 
              marginal_effect = 'arsenic', 
              start_value     = 2.2, 
              end_value       = .82, 
              at_means        = TRUE)$diffTable

}
