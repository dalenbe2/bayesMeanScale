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
                  ci           = 0.95,
                  hdi_interval = TRUE,
                  centrality   = 'mean',
                  digits       = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{marg_list}{
An object of class "bayes_mean_scale_marg."
}
  \item{ci}{
The level of the credible interval.
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
}
\value{
Returns a summary data frame with class "bayes_mean_scale_marg_compare" with the comparisons of marginal effects. These are sometimes called "second differences."
}
\author{
David Dalenberg
}
\examples{

library(rstanarm)

data(wells)

## Logit model ##

m1 <- stan_glm(switch ~ dist + educ + arsenic + assoc, data=wells, family=binomial, refresh=0)

m1Marg <- bayesMargEffF(m1, marginal_effect='arsenic', start_value=2.2, end_value=.82, at=list(educ=c(0, 5, 10)))

# compare marginal effects of 'a' between different values of 'b' #

bayesMargCompareF(m1Marg)

}
