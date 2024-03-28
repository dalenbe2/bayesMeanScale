\name{bayesPredsF}
\alias{bayesPredsF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian predictions on the mean scale.
}
\description{
Computes bayesian predictions on the mean scale along with the HPD intervals.
}
\usage{
bayesPredsF(model,
            n_draws      = 2000,
            ci           = .95,
            hdi_interval = TRUE,
            digits       = 4,
            at           = NULL,
            at_means     = F)
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



