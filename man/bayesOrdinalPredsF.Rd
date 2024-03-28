\name{bayesOrdinalPredsF}
\alias{bayesOrdinalPredsF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian ordinal predicted probabilities.
}
\description{
Computes Bayesian predicted probabilities for ordinal models along with the HPD intervals.
}
\usage{
bayesOrdinalPredsF(model,
                   n_draws = 2000,
                   ci = 0.95,
                   digits = 2,
                   at = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A model fit by 'stan_polr' from the 'rstanarm' package.
}
\item{n_draws}{
The number of draws to take from the posterior predictive distribution.
}
\item{ci}{
The level for the HPD credible intervals.
}
\item{digits}{
The number of digits to report in the summary table.
}
\item{at}{
List of covariate values to estimate the predictions at.
}
}
\value{
Returns a summary table of the predictions.
}
\author{
David Dalenberg
}
