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
            n_draws = 2000,
            ci = .95,
            digits = 3,
            at = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A model object of class 'stanreg'
}
\item{n_draws}{
The number of draws to take from the posterior distribution of the expectation.
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



