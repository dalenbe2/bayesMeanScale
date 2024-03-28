\name{bayesMargEffF}
\alias{bayesMargEffF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian marginal effects on the mean scale.
}
\description{
Computes bayesian marginal effects on the mean scale along with the probability of direction and HPD intervals.
}
\usage{
bayesMargEffF(model,
              n_draws = 2000,
              marginal_effect,
              start_value,
              end_value,
              ci = .95,
              digits = 3,
              at = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A model object of class 'stanreg' or 'brmsfit'
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
The level for the HPD credible interval.
}
\item{digits}{
The number of digits to report in the summary table.
}
\item{at}{
Optional list of covariate values to estimate the average marginal effect at.
}
}
\value{
Returns a list that contains the summary table, the data frame of the marginal effects, and a plot of the marginal effects (unless you specify the "at" argument).
}
\author{
David Dalenberg
}

