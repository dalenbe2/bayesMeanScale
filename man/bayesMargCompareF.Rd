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
Returns a data frame with the comparisons of marginal effects. These are sometimes called "second differences."
}
\author{
David Dalenberg
}

