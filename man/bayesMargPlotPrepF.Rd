\name{bayesMargPlotPrepF}
\alias{bayesMargPlotPrepF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prepare posterior distribution(s) of marginal effects for plotting.
}
\description{
Computes the probability density, HPD intervals, and central tendencies for a marginal effect computed with the "bayesMargEffF" function
}
\usage{
bayesMargPlotPrepF(x,
                   ci = 0.95,
                   groupBy = NULL)
}

\arguments{
  \item{x}{
An list produced by the "bayesMargEffF" function.
}
  \item{ci}{
Credible interval probability.
}
  \item{groupBy}{
A grouping variable, if there is one.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a data frame with the probability density, HPD intervals, and central tendencies.
}
\author{
David Dalenberg
}


