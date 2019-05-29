\name{conf_upper}
\alias{conf_upper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates upper confidence limit for TTSC
%%  ~~function to do ... ~~
}
\description{
Calculates a upper Confidence limit for the TTSC given a certain dataset and alpha value
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
conf_upper(data, alpha, y_variable, time_variable, mrl, cows, amount,....)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  the dataset to be used, can also use one of the pre-installed datasets in this package
  }
  \item{cows}{
  the amount of cows in the dataset
  }
  \item{amount}{
  the amount of milkings per cow
  }
  \item{alpha}{
  the alpha value to be used for Tolerance, Confidence or Prediction upper bounds
  }
  \item{mrl}{
  Maximum Residue Limit allowed within the milk of each cow
  }
  \item{y_variable}{
  column number for the y_variable to be used in calculating the TTSC
  }
  \item{time_variable}{
  column number referencing the variable that shows the time at which each milking occured
  }
}
\details{
This function provides a upper confidence bound with level alpha for the data and provides the TTSC to accompany it.
}
\value{
}
\references{
}
\author{
Cathal Ryan
}
\note{
}


\seealso{
}
\examples{
Using pre installed dataset pred_table:
conf_upper(data = pred_table, cows = 25, amount = 8, alpha = 0.05, y_variable = 6, time_variable = 3, mrl = log(0.04))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
