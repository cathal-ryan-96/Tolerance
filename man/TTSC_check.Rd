\name{TTSC_check}
\alias{TTSC_check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parent function of package Tolerance
%%  ~~function to do ... ~~
}
\description{
This function can be used to provide results from all other functions within Tolerance
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TTSC_check(data, cows, amount, alpha, delta, y_variable, time_variable, mrl)
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
  \item{delta}{
  the delta value to be used for the two different types of Tolerance intervals
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
  \item{Natrella_tolerance_instead}{
  (optional) Defaults to FALSE. If set to TRUE, tolerance upper bound is calculated using Natrella's method instead
  }
  \item{PredAswell}{
  (optional) Defaults to FALSE. If set to TRUE, a prediction upper bound will also be provided
  }
  \item{ConfidenceAswell}{
  (optional) Defaults to FALSE. If set to TRUE, a confidence upper bound will also be provided
  }
}
\details{
This function will give out a seperate numerical time for each seperate cow and also the difference between each milking time
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Cathal Ryan
}
\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Using pre installed dataset pred_table:
TTSC_check(data = pred_table, cows = 25, amount = 8,
    alpha = 0.05, delta = 0.05, y_variable = 6,
    time_variable = 3, mrl = log(0.04))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
