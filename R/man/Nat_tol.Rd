\name{Nat_tol}
\alias{Nat_tol}

\title{
Tolerance upper bound for Natrella method
}
\description{
Calculates the TTSC for a tolerance upper bound using Natrella's method instead of the default method
}
\usage{
Nat_tol(cows = 20, amount = 10, data = data, alpha = 0.05, ...)
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
}
\details{
This function will give out a seperate numerical time value for each seperate cow and also the difference between each milking time using Natrella's method to calculate the tolerance upper bound instead of the default method
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Using pre installed datset pred_table:
Nat_tol(data = pred_table, cows = 25, amount = 8, alpha = 0.05, delta = 0.05, y_variable = 6, time_variable = 3, mrl = log(0.04))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
