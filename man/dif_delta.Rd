\name{dif_delta}
\alias{dif_delta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creating Tolerance interval for a vector of delta values
%%  ~~function to do ... ~~
}
\description{
Create a tolerance interval for difference delta values
}
\usage{
dif_delta(data = data, delta = seq(0.01,0.05, by = 0.01), alpha = 0.05,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  data to be used
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
This function will calculate the mrl for all values given in the vector for the parameter delta. It will then provide you with the amount of cows that would be said to be safe to milk at different milking times. If there are any cows that do have a different safe milking time then it will stop, if not it will just give the TTSC for the first delta value
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
Using pre installed dataset pred_table:
dif_delta(data = pred_table, cows = 25, amount = 8, delta = c(0.05,0.1), alpha = 0.05, y_variable = 6, time_variable = 3, mrl = log(0.04))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
