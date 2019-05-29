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
conf_upper(data, alpha, y_variable, time_variable, mrl, cows, amount)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  data set to be used
}
  \item{alpha}{
  alpha value to be used in calculating qnorm
}
  \item{mrl}{
  Maximum residue Limit
}
}
\details{
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
