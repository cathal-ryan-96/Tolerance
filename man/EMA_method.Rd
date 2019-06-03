\name{EMA_method}
\alias{EMA_method}
\title{
EMA method for calculating Tolerance interval
}
\description{
Calculating TTSC by the use of Tolerance upper bounds with the help of EMA method
}
\usage{
EMA_method(data = data, LOQ = 0.02, alpha = 0.05, ...)
}
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
  \item{y_variable}{
  column number for the y_variable to be used in calculating the TTSC
  }
  \item{time_variable}{
  column number referencing the variable that shows the time at which each milking occured
  }
  \item{LOQ}{
  Value at which each milking is censored at if it reaches that point
  }
}
\details{
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

\examples{
Only tested using pred_table:
EMA_method(data = data, LOQ = 0.02, alpha = 0.05, delta = 0.05, cows = 25, amount = 8, y_variable = 5, time_variable = 3)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }