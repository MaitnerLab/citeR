% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_dependencies.R
\name{get_dependencies}
\alias{get_dependencies}
\title{Get information on dependencies used}
\usage{
get_dependencies(packages_used_df)
}
\arguments{
\item{...}{Additional arguments passed to internal functions.}
}
\value{
Dataframe containing bibtex-formatted citation information
}
\description{
Returns information on dependencies used by R
}
\examples{
{
deps <- get_dependencies()
}

}
\keyword{internal}
