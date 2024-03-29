% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_resources.R
\name{parse_resources}
\alias{parse_resources}
\title{Parse a Resource Specification}
\usage{
parse_resources(specs)
}
\arguments{
\item{specs}{Resource specification as a [base::formula].}
}
\value{
A list of resource specifications.
}
\description{
Parse a Resource Specification
}
\examples{
resources <- parse_resources(~ localhost())
print(resources)

resources <- parse_resources(~ !fork())
print(resources)

resources <- parse_resources(~ localhost() && !fork())
print(resources)

#resources <- parse_resources(~ ram(32*GiB))
#print(resources)

}
