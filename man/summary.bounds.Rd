\name{summary.bounds}
\alias{summary.bounds}
\alias{print.summary.bounds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary for Group Sequential Boundaries}
\description{
  'summary' method for class '"bounds"'.
}
\usage{
## S3 method for class '"bounds"'
summary.bounds(object, ...)

## S3 method for class '"summary.bounds"'
print.summary.bounds(x, digit = 5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class '"bounds"', a result of a call to
          \code{bounds}.}
  \item{x}{an object of class '"summary.bounds"', a result of a
          call to \code{summary.bounds}.}
  \item{digit}{the number of significant digits to use when printing.}
  \item{...}{further arguments passed to or from other methods.}
}
\value{
     The function 'summary.bounds' returns a list of summary
     values of the group sequential boundary calculations given in
     'object'.
   }
\references{Reboussin, D. M., DeMets, D. L., Kim, K. M., and Lan,
  K. K. G. (2000) Computations for group sequential boundaries using the
  Lan-DeMets spending function method. \emph{Controlled Clinical Trials},
  21:190-207.

  Fortran program 'ld98' by the same authors as above.

  DeMets, D. L. and Lan, K. K. G. (1995) \emph{Recent Advances in Clinical
  Trial Design and Analysis}, Thall, P. F. (ed.). Boston: Kluwer
  Academic Publishers.

  Lan, K. K. G. and DeMets, D. L. (1983) Discrete sequential boundaries
  for clinical trials. \emph{Biometrika}, 70:659-63.

}
\author{Oscar A. Perez \email{perez@stat.wisc.edu}}
\seealso{\code{\link{bounds}} for computation of boundaries using alpha
  spending function method.  \code{\link{drift}} for exit probabilities given boundaries or drift
  (effect) or confidence interval given power.

}
\examples{
## See function 'bounds'

}
\keyword{misc}
\keyword{methods}
