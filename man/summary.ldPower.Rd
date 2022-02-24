\name{summary.ldPower}
\alias{summary.ldPower}
\alias{print.summary.ldPower}

\title{Summary for Computations Related to Group Sequential Boundaries}
\description{
  'summary' method for class '"ldPower"'.
}
\usage{
\method{summary}{ldPower}(object, ...)
\method{print}{summary.ldPower}(x, digit = 5, ...)
}

\arguments{
  \item{object}{an object of class '"ldPower"', a result of a call to
          \code{ldPower}.}
  \item{x}{an object of class '"summary.ldPower"', a result of a
          call to \code{summary.ldPower}.}
  \item{digit}{the number of significant digits to use when printing.}
  \item{...}{further arguments passed to or from other methods.}
}
\value{
  The function 'summary.ldPower' returns a list of summary
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
\author{T. Charles Casper \email{charlie.casper@hsc.utah.edu} and Oscar A. Perez}
\seealso{\code{\link{ldBounds}} for computation of boundaries using alpha
  spending function method.  \code{\link{ldPower}} for exit probabilities given boundaries OR drift
  (effect) given power OR confidence interval OR adjusted p-value.

}
\examples{
## See function 'ldPower'
}
\keyword{misc}
\keyword{methods}
