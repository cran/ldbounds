\name{plot.bounds}
\alias{plot.drift}
\alias{plot.bounds}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{
      Plot for Group Sequential Boundaries}
\description{
      Plot of the sequential boundaries for objects of class '"bounds"' or '"drift"'. }
    \usage{
\method{plot}{bounds}(x, main = NULL, xlab = NULL, ylab = NULL, ...)
\method{plot}{drift}(x, main = NULL, xlab = NULL, ylab = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class '"bounds"' or '"drift"'.}
  \item{main}{an overall title for the plot: see \code{title}.}
  \item{xlab}{a title for the x axis: see \code{title}.}
  \item{ylab}{a title for the y axis: see \code{title}.}
  \item{...}{graphical parameters can be given as arguments to \code{plot}.}
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
\author{Charlie Casper \email{charlie.casper@hsc.utah.edu} and Oscar A. Perez}


\seealso{Generic functions \code{\link{summary.bounds}} and \code{\link{summary.drift}},
  \code{\link{bounds}} to calculate sequential boundaries,
  \code{\link{drift}} for exit probabilities given boundaries or drift (effect) or confidence interval given power. }
\examples{
## See 'bounds' or 'drift'.

}
\keyword{misc}
\keyword{methods}
