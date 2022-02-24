\name{plot.ldPower}
\alias{plot.ldPower}

\title{
      Plot for Group Sequential Boundaries}
\description{
      Plot of the sequential boundaries for objects of class '"ldBounds"' or '"ldPower"'. }
    \usage{
\method{plot}{ldPower}(x, scale = "z", main = NULL, xlab = NULL, ylab =
      NULL, xlim, ylim, las=1, pch=19, type="o", add=F, ...)
}

\arguments{
  \item{x}{an object of class '"ldBounds"' or '"ldPower"'.}
  \item{scale}{whether the y-axis should use the Z-value (default, "z")
  or the B-value ("b") scale.}
  \item{main}{an overall title for the plot: see \code{title}.}
  \item{xlab}{a title for the x axis: see \code{title}.}
  \item{ylab}{a title for the y axis: see \code{title}.}
  \item{xlim,ylim,las,pch,type,...}{graphical parameters passed to
    \code{plot}.}
  \item{add}{logical, whether to add to an existing plot or draw a new plot.}
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
\author{Charlie Casper \email{charlie.casper@hsc.utah.edu}, Thomas Cook \email{cook@biostat.wisc.edu}, and Oscar A. Perez}


\seealso{Generic functions \code{\link{summary.ldBounds}} and \code{\link{summary.ldPower}},
  \code{\link{ldBounds}} to calculate sequential boundaries,
  \code{\link{ldPower}} for exit probabilities given boundaries OR drift
  (effect) given power OR confidence interval OR adjusted p-value. }
\examples{
## See 'ldBounds' or 'ldPower'.

}
\keyword{misc}
\keyword{methods}
