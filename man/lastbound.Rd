\name{lastbound}
\alias{lastbound}


\title{Final Boundary Given Earlier Boundaries}
\description{
  'lastbound' determines the final boundary value, given earlier
  values.  This can be used, for example, to create Haybittle-Peto
  boundaries that have the correct overall alpha.
}
\usage{
lastbound(t, t2, alpha=0.05, sides=2, za=NULL, zb)
}
\arguments{
  \item{t}{a vector of analysis times or a number of analysis times.  If
    the number of analyses is specified, they are assumed to be equally
    spaced. The last analysis time corresponds to the boundary value
    that is being calculated.}
  \item{t2}{the second time scale, usually in terms of amount of
    accumulating information.  By default, same as the
    analysis times corresponding to \code{t}.}
  \item{alpha}{Type I error(s).  In two-sided situations, alpha can be a
    single value, indicating symmetric type I error control (half of
    alpha for each boundary).  If a vector of length two is given,
    this corresponds to the amount allocated to the lower and upper
    boundaries, respectively.  The total alpha must be greater than 0
    and less than or equal to 1.}
  \item{sides}{Designates one- or two-sided bounds.}
  \item{za}{the vector of lower boundaries, not including the final
    analysis time.}
  \item{zb}{the vector of upper boundaries, not including the final
    analysis time.}  
}
\details{
  This function calculates the final boundary value when all other
  boundaries have been specified.
}
\value{
     'lastbound' returns an object of 'class' '"ldBounds"'.

     For details about this object class, see the documentation for the
  \code{\link{ldBounds}} function.

}
\references{Reboussin, D. M., DeMets, D. L., Kim, K. M., and Lan,
  K. K. G. (2000) Computations for group sequential boundaries using the
  Lan-DeMets spending function method. \emph{Controlled Clinical Trials},
  21:190-207.

  DeMets, D. L. and Lan, K. K. G. (1995) \emph{Recent Advances in Clinical
  Trial Design and Analysis}, Thall, P. F. (ed.). Boston: Kluwer
  Academic Publishers.

  Lan, K. K. G. and DeMets, D. L. (1983) Discrete sequential boundaries
  for clinical trials. \emph{Biometrika}, 70:659-63.

}
\author{Charlie Casper \email{charlie.casper@hsc.utah.edu}}


\seealso{Generic functions \code{\link{summary.ldBounds}} and
  \code{\link{plot.ldPower}}.

  \code{\link{ldBounds}} for boundaries that use the alpha spending approach.

  \code{\link{commonbounds}} for boundaries that do not use alpha spending.
  
  \code{\link{ldPower}} for exit probabilities given boundaries OR drift
  (effect) given power OR confidence interval OR adjusted p-value.

}

\examples{

   # Haybittle-Peto boundary with 3 looks (two-sided)
   hpb <- lastbound(3,zb=c(3,3))
   summary(hpb)
   plot(hpb)

}
\keyword{misc}
