\name{commonbounds}
\alias{commonbounds}


\title{Commonly Used Group Sequential Boundaries}
\description{
  'commonbounds' determines group sequential boundaries of the well-known
  O'Brien-Fleming and Pocock types.  These can be used as guidelines for early
  stopping of the trial.  For Haybittle-Peto type boundaries, see function
  'lastbound'.  Note: these are NOT the alpha-spending
  versions of these boundaries.
}
\usage{
commonbounds(looks, t=(1:looks)/looks, t2=t, iuse="OF", alpha=0.05, sides=2)
}
\arguments{
  \item{looks}{A number of equally spaced analysis times.}
  \item{t}{a vector of analysis times, if \code{looks} is not provided.
    If times are provided and they are not equally spaced, there will be
    a warning.} 
  \item{t2}{the second time scale, usually in terms of amount of
    accumulating information.  By default, same as the
    equally spaced analysis times or the analysis times corresponding to
    \code{t}.} 
  \item{iuse}{the type of bounds: O'Brien-Fleming ("OF") or Pocock ("PK")
    If two-sided bounds are calculated, the same type will be used for
    each of the two boundaries. 
    If a vector of length two is given, the two values will be used for
    lower and upper bounds, respectively.  Details of specification are
    given below.} 
  \item{alpha}{Type I error(s).  In two-sided situations, alpha can be a
    single value, indicating symmetric type I error control (half of
    alpha for each boundary).  If a vector of length two is given,
    this corresponds to the amount allocated to the lower and upper
    boundaries, respectively.  The total alpha must be greater than 0
    and less than or equal to 1.}
  \item{sides}{Designates one- or two-sided bounds.}
}
\details{
  This function calculates boundaries corresponding to traditional
  O'Brien-Fleming or Pocock boundaries.  Note that these are not the
  alpha spending function versions of the boundaries.  For those, use \code{\link{ldBounds}}.
}
\value{
     'commonbounds' returns an object of 'class' '"ldBounds"'.

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
  
  \code{\link{ldPower}} for exit probabilities given boundaries OR drift
  (effect) given power OR confidence interval OR adjusted p-value.

}

\examples{
   ## From Reboussin, et al. (2000)

   time <- seq(0.2,1,length=5)
   obf.bd <- commonbounds(t=time)
   summary(obf.bd)
   plot(obf.bd)

   # Equivalent bounds to above
   obf.bd2 <- commonbounds(5)


}
\keyword{misc}
