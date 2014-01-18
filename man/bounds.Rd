\name{bounds}
\alias{bounds}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{Group Sequential Boundaries Using Spending Functions}
\description{
  'bounds' determines group sequential boundaries for interim analyses
  of accumulating data in clinical trials using the Lan-DeMets alpha
  spending function method.  These can be used as guidelines for early
  stopping of the trial.
}
\usage{
bounds(t, t2 = t, iuse = 1, asf = NULL, alpha = 0.05,
       phi = rep(1, length(alpha)), ztrun = rep(8, length(alpha)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{the vector of analysis times, which must be increasing and in
    (0,1].}
  \item{t2}{the second time scale, usually in terms of amount of
    accumulating information.  By default, same as \code{t}.}
  \item{iuse}{a vector of the type of alpha spending function(s) to
    use for lower and upper bounds, respectively (in the two-sided
    case).  Details of specification are given below.}
  \item{asf}{a list of one or two functions to be used as alpha spending
  function(s).  Used with \code{iuse}=5 (See below).}
  \item{alpha}{a vector of type I errors.  In two-sided situations,
    these correspond to the amount allocated to the lower and upper
    boundaries, respectively.  The total alpha must be greater than 0
    and less than or equal to 1.}
  \item{phi}{a vector of values used when \code{iuse}=3 or 4 (See
    below).}
  \item{ztrun}{a vector of values specifying where to truncate lower and
  upper boundaries, respectively.  Default is c(-8,8) (or just 8 for
  one-sided), which is essentially no truncation.}
}
\details{
  This is based on a Fortran program, 'ld98', by Reboussin, DeMets, Kim,
  and Lan.  It has some advantages, like making use of probability
  distributions in R and the ability to specify any valid spending
  function without changing the program.

  \code{iuse} values of 1 and 2 correspond to alpha spending functions
  which give O'Brien Fleming and Pocock type boundaries, respectively.
  A value of 3 is the power family.  Here, the spending function is
  \eqn{\alpha t^{\phi}}, where \eqn{\phi} must be greater than 0.  A value of 4 is the
  Hwang-Shih-DeCani family, with spending function
  \eqn{\alpha (1-e^{-\phi t})/(1-e^{-\phi})}, where \eqn{phi} cannot be 0.

  \code{iuse} and \code{alpha} must have the same length.  If
  \code{alpha} has length 2 and \code{phi} has length 1, the same value
  of \code{phi} will be used for the upper and the lower boundaries.

  With \code{iuse}=5, the user will specify any alpha spending function
  as \code{asf}.  Such a function asf() must be of class 'function' and
  must satisfy asf(0)=0 and asf(1)=1 and must be strictly increasing.
  Currently, this option cannot be used for one side of the boundary
  with one of the other options for the other side.  In other words, the
  user may define one spending function for a one-sided boundary or two
  for a two-sided boundary, symmetric or asymmetric, but cannot define
  one spending function and select the other from \code{iuse} 1 through
  4.
}
\value{
     'bounds' returns an object of 'class' '"bounds"'.

     An object of class '"bounds"' is a list containing the following
     components:
  \item{bounds.type}{the type of bounds: 1 is 'one-sided', 2 is
    'two-sided symmetric', and 3 is 'two-sided asymmetric'.}
  \item{spending.type}{the type(s) of spending function.  A descriptive
    version of the value(s) used for \code{iuse}}
  \item{time}{the original time scale.}
  \item{time2}{the second (information) time scale.}
  \item{alpha}{the alpha(s) used.}
  \item{overall.alpha}{if two-sided, the sum of the two alphas.  If
    one-sided, just alpha.}
  \item{lower.bounds}{the vector of lower boundaries calculated.  Should be
    ignored if one-sided.}
  \item{upper.bounds}{the vector of upper boundaries calculated.}
  \item{exit.pr}{the vector of cumulative exit probabilities at each analysis.}
  \item{diff.pr}{the vector of exit probabilities accumulated at each analysis.}

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


\seealso{Generic functions \code{\link{summary.bounds}} and
  \code{\link{plot.bounds}}.

  \code{\link{drift}} for exit probabilities given boundaries or drift
  (effect) or confidence interval given power.
}

\examples{
   ## From Reboussin, et al. (2000)

   t <- seq(0.2,1,length=5)
   obf.bd <- bounds(t,iuse=c(1,1),alpha=c(0.025,0.025))
   summary(obf.bd)
   plot(obf.bd)

   t <- c(0.2292,0.3333,0.4375,0.5833,0.7083,0.8333)
   t2 <- c(56,77,126,177,247,318)
   power.fam <- bounds(t,t2,iuse=c(3,3),alpha=c(0.025,0.025))
   summary(power.fam)

}
\keyword{misc}
