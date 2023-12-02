\name{ldBounds}
\alias{ldBounds}


\title{Group Sequential Boundaries Using Spending Functions}
\description{
  'ldBounds' determines group sequential boundaries for interim analyses
  of accumulating data in clinical trials using the Lan-DeMets alpha
  spending function method.  These can be used as guidelines for early
  stopping of the trial.
}
\usage{
ldBounds(t, t2, iuse=1, asf=NULL, alpha=0.05, phi=rep(1,length(alpha)),
       sides=2, ztrun=rep(Inf,length(alpha)))
}
\arguments{
  \item{t}{a vector of analysis times or a number of analysis times.  If
    the number of analyses is specified, they are assumed to be equally
    spaced. Analysis times must be increasing and in
    (0,1].}
  \item{t2}{the second time scale, usually in terms of amount of
    accumulating information.  By default, same as \code{t} or the
    analysis times corresponding to \code{t} looks.}
  \item{iuse}{the type of alpha spending function(s) to use for bounds.
    If two-sided bounds are calculated and \code{iuse} is a single
    value, the same type will be used for each of the two boundaries.
    If a vector of length two is given, the two values will be used for
    lower and upper bounds, respectively.  Details of specification are given below.}
  \item{asf}{a list of one or two functions to be used as alpha spending
  function(s).  Used with \code{iuse}=5 (See below).}
  \item{alpha}{Type I error(s).  In two-sided situations, alpha can be a
    single value, indicating symmetric type I error control (half of
    alpha for each boundary).  If a vector of length two is given,
    this corresponds to the amount allocated to the lower and upper
    boundaries, respectively.  The total alpha must be greater than 0
    and less than or equal to 1.  When \code{iuse}=5, any alpha
    specified is ignored (see details).}
  \item{phi}{a vector of values used when \code{iuse}=3 or 4 (See
    below).}
  \item{sides}{Designates one- or two-sided bounds.}
  \item{ztrun}{a vector of values specifying where to truncate lower and
  upper boundaries, respectively.  Default is no truncation.}
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
  \eqn{\alpha (1-e^{-\phi t})/(1-e^{-\phi})}, where \eqn{\phi} cannot be 0.

  With \code{iuse}=5, the user will specify any alpha spending function
  as \code{asf}.  Such a function asf() must be of class 'function',
  satisfy asf(t)\eqn{\leq}1 for t in (0,1), and be strictly increasing. Alpha
  will be derived as asf(1).  If two spending functions are specified,
  they represent the lower and upper boundary spending functions,
  respectively, with the values at time 1 designating the lower and
  upper alphas, respectively.  
  Currently, this option cannot be used for one side of the boundary
  with one of the other options for the other side.  In other words, the
  user may define one spending function for a one-sided boundary or two
  for a two-sided boundary, symmetric or asymmetric, but cannot define
  one spending function and select the other from \code{iuse} 1 through
  4.
}
\value{
     'ldBounds' returns an object of 'class' '"ldBounds"'.

     An object of class '"ldBounds"' is a list containing the following
     components:
  \item{bounds.type}{the type of bounds: 1 is 'one-sided', 2 is
    'two-sided symmetric', and 3 is 'two-sided asymmetric'. For
     non-alpha-spending function boundaries (see 'commonbounds'
     function) 4, 5, and 6 correspond, respectively, to types 1 through
     3 above.}
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
  \item{diff.pr}{the vector of exit probabilities accumulated at each
    analysis.}
  \item{nom.alpha}{the nominal alpha level to be used at each analysis
     (i.e., bound has been crossed if the p-value at that time is less
     than this value).  Not given for two-sided asymmetric boundaries.}

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


\seealso{Generic functions \code{\link{summary.ldBounds}} and
  \code{\link{plot.ldPower}}.

  \code{\link{ldPower}} for exit probabilities given boundaries OR drift
  (effect) given power OR confidence interval OR adjusted p-value.

  \code{\link{commonbounds}} for some commonly used boundaries that do not use alpha spending.
}

\examples{
   ## From Reboussin, et al. (2000)

   #t <- seq(0.2,1,length=5)
   #obf.bd <- ldBounds(t)
   #summary(obf.bd)
   #plot(obf.bd)

   # Equivalent bounds to above
   obf.bd2 <- ldBounds(5)

   t <- c(0.2292,0.3333,0.4375,0.5833,0.7083,0.8333)
   t2 <- c(56,77,126,177,247,318)
   power.fam <- ldBounds(t,t2,iuse=3,alpha=0.05)
   summary(power.fam)

}
\keyword{misc}
