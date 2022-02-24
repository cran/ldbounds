\name{ldPower}
\alias{ldPower}

\title{Power and Other Probabilities for Group Sequential Boundaries}
\description{
   'ldPower' calculates drift (effect), confidence interval for drift,
   adjusted p-value, or
   power and other probabilities given drift for specified group
   sequential boundaries for interim analyses of accumulating data in
   clinical trials.
}
\usage{
ldPower(t, za=NULL, zb=NULL, t2=t, pow=NULL, drift=NULL,
      conf=NULL, method=NULL, pvaltime=NULL,
      zval=zb[length(zb)])
}

\arguments{
  \item{t}{a vector of analysis times or an 'ldBounds' object (from either
    the 'ldBounds' or 'commonbounds' function). If a vector of analysis
  times, must be increasing and in (0,1].}
  \item{za}{the vector of lower boundaries.  If not specified, made
    symmetric to \code{zb}.}
  \item{zb}{the vector of upper boundaries.}
  \item{t2}{the second time scale, usually in terms of amount of
    accumulating information.  By default, same as \code{t}.}
  \item{pow}{the desired power when drift is not specified.}
  \item{drift}{the true drift (i.e. treatment effect when t=1).  Default
  is 0 when \code{pow}, \code{conf}, and \code{method} are also left
  unspecified.} 
  \item{conf}{the confidence level when a confidence interval for drift
    is wanted.}
  \item{method}{the type of adjusted p-value desired.  Possible values are
    'SW' (stage-wise) and 'LR' (likelihood ratio).}
  \item{pvaltime}{the analysis time at which the final Z-statistic was
    observed and an adjusted p-value is desired.} 
  \item{zval}{the final observed Z-statistic (i.e. when trial is
    stopped).  Used for confidence interval or ajusted p-value.  Default
  is final upper boundary value.}
}
\details{
   This is based on a Fortran program, 'ld98', by Reboussin, DeMets, Kim,
  and Lan.  It has some advantages, like making use of probability
  distributions in R.  Only one of \code{pow}, \code{drift}, 
  \code{conf}, or \code{pval} is to be specified and \code{zval} is only
  used in the latter two
  cases.

  If \code{t} is an 'ldBounds' object, \code{za}, \code{zb}, \code{t}, and
  \code{t2} are already defined and should not be specified.
}
\value{
     'ldPower' returns an object of 'class' '"ldPower"'.

     An object of class '"ldPower"' is a list containing the following
     components:

  \item{type}{Type of computation performed: 1 is drift given power, 2
    is exit probabilities given drift, 3 is confidence interval for
    drift given final Z-statistic, and 4 is adjusted p-value given final
    Z-statistic.}
  \item{time}{the original time scale.}
  \item{time2}{the second (information) time scale.}
  \item{lower.bounds}{the vector of lower boundaries given.}
  \item{upper.bounds}{the vector of upper boundaries given.}
  \item{power}{the power.  If power is given, it is returned here.
    If drift is given, the resulting power is calculated.}
  \item{drift}{the drift.  If drift is given, it is returned here.  If
    power is given, the drift resulting in given power is calculated.}
  \item{lower.probs}{the vector of exit probabilities across the lower
    boundary.  Returned if power or drift is given.}
  \item{upper.probs}{the same for upper boundary.}
  \item{exit.probs}{the probability at each analysis of crossing the
    boundary.  The sum of \code{lower.probs} and \code{upper.probs}.}
  \item{cum.exit}{the cumulative probability of crossing.}
  \item{conf.level}{the desired confidence level, if given.}
  \item{final.zvalue}{the final Z statistic, if given.}
  \item{conf.interval}{the confidence interval for drift, if \code{conf}
    and \code{zval} are given.}
  \item{p.ordering}{the ordering specified for p-value calculation (if
    given).}
  \item{p.value}{the adjusted p-value if \code{pval}, \code{pvaltime}, and
    \code{zval} are given.}

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

\seealso{Generic functions \code{\link{summary.ldPower}} and
  \code{\link{plot.ldPower}}.

  \code{\link{ldBounds}} for computation of boundaries using alpha
  spending function method.

  \code{\link{commonbounds}} for boundaries that do not use alpha spending.
}
\examples{
   ## From Reboussin, et al. (2000)

   t <- c(0.13,0.4,0.69,0.9,0.98,1)
   upper <- c(5.3666,3.7102,2.9728,2.5365,2.2154,1.9668)
   bound.pr <- ldPower(t,zb=upper,drift=3.242)
   summary(bound.pr)

   t <- c(0.2292,0.3333,0.4375,0.5833,0.7083,0.8333)
   power.fam <- ldBounds(t,iuse=3,alpha=0.05)
   bound.ci <- ldPower(power.fam,conf=0.95,zval=2.82)
   bound.p <- ldPower(power.fam,method="LR",pvaltime=5,zval=2.82)
   summary(bound.ci)
   summary(bound.p)
   plot(bound.ci)

   obf.bd <- ldBounds(5)
   obf.dr <- ldPower(obf.bd,pow=0.9)
   summary(obf.dr)




}
\keyword{misc}
