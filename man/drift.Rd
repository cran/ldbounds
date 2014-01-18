\name{drift}
\alias{drift}

\title{Drift and Probabilities for Group Sequential Boundaries}
\description{
   'drift' calculates drift (effect), confidence interval for drift, or
   power and other probabilities given drift for specified group
   sequential boundaries for interim analyses of accumulating data in
   clinical trials.
}
\usage{
drift(za = -zb, zb, t, t2 = t, pow = NULL, drft = NULL,
      conf = NULL, zval = zb[length(zb)])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{za}{the vector of lower boundaries.  Symmetric to \code{zb} by default.}
  \item{zb}{the vector of upper boundaries.}
  \item{t}{the vector of analysis times, which must be increasing and in
    (0,1].}
  \item{t2}{the second time scale, usually in terms of amount of
    accumulating information.  By default, same as \code{t}.}
  \item{pow}{the desired power when drift is not specified.}
  \item{drft}{the true drift (i.e. treatment effect when t=1).}
  \item{conf}{the confidence level when a confidence interval for drift
    is wanted.}
  \item{zval}{the final observed Z statistic (i.e. when trial is
    stopped).  Used for confidence interval.}
}
\details{
   This is based on a Fortran program, 'ld98', by Reboussin, DeMets, Kim,
  and Lan.  It has some advantages, like making use of probability
  distributions in R.  Only one of \code{pow}, \code{drft}, and
  \code{conf} is to be specified and \code{zval} is only used in the last
  case.
}
\value{
     'drift' returns an object of 'class' '"drift"'.

     An object of class '"drift"' is a list containing the following
     components:

  \item{type}{Type of computation performed: 1 is drift given power, 2
    is exit probabilities given drift, and 3 is confidence interval for
    drift given final Z statistic.}
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

\seealso{Generic functions \code{\link{summary.drift}} and
  \code{\link{plot.drift}}.

  \code{\link{bounds}} for computation of boundaries using alpha
  spending function method.
}
\examples{
   ## From Reboussin, et al. (2000)

   t <- c(0.13,0.4,0.69,0.9,0.98,1)
   upper <- c(5.3666,3.7102,2.9728,2.5365,2.2154,1.9668)
   drift.pr <- drift(zb=upper,t=t,drft=3.242)
   summary(drift.pr)

   t <- c(0.2292,0.3333,0.4375,0.5833,0.7083,0.8333)
   upper <- c(2.53,2.61,2.57,2.47,2.43,2.38)
   drift.ci <- drift(zb=upper,t=t,conf=0.95,zval=2.82)
   summary(drift.ci)
   plot(drift.ci)

   ## Using output from 'bounds'
   t <- seq(0.2,1,length=5)
   obf.bd <- bounds(t,iuse=c(1,1),alpha=c(0.025,0.025))
   drift.dr <- drift(obf.bd$lower.bounds,obf.bd$upper.bounds,t,pow=0.9)
   summary(drift.dr)




}
\keyword{misc}
