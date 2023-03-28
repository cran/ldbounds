\name{condpower}
\alias{condpower}


\title{Conditional Power Given Interim Results}
\description{
  'condpower' determines conditional power, given interim
  results and hypothesized treatment effect.  This is commonly used to
  evaluate the futility of an ongoing trial.
}
\usage{
condpower(z.crit, z.val, accr, outcome.type, par.c, par.t=NULL, N, sigma=NULL)
}
\arguments{
  \item{z.crit}{the critical value (on the z-value scale) that will be
    used at the end of the trial to determine whether the experimental
    treatment is superior to control.} 
  \item{z.val}{the current test statistic (on the z-value scale) using
    interim data.  A positive value means that the experimental arm is
    estimated to have a more favorable outcome.}
  \item{accr}{the amount of information accrued at the time of the
    interim analysis.  For binary and continuous outcomes, this is the
    number of subjects randomized.  For a survival outcome, this is the
    number of events that have been observed.}
  \item{outcome.type}{The type of outcome: binary ("bin"), continuous
    ("mean"), or survival ("surv").}
  \item{par.c}{the hypothesized parameter value in the control
    group (when par.t is specified) or the hypothesized treatment
    effect (when par.t is missing). For a binary outcome, this is the
    proportion of 
    failures in the control group. For 
    a continuous outcome, this is the mean in the control group or the
    difference in means. For a
    survival outcome, this 
    is the hazard in the control group or the hazard ratio.}
  \item{par.t}{the hypothesized parameter value in the experimental 
    group. Leave null if par.c is the difference in means or the hazard ratio.}
  \item{N}{the total target sample size (or number of events for survival) in
    the trial.}
  \item{sigma}{the assumed standard deviation (continuous outcome).}
}
\details{
  This function calculates simple conditional power. The user specifies
  either the hypothesized parameter in each group (required for binary
  outcome) or the hypothesized treatment effect (difference for
  continuous outcome or hazard ratio for survival outcome).
}
\value{
     'condpower' returns a numeric value corresponding to the
     conditional probability.

}
\author{Charlie Casper \email{charlie.casper@hsc.utah.edu}}


\examples{
  # Binary outcome
  # Assumptions used for power calculations in design of study
  # Poor outcome 25% in control group
  # Poor outcome 15% in experimental group
  # Total sample size 900
  # Interim analysis at 300 subjects
  # Final critical value 2.0 (to account for group sequential
  # monitoring)
  # At interim, intervention doing better with z-statistic 0.067

  condpower(2.0,0.067,300,"bin",0.25,0.15,900)

}
\keyword{misc}
