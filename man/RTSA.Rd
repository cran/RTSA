% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RTSA.R
\name{RTSA}
\alias{RTSA}
\alias{print.RTSA}
\title{RTSA}
\usage{
RTSA(
  type = "design",
  data = NULL,
  study = NULL,
  ana_time = NULL,
  timing = NULL,
  side = NULL,
  outcome = NULL,
  mc,
  sd_mc = NULL,
  p0 = NULL,
  alpha = NULL,
  beta = NULL,
  zero_adj = 0.5,
  futility = "none",
  fixed = FALSE,
  tau2 = NULL,
  I2 = NULL,
  D2 = NULL,
  weights = "IV",
  cont_vartype = "equal",
  re_method = "DL",
  tau_ci_method = "BJ",
  es_alpha = NULL,
  es_beta = NULL,
  gamma = NULL,
  rho = NULL,
  design = NULL,
  design_R = NULL,
  conf_int = "sw",
  conf_level = 0.95,
  ...
)
}
\arguments{
\item{type}{Type of RTSA. Options are "design" or "analysis".}

\item{data}{A data.frame containing the study results. The data set must containing a specific set of columns. These are respectively `eI` (events in intervention group), `eC` (events in control group), `nC` (participants intervention group) or `nI` (participants control group) for discrete data, or, `mI` (mean intervention group), `mC` (mean control group), `sdI` (standard error intervention group), `sdC` (standard error control group),`nC` (participants intervention group) and `nI` (participants control group)  for continuous outcomes. Preferable also a `study` column as an indicator of study.}

\item{study}{An optional vector of study names and perhaps year of study. Defaults to NULL.}

\item{ana_time}{An optional vector of analysis times. Used if the sequential analysis is not done for all studies included in the meta-analysis.}

\item{timing}{Expected timings of interim analyses when type = "design". Defaults to NULL.}

\item{side}{Whether a 1- or 2-sided hypothesis test is used. Options are 1 or 2.}

\item{outcome}{Outcome metric. Options are: RR (risk ratio/relative risk), OR (odds ratio), RD (risk difference) and MD (mean difference).}

\item{mc}{Minimal clinical relevant outcome value}

\item{sd_mc}{The expected standard deviation. Used for sample size calculation for mean differences.}

\item{p0}{The expected probability of event in the control group. Used for sample size calculation for binary outcomes.}

\item{alpha}{The level of type I error}

\item{beta}{The level of type II error}

\item{zero_adj}{Zero adjustment. Options for now is 0.5.}

\item{futility}{Futility boundaries added to design. Options are: none, non-binding and binding. Default is "none".}

\item{fixed}{Should only a fixed-effect meta-analysis be computed. Default is FALSE.}

\item{tau2}{Heterogeneity estimate. Used for sample and trial size calculation. Defaults to NULL.}

\item{I2}{Inconsistency estimate. Used for sample and trial size calculation. Defaults to NULL.}

\item{D2}{Diversity estimate. Used for sample and trial size calculation. Defaults to NULL.}

\item{weights}{Weighting method options include IV (inverse-variance) and MH (Mantel-Haenszel). Defaults to IV.}

\item{cont_vartype}{For mean difference outcomes, do we expect the variance in the different groups to be "equal" or "non-equal".}

\item{re_method}{Method for calculating the estimate of heterogeneity, tau^2, and the random-effects meta-analysis variance. Options are "DL" for DerSimonian-Laird and "HKSJ" for the Harting-Knapp-Sidik-Jonkman adjustment of the DerSimonian-Laird estimator.}

\item{tau_ci_method}{Method for calculating confidence intervals for the estimated heterogeneity tau^2. Options are "QP" for Q-profiling and "BJ" for Biggelstaff ....}

\item{es_alpha}{The spending function for alpha-spending. Options are: esOF (Lan & DeMets version of O'Brien-Fleming), esPoc (Lan & DeMets version of Pocock), HSDC (Hwang Sihi and DeCani) and rho (rho family).}

\item{es_beta}{The spending function for beta-spending. For options see es_alpha.}

\item{gamma}{Parameter for the HSDC error spending function.}

\item{rho}{Parameter for the rho family error spending function.}

\item{design}{RTSA object where type is design.}

\item{design_R}{Numerical value. The fraction used to achieve correct power. Based on sequential design.}

\item{conf_int}{Stopping time confidence interval. Options for now is sw (stage-wise).}

\item{conf_level}{Confidence level on stopping time confidence interval.}

\item{...}{other arguments}
}
\value{
A RTSA object, a list of five elements:
\item{settings}{A list containing all of the settings used in the \code{RTSA} call. See Arguments.}
\item{ris}{List containing sample and trial size calculations. See documentation for \code{ris} function.}
\item{bounds}{List of stopping boundaries, timing of trials and more. See documentation for \code{boundaries} function.}
\item{results}{List of 3 to 7 elements. \code{DARIS} diversity adjusted required information size. \code{DARIS_F} fixed-effect meta-analysis required sample size. \code{AIS} Achieved information size. \code{results_df} a data.frame of inference, see documentation for \code{inference} function.  \code{seq_inf} a list of conditional inference, see documentation for \code{inference} function. \code{metaanalysis} A metaanalysis object, see documentation for \code{metaanalysis} function. \code{design_df} a data.frame containing the stopping boundaries and timings from the design.}
\item{warnings}{List of warnings}
}
\description{
RTSA
}
\examples{
data(perioOxy)
RTSA(type = "analysis", data = perioOxy, outcome = "RR", mc = 0.8, side = 2,
 alpha = 0.05, beta = 0.2, es_alpha = "esOF")
}
