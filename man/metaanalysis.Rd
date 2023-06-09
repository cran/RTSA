% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metaanalysis.R
\name{metaanalysis}
\alias{metaanalysis}
\alias{print.metaanalysis}
\title{Fixed-effect or random-effects meta-analysis}
\usage{
metaanalysis(
  outcome,
  data,
  study = NULL,
  side = 2,
  weights = "IV",
  cont_vartype = "equal",
  alpha = 0.05,
  beta = 0.1,
  re_method = "DL",
  tau_ci_method = "BJ",
  mc = NULL,
  sd_mc = NULL,
  zero_adj = 0.5,
  ...
)
}
\arguments{
\item{outcome}{Outcome metric for the studies. Choose between: MD (mean difference), RR (relative risk), RD (risk difference) or OR (odds ratio).}

\item{data}{A data.frame containing the study results. The data set must containing a specific set of columns. These are respectively `eI` (events in intervention group), `eC` (events in control group), `nC` (participants intervention group) or `nI` (participants control group) for discrete data, or, `mI` (mean intervention group), `mC` (mean control group), `sdI` (standard error intervention group), `sdC` (standard error control group),`nC` (participants intervention group) and `nI` (participants control group)  for continuous outcomes. Preferable also a `study` column as an indicator of study.}

\item{study}{Optional vector of study IDs. If no study indicator is provided in `data`, a vector of study indicators e.g. names.}

\item{side}{Whether a 1- or 2-sided hypothesis test is used. Options are 1 or 2. Defaults to 2.}

\item{weights}{Method for calculating weights. Options are "MH" (Mantel-Haenzel and only optional for binary data) or "IV" (Inverse variance weighting). Defaults to "IV".}

\item{cont_vartype}{Variance type for continuous outcomes. Choices are "equal" (homogeneity of treatment group variances) or "non-equal" (heterogeneity of treatment group variances). Defaults to "equal".}

\item{alpha}{Type-I-error. Defaults to 0.05.}

\item{beta}{Type-II-error. Not used unless a sample and trial size calculation is wanted. Defaults to 0.1.}

\item{re_method}{Methods are "DL" for DerSimonian-Laird or "DL_HKSJ" for DerSimonian-Laird with Hartung-Knapp-Sidik-Jonkman adjustment. Defaults to "DL".}

\item{tau_ci_method}{Methods for computation of confidence interval for heterogeneity estimate tau. Calls rma.uni from the metafor package. Options are "BJ" and "QP". Defaults to "BJ"}

\item{mc}{Minimum clinically relevant value. Used for sample and trial size calculation.}

\item{sd_mc}{The expected standard deviation. Used for sample and trial size calculation for mean differences.}

\item{zero_adj}{Zero adjustment for null events in binary data. Options for now is 0.5.}

\item{...}{Additional variables. See Details.}
}
\value{
A \code{metaanalysis} object which is a list with 6 or 7 elements.
\item{study_results}{A data.frame containing study results which is information about the individual studies}
\item{meta_results}{A data.frame containing the results of the meta-analysis such as the pooled estimate, its standard error, confidence interval and p-value}
\item{hete_results}{A list containing statistics about hetergeneity.}
\item{metaPrepare}{A list containing the elements used for calculating the study results.}
\item{synthesize}{A list containing the elements used for calculating the meta-analysis results.}
\item{settings}{A list containing the arguments used in the \code{metaanalysis} call.}
\item{ris}{(Only when \code{mc} has been specified or meta-analysis is created as part of \code{RTSA}). List of sample and trial size calculation. See documentation for \code{ris}.}
}
\description{
Computes a fixed-effect or random-effects meta-analysis including heterogeneity statistics. If \code{mc} is specified, a retrospective sample and trial size is calculated.
}
\examples{
data(perioOxy)
metaanalysis(outcome = "RR", data = perioOxy, study = perioOxy$trial)

}
