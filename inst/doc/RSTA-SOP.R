## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(RTSA)
packageVersion("RTSA")

## ---- eval = FALSE------------------------------------------------------------
#  RTSA(alpha = 0.05, ...)

## ---- eval = FALSE------------------------------------------------------------
#  RTSA(beta = 0.1, ...)

## ---- eval = FALSE------------------------------------------------------------
#  RTSA(fixed = FALSE, tau2 = 0.05, ...) # adjusted by a specific size of heterogeneity

## ---- eval = FALSE------------------------------------------------------------
#  RTSA(fixed = FALSE, D2 = 0.5, ...) # adjusted by diversity

## ---- eval = FALSE------------------------------------------------------------
#  RTSA(pC = 0.1, ...)

## ---- eval = FALSE------------------------------------------------------------
#  RTSA(RRR = 0.2, ...)
#  RTSA(mc = 0.8, ...)

## ---- eval = FALSE------------------------------------------------------------
#  RTSA(zero_adj = 0.5, ...)

## ---- eval = FALSE------------------------------------------------------------
#  RTSA(mc = 5, ...)

## ---- eval = FALSE------------------------------------------------------------
#  RTSA(sd_mc = X, ...)

## ---- eval = FALSE------------------------------------------------------------
#  RTSA(fixed = FALSE, re_method = "DL") # for DerSimonian-Laird.
#  RTSA(fixed = FALSE, re_method = "DL_HKSJ") # for the Hartung-Knapp-Sidik-Jonkman adjustment of DerSimonian-Laird.

