## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(RTSA)

## -----------------------------------------------------------------------------
ris(outcome = "RR", mc = 0.9, pC = 0.1, alpha = 0.05, beta = 0.1, side = 2)

## -----------------------------------------------------------------------------
ris(outcome = "RR", mc = 0.9, pC = 0.2, fixed = FALSE, I2 = 0.2, D2 = 0.3,
    side = 2, alpha = 0.05, beta = 0.2)

## ---- eval = FALSE, include=FALSE---------------------------------------------
#  # simulate 10 trials
#  tau2 = 0.05
#  outl = matrix(NA, ncol = 3, nrow = 10)
#  outm = matrix(NA, ncol = 4, nrow = 10)
#  
#  RR <- 0.9
#  pC <- 0.1
#  pI <- round(exp(log(pC) + log(RR) / 2), 4)
#  pC <- round(exp(log(pC) - log(RR) / 2), 4)
#  theta = round(pC - pI, 4)
#  n = 2500
#  K = 10
#  nsim = 1000
#  
#  for(m in 1:10) {
#    outpvalue = matrix(NA, ncol = 3, nrow = nsim)
#    outhetero = matrix(NA, ncol = 5, nrow = nsim)
#  
#    for (h in 1:nsim) {
#      ln_RR = rnorm(K, mean = log(RR), sd = sqrt(tau2))
#  
#      pI = exp(log(pC) + ln_RR)
#      pI[pI < 0.01] = 0.01
#      pI[pI > 0.99] = 0.99
#      pC = rep(exp(log(pC)),K)
#      pC[pC < 0.01] = 0.01
#      pC[pC > 0.99] = 0.99
#      outmat = matrix(NA, ncol = 4, nrow = K)
#      zvalues = NULL
#      for (i in 1:K) {
#        eA <- apply(cbind(n / 2, pI[i]), 1,
#                    function(x)
#                      rbinom(1, size = x[1], prob = x[2]))
#        eB <- apply(cbind(n / 2, pC[i]), 1,
#                    function(x)
#                      rbinom(1, size = x[1], prob = x[2]))
#        outmat[i, 1:4] = c(eA, n / 2, eB, n / 2)
#      }
#  
#      dat <- data.frame(eI = outmat[, 1],
#        nI = outmat[, 2],
#        eC = outmat[, 3],
#        nC = outmat[, 2])
#  
#      synout = RTSA:::metaPrepare(
#        outcome = "RR", data = dat,
#        weights = "MH", alpha = 0.05
#      )
#      out1 = RTSA:::synthesize(synout, tau_ci_method = "BJ",
#                        re_method = "DL")
#  
#      ma <- metaanalysis(outcome = "RR", data = dat, mc = 0.9)
#  
#          # save the tau^2, I^2 and D^2
#      hetero = c(out1$U[c(1, 3, 4)])
#      #RISd2 = 1 / (1 - hetero[3]) * ma$ris$full_NF
#      #outhetero[h,] = c(hetero, ma$ris$NR_inc_full, ma$ris$NR_div_full)
#      dRISd2 <- ma$ris$NR_div
#      if(is.null(dRISd2)){
#        dRISd2 <- ma$ris$NF
#      }
#  
#      ln_RR = rnorm(m, mean = log(RR), sd = sqrt(tau2))
#      pI = exp(log(pC) + ln_RR)
#      pI[pI < 0.01] = 0.01
#      pI[pI > 0.99] = 0.99
#      pC = rep(exp(log(pC)),m)
#      pC[pC < 0.01] = 0.01
#      pC[pC > 0.99] = 0.99
#  
#      for (b in 1:m) {
#        eA <- apply(cbind(ceiling(dRISd2 / 2 / m), pI[b]), 1,
#                    function(x)
#                      rbinom(1, size = x[1], prob = x[2]))
#        eB <- apply(cbind(ceiling(dRISd2 / 2 / m), pC[b]), 1,
#                    function(x)
#                      rbinom(1, size = x[1], prob = x[2]))
#        outmat = rbind(outmat, c(eA, dRISd2 / 2 / m, eB, dRISd2 / 2 / m))
#      }
#  
#      dat <- data.frame(eI = outmat[, 1],
#        nI = outmat[, 2],
#        eC = outmat[, 3],
#        nC = outmat[, 2])
#  
#      synout = RTSA:::metaPrepare(
#        outcome = "RR",
#        weights = "MH", data = dat, alpha = 0.05
#      )
#  
#      out1 = RTSA:::synthesize(synout, re_method = "DL", tau_ci_method = "BJ")
#      out2 = RTSA:::synthesize(synout, re_method = "DL_HKSJ", tau_ci_method = "BJ")
#  
#      if(is.null(out1$peR[5])) out1$peR[5] <- out1$peF[5]
#      if(is.null(out2$peR[5])) out2$peR[5] <- out1$peF[5]
#  
#      outpvalue[h, c(1, 2, 3)] = c(round(out1$peF[5], 4), round(out1$peR[5], 4), out2$peR[5])
#    }
#    outm[m,] = c(
#      sum(outpvalue[, 1] < 0.05) / nsim,
#      sum(outpvalue[, 2] < 0.05) / nsim,
#      sum(outpvalue[, 3] < 0.05) / nsim,
#      mean(dRISd2)
#    )
#  }
#  
#  outm = cbind(1:10, outm)
#  colnames(outm) = c(
#    "Number of extra trials",
#    "Fixed-effect",
#    "Random-effects DL",
#    "Random-effects HKSJ",
#    "Avg. RIS"
#  )
#  rownames(outm) = rep(c(""), 10)
#  save(outm, file = "random-effects-TSA.Rda")

## ----randomTSA, echo = FALSE--------------------------------------------------
load("random-effects-TSA.Rda")
knitr::kable(outm[,-5], caption = "Power per model as a function of number of extra trials and RIS based on Diversity")

## -----------------------------------------------------------------------------
ris(outcome = "RR", mc = 0.9, fixed = FALSE, tau2 = 0.05, pC = 0.1,
    side = 2, alpha = 0.05, beta = 0.2)

## ---- eval = FALSE, echo = FALSE----------------------------------------------
#  outl = matrix(NA, ncol = 3, nrow = 4)
#  
#  RR <- 0.9
#  pC <- 0.1
#  pI <- round(exp(log(pC)+log(RR)/2),4)
#  pC <- round(exp(log(pC)-log(RR)/2),4)
#  theta = round(pC - pI,4)
#  nsim = 10000
#  tau2 = 0.05
#  
#  trial.out = minTrial(outcome = "RR", mc = 0.9, tau2 = 0.05, pC = 0.1)
#  
#  outtau = numeric(nsim)
#  outpvalue = matrix(NA, ncol = 3, nrow = nsim)
#  
#  for(l in 1:dim(trial.out$nPax)[2]){
#  K = trial.out$nPax[1,l]
#  n = trial.out$nPax[2,l]
#  
#    for(h in 1:nsim){
#      outmat = matrix(NA,ncol = 4, nrow = K)
#  
#      ln_RR = rnorm(K, mean = log(RR), sd = sqrt(tau2))
#      pI = exp(log(pC)+ln_RR/2)
#      pI[pI < 0.01] = 0.01
#      pI[pI > 0.99] = 0.99
#      pC = exp(log(pC)-ln_RR/2)
#      pC[pC < 0.01] = 0.01
#      pC[pC > 0.99] = 0.99
#      for(i in 1:(K)){
#        eA <- apply(cbind(ceiling(n/2), pI[i]), 1,
#                    function(x) rbinom(1, size = x[1], prob = x[2]))
#        eB <- apply(cbind(ceiling(n/2), pC[i]), 1,
#                    function(x) rbinom(1, size = x[1], prob = x[2]))
#        outmat[i,1:4] = c(eA, ceiling(n/2), eB, ceiling(n/2))
#      }
#  
#      dat <- data.frame(eI = outmat[,1], nI = outmat[,2],
#                            eC = outmat[,3], nC = outmat[,2])
#  
#      synout = RTSA:::metaPrepare(outcome = "RR", data = dat,
#                            weights = "IV", alpha = 0.05)
#      out1 = RTSA:::synthesize(synout, re_method = "DL", tau_ci_method = "BJ")
#      out2 = RTSA:::synthesize(synout, re_method = "DL_HKSJ", tau_ci_method = "BJ")
#      outpvalue[h, c(1,2,3)] = c(round(out1$peF[5],4), round(out1$peR[5],4), out2$peR[5])
#    }
#    outl[l,1] = sum(outpvalue[,1] <= 0.05)/nsim
#    outl[l,2] = sum(outpvalue[,2] <= 0.05)/nsim
#    outl[l,3] = sum(outpvalue[,3] <= 0.05)/nsim
#  }
#  
#  outl = cbind(outl, trial.out$nPax[1,], trial.out$nPax[2,])
#  colnames(outl) = c("Fixed-effect", "Random-effects DL", "Random-effects HKSJ",
#                     "Number of trials", "Participants per trial")
#  rownames(outl) = c("","","","")
#  save(outl, file = "random-effects.Rda")

## ----random, echo = FALSE-----------------------------------------------------
load("random-effects.Rda")
knitr::kable(outl[,c(4,5,1,2,3)], caption = "Power per model as a function of number of trials and number of participants per trial")

## -----------------------------------------------------------------------------
ma <- metaanalysis(data = perioOxy, outcome = "RR", mc = 0.8, beta = 0.1,
                   pC = 0.15)
ma$ris

## -----------------------------------------------------------------------------
ma$ris$NR_tau$NR_tau_ll

## -----------------------------------------------------------------------------
ma$ris$NR_tau$NR_tau_ul

