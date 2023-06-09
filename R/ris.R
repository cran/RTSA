#' minTrial
#'
#' Calculate minimum number of trials for wanted power in a meta-analysis with
#' heterogeneity
#'
#' @param outcome Metric of interest, options include only risk ratio (RR) for now
#' @param mc Minimal clinical relevant value.
#' @param tau2 Heterogeneity estimate
#' @param alpha Type I error
#' @param beta Type II error
#' @param p0 Probability of event in control group
#' @param p1 Probability of event in treatment group
#' @param side Whether a 1- or 2-sided hypothesis test is used. Options are 1 or 2.
#' @param var_mc Variance of the estimated effect
#' @param var_random Estimated variance from the random-effects meta-analysis
#' @param trials optional number of trials.
#'
#' @return Either a number (minimum required trials) or the minimum required
#' required trials together with a matrix of required participants per trial given
#' different number of trials.
#' @export
#'
#' @importFrom stats uniroot qnorm
#'
#' @examples
#' minTrial(outcome = "RR", p0 = 0.5, mc = 0.7, tau2 = 0.05)
#'
minTrial = function(outcome,
                    mc,
                    tau2,
                    alpha = 0.05,
                    beta = 0.2,
                    side = 2,
                    p0 = NULL,
                    p1 = NULL,
                    var_mc = NULL,
                    var_random = NULL,
                    trials = NULL) {
  if (outcome %in% c("RR", "OR")) {
    mc = log(mc)
  }

  fixed <- FALSE
  war_het <- NULL

  if(!is.null(var_random)){
    ntrial_random = function(mc, trial, alpha, beta, tau2, var_random) {
      trial / ((qnorm(1 - alpha / side) + qnorm(1 - beta)) ^ 2/mc^2-1/var_random) - tau2
    }

    minTrial <- try(ceiling(uniroot(
      function(trial)
        ntrial_random(mc, trial, alpha, beta, tau2, var_random),
      interval = c(0, 1000)
    )$root), TRUE)
    if(inherits(minTrial,"try-error")){
      fixed <- TRUE
      war_het <- c("Too low est. heterogeneity to calculate RIS based on tau^2.")
    }
  } else {
    ntrial = function(mc, trial, alpha, beta, tau2) {
      trial / ((qnorm(1 - alpha / side) + qnorm(1 - beta)) ^ 2/mc^2) - tau2
    }

    minTrial <- ceiling(uniroot(
      function(trial)
        ntrial(mc, trial, alpha, beta, tau2),
      interval = c(0, 1000)
    )$root)
  }

  if (outcome == "RR") {
    pI <- exp(log(p0) + mc / 2)
    pC <- exp(log(p0) - mc / 2)
    var_mc <- 1 / pC + 1 / pI - 2
  } else if (outcome == "OR") {
    logit <- function(x)
      log(x / (1 - x))
    invlogit <- function(x)
      1 / (1 + exp(-x))

    pI <- invlogit(logit(p0) + mc / 2)
    pC <- invlogit(logit(p0) - mc / 2)
    var_mc <- 1 / pI + 1 / pC + 1 / (1 - pI) + 1 / (1 - pC)
  } else if(outcome == "RD"){
    var_mc <- p0*(1-p0)+p1*(1-p1)
  }

  if(fixed == FALSE){
    out.mat = matrix(NA, ncol = 4, nrow = 3)
    out.mat[1, ] <- c(minTrial, minTrial + 1, minTrial + 2, minTrial + 3)
    if(is.null(var_random)){
      out.mat[2, ] <- ceiling(2 * var_mc / (
        mc ^ 2 * c(minTrial, minTrial + 1, minTrial + 2, minTrial + 3) / ((qnorm(1 -
                                                                                   alpha / side) + qnorm(1 - beta)) ^ 2) - tau2
      ))} else {
        out.mat[2, ] <- ceiling(2 * var_mc / (
          c(minTrial, minTrial + 1, minTrial + 2, minTrial + 3) / ((qnorm(1 -alpha / side) + qnorm(1 - beta)) ^ 2/mc ^ 2 -1/var_random) - tau2
        ))
      }
    out.mat[3, ] <- out.mat[1, ] * out.mat[2, ]
    row.names(out.mat) <- c("Trials", "Pax per trial", "Total nr of pax")

    if(!is.null(trials)){
      if(trials < minTrial){
        warning("The number of trials provided is less than the required number of trials")
      } else {
        if(is.null(var_random)){
          outTrials <- ceiling(2 * var_mc / (
            mc ^ 2 * trials / ((qnorm(1 - alpha / side) + qnorm(1 - beta)) ^ 2) - tau2
          ))} else {
            outTrials <- ceiling(2 * var_mc / ( trials / ((qnorm(1 -alpha / side) + qnorm(1 - beta)) ^ 2/mc ^ 2 -1/var_random) - tau2
            ))
          }

        out.mat <- cbind(out.mat, c(trials, outTrials, trials*outTrials
        ))
      }
    }

    return(list(minTrial = minTrial, nPax = out.mat, war_het = war_het))
  } else {
    return(list(war_het = war_het))
  }
}


#' Calculate required sample and trials size.
#'
#' @param outcome Choose between: "MD" (mean difference), "RR" (relative risk), "OR" (odds ratio) or "RD" (risk difference).
#' @param mc Minimum clinical relevant effect. For "OR" or "RR" set to natural scale, not log scale.
#' @param side Test type. Set to 1 or 2 depending on the test being 1- or 2-sided.
#' @param alpha Level of type-I-error. Default value is 0.05.
#' @param beta Level of type-II-error. Default value is 0.2.
#' @param fixed Should sample size be based on a fixed- or random-effects model. Defaults to TRUE.
#' @param sd_mc Standard deviation of estimated effect. Only needed when outcome = "MD".
#' @param p0 Probability of event in control group.
#' @param p1 Probability of event in treatment group.
#' @param ma An optional \code{metaanalysis} object. 
#' @param tau2 The value of the heterogeneity. Use when estimating the sample size under a random effects model. If data is provided, the estimated heterogeneity is used instead.
#' @param I2 Inconsistency.
#' @param D2 Diversity.
#' @param type Prospective or retrospective sample size calculation.
#' @param trials optional number of trials for design.
#' @param ... additional arguments
#'
#' @return A list of up to 6 elements:
#' \item{settings}{A list containing the arguments provided to the \code{ris} function.}
#' \item{NF}{The total number of required participants in a fixed-effect meta-analysis if type is prospective. Contains a list if the type is retrospective, where \code{NF} is the additional required number of participants and \code{NF_full} is the total required number of participants.}
#' \item{NR_tau}{A list containing: \code{minTrial} the minimum number of trials. \code{nPax} a matrix containing four possible number of trials with the number of participants per trial and total number of participants. \code{tau2} the estimate used for the calculation. Might contain \code{NR_tau_ll} and \code{NR_tau_ul} which contain the same three elements. \code{NR_tau_ll} is based on the lower value in the confidence interval of tau2. \code{NR_tau_ul} is based on the upper value in the confidence interval for tau2. If the type is prospective the numbers are the total required. If the type is retrospective the numbers are the additional required.}
#' \item{NR_D2}{The total number of required participants in a random-effects meta-analysis adjusted by diversity (\code{D2}) if type is prospective. Contains a list if the type is retrospective, where \code{NR_D2} is the additional required number of participants and \code{NR_D2_full} is the total required number of participants.}
#' \item{NR_I2}{The total number of required participants in a random-effects meta-analysis adjusted by inconsistency (\code{I2}) if type is prospective. Contains a list if the type is retrospective, where \code{NR_I2} is the additional required number of participants and \code{NR_I2_full} is the total required number of participants.}
#'
#' @export
#' @aliases print.ris
#'
#' @examples
#' ris(outcome = "RR", mc = 0.8, p0 = 0.12, fixed = TRUE)
#'
#' data("perioOxy")
#' ma <- metaanalysis(outcome = "RR", data = perioOxy, mc = 0.8, beta = 0.2)
#' ma$ris
#' ma <- metaanalysis(outcome = "RR", data = perioOxy)
#' ris(outcome = "RR", mc = 0.8, ma = ma, type = "retrospective", fixed = FALSE, beta = 0.2)
ris <-
  function(outcome,
           mc,
           side = 2,
           alpha = 0.05,
           beta = 0.1,
           fixed = TRUE,
           sd_mc = NULL,
           p0 = NULL,
           p1 = NULL,
           ma = NULL,
           tau2 = NULL,
           I2 = NULL,
           D2 = NULL,
           type = "prospective",
           trials = NULL,
           ...) {

    # check input
    if (outcome == "MD" & (is.null(sd_mc) | is.null(mc))) {
      stop("For continuous outcomes provide mc and sd_mc")
    }

    if (type == "prospective" & outcome %in% c("RR", "OR") & (is.null(p0) | is.null(mc))) {
      stop("For binary outcomes OR and RR provide mc and p0.")
    }

    if (outcome == "RD" & (is.null(mc) | is.null(p0) | is.null(p1))) {
      stop("For binary outcome RD provide mc, p0 and p1.")
    }

    if(type == "retrospective" & (is.null(ma))){
      stop("A metaanalysis object is required for retrospective sample size calculation")
    }

    if ((!is.null(tau2) | !is.null(I2) | !is.null(D2) ) & fixed == TRUE) {
      fixed <- FALSE
      warning("`fixed` is changed from TRUE to FALSE due to presence of tau2, I2 and/or D2.")
    }
    
    if (type == "retrospective" & fixed == TRUE & !is.null(ma$synthesize$peR[6])) {
      fixed <- FALSE
      warning("`fixed` is changed from TRUE to FALSE due to presence of heterogeneity in meta-analysis.")
    }

    # calculate theta and nu
    if (outcome %in% c("OR", "RR")) {
      # prepare helper functions
      logit <- function(x)
        log(x / (1 - x))
      invlogit <- function(x)
        1 / (1 + exp(-x))

      if(type == "retrospective" & is.null(p0)){
        p0 <- sum(ma$metaPrepare$data$eC)/sum(ma$metaPrepare$data$nC)
      }

      if (outcome == "RR") {
        pI <- exp(log(p0) + log(mc))
        pC <- p0
      } else if (outcome == "OR") {
        pI <- invlogit(logit(p0) + log(mc))
        pC <- invlogit(logit(p0))
      }
      p <- (pC + pI) / 2
      var_mc <- p * (1 - p)
      mc_nf <- pC - pI
    }
    
    if(outcome == "MD"){
      var_mc <- sd_mc^2
      mc_nf <- mc
    }

    if(outcome == "RD"){

      if(type == "retrospective" & is.null(p0)){
        p0 <- (ma$metaPrepare$data$eC)/(ma$metaPrepare$data$nC)
        p1 <- (ma$metaPrepare$data$eI)/(ma$metaPrepare$data$nI)
      }

      var_mc <- p0*(1-p0)+p1*(1-p1)
      mc_nf <- mc
    }
    
    # store argumetns
    args <- mget(names(formals()),sys.frame(sys.nframe()))

    # calculate fixed effect sample size
    NF <- 2 * (qnorm(1 - alpha / side) + qnorm(1 - beta)) ^ 2 * 2 * var_mc / mc_nf ^ 2
    NF <- ceiling(NF) + ceiling(NF) %% 2

    if(!is.null(ma)){
      args[names(args) == "ma"] <- NULL
    }
    
    args$var_mc = var_mc
    
    # create output
    outlist <-
      list(
        NF = NF,
        settings = args
      )
    
    # calculate random-effects sample size
    # when used inside metaanalysis or metaanalysis object is provided
    if (!is.null(ma) & fixed == FALSE & !is.null(ma$synthesize$peR)) {

      if(outcome %in% c("RR", "OR")){
        NR_tau <- minTrial(outcome = outcome, mc = mc, alpha = alpha, side = side,
                       beta = beta, p0 = p0, tau2 = ma$synthesize$U[1],
                       var_random = ma$synthesize$peR[6])
        NR_tau <- append(NR_tau, list(tau2 = ma$synthesize$ci.tau$random[1,1]))
        war_het <- NR_tau$war_het
        if(ma$synthesize$ci.tau$random[1,2] != 0){
        NR_tau_ll <- minTrial(outcome = outcome, mc = mc, alpha = alpha,
                          beta = beta, p0 = p0, side = side,
                          tau2 = ma$synthesize$ci.tau$random[1,2],
                          var_random = ma$synthesize$peR[6])
        NR_tau_ll <- append(NR_tau_ll, list(tau2 = ma$synthesize$ci.tau$random[1,2]))
        } else {
          NR_tau_ll <- NULL
        }
        if(!is.null(NR_tau)){
          NR_tau_ul <- minTrial(outcome = outcome, mc = mc, alpha = alpha,
                          beta = beta, p0 = p0, side = side,
                          tau2 = ma$synthesize$ci.tau$random[1,3],
                          var_random = ma$synthesize$peR[6])
        } else {
          NR_tau_ul <- NULL
        }
        NR_tau_ul <- append(NR_tau_ul, list(tau2 = ma$synthesize$ci.tau$random[1,3]))
      } else if(outcome == "RD"){
        NR_tau <- minTrial(outcome = outcome, mc = mc, alpha = alpha, side = side,
                       beta = beta, p0 = p0, p1 = p1, tau2 = ma$synthesize$U[1],
                       var_random = ma$synthesize$peR[6])
        NR_tau <- append(NR_tau, list(tau2 = ma$synthesize$ci.tau$random[1,1]))
        war_het <- NR_tau$war_het
        if(ma$synthesize$ci.tau$random[1,2] != 0){
        NR_tau_ll <- minTrial(outcome = outcome, mc = mc, alpha = alpha,
                          beta = beta, p0 = p0, p1 = p1, side = side,
                          tau2 = ma$synthesize$ci.tau$random[1,2],
                          var_random = ma$synthesize$peR[6])
        NR_tau_ll <- append(NR_tau_ll, list(tau2 = ma$synthesize$ci.tau$random[1,2]))} else {
          NR_tau_ll <- NULL
        }
        NR_tau_ul <- minTrial(outcome = outcome, mc = mc, alpha = alpha,
                          beta = beta, p0 = p0, p1 = p1, side = side,
                          tau2 = ma$synthesize$ci.tau$random[1,3],
                          var_random = ma$synthesize$peR[6])
        NR_tau_ul <- append(NR_tau_ul, list(tau2 = ma$synthesize$ci.tau$random[1,3]))
      } else {
        NR_tau <- minTrial(outcome = outcome, mc = mc, alpha = alpha, side = side,
                       beta = beta, var_mc = var_mc, tau2 = ma$synthesize$U[1],
                       var_random = ma$synthesize$peR[6])
        NR_tau <- append(NR_tau, list(tau2 = ma$synthesize$ci.tau$random[1,1]))
        war_het <- NR_tau$war_het
        if(ma$synthesize$ci.tau$random[1,2] != 0){
        NR_tau_ll <- minTrial(outcome = outcome, mc = mc, alpha = alpha,
                          beta = beta, var_mc = var_mc,
                          tau2 = ma$synthesize$ci.tau$random[1,2],
                          var_random = ma$synthesize$peR[6])
        NR_tau_ll <- append(NR_tau_ll, list(tau2 = ma$synthesize$ci.tau$random[1,2]))
        } else {
          NR_tau_ll <- NULL
        }
        NR_tau_ul <- try(minTrial(outcome = outcome, mc = mc, alpha = alpha,
                              beta = beta, var_mc = var_mc, side = side,
                              tau2 = ma$synthesize$ci.tau$random[1,3],
                              var_random = ma$synthesize$peR[6]), TRUE)
        if(inherits(NR_tau_ul,"try-error")){
          NR_tau_ul = "Too much heterogeneity to estimate an upper number of trials."
        } else {
          NR_tau_ul <- append(NR_tau_ul, list(tau2 = ma$synthesize$ci.tau$random[1,3]))
        }
      }
      
      NR_tau[names(NR_tau) == "war_het"] <- NULL
      NR_tau_ll[names(NR_tau_ll) == "war_het"] <- NULL
      NR_tau_ul[names(NR_tau_ul) == "war_het"] <- NULL

      # sample size for inconsistency adj. and diversity adj.
      NR_D2 <- 1 / (1 - ma$synthesize$U[4]) * NF
      NR_I2 <- 1 / (1 - ma$synthesize$U[3]) * NF
      NR_D2 <- ceiling(NR_D2) + ceiling(NR_D2) %% 2
      NR_I2 <- ceiling(NR_I2) + ceiling(NR_I2) %% 2
      
      # set relative to the sample size already achieved
        NR_tau_full <-
          NR_tau$nPax[3, 4] + (sum(ma$metaPrepare$data$nI) + sum(ma$metaPrepare$data$nC))
        NR_I2_full <- NR_I2
        NR_D2_full <- NR_D2
        NR_I2 <- NR_I2 - (sum(ma$metaPrepare$data$nI) + sum(ma$metaPrepare$data$nC))
        NR_D2 <- NR_D2 - (sum(ma$metaPrepare$data$nI) + sum(ma$metaPrepare$data$nC))

      outlist <-
        append(outlist, list(
          NR_D2 = list("NR_D2" = NR_D2, "NR_D2_full" = NR_D2_full),
          NR_I2 = list("NR_I2" = NR_I2, "NR_I2_full" = NR_I2_full),
          NR_tau = list("NR_tau" = NR_tau, "NR_tau_ll" = NR_tau_ll,
                        "NR_tau_ul" = NR_tau_ul, "NR_tau_full" = NR_tau_full),
          war_het = war_het
        ))
    } else if(fixed == FALSE & is.null(ma)){
      # not inside metaanalysis call or metaanalysis object is not provided

      if(is.null(tau2) & is.null(I2) & is.null(D2)){
        stop("No value for heterogeneity (tau2, I2 and/or D2) is provided")
      }

      if(is.null(tau2)) tau2 <- 0
      NR_D2 = NULL
      NR_I2 = NULL

      if(outcome %in% c("RR", "OR")){
        NR_tau <- minTrial(outcome = outcome, mc = mc, alpha = alpha,
                       beta = beta, p0 = p0, tau2 = tau2,
                       trials = trials)
      } else if(outcome == "RD"){
        NR_tau <- minTrial(outcome = outcome, mc = mc, alpha = alpha,
                       beta = beta, p0 = p0, p1 = p1, tau2 = tau2,
                       trials = trials)
      } else{
        NR_tau <- minTrial(outcome = outcome, mc = mc, alpha = alpha,
                       beta = beta, var_mc = var_mc, tau2 = tau2,
                       trials = trials)
      }
      if(!is.null(I2)){
        NR_I2 <- 1 / (1 - I2) * NF
        NR_I2 <- ceiling(NR_I2) + ceiling(NR_I2) %% 2
      }
      if(!is.null(D2)){
        NR_D2 <- 1 / (1 - D2) * NF
        NR_D2 <- ceiling(NR_D2) + ceiling(NR_D2) %% 2
      }

      outlist <-
        append(outlist, list(
          NR_tau = NR_tau,
          NR_D2 = NR_D2,
          NR_I2 = NR_I2
        ))
    }
    
    if(type == "retrospective"){
      NF_full <- NF
      NF <- NF - (sum(ma$metaPrepare$data$nI) + sum(ma$metaPrepare$data$nC))

      outlist[names(outlist) == "NF"] <- NULL
      outlist$NF <- list("NF" = NF, "NF_full" = NF_full)
    }

    class(outlist) <- "ris"
    return(outlist)
  }


# FUNCTION | print ris ----
#' @method print ris
#' @export
print.ris <- function(x, ...) {
  if (x$settings$type == "prospective") {
    cat(
      "This is a prospective meta-analysis sample size calculation.\nThe sample size calculation assumes a",
      paste0(x$settings$side, "-sided test,"), "equal group sizes,\na type-I-error of",
      x$settings$alpha,
      "and a type-II-error of",
      paste0(x$settings$beta, "."), "\nThe minimum clinical relevant value is set to:", x$settings$mc, "for outcome metric", paste0(x$settings$outcome,".\n")
    )
    cat("Additional parametres for sample size are:\n")
    if(x$settings$outcome %in% c("RR", "OR")){
      cat("Probability of event in the control group:", paste0(x$settings$p0, "."))
    } else if(x$settings$outcome == "MD"){
      cat("Standard deviation of the mean (standard error): ", paste0(x$settings$sd_mc, "."))
    } else {
      cat("Probability of event in the control group:", x$settings$p0, "and probability of event in intervention group:", paste0(x$settings$p1, "."))
    }
    cat("\n\n")
    cat("Fixed-effect required information size:\n")
    cat(paste(x$NF, "participants in total. \n"))
    if (x$settings$fixed == FALSE) {
      cat("\n")
      cat("Random-effects required information size:\n")
      if (!is.null(x$settings$tau2) & is.null(x$war_het)) {
        cat(
          paste("Adjusted by tau^2:",
            x$NR_tau$nPax[3, 1],
            "participants in total split over (at minimum)",
            x$NR_tau$nPax[1, 1],
            "trial(s).\n"
          )
        )
      }
      if (!is.null(x$NR_D2)) {
        cat(paste("Adjusted by diversity (D^2):",
          x$NR_D2,
          "participants in total.\n"
        ))
      }
      if (!is.null(x$NR_I2)) {
        cat(paste("Adjusted by inconsistency (I^2):",
          x$NR_I2,
          "participants in total.\n"
        ))
      }
    }
    cat(
      "\nFor more information about the sample size calculation see vignette:\n'Calculating required sample size and required number of trials'."
    )
    if(!is.null(x$war_het)){cat("\n\nPlease note the following warnings:\n")
      cat("*",x$war_het)}
  } else {
    cat(
      "This is a retrospective meta-analysis sample size calculation. \nThe sample size calculation assumes a", paste0(x$settings$side, "-sided test,"), "equal group sizes,\na type-I-error of",
      x$settings$alpha,
      "and a type-II-error of",
      x$settings$beta,
      "\n\n"
    )
    cat("Fixed-effect required information size:\n")
    if(x$NF$NF < 0){
      cat("The number of required participants for a fixed-effect meta-analysis is reached.\n")
    } else {
      cat(paste(x$NF$NF, "participants in total are additionally required. \n"))}
    if (x$settings$fixed == FALSE) {
      cat("\n")
      cat("Random-effects required information size:\n")
      if (!is.null(x$NR_tau$NR_tau$tau2) & is.null(x$war_het)) {
        cat(
          paste("Adjusted by tau^2:",
            x$NR_tau$NR_tau$nPax[3, 1],
            "participants in total are additionally required. \nThese can be split over (at minimum)",
            x$NR_tau$NR_tau$nPax[1, 1],
            "trial(s).\n"
          )
        )
      }
      if (!is.null(x$NR_D2$NR_D2) & x$NR_D2$NR_D2 >= 0) {
        cat(paste("Adjusted by diversity (D^2):",
          x$NR_D2$NR_D2,
          "participants in total are additionally required. \n"
        ))
      } else if(!is.null(x$NR_D2$NR_D2) & x$NR_D2$NR_D2 < 0){
        cat(paste("Adjusted by inconsistency (D^2):",
                  "The number of required participants for a random-effects meta-analysis is reached.\n"))
      }
      if (!is.null(x$NR_I2$NR_I2) & x$NR_I2$NR_I2 >= 0) {
        cat(paste("Adjusted by inconsistency (I^2):",
          x$NR_I2$NR_I2,
          "participants in total are additionally required.\n"
        ))
      } else if(!is.null(x$NR_I2$NR_I2) & x$NR_I2$NR_I2 < 0){cat(paste("Adjusted by inconsistency (I^2):",
                                                             "The number of required participants for a random-effects meta-analysis is reached.\n"
      ))}
    }
    cat(
      "\nFor more information about the sample size calculation see vignette: \n'Calculating required sample size and required number of trials'."
    )
    if(!is.null(x$war_het)){cat("\n\nPlease note the following warnings:\n")
     cat("-",x$war_het)}
  }
}
