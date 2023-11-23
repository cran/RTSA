## ---- eval = FALSE------------------------------------------------------------
#  RTSA(type = "design",...)

## ---- eval = FALSE------------------------------------------------------------
#  pro_design <- RTSA(type = "design",...)
#  pro_analysis <- RTSA(type = "analysis", design = pro_design,
#                       data = trials)

## ---- eval = FALSE------------------------------------------------------------
#  retro_analysis <- RTSA(type = "analysis", alpha = alpha_level,
#                         beta = beta_level, data = trials, ...)

## -----------------------------------------------------------------------------
library(RTSA)
design_pma <- RTSA(type = "design", outcome = "MD", alpha = 0.05, beta = 0.1,
                   mc = 0.5, sd_mc = 1,side = 2, timing = c(0.5,0.75,1),
                   es_alpha = "esOF", fixed = TRUE, weights = "IV")
design_pma

## -----------------------------------------------------------------------------
set.seed(0702)
treatA <- rnorm(n = ceiling(87/2), mean = 1.0, sd = 1)
treatB <- rnorm(n = ceiling(87/2), mean = 0.5, sd = 1)
trial1 <- data.frame(mI = mean(treatB), mC = mean(treatA), sdI = sd(treatB),
                     sdC = sd(treatA), nI = ceiling(87/2), nC = ceiling(87/2))
x <- RTSA(type = "analysis", design = design_pma, data = trial1)
x

## -----------------------------------------------------------------------------
eds <- eds[order(eds$year),]
ex_retro_rtsa <- RTSA(type = "analysis", data = eds, side = 2, outcome = "MD",
                      alpha = 0.05, beta = 0.1, futility = "none", 
                      fixed = TRUE, es_alpha = "esOF", mc = -1,
                      ana_times = c(3,6,9))

## ---- fig.width=6, fig.height=5-----------------------------------------------
plot(ex_retro_rtsa)

## -----------------------------------------------------------------------------
ex2_retro_rtsa <- RTSA(type = "analysis", data = coronary, side = 2, outcome = "OR",
                      alpha = 0.05, beta = 0.1, futility = "non-binding", 
                      fixed = FALSE, es_alpha = "esOF", es_beta = "esPoc", mc = 0.9)

## ---- fig.width=6, fig.height=5-----------------------------------------------
plot(ex2_retro_rtsa)

