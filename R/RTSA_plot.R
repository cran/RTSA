#' plot.RTSA
#'
#' @param x RTSA object
#' @param model Whether a fixed- or random-effects meta-analysis should be used. Defaults to random.
#' @param type Should Z-scores (classic) or outcome values (outcome) be plotted.
#' @param theme Whether the theme is traditional TSA (classic) or modern (modern)
#' @param sign Whether the y-axis runs from -Inf to Inf, or Inf to -Inf.
#' @param ... Other arguments to plot.RTSA
#'
#' @return Plot. Either a plot for two sided testing or one-sided
#' @export
#'
#' @importFrom ggplot2 ggplot coord_cartesian geom_hline theme_bw geom_vline geom_line geom_point aes theme element_blank geom_ribbon xlab ylab scale_x_continuous expansion scale_y_continuous scale_fill_identity scale_colour_manual ggtitle geom_segment geom_label scale_y_reverse sec_axis theme_classic element_text margin scale_linetype_manual element_rect margin guides guide_legend geom_polygon
#' @importFrom scales percent
#' @importFrom stats na.omit complete.cases
#'
#' @examples
#' data(perioOxy)
#' outRTSA <- RTSA(type = "analysis", data = perioOxy, outcome = "RR", mc = 0.8,
#'  side = 2, alpha = 0.05, beta = 0.2, fixed = FALSE, es_alpha = "esOF")
#' plot(x = outRTSA)
#'
plot.RTSA = function(x, model = "random", type = "classic", theme = "classic", sign = 1, ...){

  if(sum(class(x) == "boundaries") > 0){
    x$settings$side <- x$side
    x$results$AIS <- 1
    x$results$DARIS <- 1
    tmp_ca <- x$alpha/x$side
    x$settings$design <- NULL
    x$settings$type = "design"
    x$settings$futility <- x$futility
    x$bounds$root <- x$root
    x$settings$beta <- x$beta
    x$settings$alpha <- x$alpha
    x$settings$outcome <- "none"
    xlabz <- "Information fraction (IF)"
    x$settings$fixed <- T
    x$settings$es_alpha <- x$es_alpha
    x$settings$es_beta <- x$es_beta

      if(x$side == 1){
        df <- data.frame("timing" = c(0,x$inf_frac*x$root),
                         "alpha_upper" = c(20,x$alpha_ubound))
        if(x$futility != "none"){
          df <- cbind(df, data.frame("beta_lower" = c(-20,x$beta_lbound)))
        }
      } else {
        df <- data.frame("timing" = c(0,x$inf_frac*x$root),
                        "alpha_upper" = c(20,x$alpha_ubound),
                        "alpha_lower" = c(-20,x$alpha_lbound))
        if(x$futility != "none"){
          df <- cbind(df, data.frame("beta_upper" = c(NA,x$beta_ubound),
                                   "beta_lower" = c(NA,x$beta_lbound)))
        }
      }
  } else {

    if(x$settings$outcome %in% c("OR", "RR") & x$settings$mc < 1){
      sign = -1
    }

    xlabz <- ""
  if(model == "random" & x$settings$type == "analysis"){
    if(length(which(!is.na(x$results$results_df$naiveCIrandom_upper))) == 0){
      model <- "fixed"
      }
  }

  if(x$settings$fixed){
    model <- "fixed"
  }

  #CREATE VARIABLES
  if(x$settings$type == "analysis") df <- x$results$results_df
  if(x$settings$type == "design"){
    df <- x$results$design_df
    df$timing <- df$timing
  }
  df <- rbind(NA,df)
  df[1,c("timing", "alpha_upper")] <- c(0, 20)

  if(x$settings$side == 2){
    df[1,"alpha_lower"] <- c(-20)
  } else {
    if(x$settings$futility != "none"){
      df[1,"beta_lower"] <- c(-20)
    }
  }

  tmp_ca <- x$settings$alpha/x$settings$side

  if(!is.null(x$results$seq_inf)){
    if(x$results$seq_inf$lower > x$results$seq_inf$upper){
      temp <- x$results$seq_inf$lower
      x$results$seq_inf$lower <- x$results$seq_inf$upper
      x$results$seq_inf$upper <- temp
    }}

  #LABELS
  if(x$settings$type == "analysis"){
  if(model=="fixed"){
    tmp_outcome <- df$outcome_fixed[!is.na(df$outcome_fixed)]
    tmp_outcome <- tmp_outcome[length(tmp_outcome)]
    if(!is.null(x$results$seq_inf)){
      tmp_lcl <- c(NA,df$TSAadjCIfixed_lower[!is.na(df$TSAadjCIfixed_lower)])
      tmp_lcl <- c(tmp_lcl[-length(tmp_lcl)], x$results$seq_inf$lower)
      tmp_lcl1 <- x$results$seq_inf$lower
      tmp_ucl <- c(NA,df$TSAadjCIfixed_upper[!is.na(df$TSAadjCIfixed_upper)])
      tmp_ucl <- c(tmp_ucl[-length(tmp_ucl)], x$results$seq_inf$upper)
      tmp_ucl1 <- x$results$seq_inf$upper
      tmp_pvalue <- x$results$seq_inf$p.value
      } else {
    tmp_lcl <- df$TSAadjCIfixed_lower
    tmp_lcl1 <- df$TSAadjCIfixed_lower[!is.na(df$TSAadjCIfixed_lower)]
    tmp_lcl1 <- tmp_lcl1[length(tmp_lcl1)]
    tmp_ucl <- df$TSAadjCIfixed_upper
    tmp_ucl1 <- df$TSAadjCIfixed_upper[!is.na(df$TSAadjCIfixed_upper)]
    tmp_ucl1 <- tmp_ucl1[length(tmp_ucl1)]
    tmp_pvalue <- df$pvalues_fixed[!is.na(df$pvalues_fixed)]
    tmp_pvalue <- tmp_pvalue[length(tmp_pvalue)]}

  }else{
    tmp_outcome <- df$outcome_random[!is.na(df$outcome_random)]
    tmp_outcome <- tmp_outcome[length(tmp_outcome)]
    if(!is.null(x$results$seq_inf)){
      tmp_lcl <- c(NA,df$TSAadjCIrandom_lower[!is.na(df$TSAadjCIrandom_lower)], x$results$seq_inf$lower)
      tmp_lcl1 <- x$results$seq_inf$lower
      tmp_ucl <- c(NA,df$TSAadjCIrandom_upper[!is.na(df$TSAadjCIrandom_upper)], x$results$seq_inf$upper)
      tmp_ucl1 <- x$results$seq_inf$upper
      tmp_pvalue <- x$results$seq_inf$p.value
    } else {
    tmp_lcl <- df$TSAadjCIrandom_lower
    tmp_lcl1 <- df$TSAadjCIrandom_lower[!is.na(df$TSAadjCIrandom_lower)]
    tmp_lcl1 <- tmp_lcl1[length(tmp_lcl1)]
    tmp_ucl <- df$TSAadjCIrandom_upper
    tmp_ucl1 <- df$TSAadjCIrandom_upper[!is.na(df$TSAadjCIrandom_upper)]
    tmp_ucl1 <- tmp_ucl1[length(tmp_ucl1)]
    tmp_pvalue <- df$pvalues_random[!is.na(df$pvalues_random)]
    tmp_pvalue <- tmp_pvalue[length(tmp_pvalue)]
  }
  }} else {
    tmp_outcome <- "-"
    tmp_lcl <- "-"
    tmp_lcl1 <- "-"
    tmp_ucl <- "-"
    tmp_ucl1 <- "-"
    tmp_pvalue <- "-"
  }

  if(!is.null(x$results$seq_inf)){
    if(tmp_lcl1 > tmp_ucl1){
      temp <- tmp_lcl1
      tmp_lcl1 <- tmp_ucl1
      tmp_ucl1 <- temp
    }
    ci_text <- paste0(" (", 100*x$settings$conf_level, "% SW-adjusted CI: ",
                      format(round(tmp_lcl1,2),nsmall=2),";",
                      format(round(tmp_ucl1,2),nsmall=2))
  } else if(x$settings$type != "design"){
    ci_text <- paste0(" (",100*x$settings$conf_level, "% TSA-adjusted CI: ",
                      format(round(tmp_lcl1,2),nsmall=2),";",
                      format(round(tmp_ucl1,2),nsmall=2))
  }

  if(x$settings$type == "analysis"){ results <- paste0(
    "Pooled effect (", x$settings$outcome,") ",
    format(round(tmp_outcome,2),nsmall=2), ci_text,
    "), p-value: ",
    format(round(tmp_pvalue,4),nsmall=4))} else {
      results <- paste0(
        "Pooled effect (", x$settings$outcome,") ",
        tmp_outcome,
        " (95% TSA-adjusted CI: ",tmp_lcl1,";",
        tmp_ucl1,
        "), p-value ",tmp_pvalue)
    }

  #TYPE CLASSIC VS NEW (ESTIMATE AND CONFINT)
  if(type=="classic"){
    if(x$settings$type == "analysis"){
    if(model=="fixed"){
      tmp_z <- df$z_fixed
    }else{
      tmp_z <- df$z_random
    }
    }
    ylabz <- "Cummulative Z-score"
  }else if(type == "outcome"){
    if(x$settings$type == "analysis"){
    if(model=="fixed"){
      tmp_z <- c(0,df$outcome_fixed[-1])
    }else{
      tmp_z <- c(0,df$outcome_random[-1])
    }
      if(x$settings$outcome %in% c("OR", "RR")) tmp_z[1] <- 1
    }
    ylabz <- paste0(x$settings$outcome, " (95% TSA- and/or SW-adjusted CI)")
  }

  if(x$settings$type == "analysis"){
  results <- paste0(results,
                    "\n",
                    "tau^2 ",format(round(x$results$metaanalysis$hete_results$hete_est$tau2,2),nsmall=2),", ",
                    "I^2 ",percent(x$results$metaanalysis$hete_results$hete_est$I.2,0.1),", ",
                    "D^2 ",percent(x$results$metaanalysis$hete_results$hete_est$D.2,0.1), ", ",
                    "Heterogeneity p-value ", format(round(x$results$metaanalysis$hete_results$hete_est$Q_pval,4),nsmall=4)
  )
  }
  }

  #CREATE LABELS FOR SETTINGS
  settings <- paste0(
    if(x$settings$type  == "analysis" & is.null(x$settings$design)){ paste0("Retrospective TSA with: ")},
    if(x$settings$type  == "design"){ paste0("TSA design with: ")},
    if(x$settings$type  == "analysis" & !is.null(x$settings$design)){ paste0("Prospective or retrospective TSA with: ")},
    if(sum(class(x) == "RTSA") > 0 & x$settings$outcome %in% c("RR", "OR")){paste0( "P0 ", percent(x$settings$Pax$p0,0.1), ", ")},
    if(sum(class(x) == "RTSA") > 0 & x$settings$outcome == "RR"){paste0( "RRR ", percent(1-x$settings$mc,0.1),", ")},
    if(sum(class(x) == "RTSA") > 0 & x$settings$outcome == "OR"){paste0( "MVD OR ", percent(x$settings$mc,0.1),", ")},
    if(sum(class(x) == "RTSA") > 0 & x$settings$outcome == "MD"){paste0( "MVD ", x$settings$mc,", ")},
    if(sum(class(x) == "RTSA") > 0 & x$settings$outcome == "RD"){paste0( "MVD RD ", percent(x$settings$mc,0.1),", ")},
    "alpha ", percent(x$settings$alpha,0.1), ", ",
    "beta ", percent(x$settings$beta), ".\n",
    if(sum(class(x) == "RTSA") > 0 & x$settings$type == "design"){paste0("RIS (adjusted for sequential design): ", ceiling(x$results$DARIS), ".\n")},
    if(sum(class(x) == "RTSA") > 0 & !x$settings$fixed & model == "random"){paste0("Methods: Random-effects, ", x$settings$re_method, "; ")},
    if(sum(class(x) == "RTSA") > 0 & x$settings$fixed | model == "fixed")"Methods: Fixed-effect, ",
    if(sum(class(x) == "RTSA") > 0){paste0("Weight ", x$settings$weights, ", ")},
    "alpha spending ", x$settings$es_alpha,
    if(x$settings$futility != "none") paste0(", ","futility is ", x$settings$futility, " with "),
    if(x$settings$futility != "none") "beta spending ", x$settings$es_beta,
    "."
  )

  #COLORS AND TRANSPARENCY
  conffill <- "red"
  if(theme=="aussie"){
    colz <- c(`aline` = "springgreen3",
              `bline` = "gold",
              `zline` = "black",
              `ztype` = "solid",
              `outcomeline` = "black",
              `confline` = "red",
              `naiveline` = "#006400",
              `naivetype` = "dashed")
  }else{
    colz <- c(`aline` = "red",
              `bline` = "blue",
              `zline` = "black",
              `ztype` = "solid",
              `outcomeline` = "black",
              `confline` = "red",
              `naiveline` = "#006400",
              `naivetype` = "dashed")
  }

  # LABELS
  if(type == "classic"){
    labz <- c("alpha boundaries",
              "naive boundaries")

    if(x$settings$futility != "none"){
      labz <- c("alpha boundaries", "beta boundaries",
                "naive boundaries")
    }

    if(x$settings$type == "analysis"){
      labz <- c(labz, "z scores")
    }
  } else {
    labz <- c(paste0(100*x$settings$conf_level, "% confidence interval"),
              "cumulative outcome")
  }


  #CREATE PLOT
  p <- ggplot(data = df)

  if(type=="classic"){
    #Zoom in
    p <- p +
      coord_cartesian(xlim = c(0,max(df$timing+0.1,1.1, na.rm = T)),
                      ylim = c(ifelse(x$setting$side == 2, -8, -5),8))

    #Convetional alpha boundaires
    p <- p + geom_line(aes(x = timing, y = rep(qnorm(1-tmp_ca), length(timing)),
                           col = "naiveline", linetype = "naivetype"), linewidth = 0.25,
                       na.rm=TRUE)

    if(x$settings$side == 2){
      p <- p + geom_line(aes(x = timing, y = rep(-qnorm(1-tmp_ca), length(timing)),
                             col = "naiveline", linetype = "naivetype"), linewidth = 0.25,
                         na.rm=TRUE)
      }

    #Zero line
    p <- p + geom_segment(x=0,xend=max(df$timing,df$timing*(x$results$AIS/x$results$DARIS), na.rm = T), y=0, yend = 0,
                          linewidth = 0.25, col = "gray", linetype="solid",
                          na.rm=TRUE)

    if(is.null(x$settings$design) & x$settings$type != "design"){
      lt_alpha <- "dashed"
      colz <- c(colz, `lt_alpha` = lt_alpha)
    } else {
      lt_alpha <- "solid"
      colz <- c(colz, `lt_alpha` = lt_alpha)
    }


    #Alpha boundaries
    p <- p +
      {if(theme == "modern")geom_ribbon(aes(x=timing, ymin=Inf,
                             ymax= alpha_upper, fill = "aline"), alpha = 0.25,na.rm=TRUE)} +
      geom_line(aes(x = timing, y =  alpha_upper, col = "aline", linetype = "lt_alpha"), linewidth = 0.25,
                na.rm=TRUE) +
      geom_point(aes(x = timing, y =  alpha_upper, col = "aline"), cex = 1, na.rm=TRUE)

    if(x$settings$side == 2){
     p <- p +
       {if(theme == "modern")geom_ribbon(aes(x=timing, ymin=-Inf,
                              ymax= alpha_lower, fill = "aline"), alpha=0.25,na.rm=TRUE)} +
       geom_line(aes(x = timing, y =  alpha_lower, col = "aline", linetype = "lt_alpha"),  linewidth = 0.25,
                 na.rm=TRUE) +
       geom_point(aes(x = timing, y = alpha_lower, col = "aline"), cex = 1,
                  na.rm=TRUE)
    }

    #Beta boundaries
    if(x$settings$futility != "none"){
      if(x$settings$futility == "non-binding" | (is.null(x$settings$design) & x$settings$type == "analysis")){
        lt_beta <- "dashed"
        colz <- c(colz, `lt_beta` = lt_beta)
      } else {
        lt_beta <- "solid"
        colz <- c(colz, `lt_beta` = lt_beta)
      }

      if(x$settings$side == 1){
      p <- p +
        {if(theme == "modern") geom_ribbon(aes(x=timing, ymax=20,
                                     ymin= beta_lower, fill = "bline"), alpha=0.25,
                                     na.rm=TRUE)} +
        geom_line(aes(x = timing, y = beta_lower, col = "bline", linetype = "lt_beta"),
                  cex = 0.25, na.rm=TRUE) +
        geom_point(aes(x = timing, y = beta_lower, col = "bline"), cex = 1, na.rm=TRUE)
      }

      if(x$settings$side == 2){
        p <- p +
          {if(theme == "modern") geom_ribbon(aes(x=timing, ymin=beta_upper,
                                  ymax=beta_lower, fill = "bline"), alpha=0.25, na.rm=TRUE)} +
          geom_line(aes(x = timing, y = beta_lower, col = "bline", linetype = "lt_beta"), linewidth = 0.25,
                    na.rm=TRUE) +
          geom_point(aes(x = timing, y = beta_lower, col = "bline"), cex = 1,
                     na.rm=TRUE) +
          geom_line(aes(x = timing, y = beta_upper, col = "bline", linetype = "lt_beta"), linewidth = 0.25,
                    na.rm=TRUE) +
          geom_point(aes(x = timing, y = beta_upper, col = "bline"), cex = 1,
                     na.rm=TRUE)
      }
    }

    if(x$settings$type == "analysis"){
      #Z-curve
      x$zTiming <- c(0,cumsum(x$settings$data$nI+x$settings$data$nC)/x$results$DARIS)
      
      if(max(x$zTiming)<1){x$zTiming <- c(x$zTiming,1)}

    if(length(x$zTiming) < length(df$timing)){
        x$zTiming <- c(x$zTiming, NA)
      }

     p <- p + geom_line(aes(x = x$zTiming,y = sign*tmp_z, col = "zline", linetype = "ztype"), linewidth = 0.25,
                       na.rm=TRUE) +
      geom_point(aes(x = x$zTiming,y = sign*tmp_z, col="zline"), cex = 1.25, na.rm=TRUE)

    # labels and breaks
    breakz <- c(x$zTiming)[c(TRUE,diff(c(x$zTiming[-length(x$zTiming)]))>0.03,TRUE)]
    breakz <- breakz[-length(breakz)]
    
    #AIS + DARIS LINE
    if(x$results$AIS>x$results$DARIS) {expan_x <- 0.05} else {expan_x <- 0}
    p <- p + geom_segment(x=max(c(0,x$orgTiming)[!is.na(df$z_fixed)]),
                          xend=max(c(0,x$orgTiming)[!is.na(df$z_fixed)]),
                          y=-Inf,yend=na.omit(sign*tmp_z)[length(na.omit(tmp_z))],
                          linetype="dotted", cex = 0.5, col = "gray", na.rm=TRUE) +
      geom_vline(xintercept = max(df$timing, na.rm = T), linewidth = 0.25, col = "black") +
      scale_x_continuous(expand = expansion(0,expan_x),
                         breaks=breakz, name = xlabz,
                         labels = c(round(breakz[-(length(breakz))],2),paste0("AIS:\n",x$results$AIS)),
                         sec.axis = sec_axis(~.,
                                             breaks=max(df$timing, na.rm = T),
                                             labels = paste0("DARIS:\n",ceiling(x$results$DARIS))))
    } else {
      p <- p  +
        geom_vline(xintercept = max(df$timing), cex = 0.25, col = "black") +
        scale_x_continuous(expand = c(0,0), limits = c(0, max(df$timing, na.rm = T)),
                           breaks=round(df$timing,2), name = xlabz,
                           sec.axis = sec_axis(~.,
                                               breaks=max(df$timing, na.rm = T),
                                               labels = paste0("SMA RIS IF:\n",round(x$bounds$root,2))))
    }
  } else {
    zeropoint <- 0

    #Zoom in
    p <- p +
      coord_cartesian(xlim = c(0,max(df$timing+0.1,1.1,x$results$AIS/x$results$DARIS, na.rm = T)),
                      ylim = c(tmp_lcl1*0.5,
                               tmp_ucl1*2))

    rep.before <- function(x){
      ind = which(!is.na(x))
      if(is.na(x[1])) ind = c(1,ind)
      rep(x[ind], times = diff(c(ind, length(x)+1)))
    }

    #Confidence intervals
    y_min <- c(tmp_lcl1*0.5,tmp_lcl[-1])
    y_max <- c(tmp_ucl1*2,tmp_ucl[-1])
    timing_out <- rep.before(df$timing)
    y_min <- c(rep.before(y_min[-length(y_min)]),y_min[length(y_min)])
    y_max <- c(rep.before(y_max[-length(y_max)]),y_max[length(y_max)])
    if(length(timing_out) > length(y_min)){
       y_min <- c(y_min, NA)
       y_max <- c(y_max, NA)
    }

    if(is.null(x$settings$design) & x$settings$type != "design"){
      lt_ci <- "dashed"
      lt_z <- "solid"
      colz <- c(colz, `lt_ci` = lt_ci, `lt_z` = lt_z)
    } else {
      lt_ci <- "solid"
      lt_z <- "solid"
      colz <- c(colz, `lt_ci` = lt_ci, `lt_z` = lt_z)
    }

    p <- p + geom_ribbon(aes(x = timing_out,ymin = y_min,
                             ymax = y_max), fill="red", alpha = 0.25, linewidth = 0.25, na.rm=TRUE) +
      geom_line(aes(x = timing_out, y = y_min, col = "confline", linetype = "lt_ci"), na.rm=TRUE) +
      geom_line(aes(x = timing_out, y = y_max, col = "confline", linetype = "lt_ci"), na.rm=TRUE) +
      geom_point(aes(x = timing,y = tmp_z, col="confline"), cex = 1.25, na.rm=TRUE)

    #Zero line
    p <- p + geom_segment(x=0,xend=max(df$timing, na.rm = T), y=zeropoint, yend = zeropoint,
                          linewidth = 0.25, col = "gray", linetype="solid", na.rm=TRUE)

    #Outcome-curve
    p <- p + geom_line(aes(x = timing,y = tmp_z, col = "outcomeline", linetype = "lt_z"), linewidth = 0.25,
                       na.rm=TRUE) +
      geom_point(aes(x = timing,y = tmp_z, col="outcomeline"), cex = 1.25, na.rm=TRUE)

    # labels and breaks
    breakz <- c(0,x$orgTiming)[c(TRUE,diff(c(0,x$orgTiming[-length(x$orgTiming)]))>0.03,TRUE)]

    if(x$results$AIS>x$results$DARIS) {expan_x <- 0.05} else {expan_x <- 0}

    #AIS + DARIS LINE
      p <- p +
        geom_vline(xintercept = max(df$timing, na.rm = T), linewidth = 0.25, col = "black") +
        scale_x_continuous(expand = expansion(0,expan_x), name = xlabz,
                           breaks=breakz,
                           labels = c(round(breakz[-(length(breakz))],2),paste0("AIS:\n",x$results$AIS)),
                           sec.axis = sec_axis(~.,
                                               breaks=max(df$timing, na.rm = T),
                                               labels = paste0("DARIS:\n",ceiling(x$results$DARIS))))
  }

  if(sum(class(x) == "RTSA") > 0){
  #Labels
  p <- p + labs(tag=results, caption=settings)

  if(!(x$settings$outcome %in% c("RR", "OR") & type == "outcome")){
    y_val1 <- ifelse(x$settings$side == 2,-8,-5)
    y_val2 <- 8
  } else {
    y_val1 <- y_min[1]
    y_val2 <- y_max[1]
  }

  #THEME
  p <- p +
    {if(!(x$settings$outcome %in% c("RR", "OR") & type == "outcome")){
      scale_y_continuous(expand = expansion(0), ylabz)}} +
    {if(x$settings$outcome %in% c("RR", "OR") & type == "outcome"){
    scale_y_continuous(expand = expansion(0), ylabz, trans = "log",
                       labels = function(x) sprintf("%.2f", x))}} +
    scale_colour_manual(values=colz, labels = labz, name = "") +
    scale_linetype_manual(values=colz, labels = labz, name = "") +
    theme(legend.position="bottom",
          plot.caption.position = "plot",
          plot.caption = element_text(hjust=0,color="black",size=8),
          axis.title.y = element_text(color="black",size=10),
          axis.text.x = element_text(color="black",size=9),
          axis.text.y = element_text(color="black"),
          axis.title.x = element_text(color="black",size=10),
  #       axis.ticks.x.top = element_blank(),
          axis.line.x = element_blank(),
          panel.background = element_blank(),
          plot.tag.position = c(0,1), plot.tag = element_text(hjust=0, vjust=1.1, size=9),
  legend.key = element_rect(colour = NA, fill = NA),
  legend.box.background = element_blank()) +
    geom_segment(aes(x = 0, xend = max(timing, na.rm = T), y = y_val1, yend = y_val1)) +
    geom_segment(aes(x = 0, xend = 0, y = y_val1, yend = y_val2)) +
    guides(colour = guide_legend(override.aes = list(shape = NA, fill = NA)))
  } else {
    y_val1 <- 8
    y_val2 <- ifelse(x$settings$side == 2, -8, -5)
    ylabz <- "Cummulative Z-score"

    #Labels
    p <- p + labs(caption=settings)

    p <- p +
      scale_y_continuous(expand = expansion(0), ylabz) +
      scale_colour_manual(values=colz, labels = labz, name = "") +
      scale_linetype_manual(values=colz, labels = labz, name = "") +
      scale_fill_discrete(guide = "none") +
      theme(legend.position="bottom",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust=0,color="black",size=8),
            axis.title.y = element_text(color="black",size=10),
            axis.text.x = element_text(color="black",size=9),
            axis.text.y = element_text(color="black"),
            axis.line.x = element_blank(),
            axis.title.x = element_text(color="black",size=10),
            panel.background = element_blank(),
            plot.tag.position = c(0,1), plot.tag = element_text(hjust=0, vjust=1.1, size=9),
            legend.key=element_blank()) +
      geom_segment(aes(x = 0, xend = max(timing, na.rm = T), y = y_val2, yend = y_val2)) +
      geom_segment(aes(x = 0, xend = 0, y = y_val1, yend = y_val2)) +
      guides(colour = guide_legend(override.aes = list(shape = NA)))
  }
  return(p)
}

if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("timing", "alpha_upper", "alpha_lower", "beta_lower",
                           "beta_upper"), package = "RTSA", add = FALSE)
}



