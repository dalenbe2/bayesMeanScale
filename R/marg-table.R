

margTableF <- function(pred_diff, marg_list, at, digits, ci, hdi_interval, at_means, i){

  if(!is.null(at)){

    atVars <- names(expand.grid(at))

    if(hdi_interval==T){

    diffTableTemp <- pred_diff %>%
      .[, .(marg_effect = marg_list$marg[[i]],
            start_val   = marg_list$start[[i]],
            end_val     = marg_list$end[[i]],
            mean        = round(mean(diff), digits=digits),
            median      = round(median(diff), digits=digits),
            ci_level    = paste(paste(ci*100, "%", sep="")),
            hpd_lower   = round(hdi(diff, ci=ci)$CI_low, digits=digits),
            hpd_upper   = round(hdi(diff, ci=ci)$CI_high, digits=digits),
            pd          = round(as.numeric(p_direction(diff)), digits=digits)), by=atVars]

    } else{

      diffTableTemp <- pred_diff %>%
        .[, .(marg_effect = marg_list$marg[[i]],
              start_val   = marg_list$start[[i]],
              end_val     = marg_list$end[[i]],
              mean        = round(mean(diff), digits=digits),
              median      = round(median(diff), digits=digits),
              ci_level    = paste(paste(ci*100, "%", sep="")),
              hpd_lower   = round(quantile(diff, probs=(1-ci)/2), digits=digits),
              hpd_upper   = round(quantile(diff, probs=1-(1-ci)/2), digits=digits),
              pd          = round(as.numeric(p_direction(diff)), digits=digits)), by=atVars]

    }

  } else{

    if(hdi_interval==T){

      diffTableTemp <- data.table(marg_effect = marg_list$marg[[i]],
                                  start_val   = marg_list$start[[i]],
                                  end_val     = marg_list$end[[i]],
                                  mean        = round(mean(pred_diff$diff), digits=digits),
                                  median      = round(median(pred_diff$diff), digits=digits),
                                  ci_level    = paste(paste(ci*100, "%", sep="")),
                                  hpd_lower   = round(hdi(pred_diff$diff, ci=ci)$CI_low, digits=digits),
                                  hpd_upper   = round(hdi(pred_diff$diff, ci=ci)$CI_high, digits=digits),
                                  pd          = round(as.numeric(p_direction(pred_diff$diff)), digits=digits))

    } else{

      diffTableTemp <- data.table(marg_effect = marg_list$marg[[i]],
                                  start_val   = marg_list$start[[i]],
                                  end_val     = marg_list$end[[i]],
                                  mean        = round(mean(pred_diff$diff), digits=digits),
                                  median      = round(median(pred_diff$diff), digits=digits),
                                  ci_level    = paste(paste(ci*100, "%", sep="")),
                                  hpd_lower   = round(quantile(pred_diff$diff, probs=(1-ci)/2), digits=digits),
                                  hpd_upper   = round(quantile(pred_diff$diff, probs=1-(1-ci)/2), digits=digits),
                                  pd          = round(as.numeric(p_direction(pred_diff$diff)), digits=digits))

    }

  }

  return(diffTableTemp)

}
