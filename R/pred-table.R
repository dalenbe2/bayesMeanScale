
predTableF <- function(preds, model_data, at_vars, at_values, hdi_interval, digits, ci, at_means){

  if(at_means==F){

    # tack on the grouping values and get the means #

    predsNew <- preds %>%
      as.data.table() %>%
      cbind(model_data[, ..at_vars]) %>%
      .[, lapply(.SD, mean), by=at_vars, .SDcols=!at_vars] %>%
      melt(id.vars       = at_vars,
           variable.name = 'which_pred',
           value.name    = 'pred')

    # make the table #

    if(hdi_interval==TRUE){

      predTable <- predsNew %>%
        .[, .(mean      = round(mean(pred), digits=digits),
              median    = round(median(pred), digits=digits),
              ci_level  = paste(paste(ci*100, "%", sep="")),
              hpd_lower = round(hdi(pred, ci=ci)$CI_low, digits=digits),
              hpd_upper = round(hdi(pred, ci=ci)$CI_high, digits=digits)), by=at_vars]

    } else{

      predTable <- predsNew %>%
        .[, .(mean      = round(mean(pred), digits=digits),
              median    = round(median(pred), digits=digits),
              ci_level  = paste(paste(ci*100, "%", sep="")),
              hpd_lower = round(quantile(pred, probs=(1-ci)/2), digits=digits),
              hpd_upper = round(quantile(pred, probs=1-(1-ci)/2), digits=digits)), by=at_vars]

    }

  } else{

    # tack on the grouping values and get the means #

    predsNew <- preds %>%
      as.data.table() %>%
      cbind(at_values) %>%
      melt(id.vars       = at_vars,
           variable.name = 'which_pred',
           value.name    = 'pred')

    # make the table #

    if(hdi_interval==TRUE){

      predTable <- predsNew %>%
        .[, .(mean      = round(mean(pred), digits=digits),
              median    = round(median(pred), digits=digits),
              ci_level  = paste(paste(ci*100, "%", sep="")),
              hpd_lower = round(hdi(pred, ci=ci)$CI_low, digits=digits),
              hpd_upper = round(hdi(pred, ci=ci)$CI_high, digits=digits)), by=at_vars]

    } else{

      predTable <- predsNew %>%
        .[, .(mean      = round(mean(pred), digits=digits),
              median    = round(median(pred), digits=digits),
              ci_level  = paste(paste(ci*100, "%", sep="")),
              hpd_lower = round(quantile(pred, probs=(1-ci)/2), digits=digits),
              hpd_upper = round(quantile(pred, probs=1-(1-ci)/2), digits=digits)), by=at_vars]

    }

  }

  return(predTable)

}
