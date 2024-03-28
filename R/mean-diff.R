
meanDiffF <- function(pred_start, pred_end, model_data, marg_list, at, i){

  if(!is.null(at)){

    atVars <- names(expand.grid(at))

    predDiffOrg <- pred_start - pred_end %>%
      as.data.table()

    predDiff <- predDiffOrg %>%
      cbind(model_data[, ..atVars]) %>%
      .[, lapply(.SD, mean), by=atVars, .SDcols=!atVars] %>%
      melt(id.vars       = atVars,
           variable.name = 'which_diff',
           value.name    = 'diff') %>%
      .[, !"which_diff"] %>%
      .[, `:=`(comparison  = paste0(marg_list$start[[i]], " vs. ", marg_list$end[[i]]),
               marg_effect = marg_list$marg[[i]])]

  } else{

    predDiff <- data.table(diff        = colMeans(pred_start - pred_end),
                           comparison  = paste0(marg_list$start[[i]], " vs. ", marg_list$end[[i]]),
                           marg_effect = marg_list$marg[[i]])

  }

  return(predDiff)

}
